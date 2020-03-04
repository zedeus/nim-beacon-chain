import
  deques, sequtils, tables,
  chronicles, stew/[bitseqs, byteutils], json_serialization/std/sets,
  ./spec/[beaconstate, datatypes, crypto, digest, helpers, validator],
  ./extras, ./ssz, ./block_pool, ./beacon_node_types

logScope: topics = "attpool"

func init*(T: type AttestationPool, blockPool: BlockPool): T =
  # TODO blockPool is only used when resolving orphaned attestations - it should
  #      probably be removed as a dependency of AttestationPool (or some other
  #      smart refactoring)
  T(
    slots: initDeque[SlotData](),
    blockPool: blockPool,
    unresolved: initTable[Eth2Digest, UnresolvedAttestation](),
    latestAttestations: initTable[ValidatorPubKey, BlockRef]()
  )

proc combine*(tgt: var Attestation, src: Attestation, flags: UpdateFlags) =
  ## Combine the signature and participation bitfield, with the assumption that
  ## the same data is being signed - if the signatures overlap, they are not
  ## combined.

  doAssert tgt.data == src.data

  # In a BLS aggregate signature, one needs to count how many times a
  # particular public key has been added - since we use a single bit per key, we
  # can only it once, thus we can never combine signatures that overlap already!
  if not tgt.aggregation_bits.overlaps(src.aggregation_bits):
    tgt.aggregation_bits.combine(src.aggregation_bits)

    if skipValidation notin flags:
      tgt.signature.aggregate(src.signature)
  else:
    trace "Ignoring overlapping attestations"

proc validate(
    state: BeaconState, attestation: Attestation): bool =
  # TODO what constitutes a valid attestation when it's about to be added to
  #      the pool? we're interested in attestations that will become viable
  #      for inclusion in blocks in the future and on any fork, so we need to
  #      consider that validations might happen using the state of a different
  #      fork.
  #      Some things are always invalid (like out-of-bounds issues etc), but
  #      others are more subtle - how do we validate the signature for example?
  #      It might be valid on one fork but not another. One thing that helps
  #      is that committees are stable per epoch and that it should be OK to
  #      include an attestation in a block even if the corresponding validator
  #      was slashed in the same epoch - there's no penalty for doing this and
  #      the vote counting logic will take care of any ill effects (TODO verify)
  let data = attestation.data
  # TODO re-enable check
  #if not (data.crosslink.shard < SHARD_COUNT):
  #  notice "Attestation shard too high",
  #    attestation_shard = data.crosslink.shard
  #  return

  # Without this check, we can't get a slot number for the attestation as
  # certain helpers will assert
  # TODO this could probably be avoided by being smart about the specific state
  #      used to validate the attestation: most likely if we pick the state of
  #      the beacon block being voted for and a slot in the target epoch
  #      of the attestation, we'll be safe!
  # TODO the above state selection logic should probably live here in the
  #      attestation pool
  if not (data.target.epoch == get_previous_epoch(state) or
      data.target.epoch == get_current_epoch(state)):
    notice "Target epoch not current or previous epoch"

    return

  true

proc slotIndex(
    pool: var AttestationPool, state: BeaconState, attestationSlot: Slot): int =
  ## Grow and garbage collect pool, returning the deque index of the slot

  # We keep a sliding window of attestations, roughly from the last finalized
  # epoch to now, because these are the attestations that may affect the voting
  # outcome. Some of these attestations will already have been added to blocks,
  # while others are fresh off the network.
  # TODO only the latest vote of each validator counts. Can we use that somehow?
  logScope: pcs = "atp_slot_maintenance"

  doAssert attestationSlot >= pool.startingSlot,
    """
    We should have checked in validate that attestation is newer than
    finalized_slot and we never prune things before that, per below condition!
    """ &
    ", attestationSlot: " & $shortLog(attestationSlot) &
    ", startingSlot: " & $shortLog(pool.startingSlot)

  if pool.slots.len == 0:
    # Because the first attestations may arrive in any order, we'll make sure
    # to start counting at the last finalized epoch start slot - anything
    # earlier than that is thrown out by the above check
    info "First attestation!",
      attestationSlot =  $shortLog(attestationSlot),
      cat = "init"
    pool.startingSlot =
      state.finalized_checkpoint.epoch.compute_start_slot_at_epoch()

  if pool.startingSlot + pool.slots.len.uint64 <= attestationSlot:
    trace "Growing attestation pool",
      attestationSlot =  $shortLog(attestationSlot),
      startingSlot = $shortLog(pool.startingSlot),
      cat = "caching"

    # Make sure there's a pool entry for every slot, even when there's a gap
    while pool.startingSlot + pool.slots.len.uint64 <= attestationSlot:
      pool.slots.addLast(SlotData())

  if pool.startingSlot <
      state.finalized_checkpoint.epoch.compute_start_slot_at_epoch():
    debug "Pruning attestation pool",
      startingSlot = $shortLog(pool.startingSlot),
      finalizedSlot = $shortLog(
        state.finalized_checkpoint
             .epoch.compute_start_slot_at_epoch()),
      cat = "pruning"

    # TODO there should be a better way to remove a whole epoch of stuff..
    while pool.startingSlot <
        state.finalized_checkpoint.epoch.compute_start_slot_at_epoch():
      pool.slots.popFirst()
      pool.startingSlot += 1

  int(attestationSlot - pool.startingSlot)

func updateLatestVotes(
    pool: var AttestationPool, state: BeaconState, attestationSlot: Slot,
    participants: seq[ValidatorIndex], blck: BlockRef) =
  for validator in participants:
    let
      pubKey = state.validators[validator].pubkey
      current = pool.latestAttestations.getOrDefault(pubKey)
    if current.isNil or current.slot < attestationSlot:
      pool.latestAttestations[pubKey] = blck

func get_attesting_indices_seq(state: BeaconState,
                                attestation_data: AttestationData,
                                bits: CommitteeValidatorsBits): seq[ValidatorIndex] =
  var cache = get_empty_per_epoch_cache()
  toSeq(items(get_attesting_indices(
    state, attestation_data, bits, cache)))

func addUnresolved(pool: var AttestationPool, attestation: Attestation) =
  pool.unresolved[attestation.data.beacon_block_root] =
    UnresolvedAttestation(
      attestation: attestation,
    )

proc addResolved(pool: var AttestationPool, blck: BlockRef, attestation: Attestation) =
  doAssert blck.root == attestation.data.beacon_block_root

  # TODO Which state should we use to validate the attestation? It seems
  #      reasonable to involve the head being voted for as well as the intended
  #      slot of the attestation - double-check this with spec

  # A basic check is that the attestation is at least as new as the block being
  # voted for..
  if blck.slot > attestation.data.slot:
    notice "Invalid attestation (too new!)",
      attestation = shortLog(attestation),
      blockSlot = shortLog(blck.slot)
    return

  updateStateData(
    pool.blockPool, pool.blockPool.tmpState,
    BlockSlot(blck: blck, slot: attestation.data.slot))

  template state(): BeaconState = pool.blockPool.tmpState.data.data

  if not validate(state, attestation):
    notice "Invalid attestation",
      attestation = shortLog(attestation),
      current_epoch = get_current_epoch(state),
      cat = "filtering"
    return

  # TODO inefficient data structures..

  let
    attestationSlot = attestation.data.slot
    idx = pool.slotIndex(state, attestationSlot)
    slotData = addr pool.slots[idx]
    validation = Validation(
      aggregation_bits: attestation.aggregation_bits,
      aggregate_signature: attestation.signature)
    participants = get_attesting_indices_seq(
      state, attestation.data, validation.aggregation_bits)

  var found = false
  for a in slotData.attestations.mitems():
    if a.data == attestation.data:
      for v in a.validations:
        if validation.aggregation_bits.isSubsetOf(v.aggregation_bits):
          # The validations in the new attestation are a subset of one of the
          # attestations that we already have on file - no need to add this
          # attestation to the database
          # TODO what if the new attestation is useful for creating bigger
          #      sets by virtue of not overlapping with some other attestation
          #      and therefore being useful after all?
          trace "Ignoring subset attestation",
            existingParticipants = get_attesting_indices_seq(
              state, a.data, v.aggregation_bits),
            newParticipants = participants,
            cat = "filtering"
          found = true
          break

      if not found:
        # Attestations in the pool that are a subset of the new attestation
        # can now be removed per same logic as above

        trace "Removing subset attestations",
          existingParticipants = a.validations.filterIt(
            it.aggregation_bits.isSubsetOf(validation.aggregation_bits)
          ).mapIt(get_attesting_indices_seq(
            state, a.data, it.aggregation_bits)),
          newParticipants = participants,
          cat = "pruning"

        a.validations.keepItIf(
          not it.aggregation_bits.isSubsetOf(validation.aggregation_bits))

        a.validations.add(validation)
        pool.updateLatestVotes(state, attestationSlot, participants, a.blck)

        info "Attestation resolved",
          attestation = shortLog(attestation),
          validations = a.validations.len(),
          current_epoch = get_current_epoch(state),
          blockSlot = shortLog(blck.slot),
          cat = "filtering"

        found = true

      break

  if not found:
    slotData.attestations.add(AttestationEntry(
      data: attestation.data,
      blck: blck,
      validations: @[validation]
    ))
    pool.updateLatestVotes(state, attestationSlot, participants, blck)

    info "Attestation resolved",
      attestation = shortLog(attestation),
      current_epoch = get_current_epoch(state),
      validations = 1,
      blockSlot = shortLog(blck.slot),
      cat = "filtering"

proc add*(pool: var AttestationPool, attestation: Attestation) =
  logScope: pcs = "atp_add_attestation"

  let blck = pool.blockPool.getOrResolve(attestation.data.beacon_block_root)

  if blck.isNil:
    pool.addUnresolved(attestation)
    return

  pool.addResolved(blck, attestation)

proc getAttestationsForBlock*(
    pool: AttestationPool, state: BeaconState,
    newBlockSlot: Slot): seq[Attestation] =
  logScope: pcs = "retrieve_attestation"

  if newBlockSlot < (GENESIS_SLOT + MIN_ATTESTATION_INCLUSION_DELAY):
    debug "Too early for attestations",
      newBlockSlot = shortLog(newBlockSlot),
      cat = "query"
    return

  if pool.slots.len == 0: # startingSlot not set yet!
    info "No attestations found (pool empty)",
      newBlockSlot = shortLog(newBlockSlot),
      cat = "query"
    return

  var cache = get_empty_per_epoch_cache()
  let
    # TODO in theory we could include attestations from other slots also, but
    # we're currently not tracking which attestations have already been included
    # in blocks on the fork we're aiming for.. this is a conservative approach
    # that's guaranteed to not include any duplicates, because it's the first
    # time the attestations are up for inclusion!
    attestationSlot = newBlockSlot - MIN_ATTESTATION_INCLUSION_DELAY

  if attestationSlot < pool.startingSlot or
      attestationSlot >= pool.startingSlot + pool.slots.len.uint64:
    info "No attestations matching the slot range",
      attestationSlot = shortLog(attestationSlot),
      startingSlot = shortLog(pool.startingSlot),
      endingSlot = shortLog(pool.startingSlot + pool.slots.len.uint64),
      cat = "query"
    return

  let
    slotDequeIdx = int(attestationSlot - pool.startingSlot)
    slotData = pool.slots[slotDequeIdx]

  for a in slotData.attestations:
    var
      # https://github.com/ethereum/eth2.0-specs/blob/v0.10.1/specs/phase0/validator.md#construct-attestation
      attestation = Attestation(
        aggregation_bits: a.validations[0].aggregation_bits,
        data: a.data,
        signature: a.validations[0].aggregate_signature
      )

    if not validate(state, attestation):
      warn "Attestation no longer validates...",
        cat = "query"
      continue

    # TODO what's going on here is that when producing a block, we need to
    #      include only such attestations that will not cause block validation
    #      to fail. How this interacts with voting and the acceptance of
    #      attestations into the pool in general is an open question that needs
    #      revisiting - for example, when attestations are added, against which
    #      state should they be validated, if at all?
    # TODO we're checking signatures here every time which is very slow - this
    #      is needed because validate does nothing for now and we don't want
    #      to include a broken attestation
    if not check_attestation(
        state, attestation, {nextSlot}, cache):
      continue

    for v in a.validations[1..^1]:
      # TODO We need to select a set of attestations that maximise profit by
      #      adding the largest combined attestation set that we can find - this
      #      unfortunately looks an awful lot like
      #      https://en.wikipedia.org/wiki/Set_packing - here we just iterate
      #      and naively add as much as possible in one go, by we could also
      #      add the same attestation data twice, as long as there's at least
      #      one new attestation in there
      if not attestation.aggregation_bits.overlaps(v.aggregation_bits):
        attestation.aggregation_bits.combine(v.aggregation_bits)
        attestation.signature.aggregate(v.aggregate_signature)

    result.add(attestation)

    if result.len >= MAX_ATTESTATIONS:
      return

proc resolve*(pool: var AttestationPool) =
  logScope: pcs = "atp_resolve"

  var
    done: seq[Eth2Digest]
    resolved: seq[tuple[blck: BlockRef, attestation: Attestation]]

  for k, v in pool.unresolved.mpairs():
    if (let blck = pool.blockPool.getRef(k); not blck.isNil()):
      resolved.add((blck, v.attestation))
      done.add(k)
    elif v.tries > 8:
      done.add(k)
    else:
      inc v.tries

  for k in done:
    pool.unresolved.del(k)

  for a in resolved:
    pool.addResolved(a.blck, a.attestation)

func latestAttestation*(
    pool: AttestationPool, pubKey: ValidatorPubKey): BlockRef =
  pool.latestAttestations.getOrDefault(pubKey)

# https://github.com/ethereum/eth2.0-specs/blob/v0.8.4/specs/core/0_fork-choice.md
# The structure of this code differs from the spec since we use a different
# strategy for storing states and justification points - it should nonetheless
# be close in terms of functionality.
func lmdGhost*(
    pool: AttestationPool, start_state: BeaconState,
    start_block: BlockRef): BlockRef =
  # TODO: a Fenwick Tree datastructure to keep track of cumulated votes
  #       in O(log N) complexity
  #       https://en.wikipedia.org/wiki/Fenwick_tree
  #       Nim implementation for cumulative frequencies at
  #       https://github.com/numforge/laser/blob/990e59fffe50779cdef33aa0b8f22da19e1eb328/benchmarks/random_sampling/fenwicktree.nim

  let
    active_validator_indices =
      get_active_validator_indices(
        start_state, compute_epoch_at_slot(start_state.slot))

  var latest_messages: seq[tuple[validator: ValidatorIndex, blck: BlockRef]]
  for i in active_validator_indices:
    let pubKey = start_state.validators[i].pubkey
    if (let vote = pool.latestAttestation(pubKey); not vote.isNil):
      latest_messages.add((i, vote))

  # TODO: update to 0.10.1: https://github.com/ethereum/eth2.0-specs/pull/1589/files#diff-9fc3792aa94456eb29506fa77f77b918R143
  template get_latest_attesting_balance(blck: BlockRef): uint64 =
    var res: uint64
    for validator_index, target in latest_messages.items():
      if get_ancestor(target, blck.slot) == blck:
        res += start_state.validators[validator_index].effective_balance
    res

  var head = start_block
  while true:
    if head.children.len() == 0:
      return head

    if head.children.len() == 1:
      head = head.children[0]
    else:
      var
        winner = head.children[0]
        winCount = get_latest_attesting_balance(winner)

      for i in 1..<head.children.len:
        let
          candidate = head.children[i]
          candCount = get_latest_attesting_balance(candidate)

        if (candCount > winCount) or
            ((candCount == winCount and candidate.root.data < winner.root.data)):
          winner = candidate
          winCount = candCount
      head = winner

proc selectHead*(pool: AttestationPool): BlockRef =
  let
    justifiedHead = pool.blockPool.latestJustifiedBlock()

  let newHead =
    lmdGhost(pool, pool.blockPool.justifiedState.data.data, justifiedHead.blck)

  newHead
