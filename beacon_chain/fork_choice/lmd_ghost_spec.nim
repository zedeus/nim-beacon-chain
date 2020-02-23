# beacon_chain
# Copyright (c) 2018-2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at https://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at https://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  # Standard library
  tables, hashes,
  # Internal
  ../spec/[datatypes, digest, helpers, beaconstate, validator],
  ../time, ../ssz, ../state_transition,
  ../extras,
  ./fork_choice_interface

# Implementation of the fork choice rule
# close to the spec:
# https://github.com/ethereum/eth2.0-specs/blob/v0.10.1/specs/phase0/fork-choice.md

func hash(cp: Checkpoint): Hash =
  # Naive hash mix
  hash(cp.root) xor hash(cp.epoch)

type
  Store* = object
    time: BeaconTime
    genesis_time: BeaconTime
    justified_checkpoint: CheckPoint
    finalized_checkpoint: Checkpoint
    best_justified_checkpoint: CHeckpoint

    blocks: Table[Eth2Digest, BeaconBlock]
    block_states: Table[Eth2Digest, BeaconState]
    checkpoint_states: Table[Checkpoint, BeaconState]
    latest_messages: Table[ValidatorIndex, LatestMessage]

func get_slots_since_genesis(store: Store): Slot {.inline.} =
  Slot((store.time.uint64 - store.genesis_time.uint64) div SECONDS_PER_SLOT)

func get_current_slot(store: Store): Slot {.inline.} =
  Slot(GENESIS_SLOT + store.get_slots_since_genesis())

func compute_slots_since_epoch_start(slot: Slot): Slot =
  Slot(slot - slot.compute_epoch_at_slot().compute_start_slot_at_epoch())

func get_ancestor(
       store: Store, ancestor: var Eth2Digest,
       root: Eth2Digest, slot: Slot) =
  let blck = store.blocks[root]
  if blck.slot > slot:
    get_ancestor(store, ancestor, blck.parent_root, slot)
  elif blck.slot == slot:
    ancestor = root
  else:
    # Root is older than the queried slot, this a skip slot
    # Return the earliest root prior to slot
    ancestor = root

func get_latest_attesting_balance(
       store: Store, root: Eth2Digest): Gwei =

  let
    state = store.checkpoint_states[store.justified_checkpoint]
    active_indices = get_active_validator_indices(state, state.get_current_epoch())

  for i in active_indices:
    if i in store.latest_messages:
      var ancestor: Eth2Digest
      store.get_ancestor(
        ancestor, store.latest_messages[i].root,
        store.blocks[root].slot
      )
      if ancestor == root:
        result += state.validators[i].effective_balance

func filter_block_tree(
       store: Store, block_root: Eth2Digest,
       blocks: var Table[Eth2Digest, BeaconBlock]
  ): bool =

  ## Update a table of filtered block-tree with potential
  ## finalized/justified checkpoints, return true if a block is added

  let blck = store.blocks[block_root]
  var children: seq[Eth2Digest]
  for root, b in store.blocks.pairs():
    if b.parent_root == block_root:
      children.add root

  # If any children branches contain expected finalized/justified checkpoints,
  # add to filtered block-tree and signal viability to parent
  if children.len > 0:
    for child in children:
      result = result or store.filter_block_tree(child, blocks)
      if result:
        blocks[block_root] = blck
    return

  # If leaf block, check finalized/justified checkpoints as matching latest.
  let
    head_state = store.block_states[block_root]

    correct_justified = (
      store.justified_checkpoint.epoch == GENESIS_EPOCH or
        head_state.current_justified_checkpoint == store.justified_checkpoint
    )

    correct_finalized = (
      store.finalized_checkpoint.epoch == GENESIS_EPOCH or
        head_state.finalized_checkpoint == store.finalized_checkpoint
    )

  # If expected finalized/justified, add to viable block-tree and signal viability to parent.
  if correct_justified and correct_finalized:
    blocks[block_root] = blck
    return true
  # Otherwise branch not viable
  return false

func get_filtered_block_tree(store: Store): Table[Eth2Digest, BeaconBlock] =
  ## Retrieve a filtered block tree from ``store``, only returning branches
  ## whose leaf state's justified/finalized info agrees with that in ``store``
  let base = store.justified_checkpoint.root
  discard store.filter_block_tree(base, result)

func get_head*(store: Store, head: var Eth2Digest): GasperError =
  ## Returns the new head given the LMD-GHOST fork choice rule

  # Get filtered block tree that only include sviable branches
  let blocks = store.get_filtered_block_tree()

  # Run the fork choice
  head = store.justified_checkpoint.root
  let justified_slot = compute_start_slot_at_epoch(store.justified_checkpoint.epoch)

  var children: seq[Eth2Digest]
  while true:
    children.setLen(0)
    for root, blck in blocks.pairs:
      if blck.parent_root == head and blck.slot > justified_slot:
        children.add(root)
    if children.len == 0:
      return

    # If there are children, choose the new head
    # according to latest attesting balance with ties broken lexicographically
    var newHead: Eth2Digest
    var balance: Gwei
    for child in children:
      let childBalance = store.get_latest_attesting_balance(child)
      if childBalance > balance:
        newHead = child
        balance = childBalance
    head = newHead

func should_update_justified_checkpoint(
       store: Store, new_justified_checkpoint: Checkpoint
      ): bool =
  ## To address bouncing attacks, only update the conflicting justified checkpoints
  ## in the fork choice if in the early slots of the epoch.
  ## Otherwise delay incorporation of new justified checkpoint until next epoch boundary
  ##
  ## See https://ethresear.ch/t/prevention-of-bouncing-attack-on-ffg/6114
  ## for more detailed analysis and discussion.

  if store.get_current_slot()
          .compute_slots_since_epoch_start() < SAFE_SLOTS_TO_UPDATE_JUSTIFIED:
    return true

  let justified_slot = store.justified_checkpoint.epoch.compute_start_slot_at_epoch()
  var ancestor: Eth2Digest
  store.get_ancestor(ancestor, new_justified_checkpoint.root, justified_slot)
  if ancestor != store.justified_checkpoint.root:
    return false

  return true

func on_tick*(store: var Store, time: BeaconTime) =
  let previous_slot = store.get_current_slot()

  # Update store time
  store.time = time

  let current_slot = store.get_current_slot()
  # Not a new epoch, return
  if not (current_slot > previous_slot and
          current_slot.compute_slots_since_epoch_start() == 0):
    return
  # Update store.justified_checkpoint if a better checkpoint is known
  if store.best_justified_checkpoint.epoch >
       store.justified_checkpoint.epoch:
    store.justified_checkpoint = store.best_justified_checkpoint

proc on_block*(store: var Store, signed_block: SignedBeaconBlock, flags: UpdateFlags): GasperError =
  # TODO: no side-effect
  let blck = signed_block.message
  # Make a copy of the state to avoid mutability issues
  doAssert blck.parent_root in store.block_states
  var state = store.block_states[blck.parent_root] # "pre_state"

  # Blocks cannot be in the future. If they are, their consideration
  # must be delayed until they are in the past.
  doAssert store.get_current_slot() >= blck.slot
  # Add new block to the store
  store.blocks[blck.hash_tree_root()] = blck

  # Check that the block is later than the finalized epoch slot
  # (optimization to reduce calls to get_ancestor)
  let finalized_slot = store.finalized_checkpoint.epoch.compute_start_slot_at_epoch()
  doAssert blck.slot > finalized_slot
  # Check that the block is a descendant of the finalized block at
  # the checkpoint finalized slot
  block:
    var ancestor: Eth2Digest
    store.get_ancestor(ancestor, blck.hash_tree_root(), finalized_slot)
    doAssert ancestor == store.finalized_checkpoint.root

  # Check that the block is valid and compute the post-state
  # TODO: state_transition should use signed blocks: https://github.com/status-im/nim-beacon-chain/issues/762
  let ok = state_transition(state, signed_block.message, flags)
  doAssert ok, "State transition failure in fork choice \"on_block\""
  # Add new state for this block to the store
  store.block_states[hash_tree_root(signed_block)] = state

  # Update justified checkpoint
  if state.current_justified_checkpoint.epoch > store.justified_checkpoint.epoch:
    if state.current_justified_checkpoint.epoch > store.best_justified_checkpoint.epoch:
      store.best_justified_checkpoint = state.current_justified_checkpoint
    if store.should_update_justified_checkpoint(state.current_justified_checkpoint):
      store.justified_checkpoint = state.current_justified_checkpoint

  # Update finalized checkpoint
  if state.finalized_checkpoint.epoch > store.finalized_checkpoint.epoch:
    store.finalized_checkpoint = state.finalized_checkpoint
    let finalized_slot = store.finalized_checkpoint.epoch.compute_start_slot_at_epoch()

    # Update justified if new justified is later than store justified
    # or if store justified is not in chain with finalized checkpoint
    if state.current_justified_checkpoint.epoch > store.justified_checkpoint.epoch:
      store.justified_checkpoint = state.current_justified_checkpoint
    else:
      var ancestor: Eth2Digest
      store.get_ancestor(ancestor, store.justified_checkpoint.root, finalized_slot)
      if ancestor != store.finalized_checkpoint.root:
        store.justified_checkpoint = state.current_justified_checkpoint

proc on_attestation*(store: var Store, attestation: Attestation, flags: UpdateFlags, cache: var StateCache): GasperError =
  ## Run ``on_attestation`` upon receiving a new ``attestation`` from either within a block
  ## or directly on the wire.
  ##
  ## An ``attestation`` that is asserted as invalid may be valid at a later time,
  ## consider scheduling it for later processing in such case.

  # Alias
  template target: untyped {.dirty.} = attestation.data.target

  # Attestations must be from the current or previous epoch
  let current_epoch = store.get_current_slot().compute_epoch_at_slot()
  # Use GENESIS_EPOCH for previous when genesis to avoid underflow
  let previous_epoch = if current_epoch > GENESIS_EPOCH: current_epoch - 1
                       else: GENESIS_EPOCH
  doAssert target.epoch == current_epoch or target.epoch == previous_epoch
  doAssert target.epoch == attestation.data.slot.compute_epoch_at_slot()

  # Attestations target must be for a known block. If target block is unknown, delay consideration
  # until the block is found
  doAssert target.root in store.blocks
  # Attestations cannot be from future epochs. If they are delay consideration until the epoch arrives
  var base_state = store.block_states[target.root]
  doAssert store.get_current_slot() >= target.epoch.compute_start_slot_at_epoch()

  # Attestations must be for a known block. If block is unknown, delay consideration until the block is found
  doAssert attestation.data.beacon_block_root in store.blocks
  # Attestations must not be for blocks in the future. If not, the attestation should not be considered
  doAssert store.blocks[attestation.data.beacon_block_root].slot <= attestation.data.slot

  # Store target checkpoint state if not yet seen
  if target notin store.checkpoint_states:
    process_slots(base_state, target.epoch.compute_start_slot_at_epoch())
    store.checkpoint_states[target] = base_state
  let target_state = store.checkpoint_states[target]

  # Attestations can only affect the fork choice of subsequent slots.
  # Delay consideration in the fork choice until their slot is in the past
  doAssert store.get_current_slot() >= attestation.data.slot + 1

  # Get state at the `target` to validate attestation and calculate the committees
  let indexed_attestation = target_state.get_indexed_attestation(attestation, cache)
  doAssert target_state.is_valid_indexed_attestation(indexed_attestation, flags)

  # Update latest messages
  for i in indexed_attestation.attesting_indices:
    let v = ValidatorIndex(i) # TODO ValidatorIndex, but that doesn't serialize properly
    if v notin store.latest_messages or target.epoch > store.latest_messages[v].epoch:
      store.latest_messages[v] = LatestMessage(
        epoch: target.epoch, root: attestation.data.beacon_block_root
      )

# Sanity checks
# -----------------------------------------------------

static:
  doAssert Store is GasperContext
