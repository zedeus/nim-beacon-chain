# beacon_chain
# Copyright (c) 2018-2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at https://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at https://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  # Internal
  ../spec/[datatypes, digest, helpers, validator],
  ../extras,
  ../time

# https://github.com/ethereum/eth2.0-specs/blob/v0.10.1/specs/phase0/fork-choice.md
type
  LatestMessage* = object
    epoch*: Epoch
    root*: Eth2Digest

  GasperContext* = concept ctx, var mut_ctx
    # Properties
    # -----------------------------------------------------
    # ctx.time is uint64
    # ctx.genesis_time is uint64
    # ctx.justified_checkpoint is Checkpoint
    # ctx.finalized_checkpoint is Checkpoint
    # ctx.best_finalized_checkpoint is Checkpoint

    # ctx.blocks is Table[Eth2Digest, BeaconBlock]
    # ctx.block_states is Table[Eth2Digest, BeaconState]
    # ctx.checkpoint_states is Table[Checkpoint, BeaconState]
    # ctx.latest_messages is Table[ValidatorIndex, LatestMessage]

    # Routines
    # -----------------------------------------------------
    # ctx.get_ancestor(var Eth2Digest, Eth2Digest, Slot)
    # ctx.get_latest_attesting_balance(Eth2Digest) is Gwei
    # ctx.filter_block_tree(Eth2Digest, Table[Eth2Digest, BeaconBlock]) is bool
    # ctx.get_filtered_block_tree() is Table[Eth2Digest, BeaconBlock]
    ctx.get_head(var Eth2Digest) is GasperError
    # ctx.should_update_justified_checkpoint(Checkpoint) is bool

    # Handlers
    # -----------------------------------------------------
    mut_ctx.on_tick(BeaconTime)
    mut_ctx.on_block(SignedBeaconBlock, UpdateFlags) is GasperError
    mut_ctx.on_attestation(Attestation, UpdateFlags, var StateCache) is GasperError

  GasperError* = enum
    None
    # Block logic bugs
    Defect_BlockParentNotInStore
    Defect_BlockEarlierThanFinalizedEpochSlot
    # Blocks to delay
    Delay_BlockInTheFuture
    # Invalid blocks in current blockchain
    Consensus_BlockDoesNotShareAncestorAtFinalizedCheckpoint
    # Invalid transition
    Invalid_StateTransitionFailure

    # Attestation logic bug
    Defect_InvalidIndexedAttestation
    # Invalid attestations
    Invalid_AttestationNotFromCurrentOrPreviousEpoch
    Invalid_AttestationInconsistentTargetEpochAndSlot
    # Attestations to delay
    Delay_AttestationTargetNotInStore
    Delay_AttestationFromFutureEpoch
    Delay_AttestationForUnknownBlock
    Delay_AttestationForFutureBlock
    Delay_AttestationForFutureSlot
