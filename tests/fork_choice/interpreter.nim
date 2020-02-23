# beacon_chain
# Copyright (c) 2018 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at https://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at https://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  # Standard library
  strformat,
  # Internals
  ../../beacon_chain/spec/[datatypes, digest],
  ../../becon_chain/fork_choice/fork_choice_interface

# The fork choice tests are quite complex.
# For flexibility in block arrival, timers, operations sequencing, ...
# we create a small interpreter that will trigger events in proper order
# before fork choice.

type
  OpKind = enum
    GetHead
    InvalidGetHead
    ProcessBlock
    ProcessAttestation
    Prune

  Operation = object
    # variant specific fields
    case kind: OpKind
    of GetHead:
      expectedHead: Eth2Digest
    of InvalidGetHead:
      discard
    of ProcessBlock:
      slot: Slot
      root: Eth2Digest
      parent_root: Eth2Digest
    of ProcessAttestation:
      validator_index: ValidatorIndex
      block_root: Eth2Digest
      target_epoch: Epoch
    of Prune: # ProtoArray specific
      finalized_root: Eth2Digest
      prune_threshold: int
      expected_len: int
    # Common fields
    justified_epoch: Epoch
    justified_root: Eth2Digest
    finalized_epoch: Epoch
    justified_state_balances: seq[uint64]

func applyGetHead(ctx: var GasperContext, id: int, op: Operation) =
  doAssert op.kind == FindHead

  var head: Eth2Digest
  let err = ctx.get_head(head)
  doAssert err == None
  doAssert head == op.expectedHead, &"GetHead (Op ID={id}): expected {$op.expectedHead} but got {$head}"

func applyInvalidGetHead(ctx: var GasperContext, id: int, op: Operation) =
  doAssert op.kind == FindHead

  var head: Eth2Digest
  let err = ctx.get_head(head)
  doAssert err != None

func run(ctx: var GasperContext, ops: seq[Operation]) =
  ## Apply a sequence of fork-choice operations on a store
  for i, op in ops:
    case op.kind
    of GetHead:
      ctx.applyGetHead(id, op)
    of InvalidGetHead:
      ctx.applyInvalidGetHead(id, op)
    of ProcessBlock:
      ctx.applyProcessBlock()
