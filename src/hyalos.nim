import hyalos/memalloc
import hyalos/spec {.all.}

import pkg/nuclear
import pkg/parith/[math, bitops]
## Parith (ptr arithmetic) module introduces annotated operators for ptr arithmetic
## such as `+!`, `-!`, `&!`, `|!`, `^!`, `~!`, `<<!`, `>>!`, `==!`, `!=!` in a similar
## fashion to ptr arithmetic in C/C++.

export `~` # hint128 sugar
export spec

template rNode[T](p: T): T =
  p ^! 1 # ptr xor
template isRNode[T](p: T): bool =
  bool( cast[uint64](p) and 1'u64 )

proc helpThread*[T; N](tracker: HyalosTracker[T, N]; tid, index, mytid: int) {.inline.}
  # Forward decl

proc getEpoch*[T; N](tracker: HyalosTracker[T, N]): uint64 =
  ## Retrieves the current epoch value from the specified `tracker`.
  ## Parameters:
  ## - `tracker`: The `HyalosTracker` instance to retrieve the epoch from.
  ## Returns:
  ## The current epoch value as a `uint64`.
  return tracker.epoch.load(moAcquire)

proc getRefsNode[T; N](tracker: HyalosTracker[T, N], node: HyalosInfo): HyalosInfo {.inline.} =
  ## Retrieves the references node associated with a given node in the HyalosTracker.
  ##
  ## Parameters:
  ## - `tracker`: The HyalosTracker instance.
  ## - `node`: A pointer to the HyalosInfo node.
  ##
  ## Returns:
  ## - A pointer to the references node associated with the given node.
  ##
  ## Note:
  ## - This procedure assumes that the `batchLink` field of the `node` parameter is a valid pointer.
  ## - If the `refs` node is of type RNode, it is considered as the references node.
  ## - If the `refs` node is not of type RNode, the `node` itself is considered as the references node.
  var refs: HyalosInfo = node.batchLink.load(moAcquire)
  if isRNode(refs):
    refs = node
  return refs

# proc reclaim(obj: pointer) =
  # deallocShared(obj)
proc reclaim[T; N](tracker: HyalosTracker[T, N]; obj: ptr T) =
  `=destroy`(obj[]) # FIXME: is safe?
  deallocShared(obj)

proc traverse[T; N](tracker: HyalosTracker[T, N]; list: ptr HyalosInfo; next: HyalosInfo) =
  ## Traverses the linked list of `HyalosInfo` nodes starting from `next` and
  ## updates the `list` with nodes that have their reference count decremented to 1.
  ##
  ## Parameters:
  ## - `list`: A pointer to the head of the list where nodes with reference count 1 will be added.
  ## - `next`: A pointer to the starting node of the traversal.
  template truthy(r: HyalosInfo): bool =
    r.refs.fetchSub(1, moAcqRel) == 1'u64

  while true:
    var curr: HyalosInfo = next
    if isNil curr:
      break
    if isRNode curr:
      # a special case with a terminal refs node
      var refs: HyalosInfo
      refs = rNode curr
      if truthy refs:
        refs.next[] = cast[Nuclear[HyalosInfo]](list[])
        list[] = refs
      break
    mut next = exchange( curr.next, INVPTR, moAcqRel )

    var refs: HyalosInfo
    refs = load( curr.batchLink, moRelaxed )
    if truthy refs:
      refs.next[] = cast[Nuclear[HyalosInfo]](list[])
      list[] = refs

proc freeList[T; N](tracker: HyalosTracker[T, N], list: var HyalosInfo) {.inline.} =
  ## DESCRIPTION:
  ## This procedure is responsible for freeing a linked list of objects in the HyalosTracker.
  ## It iterates through the list and deallocates each object by calling the `reclaim` procedure.
  ## PARAMETERS:
  ## - tracker: The HyalosTracker that contains the linked list.
  ## - list: A pointer to the head of the linked list.

  # Iterate through the linked list
  while not list.isNil():
    var start: HyalosInfo = rNode load( list.batchLink, moRelaxed )
    # list = list.next # lets just use atomic load
    list = load( list.next, moRelaxed )
    # Iterate through the batch of objects
    doWhile(not start.isNil()):
      var obj: ptr T = getNode[T](start)
      start = start.batchNext
      # Deallocate the object
      tracker.reclaim obj

proc traverseCache[T; N](tracker: HyalosTracker[T, N]; batch: var HyalosBatch; next: HyalosInfo) {.inline.} =
  if not isNil next:
    if batch.listCount == MAX_WFRC:
      tracker.freeList(batch.list)
      batch.list      = nil
      batch.listCount = 0

    tracker.traverse( addr batch.list, next )
    inc batch.listCount


# Slow path operation.
# Involves updating various fields and performing compare-and-swap (CAS) operations.
# Returns a pointer to the updated object.

# PARAMETERS:
# - `tracker`: The `HyalosTracker` instance used for tracking objects.
# - `obj`: A pointer to the object being tracked.
# - `index`: The index of the object in the tracker.
# - `tid`: The thread ID associated with the object.
# - `node`: A pointer to the node associated with the object.

# DESCRIPTION:
# - The procedure starts by initializing variables `birthEpoch` and `parent`.
# - It then checks if the `node` is not nil and retrieves the `birthEpoch` from the parent node.
# - Next, it retrieves the previous epoch from the tracker and increments the slow counter.
# - Various fields in the tracker are updated using the `store` procedure.
# - The procedure then enters a loop where it performs a compare-and-swap (CAS) operation to update the result field.
# - Inside the loop, it checks if the current epoch is equal to the previous epoch.
# - If they are equal, it updates the result field and other related fields.
# - If not, it checks if the `first` field is not nil and updates the `first` field accordingly.
# - The loop continues until the CAS operation is successful or the `first` field is not equal to the current sequence number.
# - After the loop, the sequence number is incremented and various fields are updated.
# - The result pointer is retrieved and checked for validity.
# - If the result pointer is valid, it updates the reference count and performs additional operations.
# - Finally, the slow counter is decremented and the procedure returns the result pointer.

proc slowPath*[T; N](tracker: HyalosTracker[T, N]; obj: ptr Nuclear[ptr T]; index, tid: int; node: ptr T): ptr T =
  var birthEpoch: uint64
  var parent: HyalosInfo

  # Retrieve the birth epoch and parent node information
  if not isNil node:
    parent      = getInfo node
    birthEpoch  =  parent.birthEpoch

    var info: HyalosInfo
    info = load( parent.batchLink, moAcquire )
    if (not isNil info) and (not isRNode(info)):
      birthEpoch = info.birthEpoch

  var prevEpoch: uint64
  prevEpoch = load( tracker.slots[tid].epoch[index].pair[0], moAcquire )
  discard tracker.slowCounter.fetchAdd(1, moAcqRel)

  # Update the shared state with the current object information
  store( tracker.slots[tid].state[index].hyPointer, cast[uint64](obj),  moRelease )
  store( tracker.slots[tid].state[index].hyParent,  parent,             moRelease )
  store( tracker.slots[tid].state[index].hyEpoch,   birthEpoch,         moRelease )

  var seqno: uint64
  var lastResult: UnionValuePair
  seqno = load( tracker.slots[tid].epoch[index].pair[1], moAcquire )
  lastResult.pair[0] = INVPTR
  lastResult.pair[1] = seqno

  store( tracker.slots[tid].state[index].hyResult.full, lastResult.full, moRelease ) # DCAS

  var old, value: UnionValuePair
  var resultEpoch, resultPtr, expseqno: uint64
  var first: HyalosInfo
  block done: #`goto done` == break done
    doWhile(resultPtr == INVPTR):
      var
        objPtr: ptr T     = if obj: load( obj, moAcquire ) else: cast[ptr T](nil)
        currEpoch: uint64 = tracker.getEpoch()

      # Check if the current epoch matches the previous epoch
      if currEpoch == prevEpoch:
        lastResult.pair[0]  = INVPTR
        lastResult.pair[1]  = seqno
        value.pair[0]       = 0'u
        value.pair[1]       = 0'u

        # DCAS (Double Compare And Swap) operation to update the result field
        if compareExchange( tracker.slots[tid].state[index].hyResult.full, lastResult.full, value.full, moAcqRel, moAcquire ):
          store( tracker.slots[tid].epoch[index].pair[1], seqno +% 2, moRelease )
          store( tracker.slots[tid].first[index].pair[1], seqno +% 2, moRelease )
          discard tracker.slowCounter.fetchSub(1'u, moAcqRel)
          return objPtr

      # Check if the first field of the slot is not nil
      if not isNil load( tracker.slots[tid].first[index].list[0], moAcquire ):
        first = exchange( tracker.slots[tid].first[index].list[0], nil, moAcqRel )
        if load( tracker.slots[tid].first[index].pair[1], moAcquire ) != seqno:
          break done
        if first != INVPTR:
          # Traverse the linked list of nodes and update the batch list
          traverseCache tracker, addr batches[tid], first
        currEpoch = getEpoch tracker

      first         = nil
      old.pair[0]   = prevEpoch
      old.pair[1]   = seqno
      value.pair[0] = currEpoch
      value.pair[1] = seqno

      # DCAS (Double Compare And Swap) operation to update the epoch field
      discard compareExchange(
        tracker.slots[tid].epoch[index].full,
        old.full,
        value.full,
        moSeqCst,
        moAcquire
      )

      prevEpoch = currEpoch
      resultPtr = load( tracker.slots[tid].state[index].hyResult.pair[0], moAcquire )

    expseqno = seqno
    # DCAS (Double Compare And Swap) operation to update the sequence number field
    discard compareExchange(
      tracker.slots[tid].epoch[index].pair[1],
      expseqno,
      seqno +% 1,
      moAcqRel,
      moRelaxed
    )

    value.list[0] = nil
    value.pair[1] = seqno + 1
    old.pair[1]   = load( tracker.slots[tid].first[index].pair[1], moAcquire  )
    old.list[1]   = load( tracker.slots[tid].first[index].list[0], moAcquire  )

    while old.pair[1] == seqno: # v DCAS
      # DCAS (Double Compare And Swap) operation to update the first field
      if compareExchangeWeak(
          tracker.slots[tid].first[index].full,
          old.full,
          value.full,
          moAcqRel,
          moAcquire
        ):
        if old.list[0] != INVPTR:
          first = old.list[0]
        break

  inc seqno

  store( tracker.slots[tid].epoch[index].pair[1], seqno + 1'u, moRelease )
  resultEpoch = load( tracker.slots[tid].state[index].hyResult.pair[1], moAcquire )
  store( tracker.slots[tid].epoch[index].pair[0], resultEpoch, moRelease )

  store( tracker.slots[tid].first[index].pair[1], seqno + 1, moRelease )
  resultPtr = 0xFFFFFFFFFFFFFFFC and load( tracker.slots[tid].state[index].hyResult.pair[0], moAcquire )

  var ptrNode: HyalosInfo = getInfo cast[ptr T](resultPtr)

  # Check if the result pointer is valid and update the reference count
  if (resultPtr != 0'u) and (not isNil load( ptrNode.batchLink, moAcquire )):
    var refs: HyalosInfo = tracker.getRefsNode ptrNode
    discard refs.refs.fetchAdd(1'u, moAcqRel)
    if first != INVPTR:
      # Traverse the linked list of nodes and update the batch list
      tracker.traverseCache(addr tracker.batches[tid], first)
    first = exchange( tracker.slots[tid].first[index].list[0], rNode(refs), moAcqRel )

  discard tracker.slowCounter.fetchSub(1'u, moAcqRel)

  if first != INVPTR:
    # Traverse the linked list of nodes and update the batch list
    tracker.traverseCache(addr tracker.batches[tid], first)

  if (not parent.isNil) and (not isNil load( parent.batchLink, moAcquire )):
    var refs: HyalosInfo = tracker.getRefsNode(parent)
    discard refs.refs.fetchAdd(WFR_PROTECT2, moAcqRel)
    var adjs = not WFR_PROTECT2

    for i in 0..<N:
      var exp: HyalosInfo = parent
      if compareExchange(tracker.slots[i].state[tracker.hrNum].hyParent, exp, nil, moAcqRel, moRelaxed ):
        inc adjs

    discard refs.refs.fetchAdd(adjs, moAcqRel)

  return cast[ptr T](resultPtr)

proc clearAll*[T; N](tracker: HyalosTracker[T,N]; tid: int) =
  var first: array[MAX_WFR, HyalosInfo]

  for i in 0..<tracker.hrNum:
    first[i] = exchange( tracker.slots[tid].first[i].list[0], INVPTR, moAcqRel )
  for i in 0..<tracker.hrNum:
    if first[i] != INVPTR:
      tracker.traverse(addr tracker.batches[tid].list, first[i])

  tracker.freeList(tracker.batches[tid].list)
  tracker.batches[tid].list       = nil
  tracker.batches[tid].listCount  = 0

proc tryRetire*[T, N](tracker: HyalosTracker[T, N]; batch: ptr HyalosBatch) =
  # Retire unused memory slots in HyalosBatch
  #
  # Iterates over the memory slots in the batch and checks if they can be retired based on certain conditions.
  # If a memory slot can be retired, it is marked as retired and the necessary adjustments are made to the tracker.
  # Finally, if all the memory slots in the batch have been retired, the batch is freed.

  var
    curr: HyalosInfo = batch.first  # Current HyalosInfo node being processed
    refs: HyalosInfo = batch.last   # Last HyalosInfo node in the batch
    minEpoch: uint64 = refs.birthEpoch  # Minimum birth epoch among all nodes in the batch
    last: HyalosInfo = curr         # Last processed HyalosInfo node

  for i in 0..<N:
    var j = 0
    while j < tracker.hrNum:
      inc j

      var first: HyalosInfo = load( tracker.slots[i].first[j].list[0], moAcquire )
      if first ==! INVPTR:
        continue
      if load( tracker.slots[i].first[j].pair[1], moAcquire ) and 1'u:
        continue
      var epoch: uint64 = load( tracker.slots[i].epoch[j].pair[0], moAcquire )
      if epoch < minEpoch:
        continue
      if load( tracker.slots[i].epoch[j].pair[1], moAcquire ) and 1'u:
        continue
      if last == refs:
        return
      last.slot = addr tracker.slots[i].first[j]
      last = last.batchNext

    while j < (tracker.hrNum + 2):
      inc j
      var first: HyalosInfo = load( tracker.slots[i].first[j].list[0], moAcquire )
      if first ==! INVPTR:
        continue
      var epoch: uint64 = load( tracker.slots[i].epoch[j].pair[0], moAcquire )
      if epoch < minEpoch:
        continue
      if last == refs:
        return
      last.slot = addr tracker.slots[i].first[j]
      last = last.batchNext

  var adjs = not WFR_PROTECT1  # Number of adjustments to be made to the reference count

  while curr != last:
    curr = curr.batchNext

    var
      slotFirst: ptr UnionWordPair = curr.slot  # Pointer to the first field of the memory slot
      slotEpoch: ptr UnionWordPair = slotFirst +! MAX_WFR  # Pointer to the epoch field of the memory slot

    store( curr.next, nil, moRelaxed )  # Set the next field of the current node to nil

    if load( slotFirst.list[0], moAcquire ) == INVPTR:
      continue

    var epoch: uint64 = load( slotEpoch.pair[0], moAcquire )
    if epoch < minEpoch:
      continue

    var prev: HyalosInfo = exchange( slotFirst.list[0], curr, moAcqRel )  # Atomically exchange the first field with the current node
    if not prev.isNil:
      if prev ==! INVPTR:
        var exp: HyalosInfo = curr
        if compareExchange( slotFirst.list[0], exp, INVPTR, moAcqRel, moRelaxed ):
          continue
      else:
        var exp: HyalosInfo = nil
        if not compareExchange( curr.next, exp, prev, moAcqRel, moRelaxed ):
          var list: HyalosInfo = nil
          tracker.traverse(addr list, prev)  # Traverse the linked list of nodes and update the batch list
          tracker.freeList(list)  # Free the retired nodes

    inc adjs  # Increment the number of adjustments

  if refs.refs.fetchAdd(adjs, moAcqRel) == -adjs:
    refs.next = nil
    tracker.freeList(refs)  # Free the batch if all nodes have been retired

  batch.first = nil
  batch.counter = 0  # Reset the batch counter


proc retire*[T, N](tracker: HyalosTracker[T, N]; obj: ptr T; tid: int) =
  # Retire a node in the tracker
  if obj.isNil: return
  var node: HyalosInfo = getInfo obj

  if not tracker.batches[tid].first:
    # If the batch is empty, set the last node to the current node and update its reference count
    tracker.batches[tid].last = node
    store( node.refs, WFR_PROTECT1, moRelaxed )

  else:
    if (tracker.batches[tid].last.birthEpoch > node.birthEpoch):
      # Update the birth epoch of the last node if the current node has a smaller birth epoch
      tracker.batches[tid].last.birthEpoch = node.birthEpoch
    store( node.batchLink, tracker.batches[tid].last, moSeqCst )
    node.batchNext = tracker.batches[tid].first

  tracker.batches[tid].first = node
  inc tracker.batches[tid].counter

  if tracker.collect and (tracker.batches[tid].counter mod tracker.freq == 0):
    # If the collect flag is set and the counter reaches the collection threshold, retire the batch
    store( tracker.batches[tid].last.batchLink, rNode(node), moSeqCst )
    tracker.tryRetire(addr tracker.batches[tid])

proc collecting*[T, N](tracker: HyalosTracker[T, N]): bool = tracker.collect


proc reserveSlot*[T, N](tracker: HyalosTracker[T, N]; obj: ptr T; index, tid: int; node: ptr T) =
  var prevEpoch: uint64
  var attempts: int

  # Load the previous epoch from the slot
  prevEpoch = load( tracker.slots[tid].epoch[index].pair[0], moAcquire )
  attempts = 16
  doWhile(attempts != 0):
    var currEpoch: uint64
    currEpoch = getEpoch(tracker)
    # Check if the current epoch matches the previous epoch
    if currEpoch == prevEpoch:
      return
    else:
      # Update the epoch and get the new current epoch
      prevEpoch = tracker.doUpdate(currEpoch, index, tid)
    dec attempts

  # Call the slowPath function to handle the slow path case
  tracker.slowPath(cast[ptr Nuclear[ptr T]](nil), index, tid, node)


proc read*[T,N](tracker: HyalosTracker[T,N]; obj: ptr Nuclear[ptr T]; index, tid: int; node: ptr T): ptr T =
  var prevEpoch: uint64
  var attempts: int

  # Store the current epoch value of the specified index and thread ID
  prevEpoch = load( tracker.slots[tid].epoch[index].pair[0], moAcquire )

  # max number of attempts
  attempts = 16

  # Loop until the maximum number of attempts is reached
  doWhile(attempts != 0):
    var objPtr: ptr T
    var currEpoch: uint64

    # Load the object pointer from the specified memory location
    objPtr = load( obj, moAcquire )

    # Get the current epoch value from the tracker
    currEpoch = getEpoch(tracker)

    # Check if the current epoch matches the previously stored epoch
    if currEpoch == prevEpoch:
      # return object pointer if epochs match
      return objPtr
    else:
      # If the epochs don't match, update the previous epoch value using the tracker's doUpdate method
      prevEpoch = tracker.doUpdate(currEpoch, index, tid)

    dec attempts

  # If max attempts is reached, enter slowpath
  return tracker.slowPath(obj, index, tid, node)

proc doUpdate[T, N](tracker: HyalosTracker[T, N]; currEpoch: var uint64; index, tid: int): uint64 =
  template truthy: untyped =
    not isNil load( tracker.slots[tid].first[index].list[0], moAcquire )
  if truthy:
    let first = exchange( tracker.slots[tid].first[index].list[0], INVPTR, moAcqRel )
    if first !=! INVPTR:
      ## Traverse cache
    store( tracker.slots[tid].first[index].list[0], 0'u, moSeqCst )
    currEpoch = tracker.getEpoch()
  store( tracker.slots[tid].epoch[index].pair[0], currEpoch, moSeqCst )
  return currEpoch


proc helpThread*[T, N](tracker: HyalosTracker[T,N]; tid, index, mytid: int) {.inline.} =
  ## The help_thread function is designed to assist a thread in completing its operation
  ## by updating shared state and ensuring consistency.
  var lastResult: UnionValuePair = cast[UnionValuePair](
    load( tracker.slots[tid].state[index].hyResult.full, moAcquire )
    ) # Load last result for given state structure for given tid and index

  if lastResult.pair[0] != INVPTR:
    # if first part of lastResult is not an invalid ptr, return immediately
    return

  var
    birthEpoch          = load( tracker.slots[tid].state[index].hyEpoch, moAcquire )
    parent: HyalosInfo  = load( tracker.slots[tid].state[index].hyParent, moAcquire )

  if not isNil parent:
    store( tracker.slots[mytid].first[tracker.hrNum].list[0], nil, moSeqCst )
    store( tracker.slots[mytid].epoch[tracker.hrNum].pair[0], birthEpoch, moSeqCst )
  store( tracker.slots[mytid].state[tracker.hrNum].hyParent, parent, moSeqCst )

  var
    obj: ptr Nuclear[ptr T] = cast[ptr Nuclear[ptr T]](
      load( tracker.slots[tid].state[index].hyPointer, moAcquire )
    )
    seqno: uint64 = load( tracker.slots[tid].epoch[index].pair[1], moAcquire )

  if lastResult.pair[1] == seqno:
    var prevEpoch: uint64 = tracker.getEpoch()

    template truthy: untyped =
      lastResult.full == load( tracker.slots[tid].state[index].hyResult.full, moAcquire )

    block done:
      doWhile(truthy):
        prevEpoch = tracker.doUpdate(prevEpoch, tracker.hrNum +% 1, mytid)

        var
          objPtr: ptr T = if not isNil obj: load( obj, moAcquire ) else: nil
          currEpoch     = getEpoch tracker

        if currEpoch == prevEpoch:
          var value: UnionValuePair
          value.pair[0] = cast[uint64](objPtr)
          value.pair[1] = currEpoch

          # DCAS
          if compareExchange(
              tracker.slots[tid].state[index].hyResult.full,
              lastResult.full,
              value.full,
              moAcqRel,
              moAcquire
              ):
            # An empty epoch transition
            var expseqno: uint64 = seqno
            discard compareExchange(
              tracker.slots[tid].epoch[index].pair[1],
              expseqno,
              seqno + 1'u,
              moAcqRel,
              moRelaxed
            )
            # clean up the list
            var old: UnionValuePair
            value.list[0] = nil
            value.pair[1] = seqno + 1'u
            old.pair[1]   = load( tracker.slots[tid].first[index].pair[1], moAcquire )
            old.list[0]   = load( tracker.slots[tid].first[index].list[0], moAcquire )
            while old.pair[1] == seqno:
              # DCAS
              if compareExchangeWeak(
                tracker.slots[tid].first[index].full,
                old.full,
                value.full,
                moAcqRel,
                moAcquire
                ):
                # clean up the list
                if old.list[0] !=! INVPTR:
                  tracker.traverseCache(mut tracker.batches[mytid], old.list[0])
                break
            inc seqno
            # set the real epoch
            value.pair[0] = currEpoch
            value.pair[1] = seqno + 1
            old.pair[1]   = load( tracker.slots[tid].epoch[index].pair[1], moAcquire )
            old.pair[0]   = load( tracker.slots[tid].epoch[index].pair[0], moAcquire )
            while old.pair[1] == seqno:
              # DCAS - 2 iterations at most
              if compareExchangeWeak(
                tracker.slots[tid].epoch[index].full,
                old.full,
                value.full,
                moAcqRel,
                moAcquire
                ): break
            # check if the node is already retired
            var ptrVal: ptr T = objPtr &! 0xFFFFFFFFFFFFFFFC'u # ptr bitops
            var ptrNode: HyalosInfo = getInfo ptrVal
            if (ptrVal !=! 0) and (not isNil load( ptrNode.batchLink, moAcquire )):
              var refs: HyalosInfo = tracker.getRefsNode(ptrNode)
              discard refs.refs.fetchAdd(1'u, moAcqRel)
              # clean up the list
              value.list[0] = rNode refs
              value.pair[1] = seqno + 1
              old.pair[1]   = load( tracker.slots[tid].first[index].pair[1], moAcquire )
              old.list[0]   = load( tracker.slots[tid].first[index].list[0], moAcquire )
              while old.pair[1] == seqno: # n iterations at most
                # DCAS
                if compareExchangeWeak(
                  tracker.slots[tid].first[index].full,
                  old.full,
                  value.full,
                  moAcqRel,
                  moAcquire
                  ):
                  # clean up the list
                  if old.list[0] !=! INVPTR: # ptr arith
                    tracker.traverseCache(mut tracker.batches[mytid], old.list[0])
                  break done
              # already inserted
              discard refs.refs.fetchSub(1'u, moAcqRel)
            else:
              # an empty list transition
              var expseqno: uint64 = seqno
              discard compareExchangeWeak(
                tracker.slots[tid].first[index].pair[1],
                expseqno,
                seqno + 1'u,
                moAcqRel,
                moRelaxed
                )
          break
        prevEpoch = currEpoch

    if 0 != exchange( tracker.slots[mytid].epoch[tracker.hrNum +% 1].pair[0], 0'u, moSeqCst ):
      var first: HyalosInfo = exchange( tracker.slots[mytid].first[tracker.hrNum +% 1].list[0], INVPTR, moAcqRel )
      tracker.traverseCache(mut tracker.batches[mytid], first)

  # the helpee provided an extra reference
  if parent != exchange( tracker.slots[mytid].state[tracker.hrNum].hyParent, nil, moSeqCst ):
    var refs: HyalosInfo = tracker.getRefsNode(parent)
    if refs.refs.fetchSub(1, moAcqRel) == 1:
      refs.next[] = cast[Nuclear[HyalosInfo]](tracker.batches[mytid].list)
      tracker.batches[mytid].list = refs

  # the parent reservation reference
  if 0 != exchange( tracker.slots[mytid].epoch[tracker.hrNum].pair[0], 0'u, moSeqCst ):
    var first: HyalosInfo = exchange( tracker.slots[mytid].first[tracker.hrNum].list[0], INVPTR, moAcqRel )
    tracker.traverseCache(mut tracker.batches[mytid], first)

  tracker.freeList(tracker.batches[mytid].list)
  tracker.batches[mytid].list       = nil
  tracker.batches[mytid].listCount  = 0

proc helpRead*[T, N](tracker: HyalosTracker[T,N], mytid: int) =
  ## This procedure helps find threads that need help reading
  ## Arguments:
  ## - `tracker`: The HyalosTracker instance to read data from.
  ## - `mytid`: The thread ID of the current thread.

  # Template to check if the slowCounter is not zero
  template truthy: untyped =
    load( tracker.slowCounter, moAcquire ) != 0'u64

  # Template to check if the result pointer is invalid
  template checkPtrs(i, j: SomeNumber): untyped =
    let resultPtr = load( tracker.slots[i].state[j].hyResult.pair[0], moAcquire )
    if resultPtr == INVPTR:
      # Help thread with reading
      tracker.helpThread(i, j, mytid)

  # Check if the slowCounter is not zero
  if truthy:
    # Iterate over the slots
    for i in 0..<N:
      # Iterate over the states
      for j in 0..<tracker.hrNum:
        # Check if the result pointer is invalid
        checkPtrs(i, j)

template allocImpl[T; N: static int](tracker: HyalosTracker[T, N], tid: int, body: untyped) {.dirty.} =
  inc tracker.allocCounters[tid]
  if (tracker.allocCounters[tid] mod tracker.epochFreq) == 0:
    # help other threads first
    tracker.helpRead(tid)
    # only after that increment the counter
    discard tracker.epoch.addFetch(1'u64, moAcqRel)
  result = body
  var info: HyalosInfo = getInfo result
  info.birthEpoch = tracker.getEpoch()
  store( info.batchLink, nil, moRelaxed )

proc alloc*[T; N: static int](tracker: HyalosTracker[T, N], tid: int): ptr T =
  allocImpl[T](tracker, tid):
    allocShared(sizeof HyalosInfo + sizeof T)


proc allocAligned*[T; N: static int](tracker: HyalosTracker[T, N], tid: int, align: Natural): ptr T =
  allocImpl[T](tracker, tid):
    let
      allocSize = sizeof HyalosInfo + sizeof T + align - 1 + sizeof uint16
      base      = allocShared allocSize
      offset    = align - ( cast[int](base) and (align - 1) )
    cast[ptr uint16](base +! (offset - sizeof(uint16)))[] = uint16(offset) # ptr arith
    base +! offset # let template read result


proc newHyalosTracker*[T; N: static int](hrNum, epochFreq, emptyFreq: int; collect: bool = false): HyalosTracker[T, N] =
  ## Creates a new instance of the HyalosTracker[N] type.
  ## PARAMETERS:
  ## - hrNum: The number of allowed historical records.
  ## - epochFreq: The frequency at which the epoch counter is updated.
  ## - emptyFreq: The frequency at which empty records are checked and retired.
  ## - collect: A flag indicating whether retired objects should be collected, defaults to false.
  ## RETURNS: An instance of the HyalosTracker[N] type.
  result          = new HyalosTracker[T, N]
  result.hrNum    = hrNum
  result.epochFreq = epochFreq
  result.freq     = emptyFreq
  result.collect  = collect

  # Manual alloc batches and slots aligned to 128 byte boundary as per paper
  var
    batches       = allocAligned0(sizeof(result.batches), 128)
    slots         = allocAligned0(sizeof(result.slots), 128)
    # Manual alloc counters padded to cacheLineSize as per paper
    allocCounters = allocShared0(sizeof(result.allocCounters))

  result.batches        = cast[typeof result.batches](batches)
  result.slots          = cast[typeof result.slots](slots)
  result.allocCounters  = cast[typeof result.allocCounters](allocCounters)

  for i in 0..<N:
    for j in 0..<hrNum+2:
      # Initialize slots
      store( result.slots[i].first[j].list[0], INVPTR, moRelease )
      store( result.slots[i].first[j].pair[1], 0'u, moRelease )
      store( result.slots[i].epoch[j].pair[0], 0'u, moRelease )
      store( result.slots[i].epoch[j].pair[1], 0'u, moRelease )

  # Initialize counters
  store( result.slowCounter, 0, moRelease )
  store( result.epoch, 1, moRelease )
