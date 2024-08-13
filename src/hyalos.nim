import hyalos/memalloc
import hyalos/atomics2
import hyalos/spec {.all.}

template rNode[T](p: T): T =
 cast[T](cast[uint64](p) xor 1'u64)
template isRNode[T](p: T): bool =
  cast[uint64](p) and 1'u64


proc helpThread*[T, N](tracker: HyalosTracker[T,N]; tid, index, mytid: int)

proc getRefsNode[T, N](tracker: HyalosTracker[T,N], node: ptr HyalosInfo): ptr HyalosInfo {.inline.} =
  var refs: ptr HyalosInfo = node.batchLink.load(ATOMIC_ACQUIRE)
  if isRNode(refs):
    refs = node
  return refs

proc helpRead[T, N](tracker: HyalosTracker[T,N], mytid: int) =
  template truthy: untyped =
    tracker.slowCounter.load(ATOMIC_ACQUIRE) != 0'u64
  if truthy:
    for i in 0..N-1:
      for j in 0..tracker.hrNum-1:
        let resultPtr = tracker.slots[i].state[j].result.pair[0].load(ATOMIC_ACQUIRE)
        if resultPtr == invPtr[uint64]():
          tracker.helpThread i, j, mytid

proc alloc[T,N](tracker: HyalosTracker[T, N], tid: int): pointer =
  inc tracker.allocCounters[tid]
  if (tracker.allocCounters[tid] mod tracker.epochFreq) == 0:
    tracker.helpRead tid
    discard tracker.epoch.addFetch(1'u64, ATOMIC_ACQ_REL)
  var info = allocShared(sizeof HyalosInfo + sizeof T)
  info.birthEpoch = tracker.getEpoch()
  info.batchLink.store(0, ATOMIC_RELAXED)
  return cast[pointer](info)

# proc decRetired[T,N](tracker: HyalosTracker[T,N])

proc reclaim[T](obj: ptr T) =
  `=destroy`(obj[])
  deallocShared(obj)

proc traverse[T,N](tracker: HyalosTracker[T, N]; list: ptr ptr HyalosInfo; next: ptr HyalosInfo) =
  template truthy: bool {.dirty.} =
    refs.refs.fetchSub(1, ATOMIC_ACQ_REL) == 1'u64
  while true:
    var curr: ptr HyalosInfo
    curr = next
    if isNil curr:
      break
    if isRNode curr:
      var refs: ptr HyalosInfo
      refs = rNode curr
      if truthy:
        refs.next = list[]
        list[] = refs
      break
    next = curr.next.exchange(invPtr[pointer](), ATOMIC_ACQ_REL)
    var refs: ptr HyalosInfo
    refs = curr.batchLink.load(ATOMIC_RELAXED)
    if truthy:
      refs.next = list[]
      list[]

proc traverseCache[T,N](tracker: HyalosTracker[T,N]; batch: ptr HyalosBatch; next: ptr HyalosInfo) {.inline.} =
  if not isNil next:
    if batch.listCount == MAX_WFRC:
      tracker.freeList(batch.list)
      batch.list = nil
      batch.listCount = 0

    traverse addr(batch.list), next
    inc batch.listCount

proc freeList[T, N](tracker: HyalosTracker[T,N], list: ptr HyalosInfo) {.inline.} =
  while not list.isNil():
    var start: ptr HyalosInfo
    start = rNode list.batchLink.load(ATOMIC_RELAXED)
    list = list.next
    doWhile(not start.isNil()):
      var obj: ptr T
      obj = cast[typeof obj](cast[uint64](start) - 1'u64)
      start = start.batchNext
      reclaim obj
      tracker.decRetired(0)

proc getEpoch*[T, N](tracker: HyalosTracker[T,N]): uint64 =
  return tracker.epoch.load(ATOMIC_ACQUIRE)

proc slowPath*[T, N](tracker: HyalosTracker[T, N]; obj: ptr HyAtomic[ptr T]; index, tid: int; node: ptr T): ptr T =
  var birthEpoch: uint64
  var parent: ptr HyalosInfo

  if not isNil node:
    parent = cast[ptr HyalosInfo](inc cast[uint64](node))
    birthEpoch =  parent.birthEpoch

    var info: ptr HyalosInfo
    info = parent.batchLink.load(ATOMIC_ACQUIRE)
    if (not isNil info) and (not isRNode(info)):
      birthEpoch = info.birthEpoch

  var prevEpoch: uint64
  prevEpoch = tracker.slots[tid].epoch[index].pair[0].load(ATOMIC_ACQUIRE)
  discard tracker.slowCounter.fetchAdd(1, ATOMIC_ACQ_REL)
  tracker.slots[tid].state[index].hyPointer.store(cast[uint64](obj), ATOMIC_RELEASE)
  tracker.slots[tid].state[index].hyParent.store(parent, ATOMIC_RELEASE)
  tracker.slots[tid].state[index].hyEpoch.store(birthEpoch, ATOMIC_RELEASE)

  var seqno: uint64
  var lastResult: UnionValuePair
  seqno = tracker.slots[tid].epoch[index].pair[1].load(ATOMIC_ACQUIRE)
  lastResult.pair[0] = invPtr[uint64]()
  lastResult.pair[1] = seqno
  ## TODO DCAS STORE
  ## dcas_store(slots[tid].state[index].result.full, last_result.full, std::memory_order_release);

  var old, value: UnionValuePair
  var resultEpoch, resultPtr, expseqno: uint64
  var first: ptr HyalosInfo
  block done: #`goto done` == break done
    doWhile(resultPtr == invPtr[uint64]()):
      var objPtr: ptr T = if obj: obj.load(ATOMIC_ACQUIRE) else: cast[ptr T](nil)
      var currEpoch: uint64 = tracker.getEpoch()
      if currEpoch == prevEpoch:
        lastResult.pair[0] = invPtr[uint64]()
        lastResult.pair[1] = seqno
        value.pair[0] = 0'u
        value.pair[1] = 0'u
        ## TODO DCAS COMP XCHG
        ## if (dcas_compare_exchange_strong(slots[tid].state[index].result.full, last_result.full, value.full, std::memory_order_acq_rel, std::memory_order_acquire)) {
        ## 	slots[tid].epoch[index].pair[1].store(seqno + 2, std::memory_order_release);
        ## 	slots[tid].first[index].pair[1].store(seqno + 2, std::memory_order_release);
        ## 	slow_counter.ui.fetch_sub(1, std::memory_order_acq_rel);
        ##  return ptr;
        ## }
      if not tracker.slots[tid].first[index].list[0].load(ATOMIC_ACQUIRE).isNil():
        first = tracker.slots[tid].first[index].list[0].exchange(nil, ATOMIC_ACQ_REL)
        if tracker.slots[tid].first[index].pair[1].load(ATOMIC_ACQUIRE) != seqno:
          break done
        if first != invPtr[uint64]():
          traverseCache tracker, addr batches[tid], first
        currEpoch = getEpoch tracker

      first = nil
      old.pair[0] = prevEpoch
      old.pair[1] = seqno
      value.pair[0] = currEpoch
      value.pair[1] = seqno
      ## TODO DCAS XCHG
      ## dcas_compare_exchange_strong(slots[tid].epoch[index].full, old.full, value.full, std::memory_order_seq_cst, std::memory_order_acquire)
      prevEpoch = currEpoch
      resultPtr = tracker.slots[tid].state[index].hyResult.pair[0].load(ATOMIC_ACQUIRE)

    expseqno = seqno
    ## TODO CAS
    value.list[0] = nil
    value.pair[1] = seqno + 1
    old.pair[1] = tracker.slots[tid].first[index].pair[1].load(ATOMIC_ACQUIRE)
    old.list[1] = tracker.slots[tid].first[index].list[0].load(ATOMIC_ACQUIRE)
    while old.pair[1] == seqno:
      ## TODO, DCAS
      break

  inc seqno

  tracker.slots[tid].epoch[index].pair[1].store(seqno + 1'u, ATOMIC_RELEASE)
  resultEpoch = tracker.slots[tid].state[index].hyResult.pair[1].load(ATOMIC_ACQUIRE)
  tracker.slots[tid].epoch[index].pair[0].store(resultEpoch, ATOMIC_RELEASE)

  tracker.slots[tid].first[index].pair[1].store(seqno + 1, ATOMIC_RELEASE)
  resultPtr = tracker.slots[tid].state[index].hyResult.pair[0].load(ATOMIC_ACQUIRE) and 0xFFFFFFFFFFFFFFFC

  var ptrNode: ptr HyalosInfo = cast[ptr HyalosInfo](resultPtr +% sizeof(T))

  if (resultPtr != 0'u) and (not ptrNode.batchLink.load(ATOMIC_ACQUIRE).isNil()):
    var refs: ptr HyalosInfo = tracker.getRefsNode ptrNode
    discard refs.refs.fetchAdd(1'u, ATOMIC_ACQ_REL)
    if first != invPtr[uint64]():
      tracker.traverseCache(addr tracker.batches[tid], first)
    first = tracker.slots[tid].first[index].list[0].exchange(rNode(refs), ATOMIC_ACQ_REL)

  discard tracker.slowCounter.fetchSub(1'u, ATOMIC_ACQ_REL)

  if first != invPtr[uint64]():
    tracker.traverseCache(addr tracker.batches[tid], first)

  if (not parent.isNil) and (not parent.batchLink.load(ATOMIC_ACQUIRE).isNil):
    var refs: ptr HyalosInfo = tracker.getRefsNode(parent)
    discard refs.refs.fetchAdd(WFR_PROTECT2, ATOMIC_ACQ_REL)
    var adjs = not WFR_PROTECT2

    for i in 0..<N:
      var exp: ptr HyalosInfo = parent
      ## CAS TODO

    discard refs.refs.fetchAdd(adjs, ATOMIC_ACQ_REL)

  return cast[ptr T](resultPtr)

proc clearAll[T, N](tracker: HyalosTracker[T,N]; tid: int) =
  var first: array[MAX_WFR, ptr HyalosInfo]
  for i in 0..<tracker.hrNum:
    first[i] = tracker.slots[tid].first[i].list[0].exchange(invPtr[uint64](), ATOMIC_ACQ_REL)
  for i in 0..<tracker.hrNum:
    if first[i] != invPtr[uint64]():
      tracker.traverse(addr tracker.batches[tid].list, first[i])
  tracker.freeList(tracker.batches[tid].list)
  tracker.batches[tid].list = nil
  tracker.batches[tid].listCount = 0

proc tryRetire[T, N](tracker: HyalosTracker[T, N]; batch: ptr HyalosBatch) =
  var
    curr: ptr HyalosInfo = batch.first
    refs: ptr HyalosInfo = batch.last
    minEpoch: uint64 = refs.birthEpoch
    last: ptr HyalosInfo = curr

  for i in 0..<N:
    var j = 0
    while j < tracker.hrNum:
      inc j

      var first: ptr HyalosInfo = tracker.slots[i].first[j].list[0].load(ATOMIC_ACQUIRE)
      if first == invPtr[uint64]():
        continue
      if tracker.slots[i].first[j].pair[1].load(ATOMIC_ACQUIRE) and 1'u:
        continue
      var epoch: uint64 = tracker.slots[i].epoch[j].pair[0].load(ATOMIC_ACQUIRE)
      if epoch < minEpoch:
        continue
      if tracker.slots[i].epoch[j].pair[1].load(ATOMIC_ACQUIRE) and 1'u:
        continue
      if last == refs:
        return
      last.slot = addr tracker.slots[i].first[j]
      last = last.batchNext
    while j < tracker.hrNum + 2:
      inc j
      var first: ptr HyalosInfo = tracker.slots[i].first[j].list[0].load(ATOMIC_ACQUIRE)
      if first == invPtr[uint64]():
        continue
      var epoch: uint64 = tracker.slots[i].epoch[j].pair[0].load(ATOMIC_ACQUIRE)
      if epoch < minEpoch:
        continue
      if last == refs:
        return
      last.slot = addr tracker.slots[i].first[j]
      last = last.batchNext
  var adjs = not WFR_PROTECT1
  while curr != last:
    curr = curr.batchNext

    var slotFirst: ptr UnionWordPair = curr.slot
    var slotEpoch: ptr UnionWordPair = cast[ptr UnionWordPair](cast[uint64](slotFirst) + MAX_WFR)
    curr.next.store(nil, ATOMIC_RELAXED)
    if slotFirst.list[0].load(ATOMIC_ACQUIRE) == invPtr[uint64]():
      continue
    var epoch: uint64 = slotEpoch.pair[0].load(ATOMIC_ACQUIRE)
    if epoch < minEpoch:
      continue
    var prev: ptr HyalosInfo = slotFirst.list[0].exchange(curr, ATOMIC_ACQ_REL)
    if not prev.isNil:
      if prev == invPtr[ptr HyalosInfo]():
        var exp: ptr HyalosInfo = curr
        ## TODO: CAS
      else:
        var exp: ptr HyalosInfo = nil
        ## TODO: CAS
    inc adjs
  if refs.refs.fetchAdd(adjs, ATOMIC_ACQ_REL) == -adjs:
    refs.next = nil
    tracker.freeList(refs)
  batch.first = nil
  batch.counter = 0


proc retire[T, N](tracker: HyalosTracker[T, N]; obj: ptr T; tid: int) =
  if obj.isNil: return
  var node: ptr HyalosInfo = cast[ptr HyalosInfo](cast[uint64](obj) + 1)
  if not tracker.batches[tid].first:
    tracker.batches[tid].last = node
    node.refs.store(WRF_PROTECT1, ATOMIC_RELAXED)
  else:
    if (tracker.batches[tid].last.birthEpoch > node.birthEpoch):
      tracker.batches[tid].last.birthEpoch = node.birthEpoch
    node.batchLink.store(tracker.batches[tid].last, ATOMIC_SEQ_CST)
    node.batchNext = tracker.batches[tid].first

  tracker.batches[tid].first = node
  inc tracker.batches[tid].counter
  if tracker.collect and (tracker.batches[tid].counter mod tracker.freq == 0):
    tracker.batches[tid].last.batchLink.store(rNode(node), ATOMIC_SEQ_CST)
    tracker.tryRetire(addr tracker.batches[tid])

proc collecting[T, N](tracker: HyalosTracker[T, N]): bool = tracker.collect


proc reserveSlot[T, N](tracker: HyalosTracker[T, N]; obj: ptr T; index, tid: int; node: ptr T) =
  var prevEpoch: uint64
  var attempts: int

  prevEpoch = tracker.slots[tid].epoch[index].pair[0].load(ATOMIC_ACQUIRE)
  attempts = 16
  doWhile(attempts != 0):
    # var objPtr: ptr T
    # objPtr = obj.load(ATOMIC_ACQUIRE)

    var currEpoch: uint64
    currEpoch = getEpoch(tracker)
    if currEpoch == prevEpoch:
      # return objPtr
      return
    else:
      prevEpoch = tracker.doUpdate(currEpoch, index, tid)

    dec attempts

  # return tracker.slowPath(obj, index, tid, node)
  return tracker.slowPath(cast[ptr HyAtomic[ptr T]](nil), index, tid, node)


proc read[T,N](tracker: HyalosTracker[T,N]; obj: ptr HyAtomic[ptr T]; index, tid: int; node: ptr T): ptr T =
  var prevEpoch: uint64
  var attempts: int

  prevEpoch = tracker.slots[tid].epoch[index].pair[0].load(ATOMIC_ACQUIRE)
  attempts = 16
  doWhile(attempts != 0):
    var objPtr: ptr T
    var currEpoch: uint64

    objPtr = obj.load(ATOMIC_ACQUIRE)
    currEpoch = getEpoch(tracker)
    if currEpoch == prevEpoch:
      return objPtr
    else:
      prevEpoch = tracker.doUpdate(currEpoch, index, tid)
    dec attempts
  return tracker.slowPath(obj, index, tid, node)



proc doUpdate[T, N](tracker: HyalosTracker[T, N]; currEpoch: var uint64; index, tid: int): uint64 =
  template truthy: untyped =
    not isNil load(tracker.slots[tid].first[index].list[0], ATOMIC_ACQUIRE)
  if truthy:
    let first = tracker.slots[tid].first[index].list[0].exchange(invPtr[uint64](), ATOMIC_ACQ_REL)
    if first != invPtr[ptr HyalosInfo]():
      ## Traverse cache
    tracker.slots[tid].first[index].list[0].store(0'u, ATOMIC_SEQ_CST)
    currEpoch = tracker.getEpoch()
  tracker.slots[tid].epoch[index].pair[0].store(currEpoch, ATOMIC_SEQ_CST)
  return currEpoch

proc helpThread*[T, N](tracker: HyalosTracker[T,N]; tid, index, mytid: int) {.inline.} =
  var last_result: UnionValuePair
  last_result = cast[typeof last_result](load(tracker.slots[tid].state[index].hyResult.full, ATOMIC_ACQUIRE))
  if last_result.pair[0] != invPtr[uint64]():
    return
  let birthEpoch = load(tracker.slots[tid].state[index].hyEpoch, ATOMIC_ACQUIRE)
  let parent = load(tracker.slots[tid].state[index].hyParent, ATOMIC_ACQUIRE)
  if not isNil parent:
    tracker.slots[mytid].first[tracker.hrNum].list[0].store(0'u, ATOMIC_SEQ_CST)
    tracker.slots[mytid].epoch[tracker.hrNum].pair[0].store(birthEpoch, ATOMIC_SEQ_CST)
  tracker.slots[mytid].state[tracker.hrNum].hyParent.store(parent, ATOMIC_SEQ_CST)
  let obj: ptr HyAtomic[ptr T] = cast[ptr HyAtomic[ptr T]](
    tracker.slots[tid].state[index].hyPointer.load(ATOMIC_ACQUIRE)
  )
  var seqno: uint64 = tracker.slots[tid].epoch[index].pair[1].load(ATOMIC_ACQUIRE)
  if last_result.pair[1] == seqno:
    var prevEpoch = tracker.getEpoch()

    template truthy: untyped =
      last_result.full[] == load(tracker.slots[tid].state[index].hyResult.full, ATOMIC_ACQUIRE)

    while truthy:
      prevEpoch = tracker.doUpdate(prevEpoch, tracker.hrNum + 1, mytid)
      var objPtr: ptr T = if not isNil obj: obj.load(ATOMIC_ACQUIRE) else: cast[ptr T](nil) # Can be a nullptr
      var currEpoch = getEpoch tracker

proc newHyalosTracker*[T; N: static int](hrNum, epochFreq, emptyFreq: int; collect: bool = false): HyalosTracker[T, N] =
  result = new HyalosTracker[T, N]
  result.hrNum = hrNum
  result.epochFreq = epochFreq
  result.freq = emptyFreq
  result.collect = collect
  # Manual alloc batches and slots aligned to 16 as per paper
  var batches = allocAligned0(sizeof(result.batches), 16)
  var slots = allocAligned0(sizeof(result.batches), 16)
  # Manual alloc counters padded to cacheLineSize as per paper
  var allocCounters = allocShared0(sizeof(result.allocCounters))

  result.batches = cast[ptr array[N, HyalosBatch]](batches)
  result.slots = cast[ptr array[N, HyalosSlot]](slots)
  result.allocCounters = cast[typeof result.allocCounters](allocCounters)
  for i in 0..N-1:
    for j in 0..hrNum+1:
      result.slots[i].first[j].list[0].store(invPtr[uint64](), ATOMIC_RELEASE)
  result.slowCounter.store(0, ATOMIC_RELEASE)
  result.epoch.store(1, ATOMIC_RELEASE)

proc newHyalosTracker*[T; N: static int](emptyFreq: int): HyalosTracker[T, N] =
  return newHyalosTracker[T, N](0, emptyFreq = emptyFreq, true)


when isMainModule:
  let trakr = newHyalosTracker[uint, 14](12,0,0)
  helpThread(trakr, 1, 1, 2)
  echo sizeof trakr[]
