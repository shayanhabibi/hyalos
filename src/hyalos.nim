import hyalos/memalloc
import hyalos/atomics2
import hyalos/spec {.all.}

proc getEpoch*[T, N](tracker: HyalosTracker[T,N]): uint64 =
  return tracker.epoch.load(ATOMIC_ACQUIRE)

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
  last_result = load(tracker.slots[tid].state[index].hyResult.full, ATOMIC_ACQUIRE)
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
      last_result.full == load(tracker.slots[tid].state[index].hyResult.full, ATOMIC_ACQUIRE)

    while truthy:
      prevEpoch = tracker.doUpdate(prevEpoch, tracker.hrNum + 1, mytid)
      let objPtr: ptr T = obj.load(ATOMIC_ACQUIRE) # Can be a nullptr
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
