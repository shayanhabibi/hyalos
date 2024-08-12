import hyalos/atomics2
import hyalos/memalloc

const
  cacheLineSize* {.intdefine.} = 128 ## Size of local cache line; TODO automate
  MAX_WFR = 16
  MAX_WFRC = 12
  WFR_PROTECT1 = 1'u64 shl 63
  WFR_PROTECT2 = 1'u64 shl 62

func invPtr*[T](): T {.inline.} =
  cast[T](high(uint64))

template WFR_RNODE*(n: typed): untyped =
  discard

type
  UnionWordPair = object
    data {.align:16.}: hint128
  UnionValuePair = distinct array[2, uint64]
  UnionDoubleWidth = distinct array[2, uint64]
  UnionLongLong = distinct uint64

proc load(at: ptr UnionWordPair | var UnionWordPair; mo = ATOMIC_SEQ_CST): UnionValuePair =
  when at is not ptr UnionWordPair:
    let loadPtr = cast[ptr int128](addr at.data)
  else:
    let loadPtr = cast[ptr int128](addr at[].data)
  cast[UnionValuePair](atomicLoad(loadPtr, mo))

template padding(T: typedesc): int =
  ## Convenience internal template
  ## used in array padding
  if sizeof(T) mod cacheLineSize != 0:
    cacheLineSize - (sizeof(T) mod cacheLineSize)
  else:
    cacheLineSize

type
  Padded*[T] = object
    data: T
    _: array[padding(T), char]

converter unwrap*[T](pad: Padded[T]): T =
  return pad.data

type
  HyalosState = object
    hyResult: UnionWordPair
    hyEpoch: HyAtomic[uint64]
    hyPointer: HyAtomic[uint64]
    hyParent: HyAtomic[ptr HyalosInfo]
    _: pointer

  HyalosInfo = object
    hyUnion1: UnionLongLong
    batchLink: HyAtomic[ptr HyalosInfo]
    hyUnion2: UnionLongLong

  HyalosBatch = object # Align 16
    first: ptr HyalosInfo
    last: ptr HyalosInfo
    counter: uint64
    listCount: uint64
    list:  ptr HyalosInfo
    _: array[cacheLineSize, char]

  HyalosSlot = object # Align 16
    first: array[MAX_WFR, UnionWordPair]
    epoch: array[MAX_WFR, UnionWordPair]
    state: array[MAX_WFR, HyalosState]
    _: array[cacheLineSize, char]

  HyalosTrackerObj*[T; N: static int] = object
    hrNum: int
    epochFreq: int
    freq: int
    collect: bool
    slots {.align: 16.}: ptr array[N, HyalosSlot]
    batches {.align: 16.}: ptr array[N, HyalosBatch]
    allocCounters: ptr array[N, Padded[uint64]]
    epoch: Padded[HyAtomic[uint64]]
    slowCounter: Padded[HyAtomic[uint64]]
  HyalosTracker*[T; N: static int] = ref HyalosTrackerObj[T, N] # Has to be ordered after Obj or invalid nimskull issue 1413

##  To make conversion of the algorithm easier, distinct arrays
##  are used to represent unions with convenience templates
##  converting them to their respective types

func slowCounter*[T, N](self: HyalosTracker[T, N]): HyAtomic[uint64] {.inline.} =
  self.slowCounter.data
func epoch*[T, N](self: HyalosTracker[T, N]): HyAtomic[uint64] {.inline.} =
  self.epoch.data

# Union Handling
proc full*(val: var UnionValuePair): ptr hint128 {.inline.} =
  cast[ptr hint128](addr val)
proc pair*(val: var UnionValuePair): ptr array[2, uint64] {.inline.} =
  cast[ptr array[2, uint64]](addr val)
proc list*(val: var UnionValuePair): ptr array[2, ptr HyalosInfo] {.inline.} =
  cast[ptr array[2, ptr HyalosInfo]](addr val)

# Union Handling
proc full*(val: var UnionWordPair): ptr hint128 {.inline.} =
  cast[ptr hint128](addr val.data)
proc pair*(val: var UnionWordPair): ptr array[2, HyAtomic[uint64]] {.inline.} =
  cast[ptr array[2, HyAtomic[uint64]]](addr val.data)
proc list*(val: var UnionWordPair): ptr array[2, HyAtomic[ptr HyalosInfo]] {.inline.} =
  cast[ptr array[2, HyAtomic[ptr HyalosInfo]]](addr val.data)

# Union Handling - HyalosInfo Union 1
proc next*(val: HyalosInfo):  ptr HyalosInfo =
  cast[ptr HyalosInfo](val.hyUnion1)
proc slot*(val: HyalosInfo): ptr UnionWordPair =
  cast[ptr UnionWordPair](val.hyUnion1)
proc birthEpoch*(val: HyalosInfo): uint64 =
  cast[uint64](val.hyUnion1)

# Union Handling - HyalosInfo Union 2
proc refs*(val: HyalosInfo): uint64 =
  cast[uint64](val.hyUnion2)
proc batchNext*(val: HyalosInfo): ptr HyalosInfo =
  cast[ptr HyalosInfo](val.hyUnion2)



proc `=destroy`[T; N: static int](self: var HyalosTrackerObj[T, N]) =
  deallocAligned(self.batches, 16)
  deallocAligned(self.slots, 16)
  deallocShared(self.allocCounters)



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

proc getEpoch*[T, N](tracker: HyalosTracker[T,N]): uint64 =
  return tracker.epoch.load(ATOMIC_ACQUIRE)

proc doUpdate[T, N](tracker: HyalosTracker[T, N]; currEpoch: var uint64; index, tid: int): uint64 =
  template truthy: untyped =
    not isNil load(tracker.slots[tid].first[index].list[0], ATOMIC_ACQUIRE)
  if truthy:
    let first = load(tracker.slots[tid].first[index].list[0].exchange(invPtr[uint64](), ATOMIC_ACQ_REL))
    if first != invPtr[uint64]():
      ## Traverse cache
    tracker.slots[tid].first[index].list[0].store(0'u, ATOMIC_SEQ_CST)
    currEpoch = tracker.getEpoch()
  tracker.slots[tid].epoch[index].pair[0].store(currEpoch, ATOMIC_SEQ_CST)
  return currEpoch

proc helpThread*[T, N](tracker: HyalosTracker[T,N]; tid, index, mytid: int) {.inline.} =
  var last_result: UnionValuePair
  last_result = load(tracker.slots[tid].state[index].hyResult, ATOMIC_ACQUIRE)
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

  # while truthy:
  #   prevEpoch = doUpdate

when isMainModule:
  let trakr = newHyalosTracker[uint, 14](12,0,0)
  helpThread(trakr, 1, 1, 2)
