import hyalos/atomics2
import std/atomics
import hyalos/memalloc

const
  cacheLineSize* {.intdefine.} = 128 ## Size of local cache line; TODO automate
  MAX_WFR = 16
  MAX_WFRC = 12
  WFR_PROTECT1 = 1'u64 shl 63
  WFR_PROTECT2 = 1'u64 shl 62
  WFR_INVPTR = cast[pointer](high(uint64))
template isInvalid*[T](this: pointer | ptr T): bool =
  ## Convenience internal template
  cast[pointer](this) == cast[pointer](high(uint64))

template WFR_RNODE*(n: typed): untyped =
  discard

template padding(T: typedesc): int =
  ## Convenience internal template
  ## used in array padding
  if sizeof(T) mod cacheLineSize != 0:
    cacheLineSize - (sizeof(T) mod cacheLineSize)
  else:
    cacheLineSize

type
  Padded*[T] = object
    data*: T
    _: array[padding(T), char]

  UnionWordPair = object
    data {.align:16.}: hint128
  UnionValuePair = distinct array[2, uint64]
  UnionDoubleWidth = distinct array[2, uint64]
  UnionLongLong = distinct uint64

  HyalosState = object
    hyResult: UnionWordPair
    hyEpoch: Atomic[uint64]
    hyPointer: Atomic[uint64]
    hyParent: Atomic[ptr HyalosInfo]
    _: pointer

  HyalosInfo = object
    hyUnion1: UnionLongLong
    batchLink: Atomic[ptr HyalosInfo]
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
    epoch: Padded[Atomic[uint64]]
    slowCounter: Padded[Atomic[uint64]]
  HyalosTracker*[T; N: static int] = ref HyalosTrackerObj[T, N] # Has to be ordered after Obj or invalid nimskull issue 1413

##  To make conversion of the algorithm easier, distinct arrays
##  are used to represent unions with convenience templates
##  converting them to their respective types

func slowCounter*[T, N](self: HyalosTracker[T, N]): Atomic[uint64] {.inline.} =
  self.slowCounter.data
func epoch*[T, N](self: HyalosTracker[T, N]): Atomic[uint64] {.inline.} =
  self.epoch.data

# Union Handling
template full*(val: UnionValuePair): hint128 = cast[hint128](val)
template pair*(val: UnionValuePair): array[2, uint64] = cast[array[2, uint64]](val)
template list*(val: UnionValuePair): array[2, ptr HyalosInfo] = cast[array[2, ptr HyalosInfo]](val)
# Union Handling
template full*(val: UnionWordPair): hint128 = cast[hint128](val.data)
template pair*(val: UnionWordPair): array[2, Atomic[uint64]] = cast[array[2, Atomic[uint64]]](val.data)
template list*(val: UnionWordPair): array[2, Atomic[ptr HyalosInfo]] = cast[array[2, Atomic[ptr HyalosInfo]]](val.data)
# Union Handling - HyalosInfo Union 1
template next*(val: HyalosInfo):  ptr HyalosInfo = cast[ptr HyalosInfo](val.hyUnion1)
template slot*(val: HyalosInfo): ptr UnionWordPair = cast[ptr UnionWordPair](val.hyUnion1)
template birthEpoch*(val: HyalosInfo): uint64 = cast[uint64](val.hyUnion1)
# Union Handling - HyalosInfo Union 2
template refs*(val: HyalosInfo): uint64 = cast[uint64](val.hyUnion2)
template batchNext*(val: HyalosInfo): ptr HyalosInfo = cast[ptr HyalosInfo](val.hyUnion2)



proc `=destroy`[T; N: static int](self: var HyalosTrackerObj[T, N]) =
  deallocAligned(self.batches, 16)
  deallocAligned(self.slots, 16)
  deallocShared(self.allocCounters)



proc newHyalosTracker*[T; N: static int](hrNum, epochFreq, freq: int; collect: bool = false): HyalosTracker[T, N] =
  result = new HyalosTracker[T, N]
  result.hrNum = hrNum
  result.epochFreq = epochFreq
  result.freq = freq
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
      # proc giveMeMyFuckingMutability[T, N](self: HyalosTracker[T, N]): var Atomic[ptr HyalosInfo] =
      #   return self.slots[i].first[j].list[0]
      # giveMeMyFuckingMutability(result).store(WFR_INVPTR, moRelease)
      result.slots[i].first[j].list[0].store(WFR_INVPTR, moRelease)
  result.slowCounter.store(0, moRelease)
  result.epoch.store(1, moRelease)

when isMainModule:
  let trakr = newHyalosTracker[uint, 16](16,0,0)
