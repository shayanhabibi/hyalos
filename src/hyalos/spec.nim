import std/atomics
import hyalos/atomics2
import hyalos/memalloc

const
  hyalosBatchSize* {.intdefine.} = 8
  cacheLineSize* {.intdefine.} = 128
  MAX_WFR = 16
  MAX_WFRC = 12
  WFR_PROTECT1 = 1'u64 shl 63
  WFR_PROTECT2 = 1'u64 shl 62

template isInvalid*[T](this: pointer | ptr T): bool =
  cast[pointer](this) == cast[pointer](high(uint64))

template WFR_RNODE*(n: typed): untyped =
  discard

template padding(T: typedesc): int =
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
    hyParent: Atomic[uint64]
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

  HyalosTracker*[T; N: int] = object
    taskNum: int
    hrNum: int
    epochFreq: int
    freq: int
    collect: bool
    slots {.align: 16.}: ptr array[N, HyalosSlot]
    batches {.align: 16.}: ptr array[N, HyalosBatch]
    allocCounters: ptr array[N, Padded[uint64]]
    epoch: Atomic[uint64]
    slowCounter: Atomic[uint64]


##  To make conversion of the algorithm easier, distinct arrays
##  are used to represent unions with convenience templates
##  converting them to their respective types

# Union Handling
template full*(val: UnionValuePair): hint128 = cast[hint128](val)
template pair*[I](val: UnionValuePair): uint64 = cast[array[2, uint64]](val)[I]
template list*[I](val: UnionValuePair): ptr HyalosInfo = cast[array[2, ptr HyalosInfo]](val)[I]
# Union Handling
template full*(val: UnionWordPair): hint128 = cast[hint128](val.data)
template pair*[I](val: UnionWordPair): Atomic[uint64] = cast[array[2, Atomic[uint64]]](val.data)[I]
template list*[I](val: UnionWordPair): Atomic[ptr HyalosInfo] = cast[array[2, Atomic[ptr HyalosInfo]]](val.data[I])
# Union Handling - HyalosInfo Union 1
template next*(val: HyalosInfo):  ptr HyalosInfo = cast[ptr HyalosInfo](val.hyUnion1)
template slot*(val: HyalosInfo): ptr UnionWordPair = cast[ptr UnionWordPair](val.hyUnion1)
template birthEpoch*(val: HyalosInfo): uint64 = cast[uint64](val.hyUnion1)
# Union Handling - HyalosInfo Union 2
template refs*(val: HyalosInfo): uint64 = cast[uint64](val.hyUnion2)
template batchNext*(val: HyalosInfo): ptr HyalosInfo = cast[ptr HyalosInfo](val.hyUnion2)



func `=destroy`[T, N](self: var HyalosTracker[T, N]) =
  deallocAligned(self.batches, 16)
  deallocAligned(self.slots, 16)
  deallocShared(self.allocCounters)



func newHyalosTracker*[T, N](taskNum = N; hrNum, epochFreq, freq: int = 0; collect: bool = false): HyalosTracker =
  result = new HyalosTracker[T, N]
  result.taskNum = N # Can be removed, static
  result.hrNum = hrNum
  result.epochFreq = epochFreq
  result.freq = freq
  result.collect = collect
  # Manual alloc batches and slots aligned to 16 as per paper
  let batches = allocAligned0(sizeof(result.batches), 16)
  let slots = allocAligned0(sizeof(result.batches), 16)
  # Manual alloc counters padded to cacheLineSize as per paper
  let allocCounters = allocShared0(sizeof(result.allocCounters))

  result.batches = cast[ptr HyalosBatch](batches)
  result.slots = cast[ptr HyalosSlot](slots)
  result.allocCounters = allocCounters
