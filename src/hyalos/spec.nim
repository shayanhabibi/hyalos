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

type
  UnionWordPair = object
    data {.align:16.}: HyAtomic[hint128]
  UnionValuePair = distinct array[2, uint64]
  UnionDoubleWidth = distinct array[2, uint64]
  UnionLongLong = distinct uint64

proc `==`*(x,y: UnionValuePair): bool {.borrow.}
proc `==`*(x,y: UnionLongLong): bool {.borrow.}
proc `==`*(x,y: UnionDoubleWidth): bool {.borrow.}

template padding(T: typedesc): int =
  ## Convenience internal template
  ## used in array padding
  if sizeof(T) mod cacheLineSize != 0:
    cacheLineSize - (sizeof(T) mod cacheLineSize)
  else:
    cacheLineSize

type
  Padded*[T] = object
    ## Convenience container that pads the internal
    ## value to cacheLineSize
    data: T
    _: array[padding(T), char]

converter unwrap*[T](pad: Padded[T]): T =
  return pad.data

type
  HyalosState* = object
    hyResult*: UnionWordPair
    hyEpoch*: HyAtomic[uint64]
    hyPointer*: HyAtomic[uint64]
    hyParent*: HyAtomic[ptr HyalosInfo]
    _: pointer

  HyalosInfo* = object
    hyUnion1*: UnionLongLong
    batchLink*: HyAtomic[ptr HyalosInfo]
    hyUnion2*: UnionLongLong

  HyalosBatch* = object # Align 16
    first*: ptr HyalosInfo
    last*: ptr HyalosInfo
    counter*: uint64
    listCount*: uint64
    list*:  ptr HyalosInfo
    _: array[cacheLineSize, char]

  HyalosSlot* = object # Align 16
    first*: array[MAX_WFR, UnionWordPair]
    epoch*: array[MAX_WFR, UnionWordPair]
    state*: array[MAX_WFR, HyalosState]
    _: array[cacheLineSize, char]

  HyalosTrackerObj*[T; N: static int] = object
    hrNum*: int
    epochFreq*: int
    freq*: int
    collect*: bool
    slots* {.align: 16.}: ptr array[N, HyalosSlot]
    batches* {.align: 16.}: ptr array[N, HyalosBatch]
    allocCounters*: ptr array[N, Padded[uint64]]
    epoch*: Padded[HyAtomic[uint64]]
    slowCounter*: Padded[HyAtomic[uint64]]
  HyalosTracker*[T; N: static int] = ref HyalosTrackerObj[T, N] # Has to be ordered after Obj or invalid nimskull issue 1413

proc `=destroy`*[T; N: static int](self: var HyalosTrackerObj[T, N]) =
  deallocAligned(self.batches, 16)
  deallocAligned(self.slots, 16)
  deallocShared(self.allocCounters)

##  To make conversion of the algorithm easier, distinct arrays
##  are used to represent unions with convenience templates
##  converting them to their respective types

# Union Handling
proc full*(val: var UnionValuePair): ptr hint128 {.inline.} =
  cast[ptr hint128](addr val)
proc pair*(val: var UnionValuePair): ptr array[2, uint64] {.inline.} =
  cast[ptr array[2, uint64]](addr val)
proc list*(val: var UnionValuePair): ptr array[2, ptr HyalosInfo] {.inline.} =
  cast[ptr array[2, ptr HyalosInfo]](addr val)

# Union Handling
proc full*(val: var UnionWordPair): ptr HyAtomic[hint128] {.inline.} = addr val.data
proc pair*(val: var UnionWordPair): ptr array[2, HyAtomic[uint64]] {.inline.} =
  cast[ptr array[2, HyAtomic[uint64]]](addr val.data)
proc list*(val: var UnionWordPair): ptr array[2, HyAtomic[ptr HyalosInfo]] {.inline.} =
  cast[ptr array[2, HyAtomic[ptr HyalosInfo]]](addr val.data)

# Union Handling - HyalosInfo Union 1
proc next*(val: HyalosInfo):  ptr HyalosInfo {.inline.} =
  cast[ptr HyalosInfo](val.hyUnion1)
proc slot*(val: HyalosInfo): ptr UnionWordPair {.inline.} =
  cast[ptr UnionWordPair](val.hyUnion1)
proc birthEpoch*(val: HyalosInfo): uint64 {.inline.} =
  cast[uint64](val.hyUnion1)

# Union Handling - HyalosInfo Union 2
proc refs*(val: HyalosInfo): HyAtomic[uint64] {.inline.} =
  cast[HyAtomic[uint64]](val.hyUnion2)
proc batchNext*(val: HyalosInfo): ptr HyalosInfo {.inline.} =
  cast[ptr HyalosInfo](val.hyUnion2)

template doWhile*(cond: bool, body: untyped): untyped {.dirty.} =
  block doWhile:
    while True:
      body
      if not cond:
        break doWhile
