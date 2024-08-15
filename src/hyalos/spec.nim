import hyalos/memalloc
import pkg/nuclear

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
    data {.align:16.}: Nuclear[hint128]
  UnionValuePair = distinct array[2, uint64]
  UnionDoubleWidth = distinct array[2, uint64]
  UnionLongLong = distinct uint64

proc `==`*(x,y: UnionValuePair): bool {.borrow.}
proc `==`*(x,y: UnionLongLong): bool {.borrow.}
proc `==`*(x,y: UnionDoubleWidth): bool {.borrow.}

template padding(T: typedesc): int =
  ## Convenience internal template
  ## used in array padding
  cacheLineSize - (sizeof(T) mod cacheLineSize)

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
    hyEpoch*: Nuclear[uint64]
    hyPointer*: Nuclear[uint64]
    hyParent*: Nuclear[ptr HyalosInfo]
    _: pointer

  HyalosInfo* = object
    hyUnion1*: UnionLongLong
    batchLink*: Nuclear[ptr HyalosInfo]
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
    epoch: Padded[Nuclear[uint64]]
    slowCounter: Padded[Nuclear[uint64]]
  HyalosTracker*[T; N: static int] = ref HyalosTrackerObj[T, N] # Has to be ordered after Obj or invalid nimskull issue 1413

proc `=destroy`*[T; N: static int](self: var HyalosTrackerObj[T, N]) =
  deallocAligned(self.batches, 16)
  deallocAligned(self.slots, 16)
  deallocShared(self.allocCounters)

func `[]`*[T; N: static int](self: HyalosTracker[T, N]; idx: int): ptr HyalosSlot {.inline.} =
  return self.slots[idx]
func epoch*[T; N](self: HyalosTracker[T, N]): ptr Nuclear[uint64] {.inline.} =
  return addr self.epoch.data
func slowCounter*[T; N: static int](self: HyalosTracker[T, N]): ptr Nuclear[uint64] {.inline.} =
  return addr self.slowCounter.data

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
proc full*(val: var UnionWordPair): ptr Nuclear[hint128] {.inline.} = addr val.data
proc pair*(val: var UnionWordPair): ptr array[2, Nuclear[uint64]] {.inline.} =
  cast[ptr array[2, Nuclear[uint64]]](addr val.data)
proc list*(val: var UnionWordPair): ptr array[2, Nuclear[ptr HyalosInfo]] {.inline.} =
  cast[ptr array[2, Nuclear[ptr HyalosInfo]]](addr val.data)

# Union Handling - HyalosInfo Union 1
proc next*(val: HyalosInfo):  ptr HyalosInfo {.inline.} =
  cast[ptr HyalosInfo](val.hyUnion1)
proc next*(val: ptr HyalosInfo): ptr HyalosInfo {.inline.} =
  cast[ptr HyalosInfo](val[].hyUnion1)
proc `next=`*(val: var HyalosInfo; newVal: ptr HyalosInfo) {.inline.} =
  val.hyUnion1 = cast[UnionLongLong](newVal)
proc slot*(val: HyalosInfo): ptr UnionWordPair {.inline.} =
  cast[ptr UnionWordPair](val.hyUnion1)
proc birthEpoch*(val: HyalosInfo): uint64 {.inline.} =
  cast[uint64](val.hyUnion1)

# Union Handling - HyalosInfo Union 2
proc refs*(val: HyalosInfo): ptr Nuclear[uint64] {.inline.} =
  cast[ptr Nuclear[uint64]](addr val.hyUnion2)
proc batchNext*(val: HyalosInfo): ptr HyalosInfo {.inline.} =
  cast[ptr HyalosInfo](val.hyUnion2)

template doWhile*(cond: bool, body: untyped): untyped {.dirty.} =
  block nene:
    while true:
      body
      if not cond:
        break nene

proc exchange*(p: ptr HyalosInfo; q: pointer, mo: static MemoryOrder = moSeqCst): ptr HyalosInfo {.inline.} =
  return cast[ptr HyalosInfo]( cast[ptr Nuclear[ptr HyalosInfo]](addr p).exchange(cast[typeof result](q), mo) )
