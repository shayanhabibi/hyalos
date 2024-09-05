import hyalos/memalloc
import pkg/nuclear
import pkg/parith/math

const
  cacheLineSize* {.intdefine.} = 128 ## Size of local cache line; TODO automate
  MAX_WFR* = 16
  MAX_WFRC* = 12
  WFR_PROTECT1* = 1'u64 shl 63
  WFR_PROTECT2* = 1'u64 shl 62
  INVPTR* = high(uint64)

func invPtr*[T](): T {.inline.} =
  cast[T](high(uint64))

func invPtr*(T: typedesc): T {.inline.} =
  cast[T](high(uint64))


template padding(T: typedesc): int =
  ## Convenience internal template
  ## used in array padding
  cacheLineSize - (sizeof(T) mod cacheLineSize)

template hyalosBatchPadding: int =
  ## Convenience internal template
  ## used in array padding
  cacheLineSize - (sizeof(HyalosInfo) * 5 mod cacheLineSize)

template hyalosSlotPadding: int =
  cacheLineSize - ((MAX_WFR * sizeof(HyalosState) + MAX_WFR * sizeof(UnionWordPair) * 2) mod cacheLineSize)

type
  Padded*[T] = object
    ## Convenience container that pads the internal
    ## value to cacheLineSize
    data: T
    _: array[padding(T), char]

converter unwrap*[T](pad: Padded[T]): T =
  return pad.data

type
  UnionWordPair = hint128
  UnionValuePair = array[2, uint64]
  UnionLongLong = uint64

  HyalosInfo* = ptr HyalosInfoObj
  HyalosInfoObj* = object
    hyUnion1*: UnionLongLong
    batchLink*: Nuclear[HyalosInfo]
    hyUnion2*: UnionLongLong

  HyalosState* = object
    hyResult* {.align: 16.}: UnionWordPair
    hyEpoch*: Nuclear[uint64]
    hyPointer*: Nuclear[uint64]
    hyParent*: Nuclear[HyalosInfo]
    _: pointer

  HyalosBatch* = object # Align 128
    first*: HyalosInfo
    last*: HyalosInfo
    counter*: uint64
    listCount*: uint64
    list*:  HyalosInfo
    _: array[hyalosBatchPadding, char]

  HyalosBatchesArray*[N: static int] = array[N, HyalosBatch]
  HyalosBatches*[N: static int] = ptr HyalosBatchesArray[N]

  HyalosSlot* = object # Align 16
    first*: array[MAX_WFR, UnionWordPair]
    epoch*: array[MAX_WFR, UnionWordPair]
    state*: array[MAX_WFR, HyalosState]
    _: array[hyalosSlotPadding, char]

  HyalosSlotsArray*[N: static int] = array[N, HyalosSlot]
  HyalosSlots*[N: static int] = ptr HyalosSlotsArray[N]

  HyalosCountersArray*[N: static int] = array[N, Padded[uint64]]
  HyalosCounters*[N: static int] = ptr HyalosCountersArray[N]

  HyalosTrackerObj*[T; N: static int] = object
    hrNum*: int
    epochFreq*: int
    freq*: int
    collect*: bool
    slots*: HyalosSlots[N] # {.align: 128.} # Manual alloc
    batches*: HyalosBatches[N] # {.align: 128.} # Manual alloc
    allocCounters*: HyalosCounters[N] # Manual alloc
    epoch: Padded[Nuclear[uint64]]
    slowCounter: Padded[Nuclear[uint64]]
  HyalosTracker*[T; N: static int] = ref HyalosTrackerObj[T, N] # Has to be ordered after Obj or invalid nimskull issue 1413

proc `=destroy`*[T; N: static int](self: var HyalosTrackerObj[T, N]) =
  deallocAligned(self.batches, 128)
  deallocAligned(self.slots, 128)
  deallocShared(self.allocCounters)

proc dealloc[T; N: static int](self: HyalosTracker[T, N]) =
  deallocAligned(self.batches, 128)
  deallocAligned(self.slots, 128)
  deallocShared(self.allocCounters)
  deallocShared(self)

func `[]`*[T; N](self: HyalosTracker[T, N]; idx: int): var HyalosSlot {.inline.} =
  return self.slots[idx]
func epoch*[T; N](self: HyalosTracker[T, N]): var Nuclear[uint64] {.inline.} =
  return self.epoch.data
func slowCounter*[T; N](self: HyalosTracker[T, N]): var Nuclear[uint64] {.inline.} =
  return self.slowCounter.data

##  To make conversion of the algorithm easier, distinct arrays
##  are used to represent unions with convenience templates
##  converting them to their respective types

# Union Handling
template full*(val: var UnionValuePair): untyped = val
template pair*(val: var UnionValuePair): ptr array[2, uint64] = cast[ptr array[2, uint64]](addr val)
template list*(val: var UnionValuePair): ptr array[2, HyalosInfo] = cast[ptr array[2, HyalosInfo]](addr val)

# Union Handling
template full*(val: var UnionWordPair): Nuclear[hint128] = cast[ptr Nuclear[hint128]](addr val)[]
template pair*(val: var UnionWordPair): ptr array[2, Nuclear[uint64]] = cast[ptr array[2, Nuclear[uint64]]](addr val)
template list*(val: var UnionWordPair): ptr array[2, Nuclear[HyalosInfo]] = cast[ptr array[2, Nuclear[HyalosInfo]]](addr val)

# Union Handling - HyalosInfo Union 1
template next*(val: HyalosInfo): ptr Nuclear[HyalosInfo] = cast[ptr Nuclear[HyalosInfo]](addr val.hyUnion1)
template slot*(val: HyalosInfo): ptr UnionWordPair = cast[ptr UnionWordPair](val.hyUnion1)
template birthEpoch*(val: HyalosInfo): untyped = val.hyUnion1

# Union Handling - HyalosInfo Union 2
template refs*(val: HyalosInfo): Nuclear[uint64] = cast[ptr Nuclear[uint64]](addr val.hyUnion2)[]
template batchNext*(val: HyalosInfo): HyalosInfo = cast[HyalosInfo](val.hyUnion2)

template `==`*(x: UnionValuePair, y: hint128): bool = cast[hint128](x) == y

template mut*[T](val: T): var T = cast[var T](addr val)

template doWhile*(cond: bool, body: untyped): untyped {.dirty.} =
  block nene:
    while true:
      body
      if not cond:
        break nene

template getInfo*[T](p: ptr T): HyalosInfo =
  ## Navigate to the Info suffix from a pointer
  cast[HyalosInfo](p +! 1)

template getNode*[T](info: HyalosInfo): ptr T =
  ## Navigate to the Node prefix from an Info
  cast[ptr T](info) -! 1

# proc alloc[N](hyalos: HyalosTracker[N]; size: Natural; align: static Natural = MemAlign): pointer =
#   ## Allocate memory that is tracked by the HyalosTracker
#   ## Pass optional alignment
#   when align <= MemAlign:
#     return allocShared(size + sizeof(HyalosInfoObj))
#   else:
#     # Allocate (size + sizeof(HyalosInfoObj) + align - 1) necessary
#     # for alignment, plus 2 bytes to store offset
#     let base = allocShared(size + sizeof(HyalosInfoObj) + align - 1 + sizeof(uint16))
#     # Memory layout:
#     # padding + user_data + hyalosinfo + offset (2 bytes)
#     let offset = align - (cast[int](base) and (align - 1))
#     # calculates the offset for the userdata to be aligned
#     # to the next multiple of align
#     cast[ptr uint16](base +! size +! sizeof(HyalosInfoObj) +! (offset - sizeof(uint16)))[] = uint16(offset + size + sizeof(HyalosInfoObj))
#     # store the sum of the offset, size of userdata and size of hyalosinfo
#     # this allows deallocs of aligned memory to hit the correct block
#     return base +! offset
