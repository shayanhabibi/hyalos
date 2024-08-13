# Define the int128 base type and the helper type hint128 to
# use double word atomic operations on available architectures

# GCC no longer emits cmpxchg16 for double word atomics
# See https://github.com/msys2/MINGW-packages/issues/13831
# See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80878

type
  # Import int128 type from C
  int128 {.importc: "__int128".} = object
    # Any access of the fields will generate invalid
    # C code (the __int128 type has no fields)
    _, DONOTACCESS: uint64

  # Define a helper object to act as a go-between
  hint128* = object
    hi*, lo*: uint

  # Roll my own atomics. I'll only be using them for
  # 64 bit and 128 bit values, so roll over the required
  # types as depending on whats stored in T
  HyAtomic*[T] = object
    when sizeof(T) == 8:
      val: uint64
    elif sizeof(T) == 16:
      val: int128

converter toInt128*(hint: hint128): int128 =
  # hint128 and int128 are the same, there shall
  # be no fear in casting to int128
  cast[int128](hint)
converter toPtrInt128*(at: ptr hint128): ptr int128 =
  cast[ptr int128](at)

when defined(clang) and (defined(amd64) or defined(x86)):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using clang however, will call built in __atomic funcs
  proc atomicAddFetch(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.importc: "__atomic_add_fetch", nodecl.}
  proc atomicSubFetch(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.importc: "__atomic_sub_fetch", nodecl.}
  proc atomicLoadN(p: ptr int128, mo = ATOMIC_SEQ_CST): int128 {.importc: "__atomic_load_n", nodecl.}
  proc atomicCompareExchangeN(p, expected: ptr int128, val: int128,
  weak: bool = false, mo_succ = ATOMIC_SEQ_CST, mo_fail = ATOMIC_RELEASE): bool {.importc: "__atomic_compare_exchange_n", nodecl.}

elif defined(gcc) or defined(amd64):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using GCC, will use deprecated __sync funcs which still emit CMPXCHG16 instructions
  # Note that __sync calls are full synchronisations (more cost)
  proc atomicAddFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_add_and_fetch", nodecl.}
  proc atomicSubFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_sub_and_fetch", nodecl.}
  proc atomicCompareExchangeImpl(p: ptr int128, expected: int128, val: int128): bool {.importc: "__sync_bool_compare_and_swap", nodecl.}
  proc atomicCompareExchangeValImpl(p: ptr int128, expected: int128, val: int128): int128 {.importc: "__sync_val_compare_and_swap", nodecl.}
  # For compatability with atomic calls, convenience functions to discard the memory order
  proc atomicAddFetch(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.inline.} =
    atomicAddFetchImpl p, val
  proc atomicSubFetch(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.inline.} =
    atomicSubFetchImpl p, val
  proc atomicLoadN(p: ptr int128, mo = ATOMIC_SEQ_CST): int128 {.inline.} =
    # Force cmpxchg16; __sync has no load
    atomicAddFetchImpl p, hint128(hi: 0'u, lo: 0'u)
  proc atomicCompareExchangeN(p: ptr int128, expected: ptr int128, val: int128, weak = true, mo_succ = ATOMIC_SEQ_CST, mo_fail = ATOMIC_SEQ_CST): bool {.inline.} =
    atomicCompareExchangeImpl(p, expected[], val)
else:
  # Currently have not ported Hyaline-1 support for single word atomic only architectures
  {.error: "Compilation targets must support DCAS atomics; please submit an issue if this is erroneous".}
  # Because we fail compilation for non-target archs, we can accept int/uint as types since they are 64bit

type
  # Accepted types as arguments
  AtomicIntegers = ptr | pointer | uint64 | int64 | int | uint | int128 | hint128

# Exported atomic ops
proc addFetch*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
      atomicAddFetch(pptr, cast[typeof pptr[]](v), mo)
    )
proc fetchAdd*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicFetchAdd(pptr, cast[typeof pptr[]](v), mo)
  )
proc subFetch*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
      atomicSubFetch(pptr, cast[typeof pptr[]](v), mo)
    )
proc fetchSub*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicFetchSub(pptr, cast[typeof pptr[]](v), mo)
  )
proc store*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = ATOMIC_SEQ_CST) {.inline.} =
  let pptr = addr p.val
  atomicStoreN(pptr, cast[typeof pptr[]](v), mo)
proc load*[T](p: ptr HyAtomic[T] | var HyAtomic[T]; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicLoadN(pptr, mo)
  )
proc exchange*[T](p: ptr HyAtomic[T] | var HyAtomic[T]; v: AtomicIntegers; mo = ATOMIC_SEQ_CST): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicExchangeN(pptr, cast[typeof pptr[]](v), mo)
  )
proc compareExchange*[T](p, e: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; weak = false; mo_succ = ATOMIC_SEQ_CST, mo_fail = ATOMIC_SEQ_CST): bool {.inline.} =
  let pptr = addr p.val
  let eptr = addr e.val
  cast[bool](
    atomicCompareExchangeN(pptr, eptr, cast[typeof pptr[]](v), weak, mo_succ, mo_fail)
  )
