# Define the int128 base type and the helper type hint128 to
# use double word atomic operations on available architectures

# GCC no longer emits cmpxchg16 for double word atomics
# See https://github.com/msys2/MINGW-packages/issues/13831
# See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80878

type
  # Import int128 type from C
  int128 {.importc: "__int128".} = distinct array[2, int64]
    # Any access of the fields will generate invalid
    # C code (the __int128 type has no fields)

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

  MemOrder = distinct cint

  HyAtomTypes* = int128|int|uint|int64|uint64|pointer|ptr


var
  Relaxed {.importc: "__ATOMIC_RELAXED", nodecl, used.}: MemOrder
  Consume {.importc: "__ATOMIC_CONSUME", nodecl, used.}: MemOrder
  Acquire {.importc: "__ATOMIC_ACQUIRE", nodecl, used.}: MemOrder
  Release {.importc: "__ATOMIC_RELEASE", nodecl, used.}: MemOrder
  AcqRel {.importc: "__ATOMIC_ACQ_REL", nodecl, used.}: MemOrder
  SeqCst {.importc: "__ATOMIC_SEQ_CST", nodecl, used.}: MemOrder

converter toInt128*(hint: hint128): int128 =
  # hint128 and int128 are the same, there shall
  # be no fear in casting to int128
  cast[int128](hint)
converter toPtrInt128*(at: ptr hint128): ptr int128 =
  cast[ptr int128](at)

when defined(clang) and (defined(amd64) or defined(x86)):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using clang however, will call built in __atomic funcs
  proc addFetchImpl[T: HyAtomTypes](p: ptr T, val: T, mo = SeqCst): T {.importc: "__atomic_add_fetch", nodecl.}
  proc subFetchImpl[T: HyAtomTypes](p: ptr T, val: T, mo = SeqCst): T {.importc: "__atomic_sub_fetch", nodecl.}
  proc loadNImpl[T: HyAtomTypes](p: ptr T, mo = SeqCst): T {.importc: "__atomic_load_n", nodecl.}
  proc compareExchangeNImpl[T: HyAtomTypes](p, expected: ptr T, val: T,
    weak: bool = false, mo_succ = SeqCst, mo_fail = Release
    ): bool {.importc: "__atomic_compare_exchange_n", nodecl.}
  proc storeNImpl(p: ptr int128, val: int128, mo = SeqCst) {.importc: "__atomic_store_n", nodecl.}
elif defined(gcc) or defined(amd64):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using GCC, will use deprecated __sync funcs which still emit CMPXCHG16 instructions
  # Note that __sync calls are full synchronisations (more cost)
  proc addFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_add_and_fetch", nodecl.}
  proc subFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_sub_and_fetch", nodecl.}
  proc compareExchangeImpl(p: ptr int128, expected: int128, val: int128): bool {.importc: "__sync_bool_compare_and_swap", nodecl.}
  proc compareExchangeValImpl(p: ptr int128, expected: int128, val: int128): int128 {.importc: "__sync_val_compare_and_swap", nodecl.}
  # For compatability with atomic calls, convenience functions to discard the memory order
  proc addFetch(p: ptr int128, val: int128, mo = SeqCst): int128 {.inline.} =
    atomicAddFetchImpl p, val
  proc subFetch(p: ptr int128, val: int128, mo = SeqCst): int128 {.inline.} =
    atomicSubFetchImpl p, val
  proc loadN(p: ptr int128, mo = SeqCst): int128 {.inline.} =
    # Force cmpxchg16; __sync has no load
    atomicAddFetchImpl p, hint128(hi: 0'u, lo: 0'u)
  proc atomicCompareExchangeN(p: ptr int128, expected: ptr int128, val: int128, weak = true, mo_succ = SeqCst, mo_fail = SeqCst): bool {.inline.} =
    atomicCompareExchangeImpl(p, expected[], val)
  proc atomicStoreN(p: ptr int128, val: int128, mo = SeqCst) {.inline.} =
    # A horrible solution; should strongly consider switching to clang after this
    while not atomicCompareExchangeImpl(p, p[], val):
      p[] = atomicLoadN(p)

else:
  # Currently have not ported Hyaline-1 support for single word atomic only architectures
  {.error: "Compilation targets must support DCAS atomics; please submit an issue if this is erroneous".}
  # Because we fail compilation for non-target archs, we can accept int/uint as types since they are 64bit

type
  # Accepted types as arguments
  AtomicIntegers = ptr | pointer | uint64 | int64 | int | uint | int128 | hint128

# Exported atomic ops
proc addFetch*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = SeqCst): T {.inline.} =
  when p is ptr | pointer:
    let pptr = p
  else:
    let pptr = addr p.val
  cast[T](
      atomicAddFetch(pptr, cast[typeof pptr[]](v), mo)
    )
proc fetchAdd*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = SeqCst): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicFetchAdd(pptr, cast[typeof pptr[]](v), mo)
  )
proc subFetch*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = SeqCst): T {.inline.} =
  let pptr = addr p.val
  cast[T](
      atomicSubFetch(pptr, cast[typeof pptr[]](v), mo)
    )
proc fetchSub*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = SeqCst): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicFetchSub(pptr, cast[typeof pptr[]](v), mo)
  )
proc store*[T](p: ptr HyAtomic[T] | var HyAtomic[T], v: AtomicIntegers; mo = SeqCst) {.inline.} =
  let pptr = addr p.val
  atomicStoreN(pptr, cast[typeof pptr[]](v), mo)
proc load*[T](p: ptr HyAtomic[T] | var HyAtomic[T]; mo = SeqCst): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicLoadN(pptr, mo)
  )
proc exchange*[T](p: ptr HyAtomic[T] | var HyAtomic[T]; v: AtomicIntegers; mo = SeqCst): T {.inline.} =
  let pptr = addr p.val
  cast[T](
    atomicExchangeN(pptr, cast[typeof pptr[]](v), mo)
  )
proc compareExchange*[T](p: ptr HyAtomic[T] | var HyAtomic[T], e: ptr T | var T, v: AtomicIntegers; weak = false; mo_succ = SeqCst, mo_fail = SeqCst): bool {.inline.} =
  let pptr = addr p.val
  when e is ptr T:
    let eptr = e
  else:
    let eptr = addr e
  cast[bool](
    atomicCompareExchangeN(pptr, eptr, cast[typeof pptr[]](v), weak, mo_succ, mo_fail)
  )

