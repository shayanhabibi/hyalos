# Define the int128 base type and the helper type hint128 to
# use double word atomic operations on available architectures

# GCC no longer emits cmpxchg16 for double word atomics
# See https://github.com/msys2/MINGW-packages/issues/13831
# See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=80878

type
  int128* {.importc: "__int128".} = object
    _, DONOTACCESS: uint64
  hint128* = object
    hi*, lo*: uint

when defined(clang) and (defined(amd64) or defined(x86)):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using clang however, will call built in __atomic funcs
  proc atomicAddFetchImpl(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.importc: "__atomic_add_fetch", nodecl.}
  proc atomicSubFetchImpl(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): int128 {.importc: "__atomic_sub_fetch", nodecl.}

  proc atomicAddFetch*(self: ptr hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function
    cast[hint128](
      atomicAddFetchImpl(cast[ptr int128](self), cast[int128](value), memoryOrder)
    )
  proc atomicSubFetch*(self: ptr hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function
    cast[hint128](
      atomicSubFetchImpl(cast[ptr int128](self), cast[int128](value), memoryOrder)
    )

  proc atomicAddFetch*(self: var hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function
    let p = cast[ptr int128](unsafeAddr self)
    let val = cast[int128](value)
    return cast[hint128](
      atomicAddFetchImpl(p, val, memoryOrder)
    )
  proc atomicSubFetch*(self: var hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function
    let p = cast[ptr int128](unsafeAddr self)
    let val = cast[int128](value)
    return cast[hint128](
      atomicSubFetchImpl(p, val, memoryOrder)
    )
elif defined(gcc) or defined(amd64):
  # GCC no longer emits cmpxchg16 for double word atomics
  # When using GCC, will use deprecated __sync funcs which still emit CMPXCHG16 instructions

  proc atomicAddFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_add_and_fetch", nodecl.}
  proc atomicSubFetchImpl(p: ptr int128, val: int128): int128 {.importc: "__sync_sub_and_fetch", nodecl.}

  proc atomicAddFetch*(p: ptr int128 | ptr hint128, val: int128 | hint128, mo = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function with `mo` for compatability with __atomic func calls
    ## when compiling with `clang`
    return cast[hint128](
      atomicAddFetchImpl(cast[ptr int128](p), cast[int128](val))
    )
  proc atomicSubFetch*(p: ptr int128, val: int128, mo = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function with `mo` for compatability with __atomic func calls
    ## when compiling with `clang`
    return cast[hint128](
      atomicSubFetchImpl(cast[ptr int128](p), cast[int128](val))
    )

  proc atomicAddFetch*(self: var hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function with `mo` for compatability with __atomic func calls
    ## when compiling with `clang`
    let p = cast[ptr int128](unsafeAddr self)
    let val = cast[int128](value)
    return cast[hint128](
      atomicAddFetchImpl(p, val)
    )
  proc atomicSubFetch*(self: var hint128, value: hint128, memoryOrder = ATOMIC_SEQ_CST): hint128 =
    ## Convenience Function with `mo` for compatability with __atomic func calls
    ## when compiling with `clang`
    let p = cast[ptr int128](unsafeAddr self)
    let val = cast[int128](value)
    return cast[hint128](
      atomicSubFetchImpl(p, val)
    )

else:
  # Currently have not ported Hyaline-1 support for single word atomic only architectures
  {.error: "No beef here".}
