import std/strutils
import pkg/balls
import ../src/hyalos/atomics2

suite "Double Word Atomics":
  block:
    ## Use double word atomic fetch add on a int128
    
    var count = hint128(hi: 0'u, lo: 1'u)
    checkpoint "Created int128 var " % [ $count ]

    let res = atomicAddFetch(count, hint128(hi: 1'u))
    checkpoint "Performed doubleword add fetch"
    check res == hint128(hi: 1'u, lo: 1'u)