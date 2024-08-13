import std/strutils
import pkg/balls
import ../src/hyalos/atomics2 {.all.}

var littleMan: HyAtomic[int]
var fatBoy: HyAtomic[hint128]

suite "Normal Atomic Ops":
  block:
    ## check addFetch
    littleMan.store(1'u)
    check littleMan.addFetch(1'u) == 2 # fails
