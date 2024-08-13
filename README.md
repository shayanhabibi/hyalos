# Hyalos

Under development NimSkull port of [Crystalline](https://github.com/shayanhabibi/hyalos/blob/master/paper/Crystalline.pdf)


## Description

*Crystalline* is a modern Lock and Wait free safe memory reclamation
algorithm for concurrent lock-free data structures.

It would be a replacement for ARC in managing the memory of currently-in-development concurrent data structures based on other
research such as the BonsaiQ which is based on TSLQueue.

## Progress

The algorithm has almost been transcribed, working around numerous
obstacles such as lack of DCAS operations on AMD64 w/ GCC toolchain.

- [x] *Algorithm transcribed* - The algorithm has been transcribed but the atomics (among other things) have to be completely refactored to avoid any use of system lib

  - [ ] __sync legacy funcs for DCAS ops on GCC
  - [ ] Test DCAS ops

- [ ] Nimskull doesnt scream when it's compiled

- [ ] Abstract tests pass

- [ ] Crucible of grinds

- [ ] Macros & API

- [ ] Release to a crowd of crickets
