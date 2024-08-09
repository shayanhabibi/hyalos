const
  hyalosBatchSize* {.intdefine.} = 8

type
  Node* = object
    nRef*: int # can be pointer to next slot/node
    nRefNode*: ptr Node
    batchNext*: ptr Node
  Head* = object
    hRef*: int
    hPtr*: ptr Node
  LocalBatch* = object
    nRefNode*: ptr Node
    firstNode*: ptr Node
    when defined(hyalosStall):
      minBirth*: int # Hyaline-S

var hyHeads*: array[hyalosBatchSize, Head]
when defined(hyalosStall):
  # Hyaline-S
  var hyAccesses*: array[hyalosBatchSize, int]
  var hyAcks*: array[hyalosBatchSize, int]
