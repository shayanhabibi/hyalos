import std/osproc
import std/strutils
import std/logging
import std/os

import pkg/balls
import pkg/nuclear
import hyalos

echo "STARTING"

var hyTracker = newHyalosTracker[uint, 16](14, 1, 1)

hyTracker.alloc(1)

echo "DONE"
