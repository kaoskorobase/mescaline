#!/bin/bash

args="-XScopedTypeVariables"
ghci $args -Iinclude -isrc -idist/build/autogen -hide-package transformers -hide-package monads-tf "$@"
