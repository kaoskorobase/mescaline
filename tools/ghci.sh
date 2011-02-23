#!/bin/bash

args="-XScopedTypeVariables"
ghci $args -Iinclude -isrc -idist/build/autogen -hide-package monads-fd -hide-package monads-tf "$@"
