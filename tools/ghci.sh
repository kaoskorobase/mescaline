#!/bin/bash

args="-XScopedTypeVariables -XOverloadedStrings"
ghci $args -Iinclude -ilib/mescaline -idist/build/autogen "$@"
