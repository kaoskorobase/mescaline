#!/bin/bash

args="-XScopedTypeVariables -XOverloadedStrings"
ghci $args -Ilib/mescaline/include -ilib/mescaline -idist/build/autogen "$@"
