#!/bin/bash

while true; do
    inotifywait -r ./src ./test --exclude "swp" -e modify
    cabal test
done
