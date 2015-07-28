#!/bin/bash

for i in `ls *.hs`; do
  echo $i | sed -e s/\.hs//g | rev | xargs -I {} sh -c "git mv $i {}.hs"
done
