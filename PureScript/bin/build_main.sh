#!/bin/bash

if [ $# -ne 3 ] ; then
  echo 'usage: ./build_main.sh <js_file> <main module> <main function>' && exit 1
fi

export file=$1
export module=$2
export function=$3

pulp browserify -m ${module} --to dist/${file}
sed -i -e s/main\(\)/${function}\(\)/g dist/${file}
