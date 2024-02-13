#!/bin/bash

set -e

# Remove existing plist directory if it exists
if [ -d "plist" ]; then
  rm -rf plist
fi

git clone https://github.com/8ta4/plist.git
cd plist
direnv allow
devenv shell build
