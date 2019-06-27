#!/bin/bash

SOLC_PATH="$HOME/bin/solc"

if [ -x "$SOLC_PATH" ]; then
  SOLC_VERSION="$($SOLC_PATH --version)"
  if [[ $SOLC_VERSION == *"0.5.10"* ]]; then exit 0;
  else rm -f "$SOLC_PATH";
  fi
fi

wget -O "$SOLC_PATH" https://github.com/ethereum/solidity/releases/download/v0.5.10/solc-static-linux
chmod u+x "$SOLC_PATH"

SOLC_VERSION="$($SOLC_PATH --version)"
if [[ $SOLC_VERSION == *"0.5.10"* ]]; then exit 0;
else echo "Something went wrong, solc version is not as expected: $SOLC_VERSION"; exit 1;
fi
