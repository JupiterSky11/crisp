#!/bin/bash

if [[ $# -ge 1 ]]; then
	nix develop -c fish -c $@
else
	nix develop -c fish
fi
