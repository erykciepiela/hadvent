#!/bin/bash
ghcid --command "stack ghci hadvent:exe:2019-11 --ghci-options=-fobject-code" --test "main"