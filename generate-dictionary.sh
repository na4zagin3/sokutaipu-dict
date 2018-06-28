#!/bin/bash

stack build && stack exec sokutype-dict-exe | jq . > sokutaipu-dictionary.json || exit 1
