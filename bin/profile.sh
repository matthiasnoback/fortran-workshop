#!/usr/bin/env bash

# This script runs well on Linux, with valgrind and massif-visualizer installed.

set -eu

command="${1}"
file_suffix="${2-1}"
file="massif.out.${file_suffix}"

valgrind --tool=massif --time-unit=B --massif-out-file="${file}" -- "${command}"
massif-visualizer "${file}"
