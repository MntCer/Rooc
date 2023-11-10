#!/bin/sh

LLC="llc"
CC="cc"
Rooc="Rooc"

# Run the commands directly without 'Run'
dune exec -- $Rooc "./tests/test-success-print.rooc" > "./tests/print.ll" &&
$LLC -relocation-model=pic "./tests/print.ll" > "./tests/print.s" &&
$CC -o "./tests/print.exe" "./tests/print.s" &&
./tests/print.exe