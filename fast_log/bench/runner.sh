#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
exec dune exec --profile release -- ./run_benchmarks.exe -fork -run-without-cross-library-inlining "$@" -quota 2