#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
# export BENCH_LIB=csexp_bench
exec dune exec -- ./run_benchmarks.exe -fork -run-without-cross-library-inlining "$@" -quota 2