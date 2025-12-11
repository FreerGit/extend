open! Core_bench
open! Bench_lib

(* This executable runs all the benchmarks *)
let () = Command_unix.run (Bench.make_command [])
