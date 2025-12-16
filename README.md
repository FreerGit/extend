# Extend

Extend is a monorepo of small libraries for writing high-performance software, while keeping reasonable ergonomics.

## [fast_log (ppx_log)](./fast_log/)

Logging meant to be used in hot paths.

- Cheap log calls
- I/O and string construction runs on seperate thread
- Exists with various backing queues for different usecases, (un)bounded MPSC/SPSC.

## [ppx_assert](./ppx_assert/)

Easier assertions, intended for invariants which are not obvious to encode in the type system. Can be turned off at compile time, 

## [ppx_defer](./ppx_defer/)

A Go-style `defer`

- Simple expansion
- Has `defer if`
- Runs even if an exception is thrown
- Good composability with ppx_assert, to model pre- and postconditions

## Notes

Libraries are independent but designed to fit together. The libraries are heavily inspired by [Tigerstyle](https://tigerstyle.dev/)


### TODOs

- Log to file, stdout or both.
- Tests, move to non-determenistic tests to test folder.
- Simple doc and usage example in each readme
- Benchmark
- Look at allocs
- Logger levels
- timestamp
- MPSC is slow
