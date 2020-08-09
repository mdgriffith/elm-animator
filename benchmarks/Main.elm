module Main exposing (main)

{-| -}

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmarks


main : BenchmarkProgram
main =
    program Benchmarks.suite
