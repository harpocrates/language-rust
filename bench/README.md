We have two types of benchmarks. If you are using `stack` you can run them with

```
$ stack bench                         # runs all benchmarks
$ stack bench :allocation-benchmarks  # runs allocation benchmarks only (faster)
$ stack bench :timing-benchmarks      # runs timing benchmarks only (slower)
```

## `allocation-benchmarks`

Benchmarks how much memory is allocated by the runtime when parsing the files inside of the
`sample-sources` directory at the project root. Resulting information is stored in a JSON file in
the `allocations` folder (automatically created in this directory).

## `timing-benchmarks`

Benchmark how long it takes to parse the files inside the `sample-sources` directory. Resulting
information is stored in a JSON file in the `timings` folder (automatically created in this
directory).

# Tools

Since some of these tests take a while, you can add a `.benchignore` file in `sample-sources` which
lists files to skip for benchmarking (one file name per line). 

There is also a `bench.py` utility in this directory which lets you compare benchmarks across
different commits. It relies on the JSON files in `allocations` and `timings`, so you will have to
checkout and run the benchmarks on commits you want to compare against (to generate the 
corresponding JSON file).

```
$ ./bench.py --folder allocations      # compare the last several commits for allocations
$ ./bench.py --folder timings          # compare the last several commits for timings
```

