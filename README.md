# Advent of Code solutions in Clojure

If you're reading this you probably either want to see my solutions -- they're
arranged by year and then day at [src/aoc][] -- or you want to run them. Maybe
you're me from the future. Entry points for running solutions and tests are in
the [user namespace][], you probably want `run-tests`, `run`, or `rrr`
(do all of refresh, run tests, and then run).

`run` will download input (exactly once ever for each puzzle) and submit
solutions (or check them against a local cache). When it does make requests it
looks for a `.session` file.

Solutions are functions of no arguments called `part-1` or `part-2` in the
relevant namespace. `run` will also run functions called `scrap` for the
purpose of checking intermediate results without submitting anything.

Arguments to `run` (and `run-tests` and `rrr`) are integers: year, day, part;
all optional.

- `(run)` runs everything
- `(run 2021)` runs every day in 2021
- `(run 2015 19)` runs 2015 day 19
- `(run 2017 1 2)` runs 2017 day 1 part 2
- `(run 24)` runs day 24 in whatever `user/default-year` currently is

`(log)` spits the output of `(run)` to `results.log`.

[src/aoc]: src/aoc
[user namespace]: src/user.clj
