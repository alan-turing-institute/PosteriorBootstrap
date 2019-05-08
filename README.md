PosteriorBootstrap
[![Build Status](https://travis-ci.com/alan-turing-institute/PosteriorBootstrap.svg?branch=master)](https://travis-ci.com/alan-turing-institute/PosteriorBootstrap)
[![codecov](https://codecov.io/gh/alan-turing-institute/PosteriorBootstrap/branch/master/graph/badge.svg)](https://codecov.io/gh/alan-turing-institute/PosteriorBootstrap)
==================

## `gfortran`
To install the `gfortran` dependency on OSX, I used the [`.dmg` from here](https://github.com/fxcoudert/gfortran-for-macOS/releases) which installs into `/usr/local/gfortran/`.

## Parallelisation

The calculation of the expected speedup depends on the number of bootstrap
samples and the number of processors. It also depends on the system: it is
larger on macOS than on Linux, with some variation depending on the version of
R.

### Ahmdal's law

Fixing the number of samples corresponds to [Ahmdal's
law](https://en.wikipedia.org/wiki/Ahmdal's_Law), or the speedup in the task as
a function of the number of processors. The speedup `S_latency` of `N` processors is defined as
the duration of the task with one core divided by the duration of the task with
`N` processors. For the number of bootstrap samples in
100, 1000, and 10,000, the speedup is:

![Parallelisation speedup]("Speedup.pdf")

I inverted Ahmdal's law to compute the proportion of the execution time that is
parallelisable from the speedup as as:

$$ p = \frac{\frac{1}{S_{latency}}} - 1}{\frac{1}{s} - 1}

where $S_{latency}$ is the theoretical speedup of the whole task in Ahmdal's law
and the observed speedup here, and $s$ is the speedup of the part of the task
that can be parallelised, and thus equal to the number of
processors. Calculating this value for the durations from 1 to 8 cores, I got
this plot:

![Parallelisation proportion]("Proportion.pdf")

The proportion of the code that can be parallelised is high, and higher the
large the bootstrap samples, and always below 1. For large samples with
`n_bootstrap = 10,000`, one estimate of proportion is close to 100%.

### Speedup as a function of `n_bootstrap`

On a macOS machine, I ran `n_boostrap` from 10 to 1000 by steps of 100, and 3
tries for each value, and timed the duration of the code, then ran a linear
regression

$$ t \approx a + b * n_boostrap $$

for 1 or 2 cores separately. I found an overhead of around 0.13 seconds
(intercept $a$) for both 1 and 2 cores. Each draw takes 0.01186 seconds on one
core and 0.006357 on two cores (slope $b$). The maximum speedup is 1.86, which
is Amdahl's law for 2 processors, i.e. 92% of the code is parallelisable (= 2 *
(1.85 - 1) / 1.85, for s = 2 and S_latency = 1.85).


