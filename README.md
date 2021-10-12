# fitorbit
Program to fit binary pulsar orbits

This software was orignially written by Prof. Andrew Lyne (andrew.lyne@manchester.ac.uk) and is the owner. There have been minor contributions from Dr. Paulo Freire, Dr. Alessandro Ridolfi and me. 

# Installation

For most people, running `install.sh` should do the work. If it doesn't, read futher.

# Dependencies

The package already includes the `dialog` and `sla` libraries that are needed. If you already have these installed previously, make sure they are present in the `LIBRARY_PATH` environment variable, and remove their corresponding `make` lines from `install.sh`. 

fitorbit also depends on `pgplot` and its corresponding environment variables: `PGPLOT_DIR`, `PGPLOT_DEV` and `PGPLOT_FONT` should be set before installation.

# Usage

1. Set the environment variable `fitorbitdir` to point to the source directory,  so something like `export fitorbitdir="/home/psr/software/fitorbit/src"
2. Put the ephemeris file (named `NAME.eph`) and mod file containing your spin period measurements (named `NAME.mod`) where `NAME` is the same as the directory where you put these
3. This directory itself needs to go inside another directory named "timing". So your directory structure should be something like `/home/psr/timing/NGC1851D/` containing `NGC1851D.mod` and `NGC1851D.eph`
4. cd to the directory containing the ephemeris and then run `fitorbit`


