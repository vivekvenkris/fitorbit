# fitorbit
Program to fit binary pulsar orbits

This software was orignially written by Prof. Andrew Lyne (andrew.lyne@manchester.ac.uk) and is the owner. There has been minor contributions from Dr. Paulo Freire, Dr. Alessandro Ridolfi and me. 

# Installation

For most people, running `install.sh` should do the work. If it doesn't, read futher. 

# Dependencies

The package already includes the `dialog` and `sla` libraries that are needed. If you already have these installed previously, make sure they are present in the `LIBRARY_PATH` environment variable, and directly compile `fitorbit`. 

fitorbit also depends on `pgplot` and its corresponding environment variables: `PGPLOT_DIR`, `PGPLOT_DEV` and `PGPLOT_FONT` should be set before installation.

