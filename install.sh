#!/bin/bash

fitorbit_dir=$(pwd)
echo "Fitorbit will be installed in "$fitorbit_dir

sed -i 's+FTORBIT=fitorbit_root_directory+FTORBIT='${fitorbit_dir}'+g' ${fitorbit_dir}/Makefile_Custom


cd ${fitorbit_dir}/dialog
make

cd ${fitorbit_dir}/slalib
make 

cd ${fitorbit_dir}/psrlib
make

cd ${fitorbit_dir}/src
make && echo && echo "fitorbit successfully installed in "${fitorbit_dir}/bin." Please add this directory to your PATH environment variable."
