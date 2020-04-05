#!/bin/bash

fitorbit_dir=$(pwd)
echo "Fitorbit will be installed in "$fitorbit_dir

sed -i 's+FTORBIT=fitorbit_root_directory+FTORBIT='${fitorbit_dir}'+g' ${fitorbit_dir}/Makefile_Custom


mkdir -p ${fitorbit_dir}/bin ${fitorbit_dir}/lib

cd ${fitorbit_dir}/dialog
make

cd ${fitorbit_dir}/slalib
make 

cd ${fitorbit_dir}/psrlib
make

cp ${fitorbit_dir}/dialog/libgialog.a ${fitorbit_dir}/slalib/libsla.a ${fitorbit_dir}/psrlib/libpsr.a ${fitorbit_dir}/lib/

cd ${fitorbit_dir}/src
make && echo && echo "fitorbit successfully installed in "${fitorbit_dir}/bin." Please add this directory to your PATH environment variable."
