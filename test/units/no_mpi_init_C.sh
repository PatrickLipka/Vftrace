#!/bin/bash
set -x
test_name=no_mpi_init_C

if [ "x$HAS_MPI" == "xYES" ]; then
   ${MPI_EXEC} ${MPI_OPTS} ${NP} 1 ./${test_name} || exit 1
else
   ./${test_name} || exit 1
fi