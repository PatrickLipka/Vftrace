#!/bin/bash

set -x
test_name=sorting_stacks
configfile=${test_name}_calls.json
output_file=${test_name}_calls.out
ref_file=${srcdir}/ref_output/${test_name}_calls.out

# create logfile
echo "{\"profile_table\": {\"sort_table\": {\"column\": \"calls\", \"ascending\": false}}}" > ${configfile}
export VFTR_CONFIG=${configfile}

rm -f ${output_file}

if [ "x${HAS_MPI}" == "xYES" ]; then
   ${MPI_EXEC} ${MPI_OPTS} ${NP} 1 ./${test_name} > ${output_file} || exit 1
else
   ./${test_name} > ${output_file} || exit 1
fi

diff ${output_file} ${ref_file}
