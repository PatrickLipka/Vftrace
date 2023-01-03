#!/bin/bash

source ${srcdir}/../../environment/filenames.sh

vftr_binary=ibarrier
configfile=${vftr_binary}.json
nprocs=4

determine_bin_prefix $vftr_binary

echo "{\"sampling\": {\"active\": true}}" > ${configfile}
export VFTR_CONFIG=${configfile}

${MPI_EXEC} ${MPI_OPTS} ${NP} ${nprocs} ./${vftr_binary} || exit 1

for irank in $(seq 0 1 $(bc <<< "${nprocs}-1"));
do
   vfdfile=$(get_vfdfile_name ${vftr_binary} ${irank})
   ../../../tools/vftrace_vfd_dump ${vfdfile}

   n=$(../../../tools/vftrace_vfd_dump ${vfdfile} | \
       grep -i "call MPI_Ibarrier" | wc -l)
   if [[ ${n} -le 0 ]] ; then
      exit 1;
   fi

   n=$(../../../tools/vftrace_vfd_dump ${vfdfile} | \
       grep -i "exit MPI_Ibarrier" | wc -l)
   if [[ ${n} -le 0 ]] ; then
      exit 1;
   fi
done
