#!/bin/bash

vftr_binary=sendrecv_replace
configfile=${vftr_binary}.json
nprocs=4
ntrials=1

echo "{\"sampling\": {\"active\": true}}" > ${configfile}
export VFTR_CONFIG=${configfile}

for itrial in $(seq 1 1 ${ntrials});
do
   # Generate a random message size
   nb=$(bc <<< "32*${RANDOM}")
   ${MPI_EXEC} ${MPI_OPTS} ${NP} ${nprocs} ./${vftr_binary} ${nb} || exit 1

   # check each rank for the correct message communication
   # patterns in the vfd file
   for irank in $(seq 0 1 $(bc <<< "${nprocs}-1"));
   do
      ../../../tools/vftrace_vfd_dump ${vftr_binary}_${irank}.vfd

      destrank=$(bc <<< "(${irank} + 1) % ${nprocs}")
      sourcerank=$(bc <<< "${irank} -1")
      if [ "${sourcerank}" -lt "0" ] ; then
         sourcerank=$(bc <<< "${nprocs} - 1")
      fi
      echo ${irank} ${destrank} ${sourcerank}
      # Validate sending
      # Get actually used message size
      count=$(../../../tools/vftrace_vfd_dump ${vftr_binary}_${irank}.vfd | \
              awk '$2=="send" && $3!="end"{getline;print;}' | \
              sed 's/=/ /g' | \
              sort -nk 9 | \
              awk '{print $2}' | \
              head -n 1)
      # get peer process
      peer=$(../../../tools/vftrace_vfd_dump ${vftr_binary}_${irank}.vfd | \
             awk '$2=="send" && $3!="end"{getline;print;}' | \
             sed 's/=/ /g' | \
             sort -nk 9 | \
             awk '{print $9}' | \
             head -n 1)
      # Check if actually used message size is consistent
      # with expected message size
      if [[ "${count}" -ne "${nb}" ]] ; then
         echo "Message send size from rank ${irank} to ${destrank} is ${count}!"
         echo "Was expecting message size of ${nb}!"
         exit 1;
      fi
      # Check if actually used peer process is consistent
      # with expected peer process
      if [[ "${peer}" -ne "${destrank}" ]] ; then
         echo "Message send from rank ${irank} to ${peer}!"
         echo "Was expecting sending to rank ${destrank}!"
         exit 1;
      fi

      # Validate receiving
      # Get actually used message size
      count=$(../../../tools/vftrace_vfd_dump ${vftr_binary}_${irank}.vfd | \
              awk '$2=="recv" && $3!="end"{getline;print;}' | \
              sed 's/=/ /g' | \
              sort -nk 9 | \
              awk '{print $2}' | \
              head -n 1)
      # get peer process
      peer=$(../../../tools/vftrace_vfd_dump ${vftr_binary}_${irank}.vfd | \
             awk '$2=="recv" && $3!="end"{getline;print;}' | \
             sed 's/=/ /g' | \
             sort -nk 9 | \
             awk '{print $9}' | \
             head -n 1)
      # Check if actually used message size is consistent
      # with expected message size
      if [[ "${count}" -ne "${nb}" ]] ; then
         echo "Message recv size from rank ${sourcerank} to ${irank} is ${count}!"
         echo "Was expecting message size of ${nb}!"
         exit 1;
      fi
      # Check if actually used peer process is consistent
      # with expected peer process
      if [[ "${peer}" -ne "${sourcerank}" ]] ; then
         echo "Message received from rank ${peer} by ${irank}!"
         echo "Was expecting receiving from rank ${sourcerank}!"
         exit 1;
      fi
   done
done
