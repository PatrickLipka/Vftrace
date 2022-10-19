#ifdef _MPI
#include <mpi.h>

#include "mpi_buf_addr_const.h"
#include "alltoallw.h"

int vftr_MPI_Alltoallw_c2vftr(const void *sendbuf, const int *sendcounts,
                              const int *sdispls, const MPI_Datatype *sendtypes,
                              void *recvbuf, const int *recvcounts,
                              const int *rdispls, const MPI_Datatype *recvtypes,
                              MPI_Comm comm) {
   // determine if inter or intra communicator
   int isintercom;
   PMPI_Comm_test_inter(comm, &isintercom);
   if (isintercom) {
      return vftr_MPI_Alltoallw_intercom(sendbuf, sendcounts,
                                         sdispls, sendtypes,
                                         recvbuf, recvcounts,
                                         rdispls, recvtypes,
                                         comm);
   } else {
      if (vftr_is_C_MPI_IN_PLACE(sendbuf)) {
         return vftr_MPI_Alltoallw_inplace(sendbuf, sendcounts,
                                           sdispls, sendtypes,
                                           recvbuf, recvcounts,
                                           rdispls, recvtypes,
                                           comm);
      } else {
         return vftr_MPI_Alltoallw(sendbuf, sendcounts,
                                   sdispls, sendtypes,
                                   recvbuf, recvcounts,
                                   rdispls, recvtypes,
                                   comm);
      }
   }
}

#endif
