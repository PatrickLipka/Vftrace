#ifdef _MPI
#include <mpi.h>

#include "mpi_logging.h"
#include "sync_time.h"
#include "alltoallv_c2vftr.h"

int MPI_Alltoallv(const void *sendbuf, const int *sendcounts,
                  const int *sdispls, MPI_Datatype sendtype,
                  void *recvbuf, const int *recvcounts,
                  const int *rdispls, MPI_Datatype recvtype,
                  MPI_Comm comm) {

   vftr_estimate_sync_time("MPI_Alltoallv_sync", comm);

   if (vftr_no_mpi_logging()) {
      return PMPI_Alltoallv(sendbuf, sendcounts,
                            sdispls, sendtype,
                            recvbuf, recvcounts,
                            rdispls, recvtype,
                            comm);
   } else {
      return vftr_MPI_Alltoallv_c2vftr(sendbuf, sendcounts,
                                       sdispls, sendtype,
                                       recvbuf, recvcounts,
                                       rdispls, recvtype,
                                       comm);
   }
}

#endif
