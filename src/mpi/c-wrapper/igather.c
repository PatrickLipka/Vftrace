#ifdef _MPI
#include <mpi.h>

#include "mpi_logging.h"
#include "igather_c2vftr.h"

int MPI_Igather(const void *sendbuf, int sendcount,
                MPI_Datatype sendtype, void *recvbuf,
                int recvcount, MPI_Datatype recvtype,
                int root, MPI_Comm comm,
                MPI_Request *request) {
   if (vftr_no_mpi_logging()) {
      return PMPI_Igather(sendbuf, sendcount, sendtype, recvbuf, recvcount,
                          recvtype, root, comm, request);
   } else {
      return vftr_MPI_Igather_c2vftr(sendbuf, sendcount, sendtype,
                                     recvbuf, recvcount, recvtype,
                                     root, comm, request);
   }
}

#endif
