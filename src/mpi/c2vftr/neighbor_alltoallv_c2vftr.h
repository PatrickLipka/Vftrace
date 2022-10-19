#ifndef NEIGHBOR_ALLTOALLV_C2VFTR_H
#define NEIGHBOR_ALLTOALLV_C2VFTR_H

#ifdef _MPI
#include <mpi.h>

int vftr_MPI_Neighbor_alltoallv_c2vftr(const void *sendbuf, const int *sendcounts,
                                       const int *sdispls, MPI_Datatype sendtype,
                                       void *recvbuf, const int *recvcounts,
                                       const int *rdispls, MPI_Datatype recvtype,
                                       MPI_Comm comm);

#endif
#endif
