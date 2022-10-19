#ifndef IGATHER_C2VFTR_H
#define IGATHER_C2VFTR_H

#ifdef _MPI
#include <mpi.h>

int vftr_MPI_Igather_c2vftr(const void *sendbuf, int sendcount,
                            MPI_Datatype sendtype, void *recvbuf,
                            int recvcount, MPI_Datatype recvtype,
                            int root, MPI_Comm comm,
                            MPI_Request *request);

#endif
#endif
