/*
   This file is part of Vftrace.

   Vftrace is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Vftrace is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

#ifndef VFTR_MPI_COLLECTIVE_H
#define VFTR_MPI_COLLECTIVE_H

#ifdef _MPI
#include <mpi.h>

int vftr_MPI_Allgather(const void *sendbuf, int sendcount,
                       MPI_Datatype sendtype, void *recvbuf, int recvcount,
                       MPI_Datatype recvtype, MPI_Comm comm);

int vftr_MPI_Allgatherv(const void *sendbuf, int sendcount,
                        MPI_Datatype sendtype, void *recvbuf,
                        const int *recvcounts, const int *displs,
                        MPI_Datatype recvtype, MPI_Comm comm);

int vftr_MPI_Allreduce(const void *sendbuf, void *recvbuf, int count,
                       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm);

int vftr_MPI_Alltoall(const void *sendbuf, int sendcount,
                      MPI_Datatype sendtype, void *recvbuf, int recvcount,
                      MPI_Datatype recvtype, MPI_Comm comm);

int vftr_MPI_Ialltoall(const void *sendbuf, int sendcount,
                       MPI_Datatype sendtype, void *recvbuf, int recvcount,
                       MPI_Datatype recvtype, MPI_Comm comm,
                       MPI_Request *request);

int vftr_MPI_Alltoallv(const void *sendbuf, const int *sendcounts,
                       const int *sdispls, MPI_Datatype sendtype, void *recvbuf,
                       const int *recvcounts, const int *rdispls,
                       MPI_Datatype recvtype, MPI_Comm comm);

int vftr_MPI_Alltoallw(const void *sendbuf, const int *sendcounts,
                       const int *sdispls, const MPI_Datatype *sendtypes,
                       void *recvbuf, const int *recvcounts, const int *rdispls,
                       const MPI_Datatype *recvtypes, MPI_Comm comm);

int vftr_MPI_Bcast(void *buffer, int count, MPI_Datatype datatype,
                   int root, MPI_Comm comm);

int vftr_MPI_Gather(const void *sendbuf, int sendcount,
                    MPI_Datatype sendtype, void *recvbuf, int recvcount,
                    MPI_Datatype recvtype, int root, MPI_Comm comm);

int vftr_MPI_Gatherv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                     void *recvbuf, const int *recvcounts, const int *displs,
                     MPI_Datatype recvtype, int root, MPI_Comm comm);

int vftr_MPI_Reduce(const void *sendbuf, void *recvbuf, int count,
                    MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm);

int vftr_MPI_Reduce_scatter(const void *sendbuf, void *recvbuf,
                            const int *recvcounts, MPI_Datatype datatype,
                            MPI_Op op, MPI_Comm comm);

int vftr_MPI_Scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                     void *recvbuf, int recvcount, MPI_Datatype recvtype,
                     int root, MPI_Comm comm);

int vftr_MPI_Scatterv(const void *sendbuf, const int *sendcounts,
                      const int *displs, MPI_Datatype sendtype,
                      void *recvbuf, int recvcount, MPI_Datatype recvtype,
                      int root, MPI_Comm comm);

#endif
#endif
