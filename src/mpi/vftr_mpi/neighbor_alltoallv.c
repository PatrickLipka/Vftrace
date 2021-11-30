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
#include <stdlib.h>

#include <mpi.h>

#include "rank_translate.h"
#include "vftr_timer.h"
#include "sync_messages.h"
#include "cart_comms.h"

int vftr_MPI_Neighbor_alltoallv_graph(const void *sendbuf, const int *sendcounts,
                                      const int *sdispls, MPI_Datatype sendtype,
                                      void *recvbuf, const int *recvcounts,
                                      const int *rdispls, MPI_Datatype recvtype,
                                      MPI_Comm comm) {
   long long tstart = vftr_get_runtime_usec();
   int retVal = PMPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype,
                                        recvbuf, recvcounts, rdispls, recvtype,
                                        comm);
   long long tend = vftr_get_runtime_usec();

   long long t2start = vftr_get_runtime_usec();
   int rank;
   PMPI_Comm_rank(comm, &rank);
   int nneighbors;
   MPI_Graph_neighbors_count(comm, rank, &nneighbors);
   int *neighbors = (int*) malloc(nneighbors*sizeof(int));
   MPI_Graph_neighbors(comm, rank, nneighbors, neighbors);
   for (int ineighbor=0; ineighbor<nneighbors; ineighbor++) {
      vftr_store_sync_message_info(send, sendcounts[ineighbor], sendtype,
                                   neighbors[ineighbor], -1,
                                   comm, tstart, tend);
      vftr_store_sync_message_info(recv, recvcounts[ineighbor], recvtype,
                                   neighbors[ineighbor], -1,
                                   comm, tstart, tend);
   }
   free(neighbors);
   neighbors = NULL;
   long long t2end = vftr_get_runtime_usec();

   vftr_mpi_overhead_usec += t2end - t2start;

   return retVal;
}

int vftr_MPI_Neighbor_alltoallv_cart(const void *sendbuf, const int *sendcounts,
                                     const int *sdispls, MPI_Datatype sendtype,
                                     void *recvbuf, const int *recvcounts,
                                     const int *rdispls, MPI_Datatype recvtype,
                                     MPI_Comm comm) {
   long long tstart = vftr_get_runtime_usec();
   int retVal = PMPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype,
                                        recvbuf, recvcounts, rdispls, recvtype,
                                        comm);
   long long tend = vftr_get_runtime_usec();

   long long t2start = vftr_get_runtime_usec();
   int rank;
   PMPI_Comm_rank(comm, &rank);
   int nneighbors;
   int *neighbors;
   vftr_mpi_cart_neighbor_ranks(comm, &nneighbors, &neighbors);
   for (int ineighbor=0; ineighbor<nneighbors; ineighbor++) {
      // the neighborlist contains a -1 for non existent neighbors
      // due to non-periodic boundaries
      if (neighbors[ineighbor] >= 0) {
         vftr_store_sync_message_info(send, sendcounts[ineighbor], sendtype,
                                      neighbors[ineighbor], -1,
                                      comm, tstart, tend);
         vftr_store_sync_message_info(recv, recvcounts[ineighbor], recvtype,
                                      neighbors[ineighbor], -1,
                                      comm, tstart, tend);
      }
   }
   free(neighbors);
   neighbors = NULL;
   long long t2end = vftr_get_runtime_usec();

   vftr_mpi_overhead_usec += t2end - t2start;

   return retVal;
}

int vftr_MPI_Neighbor_allgatherv_dist_graph(const void *sendbuf, const int *sendcounts,
                                            const int *sdispls, MPI_Datatype sendtype,
                                            void *recvbuf, const int *recvcounts,
                                            const int *rdispls, MPI_Datatype recvtype,
                                            MPI_Comm comm) {
   long long tstart = vftr_get_runtime_usec();
   int retVal = PMPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype,
                                        recvbuf, recvcounts, rdispls, recvtype,
                                        comm);
   long long tend = vftr_get_runtime_usec();

   long long t2start = vftr_get_runtime_usec();
   // first obtain the distributed graph info for this process
   int ninneighbors;
   int noutneighbors;
   int weighted;
   PMPI_Dist_graph_neighbors_count(comm, &ninneighbors,
                                   &noutneighbors, &weighted);
   int *inneighbors = (int*) malloc(ninneighbors*sizeof(int));
   int *inweights = (int*) malloc(ninneighbors*sizeof(int));
   int *outneighbors = (int*) malloc(noutneighbors*sizeof(int));
   int *outweights = (int*) malloc(noutneighbors*sizeof(int));
   PMPI_Dist_graph_neighbors(comm,
                             ninneighbors, inneighbors, inweights,
                             noutneighbors, outneighbors, outweights);
   for (int ineighbor=0; ineighbor<noutneighbors; ineighbor++) {
      vftr_store_sync_message_info(send, sendcounts[ineighbor], sendtype,
                                   outneighbors[ineighbor], -1,
                                   comm, tstart, tend);
   }
   for (int ineighbor=0; ineighbor<ninneighbors; ineighbor++) {
      vftr_store_sync_message_info(recv, recvcounts[ineighbor], recvtype,
                                   inneighbors[ineighbor], -1,
                                   comm, tstart, tend);
   }
   free(inneighbors);
   inneighbors = NULL;
   free(inweights);
   inweights = NULL;
   free(outneighbors);
   outneighbors = NULL;
   free(outweights);
   outweights = NULL;

   long long t2end = vftr_get_runtime_usec();

   vftr_mpi_overhead_usec += t2end - t2start;

   return retVal;
}