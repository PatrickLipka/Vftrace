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

#ifdef _MPI
#include <mpi.h>

#include "vftr_buf_addr_const.h"
#include "alltoallv.h"

int vftr_MPI_Alltoallv_c2vftr(const void *sendbuf, const int *sendcounts,
                              const int *sdispls, MPI_Datatype sendtype,
                              void *recvbuf, const int *recvcounts,
                              const int *rdispls, MPI_Datatype recvtype,
                              MPI_Comm comm) {
   // determine if inter or intra communicator
   int isintercom;
   PMPI_Comm_test_inter(comm, &isintercom);
   if (isintercom) {
      return vftr_MPI_Alltoallv_intercom(sendbuf, sendcounts,
                                         sdispls, sendtype,
                                         recvbuf, recvcounts,
                                         rdispls, recvtype,
                                         comm);
   } else {
      if (vftr_is_C_MPI_IN_PLACE(sendbuf)) {
         return vftr_MPI_Alltoallv_inplace(sendbuf, sendcounts,
                                           sdispls, sendtype,
                                           recvbuf, recvcounts,
                                           rdispls, recvtype,
                                           comm);
      } else {
         return vftr_MPI_Alltoallv(sendbuf, sendcounts,
                                   sdispls, sendtype,
                                   recvbuf, recvcounts,
                                   rdispls, recvtype,
                                   comm);
      }
   }
}

#endif