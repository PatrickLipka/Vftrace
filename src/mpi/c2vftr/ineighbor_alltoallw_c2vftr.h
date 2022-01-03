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

#ifndef INEIGHBOR_ALLTOALLW_C2VFTR_H
#define INEIGHBOR_ALLTOALLW_C2VFTR_H

#ifdef _MPI
#include <mpi.h>

int vftr_MPI_Ineighbor_alltoallw_c2vftr(const void *sendbuf, const int *sendcounts,
                                        const MPI_Aint *sdispls, const MPI_Datatype *sendtypes,
                                        void *recvbuf, const int *recvcounts,
                                        const MPI_Aint *rdispls, const MPI_Datatype *recvtypes,
                                        MPI_Comm comm, MPI_Request *request);

#endif
#endif
