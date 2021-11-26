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

#include "mpi_buf_addr_const.h"
#include "igather.h"

void vftr_MPI_Igather_f082vftr(void *sendbuf, MPI_Fint *sendcount, MPI_Fint *f_sendtype,
                               void *recvbuf, MPI_Fint *recvcount, MPI_Fint *f_recvtype,
                               MPI_Fint *root, MPI_Fint *f_comm, MPI_Fint *f_request,
                               MPI_Fint *f_error) {

   MPI_Datatype c_sendtype = PMPI_Type_f2c(*f_sendtype);
   MPI_Datatype c_recvtype = PMPI_Type_f2c(*f_recvtype);
   MPI_Comm c_comm = PMPI_Comm_f2c(*f_comm);
   MPI_Request c_request;

   sendbuf = (void*) vftr_is_F_MPI_IN_PLACE(sendbuf) ? MPI_IN_PLACE : sendbuf;
   sendbuf = (void*) vftr_is_F_MPI_BOTTOM(sendbuf) ? MPI_BOTTOM : sendbuf;
   recvbuf = (void*) vftr_is_F_MPI_BOTTOM(recvbuf) ? MPI_BOTTOM : recvbuf;

   int c_error;
   int isintercom;
   PMPI_Comm_test_inter(c_comm, &isintercom);
   if (isintercom) {
      c_error = vftr_MPI_Igather_intercom(sendbuf,
                                          (int)(*sendcount),
                                          c_sendtype,
                                          recvbuf,
                                          (int)(*recvcount),
                                          c_recvtype,
                                          (int)(*root),
                                          c_comm,
                                          &c_request);
   } else {
      if (vftr_is_C_MPI_IN_PLACE(sendbuf)) {
         c_error = vftr_MPI_Igather_inplace(sendbuf,
                                            (int)(*sendcount),
                                            c_sendtype,
                                            recvbuf,
                                            (int)(*recvcount),
                                            c_recvtype,
                                            (int)(*root),
                                            c_comm,
                                            &c_request);
      } else {
         c_error = vftr_MPI_Igather(sendbuf,
                                    (int)(*sendcount),
                                     c_sendtype,
                                     recvbuf,
                                     (int)(*recvcount),
                                     c_recvtype,
                                     (int)(*root),
                                     c_comm,
                                     &c_request);
      }
   }

   *f_error = (MPI_Fint) (c_error);
   *f_request = PMPI_Request_c2f(c_request);
}

#endif