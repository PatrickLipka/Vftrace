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

#include "vftr_mpi_utils.h"
#include "vftr_mpi_bsend.h"

int MPI_Bsend(const void *buf, int count, MPI_Datatype datatype,
             int dest, int tag, MPI_Comm comm) {
   if (vftr_no_mpi_logging()) {
      return PMPI_Bsend(buf, count, datatype, dest, tag, comm);
   } else {
      return vftr_MPI_Bsend(buf, count, datatype, dest, tag, comm);
   }
}

#endif
