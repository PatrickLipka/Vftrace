! This file is part of Vftrace.
!
! Vftrace is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
!
! Vftrace is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along
! with this program; if not, write to the Free Software Foundation, Inc.,
! 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#ifdef _MPI

SUBROUTINE MPI_Start_f08(request, error)
   USE vftr_mpi_start_f082c_f08interface, &
      ONLY : vftr_MPI_Start_f082c
   IMPLICIT NONE
   INTEGER, INTENT(INOUT) :: request
   INTEGER, INTENT(OUT) :: error

   CALL vftr_MPI_Start_f082c(request, error)

END SUBROUTINE MPI_Start_f08

#endif 
