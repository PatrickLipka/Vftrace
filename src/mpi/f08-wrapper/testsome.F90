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

SUBROUTINE MPI_Testsome_f08(incount, array_orequests, outcount, &
                            array_oindices, array_ostatuses, error)
   USE vftr_mpi_testsome_f082c_f08interface, &
      ONLY : vftr_MPI_Testsome_f082c
   USE mpi_f08, ONLY: MPI_Request, &
                      MPI_Status
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: incount
   TYPE(MPI_Request), INTENT(INOUT) :: array_orequests(*)
   INTEGER, INTENT(OUT) :: outcount
   INTEGER, INTENT(OUT) :: array_oindices(*)
   TYPE(MPI_Status) :: array_ostatuses(*)
   INTEGER, OPTIONAL, INTENT(OUT) :: error
   INTEGER :: tmperror

   INTEGER, DIMENSION(incount) :: tmp_array_orequests
   INTEGER :: i

   DO i = 1, incount
      tmp_array_orequests(i) = array_orequests(i)%MPI_VAL
   END DO

   CALL vftr_MPI_Testsome_f082c(incount, tmp_array_orequests, outcount, &
                                array_oindices, array_ostatuses, tmperror)

   DO i = 1, incount
      array_orequests(i)%MPI_VAL = tmp_array_orequests(i)
   END DO

   IF (PRESENT(error)) error = tmperror

END SUBROUTINE MPI_Testsome_f08

#endif 
