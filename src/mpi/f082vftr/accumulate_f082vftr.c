#ifdef _MPI
#include <mpi.h>

#include "accumulate.h"

void vftr_MPI_Accumulate_f082vftr(void *origin_addr, MPI_Fint *origin_count,
                                  MPI_Fint *f_origin_datatype, MPI_Fint *target_rank,
                                  MPI_Aint *target_disp, MPI_Fint *target_count,
                                  MPI_Fint *f_target_datatype, MPI_Fint *f_op,
                                  MPI_Fint *f_win, MPI_Fint *f_error) {

   MPI_Datatype c_origin_datatype = PMPI_Type_f2c(*f_origin_datatype);
   MPI_Datatype c_target_datatype = PMPI_Type_f2c(*f_target_datatype);
   MPI_Op c_op = PMPI_Op_f2c(*f_op);
   MPI_Win c_win = PMPI_Win_f2c(*f_win);

   int c_error = vftr_MPI_Accumulate(origin_addr,
                                     (int)(*origin_count),
                                     c_origin_datatype,
                                     (int)(*target_rank),
                                     *target_disp,
                                     (int)(*target_count),
                                     c_target_datatype,
                                     c_op,
                                     c_win);

   *f_error = (MPI_Fint) c_error;
}

#endif
