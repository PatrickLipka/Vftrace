#ifdef _MPI
#include <mpi.h>

#include "mpi_logging.h"
#include "rput_c2vftr.h"

int MPI_Rput(const void *origin_addr, int origin_count,
             MPI_Datatype origin_datatype, int target_rank,
             MPI_Aint target_disp, int target_count,
             MPI_Datatype target_datatype, MPI_Win win,
             MPI_Request *request) {
   if (vftr_no_mpi_logging()) {
      return PMPI_Rput(origin_addr, origin_count, origin_datatype,
                       target_rank, target_disp, target_count,
                       target_datatype, win, request);
   } else {
      return vftr_MPI_Rput_c2vftr(origin_addr, origin_count, origin_datatype,
                                  target_rank, target_disp, target_count,
                                  target_datatype, win, request);
   }
}

#endif
