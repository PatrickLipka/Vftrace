#ifndef RGET_ACCUMULATE_H
#define RGET_ACCUMULATE_H

#include <mpi.h>

int vftr_MPI_Rget_accumulate(const void *origin_addr, int origin_count,
                             MPI_Datatype origin_datatype, void *result_addr,
                             int result_count, MPI_Datatype result_datatype,
                             int target_rank, MPI_Aint target_disp, int target_count,
                             MPI_Datatype target_datatype, MPI_Op op, MPI_Win win,
                             MPI_Request *request);

#endif
