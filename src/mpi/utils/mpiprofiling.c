#include <stdlib.h>
#include <stdbool.h>

#include <string.h>

#include "vftrace_state.h"
#include "mpi_state_types.h"
#include "process_types.h"
#include "environment_types.h"
#include "mpi_util_types.h"
#include "mpiprofiling_types.h"

#include "range_expand.h"
#include "search.h"

mpiProfile_t vftr_new_mpiprofiling() {
   mpiProfile_t prof;
   prof.nsendmessages = 0ll;
   prof.nrecvmessages = 0ll;
   prof.send_bytes = 0ll;
   prof.recv_bytes = 0ll;
   prof.acc_send_bw = 0.0;
   prof.acc_recv_bw = 0.0;
   prof.total_time_usec = 0ll;
   prof.overhead_usec = 0ll;

   return prof;
}

static bool vftr_should_accumulate_message_info(mpi_state_t mpi_state, int rank) {
   bool should = mpi_state.my_rank_in_prof;
   should = should && vftr_binary_search_int(mpi_state.nprof_ranks,
                                             mpi_state.prof_ranks,
                                             rank) >= 0;
   return should;
}

void vftr_accumulate_message_info(mpiProfile_t *prof_ptr,
                                  mpi_state_t mpi_state,
                                  message_direction dir,
                                  long long count,
                                  int type_idx, int type_size,
                                  int rank, int tag,
                                  long long tstart,
                                  long long tend) {
   (void) type_idx;
   (void) tag;
   if (vftr_should_accumulate_message_info(mpi_state, rank)) {
      int nbytes = count * type_size;
      long long time = tend - tstart;
      double bw = nbytes * 1.0e6 / time;

      if (dir == send) {
         prof_ptr->nsendmessages++;
         prof_ptr->send_bytes += nbytes;
         prof_ptr->acc_send_bw += bw;
      } else {
         prof_ptr->nrecvmessages++;
         prof_ptr->recv_bytes += nbytes;
         prof_ptr->acc_recv_bw += bw;
      }
      prof_ptr->total_time_usec += time;
   }
}

void vftr_accumulate_mpiprofiling_overhead(mpiProfile_t *prof,
                                           long long overhead_usec) {
   prof->overhead_usec += overhead_usec;
}

void vftr_mpiprofiling_free(mpiProfile_t *prof_ptr) {
   (void) prof_ptr;
}

void vftr_create_profiled_ranks_list(environment_t environment,
                                     process_t process,
                                     mpi_state_t *mpi_state) {
   char *rangestr = environment.ranks_in_mpi_profile.value.string_val;
   if (!strcmp(rangestr, "all")) {
      mpi_state->nprof_ranks = process.nprocesses;
      mpi_state->prof_ranks = (int*) malloc(mpi_state->nprof_ranks*sizeof(int));
      for (int irank=0; irank<mpi_state->nprof_ranks; irank++) {
         mpi_state->prof_ranks[irank] = irank;
      }
      mpi_state->my_rank_in_prof = true;
   } else {
      mpi_state->prof_ranks = vftr_expand_rangelist(rangestr, &(mpi_state->nprof_ranks));
      int idx = vftr_binary_search_int(mpi_state->nprof_ranks,
                                       mpi_state->prof_ranks,
                                       process.processID);
      mpi_state->my_rank_in_prof = idx >= 0;
   }
}

void vftr_free_profiled_ranks_list(mpi_state_t *mpi_state) {
   if (mpi_state->nprof_ranks > 0) {
      mpi_state->nprof_ranks = 0;
      free(mpi_state->prof_ranks);
      mpi_state->prof_ranks = NULL;
      mpi_state->my_rank_in_prof = false;
   }
}

long long *vftr_get_total_mpi_overhead(stacktree_t stacktree, int nthreads) {
   // accumulate the mpi overhead for each thread separately
   long long *overheads_usec = (long long*) malloc(nthreads*sizeof(long long));
   for (int ithread=0; ithread<nthreads; ithread++) {
      overheads_usec[ithread] = 0ll;
   }

   int nstacks = stacktree.nstacks;
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack = stacktree.stacks+istack;
      int nprofs = stack->profiling.nprofiles;
      for (int iprof=0; iprof<nprofs; iprof++) {
         profile_t *prof = stack->profiling.profiles+iprof;
         int threadID = prof->threadID;
         overheads_usec[threadID] += prof->mpiProf.overhead_usec;
      }
   }
   return overheads_usec;
}

long long vftr_get_total_collated_mpi_overhead(collated_stacktree_t stacktree) {
   long long overheads_usec = 0ll;

   int nstacks = stacktree.nstacks;
   for (int istack=0; istack<nstacks; istack++) {
      collated_stack_t *stack = stacktree.stacks+istack;
      profile_t *prof = &(stack->profile);
      overheads_usec += prof->mpiProf.overhead_usec;
   }
   return overheads_usec;
}

void vftr_print_mpiprofiling(FILE *fp, mpiProfile_t mpiprof) {
   fprintf(fp, "nmsg: %lld/%lld, msgsize: %lld/%lld\n",
           mpiprof.nsendmessages,
           mpiprof.nrecvmessages,
           mpiprof.send_bytes,
           mpiprof.recv_bytes);
}
