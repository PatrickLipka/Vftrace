#include <stdlib.h>
#include <stdio.h>

#include "environment_types.h"
#include "environment.h"
#include "stack_types.h"
#include "stacks.h"
#include "profiling_types.h"
#include "profiling.h"
#include "callprofiling_types.h"
#include "callprofiling.h"

#ifdef _MPI
#include <mpi.h>
#endif

int main(int argc, char **argv) {
#if defined(_MPI)
   PMPI_Init(&argc, &argv);
#else
   (void) argc;
   (void) argv;
#endif

   environment_t environment;
   environment = vftr_read_environment();

   // build stacktree
   stacktree_t stacktree = vftr_new_stacktree();
#define NTHREADS 10
   int threads[NTHREADS] = {4,8,1,5,6,7,0,9,3,2};
   for (int ithread=0; ithread<NTHREADS; ithread++) {
      vftr_new_profile(ithread, &(stacktree.stacks[0].profiling));
      profile_t *profile = stacktree.stacks[0].profiling.profiles+ithread;
      vftr_accumulate_callprofiling(&(profile->callProf),
                                    ithread*ithread, // calls
                                    42*ithread, // cycles
                                    137*ithread);
   }

   for (int ithread=0; ithread<NTHREADS; ithread++) {
      fprintf(stdout, "Thread: %d ", ithread);
      profile_t *profile = stacktree.stacks[0].profiling.profiles+ithread;
      vftr_print_callprofiling(stdout, profile->callProf);
   }

   vftr_stacktree_free(&stacktree);
   vftr_environment_free(&environment);
#ifdef _MPI
   PMPI_Finalize();
#endif

   return 0;
}