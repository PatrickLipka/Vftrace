#include <stdlib.h>
#include <stdio.h>

#include "environment_types.h"
#include "environment.h"
#include "stack_types.h"
#include "collated_stack_types.h"
#include "collate_stacks.h"
#include "collate_profiles.h"
#include "callprofiling_types.h"

#include "callbacks.h"
#include "cuptiprofiling_types.h"

#include "dummy_stacktree.h"

// COMPUTE_CBIDS: CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020,
//                CUPTI_RUNTIME_TRACE_CBID_cudaLaunchKernel_v7000
// MEMCPY_CBIDS:  CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy_v3020,
//                CUPTI_RUNTIME_TRACE_CBID_cudaMemcpyAsync_v3020

int main(int argc, char **argv) {

   environment_t environment;
   environment = vftr_read_environment();

   vftr_init_dummy_stacktree (10);
   vftr_register_dummy_call_stack ("func0<init", 1);
   vftr_register_dummy_call_stack ("cudafunc1<init", 2);
   vftr_register_dummy_cupti_stack ("cudafunc1<init", CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020,
                                    1000.0, CUPTI_NOCOPY, 0);
   vftr_register_dummy_cupti_stack ("cudafunc2<func0<init", CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy_v3020,
				    1000.0, CUPTI_COPY_IN, 1048576);
   vftr_register_dummy_cupti_stack ("cudafunc3<func0<init", CUPTI_RUNTIME_TRACE_CBID_cudaMemcpy_v3020,
				    20000.0, CUPTI_COPY_OUT, 524288);
   vftr_register_dummy_call_stack ("cudafunc1<init", 2);
   for (int i = 0; i < 4; i++) {
      vftr_register_dummy_cupti_stack ("cudafunc1<init", CUPTI_RUNTIME_TRACE_CBID_cudaLaunch_v3020,
                                       1000.0, CUPTI_NOCOPY, 0);
   }


   stacktree_t stacktree = vftr_get_dummy_stacktree();
   
   collated_stacktree_t collated_stacktree = vftr_collate_stacks(&stacktree);
   vftr_collate_profiles(&collated_stacktree, &stacktree);

   for (int istack = 0; istack < collated_stacktree.nstacks; istack++) {
      collated_stack_t this_stack = collated_stacktree.stacks[istack];
      collated_callprofile_t callprof = this_stack.profile.callprof;
      cuptiprofile_t cuptiprof = this_stack.profile.cuptiprof;
      printf ("istack: %d\n", istack);
      printf ("name: %s\n", this_stack.name);
      printf ("callprof.t_excl: %lld\n", callprof.time_excl_nsec);
      printf ("callprof.calls: %d\n", callprof.calls);
      printf ("cuptiprof.cbid: %d\n", cuptiprof.cbid);
      printf ("cuptiprof.t_ms: %.3f\n", cuptiprof.t_ms);
      printf ("cuptiprof.memcpy_in: %lld\n", cuptiprof.memcpy_bytes[CUPTI_COPY_IN]);
      printf ("cuptiprof.memcpy_out: %lld\n", cuptiprof.memcpy_bytes[CUPTI_COPY_OUT]);
      printf ("------------------------------------\n");
   }
}
