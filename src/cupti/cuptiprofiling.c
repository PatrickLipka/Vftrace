#include <stdlib.h>
#include <stdio.h>

#include "cuptiprofiling_types.h"
#include "callbacks.h"

cuptiprofile_t vftr_new_cuptiprofiling() {
  cuptiprofile_t prof;
  cudaEventCreate (&(prof.start));
  cudaEventCreate (&(prof.stop));
  prof.cbid = 0;
  prof.n_calls = 0;
  prof.t_ms = 0;
  prof.t_vftr = 0;
  prof.memcpy_bytes[0] = 0;
  prof.memcpy_bytes[1] = 0; 
  return prof;
}

void vftr_accumulate_cuptiprofiling (cuptiprofile_t *prof, int cbid, int n_calls, long long t_vftr,
                                     float t_ms, int mem_dir, uint64_t memcpy_bytes) {
   prof->cbid = cbid;
   prof->n_calls += n_calls;
   if (cbid == 211) printf ("Accumulate 211: %lld %f %d\n", t_vftr, t_ms,
                            vftr_cupti_cbid_belongs_to_class (cbid, T_CUPTI_COMP));
   prof->t_vftr += t_vftr;
   prof->t_ms += t_ms;
   if (mem_dir != CUPTI_NOCOPY) prof->memcpy_bytes[mem_dir] += memcpy_bytes;
}

void vftr_cuptiprofiling_free(cuptiprofile_t *prof_ptr) {
  (void)prof_ptr;
}
