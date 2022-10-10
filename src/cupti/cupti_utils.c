#include <stdio.h>

#include <cuda_runtime_api.h>

#include "vftrace_state.h"

void vftr_show_used_gpu_info (FILE *fp) {
   struct cudaDeviceProp prop;
   int n_gpus = vftrace.cupti_state.n_devices;
   char *gpu_names[n_gpus];
   
   for (int i = 0; i < n_gpus; i++) {
      cudaGetDeviceProperties (&prop, i);
      gpu_names[i] = prop.name;
   }

   bool all_gpu_same = true;
   for (int i = 0; i < n_gpus; i++) {
      if (strcmp(gpu_names[i], prop.name)) {
	  all_gpu_same = false;
          break;
      }
   }

   fprintf (fp, "\n");
   
   if (all_gpu_same) {
      fprintf (fp, "Using %d GPUs: %s\n", n_gpus, prop.name);
   } else {
      fprintf (fp, "Using %d GPUs: \n", n_gpus);
      for (int i = 0; i < n_gpus; i++) {
         fprintf (fp, "   %d: %s\n", i, gpu_names[i]);
      }
   }
   
   char *visible_devices = getenv("CUDA_VISIBLE_DEVICES");
   fprintf (fp, "Visible GPUs: %s\n", visible_devices == NULL ? "all" : visible_devices);
   
}


