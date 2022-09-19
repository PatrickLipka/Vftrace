#include <stdlib.h>
#include <stdio.h>

#include "self_profile.h"
#include "environment_types.h"
#include "process_types.h"
#include "sampling_types.h"
#include "timer_types.h"

#include "vfdfiles.h"

sampling_t vftr_new_sampling(environment_t environment) {
   SELF_PROFILE_START_FUNCTION;
   sampling_t sampling;
   sampling.do_sampling = environment.do_sampling.value.bool_val;
   if (sampling.do_sampling) {
      // for now get a preliminary filename
      // that will be corrected later.
      // Parts of the filename might only be known later
      sampling.vfdfilename = vftr_get_preliminary_vfdfile_name(environment);
      sampling.vfdfilefp = vftr_open_vfdfile(sampling.vfdfilename);
      sampling.iobuffer = vftr_attach_iobuffer_vfdfile(sampling.vfdfilefp,
                                                       environment);
      sampling.interval = (long long) (environment.sampletime.value.double_val*1.0e9);
      sampling.nextsampletime = 0;
      sampling.function_samplecount = 0;
      sampling.message_samplecount = 0;
      sampling.stacktable_offset = 0;
      sampling.samples_offset = 0;

      // already write the incomplete header
      vftr_write_incomplete_vfd_header(&sampling);
   } else {
      sampling.vfdfilename = NULL;
      sampling.vfdfilefp = NULL;
      sampling.iobuffer = NULL;
      sampling.nextsampletime = 0;
      sampling.interval = 0;
      sampling.function_samplecount = 0;
      sampling.message_samplecount = 0;
      sampling.stacktable_offset = 0;
      sampling.samples_offset = 0;
   }
   SELF_PROFILE_END_FUNCTION;
   return sampling;
}

void vftr_sampling_free(sampling_t *sampling) {
   SELF_PROFILE_START_FUNCTION;
   if (sampling->do_sampling) {
      sampling->do_sampling = false;
      free(sampling->vfdfilename);
      free(sampling->iobuffer);
      sampling->vfdfilename = NULL;
      sampling->vfdfilefp = NULL;
      sampling->iobuffer = NULL;
      sampling->nextsampletime = 0;
      sampling->function_samplecount = 0;
      sampling->message_samplecount = 0;
      sampling->stacktable_offset = 0;
      sampling->samples_offset = 0;
   }
   SELF_PROFILE_END_FUNCTION;
}

void vftr_finalize_sampling(sampling_t *sampling,
                            environment_t environment, process_t process,
                            time_strings_t timestrings, double runtime) {
   SELF_PROFILE_START_FUNCTION;
   if (sampling->do_sampling) {
      vftr_write_vfd_stacks(sampling, process.stacktree);
      vftr_write_vfd_threadtree(sampling, process.threadtree);
      vftr_update_vfd_header(sampling, process, timestrings, runtime);

      // Close the vfdfile
      int status = fclose(sampling->vfdfilefp);
      if (status != 0) {
         perror(sampling->vfdfilename);
      }

      // get the final filename and
      // move the preliminary file to its final location
      char *vfdfilename = vftr_get_vfdfile_name(environment,
                                                process.processID,
                                                process.nprocesses);
      status = vftr_rename_vfdfile(sampling->vfdfilename, vfdfilename);
      free(vfdfilename);

      vftr_sampling_free(sampling);
   }
   SELF_PROFILE_END_FUNCTION;
}

void vftr_sample_function_entry(sampling_t *sampling, stack_t stack,
                                long long timestamp) {
   SELF_PROFILE_START_FUNCTION;
   if (sampling->do_sampling &&
       (timestamp > sampling->nextsampletime || stack.precise)) {
      vftr_write_vfd_function_sample(sampling, samp_function_entry,
                                     stack.lid, timestamp);

      sampling->function_samplecount++;
      sampling->nextsampletime = timestamp + sampling->interval;
   }
   SELF_PROFILE_END_FUNCTION;
}

void vftr_sample_function_exit(sampling_t *sampling, stack_t stack,
                               long long timestamp) {
   SELF_PROFILE_START_FUNCTION;
   if (sampling->do_sampling &&
       (timestamp > sampling->nextsampletime || stack.precise)) {
      vftr_write_vfd_function_sample(sampling, samp_function_exit,
                                     stack.lid, timestamp);

      sampling->function_samplecount++;
      sampling->nextsampletime = timestamp + sampling->interval;
   }
   SELF_PROFILE_END_FUNCTION;
}
