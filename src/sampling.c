#include <stdlib.h>
#include <stdio.h>

#include "environment_types.h"
#include "process_types.h"
#include "sampling_types.h"

#include "vfdfiles.h"

sampling_t vftr_new_sampling(environment_t environment) {
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
      sampling.nextsampletime = 0;
      sampling.function_samplecount = 0;
      sampling.message_samplecount = 0;
      sampling.stacktable_offset = 0;
      sampling.samples_offset = 0;
   } else {
      sampling.vfdfilename = NULL;
      sampling.vfdfilefp = NULL;
      sampling.iobuffer = NULL;
      sampling.nextsampletime = 0;
      sampling.function_samplecount = 0;
      sampling.message_samplecount = 0;
      sampling.stacktable_offset = 0;
      sampling.samples_offset = 0;
   }
   return sampling;
}

void vftr_sampling_free(sampling_t *sampling) {
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
}

void vftr_finalize_sampling(sampling_t *sampling, environment_t environment,
                            process_t process) {
   // TODO: update vfdheader


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

   vftr_sampling_free(sampling);
}
