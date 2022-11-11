#include <stdlib.h>
#include <string.h>

#include "accprofiling_types.h"

accprofile_t vftr_new_accprofiling () {
   accprofile_t prof;
   prof.event_type = acc_ev_none;
   prof.line_start = 0;
   prof.line_end = 0;
   prof.copied_bytes = 0;
   prof.source_file = NULL;
   prof.func_name = NULL;
   prof.var_name = NULL;
   prof.kernel_name = NULL;
   return prof;
}

void vftr_accumulate_accprofiling (accprofile_t *prof, acc_event_t ev,
                                   int line_start, int line_end,
                                   const char *source_file, const char *func_name,
                                   const char *kernel_name, const char *var_name, size_t copied_bytes) {
   prof->event_type = ev;
   prof->line_start = line_start;
   prof->line_end = line_end;
   if (source_file != NULL && prof->source_file == NULL) {
      prof->source_file = (char*)malloc((strlen(source_file) + 1) * sizeof(char));
      strcpy (prof->source_file, source_file);
   }
   if (func_name != NULL && prof->func_name == NULL) {
      prof->func_name = (char*)malloc((strlen(func_name) + 1) * sizeof(char));
      strcpy (prof->func_name, func_name);
   }
   if (kernel_name != NULL && prof->kernel_name == NULL) {
      prof->kernel_name = (char*)malloc((strlen(kernel_name) + 1) * sizeof(char));
      strcpy (prof->kernel_name, kernel_name);
   } 
   if (var_name != NULL && prof->var_name == NULL) {
      prof->var_name = (char*)malloc((strlen(var_name) + 1) * sizeof(char));
      strcpy (prof->var_name, var_name);
   }

   prof->copied_bytes += copied_bytes;
}

accprofile_t vftr_add_accprofiles (accprofile_t profA, accprofile_t profB) {
   // Only the amount of bytes moved can differ between the profiles
   accprofile_t profC;
   profC.event_type = profA.event_type; 
   profC.var_name = profA.var_name;
   profC.kernel_name = profA.kernel_name;
   profC.copied_bytes = profA.copied_bytes + profB.copied_bytes;
}

void vftr_accprofiling_free (accprofile_t *prof_ptr) {
   if (prof_ptr->source_file != NULL) free (prof_ptr->source_file);
   if (prof_ptr->var_name != NULL) free (prof_ptr->var_name);
   if (prof_ptr->kernel_name != NULL) free (prof_ptr->kernel_name);
}
