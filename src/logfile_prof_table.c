#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "timer_types.h"
#include "table_types.h"
#include "environment_types.h"
#include "stack_types.h"
#include "vftrace_state.h"

#include "filenames.h"
#include "license.h"
#include "config.h"
#include "environment.h"
#include "stacks.h"
#include "collate_stacks.h"
#include "tables.h"
#include "overheadprofiling_types.h"
#include "overheadprofiling.h"
#include "sorting.h"

int *vftr_logfile_prof_table_stack_calls_list(int nstacks, stack_t **stack_ptrs) {
   int *calls_list = (int*) malloc(nstacks*sizeof(int));

   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      calls_list[istack] = 0;
      for (int iprof=0; iprof<stack_ptr->profiling.nprofiles; iprof++) {
         profile_t *prof_ptr = stack_ptr->profiling.profiles+iprof;
         calls_list[istack] += prof_ptr->callProf.calls;
      }
   }
   return calls_list;
}

double *vftr_logfile_prof_table_stack_inclusive_time_list(int nstacks, stack_t **stack_ptrs) {
   double *inclusive_time_list = (double*) malloc(nstacks*sizeof(double));

   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      inclusive_time_list[istack] = 0.0;
      for (int iprof=0; iprof<stack_ptr->profiling.nprofiles; iprof++) {
         profile_t *prof_ptr = stack_ptr->profiling.profiles+iprof;
         inclusive_time_list[istack] += prof_ptr->callProf.time_usec;
      }
      inclusive_time_list[istack] *= 1.0e-6;
   }
   return inclusive_time_list;
}

double *vftr_logfile_prof_table_stack_exclusive_time_list(int nstacks, stack_t **stack_ptrs) {
   double *exclusive_time_list = (double*) malloc(nstacks*sizeof(double));

   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      exclusive_time_list[istack] = 0.0;
      for (int iprof=0; iprof<stack_ptr->profiling.nprofiles; iprof++) {
         profile_t *prof_ptr = stack_ptr->profiling.profiles+iprof;
         exclusive_time_list[istack] += prof_ptr->callProf.time_excl_usec;
      }
      exclusive_time_list[istack] *= 1.0e-6;
   }
   return exclusive_time_list;
}

double *vftr_logfile_prof_table_stack_exclusive_time_percentage_list(int nstacks, stack_t **stack_ptrs) {
   double *percent_list = (double*) malloc(nstacks*sizeof(double));
   long long total_time = 0ll;
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      for (int iprof=0; iprof<stack_ptr->profiling.nprofiles; iprof++) {
         profile_t *prof_ptr = stack_ptr->profiling.profiles+iprof;
         total_time += prof_ptr->callProf.time_excl_usec;
      }
   }
   double invtotal_time = 100.0/total_time;
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      percent_list[istack] = 0.0;
      for (int iprof=0; iprof<stack_ptr->profiling.nprofiles; iprof++) {
         profile_t *prof_ptr = stack_ptr->profiling.profiles+iprof;
         percent_list[istack] += prof_ptr->callProf.time_excl_usec;
      }
      percent_list[istack] *= invtotal_time;
   }
   return percent_list;
}

//double *vftr_logfile_prof_table_stack_overhead_time_list(int nstacks, stack_t **stack_ptrs) {
//   double *overhead_time_list = (double*) malloc(nstacks*sizeof(double));
//
//   for (int istack=0; istack<nstacks; istack++) {
//      stack_t *stack_ptr = stack_ptrs[istack];
//      profile_t *prof_ptr = stack_ptr->profiling.profiles;
//      overhead_time_list[istack] = prof_ptr->overheadProf.hook_usec;
//      overhead_time_list[istack] *= 1.0e-6;
//   }
//   return overhead_time_list;
//}

char **vftr_logfile_prof_table_stack_function_name_list(int nstacks, stack_t **stack_ptrs) {
   char **name_list = (char**) malloc(nstacks*sizeof(char*));
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      name_list[istack] = stack_ptr->name;
   }
   return name_list;
}

char **vftr_logfile_prof_table_stack_caller_name_list(stacktree_t stacktree, stack_t **stack_ptrs) {
   int nstacks = stacktree.nstacks;
   char **name_list = (char**) malloc(nstacks*sizeof(char*));
   // the init function is never called
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      int callerID = stack_ptr->caller;
      if (callerID >= 0) {
         name_list[istack] = stacktree.stacks[callerID].name;
      } else {
         name_list[istack] = "----";
      }
   }
   return name_list;
}

int *vftr_logfile_prof_table_stack_stackID_list(int nstacks, stack_t **stack_ptrs) {
   int *id_list = (int*) malloc(nstacks*sizeof(int));
   for (int istack=0; istack<nstacks; istack++) {
      stack_t *stack_ptr = stack_ptrs[istack];
      id_list[istack] = stack_ptr->gid;
   }
   return id_list;
}

void vftr_write_logfile_profile_table(FILE *fp, stacktree_t stacktree,
                                      environment_t environment) {

   // first sort the stacktree according to the set environment variables
   stack_t **sorted_stacks = vftr_sort_stacks_for_prof(environment, stacktree);

   fprintf(fp, "\nRuntime profile\n");

   table_t table = vftr_new_table();
   vftr_table_set_nrows(&table, stacktree.nstacks);

   int *calls = vftr_logfile_prof_table_stack_calls_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_int, "Calls", "%d", 'c', 'r', (void*) calls);

   double *excl_time = vftr_logfile_prof_table_stack_exclusive_time_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_double, "t_excl/s", "%.3f", 'c', 'r', (void*) excl_time);

   double *excl_timer_perc = vftr_logfile_prof_table_stack_exclusive_time_percentage_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_double, "t_excl/%", "%.1f", 'c', 'r', (void*) excl_timer_perc);

   double *incl_time = vftr_logfile_prof_table_stack_inclusive_time_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_double, "t_incl/s", "%.3f", 'c', 'r', (void*) incl_time);

   char **function_names = vftr_logfile_prof_table_stack_function_name_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_string, "Function", "%s", 'c', 'r', (void*) function_names);

   char **caller_names = vftr_logfile_prof_table_stack_caller_name_list(stacktree, sorted_stacks);
   vftr_table_add_column(&table, col_string, "Caller", "%s", 'c', 'r', (void*) caller_names);

   int *stack_IDs = vftr_logfile_prof_table_stack_stackID_list(stacktree.nstacks, sorted_stacks);
   vftr_table_add_column(&table, col_int, "ID", "%d", 'c', 'r', (void*) stack_IDs);

   vftr_print_table(fp, table);

   vftr_table_free(&table);
   free(calls);
   free(excl_time);
   free(excl_timer_perc);
   free(incl_time);
   free(function_names);
   free(caller_names);
   free(stack_IDs);

   free(sorted_stacks);
}