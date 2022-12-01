#include <stdio.h>
#include <string.h>

#include "vftrace_state.h"
#include "configuration_types.h"
#include "collated_stack_types.h"
#include "table_types.h"
#include "tables.h"
#include "sorting.h"

#include "papiprofiling_types.h"
#include "papi_calculator.h"

void vftr_write_papi_table (FILE *fp, collated_stacktree_t stacktree, config_t config) {

   collated_stack_t **sorted_stacks =
      vftr_sort_collated_stacks_for_prof(config, stacktree);

   int n_events = PAPI_num_events (vftrace.papi_state.eventset);
   //long long **counters = (long long**)malloc (n_events * sizeof(long long*));

   fprintf (fp, "\nRuntime PAPI profile\n");

   int n_observables = vftrace.papi_state.calculator.n_observables;

   int *calls = (int*)malloc(stacktree.nstacks * sizeof(int));
   char **func = (char**)malloc(stacktree.nstacks * sizeof(char*));
   double **observables = (double**)malloc(n_observables * sizeof(double*));
   for (int i = 0; i < n_observables; i++) {
      observables[i] = (double*)malloc(stacktree.nstacks * sizeof(double)); 
   }

   for (int istack = 0; istack < stacktree.nstacks; istack++) {
      collated_stack_t *this_stack = sorted_stacks[istack];
      collated_callprofile_t callprof = this_stack->profile.callprof;
      papiprofile_t papiprof = this_stack->profile.papiprof;

      calls[istack] = callprof.calls;
      func[istack] = this_stack->name;

      vftr_set_papi_calculator_counters (&(vftrace.papi_state.calculator), papiprof.counters);
      vftr_set_papi_calculator_builtin (&(vftrace.papi_state.calculator),
                                            PCALC_T, (double)callprof.time_excl_nsec * 1e-9);
      vftr_set_papi_calculator_builtin (&(vftrace.papi_state.calculator),
                                            PCALC_ONE, 1.0);

      vftr_print_papi_calculator_state (vftrace.papi_state.calculator);
      for (int i = 0; i < n_observables; i++) {
         observables[i][istack] = vftr_papi_calculator_evaluate (vftrace.papi_state.calculator, i);   
      }
   }

   table_t table = vftr_new_table();
   vftr_table_set_nrows (&table, stacktree.nstacks); 

   vftr_table_add_column (&table, col_int, "#Calls", "%d", 'c', 'r', (void*)calls);
   vftr_table_add_column (&table, col_string, "Func", "%s", 'c', 'r', (void*)func);
   for (int i = 0; i < n_observables; i++) {
      char *obs_name = config.papi.observables.obs_name.values[i];
      char *obs_unit = config.papi.observables.unit.values[i];
      int slen = strlen(obs_name) + strlen(obs_unit) + 4;
      char *header = (char*)malloc(slen * sizeof(char));
      snprintf (header, slen, "%s [%s]", obs_name, obs_unit);
      vftr_table_add_column(&table, col_double, header, "%lf", 'c', 'r', (void*)observables[i]);
      free (obs_name);
      free(obs_unit);
   }

   vftr_print_table (fp, table);
 
   vftr_table_free(&table);

   //for (int i = 0; i < stacktree.nstacks; i++) {
   //   papiprofile_t papiprof = stacktree.stacks[i].profile.papiprof;
   //   collated_callprofile_t callprof = stacktree.stacks[i].profile.callprof;
   //   fprintf (fp, "StackID %d: \n", i);
   //   for (int e = 0; e < n_events; e++) {
   //      fprintf (fp, "    %s: %lld / %lld\n", config.papi.counters.native_name.values[e],
   //                                            papiprof.counters[e], callprof.time_excl_nsec);
   //   }
   //       
   //   vftr_set_papi_calculator_counters (&(vftrace.papi_state.calculator), papiprof.counters);
   //   vftr_set_papi_calculator_counter_dbl (&(vftrace.papi_state.calculator),
   //                                         PCALC_T, (double)callprof.time_excl_nsec * 1e-9);
   //   vftr_set_papi_calculator_counter_dbl (&(vftrace.papi_state.calculator),
   //                                         PCALC_ONE, 1.0);

   //   vftr_print_papi_calculator_state (vftrace.papi_state.calculator);

   //   for (int e = 0; e < n_events; e++) {
   //      printf ("Observable: %lf\n", vftr_papi_calculator_evaluate (vftrace.papi_state.calculator, e));
   //   }
   //}
   //free(counters);
}

void vftr_write_event_descriptions (FILE *fp) {
   for (int i = 0; i < vftrace.papi_state.n_available_events; i++) {
      fprintf (fp, "%s: %s\n", vftrace.papi_state.event_names[i], vftrace.papi_state.event_descriptions[i]);
   }
}
