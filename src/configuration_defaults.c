#include <stdlib.h>
#include <stdbool.h>

#include <string.h>

#include "configuration_types.h"
#include "regular_expressions.h"
#include "range_expand.h"

config_bool_t vftr_set_config_bool_default(char *name, bool value) {
   config_bool_t cfg_bool;
   cfg_bool.name = strdup(name);
   cfg_bool.set = false;
   cfg_bool.value = value;
   return cfg_bool;
}

config_int_t vftr_set_config_int_default(char *name, int value) {
   config_int_t cfg_int;
   cfg_int.name = strdup(name);
   cfg_int.set = false;
   cfg_int.value = value;
   return cfg_int;
}

config_float_t vftr_set_config_float_default(char *name, double value) {
   config_float_t cfg_float;
   cfg_float.name = strdup(name);
   cfg_float.set = false;
   cfg_float.value = value;
   return cfg_float;
}

config_string_t vftr_set_config_string_default(char *name, char *value) {
   config_string_t cfg_string;
   cfg_string.name = strdup(name);
   cfg_string.set = false;
   cfg_string.value = value == NULL ? NULL : strdup(value);
   return cfg_string;
}

config_string_list_t vftr_set_config_string_list_default (char *name) {
   config_string_list_t cfg_string_list;
   cfg_string_list.name = strdup(name);
   cfg_string_list.set = false;
   cfg_string_list.n_elements = 0;
   cfg_string_list.values = NULL;
   return cfg_string_list;
}

config_regex_t vftr_set_config_regex_default(char *name) {
   config_regex_t cfg_regex;
   cfg_regex.name = strdup(name);
   cfg_regex.set = false;
   cfg_regex.value = NULL;
   cfg_regex.regex = NULL;
   return cfg_regex;
}

config_sort_table_t vftr_set_config_sort_table_default(char *column, bool ascending) {
   config_sort_table_t cfg_sort_table;
   cfg_sort_table.name = strdup("sort_table");
   cfg_sort_table.set = false;
   cfg_sort_table.column =
      vftr_set_config_string_default("column", column);
   cfg_sort_table.ascending =
      vftr_set_config_bool_default("ascending", ascending);
   return cfg_sort_table;
}

config_profile_table_t vft_set_config_profile_table_default() {
   config_profile_table_t cfg_profile_table;
   cfg_profile_table.name = strdup("profile_table");
   cfg_profile_table.set = false;
   cfg_profile_table.show_table =
      vftr_set_config_bool_default("show_table", true);
   cfg_profile_table.show_calltime_imbalances =
      vftr_set_config_bool_default("show_calltime_imbalances", false);
   cfg_profile_table.show_callpath =
      vftr_set_config_bool_default("show_callpath", false);
   cfg_profile_table.show_overhead =
      vftr_set_config_bool_default("show_overhead", false);
   cfg_profile_table.sort_table =
      vftr_set_config_sort_table_default("time_excl", false);
   return cfg_profile_table;
}

config_name_grouped_profile_table_t vftr_set_config_name_grouped_profile_table_default() {
   config_name_grouped_profile_table_t cfg_profile_table;
   cfg_profile_table.name = strdup("name_grouped_profile_table");
   cfg_profile_table.set = false;
   cfg_profile_table.show_table =
      vftr_set_config_bool_default("show_table", false);
   cfg_profile_table.max_stack_ids =
      vftr_set_config_int_default("max_stack_ids", 8);
   cfg_profile_table.sort_table =
      vftr_set_config_sort_table_default("time_excl", false);
   return cfg_profile_table;
}

config_sampling_t vftr_set_config_sampling_default() {
   config_sampling_t cfg_sampling;
   cfg_sampling.name = strdup("sampling");
   cfg_sampling.set = false;
   cfg_sampling.active =
      vftr_set_config_bool_default("active", false);
   cfg_sampling.sample_interval =
      vftr_set_config_float_default("sample_interval", 0.005);
   cfg_sampling.outbuffer_size =
      vftr_set_config_int_default("outbuffer_size", 8);
   cfg_sampling.precise_functions =
      vftr_set_config_regex_default("precise_functions");
   return cfg_sampling;
}

config_mpi_t vftr_set_config_mpi_default() {
   config_mpi_t cfg_mpi;
   cfg_mpi.name = strdup("mpi");
   cfg_mpi.set = false;
   cfg_mpi.show_table =
      vftr_set_config_bool_default("show_table", true);
   cfg_mpi.log_messages = 
      vftr_set_config_bool_default("log_messages", true);
   cfg_mpi.only_for_ranks =
      vftr_set_config_string_default("only_for_ranks", "all");
   cfg_mpi.show_sync_time =
      vftr_set_config_bool_default("show_sync_time", false);
   cfg_mpi.show_callpath =
      vftr_set_config_bool_default("show_callpath", false);
   cfg_mpi.sort_table =
      vftr_set_config_sort_table_default("none", false);
   return cfg_mpi;
}

config_cuda_t vftr_set_config_cuda_default() {
   config_cuda_t cfg_cuda;
   cfg_cuda.name = strdup("cuda");
   cfg_cuda.set = false;
   cfg_cuda.show_table =
      vftr_set_config_bool_default("show_table", true);
   cfg_cuda.sort_table =
      vftr_set_config_sort_table_default("time", false);
   return cfg_cuda;
}

config_accprof_t vftr_set_config_accprof_default() {
   config_accprof_t cfg_accprof;
   cfg_accprof.name = strdup("openacc");
   cfg_accprof.set = false;
   cfg_accprof.show_table = vftr_set_config_bool_default("show_table", true);
   cfg_accprof.show_event_details = vftr_set_config_bool_default("show_event_details", false);
   cfg_accprof.sort_table =
      vftr_set_config_sort_table_default("time", false);
   return cfg_accprof;
}

config_hwcounters_t vftr_set_config_hwcounters_default() {
   config_hwcounters_t cfg_hwc;
   cfg_hwc.name = strdup("counters");
   cfg_hwc.set = false;
   cfg_hwc.native_name = vftr_set_config_string_list_default("native");
   cfg_hwc.preset_name = vftr_set_config_string_list_default("preset");
   cfg_hwc.symbol = vftr_set_config_string_list_default("symbol");
   return cfg_hwc;
}

config_hwobservables_t vftr_set_config_hwobservables_default() {
   config_hwobservables_t cfg_hwobs;
   cfg_hwobs.name = strdup("observables");
   cfg_hwobs.set = false;
   cfg_hwobs.obs_name = vftr_set_config_string_list_default("name");
   cfg_hwobs.formula_expr = vftr_set_config_string_list_default("formula");
   cfg_hwobs.unit = vftr_set_config_string_list_default("unit");
   return cfg_hwobs;
}

config_papi_t vftr_set_config_papi_default() {
   config_papi_t cfg_papi;
   cfg_papi.name = strdup("papi");
   cfg_papi.set = false;
   cfg_papi.disable = vftr_set_config_bool_default ("disable", false);
   cfg_papi.show_tables = vftr_set_config_bool_default ("show_tables", true);
   cfg_papi.show_counters = vftr_set_config_bool_default ("show_counters", false);
   cfg_papi.sort_by_column = vftr_set_config_int_default ("sort_by_column", 0);
   cfg_papi.counters = vftr_set_config_hwcounters_default();
   cfg_papi.observables = vftr_set_config_hwobservables_default();
   return cfg_papi;
}

config_t vftr_set_config_default() {
   config_t config;
   config.off =
      vftr_set_config_bool_default("off", false);
   config.output_directory =
      vftr_set_config_string_default("output_directory", ".");
   config.outfile_basename =
      vftr_set_config_string_default("outfile_basename", NULL);
   config.logfile_for_ranks =
      vftr_set_config_string_default("logfile_for_ranks", "none");
   config.print_config =
      vftr_set_config_bool_default("print_config", true);
   config.strip_module_names =
      vftr_set_config_bool_default("strip_module_names", false);
   config.demangle_cxx =
      vftr_set_config_bool_default("demangle_cxx", false);
   config.include_cxx_prelude =
      vftr_set_config_bool_default("include_cxx_prelude" , false);
   config.profile_table =
      vft_set_config_profile_table_default();
   config.name_grouped_profile_table =
      vftr_set_config_name_grouped_profile_table_default();
   config.sampling =
      vftr_set_config_sampling_default();
   config.mpi =
      vftr_set_config_mpi_default();
   config.cuda =
      vftr_set_config_cuda_default();
   config.accprof = vftr_set_config_accprof_default();
   config.papi = vftr_set_config_papi_default();
   config.config_file_path = NULL;
   config.valid = true;
   return config;
}

