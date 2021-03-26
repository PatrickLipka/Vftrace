/*
   This file is part of Vftrace.

   Vftrace is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   Vftrace is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "vftr_hwcounters.h"
#include "vftr_environment.h"
#include "vftr_filewrite.h"
#include "vftr_fileutils.h"

vftr_envs_t vftr_environment;

env_var_int_t *vftr_read_env_int (char *env_name, int val_default) {
    char *s;
    env_var_int_t *var;
    var = (env_var_int_t*)malloc (sizeof (env_var_int_t));
    if ((s = getenv (env_name))) {
	var->value = atoi(s);
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_int (FILE *fp, char *env_name, env_var_int_t *var) {
	char display_name[VFTR_ENV_VAR_MAX_LENGTH + 1];
	if (var->set) {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s", env_name);
	} else {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s(default)", env_name);
	}
	fprintf (fp, "%s: %d\n", display_name, var->value);	
}

/**********************************************************************/

env_var_long_t *vftr_read_env_long (char *env_name, long val_default) {
    char *s;
    env_var_long_t *var;
    var = (env_var_long_t*)malloc (sizeof (env_var_long_t));
    if ((s = getenv (env_name))) {
	var->value = atol(s);
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_long (FILE *fp, char *env_name, env_var_long_t *var) {
	char *s = strdup(env_name);
	if (!var->set) strcat (s, strdup ("(default)"));
	fprintf (fp, "%s: %ld\n", s, var->value);
}

/**********************************************************************/

env_var_long_long_t *vftr_read_env_long_long (char *env_name, long long val_default) {
    char *s;
    env_var_long_long_t *var;
    var = (env_var_long_long_t*)malloc (sizeof (env_var_long_long_t));
    if ((s = getenv (env_name))) {
	var->value = atoll(s);
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_long_long (FILE *fp, char *env_name, env_var_long_long_t *var) {
	char display_name[VFTR_ENV_VAR_MAX_LENGTH + 1];
	if (var->set) {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s", env_name);
	} else {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s(default)", env_name);
	}
	fprintf (fp, "%s: %lld\n", display_name, var->value);
}

/**********************************************************************/

env_var_bool_t *vftr_read_env_bool (char *env_name, bool val_default) {
    env_var_bool_t *var;
    var = (env_var_bool_t*)malloc (sizeof (env_var_bool_t));
    char *s;
    if ((s = getenv (env_name))) {
        // convert string to only lowercase to ease comparison
        char *s_lower = strdup(s);
        for (int i = 0; i < strlen(s_lower); i++) {
            s_lower[i] = tolower (s_lower[i]);
        }
        if (!strcmp(s_lower, "1") ||
            !strcmp(s_lower, "yes") ||
            !strcmp(s_lower, "on")) {
           var->value = true;
        } else if (!strcmp(s_lower, "0") ||
                   !strcmp(s_lower, "no") ||
                   !strcmp(s_lower, "off")) {
           var->value = false;
        } else {
           var->value = false;
        }
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_bool (FILE *fp, char *env_name, env_var_bool_t *var) {
	char display_name[VFTR_ENV_VAR_MAX_LENGTH + 1];
	if (var->set) {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s", env_name);
	} else {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s(default)", env_name);
	}
	fprintf (fp, "%s: %s\n", display_name, vftr_bool_to_string (var->value));
}

/**********************************************************************/

env_var_double_t *vftr_read_env_double (char *env_name, double val_default) {
    char *s;
    env_var_double_t *var;
    var = (env_var_double_t*)malloc (sizeof (env_var_double_t));
    if ((s = getenv (env_name))) {
	sscanf (s, "%lf", &var->value);
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_double (FILE *fp, char *env_name, env_var_double_t *var) {
	char display_name[VFTR_ENV_VAR_MAX_LENGTH + 1];
	if (var->set) {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s", env_name);
	} else {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s(default)", env_name);
	}
	fprintf (fp, "%s: %4.2f\n", display_name, var->value);
}	

/**********************************************************************/

env_var_string_t *vftr_read_env_string (char *env_name, char *val_default) {
    char *s;
    env_var_string_t *var;
    var = (env_var_string_t*)malloc (sizeof (env_var_string_t));
    if ((s = getenv (env_name))) {
	var->value = s;
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

void vftr_print_env_string (FILE *fp, char *env_name, env_var_string_t *var) {
	char display_name[VFTR_ENV_VAR_MAX_LENGTH];
	if (var->set) {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s", env_name);
	} else {
		snprintf (display_name, VFTR_ENV_VAR_MAX_LENGTH, "%s(default)", env_name);
	}
	fprintf (fp, "%s: %s\n", display_name, var->value);
}

/**********************************************************************/

env_var_regex_t *vftr_read_env_regex (char *env_name, regex_t *val_default) {
    char *s;
    env_var_regex_t *var;
    var = (env_var_regex_t*)malloc (sizeof (env_var_regex_t));
    if ((s = getenv (env_name))) {
	var->value = vftr_compile_regexp (s);
 	var->set = true;
    } else {
	var->value = val_default;
        var->set = false;
    }
    return var;
}

/**********************************************************************/

int vftr_profile_sorting_method () {
  char *s = vftr_environment.sort_profile_table->value;
  if (!strcmp (s, "EXCL_TIME")) {
     return SORT_EXCL_TIME;
  } else if (!strcmp (s, "INCL_TIME")) {
     return SORT_INCL_TIME;
  } else if (!strcmp (s, "CALLS")) {
     return SORT_N_CALLS;
  } else if (!strcmp (s, "STACK_ID")) {
     return SORT_STACK_ID;
  } else if (!strcmp (s, "OVERHEAD")) {
     return SORT_OVERHEAD;
  } else if (!strcmp (s, "OVERHEAD_RELATIVE")) {
     return SORT_OVERHEAD_RELATIVE;
  } else if (!strcmp (s, "NONE")) {
     return SORT_NONE;
  } else {
     return SORT_INVALID;
  } 
}

char *vftr_profile_sorting_method_string () {
  switch (vftr_profile_sorting_method()) {
    case SORT_EXCL_TIME:
      return "sorted by exclusive time";
    case SORT_INCL_TIME:
      return "sorted by inclusive time";
    case SORT_N_CALLS:
      return "sorted by number of calls";
    case SORT_STACK_ID:
      return "sorted by stack ID";
    case SORT_OVERHEAD:
      return "sorted by overhead time";
    case SORT_OVERHEAD_RELATIVE:
      return "sorted by relative overhead time";
    case SORT_NONE:
      return "unsorted";
  }
}

/**********************************************************************/

void vftr_read_environment () {
    vftr_environment.vftrace_off = vftr_read_env_bool ("VFTR_OFF", false);
    vftr_environment.do_sampling = vftr_read_env_bool ("VFTR_SAMPLING", false);
    vftr_environment.regions_precise = vftr_read_env_bool ("VFTR_REGIONS_PRECISE", true);
    vftr_environment.output_directory = vftr_read_env_string ("VFTR_OUT_DIRECTORY", ".");
    vftr_environment.logfile_basename = vftr_read_env_string ("VFTR_LOGFILE_BASENAME", NULL);
    vftr_environment.logfile_all_ranks = vftr_read_env_bool ("VFTR_LOGFILE_ALL_RANKS", false);
    vftr_environment.sampletime = vftr_read_env_double ("VFTR_SAMPLETIME", 0.005);
    vftr_environment.stoptime = vftr_read_env_long_long ("VFTR_STOPTIME", 7ll*24ll*60ll*60ll);
    vftr_environment.accurate_profile = vftr_read_env_bool ("VFTR_ACCURATE_PROFILE", false);
    vftr_environment.prof_truncate = vftr_read_env_bool ("VFTR_PROF_TRUNCATE", true);
    vftr_environment.prof_truncate_cutoff = vftr_read_env_double ("VFTR_PROF_TRUNCATE_CUTOFF", 98.0);
    vftr_environment.mpi_log = vftr_read_env_bool ("VFTR_MPI_LOG", false);
    vftr_environment.mpi_show_sync_time = vftr_read_env_bool ("VFTR_MPI_SHOW_SYNC_TIME", false);
    vftr_environment.signals_off = vftr_read_env_bool ("VFTR_SIGNALS_OFF", true);
    vftr_environment.bufsize = vftr_read_env_int ("VFTR_BUFSIZE", 8); 
    vftr_environment.runtime_profile_funcs = vftr_read_env_regex ("VFTR_RUNTIME_PROFILE_FUNCS", NULL);
    vftr_environment.include_only_regex = vftr_read_env_regex ("VFTR_INCLUDE_ONLY", NULL);
    vftr_environment.detail_until_cum_cycles = vftr_read_env_double ("VFTR_DETAIL_UNTIL_CUM_CYCLES", 90.0);
    vftr_environment.scenario_file = vftr_read_env_string ("VFTR_SCENARIO_FILE", NULL);
    vftr_environment.preciseregex = vftr_read_env_regex ("VFTR_PRECISE", NULL);
    vftr_environment.print_stack_profile = vftr_read_env_regex ("VFTR_PRINT_STACK_PROFILE", NULL);
    vftr_environment.license_verbose = vftr_read_env_bool ("VFTR_LICENSE_VERBOSE", false);
    vftr_environment.print_stacks_for = vftr_read_env_string ("VFTR_PRINT_STACKS_FOR", NULL);
    vftr_environment.print_loadinfo_for = vftr_read_env_string ("VFTR_PRINT_LOADINFO_FOR", NULL);
    vftr_environment.strip_module_names = vftr_read_env_bool ("VFTR_STRIP_MODULE_NAMES", false);
    vftr_environment.create_html = vftr_read_env_bool ("VFTR_CREATE_HTML", false);
    vftr_environment.sort_profile_table = vftr_read_env_string ("VFTR_SORT_PROFILE_TABLE", "EXCL_TIME");
    vftr_environment.show_overhead = vftr_read_env_bool ("VFTR_SHOW_FUNCTION_OVERHEAD", false);
    vftr_environment.print_env = vftr_read_env_bool ("VFTR_PRINT_ENVIRONMENT", false);
    vftr_environment.no_memtrace = vftr_read_env_bool ("VFTR_NO_MEMTRACE", false);
    vftr_environment.all_mpi_summary = vftr_read_env_bool ("VFTR_COMPLETE_MPI_SUMMARY", false);
    vftr_environment.show_stacks_in_profile = vftr_read_env_bool ("VFTR_SHOW_STACKS_IN_PROFILE", false);
}

/**********************************************************************/

void vftr_assert_environment () {
	assert (vftr_environment.bufsize->value > 0);

	if (vftr_environment.scenario_file->set) {
		FILE *fp;
		if (!(fp = fopen (vftr_environment.scenario_file->value, "r"))) {
			vftr_environment.scenario_file->set = false;
		} else {
			fclose(fp);
		}
	}

	// When neither VEPERF nor PAPI are activated, any given scenario file is ignored
#if !defined(HAS_SXHWC) && !defined(HAS_PAPI)
	vftr_environment.scenario_file->set = false;
#endif

	if (vftr_environment.detail_until_cum_cycles->value > 100.0 || 
	    vftr_environment.detail_until_cum_cycles->value < 0.0) {
		printf ("Warning: Invalid value for VFTR_DETAIL_UNTIL_CUM_CYLES (%lf). Set to default value of 90.0\n",
			vftr_environment.detail_until_cum_cycles->value);
		vftr_environment.detail_until_cum_cycles->value = 90.0;
	}

	// We check whether the number given after the ":", i.e. the group size, is positive.
	// Note that group_size == 0 is not supported, it is to be activated by leaving out the
	// colon instead.
	if (vftr_environment.print_stacks_for->set) {
		char *vftr_print_groups = vftr_environment.print_stacks_for->value;
		while (*vftr_print_groups) {
			int group_base, group_size;
			char *p;
			for (p = vftr_print_groups; *p && *p != ','; p++);
			switch (sscanf (vftr_print_groups, "%d:%d", &group_base, &group_size)) {
			case 2:
				if (group_size <= 0) {
					printf ("Warning: The group size in the environment variable VFTR_PRINT_STACKS_FOR=<group_base>:<group_size>	has to be greater than zero.\n");
					printf ("No additional stack info will be printed.\n");
					vftr_environment.print_stacks_for->set = false;
				}
			}
			vftr_print_groups = *p ? p + 1 : p;
		}		
	}
	// The same for VFTR_PRINT_LOADINFO_FOR
	if (vftr_environment.print_loadinfo_for->set) {
		char *vftr_mpi_groups = vftr_environment.print_loadinfo_for->value;
		while (*vftr_mpi_groups) {
			int group_base, group_size;
			char *p;
			for (p = vftr_mpi_groups; *p && *p != ','; p++);
			switch (sscanf (vftr_mpi_groups, "%d:%d", &group_base, &group_size)) {
			case 2:
				if (group_size <= 0) {
					printf ("Warning: The group size in the environment variable VFTR_PRINT_LOADINFO_FOR=<group_base>:<group_size>	has to be greater than zero.\n");
					printf ("No additional stack info will be printed.\n");
					vftr_environment.print_loadinfo_for->set = false;
				}
			}
			vftr_mpi_groups = *p ? p + 1 : p;
		}		
	}
        
       	if (vftr_environment.sort_profile_table->set) {
      	   int method = vftr_profile_sorting_method(); 
	   if (method == SORT_INVALID) {
               printf ("Warning: The profile table sorting method \"%s\" is not defined. Defaulting to TIME_EXCL.\n", vftr_environment.sort_profile_table->value);
	       vftr_environment.sort_profile_table->value = SORT_EXCL_TIME;
	   } else if ((method == SORT_OVERHEAD || method == SORT_OVERHEAD_RELATIVE) && !vftr_environment.show_overhead->value) {
	       printf ("Warning: You specified VFTR_SORT_PROFILE_TABLE=OVERHEAD(_RELATIVE), but overhead display is not enabled. Defaulting to TIME_EXLC.\n");
	       vftr_environment.sort_profile_table->value = SORT_EXCL_TIME;
	   }
        } 

	if (vftr_environment.prof_truncate_cutoff->set && !vftr_environment.prof_truncate->value) {
    	   printf ("Warning: Profile cutoff is given but VFTR_PROF_TRUNCATE is not set. Ignore!\n");
        } 

#ifndef _MPI
        if (vftr_environment.mpi_show_sync_time->set) {
            printf ("Warning: This is a serial Vftrace build (no MPI support)\n");
            printf ("VFTR_MPI_SHOW_SYNC_TIME is only supported with MPI. Switched off.\n");
            vftr_environment.mpi_show_sync_time->value = false;
        }
#endif

        if (vftr_environment.all_mpi_summary->value) {
           bool veto = vftr_environment.mpi_show_sync_time->value || vftr_environment.print_stack_profile->value;
           if (veto) {
              printf ("Warning: ");
              if (vftr_environment.mpi_show_sync_time->value) {
                 printf ("VFTR_MPI_SHOW_SYNC_TIME ");
              } else {
                 printf ("VFTR_PRINT_STACK_PROFILE ");
              }
	      printf ("is incompatible with VFTR_COMPLETE_MPI_SUMMARY.\n");
              printf ("DISABLED: VFTR_COMPLETE_MPI_SUMMARY.\n");
              vftr_environment.all_mpi_summary->value = false;
	   }
        }
}

/**********************************************************************/

// We check if Vftrace has been switched off, either by an environment variable
// or by a call to vftr_switch off. We need to check if the evironment is initialized
// beforehand, otherwise this yields a segfault in vftr_finalize, which is always called.
bool vftr_off () {
   return vftr_environment.vftrace_off->value;
}

void vftr_switch_off () {
   vftr_environment.vftrace_off->value = true;
}

bool vftr_env_do_sampling () {
   return vftr_environment.do_sampling->value;
}

bool vftr_env_no_memtrace () {
   return vftr_environment.no_memtrace->value;
}

bool vftr_env_need_display_functions () {
   return vftr_environment.print_stack_profile->value || vftr_environment.create_html->value || vftr_environment.all_mpi_summary->value;
}

bool vftr_env_distribute_gStack () {
   return vftr_environment.logfile_all_ranks->value ||
          vftr_environment.print_stack_profile->value ||
          vftr_environment.all_mpi_summary->value; 
}

/**********************************************************************/

void vftr_free_environment () {
	free (vftr_environment.vftrace_off);
	free (vftr_environment.do_sampling);
	free (vftr_environment.regions_precise);
	free (vftr_environment.output_directory);
	free (vftr_environment.logfile_basename);
	free (vftr_environment.logfile_all_ranks);
	free (vftr_environment.sampletime);
	free (vftr_environment.stoptime);
	free (vftr_environment.accurate_profile);
	free (vftr_environment.prof_truncate);
	free (vftr_environment.mpi_log);
	free (vftr_environment.mpi_show_sync_time);
	free (vftr_environment.signals_off);
	free (vftr_environment.bufsize);
	free (vftr_environment.runtime_profile_funcs);
	free (vftr_environment.include_only_regex);
	free (vftr_environment.detail_until_cum_cycles);
	free (vftr_environment.scenario_file);
	free (vftr_environment.preciseregex);
	free (vftr_environment.license_verbose);
	free (vftr_environment.print_stacks_for);
	free (vftr_environment.print_loadinfo_for);
	free (vftr_environment.strip_module_names);
	free (vftr_environment.create_html);
        free (vftr_environment.sort_profile_table);
	free (vftr_environment.show_overhead);
        free (vftr_environment.print_env);
	free (vftr_environment.no_memtrace);
        free (vftr_environment.all_mpi_summary);
        free (vftr_environment.show_stacks_in_profile);
}

/**********************************************************************/

// We leave out the regular expression in this printing function

void vftr_print_environment (FILE *fp) {
	vftr_print_env_bool (fp, "VFTR_OFF", vftr_environment.vftrace_off);
	vftr_print_env_bool (fp, "VFTR_SAMPLING", vftr_environment.do_sampling);
	vftr_print_env_bool (fp, "VFTR_REGIONS_PRECISE", vftr_environment.regions_precise);
	vftr_print_env_string (fp, "VFTR_OUT_DIRECTORY", vftr_environment.output_directory);
	vftr_print_env_string (fp, "VFTR_LOGFILE_BASENAME", vftr_environment.logfile_basename);
	vftr_print_env_bool (fp, "VFTR_LOGFILE_ALL_RANKS", vftr_environment.logfile_all_ranks);
	vftr_print_env_double (fp, "VFTR_SAMPLETIME", vftr_environment.sampletime);
	vftr_print_env_long_long (fp, "VFTR_STOPTIME", vftr_environment.stoptime);
	vftr_print_env_bool (fp, "VFTR_ACCURATE_PROFILE", vftr_environment.accurate_profile);
	vftr_print_env_bool (fp, "VFTR_PROF_TRUNCATE", vftr_environment.prof_truncate);
	vftr_print_env_bool (fp, "VFTR_MPI_LOG", vftr_environment.mpi_log);
	vftr_print_env_bool (fp, "VFTR_MPI_SHOW_SYNC_TIME", vftr_environment.mpi_show_sync_time);
	vftr_print_env_bool (fp, "VFTR_SIGNALS_OFF", vftr_environment.signals_off);
	vftr_print_env_int (fp, "VFTR_BUFSIZE", vftr_environment.bufsize);
	vftr_print_env_bool (fp, "VFTR_ACCURATE_PROFILE", vftr_environment.accurate_profile);
	vftr_print_env_double (fp, "VFTR_DETAIL_UNTIL_CUM_CYCLES" , vftr_environment.detail_until_cum_cycles);
	vftr_print_env_string (fp, "VFTR_SCENARIO_FILE", vftr_environment.scenario_file);
	vftr_print_env_bool (fp, "VFTR_LICENSE_VERBOSE", vftr_environment.license_verbose);
	vftr_print_env_string (fp, "VFTR_PRINT_STACKS_FOR", vftr_environment.print_stacks_for);
	vftr_print_env_string (fp, "VFTR_PRINT_LOADINFO_FOR", vftr_environment.print_loadinfo_for);
	vftr_print_env_bool (fp, "VFTR_STRIP_MODULE_NAMES", vftr_environment.strip_module_names);
	vftr_print_env_bool (fp, "VFTR_CREATE_HTML", vftr_environment.create_html);
	vftr_print_env_string (fp, "VFTR_SORT_PROFILE_TABLE", vftr_environment.sort_profile_table);
        vftr_print_env_bool (fp, "VFTR_SHOW_FUNCTION_OVERHEAD", vftr_environment.show_overhead);
        vftr_print_env_bool (fp, "VFTR_PRINT_ENVIRONMENT", vftr_environment.print_env);
        vftr_print_env_bool (fp, "VFTR_NO_MEMTRACE", vftr_environment.no_memtrace);
        vftr_print_env_bool (fp, "VFTR_COMPLETE_MPI_SUMMARY", vftr_environment.all_mpi_summary);
        vftr_print_env_bool (fp, "VFTR_SHOW_STACKS_IN_PROFILE", vftr_environment.show_stacks_in_profile);
}

/**********************************************************************/

int vftr_environment_test_1 (FILE *fp) {
	putenv ("VFTR_OFF=yes");
	vftr_read_environment ();
	vftr_assert_environment ();
	vftr_print_environment (fp);
	vftr_free_environment ();
		
	fprintf (fp, "***************************\n");
	putenv ("VFTR_OFF=no");
	putenv ("VFTR_SAMPLING=YES");
	putenv ("VFTR_REGIONS_PRECISE=0");
	putenv ("VFTR_MPI_LOG=on");
	putenv ("VFTR_OUT_DIRECTORY=\"foo/bar\"");
	putenv ("VFTR_BUFSIZE=1234");
	putenv ("VFTR_SAMPLETIME=12.34");
	
	vftr_read_environment ();
	vftr_assert_environment ();
	vftr_print_environment (fp);
	vftr_free_environment ();
	return 0;
}
