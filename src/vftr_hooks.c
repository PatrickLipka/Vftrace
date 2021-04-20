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

#define _GNU_SOURCE

#include <assert.h>
#include <string.h>
#include <signal.h>

#include <stdbool.h>
#include "vftr_symbols.h"
#include "vftr_hwcounters.h"
#include "vftr_setup.h"
#include "vftr_environment.h"
#include "vftr_hooks.h"
#include "vftr_dlopen.h"
#include "vftr_filewrite.h"
#include "vftr_functions.h"
#include "vftr_pause.h"
#include "vftr_timer.h"
#include "vftr_stacks.h"
#include "vftr_clear_requests.h"
#include "vftr_sorting.h"
#include "vftr_socket.h"

bool vftr_profile_wanted = false;

void vftr_save_old_state () {
    int i,j;

    for (j = 0; j < vftr_stackscount; j++) {
        function_t *func = vftr_func_table[j];
        func->prof_previous.calls  = func->prof_current.calls;
        func->prof_previous.cycles = func->prof_current.cycles;
        func->prof_previous.time_excl = func->prof_current.time_excl;
        for (i = 0; i < vftr_n_hw_obs; i++) {
            func->prof_previous.event_count[i] = func->prof_current.event_count[i];
	}
    }
}

/**********************************************************************/

void vftr_function_entry (const char *s, void *addr, bool isPrecise) {
    int e, read_counters;
    unsigned long long timer, delta;
    unsigned long long cycles0;
    double wtime;
    static int vftr_init = 1;
    bool time_to_sample = false;
    function_t *caller, *func, *callee;
    profdata_t *prof_return;

    if (vftr_init) {
	vftr_initialize ();
	vftr_init = 0;
    }
    if (vftr_off() || vftr_paused) return;

    long long func_entry_time = vftr_get_runtime_usec();
    // log function entry and exit time to estimate the overhead time
    long long overhead_time_start = func_entry_time;
    timer = vftr_get_runtime_usec ();
    cycles0 = vftr_get_cycles() - vftr_initcycles;

    // This is the hook for shared libraries opened during the
    // application's runtime. The preloaded library vftr_dlopen.so
    // triggers this flag, which leads to a renewed creation of the
    // symbol table, containing the symbols of the dlopened library
    //
    if (lib_opened) {
	lib_opened = 0;
    	vftr_create_symbol_table (vftr_mpirank);
    }

    caller = vftr_fstack;
    // If the caller address equals the current address, we are
    // dealing with (simple) recursive function call. We filter
    // them out to avoid extensively large call stacks, e.g. 
    // in the viewer. 
    // In principle, we could just return from this function
    // in the given situation. However, the same stack is encountered
    // in the function exit as well, and its treatment needs to be 
    // avoided there too. This is easily solved by introducing the
    // counter "recursion_depth". For each recursive function entry,
    // it is incremented, for each recursive function exit it is decremented.
    // This way, in vftr_function_exit, when it is zero we know that
    // we are not dealing with a recursive function. 
    // 
    if (addr == caller->address) {
        caller->prof_current.calls++;
	caller->recursion_depth++;
        return;
    }

    //
    // Check if the function is in the table
    //
    callee = caller->callee;
    
    if (callee == NULL) {
        // No calls at all yet: add new function
        func = vftr_new_function(addr, s, caller, isPrecise);
    } else {
	// Search the function address in the function list
        func = callee;
        if (func->address != addr) {
           for ( ;; ) {
               func = func->next_in_level;
               if (func == callee || func->address == addr) {
           	  break;
               }
           }
           if (func == callee) {
               // No call from this callee yet: add new function
               func = vftr_new_function(addr, s, caller, isPrecise);
           }
        }
    }

    if (vftr_mpirank == vftr_environment.live_rank->value) {
      pthread_mutex_lock (&vftr_socket_lock_handle);
      vftr_socket_ident_level += 2;
      //printf ("Compare: %s %s\n", func->name, vftr_socket_recent_func);
      if (!strcmp(func->name, vftr_socket_recent_func)) {
        vftr_socket_it_count++;
      } else {
        vftr_socket_it_count = 0;
        vftr_socket_recent_func = func->name;
      }
      if (vftr_n_funcs_to_send < VFTR_SOCK_BUFSIZE) {
        vftr_funcs_to_send[vftr_n_funcs_to_send] = func->name;
        vftr_timestamps_to_send[vftr_n_funcs_to_send] = vftr_get_runtime_usec();
        vftr_idents[vftr_n_funcs_to_send] = vftr_socket_ident_level;
        vftr_its[vftr_n_funcs_to_send] = vftr_socket_it_count;
        vftr_n_funcs_to_send++;
      } else {
        for (int i = 1; i < vftr_n_funcs_to_send; i++) {
          vftr_funcs_to_send[i-1] = vftr_funcs_to_send[i];
          vftr_timestamps_to_send[i-1] = vftr_timestamps_to_send[i];
          vftr_idents[i-1] = vftr_idents[i];
          vftr_its[i-1] = vftr_its[i];
        }
        vftr_funcs_to_send[vftr_n_funcs_to_send - 1] = func->name;
        vftr_timestamps_to_send[vftr_n_funcs_to_send - 1] = vftr_get_runtime_usec();
        vftr_idents[vftr_n_funcs_to_send - 1] = vftr_socket_ident_level;
        vftr_its[vftr_n_funcs_to_send - 1] = vftr_socket_it_count;
      }
      pthread_mutex_unlock (&vftr_socket_lock_handle);
    }

    caller->callee = func; // Faster lookup next time around
    vftr_fstack = func; /* Here's where we are now */
    func->open = true;

    if (func->profile_this) {
        wtime = (vftr_get_runtime_usec() - vftr_overhead_usec) * 1.0e-6;
        vftr_write_stack_ascii (vftr_log, wtime, func, "profile before call to", 0);
        vftr_profile_wanted = true;
        int ntop;
        vftr_print_profile (vftr_log, NULL, &ntop, timer, 0, NULL);
        vftr_print_local_stacklist (vftr_func_table, vftr_log, ntop);
	vftr_save_old_state ();
    }


    // Is it time for the next sample?
    time_to_sample = (func_entry_time > vftr_nextsampletime) || func->precise;  

    read_counters = (func->return_to->detail || func->detail) &&
		    vftr_events_enabled && 
                    (time_to_sample || vftr_environment.accurate_profile->value);

    if (func->return_to) {
        prof_return = &func->return_to->prof_current;
        if (read_counters) {
           int ic = vftr_prof_data.ic;
           vftr_read_counters (vftr_prof_data.events[ic]);
           if (prof_return->event_count && func->return_to->detail) {
               for (e = 0; e < vftr_n_hw_obs; e++) {
                   long long delta = vftr_prof_data.events[ic][e] - vftr_prof_data.events[1-ic][e];
#ifdef __ve__
                   if (delta < 0) /* Handle counter overflow */
                       delta += e < 2 ? (long long) 0x000fffffffffffff
                                      : (long long) 0x00ffffffffffffff;
#endif
    	           prof_return->event_count[e] += delta;
               }
           }
           vftr_prof_data.ic = 1 - ic;
       }
    }


    if (time_to_sample && vftr_env_do_sampling ()) {
        profdata_t *prof_current = &func->prof_current;
        profdata_t *prof_previous = &func->prof_previous;
        vftr_write_to_vfd (func_entry_time, prof_current, prof_previous, func->id, SID_ENTRY);
#ifdef _MPI
        int mpi_isinit;
        PMPI_Initialized(&mpi_isinit);
        if (mpi_isinit) {
           int mpi_isfinal;
           PMPI_Finalized(&mpi_isfinal);
           if (!mpi_isfinal) {
              vftr_clear_completed_requests();
           }
        }
#endif
    }

    func->prof_current.calls++;

    // Maintain profile

    if (func->return_to) {
        delta = cycles0 - vftr_prof_data.cycles;
	prof_return->cycles += delta;
        prof_return->time_excl += func_entry_time - vftr_prof_data.time_excl;
        vftr_prog_cycles += delta;
        func->prof_current.time_incl -= func_entry_time;
    }

    /* Compensate overhead */
    // The stuff we did here added up cycles. Therefore, we have to reset
    // the global cycle count and time value.
    vftr_prof_data.cycles = vftr_get_cycles() - vftr_initcycles;
    long long overhead_time_end = vftr_get_runtime_usec();
    vftr_prof_data.time_excl = overhead_time_end;
    vftr_overhead_usec += overhead_time_end - overhead_time_start;
    func->overhead += overhead_time_end - overhead_time_start;
}

/**********************************************************************/

void vftr_function_exit () {
    int           e, read_counters, timeToSample;
    long long     timer;
    unsigned long long cycles0;
    function_t    *func;
    double        wtime;
    profdata_t *prof_current;

    if (vftr_off() || vftr_paused) return;

    /* See at the beginning of vftr_function_entry: If
     * we are dealing with a recursive function call, exit.
     */
    if (vftr_fstack->recursion_depth) {
        vftr_fstack->recursion_depth--;
        return;
    }
    long long func_exit_time = vftr_get_runtime_usec();
    // get the time to estimate vftrace overhead
    long long overhead_time_start = func_exit_time;

    timer = vftr_get_runtime_usec ();
    cycles0 = vftr_get_cycles() - vftr_initcycles;
    func  = vftr_fstack;

    prof_current = &func->prof_current;
    prof_current->time_incl += func_exit_time;   /* Inclusive time */
    
    vftr_fstack = func->return_to;

    /* Is it time for the next sample? */

    timeToSample = (func_exit_time > vftr_nextsampletime) || func->precise || 
                   (func->return_to && !func->return_to->id) /* Return from main program: end of execution */;

    read_counters = (func->return_to->detail || func->detail) &&
	  	    vftr_events_enabled && 
                    (timeToSample || vftr_environment.accurate_profile->value);

    if (read_counters) {
        int ic = vftr_prof_data.ic;
        vftr_read_counters (vftr_prof_data.events[ic]);
        if (prof_current->event_count && func->detail) {
            for (e = 0; e < vftr_n_hw_obs; e++) {
                long long delta = vftr_prof_data.events[ic][e] - vftr_prof_data.events[1-ic][e];
#ifdef __ve__
	        /* Handle counter overflow */
                if (delta < 0) {
                        delta += e < 2 ? (long long) 0x000fffffffffffff
                                       : (long long) 0x00ffffffffffffff;
 		}
#endif
		prof_current->event_count[e] += delta;
            }
        }
        vftr_prof_data.ic = 1 - ic;
    }

    if (timeToSample && vftr_env_do_sampling ()) {
        profdata_t *prof_previous = &func->prof_previous;
        vftr_write_to_vfd(func_exit_time, prof_current, prof_previous, func->id, SID_EXIT);
#ifdef _MPI
        int mpi_isinit;
        PMPI_Initialized(&mpi_isinit);
        if (mpi_isinit) {
           int mpi_isfinal;
           PMPI_Finalized(&mpi_isfinal);
           if (!mpi_isfinal) {
              vftr_clear_completed_requests();
           }
        }
#endif
    }

    /* Maintain profile info */

    prof_current->cycles += cycles0;
    prof_current->time_excl += func_exit_time;
    vftr_prog_cycles += cycles0;
    if (func->return_to) {
        prof_current->cycles -= vftr_prof_data.cycles;
        prof_current->time_excl -= vftr_prof_data.time_excl;
        vftr_prog_cycles -= vftr_prof_data.cycles;
    }

    wtime = (vftr_get_runtime_usec() - vftr_overhead_usec) * 1.0e-6;

    if (func->profile_this)  {
        vftr_write_stack_ascii (vftr_log, wtime, func, "profile at exit from", timeToSample);
        vftr_profile_wanted = true;
        int ntop;
        vftr_print_profile (stdout, NULL, &ntop, timer, 0, NULL);
        vftr_print_local_stacklist( vftr_func_table, stdout, ntop );
    }

    if (timer >= vftr_timelimit) {
        fprintf (vftr_log, "vftr_timelimit exceeded - terminating execution\n");
	kill (getpid(), SIGTERM);
    }

    /* Sort profile if it is time */
    
    if (wtime >= vftr_sorttime)  {
        int i;
        double tsum = 0.;
        double scale = 100. / (double)vftr_prog_cycles;

        qsort ((void*)vftr_func_table, (size_t)vftr_stackscount, sizeof (function_t *), vftr_get_profile_compare_function());

        /* Set function detail flags while sum(time) < max */
        for (i = 0; i < vftr_stackscount; i++) {
            function_t *f = vftr_func_table[i];
            tsum += (double)f->prof_current.cycles;
	    double cutoff = vftr_environment.detail_until_cum_cycles->value;
            if ((tsum * scale) > cutoff) break;
            f->detail = true;
        }
        /* Clear function detail flags for all others */
        for(; i < vftr_stackscount; i++) 
            vftr_func_table[i]->detail = false;

        vftr_sorttime *= vftr_sorttime_growth;
    }

    /* Compensate overhead */
    // The stuff we did here added up cycles. Therefore, we have to reset
    // the global cycle count and time value.
    vftr_prof_data.cycles = vftr_get_cycles() - vftr_initcycles;
    long long overhead_time_end = vftr_get_runtime_usec();
    vftr_prof_data.time_excl = overhead_time_end;
    vftr_overhead_usec += overhead_time_end - overhead_time_start;
    func->overhead += overhead_time_end - overhead_time_start;
    
    if (vftr_mpirank == vftr_environment.live_rank->value) {
      pthread_mutex_lock (&vftr_socket_lock_handle);
      vftr_socket_ident_level -= 2;
      pthread_mutex_unlock (&vftr_socket_lock_handle);
    }

    /* Terminate Vftrace if we are exiting the main routine */
    // When exiting main, there is no return value.
    // This approach is in contrast to previous implementations, where
    // vftr_finalize was a destructor. It has been agreed upon that
    // we do not want to have invisible side effects, wherefore this
    // method is much more transparent. Also, unit tests do not have 
    // to cope with possibly non-associated symbols when calling vftr_finalize
    // and are also purer that way. 
    // A downside is that everything between the exit from the main function
    // and the actual program termination as experienced by the user is not
    // measured. Therefore, there is a theoretical, but miniscule, discrepancy
    // the user time and the time measured by Vftrace.
    if (!vftr_fstack->return_to) vftr_finalize();
    func->open = false;

}

// These are the actual Cygnus function hooks. 
//
#if defined(__x86_64__) || defined(__ve__)

void __cyg_profile_func_enter (void *func, void *caller) {
    vftr_function_entry (NULL, func, false);
}

void __cyg_profile_func_exit (void *func, void *caller) {
    vftr_function_exit ();
}

#elif defined(__ia64__)

// The argument func is a pointer to a pointer instead of a pointer.

void __cyg_profile_func_enter (void **func, void *caller) {
    vftr_function_entry (NULL, *func, false);
}

void __cyg_profile_func_exit (void **func, void *caller) {
    vftr_function_exit ();
}
#endif
