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

#ifdef _MPI
#include <mpi.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdbool.h>
#include <sys/time.h>

#include "vftr_setup.h"
#include "vftr_signals.h"

void vftr_abort (int errcode) {
#ifdef _MPI
    (void) PMPI_Abort( MPI_COMM_WORLD, errcode );
#else
    abort();
#endif
}

int vftr_signal_number;

struct sigaction vftr_signals[NSIG];

/**********************************************************************/

void vftr_signal_handler (int signum) {
  if (vftr_signal_number < 0) {
    vftr_signal_number = signum;
    vftr_finalize();
    vftr_signals[signum].sa_handler = SIG_DFL;
    sigaction (signum, &vftr_signals[signum], NULL);
    raise(signum);
  }
}

/**********************************************************************/

void vftr_setup_signal (int signum) {
  memset (&vftr_signals[signum], 0, sizeof(vftr_signals[signum]));
  vftr_signals[signum].sa_handler = vftr_signal_handler;
  vftr_signals[signum].sa_flags = SA_SIGINFO;
  sigaction (signum, &vftr_signals[signum], NULL);
}

void vftr_setup_signals () {

  vftr_signal_number = -1;

  vftr_setup_signal (SIGTERM);
  vftr_setup_signal (SIGINT);
  vftr_setup_signal (SIGABRT);
  vftr_setup_signal (SIGFPE);
  vftr_setup_signal (SIGQUIT);
}

/**********************************************************************/
