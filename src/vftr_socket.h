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

#ifndef VFTR_SOCKET_H
#define VFTR_SOCKET_H

#include <pthread.h>
#include <sys/un.h>

#define VFTR_SOCK_BUFSIZE 100

enum server_states {ACCEPT, SEND, CLOSE};
enum comm_commands {GIVE, OKAY, CONN_STOP, VFTR_CLOSE};

typedef struct vftr_socket {
  int state;  
  int fd;
  struct sockaddr_un addr; 
} vftr_socket_t;

extern pthread_mutex_t vftr_socket_lock_handle;

extern int vftr_n_funcs_to_send;
extern int vftr_socket_ident_level;
extern char *vftr_funcs_to_send[VFTR_SOCK_BUFSIZE];
extern long long vftr_timestamps_to_send[VFTR_SOCK_BUFSIZE];
extern int vftr_idents[VFTR_SOCK_BUFSIZE];
extern int vftr_socket_it_count;
extern int vftr_its[VFTR_SOCK_BUFSIZE];
extern char *vftr_socket_recent_func;

extern bool vftr_socket_thread_active;
extern vftr_socket_t vftr_serv;

void vftr_create_socket_thread();
void vftr_join_socket_thread();

void vftr_connect_client();

#endif
