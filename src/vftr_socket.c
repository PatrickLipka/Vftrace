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

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

#include "vftr_stacks.h"
#include "vftr_socket.h"

#define LISTENQ 20

pthread_t vftr_socket_thread;
pthread_mutex_t vftr_socket_lock_handle;

bool vftr_socket_thread_active;

vftr_socket_t vftr_serv;

int vftr_n_funcs_to_send;
char* vftr_funcs_to_send[VFTR_SOCK_BUFSIZE];
long long vftr_socket_first_timestamp;
long long vftr_timestamps_to_send[VFTR_SOCK_BUFSIZE];
int vftr_idents[VFTR_SOCK_BUFSIZE];
int vftr_socket_ident_level;
int vftr_socket_it_count;
int vftr_its[VFTR_SOCK_BUFSIZE];
char *vftr_socket_recent_func;

/**********************************************************************/

void vftr_create_packet (int *packet_size, char **packet) {
  // Packet structure
  *packet_size = (2 * vftr_n_funcs_to_send + 1) * sizeof(int)
               + (vftr_n_funcs_to_send + 1) * sizeof(long long);
  int *strlens = (int*)malloc (vftr_n_funcs_to_send * sizeof(int));
  for (int i = 0; i < vftr_n_funcs_to_send; i++) {
    strlens[i] = strlen(vftr_funcs_to_send[i]) + 1;
    *packet_size += strlens[i] * sizeof(char) + sizeof(int);
  }
  *packet = (char*)malloc (*packet_size);
  char *packet0 = *packet;
  memcpy (*packet, &vftr_n_funcs_to_send, sizeof(int));
  *packet += sizeof(int);
  memcpy (*packet, &vftr_socket_first_timestamp, sizeof(long long));
  *packet += sizeof(long long);
  if (vftr_n_funcs_to_send > 0) {
     memcpy (*packet, vftr_timestamps_to_send, vftr_n_funcs_to_send * sizeof(long long));
     *packet += vftr_n_funcs_to_send * sizeof(long long);
     memcpy (*packet, vftr_idents, vftr_n_funcs_to_send * sizeof(int));
     *packet += vftr_n_funcs_to_send * sizeof(int);
     memcpy (*packet, vftr_its, vftr_n_funcs_to_send * sizeof(int));
     *packet += vftr_n_funcs_to_send * sizeof(int);
  }
  for (int i = 0; i < vftr_n_funcs_to_send; i++) {
    memcpy (*packet, &strlens[i], sizeof(int));
    *packet += sizeof(int);
    memcpy (*packet, vftr_funcs_to_send[i], strlens[i] * sizeof(char));
    *packet += strlens[i] * sizeof(char);
  }
  *packet = packet0;
  free (strlens);
}

/**********************************************************************/

void vftr_print_packet (FILE *fp, char *packet) {
  char *packet0 = packet;
  fprintf (fp, "Package content: \n");
  int n_funcs = *((int*)packet);
  packet += sizeof(int);
  fprintf (fp, "Nr. of functions: %d\n", n_funcs);
  fprintf (fp, "First timestamp: %lld\n", *((long long*)packet));
  packet += sizeof(long long);
  if (n_funcs > 0) {
    fprintf (fp, "Timestamps: ");
    for (int i = 0; i < n_funcs; i++) {
      fprintf (fp, "%lld ", *((long long*)packet));
      packet += sizeof(long long);
    }
    fprintf (fp, "\n");
    fprintf (fp, "Indentations: ");
    for (int i = 0; i < n_funcs; i++) {
      fprintf (fp, "%d ", *((int*)packet));
      packet += sizeof(int);
    }
    fprintf (fp, "\n");
    fprintf (fp, "Function names: \n");
    for (int i = 0; i < n_funcs; i++) {
      int len = *((int*)packet);
      packet += sizeof(int);
      fprintf (fp, "len: %d, ", len);
      fprintf (fp, "%s\n", (char*)packet);
      packet += len * sizeof(char);
    }
  }
  packet = packet0;
  printf ("*******************\n");
}

/**********************************************************************/

void *vftr_do_socket (void *arg) {
  unlink ("foo");
  vftr_serv.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  vftr_serv.state = ACCEPT;
  bzero (&(vftr_serv.addr), sizeof(vftr_serv.addr));
  vftr_serv.addr.sun_family = AF_LOCAL;
  strcpy (vftr_serv.addr.sun_path, "foo");
  int ret1 = bind(vftr_serv.fd, (struct sockaddr*)&vftr_serv.addr, sizeof(vftr_serv.addr));
  int ret2 = listen (vftr_serv.fd, LISTENQ);
  pthread_mutex_lock (&vftr_socket_lock_handle);
  vftr_socket_thread_active = true;
  pthread_mutex_unlock (&vftr_socket_lock_handle);
  struct sockaddr_un client_addr;
  socklen_t socklen = sizeof (client_addr);
  vftr_socket_first_timestamp = vftr_get_runtime_usec();
  vftr_socket_ident_level = 0;
  vftr_socket_it_count = 0;
  vftr_socket_recent_func = "";
  while (true) {
    int connfd = accept (vftr_serv.fd, (struct sockaddr *) &client_addr, &socklen); 
    bool keep_open = connfd > 0;
    int recv_command, send_command;
    while (keep_open) {
       switch (vftr_serv.state) {
         case ACCEPT:
           read (connfd, &recv_command, sizeof(int));
           if (recv_command == GIVE) {
             send_command = OKAY;
             write (connfd, &send_command, sizeof(int)); 

             pthread_mutex_lock (&vftr_socket_lock_handle);
             int packet_size;
             char *packet;
             vftr_create_packet (&packet_size, &packet);
             write (connfd, &packet_size, sizeof(int));
             write (connfd, packet, packet_size);
             free (packet);

             vftr_n_funcs_to_send = 0;
             pthread_mutex_unlock (&vftr_socket_lock_handle);
           } else if (recv_command == CONN_STOP) {
             keep_open = false;
           } else if (recv_command == VFTR_CLOSE) {
             printf ("Received STOP signal\n");
             keep_open = false;
             pthread_mutex_lock (&vftr_socket_lock_handle);
             vftr_socket_thread_active = false;
             pthread_mutex_unlock (&vftr_socket_lock_handle);
             vftr_serv.state = CLOSE;
             close(vftr_serv.fd); 
           }
           break;
         case CLOSE:
           read (connfd, &recv_command, sizeof(int)); // Dummy read-in, because the client expects his "GIVE" command to be received.
           printf ("Tell the client that it's over\n");
           send_command = VFTR_CLOSE;
           write (connfd, &send_command, sizeof(int));
           keep_open = false;
       }
       pthread_mutex_lock (&vftr_socket_lock_handle);
       if (!vftr_socket_thread_active) {
          printf ("Thread is not active any more\n");
          vftr_serv.state = CLOSE; 
       }
       pthread_mutex_unlock (&vftr_socket_lock_handle);
    }
    pthread_mutex_lock (&vftr_socket_lock_handle);
    if (!vftr_socket_thread_active) {
       pthread_mutex_unlock (&vftr_socket_lock_handle);
       break;
    }
    pthread_mutex_unlock (&vftr_socket_lock_handle);
  } 
}

/**********************************************************************/

void vftr_connect_and_close () {
  printf ("Connect and close!\n");
  vftr_socket_t tmp_close;
  tmp_close.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  bzero (&(tmp_close.addr), sizeof(tmp_close.addr));
  tmp_close.addr.sun_family = AF_LOCAL;
  strcpy (tmp_close.addr.sun_path, "foo");
  int ret = connect (tmp_close.fd, (struct sockaddr*) &(tmp_close.addr), sizeof(tmp_close.addr));
  if (ret < 0) {
    printf ("Error connecting to sockect: %d\n", errno);
  } else {
    printf ("Connection OKAY: %d\n", tmp_close.fd);
  }
  int send = VFTR_CLOSE;
  printf ("Send stop signal\n");
  write (tmp_close.fd, &send, sizeof(int));
  printf ("Send stop signal - done\n");
  close (tmp_close.fd); 
}

/**********************************************************************/

void vftr_create_socket_thread () {
  printf  ("Create socket thread\n");
  vftr_n_funcs_to_send = 0;
  memset (vftr_funcs_to_send, -1, VFTR_SOCK_BUFSIZE * sizeof(int));
  memset (vftr_timestamps_to_send, -1, VFTR_SOCK_BUFSIZE * sizeof(long long));
  memset (vftr_idents, -1, VFTR_SOCK_BUFSIZE * sizeof(int));
  vftr_socket_ident_level = 0;
  pthread_mutex_init (&vftr_socket_lock_handle, NULL);
  pthread_create (&vftr_socket_thread, NULL, vftr_do_socket, NULL);
  while (true) {
    pthread_mutex_lock (&vftr_socket_lock_handle);
    if (vftr_socket_thread_active) {
       pthread_mutex_unlock (&vftr_socket_lock_handle);
       break;
    }
    pthread_mutex_unlock (&vftr_socket_lock_handle);
  }
}

/**********************************************************************/

void vftr_join_socket_thread() {
  printf ("Join socket thread 0\n");
  pthread_mutex_lock (&vftr_socket_lock_handle);
  vftr_socket_thread_active = false;
  pthread_mutex_unlock (&vftr_socket_lock_handle);
  printf ("Join socket thread 1\n");
  vftr_connect_and_close();
  pthread_join (vftr_socket_thread, NULL);
  printf ("Join socket thread 2\n");
  pthread_mutex_destroy (&vftr_socket_lock_handle);
  printf ("Join socket thread 3\n");
}

/**********************************************************************/
