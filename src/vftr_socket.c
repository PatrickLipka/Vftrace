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
//vftr_socket_t vftr_client;

//int vftr_n_stackids_to_send;
int vftr_n_funcs_to_send;
//int vftr_stackids_to_send[VFTR_SOCK_BUFSIZE];
char* vftr_funcs_to_send[VFTR_SOCK_BUFSIZE];
long long vftr_timestamps_to_send[VFTR_SOCK_BUFSIZE];

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
  while (true) {
    printf ("Wait for connections: \n");
    int connfd = accept (vftr_serv.fd, (struct sockaddr *) &client_addr, &socklen); 
    printf ("There is a connection: \n");
    bool keep_open = connfd > 0;
    while (keep_open) {
       char linebuf[4];
       int command, send;
       command = -999;
       switch (vftr_serv.state) {
         case ACCEPT:
           read (connfd, &command, sizeof(int));
           if (command == GIVE) {
             pthread_mutex_lock (&vftr_socket_lock_handle);
             send = OKAY;
             write (connfd, &send, sizeof(int)); 
             send = vftr_n_funcs_to_send;
             int packet_size = sizeof(int) + vftr_n_funcs_to_send * sizeof(long long);
             char *packet = (char*)malloc (packet_size);
             char *packet0 = packet;
             memcpy (packet, &vftr_n_funcs_to_send, sizeof(int));
             packet += sizeof(int);
             if (vftr_n_funcs_to_send > 0) memcpy (packet, vftr_timestamps_to_send, vftr_n_funcs_to_send * sizeof(long long));
             packet = packet0;
               
             write (connfd, &packet_size, sizeof(int));
             write (connfd, packet, packet_size);
             free (packet); // Also frees packet0
             vftr_n_funcs_to_send = 0;
             pthread_mutex_unlock (&vftr_socket_lock_handle);
           } else if (command == CONN_STOP) {
             keep_open = false;
           } else if (command == VFTR_CLOSE) {
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
           read (connfd, &command, sizeof(int));
           printf ("Tell the client that it's over\n");
           // Tell the client that it's over
           send = VFTR_CLOSE;
           write (connfd, &send, sizeof(int));
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

//void vftr_connect_client () {
//  pthread_mutex_lock (&vftr_socket_lock_handle);
//  if (!vftr_socket_thread_active) {
//     pthread_mutex_unlock (&vftr_socket_lock_handle);
//     return;
//  }
//  pthread_mutex_unlock (&vftr_socket_lock_handle);
//  vftr_client.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
//  bzero (&(vftr_client.addr), sizeof(vftr_client.addr));
//  vftr_client.addr.sun_family = AF_LOCAL;
//  strcpy (vftr_client.addr.sun_path, "foo");
//  connect (vftr_client.fd, (struct sockaddr*) &(vftr_client.addr), sizeof(vftr_client.addr));
//  //int send = GIVE;
//  //int recv;
//  //write (vftr_client.fd, &send, sizeof(int));
//  //read (vftr_client.fd, &recv, sizeof(int));
//  //if (recv == OKAY) {
//  //  int n_stack_ids;
//  //  read (vftr_client.fd, &n_stack_ids, sizeof(int)); 
//  //  int *stack_ids = (int*)malloc (n_stack_ids * sizeof(int));
//  //  for (int i = 0; i < n_stack_ids; i++) {
//  //    read (vftr_client.fd, &stack_ids[i], sizeof(int)); 
//  //  }
//  //  printf ("%d StackIDs: ", n_stack_ids);
//  //  for (int i = 0; i < n_stack_ids; i++) {
//  //    printf ("%d ", stack_ids[i]);
//  //  }
//  //  printf ("\n");
//  //} 
//  close (vftr_client.fd);
//}

void vftr_connect_and_close () {
  printf ("Connect and close!\n");
  vftr_socket_t tmp_close;
  tmp_close.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  bzero (&(tmp_close.addr), sizeof(tmp_close.addr));
  tmp_close.addr.sun_family = AF_LOCAL;
  strcpy (tmp_close.addr.sun_path, "foo");
  int ret = connect (tmp_close.fd, (struct sockaddr*) &(tmp_close.addr), sizeof(tmp_close.addr));
  //printf ("test: %d\n", vftr_serv.fd);
  //int ret = connect (vftr_serv.fd, (struct sockaddr*)&tmp_close.addr, sizeof(tmp_close.addr));
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

void vftr_create_socket_thread () {
  printf  ("Create socket thread\n");
  vftr_n_funcs_to_send = 0;
  memset (vftr_funcs_to_send, -1, VFTR_SOCK_BUFSIZE * sizeof(int));
  memset (vftr_timestamps_to_send, -1, VFTR_SOCK_BUFSIZE * sizeof(long long));
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
