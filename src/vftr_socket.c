#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

#include "vftr_socket.h"

#define LISTENQ 20

pthread_t vftr_socket_thread;
pthread_mutex_t vftr_socket_lock_handle;

bool vftr_socket_thread_active;

vftr_socket_t vftr_serv;
vftr_socket_t vftr_client;

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
  while (true) {
    struct sockaddr_un client_addr;
    socklen_t socklen = sizeof (client_addr);
    printf ("Accept incoming: \n");
    int connfd = accept (vftr_serv.fd, (struct sockaddr *) &client_addr, &socklen); 
    printf ("There's a connection \n");
    char linebuf[4];
    int command, send;
    switch (vftr_serv.state) {
      case ACCEPT:
        printf ("Server reads\n");
        read (connfd, &command, sizeof(int));
        printf ("Server Received: %d \n", command);
        if (command == GIVE) {
          printf ("Send okay\n");
          send = OKAY;
          write (connfd, &send, sizeof(int)); 
          printf ("Going to send stuff: %d\n", send);
          send = 12345; 
          write (connfd, &send, sizeof(int));
        } else if (command == STOP) {
          printf ("Need to shut down server\n");
          pthread_mutex_lock (&vftr_socket_lock_handle);
          vftr_socket_thread_active = false;
          pthread_mutex_unlock (&vftr_socket_lock_handle);
          close(vftr_serv.fd); 
        }
        printf ("Server done\n");
    }
    pthread_mutex_lock (&vftr_socket_lock_handle);
    printf ("next iteration: %d\n", vftr_socket_thread_active);
    if (!vftr_socket_thread_active) {
       pthread_mutex_unlock (&vftr_socket_lock_handle);
       break;
    }
    pthread_mutex_unlock (&vftr_socket_lock_handle);
  } 
}

void vftr_connect_client () {
  pthread_mutex_lock (&vftr_socket_lock_handle);
  if (!vftr_socket_thread_active) {
     pthread_mutex_unlock (&vftr_socket_lock_handle);
     return;
  }
  pthread_mutex_unlock (&vftr_socket_lock_handle);
  vftr_client.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  bzero (&(vftr_client.addr), sizeof(vftr_client.addr));
  vftr_client.addr.sun_family = AF_LOCAL;
  strcpy (vftr_client.addr.sun_path, "foo");
  connect (vftr_client.fd, (struct sockaddr*) &(vftr_client.addr), sizeof(vftr_client.addr));
  int send = GIVE;
  int recv;
  write (vftr_client.fd, &send, sizeof(int));
  read (vftr_client.fd, &recv, sizeof(int));
  if (recv == OKAY) {
    int n_stack_ids;
    read (vftr_client.fd, &n_stack_ids, sizeof(int)); 
  } 
  close (vftr_client.fd);
}

void vftr_connect_and_close () {
  vftr_client.fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  bzero (&(vftr_client.addr), sizeof(vftr_client.addr));
  vftr_client.addr.sun_family = AF_LOCAL;
  strcpy (vftr_client.addr.sun_path, "foo");
  connect (vftr_client.fd, (struct sockaddr*) &(vftr_client.addr), sizeof(vftr_client.addr));
  int send = STOP;
  write (vftr_client.fd, &send, sizeof(int));
  close (vftr_client.fd); 
}

void vftr_create_socket_thread () {
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
  vftr_connect_and_close();
  pthread_join (vftr_socket_thread, NULL);
  pthread_mutex_destroy (&vftr_socket_lock_handle);
}
