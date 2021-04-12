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

struct sockaddr_un vftr_serv_addr;
int vftr_socket_fd;
int vftr_client_fd;

void *vftr_do_socket (void *arg) {
  //printf ("Socket start\n");
  //struct sockaddr_un serv_addr;
  int listenfd;
  unlink ("foo");
  vftr_socket_fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  bzero (&vftr_serv_addr, sizeof(vftr_serv_addr));
  vftr_serv_addr.sun_family  = AF_LOCAL;
  strcpy (vftr_serv_addr.sun_path, "foo");
  int ret1 = bind(vftr_socket_fd, (struct sockaddr*)&vftr_serv_addr, sizeof(vftr_serv_addr));
  //printf ("bind: %d %d\n", ret1, errno);
  int ret2 = listen (vftr_socket_fd, LISTENQ);
  //printf ("listen: %d %d\n", ret2, errno);
  //printf ("vftr_socket_fd: %d\n",  vftr_socket_fd);
  //printf ("socket active: %d\n", vftr_socket_thread_active);
    pthread_mutex_lock (&vftr_socket_lock_handle);
  vftr_socket_thread_active = true;
    pthread_mutex_unlock (&vftr_socket_lock_handle);
  while (true) {
    //printf ("Wait\n");
    struct sockaddr_un client_addr;
    socklen_t socklen = sizeof (client_addr);
    //printf ("socklen: %d\n", socklen);
    int connfd = accept (vftr_socket_fd, (struct sockaddr *) &client_addr, &socklen); 
    //printf ("connfd: %d %d\n", connfd, errno);
    //int connfd = accept (vftr_socket_fd(struct sockaddr *) &client_addr, (socklen_t*)sizeof(client_addr)); 
    //char linebuf[64];
    char line;
    //while (read (connfd, linebuf, sizeof(char) * 64) > 0) {
    while (read (connfd, &line, sizeof(char)) > 0) {
       printf ("Received: %c\n", line);
    }
    //printf ("lock 1\n");
    pthread_mutex_lock (&vftr_socket_lock_handle);
    if (!vftr_socket_thread_active) {
       pthread_mutex_unlock (&vftr_socket_lock_handle);
       break;
    }
    pthread_mutex_unlock (&vftr_socket_lock_handle);
    //printf ("unlock 1\n");
  } 
  //printf ("Socket end\n");
}

void vftr_connect_client () {
    //printf ("lock 2\n");
  pthread_mutex_lock (&vftr_socket_lock_handle);
  if (!vftr_socket_thread_active) {
     //printf ("Return because the socket is not active\n");
     pthread_mutex_unlock (&vftr_socket_lock_handle);
     return;
  } else {
     //printf ("SEND\n");
  }
  pthread_mutex_unlock (&vftr_socket_lock_handle);
  vftr_client_fd  = socket (AF_LOCAL, SOCK_STREAM, 0);
  struct sockaddr_un connect_to;
  bzero (&connect_to, sizeof(connect_to));
  connect_to.sun_family = AF_LOCAL;
  strcpy (connect_to.sun_path, "foo");
  vftr_client_fd = socket (AF_LOCAL, SOCK_STREAM, 0);
  connect (vftr_client_fd, (struct sockaddr*) &connect_to, sizeof(connect_to));
  ///connect (vftr_socket_fd, (struct sockaddr*) &connect_to, sizeof(connect_to));
  //printf ("clientfd: %d\n", vftr_socket_fd);
  //write (vftr_client_fd, "HUHU", strlen("HUHU") * sizeof(char)); 
  char send = 'x';
  //write (vftr_socket_fd, &send, sizeof(char));
  write (vftr_client_fd, &send, sizeof(char));
  close (vftr_client_fd);
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
  pthread_mutex_lock (&vftr_socket_lock_handle);
  vftr_socket_thread_active = false;
  pthread_mutex_unlock (&vftr_socket_lock_handle);
  pthread_join (vftr_socket_thread, NULL);
  pthread_mutex_destroy (&vftr_socket_lock_handle);
}
