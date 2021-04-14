#ifndef VFTR_SOCKET_H
#define VFTR_SOCKET_H

#include <pthread.h>
#include <sys/un.h>

#define VFTR_SOCK_BUFSIZE 10

enum server_states {ACCEPT, SEND, CLOSE};
enum comm_commands {GIVE, OKAY, CONN_STOP, VFTR_CLOSE};

typedef struct vftr_socket {
  int state;  
  int fd;
  struct sockaddr_un addr; 
} vftr_socket_t;

extern pthread_mutex_t vftr_socket_lock_handle;

//extern int vftr_n_stackids_to_send;
extern int vftr_n_funcs_to_send;
extern char *vftr_funcs_to_send[VFTR_SOCK_BUFSIZE];
//extern int vftr_stackids_to_send[VFTR_SOCK_BUFSIZE];
extern long long vftr_timestamps_to_send[VFTR_SOCK_BUFSIZE];

extern bool vftr_socket_thread_active;
extern vftr_socket_t vftr_serv;
//extern vftr_socket_t vftr_client;

void vftr_create_socket_thread();
void vftr_join_socket_thread();

void vftr_connect_client();

#endif
