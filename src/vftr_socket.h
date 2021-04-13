#ifndef VFTR_SOCKET_H
#define VFTR_SOCKET_H

#include <sys/un.h>

enum server_states {ACCEPT, SEND, CLOSE};
enum comm_commands {GIVE, OKAY, STOP};

typedef struct vftr_socket {
  int state;  
  int fd;
  struct sockaddr_un addr; 
} vftr_socket_t;

extern bool vftr_socket_thread_active;
//extern int vftr_client_fd;
//extern struct sockaddr_un vftr_serv_addr;
extern vftr_socket_t vftr_serv;
extern vftr_socket_t vftr_client;

void vftr_create_socket_thread();
void vftr_join_socket_thread();

void vftr_connect_client();

#endif
