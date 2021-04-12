#ifndef VFTR_SOCKET_H
#define VFTR_SOCKET_H

#include <sys/un.h>

extern bool vftr_socket_thread_active;
extern int vftr_client_fd;
extern struct sockaddr_un vftr_serv_addr;

void vftr_create_socket_thread();
void vftr_join_socket_thread();

void vftr_connect_client();

#endif
