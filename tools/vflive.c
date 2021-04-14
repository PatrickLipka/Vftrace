#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "vftr_socket.h"
#include <sys/socket.h>

int main (int argc, char *argv[]) {
  if (argc < 2) {
     printf ("Usage: ./vflive <socket-path>\n");
     return -1;
  }
  char *socket_name = argv[1];

  vftr_socket_t sock;
  sock.fd = socket (AF_LOCAL, SOCK_STREAM, 0); 
  bzero (&(sock.addr), sizeof(sock.addr));
  sock.addr.sun_family = AF_LOCAL;
  strcpy (sock.addr.sun_path, socket_name);
  connect (sock.fd, (struct sockaddr*) &(sock.addr), sizeof(sock.addr));
  while (true) {
    int send = GIVE; 
    int recv;
    //printf ("Send: \n");
    write (sock.fd, &send, sizeof(int));
    //printf ("Receive: \n");
    read (sock.fd, &recv, sizeof(int));
    //printf ("Recv: %d\n", recv);
    if (recv == OKAY) {
      int n_stack_ids;
      read (sock.fd, &n_stack_ids, sizeof(int));
      //int *stack_ids = (int*)malloc (n_stack_ids * sizeof(int));
      for (int i = 0; i < n_stack_ids; i++) {
        int s;
        //read (sock.fd, &stack_ids[i], sizeof(int));
        read (sock.fd, &s, sizeof(int));
        char name[s+1];
        read (sock.fd, name, s * sizeof(char));
        name[s+1] = '\0';
        long long ts;
        read (sock.fd, &ts, sizeof(long long));
        printf ("%lld: %s\n", ts, name); 
        fflush(stdout);
      }
      //printf ("%d StackIDs: ", n_stack_ids);
      //for (int i = 0; i < n_stack_ids; i++) {
      //  printf ("%d ", stack_ids[i]);
      //}
      //printf ("\n");
      //free (stack_ids);
    }
  }
  close (sock.fd);
  return 0;
}
