#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

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
    write (sock.fd, &send, sizeof(int));
    read (sock.fd, &recv, sizeof(int));
    if (recv == OKAY) {
      int packet_size;
      read (sock.fd, &packet_size, sizeof(int));
      char *packet = (char*)malloc (packet_size);
      char *packet0 = packet;
      read (sock.fd, packet, packet_size);
      int n_entries = *((int*)packet);
      packet += sizeof(int);
      if (n_entries > 0) {
        long long *ts = (long long*) malloc (n_entries * sizeof(long long));
        for (int i = 0; i < n_entries; i++) {
          ts[i] = *((long long*)packet); 
          packet += sizeof(long long);
        }
        printf ("ts: ");
        for (int i = 0; i < n_entries; i++) {
          printf ("%lld ", ts[i]);
        }
        printf ("\n");
      }
      packet = packet0;
      free(packet);
    } else if (recv == VFTR_CLOSE) {
      break;
    }
  }
  close (sock.fd);
  return 0;
}
