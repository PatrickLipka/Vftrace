#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <signal.h>
#include <sys/socket.h>

#include "vftr_socket.h"

void display_timestamp (long long ts, int len, char *func_name) {
  int seconds = ts / 1000000;
  int milli_seconds = (ts - seconds * 1000000) / 1000;
  printf ("%4d:%2d %*s\n", seconds, milli_seconds, len, func_name);
}

bool keep_alive = true;
bool send_close = false;

void int_handler (int dummy) {
  keep_alive = false;
  send_close = true;
}

int main (int argc, char *argv[]) {
  if (argc < 2) {
     printf ("Usage: ./vflive <socket-path>\n");
     return -1;
  }
  char *socket_name = argv[1];

  signal (SIGINT, int_handler);

  vftr_socket_t sock;
  sock.fd = socket (AF_LOCAL, SOCK_STREAM, 0); 
  bzero (&(sock.addr), sizeof(sock.addr));
  sock.addr.sun_family = AF_LOCAL;
  strcpy (sock.addr.sun_path, socket_name);
  connect (sock.fd, (struct sockaddr*) &(sock.addr), sizeof(sock.addr));
  while (keep_alive) {
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
      long long t_first = *((long long*)packet);
      packet += sizeof(long long);
      long long *ts;
      int *idents;
      if (n_entries > 0) {
        ts = (long long*) malloc (n_entries * sizeof(long long));
        for (int i = 0; i < n_entries; i++) {
          memcpy (&ts[i], (long long*)packet, sizeof(long long));
          packet += sizeof(long long);
        }

        idents = (int*) malloc (n_entries * sizeof(int));
        for (int i = 0; i < n_entries; i++) {
          memcpy (&idents[i], (int*)packet, sizeof(int));
          packet += sizeof(int);
        }
      }
 
      for (int i = 0; i < n_entries; i++) {
        int s = *((int*)packet);
        packet += sizeof(int);
        display_timestamp (ts[i] - t_first, s + idents[i], (char*)packet);
        packet += s * sizeof(char);
      }

      fflush(stdout);
      packet = packet0;
      free(packet);
      if (n_entries > 0) {
         free(ts);
         free(idents);
      }
    } else if (recv == VFTR_CLOSE) {
      printf ("The traced program terminated\n");
      keep_alive = false;
    }
  }
  if (send_close) {
    int send = VFTR_CLOSE;
    write (sock.fd, &send, sizeof(int));
  }
  close (sock.fd);
  return 0;
}
