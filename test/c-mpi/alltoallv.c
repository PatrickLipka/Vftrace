#include <mpi.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char** argv) {
   MPI_Init(&argc, &argv);

   // Get number or processes
   int comm_size;
   MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
   // Get rank of process
   int my_rank;
   MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

   // require cmd-line argument
   if (argc < 2) {
      printf("./alltoallv <msgsize in ints>\n");
      return 1;
   }

   // allocating send/recv buffer
   int nints = atoi(argv[1]) + my_rank;

   // prepare special arrays for send
   int *scounts = (int*) malloc(comm_size*sizeof(int));
   int *sdispls = (int*) malloc(comm_size*sizeof(int));
   int nstot = 0;
   for (int i=0; i<comm_size; i++) {
      scounts[i] = nints;
      sdispls[i] = nstot;
      nstot += scounts[i];
   }
   int *sbuffer = (int*) malloc(nstot*sizeof(int));
   for (int i=0; i<nstot; i++) {sbuffer[i]=my_rank;}

   // prepare special arrays for recv
   int *rcounts = (int*) malloc(comm_size*sizeof(int));
   int *rdispls = (int*) malloc(comm_size*sizeof(int));
   int nrtot = 0;
   for (int i=0; i<comm_size; i++) {
      rcounts[i] = nints - my_rank + i;
      rdispls[i] = nrtot;
      nrtot += rcounts[i];
   }
   int *rbuffer = (int*) malloc(nrtot*sizeof(int));
   for (int i=0; i<nrtot; i++) {rbuffer[i] = -1;}

for (int irank=0; irank<comm_size; irank++) {
   if (my_rank == irank) {
      printf("Rank %d: ", my_rank);
      for (int i=0; i<nstot; i++) {
         printf("%d ", sbuffer[i]);
      }
      printf("\n");
      printf("   scounts: ");
      for (int i=0; i<comm_size; i++) {
         printf("%d ", scounts[i]);
      }
      printf("\n");
      printf("   sdispls: ");
      for (int i=0; i<comm_size; i++) {
         printf("%d ", sdispls[i]);
      }
      printf("\n");
      printf("   rcounts: ");
      for (int i=0; i<comm_size; i++) {
         printf("%d ", rcounts[i]);
      }
      printf("\n");
      printf("   rdispls: ");
      for (int i=0; i<comm_size; i++) {
         printf("%d ", rdispls[i]);
      }
      printf("\n");

   }
   MPI_Barrier(MPI_COMM_WORLD);
}

   // Messaging
   MPI_Alltoallv(sbuffer, scounts, sdispls, MPI_INT,
                 rbuffer, rcounts, rdispls, MPI_INT,
                 MPI_COMM_WORLD);
   printf("Communicating with all ranks\n");

for (int irank=0; irank<comm_size; irank++) {
   if (my_rank == irank) {
      printf("Rank %d: ", my_rank);
      for (int i=0; i<nrtot; i++) {
         printf("%d ", rbuffer[i]);
      }
      printf("\n");
   }
   MPI_Barrier(MPI_COMM_WORLD);
}

   // validate data
   bool valid_data = true;
   for (int irank=0; irank<comm_size; irank++) {
      for (int i=0; i<rcounts[irank]; i++) {
         if (rbuffer[i+rdispls[irank]] != irank) {
            printf("Rank %d received faulty data from rank %d\n", my_rank, irank);
            valid_data = false;
            break;
         }
      }
   }

   free(sbuffer);
   sbuffer=NULL;

   free(scounts);
   scounts=NULL;

   free(sdispls);
   sdispls=NULL;

   free(rbuffer);
   rbuffer=NULL;

   free(rcounts);
   rcounts=NULL;

   free(rdispls);
   rdispls=NULL;

   MPI_Finalize();

   return valid_data ? 0 : 1;
}
