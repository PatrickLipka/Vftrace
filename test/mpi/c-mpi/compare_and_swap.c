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
   
   // require at least two processes
   if (comm_size < 2) {
      printf("At least two ranks are required\n");
      printf("Run again with '-np 2'\n");
      MPI_Finalize();
      return 1;
   }

   // require cmd-line argument
   if (argc < 2) {
      printf("./compare_and_swap <msgsize in ints>\n");
      return 1;
   }

   // allocating send/recv buffer
   int nints = atoi(argv[1]);
   nints = (nints < comm_size) ? comm_size : nints;
   int *originbuffer;
   int *resultbuffer;
   int *comparebuffer;
   int *targetbuffer;
   if (my_rank == 0) {
      originbuffer = (int*) malloc(nints*sizeof(int));
      for (int i=0; i<nints; i++) {originbuffer[i]=my_rank;}
      resultbuffer = (int*) malloc(nints*sizeof(int));
      for (int i=0; i<nints; i++) {resultbuffer[i]=0;}
      comparebuffer = (int*) malloc(nints*sizeof(int));
      for (int i=0; i<nints; i++) {comparebuffer[i]=2*((i%comm_size)/2);}
   } else {
      targetbuffer = (int*) malloc(nints*sizeof(int));
      for (int i=0; i<nints; i++) {targetbuffer[i]=my_rank;}
   }

   // open memory to remote memory access
   MPI_Win window;
   MPI_Win_create(targetbuffer, nints*sizeof(int), sizeof(int),
                  MPI_INFO_NULL, MPI_COMM_WORLD, &window);

   MPI_Win_fence(0, window);

   // Remote memory access
   if (my_rank == 0) {
      int *tmporiginbuffer = originbuffer+1;
      int *tmpresultbuffer = resultbuffer+1;
      int *tmpcomparebuffer = comparebuffer+1;

      for (int irank=1; irank<comm_size; irank++) {
         // Origin stays unchanged
         // Resultbuffer gets a copy of the target buffer from remote process
         // The remote target buffer gets the sum of origin+itself
         MPI_Compare_and_swap(tmporiginbuffer, tmpcomparebuffer, tmpresultbuffer, MPI_INT,
                              irank, 0, window);
         tmporiginbuffer++;
         tmpresultbuffer++;
         tmpcomparebuffer++;
      }
   }
   
   MPI_Win_fence(0, window);
   MPI_Win_free(&window);

   // validate data
   bool valid_data = true;
   if (my_rank == 0) {
      // contents of origin buffer should be 0
      int refresult = 0;
      for (int i=0; i<nints; i++) {
         if (originbuffer[i] != refresult) {
            printf("Rank %d received faulty data\n", my_rank);
            valid_data = false;
            break;
         }
      }
      // contents of Result buffer should be the largest rank
      for (int i=0; i<nints; i++) {
         if (i<comm_size) {
            refresult = i;
         } else {
            refresult = 0;
         }
         if (resultbuffer[i] != refresult) {
            printf("Rank %d received faulty data\n", my_rank);
            valid_data = false;
            break;
         }
      }
      // contens of compare buffer should be unchanged
      for (int i=0; i<nints; i++) {
         refresult = 2*((i%comm_size)/2);
         if (comparebuffer[i] != refresult) {
            printf("Rank %d received faulty data\n", my_rank);
            valid_data = false;
            break;
         }
      }

      free(originbuffer);
      originbuffer=NULL;
      free(resultbuffer);
      resultbuffer=NULL;
      free(comparebuffer);
      comparebuffer=NULL;
   } else {
      // contents of target buffer should be the sum of all ranks up to this one
      int refresult = (my_rank*(my_rank+1))/2;
      for (int i=0; i<nints; i++) {
         int refresult;
         if (i==0 && my_rank%2==0) {
            refresult = 0;
         } else {
            refresult = my_rank;
         }
         if (targetbuffer[i] != refresult) {
            printf("Rank %d received faulty data\n", my_rank);
            valid_data = false;
            break;
         }
      }
      free(targetbuffer);
      targetbuffer=NULL;
   }

   MPI_Finalize();

   return valid_data ? 0 : 1;
}
