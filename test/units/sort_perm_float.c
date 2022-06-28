#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#ifdef _MPI
#include <mpi.h>
#endif

#include <sorting.h>
#include "bad_rng.h"

bool float_list_sorted(int n, float *list, bool ascending) {
   bool sorted = true;
   if (ascending) {
      for (int i=1; i<n; i++) {
         sorted = sorted && (list[i-1] <= list[i]);
      }
   } else {
      for (int i=1; i<n; i++) {
         sorted = sorted && (list[i-1] >= list[i]);
      }
   }
   return sorted;
}

int main(int argc, char **argv) {

#ifdef _MPI
   PMPI_Init(&argc, &argv);
#endif

   // require cmd-line argument
   if (argc < 3) {
      printf("./sort_perm_float <listsize> <ascending>\n");
      return 1;
   }

   int n = atoi(argv[1]);
   if (n < 2) {
      printf("listsize needs to be integer >= 2\n");
      return 1;
   }

   int ascending_int = atoi(argv[2]);
   bool ascending = ascending_int ? true : false;

   float *list = (float*) malloc(n*sizeof(float));
   float *list2 = (float*) malloc(n*sizeof(float));
   bool sorted_before = true;
   while (sorted_before) {
      for (int i=0; i<n; i++) {
         list[i] = random_float();
         list2[i] = list[i];
      }
      sorted_before = float_list_sorted(n, list, ascending);
   }
   printf("sorted before: %s\n", sorted_before ? "true" : "false");

   int *perm = NULL;
   vftr_sort_perm_float(n, list, &perm, ascending);

   bool sorted_after = float_list_sorted(n, list, ascending);
   printf("sorted after: %s\n", sorted_after ? "true" : "false");

   vftr_apply_perm_float(n, list2, perm);

   bool sorted_other_list = float_list_sorted(n, list2, ascending);
   printf("other list sorted: %s\n", sorted_other_list ? "true" : "false");

   free(list);
   free(list2);
   free(perm);
   list = NULL;

#ifdef _MPI
   PMPI_Finalize();
#endif

   if (sorted_before) {
      printf("Initial test array was already sorted\n");
      return 1;
   } else if (!sorted_after) {
      printf("Array was not properly sorted\n");
      return 1;
   } else if (!sorted_other_list) {
      printf("Other list not properly sorted with permutation\n");
      return 1;
   } else {
      return 0;
   }
}
