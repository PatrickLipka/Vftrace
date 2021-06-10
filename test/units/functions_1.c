#include "vftr_functions.h"
#include "vftr_stacks.h"
#include "vftr_environment.h"

int main (int argc, char **argv) {

#ifdef _MPI
  PMPI_Init(&argc, &argv);
#endif

  vftr_read_environment();

  unsigned long long addr[1];
  fprintf (stdout, "Initial vftr_stackscount: %d\n", vftr_stackscount);
  int i0 = vftr_stackscount;
  if (i0 > 0) fprintf (stdout, "Check additional MPI entries:\n");
  for (int i = 0; i < i0; i++) {
  	vftr_write_function (stdout, vftr_func_table[i], true);
  }
  function_t *func1 = vftr_new_function (NULL, "init_vftr", NULL, false);
  function_t *func2 = vftr_new_function ((void*)addr, "test_1", func1, true);
  fprintf (stdout, "Check test entries:\n");
  for (int i = i0; i < vftr_stackscount; i++) {
  	vftr_write_function (stdout, vftr_func_table[i], true);
  }

#ifdef _MPI
  PMPI_Finalize();
#endif

  return 0;
}


