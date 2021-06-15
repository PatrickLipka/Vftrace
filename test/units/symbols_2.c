#include "vftr_setup.h"
#include "vftr_symbols.h"
#ifdef _MPI
#include <mpi.h>
#endif

int main (int argc, char **argv) {

#if defined(_MPI)
  PMPI_Init(&argc, &argv);
  vftr_get_mpi_info (&vftr_mpirank, &vftr_mpisize);
#else 
  vftr_mpirank = 0;
  vftr_mpisize = 1;
#endif

  if (argc != 2) {
    printf ("Nr. of arguments: %d\n", argc);
    printf ("symbols_test_1: No input file\n", argc);
    return 1;
  }

  char *filename_in = argv[1];
  FILE *fp_in = fopen (filename_in, "r");
  if (fp_in == NULL) {
     printf ("File %s not found!\n", filename_in);
     return 1;
  }

  vftr_nsymbols = 0;
  vftr_get_library_symtab ("", fp_in, 0, 0);
  vftr_symtab = (symtab_t **) malloc (vftr_nsymbols * sizeof(symtab_t*));
  
  vftr_nsymbols = 0;
  rewind(fp_in);
  vftr_get_library_symtab("", fp_in, 0, 1);

  for (int i = 0; i < vftr_nsymbols; i++) {
    fprintf (stdout, "%s -> %s\n", vftr_symtab[i]->name, vftr_demangle_cpp(vftr_symtab[i]->name));
  }

  free (vftr_symtab);
  fclose(fp_in);
#ifdef _MPI
  PMPI_Finalize();
#endif
}
