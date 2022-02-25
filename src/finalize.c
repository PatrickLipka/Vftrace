#ifdef _DEBUG
#include <stdio.h>
#include "timer.h"
#endif

#include "off_hooks.h"
#include "cyghooks.h"
#include "vftr_hooks.h"
#include "vftrace_state.h"
#include "environment.h"
#include "symbols.h"

void vftr_finalize() {
   // update the vftrace state
   vftrace.state = finalized;
#ifdef _DEBUG
   fprintf(stderr, "Vftrace finalized at ");
   vftr_print_date_str(stderr);
   fprintf(stderr, "\n");
#endif





   // free the symbol table
   vftr_symboltable_free(&vftrace.symbols);

   // free the environment to avoid memory leaks
   vftr_environment_free(&(vftrace.environment));

   // redirect the function entry and exit hooks to deactivate them
   // use a dummy function that does nothing
   vftr_set_enter_func_hook(vftr_function_hook_off);
   vftr_set_exit_func_hook(vftr_function_hook_off);
}