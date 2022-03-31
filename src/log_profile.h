#ifndef LOG_PROFILE_H
#define LOG_PROFILE_H

#include "stack_types.h"

long long vftr_total_overhead_usec(stacktree_t stacktree);

int *vftr_stack_calls_list(int nstacks, stack_t *stacks);

double *vftr_stack_inclusive_time_list(int nstacks, stack_t *stacks);

double *vftr_stack_exclusive_time_list(int nstacks, stack_t *stacks);

double *vftr_stack_overhead_time_list(int nstacks, stack_t *stacks);

char **vftr_stack_function_name_list(int nstacks, stack_t *stacks);

char **vftr_stack_caller_name_list(int nstacks, stack_t *stacks);

#endif