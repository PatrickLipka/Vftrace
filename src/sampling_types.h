#ifndef SAMPLING_TYPES_H
#define SAMPLING_TYPES_H

#include <stdio.h>
#include <stdbool.h>

typedef enum {
   samp_function_entry,
   samp_function_exit,
   samp_message
} sample_kind;

typedef struct {
   bool do_sampling;
   char *vfdfilename;
   FILE *vfdfilefp;
   char *iobuffer;
   long long interval;
   long long nextsampletime;
   unsigned int function_samplecount;
   unsigned int message_samplecount;
   long int stacktable_offset;
   long int samples_offset;
} sampling_t;

#endif