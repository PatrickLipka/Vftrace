#ifndef RANKLOGFILE_CUPTI_TABLE_H
#define RANKLOGFILE_CUPTI_TABLE_H

void vftr_get_total_cupti_times_for_ranklogfile (stacktree_t stacktree, 
                                             float *tot_compute_s, float *tot_memcpy_s, float *tot_other_s);

void vftr_write_ranklogfile_cupti_table(FILE *fp, stacktree_t stacktree);

#endif