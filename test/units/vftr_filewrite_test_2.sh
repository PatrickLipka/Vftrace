#!/bin/bash
set -x
ref_out_dir=ref_output
ref_in_dir=ref_input
testname=vftr_filewrite_test_2
outfile=$testname.out

rm -f $outfile

./test_vftrace $testname
diff $ref_out_dir/$outfile $outfile

