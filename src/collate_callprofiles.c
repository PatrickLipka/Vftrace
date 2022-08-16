#include <stdlib.h>

#include <string.h>

#ifdef _MPI
#include <mpi.h>
#endif

#include "callprofiling_types.h"
#include "collated_stack_types.h"
#include "stack_types.h"

static void vftr_collate_callprofiles_root_self(collated_stacktree_t *collstacktree_ptr,
                                                stacktree_t *stacktree_ptr) {
   for (int istack=0; istack<stacktree_ptr->nstacks; istack++) {
      stack_t *stack = stacktree_ptr->stacks+istack;
      int icollstack = stack->gid;

      collated_stack_t *collstack = collstacktree_ptr->stacks+icollstack;
      callProfile_t *collcallprof = &(collstack->profile.callProf);

      collcallprof->calls = 0ll;
      collcallprof->time_usec = 0ll;
      collcallprof->time_excl_usec = 0ll;
      collcallprof->overhead_usec = 0ll;

      for (int iprof=0; iprof<stack->profiling.nprofiles; iprof++) {
         callProfile_t *callprof = &(stack->profiling.profiles[iprof].callProf);
   
         collcallprof->calls += callprof->calls;
         collcallprof->time_usec += callprof->time_usec;
         collcallprof->time_excl_usec += callprof->time_excl_usec;
         collcallprof->overhead_usec += callprof->overhead_usec;
      }
   }
}

#ifdef _MPI
static void vftr_collate_callprofiles_on_root(collated_stacktree_t *collstacktree_ptr,
                                              stacktree_t *stacktree_ptr,
                                              int myrank, int nranks,
                                              int *nremote_profiles) {
   // define datatypes required for collating callprofiles
   typedef struct {
      int gid;
      long long calls;
      long long time_usec;
      long long time_excl_usec;
      long long overhead_usec;
   } callProfile_transfer_t;

   int nblocks = 2;
   const int blocklengths[] = {1,4};
   const MPI_Aint displacements[] = {0, sizeof(int)};
   const MPI_Datatype types[] = {MPI_INT, MPI_LONG_LONG_INT};
   MPI_Datatype callProfile_transfer_mpi_t;
   PMPI_Type_create_struct(nblocks, blocklengths,
                           displacements, types,
                           &callProfile_transfer_mpi_t);
   PMPI_Type_commit(&callProfile_transfer_mpi_t);

   if (myrank > 0) {
      // every rank fills their sendbuffer
      int nprofiles = stacktree_ptr->nstacks;
      callProfile_transfer_t *sendbuf = (callProfile_transfer_t*)
         malloc(nprofiles*sizeof(callProfile_transfer_t));
      for (int istack=0; istack<nprofiles; istack++) {
         sendbuf[istack].gid = 0;
         sendbuf[istack].calls = 0ll;
         sendbuf[istack].time_usec = 0ll;
         sendbuf[istack].time_excl_usec = 0ll;
         sendbuf[istack].overhead_usec = 0ll;
      }
      for (int istack=0; istack<nprofiles; istack++) {
         stack_t *mystack = stacktree_ptr->stacks+istack;
         sendbuf[istack].gid = mystack->gid;
         // need to go over the calling profiles threadwise
         for (int iprof=0; iprof<mystack->profiling.nprofiles; iprof++) {
            profile_t *myprof = mystack->profiling.profiles+iprof;
            callProfile_t callprof = myprof->callProf;
            sendbuf[istack].calls += callprof.calls;
            sendbuf[istack].time_usec += callprof.time_usec;
            sendbuf[istack].time_excl_usec += callprof.time_excl_usec;
            sendbuf[istack].overhead_usec += callprof.overhead_usec;
         }
      }
      PMPI_Send(sendbuf, nprofiles,
                callProfile_transfer_mpi_t,
                0, myrank,
                MPI_COMM_WORLD);
      free(sendbuf);
   } else {
      int maxprofiles = 0;
      for (int irank=1; irank<nranks; irank++) {
         maxprofiles = nremote_profiles[irank] > maxprofiles ? 
                       nremote_profiles[irank] :
                       maxprofiles;
      }
      callProfile_transfer_t *recvbuf = (callProfile_transfer_t*)
         malloc(maxprofiles*sizeof(callProfile_transfer_t));
      memset(recvbuf, 0, maxprofiles*sizeof(callProfile_transfer_t));
      for (int irank=1; irank<nranks; irank++) {
         int nprofiles = nremote_profiles[irank];
         MPI_Status status;
         PMPI_Recv(recvbuf, nprofiles,
                   callProfile_transfer_mpi_t,
                   irank, irank,
                   MPI_COMM_WORLD,
                   &status);
         for (int iprof=0; iprof<nprofiles; iprof++) {
            int gid = recvbuf[iprof].gid;
            collated_stack_t *collstack = collstacktree_ptr->stacks+gid;
            callProfile_t *collcallprof = &(collstack->profile.callProf);
     
            collcallprof->calls += recvbuf[iprof].calls;
            collcallprof->time_usec += recvbuf[iprof].time_usec;
            collcallprof->time_excl_usec += recvbuf[iprof].time_excl_usec;
            collcallprof->overhead_usec += recvbuf[iprof].overhead_usec;
         }
      }
      free(recvbuf);
   }

   PMPI_Type_free(&callProfile_transfer_mpi_t);
}
#endif

void vftr_collate_callprofiles(collated_stacktree_t *collstacktree_ptr,
                               stacktree_t *stacktree_ptr,
                               int myrank, int nranks,
                               int *nremote_profiles) {
   vftr_collate_callprofiles_root_self(collstacktree_ptr, stacktree_ptr);
#ifdef _MPI
   vftr_collate_callprofiles_on_root(collstacktree_ptr, stacktree_ptr,
                                     myrank, nranks, nremote_profiles);
#else
   (void) myrank;
   (void) nranks;
   (void) nremote_profiles;
#endif
}