#include <mpi.h>

#include <stdlib.h>
#include <stdbool.h>

#include "self_profile.h"
#include "request_utils.h"
#include "requests.h"

int vftr_MPI_Waitsome(int incount, MPI_Request array_of_requests[],
                      int *outcount, int array_of_indices[],
                      MPI_Status array_of_statuses[]) {
   SELF_PROFILE_START_FUNCTION;
   if (incount <= 0) {
      outcount = 0;
      SELF_PROFILE_END_FUNCTION;
      return MPI_SUCCESS;
   }

   // First check if the request array contains at least one active handle
   bool activereqs = false;
   for (int ireq=0; ireq<incount; ireq++) {
      if (vftr_mpi_request_is_active(array_of_requests[ireq])) {
         activereqs = true;
         break;
      }
   }
   // if no active request is found return with the following settings
   if (!activereqs) {
      *outcount = MPI_UNDEFINED;
      SELF_PROFILE_END_FUNCTION;
      return MPI_SUCCESS;
   }

   int retVal = MPI_SUCCESS;
   int tmpretVal;
   *outcount = 0;
   // loop while outcount is 0
   while (*outcount == 0) {
      // loop over all requests and check for completion
      for (int ireq=0; ireq<incount; ireq++) {
         int flag;
         if (array_of_statuses == MPI_STATUSES_IGNORE) {
            tmpretVal = PMPI_Request_get_status(array_of_requests[ireq],
                                                &flag,
                                                MPI_STATUS_IGNORE);
         } else {
            tmpretVal = PMPI_Request_get_status(array_of_requests[ireq],
                                                &flag,
                                                array_of_statuses+ireq);
         }
         if (tmpretVal != MPI_SUCCESS) {
            // if something goes wrong inform the
            // user to check the status variable
            retVal = MPI_ERR_IN_STATUS;
         }
         if (flag) {
            // record completed communications for return
            (*outcount)++;
            array_of_indices[(*outcount)-1] = ireq;
            // remove completed communications from the list of open requests
            vftr_clear_completed_requests_from_wait();
            // Mark the request as inactive, or deallocate it.
            if (array_of_statuses == MPI_STATUSES_IGNORE) {
               tmpretVal = PMPI_Wait(array_of_requests+ireq,
                                     MPI_STATUS_IGNORE);
            } else {
               tmpretVal = PMPI_Wait(array_of_requests+ireq,
                                     array_of_statuses+ireq);
            }
            if (tmpretVal != MPI_SUCCESS) {
               // if something goes wrong inform the
               // user to check the status variable
               retVal = MPI_ERR_IN_STATUS;
            }
         }
      }
   }

   SELF_PROFILE_END_FUNCTION;
   return retVal;
}
