#include <stdio.h>
#define UNUSED(x) (void)(x)
#ifdef __cplusplus
extern "C" {
#endif
void MPI_BCAST(int *p, int Count, int MPI_TYPE, int Source, int MPI_COMM_WORLD,
               int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(Source);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_bcast_(int *p, int Count, int MPI_TYPE, int Source, int MPI_COMM_WORLD,
                int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(Source);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_SEND(int *p, int Count, int MPI_TYPE, int dest, int tag,
              int MPI_COMM_WORLD, int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(dest);
  UNUSED(tag);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_send_(int *p, int Count, int MPI_TYPE, int dest, int tag,
               int MPI_COMM_WORLD, int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(dest);
  UNUSED(tag);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_RECV(int *p, int Count, int MPI_TYPE, int source, int tag,
              int MPI_COMM_WORLD, int stat, int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(source);
  UNUSED(tag);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(stat);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_recv_(int *p, int Count, int MPI_TYPE, int source, int tag,
               int MPI_COMM_WORLD, int stat, int ierr) {
  UNUSED(p);
  UNUSED(Count);
  UNUSED(MPI_TYPE);
  UNUSED(source);
  UNUSED(tag);
  UNUSED(MPI_COMM_WORLD);
  UNUSED(stat);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void MPI_GET_ADDRESS(int *p, int Address, int ierr) {
  UNUSED(p);
  UNUSED(Address);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
void mpi_get_address_(int *p, int Address, int ierr) {
  UNUSED(p);
  UNUSED(Address);
  UNUSED(ierr);
  printf("\nError: Must link with mpich2 and not stubMPI for using MPI\n");
}
#ifdef __cplusplus
}
#endif
