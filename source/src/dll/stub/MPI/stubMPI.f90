MODULE STUBMPI

! Stub module for MPI to allow SCICHEM to be compiled *
! without installing MPI.                             *
! This code has been developed under contract EP-     *
! for  EPRI, 3412 Hillview Ave., Palo Alto, CA 94304  *
!                                                     *
! Developed in April 2012 by Biswanath Chowdhury,     *
! Sage Management, 100 Rozel Road, Suite 102          *
! Princeton, NJ                                       *

  INTEGER, PARAMETER :: MPI_COMM_WORLD       = 0
  INTEGER, PARAMETER :: MPI_CHARACTER        = 0
  INTEGER, PARAMETER :: MPI_INTEGER          = 0
  INTEGER, PARAMETER :: MPI_INTEGER1         = 0
  INTEGER, PARAMETER :: MPI_REAL             = 0
  INTEGER, PARAMETER :: MPI_DOUBLE_PRECISION = 0
  INTEGER, PARAMETER :: MPI_LOGICAL          = 0
  INTEGER, PARAMETER :: MPI_PACKED           = 0
  INTEGER, PARAMETER :: MPI_SUCCESS          = 0
  INTEGER, PARAMETER :: MPI_ADDRESS_KIND     = 4
  INTEGER, PARAMETER :: MPI_STATUS_SIZE      = 1
  INTEGER, PARAMETER :: MPI_MAX_ERROR_STRING = 1

  CONTAINS

  !=======================================================================

  SUBROUTINE MPI_INIT( ierr )

  IMPLICIT NONE

  INTEGER ierr

  !Skip writing MPI library message for beta2 version
  !WRITE(*,*)
  !WRITE(*,*)'***************************************************************'
  !WRITE(*,*)'                    Using stubMPI library'
  !WRITE(*,*)' Change localMPI.f90 to use MPICH2 libs for multiprocessor runs'
  !WRITE(*,*)'***************************************************************'
  !WRITE(*,*)

  ierr = 0

  RETURN
  END SUBROUTINE MPI_INIT

  !=======================================================================

  SUBROUTINE MPI_FINALIZE( ierr )

  IMPLICIT NONE

  INTEGER ierr

  ierr = 0

  RETURN
  END SUBROUTINE MPI_FINALIZE

  !=======================================================================

  SUBROUTINE MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )

  IMPLICIT NONE

  INTEGER MPI_COMM_WORLD, myid, ierr

  myid = 0
  ierr = 0

  RETURN
  END SUBROUTINE MPI_COMM_RANK

  !=======================================================================

  SUBROUTINE MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

  IMPLICIT NONE

  INTEGER MPI_COMM_WORLD, numprocs, ierr

  numprocs = -1
  ierr = 0

  RETURN
  END SUBROUTINE MPI_COMM_SIZE

  !=======================================================================

  SUBROUTINE MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, displacements,typelist, MPI_PUFFSTRUC, ierr)

  IMPLICIT NONE

  INTEGER                                       ::  nsize, MPI_PUFFSTRUC, ierr
  INTEGER (KIND=MPI_ADDRESS_KIND), DIMENSION(*) :: displacements
  INTEGER, DIMENSION(*)                         :: block_lengths, typelist

  ierr = 0

  RETURN
  END SUBROUTINE MPI_TYPE_CREATE_STRUCT

  !=======================================================================

  SUBROUTINE MPI_TYPE_COMMIT( MPI_PUFFSTRUC, ierr )

  IMPLICIT NONE

  INTEGER  MPI_PUFFSTRUC, ierr

  ierr = 0

  RETURN
  END SUBROUTINE MPI_TYPE_COMMIT

  !=======================================================================

  SUBROUTINE MPI_ABORT( MPI_COMM_WORLD, irv, ierr )

  IMPLICIT NONE

  INTEGER  MPI_COMM_WORLD, irv, ierr

  ierr = 0
  irv  = 0

  RETURN
  END SUBROUTINE MPI_ABORT

  !=======================================================================

  REAL FUNCTION MPI_Wtime( )

  IMPLICIT NONE

  INTEGER, DIMENSION(8) :: values

  INTEGER jday

  INTEGER, EXTERNAL :: julian_day

  CALL DATE_AND_TIME(VALUES = values)

  !Value 1: 4 digit yr, 2: month, 3: day, local -> 5: hr, 6: min, 7: sec, 8: millisec

  jday = julian_day( values(2),values(3),values(1) )

  MPI_Wtime = ((jday*24. + values(5))*60. + values(6))*60. + values(7)

  RETURN
  END FUNCTION MPI_Wtime

END MODULE STUBMPI
