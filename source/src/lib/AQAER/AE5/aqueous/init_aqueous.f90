!***********************************************************************
! This is the SCICHEM initialization routine for the RADM aqueous-phase*
! chemistry module in CMAQ (October 2004 version)                      *
! This code has been developed for Southern Company Services under     *
! subcontract to EPRI, 3412 Hillview Ave., Palo Alto, CA 94304         *
! Contract EP-P14638/C7185                                             *
!                                                                      *
! Developed by Prakash Karamchandani                                   *
! Atmospheric and Environmental Research, Inc., 2682 Bishop Drive,     *
! Suite 120, San Ramon, CA 94583                                       *
!                                                                      *
! Revisions:                                                           *
!    February 2012: Updated for CMAQ 4.7.1, PK, ENVIRON                *
!***********************************************************************
subroutine init_aqueous(lun)

!====  Initialize aqueous module for multi-component runs
!      This routine is called by init_mcp, which is called in start via
!      read_mc and directly from restart
!
!      This version is compatible with the CMAQ AE5 aerosol/aqueous modules
!      (modal approach), Prakash Karamchandani, ENVIRON, February 2012
!***********************************************************************

use AqAer_fi
use aqueous_species_inc
!USE mpi_fi, ONLY: myid

implicit none

integer i, j, lun, spc ! loop variables

!debug
!WRITE(99+myid,*)'Total no. of aqueous species, naqueous: ',naqueous
!WRITE(99+myid,*)'species names: '
!do i = 1, naqueous
!  WRITE(99+myid,*)'spc, name: ',i,aqueous_names(i)
!end do
!debug

! Initialize required species for aqueous module
index_aqueous = 0

! First all particle species common to both aero and aqueous
index_aqueous(1:naerp) = index_aero(1:naerp)

! Next all gas-phase species required for aqueous chemistry
! and scavenging

! ---  identify all species that are passed to aqueous module
outer: do j = naerp+1,naqueous
  do i = 1,nsp
    if (TRIM(name(i)) == TRIM(aqueous_names(j))) then
      index_aqueous(j)    = i        !aqueous module species pointer
      CYCLE outer ! search for next species
    end if
  end do
  error%nError   = IV_ERROR
  error%eRoutine = 'init_aqueous'
  error%eMessage = 'Necessary species for aqueous module: ' // &
             TRIM(aqueous_names(j)) // ',  not found in species list'
  go to 9999
end do outer

! below is redundant since check is above
! kept for the time being for the debug statement in the loop
do j = 1, naqueous
  if ( index_aqueous(j) == 0 ) then
    error%nError   = IV_ERROR
    error%eRoutine = 'init_aqueous'
    error%eMessage = 'Necessary species for aqueous module: ' // &
               TRIM(aqueous_names(j)) // ',  not found in species list'
    go to 9999
  end if
!debug
!  WRITE(99+myid,*)'j, index: ',j,index_aqueous(j)
!debug
end do

!debug
!WRITE(99+myid,*)'init_aqueous'
!do j = 1, naqueous
!   WRITE(99+myid,*)'Aqueous species, index-local, model species: ', &
!             TRIM(aqueous_names(j)),index_aqueous(j), &
!             TRIM(name(index_aqueous(j)))
!end do
!debug

9999  return
      end
