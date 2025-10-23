subroutine init_aero(lun)

!====  Initialize aerosol module for multi-component runs
!      This routine is called by init_mcp, which is called in start via
!      read_mc and directly from restart
!
!      This version is compatible with the CMAQ v4.7.1 modal approach
!      Updated February 2012 by Prakash Karamchandani, ENVIRON

use AqAer_fi

implicit none

INTEGER lun
!character*(*) name(*), file_imc
!integer nsp,lun,index_aero(*),index_aerp(*),index_sec(*)
!integer nsec_aer,naero,naerp

!real dm_aer(*),secbnds_aer(*),aeroma(*),surfp(*)

! Locals
integer i, j, spc ! loop variables

!debug
!debugwrite(lun,*)'naero,naerp: ',naero,naerp
!debugwrite(lun,*)'species names: '
!debugdo i = 1, naero
!debugend do
!debug

! ---  identify all species that are passed to the aerosol module
index_aero = 0

outer: do j = 1,naero
  do i = 1,nsp
    if (TRIM(name(i)) == TRIM(aero_names(j))) then
      index_aero(j)    = i        !aerosol module species pointer
      CYCLE outer ! search for next species
    end if
  end do
! No match found
  error%nError   = IV_ERROR
  error%eRoutine = 'init_aero'
  error%eMessage = 'Necessary species for aerosol module: ' // &
              TRIM(aero_names(j)) // ',  not found in species list'
  go to 9999
end do outer

! below is redundant since check is above
! kept for the time being for the debug statement in the loop
do j = 1, naero
  if ( index_aero(j) == 0 ) then
    error%nError   = IV_ERROR
    error%eRoutine = 'init_aero'
    error%eMessage = 'Necessary species for aerosol module: ' // &
               TRIM(aero_names(j)) // ',  not found in species list'
    go to 9999
  end if
!debug
!debug
end do

!debug
!debugwrite(lun,*)'init_aero'
!debugdo j = 1, naero
!debugend do
!debug

9999  return
end
