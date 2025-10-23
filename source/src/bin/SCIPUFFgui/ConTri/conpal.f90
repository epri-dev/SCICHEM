subroutine read_pal
use errorParam_fd
use files_fi
use contri_fi

IMPLICIT NONE

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

integer i,j,nch,nblank,irv

nError = NO_ERROR

open(unit=lun_pal,file=cpal,status='old',iostat=irv)
if(irv /= NO_ERROR)then
  nError = OP_ERROR
  eRoutine ='ReadPal'
  eMessage ='Cannot find Palette File'
  CALL ReportFileName( eInform,'File=',cpal )
  eAction  ='Make sure the file exists'
  goto 9999
end if

read(lun_pal,*,iostat=irv)npalln,npalar,npalcn
if(irv /= NO_ERROR)then
  nError = RD_ERROR
  eRoutine ='ReadPal'
  eMessage ='Cannot read Palette File'
  CALL ReportFileName( eInform,'File=',cpal )
  eAction  ='Make sure file is constructed correctly'
  goto 9999
end if

ipalln = 1
ipalar = ipalln + npalln
ipalcn = ipalar + npalar
npal = ipalcn + npalcn
do i = 0,npal
  read(lun_pal,*,iostat=irv)(rgb(j,i),j=1,3)
end do
if(irv /= NO_ERROR)then
  nError = RD_ERROR
  eRoutine ='SetContriDefault'
  eMessage ='Cannot read Palette File'
  CALL ReportFileName( eInform,'File=',cpal )
  eAction  ='Make sure file is constructed correctly'
  goto 9999
end if
close(unit=lun_pal,iostat=irv)
lpal = .true.

return

!------ Error section

9999	nch = nblank(cpal)
lpal = .false.
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
return

end

!----------------------------------------------------------------------

subroutine setpal(lprint)

USE winAPI_fd, ONLY: POINTER_LEN

IMPLICIT NONE

logical lprint

INTEGER(POINTER_LEN) idc,jdc

if(.not.lprint)then
  call GetNCARDC(idc,jdc)
  call ResetPalette(idc)
end if

call SetNCARPrint(lprint)

return
	end

!----------------------------------------------------------------------

subroutine loadpal
use contri_fi

IMPLICIT NONE


integer i,j
logical lerr

lerr = .false.

if(lpal)then
  do i=0,npal
    call gscr(i,rgb(1,i),rgb(2,i),rgb(3,i),lerr)
    if(lerr)goto 9999
  end do
  do j = 0,npal
    i = j + npal + 1
    call gscr(i,1.-rgb(1,j),1.-rgb(2,j),1.-rgb(3,j),lerr)
    if(lerr)goto 9999
  end do
  lpal = .false.
end if

return
9999  call checkpalette
return
end

!----------------------------------------------------------------------

subroutine ResetPalette(idc)
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) idc

INTEGER(POINTER_LEN) ihpal
INTEGER              iii

call GetNCARPalette(ihpal)
if(ihpal /= 0)then
  iii = SelectPalette(idc,ihpal,0)
  iii = RealizePalette(idc)
!	  if(iii .eq. 0)then
!	    iii = UpdateColors(idc)
!	  end if
else
  call checkpalette
end if

return
	end
