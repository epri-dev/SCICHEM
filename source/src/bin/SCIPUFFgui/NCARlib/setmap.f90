SUBROUTINE SetMapNCAR( iflag )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iflag

TYPE( T_RECT  ) client
TYPE( T_POINT ) pt
TYPE( T_SIZE  ) sz

INTEGER lok,irv
INTEGER ii,iwidp,iwidmm,ihgtp,ihgtmm,isizp,isizmm
INTEGER iwid,ihgt,iwidv,ihgtv,iofx,iofy,jofx,jofy

ii = SetMapMode( ihdcnc,MM_ISOTROPIC )

iwidp  = GetDeviceCaps( ihdcget,HORZRES )
iwidmm = GetDeviceCaps( ihdcget,HORZSIZE )
ihgtp  = GetDeviceCaps( ihdcget,VERTRES )
ihgtmm = GetDeviceCaps( ihdcget,VERTSIZE )
isizp  = MIN(iwidp,ihgtp)
isizmm = MIN(iwidmm,ihgtmm)

IF( iflag == 0 )THEN !Map to entire device
  iwid = 7000 !7.0 inches
  ihgt = 7000 !7.0 inches
  IF( isizp > 0 .AND. isizmm > 0 )THEN
    iwidv = NINT((FLOAT(iwid*iwidp)*25.4)/FLOAT(iwidmm*1000))
    ihgtv = NINT((FLOAT(ihgt*ihgtp)*25.4)/FLOAT(ihgtmm*1000))
  ELSE
    iwidv = 1024
    ihgtv = 1024
  END IF
  IF( iwidv > iwidp )THEN
    iwid  = (iwid*iwidp)/iwidv
    iwidv = iwidp
  end if
  if(ihgtv > ihgtp)then
    ihgt  = (ihgt*ihgtp)/ihgtv
    ihgtv = ihgtp
  END IF
ELSE !Map to clent window only
  irv   = GetClientRect(ihwndnc,client)
  iwidv = client%right
  ihgtv = client%bottom
  iwid  = NINT(FLOAT(iwidv*iwidmm*1000)/(FLOAT(iwidp)*25.4))
  ihgt  = NINT(FLOAT(ihgtv*ihgtmm*1000)/(FLOAT(ihgtp)*25.4))
END IF

irv = SetWindowExtEx  ( ihdcnc,iwid, ihgt, sz )
irv = SetWindowOrgEx  ( ihdcnc,0,    0,    pt )
irv = SetViewportOrgEx( ihdcnc,0,    0,    pt )
irv = SetViewportExtEx( ihdcnc,iwidv,ihgtv,sz )

ncres = MAX(iwid,ihgt)

IF( iflag == 0 )THEN !Shift on page
  jofx = MAX(0,1000 - (8500 - NINT(FLOAT(1000*iwidmm)/25.4))/2)
  jofy = MAX(0,500 - (11000 - NINT(FLOAT(1000*ihgtmm)/25.4))/2)
  pt%x = jofx
  pt%y = jofy
  lok = LPtoDP( ihdcnc,pt,1 )
  iofx = pt%x
  iofy = pt%y
  irv = SetViewportOrgEx( ihdcnc,iofx,iofy,pt )
END IF

RETURN
END
