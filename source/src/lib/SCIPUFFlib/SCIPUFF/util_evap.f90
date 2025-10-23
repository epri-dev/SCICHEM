SUBROUTINE set_srf_evap( p,pq,d_evap,w_evap )

USE scipuff_fi

!  Sets surface deposition variables for secondary evaporation
!       d_evap = mean diameter
!       w_evap = total wetted area

IMPLICIT NONE

TYPE( puff_str ),     INTENT( IN  ) :: p
TYPE( puff_liquid  ), INTENT( IN  ) :: pq
REAL,                 INTENT( OUT ) :: d_evap, w_evap

REAL rhod

TYPE( puff_material )   pmatlIn
TYPE( liquid_material ) pmatl

CALL get_puff_material( p%ityp,pmatlIn )
pmatl = TRANSFER(pmatlIn,pmatl)

rhod   = pmatl%rho - pmatl%rhob*(pq%t+ABSZERO)
d_evap = pmatl%sf*pq%d
w_evap = 1.5*pmatl%sf**2/(rhod*pq%d)

RETURN
END

!===============================================================================

SUBROUTINE update_evap_step( si,mlev )

USE scipuff_fi
USE files_fi
USE srfevap_fi

!  Updates surface evaporation time step information (called from puff_dos)

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mlev
REAL,    INTENT( IN ) :: si

INTEGER ilev, jlev, ios
REAL    facu, fac, dti

INTEGER, EXTERNAL :: time_level

!Skip this function if call to surface_dos is originating in slice routines
IF( dx_evap == NOT_SET_R )GOTO 9999

dti  = del_evap
facu = facu_evap

ilev = MAX(mlev-2,0)

fac = 0.5**ilev
dti = dti * fac
dti  = MIN(si**TwoThirds*facu,dti)

jlev = time_level( dti )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'update_evap_step'
  WRITE(lun_log,*,IOSTAT=ios)'******* TIME LEVEL ERROR ********'
  WRITE(lun_log,*,IOSTAT=ios)TRIM(eRoutine)
  WRITE(lun_log,*,IOSTAT=ios)TRIM(eInform)
  WRITE(lun_log,*,IOSTAT=ios)'DT=',dti
  WRITE(lun_log,*,IOSTAT=ios)'DELT=',delt
  WRITE(lun_log,*,IOSTAT=ios)'LEVEL=',jlev,MAXTLV,mlev
  WRITE(lun_log,*,IOSTAT=ios)'dx,facu,si=',dx_evap,facu,si
  GOTO 9999
END IF

lev_evap(ilev) = MAX(lev_evap(ilev),jlev)

mxlev_evap  = MAX(mxlev_evap,ilev)
mxtlev_evap = MAX(mxtlev_evap,lev_evap(ilev))

9999 CONTINUE

RETURN
END
