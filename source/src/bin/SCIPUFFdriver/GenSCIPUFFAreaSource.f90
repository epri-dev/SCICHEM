INTEGER FUNCTION GenSCIPUFFAreaSource()

!------ Generate SCIPUFF area sources
!       Add multicomponent releases for all sources

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

INTEGER nrel, irel, alloc_stat !, nx, nxy, ns
INTEGER i, j
REAL    area, rho, vel

REAL, EXTERNAL :: GetMatlDensity

GenSCIPUFFAreaSource = FAILURE

!------ Initialize for zero area sources

nrel  = new%scnHead%number
nrel0 = nrel

ALLOCATE( iAreaSrc(nrel,2),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating area source index array'
  GOTO 9999
END IF

iAreaSrc = 0

!------ Check for area sources

DO irel = 1,nrel0

  IF( relList(irel)%type == HR_AREA .OR. relList(irel)%type == HR_AREAF )THEN

    relAreaData = TRANSFER(relList(irel)%relData,relAreaData)

!    CALL NumAreaSources( relAreaData%xlen,relAreaData%ylen,nx,ny ); ns = nx*ny

    area = relAreaData%xlen * relAreaData%ylen

!------ Compute velocity to insure implied density is less than material density
!       but set to at least 1 cm/s. N.B. Assumes material density is in kg/m^3

    rho = GetMatlDensity( relList(irel)%material )
    IF( rho > 0. )THEN
      vel = MAX(relAreaData%rate/rho,0.01)
    ELSE
      vel = 0.01
    END IF

!------ Set dynamice parameters

    relAreaData%rate     = relAreaData%rate * area  !Emissions is mass/s/area for area sources (only)
    relAreaData%exitTemp = DEF_VAL_R                !Neutrally buoyant
    relAreaData%exitVel  = vel                      !Small velocity

!------ Multicomponent rates are per area

    IF( nMC > 0 )THEN
      DO i = 1,nMC
        j = (irel-1)*nMC + i
        relMCList(j)%MCmass = relMCList(j)%MCmass * area
      END DO
    END IF


     relList(irel)%relData = TRANSFER(relAreaData,relList(irel)%relData)

     iAreaSrc(irel,1) = 1 !ns

 END IF

END DO

new%scnHead%number = nrel

GenSCIPUFFAreaSource = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE NumAreaSources( xlen,ylen,nx,ny )

USE SCIPUFFdriver_fi

IMPLICIT NONE

REAL,    INTENT( IN  ) :: xlen, ylen !Length and width of source rectangle
INTEGER, INTENT( OUT ) :: nx,   ny   !Number of sources dividing rectangle in x,y

REAL, PARAMETER :: AR  = 10.        !Maximum allowable aspect ratio
REAL, PARAMETER :: tol = 0.01       !Tolerance

REAL r

IF( ylen < xlen )THEN
  r  = xlen/ylen
  ny = nAreaSrc
  nx = INT((r+AR-tol)/AR) * ny
ELSE
  r  = ylen/xlen
  nx = nAreaSrc
  ny = INT((r+AR-tol)/AR) * nx
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SetMCrelease()

!------ Setup multicomponent releases

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, k

SetMCrelease = FAILURE

nMCrel = nMC * new%scnHead%number

IF( nMCrel > 0 )THEN

  ALLOCATE( relMCList(nMCrel),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    WRITE(*,*) 'Error allocating multicomponent list array'
    WRITE(*,*) 'No. of releases: ',new%scnHead%number
    GOTO 9999
  END IF

  k = 0
  DO i = 1,new%scnHead%number
    DO j = 1,nMC
      k = k + 1
      relMCList(k)%relID = i
      relMCList(k)%MCname = TRIM(MCname(j))
      relMCList(k)%MCmass = MCrate(j,i)
    END DO
  END DO

END IF

SetMCrelease = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReallocMCRelList( inc )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: inc  !Number of new releases

INTEGER alloc_stat, i

TYPE( releaseMCT ), DIMENSION(:), ALLOCATABLE :: tmpList

ReallocMCRelList = FAILURE

!------ Allocate temporary list

ALLOCATE( tmpList(nMCrel),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating temporary MC release array'
  GOTO 9999
END IF

!------ Fill it with current releases

DO i = 1,nMCrel
  tmpList(i) = relMCList(i)
END DO

!------ Deallocate old list; reallocate with larger size

DEALLOCATE( relMCList,STAT=alloc_stat )

ALLOCATE( relMCList(nMCrel+inc*nMC),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error re-allocating new MC release array'
  GOTO 9999
END IF

!------ Copy current releases back; increment new%scnHead%max

DO i = 1,nMCrel
  relMCList(i)   = tmpList(i)
END DO

!------ Deallocate temporary list

DEALLOCATE( tmpList,STAT=alloc_stat )

ReallocMCRelList = SUCCESS

9999 CONTINUE

RETURN
END

