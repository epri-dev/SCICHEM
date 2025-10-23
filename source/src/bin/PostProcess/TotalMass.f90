SUBROUTINE TotalMass()

USE Extract_fi
USE GetTimes_fi
USE TotalMass_fi

IMPLICIT NONE

INTEGER it, irv
CHARACTER(16) selection

INTEGER,       EXTERNAL :: GetProjectPlotTimes
INTEGER,       EXTERNAL :: GetProjectPlotLists
CHARACTER(16), EXTERNAL :: SelectMaterial

nMatl = 0

DO

  CALL GetProject()
  IF( nError/= NO_ERROR)GOTO 9999

  irv = GetProjectPlotLists( Project )
  IF( irv /= SCIPsuccess )GOTO 9999

  irv = GetProjectPlotTimes( Project )
  IF( irv /= SCIPsuccess )GOTO 9999

  CALL AllocateTotalMass( nTimePuff,nTimeSrf )
  IF( irv /= SCIPsuccess )GOTO 9999


  DO
    selection = SelectMaterial()
    IF( nError/= NO_ERROR)GOTO 9999

    IF( LEN_TRIM(selection) <= 0 )EXIT

    WRITE(6,'(/,A)')'Active Mass of '//TRIM(selection)//' in project '//TRIM(Project%name)

    IF( nTimePuff > 0 )THEN
      airMass = 0.
      WRITE(6,'(/,A)')'Cloud Mass of '//TRIM(selection)
      WRITE(6,'(A)')'   Time     Puffs   Mass    StaticMass  CC sum    CCB sum    Xbar       Ybar      Zbar'
      DO it = 1,nTimePuff
        CALL GetCloudStats( selection,it )
        IF( nError/= NO_ERROR )GOTO 9999
      END DO
    ELSE
      WRITE(6,'(/,A)')'No Cloud Mass of '//TRIM(selection)
    END IF

    IF( nTimeSrf > 0 )THEN
      Field%maxCells = limit%surfaceGrid
      Field%project  = TRIM(Project%name)
      Field%path     = TRIM(Project%path)
      CALL BuildField( selection )
      IF( Field%class /= 0 )THEN
        srfMass = 0.
        WRITE(6,'(/,A)')'Surface Mass of '//TRIM(selection)
        WRITE(6,'(A)')'   Time     Nodes   Mass       Area       Xbar       Ybar'
        DO it = 1,nTimeSrf
          CALL GetSurfaceStats( it )
          IF( nError/= NO_ERROR)GOTO 9999
        END DO
      ELSE
        WRITE(6,'(/,A)')'No Surface Mass of '//TRIM(selection)
      END IF
    ELSE
      WRITE(6,'(/,A)')'No Surface Mass of '//TRIM(selection)
    END IF
    IF( nTimeSrf*nTimePuff > 0 )THEN
      WRITE(6,'(/,A)')'Total Mass of '//TRIM(selection)
      WRITE(6,'(A)')'   Time      Mass'
      DO it = 1,nTimeSrf
        CALL GetTotalStats( it )
        IF( nError/= NO_ERROR )GOTO 9999
      END DO
    END IF

    WRITE(6,'(/)')

    IF( nMatl == 1 )EXIT

  END DO

  nMatl   = 0
  PrjName = ' '
  CALL ClearMemory()

END DO

9999 CONTINUE

IF( nError /= NO_ERROR )CALL showError()

IF( ALLOCATED(materials) )DEALLOCATE( materials,STAT=irv )
CALL ClearTotalMass()
CALL ClearLocalPuffs()
CALL ClearMemory()

RETURN
END
!==============================================================================
! SelectMaterial
!==============================================================================
CHARACTER(*) FUNCTION SelectMaterial()

USE Extract_fi
USE SCIPtool
USE TotalMass_fi

IMPLICIT NONE

INTEGER nMtl, irv, i, j, itry

CHARACTER(32)       string, string1
TYPE( fileNameT )   filename
TYPE( pmaterialT )  matHead

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

IF( nMatl == 0 )THEN
  filename%string = TRIM(AddExtension(Project%name,'inp'))
  CALL AddPath( filename%string,TRIM(Project%path) )

  irv = SCIPCountMaterial( callerID,filename,nMtl )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Failed retrieving number of project materials' )
    GOTO 9999
  END IF

  IF( ALLOCATED(materials) )DEALLOCATE( materials,STAT=irv )
  ALLOCATE( materials(nMtl),STAT=irv )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SelectMaterial'
    eMessage = 'Error allocating material array'
    WRITE(eInform,'(A,I0,A,I0)')'Request =',nMtl,'  : Error =',irv
    GOTO 9999
  END IF

  matHead%project        = Project
  matHead%mtlHead%max    = nMtl
  matHead%mtlHead%number = 0
  matHead%control%mode   = SCIPnull
  matHead%control%mode   = IBSET(matHead%control%mode,HCB_FILE)
  matHead%control%searchName    = ' '
  matHead%control%fileExtension = 'inp'
  irv = SCIPLoadMaterialF( callerID,matHead,materials )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Failed retrieving project materials' )
    GOTO 9999
  END IF

  nMatl = matHead%mtlHead%number
END IF

IF( nMatl > 1 )THEN
  WRITE(6,'(A)')'Available Materials for '//TRIM(Project%name)
  WRITE(6,'(A)')'===================================='
  DO i = 1,nMatl
    WRITE(6,'(I3,1X,A)')i,TRIM(materials(i)%name)
  END DO

  SelectMaterial = ' '

  itry = 0

  100 CONTINUE

  WRITE(6,'(/,A,$)')'Material (CR=Exit)? '
  READ(lun_in,'(A)')string
  IF( LEN_TRIM(string) <= 0 )GOTO 9999
  string = ADJUSTL(string)
  CALL CUPPER( string )
  j = LEN_TRIM(string)
  DO i = 1,nMatl
    string1 = materials(i)%name
    CALL CUPPER( string1 )
    IF( INDEX(string1,string(1:j)) > 0 )THEN
      SelectMaterial = materials(i)%name
      EXIT
    END IF
  END DO
  WRITE(6,*)'Selected '//TRIM(SelectMaterial)

  itry = itry + 1
  IF( LEN_TRIM(SelectMaterial) < 1 )THEN
    IF( itry < maxTry )THEN
      WRITE(6,'(A)')'Invalid material name, try again'
      GOTO 100
    ELSE
      WRITE(6,'(A)')'Unable to set material selection, exiting'
      nError   = UK_ERROR
      eRoutine = 'SelectMaterial'
      eMessage = 'Unable to set material selection'
      GOTO 9999
    END IF
  END IF
ELSE
  SelectMaterial = materials(1)%name
END IF


WRITE(6,*)'Selected '//TRIM(SelectMaterial)
9999 CONTINUE

RETURN
END
!==============================================================================
! GetCloudStats
!==============================================================================
SUBROUTINE GetCloudStats( selection,it )

USE Extract_fi
USE SCIPtool
USE GetTimes_fi
USE localpuf
USE TotalMass_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: selection
INTEGER,      INTENT( IN ) :: it

INTEGER i, irv, ipuff

REAL(8) csum, ccsum, ccbsum, xsum, ysum, zsum, mass, ssum
REAL(4) tx

TYPE( ppuffHeadT ) :: puffHead

INTEGER, EXTERNAL :: GetProjectPuffs

puffHead%project = Project

irv = GetProjectPuffs( callerID,puffHead,it,.FALSE.,.FALSE.,0 )
IF( irv /= SCIPsuccess )GOTO 9999

IF( it == 1 )THEN
  iMatl = 0
  DO i = 1,puffHead%puff%nType
    IF( TRIM(puffType(i)%material) == TRIM(selection) )THEN
      iMatl = puffType(i)%imat
      EXIT
    END IF
  END DO
END IF

tx = TimePuff(it)%time%runtime

csum  = 0.D0
ssum  = 0.D0
ccsum = 0.D0
ccbsum = 0.D0
xsum = 0.D0
ysum = 0.D0
zsum = 0.D0
ipuff = 0
DO i = 1,puffHead%puff%nPuff
  IF( puffType(puffs(i)%ityp)%imat == iMatl )THEN
      ipuff = ipuff + 1
      mass = DBLE(puffs(i)%c)*DBLE(puffs(i)%cfo)
      csum  = csum  + mass
      IF( puffs(i)%idtl < 0 )ssum = ssum + mass
      ccsum  = ccsum  + DBLE(puffs(i)%cc) *DBLE(puffs(i)%cfo)**2
      ccbsum = ccbsum + DBLE(puffs(i)%ccb)*DBLE(puffs(i)%cfo)**2
      xsum   = xsum   + DBLE(puffs(i)%xbar)*mass
      ysum   = ysum   + DBLE(puffs(i)%ybar)*mass
      zsum   = zsum   + DBLE(puffs(i)%zbar)*mass
  END IF
END DO
IF( csum > 0.0 )THEN
  WRITE(6,100)tx,ipuff,csum,ssum,ccsum,ccbsum,xsum/csum,ysum/csum,zsum/csum
ELSE
  WRITE(6,100)tx,ipuff,csum,ccsum,ccbsum,(0.,i=1,4)
END IF
100 FORMAT(ES10.3,I7,4ES10.3,2ES11.3,ES10.3 )

airMass(it) = csum

9999 CONTINUE

RETURN
END
!==============================================================================
! GetSurfaceStats
!==============================================================================
SUBROUTINE GetSurfaceStats( it )

USE Extract_fi
USE SCIPtool
USE GetTimes_fi
USE localpuf
USE TotalMass_fi
USE scipuff_fi, ONLY: lmap

IMPLICIT NONE

INTEGER, INTENT( IN ) :: it

INTEGER i, irv, fID

REAL(8) csum, xsum, ysum, mass, area, asum
REAL(4) xfac, yfac

INTEGER, EXTERNAL :: PostCreateField

fID = -1

IF( Field%class == 0 )GOTO 9999

Field%timeID = it
Field%userTime = TimeSrf(it)%time%runtime

fID = PostCreateField( Field,ClassData,.FALSE.,'TotalMass' )
IF( fID < 0 )GOTO 9999

CALL ExtractNative( fID,Field )

lmap = Field%coordinate%mode

csum = 0.0D0
xsum = 0.0D0
ysum = 0.0D0
asum = 0.0D0
!WRITE(123,*)'======================================='
!WRITE(123,*)Field%userTime
!WRITE(123,*)'======================================='
!WRITE(124,*)'======================================='
!WRITE(124,*)Field%userTime
!WRITE(124,*)'======================================='
DO i = 1,nNode
!  WRITE(123,*)i,fNodes(i)%x,fNodes(i)%y,fNodes(i)%v
  IF( fNodes(i)%v > 1.0E-30 )THEN
    CALL mapfac( fNodes(i)%x,fNodes(i)%y,xfac,yfac )
    area = DBLE((fNodes(i)%hx)/DBLE(xfac)) * DBLE((fNodes(i)%hy)/DBLE(yfac))
!    WRITE(124,*)i,fNodes(i)%hx,fNodes(i)%hy,SNGL(area)
    asum = asum + area
    mass = DBLE(fNodes(i)%v)*area
    csum = csum + mass
    xsum = xsum + DBLE(fNodes(i)%x)*mass
    ysum = ysum + DBLE(fNodes(i)%y)*mass
  END IF
END DO

IF( csum > 0.0 )THEN
  WRITE(6,100) Field%userTime,nNode,csum, asum, xsum/csum,ysum/csum
ELSE
  WRITE(6,100) Field%userTime,nNode,csum,0.,0.,0.
END IF
100 FORMAT(ES10.3,I7,ES10.3,3ES11.3 )

srfMass(it) = csum

9999 CONTINUE

IF( fID > 0 )THEN
  irv = SCIPDeleteField( callerID,fID )
END IF

IF( ALLOCATED(fNodes    ) )DEALLOCATE( fNodes,    STAT=irv )
IF( ALLOCATED(fTriangles) )DEALLOCATE( fTriangles,STAT=irv )

RETURN
END
!==============================================================================
! GetTotalStats
!==============================================================================
SUBROUTINE GetTotalStats( it )

USE Extract_fi
USE SCIPtool
USE GetTimes_fi
USE localpuf
USE TotalMass_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: it

INTEGER i

REAL(8) csum

csum = srfMass(it)

DO i = 1,nTimePuff
  IF( ABS( TimeSrf(it)%time%runtime - TimePuff(i)%time%runtime  ) < TimeSrf(it)%time%runtime*EPSILON(TimeSrf(it)%time%runtime) )THEN
    csum = csum + airMass(i)
    EXIT
  END IF
END DO

WRITE(6,101)TimeSrf(it)%time%runtime,csum
101 FORMAT(2ES10.3)

9999 CONTINUE

RETURN
END
!==============================================================================
! BuildField
!==============================================================================
SUBROUTINE BuildField( selection )

USE Extract_fi
USE GetTimes_fi
USE localpuf
USE TotalMass_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: selection
INTEGER i, j
LOGICAL hasChoice, hasKind, hasCat

CHARACTER(64) string, string1

Field%class = 0

string = 'SURFACE DEPOSITION'
j = LEN_TRIM(string)
DO i = 1,nClass
  string1 = ClassStr(i)%string
  CALL CUPPER( string1 )
  IF( INDEX(string1,string(1:j)) > 0 )THEN
    Field%class = i
    EXIT
  END IF
END DO

IF( Field%class == 0 )GOTO 9999

Field%choice = 0

string = TRIM(selection)
string = ADJUSTL(string)
CALL CUPPER( string )
j = LEN_TRIM(string)
DO i = 1,nchoice
  hasChoice = BTEST(ClassChoiceArray(Field%class,i)%available,HPB_AVAILABLE)
  IF( hasChoice )THEN
    string1 = ChoiceStr(i)%string
    CALL CUPPER( string1 )
    IF( INDEX(string1,string(1:j)) > 0 )THEN
      Field%choice = i
      EXIT
    END IF
  END IF
END DO

IF( Field%choice == 0 )THEN
  Field%class = 0
  GOTO 9999
END IF

Field%kind = ClassChoiceArray(Field%class,Field%choice)%ikind

hasKind = BTEST(ClassChoiceArray(Field%class,Field%choice)%kind,HPB_AVAILABLE)
IF( hasKind )THEN
  IF( ClassChoiceArray(Field%class,Field%choice)%nkind > 1 )THEN
    string = 'TOTAL'
    j = LEN_TRIM(String)
    DO i = 1,ClassChoiceArray(Field%class,Field%choice)%nkind
      string1 = KindStr(i+ClassChoiceArray(Field%class,Field%choice)%ikind-1)%string
      CALL CUPPER(string1)
      IF( INDEX(string1,String(1:j)) > 0 )THEN
        Field%kind = i + ClassChoiceArray(Field%class,Field%choice)%ikind-1
        EXIT
      END IF
    END DO
  END IF
END IF

Field%category = 1

string = 'SURFACE'
j = LEN_TRIM(String)
DO i = 1,HP_NUMCAT
  hasCat = CatClassArray(i,Field%class)%available == SCIPtrue
  IF( hasCat )THEN
    string1 = CATEGORY_STRING(i)
    CALL CUPPER( string1 )
    IF( INDEX(string1,String(1:j)) > 0 )THEN
      Field%category = i
      EXIT
    END IF
  END IF
END DO

PlotType%type = HP_MEAN
PlotType%data = 0.0

CALL setPlotClassData(.FALSE.,.FALSE.)

9999 CONTINUE

RETURN
END
!==============================================================================
! ClearTotalMass
!==============================================================================
SUBROUTINE ClearTotalMass()

USE Extract_fi
USE TotalMass_fi

IMPLICIT NONE

INTEGER irv

IF( ALLOCATED(airMass) )DEALLOCATE( airMass,STAT=irv )
IF( ALLOCATED(srfMass) )DEALLOCATE( srfMass,STAT=irv )

9999 CONTINUE

RETURN
END
!==============================================================================
! AllocateTotalMass
!==============================================================================
SUBROUTINE AllocateTotalMass( na,ns )

USE Extract_fi
USE TotalMass_fi

IMPLICIT NONE

INTEGER :: na
INTEGER :: ns

INTEGER irv

CALL ClearTotalMass()
IF( nError /= NO_ERROR )GOTO 9999

IF( na > 0 )THEN
  ALLOCATE( airMass(na),STAT=irv )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AllocateTotalMass'
    eMessage = 'Error allocating airMass array'
    WRITE(eInform,'(A,I0,A,I0)')'Request = ',na,'  :Error =',irv
    GOTO 9999
  END IF
  airMass = 0.0
END IF

IF( ns > 0 )THEN
  ALLOCATE( srfMass(ns),STAT=irv )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AllocateTotalMass'
    eMessage = 'Error allocating srfMass array'
    WRITE(eInform,'(A,I0,A,I0)')'Request = ',ns,'  :Error =',irv
    GOTO 9999
  END IF
  srfMass = 0.0
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE TotalMassByGroup()

USE Extract_fi
USE GetTimes_fi
USE TotalMass_fi
USE SCIPtool
USE scipuff_fi, ONLY: puff, typeID, npuf, dynamic, buoy_gas, dense_gas
USE struct_fd
USE   constants_fd

IMPLICIT NONE

INTEGER irv, noutx, ios, iversion
INTEGER i, j, it, nMtl, nsg, max_type
INTEGER ityp, icls, imat, ic
INTEGER ib
LOGICAL lWetParticle
REAL    rhop, dryfrac, dp

CHARACTER(1) :: q = CHAR(34) !Double quote
CHARACTER(1) :: c = CHAR(44) !Comma

TYPE column
  INTEGER :: is
  INTEGER :: ie
END TYPE column

TYPE( column ), DIMENSION(:), ALLOCATABLE :: col_mat

REAL,    DIMENSION(:),   ALLOCATABLE :: csum_grp, pbounds
INTEGER, DIMENSION(:),   ALLOCATABLE :: nsg_mat
REAL,    DIMENSION(:),   ALLOCATABLE :: rholiq
REAL,    DIMENSION(:,:), ALLOCATABLE :: dbar_grp, frac_grp

CHARACTER(1024) ofile
CHARACTER(16)   cmat, string
CHARACTER(1024) colHeader

TYPE( puff_aerosol ) :: pa
TYPE( puff_liquid  ) :: pq
TYPE( ppuffHeadT ) :: puffHead


TYPE( fileNameT )   filename
TYPE( pmaterialT )  matHead

TYPE( matParticleT ) matPart
TYPE( matLiquidT ) matLiq

INTEGER, EXTERNAL :: GetProjectPlotTimes
INTEGER, EXTERNAL :: GetProjectPlotLists

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: GetProjectPuffs, GetProjectModes

ProjectLoop: DO !Loop over projects

  nMatl = 0

  CALL GetProject()
  IF( nError/= NO_ERROR)GOTO 9999

  irv = GetProjectPlotLists( Project )
  IF( irv /= SCIPsuccess )GOTO 9999

  irv = GetProjectPlotTimes( Project )
  IF( irv /= SCIPsuccess )GOTO 9999

  irv = SCIPGetProjectVersion( callerID,Project )
  iversion = Project%version

  irv = GetProjectModes( )
  IF( irv /= SCIPsuccess )GOTO 9999

!------ File (.csv) for output-by-group

  noutx = 2
  WRITE(*,'(A)') 'Output file (w/o ext)'
  READ(*,'(A)') ofile

  ofile = TRIM(ofile)//'.csv'

  OPEN( UNIT=noutx,FILE=TRIM(ofile),STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios )
  IF( ios /= 0 )THEN
    WRITE(*,*) 'Error opening '//TRIM(ofile)
    GOTO 9999
  END IF
  WRITE(*,*) 'Output to '//TRIM(ofile)

!------ Write header
!       Project name

  WRITE(noutx,'(A)',IOSTAT=ios) q//TRIM(Project%name)//q
  IF( ios /= 0 )THEN
    WRITE(*,*) 'Error writing puff file name'
    GOTO  9999
  END IF

  lWetParticle = .FALSE.

!------ Get materials

  filename%string = TRIM(AddExtension(Project%name,'inp'))
  CALL AddPath( filename%string,TRIM(Project%path) )

  irv = SCIPCountMaterial( callerID,filename,nMtl )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Failed retrieving number of project materials' )
    GOTO 9999
  END IF

  IF( ALLOCATED(materials) )DEALLOCATE( materials,STAT=irv )
  ALLOCATE( materials(nMtl),STAT=irv )
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'TotalMassByGroup'
    eMessage = 'Error allocating material array'
    WRITE(eInform,'(A,I0,A,I0)')'Request =',nMtl,'  : Error =',irv
    GOTO 9999
  END IF

  matHead%project        = Project
  matHead%mtlHead%max    = nMtl
  matHead%mtlHead%number = 0
  matHead%control%mode   = SCIPnull
  matHead%control%mode   = IBSET(matHead%control%mode,HCB_FILE)
  matHead%control%searchName    = ' '
  matHead%control%fileExtension = 'inp'

  irv = SCIPLoadMaterialF( callerID,matHead,materials )
  IF( irv /= SCIPsuccess )THEN
    CALL toolError( 'Failed retrieving project materials' )
    GOTO 9999
  END IF

  nMatl = matHead%mtlHead%number

!------ List material names and bins

  ALLOCATE( col_mat(nMatl),nsg_mat(nMatl),STAT=irv )  !,bfac(nMatl)
  IF( irv /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'TotalMassByGroup'
    eMessage = 'Error allocating material column array'
    WRITE(eInform,'(A,I0,A,I0)')'Request =',nMtl,'  : Error =',irv
    GOTO 9999
  END IF

  max_type = 0

  DO i = 1,nMatl

    WRITE(noutx,'(A)',IOSTAT=ios) q//'Material: '//q//c//q//TRIM(materials(i)%name)//q
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'TotalMassByGroup'
      eMessage = 'Error writing material name'
      GOTO 9999
    END IF

    nsg  = 0

    IF( BTEST(materials(i)%type,HMB_PARTICLE) )THEN
      matPart = TRANSFER(materials(i)%matData,matPart)
      nsg = matPart%nSizeBins
      ALLOCATE( pbounds(nsg+1),STAT=irv )
      pbounds = matPart%binBounds(1:nsg+1)

    ELSE IF( BTEST(materials(i)%type,HMB_WETPARTICLE) )THEN
      matPart = TRANSFER(materials(i)%matData,matPart)
      nsg = matPart%nSizeBins
      ALLOCATE( pbounds(nsg+1),STAT=irv )
      pbounds = matPart%binBounds(1:nsg+1)

      lWetParticle = .TRUE.

    ELSE IF( BTEST(materials(i)%type,HMB_LIQUID) )THEN
      matLiq = TRANSFER(materials(i)%matData,matLiq)
      nsg = matLiq%nSizeBins
      ALLOCATE( pbounds(nsg+1),STAT=irv )
      pbounds = matLiq%binBounds(1:nsg+1)

    END IF

    col_mat(i)%is = max_type + 1

    nsg_mat(i) = nsg

    IF( nsg > 0 )THEN

      WRITE(noutx,'(A)',ADVANCE='NO',IOSTAT=ios) q//'Bins: '//q

      DO j = 1,nsg+1
        WRITE(noutx,FMT='(A,ES14.7)',ADVANCE='NO',IOSTAT=ios) c,pbounds(j)
        IF( ios /= 0 )THEN
          WRITE(*,*) 'Error writing material bins'
          GOTO  9999
        END IF
      END DO

      DEALLOCATE( pbounds,STAT=irv )

      IF( BTEST(materials(i)%type,HMB_LIQUID) )THEN
        max_type = max_type + 1 !Vapor
        IF( iversion >= 2400 )max_type = max_type + 1 !Aerosol
      END IF

      max_type = max_type + nsg

      WRITE(noutx,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record

    ELSE

      max_type = max_type + 1

    END IF

    col_mat(i)%ie = max_type

  END DO

  WRITE(noutx,FMT='(A)',IOSTAT=ios) q//q

  ALLOCATE( csum_grp(max_type),STAT=irv )
  IF( lWetParticle )THEN
    ALLOCATE( dbar_grp(max_type,nTimePuff),frac_grp(max_type,nTimePuff),rholiq(nMatl),STAT=irv )
  END IF

!------ Set output columns names

  WRITE(noutx,'(A)',ADVANCE='NO',IOSTAT=ios) q//'Time'//q//c
  IF( lWetParticle )THEN
    colHeader = q//'Time'//q//c
  END IF

  DO i = 1,nMatl

    cmat = TRIM(materials(i)%name)

    nsg = nsg_mat(i)

    IF( nsg > 0 )THEN

      IF( BTEST(materials(i)%type,HMB_LIQUID) )THEN
        WRITE(noutx,FMT='(A)',ADVANCE='NO',IOSTAT=ios) q//TRIM(cmat)//':Vapor'//q//c
        IF( lWetParticle )colHeader = TRIM(colHeader)//q//TRIM(cmat)//':Vapor'//q//c
      END IF

      DO j = 1,nsg
        IF( j < 10 )THEN
          WRITE(string,'(I1)',IOSTAT=ios) j
        ELSE IF( j < 100 )THEN
          WRITE(string,'(I2)',IOSTAT=ios) j
        ELSE
          WRITE(string,'(I3)',IOSTAT=ios) j
        END IF
        WRITE(noutx,FMT='(A)',ADVANCE='NO',IOSTAT=ios) q//TRIM(cmat)//':'//TRIM(string)//q//c
        IF( ios /= 0 )THEN
          WRITE(*,*) 'Error writing column name'
          GOTO  9999
        END IF
        IF( lWetParticle )colHeader = TRIM(colHeader)//q//TRIM(cmat)//':'//TRIM(string)//q//c
      END DO

      IF( BTEST(materials(i)%type,HMB_LIQUID) .AND. iversion >= 2400 )THEN
        WRITE(noutx,FMT='(A)',ADVANCE='NO',IOSTAT=ios) q//TRIM(cmat)//':Aerosol'//q//c
        IF( lWetParticle )colHeader = TRIM(colHeader)//q//TRIM(cmat)//':Aerosol'//q//c
      END IF

    ELSE

      WRITE(noutx,'(A)',ADVANCE='NO',IOSTAT=ios) q//TRIM(cmat)//q//c
      IF( ios /= 0 )THEN
        WRITE(*,*) 'Error writing column name'
        GOTO  9999
      END IF
      IF( lWetParticle )colHeader = TRIM(colHeader)//q//TRIM(cmat)//q//c

    END IF

    IF( lWetParticle )THEN
      IF( LEN_TRIM(materials(i)%file) > 0 )THEN
        ofile = TRIM(materials(i)%file)
        CALL AddPath( ofile,materials(i)%path )
        CALL GetLiqDensityMtlFile( noutx+1,ofile,rholiq(i) )
        IF( rholiq(i) <= 0. )THEN
          WRITE(*,*) 'Error reading slurry liquid density'
          WRITE(*,*) 'File=',TRIM(ofile)
          GOTO  9999
        END IF
      ELSE
        rholiq(i) = RHO_WATER
      END IF
    END IF

  END DO

  WRITE(noutx,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record

  puffHead%project = Project

  dbar_grp = 0.; frac_grp = 0.

!------ Loop over output times

  PuffTimes: DO it = 1,nTimePuff

    irv = GetProjectPuffs( callerID,puffHead,it,.TRUE.,.FALSE.,0 )
    IF( irv /= SCIPsuccess )GOTO 9999
    dynamic   = (puffHead%puff%dynamic == SCIPtrue)
    dense_gas = (puffHead%puff%dense   == SCIPtrue)
    buoy_gas  = (puffHead%puff%buoy    == SCIPtrue)
    CALL set_auxskp()

    csum_grp = 0.

!------ Sum by puff type

	  PuffLoop: DO i = 1,npuf
      ityp = puff(i)%ityp
      icls = typeID(ityp)%icls
      imat = typeID(ityp)%imat

      ic = col_mat(imat)%is + typeID(ityp)%igrp - 1

      IF( ic > col_mat(imat)%ie )THEN
        WRITE(*,*) 'Invalid type for material '//TRIM(materials(imat)%name)
        GOTO 9999
      END IF

      IF( IsAerosol(icls) )THEN

        CALL get_aerosol( puff(i),pa )
	      csum_grp(ic)  = csum_grp(ic)  + puff(i)%c * pa%fl

        ic = col_mat(imat)%is
	      csum_grp(ic)  = csum_grp(ic)  + puff(i)%c * (1.0-pa%fl)

      ELSE

	      csum_grp(ic)  = csum_grp(ic)  + puff(i)%c
      IF( lWetParticle )THEN
        IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN
          matPart = TRANSFER(materials(imat)%matData,matPart)
          ib = typeID(ityp)%igrp
          dp = matPart%binSize(ib)
        END IF
        IF( IsWetParticle(icls) )THEN
          CALL get_liquid( puff(i),pq )
          rhop = matPart%density
          dryfrac = rhop*dp**3/(rhop*dp**3 + rholiq(imat)*(pq%d**3-dp**3))
          dbar_grp(ic,it) = dbar_grp(ic,it)  + puff(i)%c*pq%D
          frac_grp(ic,it) = frac_grp(ic,it)  + puff(i)%c*dryfrac
        ELSE IF( IsParticle(icls) )THEN
          dryfrac = 1.
          dbar_grp(ic,it) = dbar_grp(ic,it)  + puff(i)%c*dp
          frac_grp(ic,it) = frac_grp(ic,it)  + puff(i)%c*dryfrac
        ELSE IF( IsLiquid(icls) )THEN
          CALL get_liquid( puff(i),pq )
          dbar_grp(ic,it) = dbar_grp(ic,it)  + puff(i)%c*pq%D
        END IF
      END IF

      END IF

    END DO PuffLoop

!------ Output for this time

  WRITE(noutx,FMT='(ES12.5,A)',ADVANCE='NO',IOSTAT=ios) TimePuff(it)%time%runtime
  DO i = 1,max_type
    WRITE(noutx,FMT='(A,ES10.3)',ADVANCE='NO',IOSTAT=ios) c,csum_grp(i)
    IF( ios /= 0 )THEN
      WRITE(*,*) 'Error writing bin mass ',i
      GOTO  9999
    END IF
  END DO

  WRITE(noutx,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record

!------ Compute mean droplet diameter and dry fraction
!       N.B Write out after time loop

  IF( lWetParticle )THEN
    DO ic = 1,max_type
      IF( csum_grp(ic) > 0. )THEN
        dbar_grp(ic,it) = dbar_grp(ic,it) / csum_grp(ic)
        frac_grp(ic,it) = frac_grp(ic,it) / csum_grp(ic)
      ELSE
        dbar_grp(ic,it) = 0.
        frac_grp(ic,it) = 0.
      END IF
    END DO
  END IF

  END DO PuffTimes

  IF( lWetParticle )THEN

    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) q//q
    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) q//'Droplet Diameter'//q
    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) TRIM(colHeader)
    DO it = 1,nTimePuff
      WRITE(noutx,FMT='(ES12.5,A)',ADVANCE='NO',IOSTAT=ios) TimePuff(it)%time%runtime
      DO i = 1,max_type
        WRITE(noutx,FMT='(A,ES10.3)',ADVANCE='NO',IOSTAT=ios) c,dbar_grp(i,it)
        IF( ios /= 0 )THEN
          WRITE(*,*) 'Error writing droplet diameter ',i
          GOTO  9999
        END IF
      END DO
      WRITE(noutx,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record
    END DO


    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) q//q
    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) q//'Dry Fraction'//q
    WRITE(noutx,'(A)',ADVANCE='YES',IOSTAT=ios) TRIM(colHeader)
    DO it = 1,nTimePuff
      WRITE(noutx,FMT='(ES12.5,A)',ADVANCE='NO',IOSTAT=ios) TimePuff(it)%time%runtime
      DO i = 1,max_type
        WRITE(noutx,FMT='(A,ES10.3)',ADVANCE='NO',IOSTAT=ios) c,frac_grp(i,it)
        IF( ios /= 0 )THEN
          WRITE(*,*) 'Error writing dry fraction',i
          GOTO  9999
        END IF
      END DO
      WRITE(noutx,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record
    END DO

  END IF

  nMatl   = 0
  PrjName = ' '
  CALL ClearMemory()

  IF( ALLOCATED(materials) )DEALLOCATE( materials,STAT=irv )
  IF( ALLOCATED(col_mat)   )DEALLOCATE( col_mat,  STAT=irv )
  IF( ALLOCATED(pbounds)   )DEALLOCATE( pbounds,  STAT=irv )
  IF( ALLOCATED(csum_grp)  )DEALLOCATE( csum_grp, STAT=irv )
  IF( ALLOCATED(nsg_mat)   )DEALLOCATE( nsg_mat,  STAT=irv )
  IF( ALLOCATED(dbar_grp)  )DEALLOCATE( dbar_grp, STAT=irv )
  IF( ALLOCATED(frac_grp)  )DEALLOCATE( frac_grp, STAT=irv )

END DO ProjectLoop

9999 CONTINUE

IF( nError /= NO_ERROR )CALL showError()

IF( ALLOCATED(materials) )DEALLOCATE( materials,STAT=irv )
IF( ALLOCATED(col_mat)   )DEALLOCATE( col_mat,  STAT=irv )
IF( ALLOCATED(pbounds)   )DEALLOCATE( pbounds,  STAT=irv )
IF( ALLOCATED(csum_grp)  )DEALLOCATE( csum_grp, STAT=irv )
IF( ALLOCATED(nsg_mat)   )DEALLOCATE( nsg_mat,  STAT=irv )
IF( ALLOCATED(dbar_grp)  )DEALLOCATE( dbar_grp, STAT=irv )
IF( ALLOCATED(frac_grp)  )DEALLOCATE( frac_grp, STAT=irv )
IF( ALLOCATED(rholiq)    )DEALLOCATE( rholiq,   STAT=irv )

CALL ClearLocalPuffs()
CALL ClearMemory()

RETURN
END

!==============================================================================

SUBROUTINE GetLiqDensityMtlFile( lun,ofile,rholiq )

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: lun
CHARACTER(*), INTENT( IN  ) :: ofile
REAL,         INTENT( OUT ) :: rholiq

INTEGER ios, i, nch

CHARACTER(1024) string

OPEN(UNIT=lun,FILE=TRIM(ofile),ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )GOTO 9999

DO
  READ(lun,'(A)',IOSTAT=ios) string
  IF( ios /= 0 )GOTO 9999
  CALL cupper(string)
  IF( INDEX(string,'LIQUID_DENSITY') /= 0 )THEN
    i = INDEX(string,'=')
    IF( i == 0 )GOTO 9999
    nch = LEN_TRIM(string)
    READ(string(i+1:nch),*,IOSTAT=ios) rholiq
    IF( ios /= 0 )GOTO 9999
    EXIT
  END IF
END DO

9999 CONTINUE

IF( ios /= 0 )rholiq = -1.

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END
