!======================================================================
SUBROUTINE GetAllFieldValues( grdI,grdA,nAreas,maxRec,nRec,lDep,isMult,lSkip,lLL)

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE PtrGrdStrItf
USE extract_fi
USE multcomp_fd
USE scipuff_fi , ONLY: ntypm, material,mat_mc
USE plotlist_fi
USE sciDOSpostTools, ONLY:srfFldVal,xRec,yRec,lat,lon,debug

IMPLICIT NONE

INTEGER,                                INTENT( INOUT ) :: grdI, grdA
INTEGER,                                INTENT( IN    ) :: nAreas
INTEGER,                                INTENT( IN    ) :: maxRec
INTEGER, DIMENSION(nAreas),             INTENT( IN    ) :: nRec

LOGICAL,                                INTENT( IN    ) :: lDep,isMULT,lSkip,lLL

TYPE( SAGgrid_str ), POINTER :: grd, agrd

INTEGER irv, astat, ios
INTEGER i, j, iKind, iMult, iSp
INTEGER nFld, nVar

INTEGER, DIMENSION(:),ALLOCATABLE :: ifld
REAL,    DIMENSION(:),ALLOCATABLE :: fpnt,apnt

REAL    xpnt, ypnt
CHARACTER(1), DIMENSION(1) :: VarNames  !Var names to read

INTEGER, EXTERNAL :: SAG_ReadHeaderID, SAG_ReadGridID, SAG_ReadMCDataID
INTEGER, EXTERNAL :: SAG_BottomValueID, SAG_ReadNextBreakID
INTEGER, EXTERNAL :: output_groups

CALL init_error()

if (debug .and. .false.) then
   do i = 1,nAreas
      print*
      print*,"Receptor set ",i
      print*,"Min,Max X: ",minval(xRec(i,1:nRec(i))), maxval(xRec(i,1:nRec(i)))
      print*,"Min,Max Y: ",minval(yRec(i,1:nRec(i))), maxval(yRec(i,1:nRec(i)))
      print*,"Min,Max lat: ",minval(lat(i,1:nRec(i))), maxval(lat(i,1:nRec(i)))
      print*,"Min,Max lon: ",minval(lon(i,1:nRec(i))), maxval(lon(i,1:nRec(i)))
      do j = 1,nRec(i)
         write(*,'(a,2i3,4f12.6)') "iRset,iRec, X,Y,lat,lon = ",i,j,     &
              xRec(i,j),yRec(i,j),lat(i,j),lon(i,j)
      end do
   end do
end if

IF ( lSkip )THEN
  nVar = 0
  varNames(1) = ''
  grd => SAG_PtrGrdStr( grdI )
  irv = SAG_ReadNextBreakID( grdI,nVar,varNames )
  if (debug) WRITE(24,*)"Skip reading data from ",TRIM(grd%file)," for time ", &
       grd%time
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'InitGrid'
    eMessage = 'Error reading next break for SAG file'
    CALL ReportFileName( eInform,'File=',TRIM(grd%file) )
    GOTO 9999
  END IF
  IF( .NOT.lDep .AND. grdA > 0 )THEN
    agrd => SAG_PtrGrdStr( grdA )
    irv = SAG_ReadNextBreakID( grdA,nVar,varNames )
    if (debug) WRITE(24,*)"Skip reading data from ",TRIM(agrd%file)," for time ",&
         agrd%time
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error reading next break for SAG file'
      CALL ReportFileName( eInform,'File=',TRIM(agrd%file) )
      GOTO 9999
    END IF
  END IF
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )   ! Associate "local" grid structure pointer
irv = SAG_ReadHeaderID( grdI )
if (debug) WRITE(24,*)"Reading data from ",TRIM(grd%file)," for time ",grd%time
IF( irv == SAG_OK .AND. .NOT.lDep .AND. grdA > 0 )THEN
  agrd => SAG_PtrGrdStr( grdA )
  irv = SAG_ReadHeaderID( grdA )
  if (debug) WRITE(24,*)"Reading data from ",TRIM(agrd%file)," for time ", &
       agrd%time
END IF
IF( irv == SAG_EOF )GOTO 9999
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'Error reading SAG file'
  CALL ReportFileName( eInform,'File=',grd%file )
  GOTO 9999
END IF

!------ Allocate grid and data fields for ncells

grd%mxgrd = grd%ncells
grd%mxfld = grd%nvart  ! All variables
grd%naux  = 0          ! turn off any auxiliary fields
IF( .NOT.lDep .AND. grdA > 0 )THEN
  agrd%mxgrd = agrd%ncells
  agrd%mxfld = agrd%nvart  ! All variables
  agrd%naux  = 0          ! turn off any auxiliary fields
END IF

ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
IF( ios == 0 .AND. .NOT.lDep .AND. grdA > 0 )&
  ALLOCATE( agrd%ipgrd(agrd%mxgrd),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )
IF( ios == 0 .AND. .NOT.lDep .AND. grdA > 0 )&
  ALLOCATE( agrd%ipdat(agrd%mxgrd*agrd%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

!------ Read grid

irv = SAG_ReadGridID( grdI )
IF( irv == SAG_OK .AND. .NOT.lDep .AND. grdA > 0 )&
  irv = SAG_ReadGridID( grdA )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error reading surface grid'
  GOTO 9999
END IF

!------ Read specified fields

irv = SAG_ReadMCDataID( grdI ) ! All fields
IF( irv == SAG_OK .AND. .NOT.lDep .AND. grdA > 0 )&
  irv = SAG_ReadMCDataID( grdA )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error reading surface grid'
  GOTO 9999
END IF

nfld = grd%mxfld

ALLOCATE(ifld(nfld),fpnt(nfld),STAT=astat)
IF( astat == 0 .AND. .NOT.lDep .AND. grdA > 0 )&
  ALLOCATE(apnt(nfld),STAT=astat)
IF( astat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error allocating fpnt array'
  GOTO 9999
END IF

DO i = 1,nfld
  ifld(i) = i
END DO

irv = SAG_BottomValueID( grdI,nfld,ifld )
IF( irv == SAG_OK .AND. .NOT.lDep .AND. grdA > 0 )&
  irv = SAG_BottomValueID( grdA,nfld,ifld )

DO i = 1,nAreas
  DO j = 1,nRec(i)

    if (lLL) then      ! we need to ask for point in Project coordinates
       xpnt = lon(i,j)
       ypnt = lat(i,j)
    else
       xpnt = xRec(i,j)
       ypnt = yRec(i,j)
    end if

    fpnt = 0.

    CALL GetFieldMCValue( grdI,Field,PlotType,xpnt,ypnt,fpnt )
    IF( nError /= NO_ERROR )THEN
      if (debug) print*,"GetFieldMCValue grdI error at ",xpnt,ypnt
      GOTO 9999
    END IF

    IF( .NOT.lDep .AND. grdA > 0 )THEN
      apnt = 0.
      CALL GetFieldMCValue( grdA,Field,PlotType,xpnt,ypnt,apnt )
      IF( nError /= NO_ERROR )THEN
        if (debug) print*,"GetFieldMCValue grdA error at ",xpnt,ypnt
        GOTO 9999
      END IF
    END IF

    IF( isMult )THEN

      IF( lDep )THEN

        iMult = grd%nvart - 2*nKind

        DO iKind = 1,nKind
          iSp = 2*iKind - 1 + iMult
          srfFldVal(i,j,iKind,1) = fpnt(iSp)     ! Dry
          srfFldVal(i,j,iKind,2) = fpnt(iSp+1)   ! Wet
        END DO

      ELSE

        iMult = grd%nvart - nKind

        IF( grdA > 0 )THEN
          DO iKind = 1,nKind
            iSp = iMult + iKind
            srfFldVal(i,j,iKind,2) = apnt(iSp)     ! Ambient Conc
          END DO
          if (debug .and. .false.) print*,"iRset,iRec,Temp = ",i,j,apnt(1)
          srfFldVal(i,j,nKind+1,2) = apnt(1) ! Temp(K)
          srfFldVal(i,j,nKind+2,2) = apnt(2) ! Press(Atm)
          srfFldVal(i,j,nKind+3,2) = apnt(3) ! Humidity(g/g)
        END IF

        DO iKind = 1,nKind
          iSp = iMult + iKind
          srfFldVal(i,j,iKind,1) = fpnt(iSp)     ! Total Conc
        END DO

      END IF

    ELSE

      srfFldVal(i,j,1,1) = fpnt(1)

    END IF

  END DO

END DO

9999 CONTINUE

IF( ALLOCATED(ifld) )DEALLOCATE(ifld,fpnt,STAT=astat)

IF( ASSOCIATED(grd%ipgrd) )THEN
  DEALLOCATE(grd%ipgrd,STAT=astat)
  NULLIFY(grd%ipgrd)
END IF

IF( ASSOCIATED(grd%ipdat) )THEN
  DEALLOCATE(grd%ipdat,STAT=astat)
  NULLIFY(grd%ipdat)
END IF

grd%mxgrd = 0

IF( .NOT.lDep .AND. grdA > 0 )THEN
  IF( ALLOCATED(apnt) )DEALLOCATE(apnt,STAT=astat)
  IF( ASSOCIATED(agrd%ipgrd) )THEN
    DEALLOCATE(agrd%ipgrd,STAT=astat)
    NULLIFY(agrd%ipgrd)
  END IF
  IF( ASSOCIATED(agrd%ipdat) )THEN
    DEALLOCATE(agrd%ipdat,STAT=astat)
    NULLIFY(agrd%ipdat)
  END IF
  agrd%mxgrd = 0
END IF

RETURN
END
