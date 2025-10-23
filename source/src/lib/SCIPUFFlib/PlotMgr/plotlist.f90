SUBROUTINE BuildPlotList()

!------ Build list of plot choices
!
!     Category      Class         Choice          Kind
!     =========    =======       ==========      ======
!     Horiz Slice   Conc          Material       Subgroups
!     Vert  Slice   Met           Terrain        --------
!     V Int Slice   Dep           ---------      Integrated Dose
!     Surface       Dos           NFAC Dose      Dose Rate
!     -----------   --------      Total NWPN     Casualties, Open
!     Table         SCIP Rad      Prompt Cas     Fatalities, Open
!                   NWPN Rad      Total Cas      Casualties, Prot
!                   NWPN Cas                     Fatalities, Prot
!                   Exp Time                     FX Kind (Performance)
!                   FX class (CODA)

USE scipuff_fi
USE plotlist_fi
USE field_fd

IMPLICIT NONE

INTEGER ios

LOGICAL, EXTERNAL :: DepPlotAvail, DosPlotAvail

!------ Deallocate arrays

CALL ClearPlotLists()

!------ Set time indicators

IF( MemoryField )THEN
  hasPlotTimes(HP_NOTIME  +1) = .TRUE.
  hasPlotTimes(HP_PUFFTIME+1) = npuf > 0
  hasPlotTimes(HP_SRFTIME +1) = DosPlotAvail() .OR. DepPlotAvail()
  hasPlotTimes(HP_METTIME +1) = .FALSE.
ELSE
  CALL SetPlotTimes()
  IF( nError /= NO_ERROR )GOTO 9999
  hasPlotTimes(HP_NOTIME  +1) = .TRUE.
  hasPlotTimes(HP_PUFFTIME+1) = nPuffTime > 0
  hasPlotTimes(HP_SRFTIME +1) = nSrfTime  > 0
  hasPlotTimes(HP_METTIME +1) = nMetTime  > 0
END IF

!------ Get counts of plot classes, variables and kinds

CALL CountPlotClasses()
IF( nError /= NO_ERROR )GOTO 9999

!------ Allocate space for plot strings and combinations tables

ALLOCATE( ClassString(nPclassT),STAT=ios )
IF( ios == 0 )ALLOCATE( ChoiceString(nPchoiceT),STAT=ios )
IF( ios == 0 )ALLOCATE( KindString(nPkindT)    ,STAT=ios )

IF( ios == 0 )ALLOCATE( CatClassComb   (nPcat   ,nPclassT ),STAT=ios )
IF( ios == 0 )ALLOCATE( ClassChoiceComb(nPclassT,nPchoiceT),STAT=ios )

IF( ios == 0 )ALLOCATE( ClassID(nPclassT),STAT=ios )
IF( ios == 0 )ALLOCATE( ClassInterp(nPclassT),STAT=ios )
IF( ios == 0 )ALLOCATE( ClassOrder(nPclassT),STAT=ios )
IF( ios == 0 )ALLOCATE( ChoiceOrder(nPchoiceT),STAT=ios )
IF( ios == 0 )ALLOCATE( ChoiceMCID(nPchoiceMC),is_kindMC(4,nPchoiceMC), &
                                                n_kindMC(4,nPchoiceMC),STAT=ios )

IF( ios == 0 )ALLOCATE( is_kind(nPchoice),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'BuildPlotList'
  eMessage = 'Error allocating plot list'
  GOTO 9999
END IF

!------ Order Class list - ClassOrder(iclass) -> index into class lists

CALL OrderPlotClasses()

!------ Fill lists

CALL FillPlotLists()

!------ Reorder Class list - ClassOrder(iclass) -> original class order

CALL ReorderPlotClasses()

!------ Set Choice order

CALL SetChoiceOrder()

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CountPlotClasses()

!------ Count plot choices

USE scipuff_fi
USE met_fi
USE plotlist_fi
USE field_fd

IMPLICIT NONE

INTEGER i, nsg, nAveMax
INTEGER n

INTEGER, EXTERNAL :: output_groups, AvePlotNum
LOGICAL, EXTERNAL :: HasPuffs, DepPlotAvail, DosPlotAvail, AvePlotAvail
LOGICAL, EXTERNAL :: IsEvap

!------ Plot categories

nPcat = HP_NUMCAT

!------ Plot classes

nPclass = 0

IF( DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )nPclass = nPclass + 1
IF( DepPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )nPclass = nPclass + 1

!----- AVE file for AEGL plots

IF( AvePlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )THEN
  nAveMax = AvePlotNum()
  nPclass = nPclass + nAveMax
END IF

IF( HasPuffs() )nPclass = nPclass + 2 !(Concentration and Integrated Concentration)

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() )nPclass = nPclass + 1                  !(Source Estimation (Inst))
  IF( DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) ) &
    nPclass = nPclass + 1                                !(Source Estimation (Cont))
END IF

IF( lter )nPclass = nPclass + 1

!------ Plot choices

nPchoice = ntypm   !One for each material

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() .OR. (DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1)) )THEN
    nPchoice = nPchoice + HP_NUMADJ
  END IF
END IF

nPchoiceMC = 0

DO i = 1,ntypm
  IF( material(i)%mcID > 0 )THEN
    n = 0
    CALL CountKindMC( i,n ) !check if Degrade/Urban which may have no choices
    IF( n > 0 )THEN
      nPchoice   = nPchoice   + 1
      nPchoiceMC = nPchoiceMC + 1
    END IF
  END IF
END DO

IF( lter )THEN
  DO i = 1,nBaseMet
    IF( MetGrid(i)%lter )nPchoice = nPchoice + 1
  END DO
END IF

!------ Plot kind

nPkind = 0

DO i = 1,ntypm

  nsg = output_groups( material(i) )
  IF( nsg > 1 )nPkind = nPkind + nsg + 1 !(Groups + Total)
  IF( IsEvap(material(i)%icls) )nPkind = nPkind + 1 !(Total w/o 2nd evap)

  IF( material(i)%mcID > 0 )THEN
    CALL CountKindMC( i,nPkind )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

END DO

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  nPkind = nPkind + 2          ! Source Estimation (Fast/Full search)
END IF

nPclassEff = 0
nPchoiceEff = 0
nPkindEff = 0

!------ Set totals

nPclassT  = nPclass  + nPclassEff
nPchoiceT = nPchoice + nPchoiceEff
nPkindT   = nPkind   + nPkindEff

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE CountKindMC( imat,n )

!------ Count kinds for multicomponent material - conc, dep, and dos

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: imat
INTEGER, INTENT( INOUT ) :: n

INTEGER mcID, nkind

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL CountChemKindMC( mat_mc%ID(mcID),nkind )

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'CountKindMC'
    eMessage = 'Multicomponent error'
    WRITE(eMessage,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999

END SELECT

n = n + nkind

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE OrderPlotClasses()

!------ Order plot Class list - sets the order Class list strings are to appear to user

USE scipuff_fi
USE plotlist_fi

IMPLICIT NONE

INTEGER i

!------ Set Default order - order as set

DO i = 1,nPclassT
  ClassOrder(i) = i
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ReorderPlotClasses()

!------ Reorder plot Class list - Sets the original order of class

USE scipuff_fi
USE plotlist_fi

IMPLICIT NONE

INTEGER i,ios
INTEGER, ALLOCATABLE, DIMENSION(:) :: tmpOrder

!------ Set Default order - order as set

ALLOCATE( tmpOrder(nPclassT),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReorderPlotClasses'
  eMessage = 'Error allocating temporary work space'
  GOTO 9999
END IF

tmpOrder = ClassOrder

DO i = 1,nPclassT
  ClassOrder(tmpOrder(i)) = i
END DO

IF( ALLOCATED(tmpOrder) )DEALLOCATE( tmpOrder )

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetChoiceOrder()

!------ Reorder plot Class list - Sets the original order of class

USE plotlist_fi

IMPLICIT NONE

INTEGER i
INTEGER j
INTEGER imat

!------ Set default order initially

DO i = 1,nPchoiceT
  ChoiceOrder(i) = i
END DO

!------ Move multi-component choices next to materials

DO i = 1,nPchoiceMC
  imat = ChoiceMCID(nPchoiceMC-i+1)
  DO j = nPchoice,imat+1,-1
    ChoiceOrder(j) = ChoiceOrder(j-1)
  END DO
  ChoiceOrder(imat+1) = nPchoice - i + 1
END DO

RETURN
END

!==============================================================================

SUBROUTINE FillPlotLists()

!------ Fill list of plot choices

USE scipuff_fi
USE met_fi
USE plotlist_fi
USE SCIPResults_fd
USE field_fd

IMPLICIT NONE

INTEGER iclass, ikind, i, j, ios, nsg, nl, nu, cID, type_flag, nter
INTEGER nAveMax, alloc_stat, irv
INTEGER ii
INTEGER nbase
INTEGER nn

CHARACTER(16) lower, upper
CHARACTER(8)  ext

CHARACTER(64), DIMENSION(:), ALLOCATABLE :: AveMaxNames

REAL, ALLOCATABLE, DIMENSION(:) :: pbounds

INTEGER, EXTERNAL :: output_groups, AvePlotNum, AvePlotNames
LOGICAL, EXTERNAL :: HasPuffs, DepPlotAvail, DosPlotAvail, AvePlotAvail
LOGICAL, EXTERNAL :: IsLiquid,IsEvap

!------ Initialize classInterp

DO iclass = 1,nPclassT
  ClassInterp(iclass) = SCIPoff
END DO

!------ Class strings

iclass = 0

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() )THEN
    iclass = iclass + 1
    cID = ClassOrder(iclass)
    ClassString(cID) = 'Source Estimation (Inst)'
    ClassID(cID)     = HP_ADJOINT
  END IF
  IF( DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )THEN
    iclass = iclass + 1
    cID = ClassOrder(iclass)
    ClassString(cID) = 'Source Estimation (Cont)'
    ClassID(cID)     = HP_ADJ_SFC
  END IF
END IF

IF( DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )THEN
  iclass = iclass + 1
  cID = ClassOrder(iclass)
  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ClassString(cID) = 'Adjoint Surface Dosage'
  ELSE
    ClassString(cID) = 'Surface Dosage'
  END IF
  ClassID(cID)     = HP_DOS
  srfDosClass      = cID
END IF

IF( DepPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )THEN
  iclass = iclass + 1
  cID = ClassOrder(iclass)
  ClassString(cID) = 'Surface Deposition'
  ClassID(cID)     = HP_DEP
  srfDepClass      = cID
END IF

!----- AVE file for AEGL plots

IF( AvePlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1) )THEN
  nAveMax = AvePlotNum()
  ALLOCATE( AveMaxNames(nAveMax),STAT=alloc_stat )
  irv = AvePlotNames( nAveMax,AveMaxNames )
  DO i = 1,nAveMax
    iclass = iclass + 1
    cID = ClassOrder(iclass)
      ClassString(cID) = TRIM(AveMaxNames(i))//' Conc'
      ClassID(cID)     = HP_CMAX
  END DO
END IF

IF( HasPuffs() )THEN
  iclass = iclass + 1
  cID = ClassOrder(iclass)
  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ClassString(cID) = 'Adjoint Concentration'
  ELSE
    ClassString(cID) = 'Concentration'
  END IF
  ClassID(cID)     = HP_CONC
  iclass = iclass + 1
  cID = ClassOrder(iclass)
  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ClassString(cID) = 'Integrated Adjoint Conc'
  ELSE
    ClassString(cID) = 'Integrated Concentration'
  END IF
  ClassID(cID)     = HP_INTCONC
END IF

IF( lter )THEN
  iclass = iclass + 1
  cID = ClassOrder(iclass)
  ClassString(cID) = 'Met/Terrain'
  ClassID(cID)     = HP_MET
  ClassInterp(cID) = SCIPon
END IF

!------ Choice strings  -  1 to ntypm are material names
!                          ntypm+1 is terrain (if present)

j = 0
DO i = 1,ntypm
  j = j + 1
  ChoiceString(j) = TRIM(material(i)%cmat)
END DO

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() .OR. (DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1)) )THEN
    DO i = 1,HP_NUMADJ   !Clear all strings first
      ChoiceString(j+i) = ' '
    END DO
    ChoiceString(j+ADJ_MAXLOC) = 'Max Location Estimate'
    ChoiceString(j+ADJ_LOC   ) = 'Location Estimate'
    ChoiceString(j+ADJ_MASS  ) = 'Mass Estimate'
    j = j + HP_NUMADJ
  END IF
END IF

nter = 0
IF( lter )THEN
  IF( numMet == 1 )THEN
    j = j + 1
    ChoiceString(j) = 'Terrain'
    nter = 1
  ELSE
    DO i = 1,nBaseMet
      IF( MetGrid(i)%lter )THEN
        nter = nter + 1
        j    = j    + 1
        WRITE(ext,'(I8)',IOSTAT=ios) i
        ChoiceString(j) = 'Terrain, Grid '//TRIM(ADJUSTL(ext))
      END IF
    END DO
  END IF
END IF

!----- Add multi-component Choices at end

ii = 0
DO i = 1,ntypm
  IF( material(i)%mcID > 0 )THEN
    nn = 0
    CALL CountKindMC( i,nn ) !check if Degrade/Urban which may have no choices
    IF( nn > 0 )THEN
      j  = j + 1
      ii = ii + 1
      ChoiceString(j) = TRIM(material(i)%cmat)//':Components'
      ChoiceMCID(ii)  = i
    END IF
  END IF
END DO

!------ Kind strings - first nPkind are material subgroups and total

ikind = 0

DO i = 1,ntypm

  nsg = output_groups( material(i) )
  IF( nsg > 1 )THEN
    is_kind(i) = ikind + 1
    IF( IsLiquid(material(i)%icls) )THEN
      ikind = ikind + 1
      KindString(ikind) = 'Vapor'
      ikind = ikind + 1
      KindString(ikind) = 'Liquid'
      ikind = ikind + 1
      KindString(ikind) = 'Total'
      IF( IsEvap(material(i)%icls) )THEN
        ikind = ikind + 1
        KindString(ikind) = 'Total w/o secondary evap.'
      END IF
    ELSE
      ALLOCATE( pbounds(nsg+1),STAT=ios )
      IF( ios /= 0 )THEN
        nError = UK_ERROR
        eRoutine = 'FillPlotList'
        eMessage = 'Error allocating plot list'
        GOTO 9999
      END IF
      CALL get_bounds( material(i),nsg,pbounds )
      CALL c_format( 1.E6*pbounds(1),nl,lower )
      DO j = 1,nsg
        ikind = ikind + 1
        CALL c_format( 1.E6*pbounds(j+1),nu,upper )
        KindString(ikind) = lower(1:nl)//'-'//upper(1:nu)//' microns'
        lower = upper(1:nu)
        nl = nu
      END DO
      DEALLOCATE( pbounds )
      ikind = ikind + 1
      KindString(ikind) = 'Total'
    END IF
  ELSE
    is_kind(i) = 0
  END IF

END DO

DO i = 1,nPchoiceMC
  CALL AddKindMC( i,ikind )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() .OR. (DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1)) )THEN
    ikind = ikind + 1
    KindString(ikind) = 'Fast Search'
    adj_kind = ikind
    ikind = ikind + 1
    KindString(ikind) = 'Full Search'
  END IF
END IF

!------ Nullify tables


DO i = 1,nPcat
  DO j = 1,nPclassT
    CatClassComb(i,j)%available = SCIPfalse
    CatClassComb(i,j)%type      = SCIPfalse
  END DO
END DO

DO i = 1,nPclassT
  DO j = 1,nPchoiceT
    ClassChoiceComb(i,j)%available = 0
    ClassChoiceComb(i,j)%kind      = SCIPfalse
    ClassChoiceComb(i,j)%ikind     = 0
    ClassChoiceComb(i,j)%nkind     = 0
    ClassChoiceComb(i,j)%itime     = 0
    ClassChoiceComb(i,j)%usertime  = SCIPfalse
    ClassChoiceComb(i,j)%units     = ' '
  END DO
END DO

type_flag = SCIPtrue

!------ Fill tables for available combinations

DO i = 1,nPclass

  cID = ClassOrder(i)

  SELECT CASE( ClassID(cID) )

    CASE( HP_CONC )

      CatClassComb(HP_HSLICE,cID)%available = SCIPtrue
      CatClassComb(HP_HSLICE,cID)%type      = type_flag
      CatClassComb(HP_VSLICE,cID)%available = SCIPtrue
      CatClassComb(HP_VSLICE,cID)%type      = type_flag
      IF( lter )THEN
        CatClassComb(HP_SSLICE,cID)%available = SCIPtrue
        CatClassComb(HP_SSLICE,cID)%type      = type_flag
      END IF

    CASE( HP_ADJOINT )

      CatClassComb(HP_HSLICE,cID)%available = SCIPtrue
      CatClassComb(HP_HSLICE,cID)%type      = SCIPfalse
      CatClassComb(HP_VSLICE,cID)%available = SCIPtrue
      CatClassComb(HP_VSLICE,cID)%type      = SCIPfalse
      IF( lter )THEN
        CatClassComb(HP_SSLICE,cID)%available = SCIPtrue
        CatClassComb(HP_SSLICE,cID)%type      = SCIPfalse
      END IF

    CASE( HP_ADJ_SFC )

      CatClassComb(HP_SURF,cID)%available = SCIPtrue
      CatClassComb(HP_SURF,cID)%type      = SCIPfalse

    CASE( HP_INTCONC )

      CatClassComb(HP_VINT,cID)%available = SCIPtrue
      CatClassComb(HP_VINT,cID)%type      = type_flag
      CatClassComb(HP_HINT,cID)%available = SCIPtrue
      CatClassComb(HP_HINT,cID)%type      = type_flag

    CASE( HP_DEP,HP_DOS )

      CatClassComb(HP_SURF,cID)%available = SCIPtrue
      CatClassComb(HP_SURF,cID)%type      = type_flag

    CASE( HP_CMAX )

      CatClassComb(HP_SURF,cID)%available = SCIPtrue
        CatClassComb(HP_SURF,cID)%type = type_flag

    CASE( HP_EMAX )

      CatClassComb(HP_SURF,cID)%available = SCIPtrue
      CatClassComb(HP_SURF,cID)%type      = SCIPfalse

    CASE( HP_MET )

      CatClassComb(HP_SURF,cID)%available = SCIPtrue
      CatClassComb(HP_SURF,cID)%type      = SCIPfalse

  END SELECT

END DO

nbase = ntypm
IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )THEN
  IF( HasPuffs() .OR. (DosPlotAvail() .AND. hasPlotTimes(HP_SRFTIME+1)) )THEN
    nbase = nbase + HP_NUMADJ
  END IF
END IF

DO i = 1,nPclass

  cID = ClassOrder(i)

  SELECT CASE( ClassID(cID) )

    CASE( HP_CONC )

      DO j = 1,ntypm
        ClassChoiceComb(cID,j)%available = HP_AVAILABLE
        IF( is_kind(j) == 0 )THEN
          ClassChoiceComb(cID,j)%kind = SCIPfalse
        ELSE
          nsg = output_groups( material(j) )
          ClassChoiceComb(cID,j)%kind  = SCIPtrue
          ClassChoiceComb(cID,j)%ikind = is_kind(j)
          ClassChoiceComb(cID,j)%nkind = nsg + 1
        END IF
        ClassChoiceComb(cID,j)%itime    = HP_PUFFTIME
        ClassChoiceComb(cID,j)%usertime = SCIPfalse
        ClassChoiceComb(cID,j)%units = TRIM(material(j)%unit)//'/m3'
      END DO

      IF( nPchoiceMC > 0 )THEN
        DO ii = 1,nPchoiceMC
          j = nPchoice - nPchoiceMC + ii
          ClassChoiceComb(cID,j)%available = HP_AVAILABLE
          ClassChoiceComb(cID,j)%kind      = SCIPtrue
          ClassChoiceComb(cID,j)%ikind     = is_kindMC(1,ii)
          ClassChoiceComb(cID,j)%nkind     = n_kindMC(1,ii)
          ClassChoiceComb(cID,j)%itime     = HP_PUFFTIME
          ClassChoiceComb(cID,j)%usertime  = SCIPfalse
          CALL SetUnitsConcMC( ClassChoiceComb(cID,j)%units,ChoiceMCID(ii) )
        END DO
      END IF

    CASE( HP_ADJOINT )

      j = ADJ_MAXLOC
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE + HP_ASSOC_LINE    !Max Location
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_NOTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

      j = ADJ_LOC
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE     !Location
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_PUFFTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

      j = ADJ_MASS
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE     !Mass
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_PUFFTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

    CASE( HP_ADJ_SFC )

      j = ADJ_MAXLOC
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE + HP_ASSOC_LINE    !Max Location
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_NOTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

      j = ADJ_LOC
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE + HP_ASSOC_LINE     !Location
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_SRFTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

      j = ADJ_MASS
      ClassChoiceComb(cID,ntypm+j)%available = HP_AVAILABLE     !Mass
      ClassChoiceComb(cID,ntypm+j)%kind      = SCIPtrue
      ClassChoiceComb(cID,ntypm+j)%ikind     = adj_kind
      ClassChoiceComb(cID,ntypm+j)%nkind     = 2
      ClassChoiceComb(cID,ntypm+j)%itime     = HP_SRFTIME
      ClassChoiceComb(cID,ntypm+j)%usertime  = SCIPfalse
      ClassChoiceComb(cID,ntypm+j)%units     = ' '

    CASE( HP_INTCONC )

      DO j = 1,ntypm
        ClassChoiceComb(cID,j)%available = HP_AVAILABLE
        IF( is_kind(j) == 0 )THEN
          ClassChoiceComb(cID,j)%kind = SCIPfalse
        ELSE
          nsg = output_groups( material(j) )
          ClassChoiceComb(cID,j)%kind  = SCIPtrue
          ClassChoiceComb(cID,j)%ikind = is_kind(j)
          ClassChoiceComb(cID,j)%nkind = nsg + 1
        END IF
        ClassChoiceComb(cID,j)%itime    = HP_PUFFTIME
        ClassChoiceComb(cID,j)%usertime = SCIPfalse
        ClassChoiceComb(cID,j)%units = TRIM(material(j)%unit)//'/m2'
      END DO

      IF( nPchoiceMC > 0 )THEN
        DO ii = 1,nPchoiceMC
          IF( is_kindMC(2,ii) > 0 )THEN
            j = nPchoice - nPchoiceMC + ii
            ClassChoiceComb(cID,j)%available = HP_AVAILABLE
            ClassChoiceComb(cID,j)%kind      = SCIPtrue
            ClassChoiceComb(cID,j)%ikind     = is_kindMC(2,ii)
            ClassChoiceComb(cID,j)%nkind     = n_kindMC(2,ii)
            ClassChoiceComb(cID,j)%itime     = HP_PUFFTIME
            ClassChoiceComb(cID,j)%usertime  = SCIPfalse
            ClassChoiceComb(cID,j)%units     = TRIM(material(ChoiceMCID(ii))%unit)//'/m2'
          END IF
        END DO
      END IF

    CASE( HP_MET )

      DO j = nbase+1,nbase+nter
        ClassChoiceComb(cID,j)%available = HP_AVAILABLE
        ClassChoiceComb(cID,j)%kind      = SCIPfalse
        ClassChoiceComb(cID,j)%itime     = HP_NOTIME
      END DO

    CASE( HP_DEP )

      DO j = 1,ntypm
        CALL SetSrfComb( material(j)%lsrfg,material(j)%lsrft,cID,j )
        ClassChoiceComb(cID,j)%units = TRIM(material(j)%unit)//'/m2'
      END DO


      IF( nPchoiceMC > 0 )THEN
        DO ii = 1,nPchoiceMC
          IF( is_kindMC(3,ii) > 0 )THEN
            j = nPchoice - nPchoiceMC + ii
            ClassChoiceComb(cID,j)%available = HP_AVAILABLE
            ClassChoiceComb(cID,j)%kind      = SCIPtrue
            ClassChoiceComb(cID,j)%ikind     = is_kindMC(3,ii)
            ClassChoiceComb(cID,j)%nkind     = n_kindMC(3,ii)
            ClassChoiceComb(cID,j)%itime     = HP_SRFTIME
            ClassChoiceComb(cID,j)%usertime  = SCIPfalse
            ClassChoiceComb(cID,j)%units     = TRIM(material(ChoiceMCID(ii))%unit)//'/m2'
            CALL SetUnitsDepMC( ClassChoiceComb(cID,j)%units,ChoiceMCID(ii) )
          END IF
        END DO
      END IF

    CASE( HP_DOS,HP_CMAX,HP_EMAX )

      DO j = 1,ntypm
        CALL SetSrfComb( material(j)%ldosg,material(j)%ldost,cID,j )
        ClassChoiceComb(cID,j)%units = TRIM(material(j)%unit)//'-s/m3'
      END DO

      IF( nPchoiceMC > 0 )THEN
        DO ii = 1,nPchoiceMC
          IF( is_kindMC(4,ii) > 0 )THEN
            j = nPchoice - nPchoiceMC + ii
            ClassChoiceComb(cID,j)%available = HP_AVAILABLE
            ClassChoiceComb(cID,j)%kind      = SCIPtrue
            ClassChoiceComb(cID,j)%ikind     = is_kindMC(4,ii)
            ClassChoiceComb(cID,j)%nkind     = n_kindMC(4,ii)
            ClassChoiceComb(cID,j)%itime     = HP_SRFTIME
            ClassChoiceComb(cID,j)%usertime  = SCIPfalse
            CALL SetUnitsDosMC( ClassChoiceComb(cID,j)%units,ChoiceMCID(ii) )
          END IF
        END DO
      END IF

  END SELECT

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetSrfComb( lgrp,ltot,i,j )

USE scipuff_fi
USE field_fd
USE plotlist_fi
USE SCIPresults_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lgrp, ltot !Material surface output flags
INTEGER, INTENT( IN ) :: i          !Plot class
INTEGER, INTENT( IN ) :: j          !Material no.

INTEGER nsg, n1, n2

INTEGER, EXTERNAL :: output_groups
LOGICAL, EXTERNAL :: IsEvap

IF( lgrp .OR. ltot )THEN

  ClassChoiceComb(i,j)%available = HP_AVAILABLE

  IF( is_kind(j) > 0 )THEN
    ClassChoiceComb(i,j)%kind = SCIPtrue
    nsg = output_groups( material(j) )
    IF( lgrp .AND. ltot )THEN
      n1 = 0; n2 = nsg+1
    ELSE IF( ltot )THEN
      n1 = nsg; n2 = 1
    ELSE
      n1 = 0; n2 = nsg
    END IF
    IF( IsEvap( material(j)%icls ) .AND. ClassID(i)==HP_DEP )n2 = n2 + 1
    ClassChoiceComb(i,j)%ikind = is_kind(j) + n1
    ClassChoiceComb(i,j)%nkind = n2
  ELSE
    ClassChoiceComb(i,j)%kind = SCIPfalse
  END IF

  ClassChoiceComb(i,j)%itime    = HP_SRFTIME
  ClassChoiceComb(i,j)%usertime = SCIPfalse

END IF

RETURN
END

!==============================================================================

LOGICAL FUNCTION DepPlotAvail() RESULT( lPlot )

USE scipuff_fi

IMPLICIT NONE

INTEGER i

lPlot = .FALSE.

IF( ntyps > 0 )THEN
  DO i = 1,ntypm
    IF(  material(i)%lsrfg .OR. material(i)%lsrft )THEN
      lPlot = .TRUE.; EXIT
    END IF
  END DO
END IF

RETURN
END
!==============================================================================

LOGICAL FUNCTION DosPlotAvail() RESULT( lPlot )

USE scipuff_fi

IMPLICIT NONE

INTEGER i

lPlot = .FALSE.

IF( ntypd > 0 )THEN
  DO i = 1,ntypm
    IF(  material(i)%ldosg .OR. material(i)%ldost )THEN
      lPlot = .TRUE.; EXIT
    END IF
  END DO
END IF

RETURN
END

!==============================================================================

LOGICAL FUNCTION AvePlotAvail() RESULT( lPlot )

USE files_fi

IMPLICIT NONE

INTEGER n

CHARACTER(PATH_MAXLENGTH) fname

n = LEN_TRIM(file_prj)

fname = file_prj(1:n-3)//'ave'

INQUIRE( FILE=fname,EXIST=lPlot )

RETURN
END

!==============================================================================

INTEGER FUNCTION AvePlotNum()

USE files_fi
USE error_fi
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER irv, n, grdI, i, im, nm

CHARACTER(PATH_MAXLENGTH) fname

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID
INTEGER, EXTERNAL :: SAG_FreeGridID, SAG_CloseID

AvePlotNum = 0

n = LEN_TRIM(file_prj)

fname = file_prj(1:n-3)//'ave'

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error allocating new SAG grid structure'
  GOTO 9999
END IF

irv = SAG_InitGridID( fname,lun_tmp,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error initializing SAG grid structure'
  GOTO 9999
END IF

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error reading SAG grid structure'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )

AvePlotNum = 1

IF( grd%nblk > 1 )THEN

  im = INDEX( grd%ipblk(1)%name,'Max' )
  nm = 1

  DO n = 2,grd%nblk
    i = INDEX( grd%ipblk(n)%name,'Max' )
    IF( i == 0 )CYCLE
    IF( TRIM(grd%ipblk(nm)%name(im:)) /= TRIM(grd%ipblk(n)%name(i:)) )THEN
      AvePlotNum = AvePlotNum + 1
      nm = n
      im = i
    END IF
  END DO

END IF

9999 CONTINUE

IF( grdI > 0 )THEN
  irv = SAG_CloseID( grdI )
  irv = SAG_FreeGridID( grdI )
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION AvePlotNames( nc,names )

USE files_fi
USE error_fi
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,                      INTENT( IN  ) :: nc
CHARACTER(64), DIMENSION(nc), INTENT( OUT ) :: names

INTEGER irv, grdI, i, im, n, nm

CHARACTER(PATH_MAXLENGTH) fname

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID
INTEGER, EXTERNAL :: SAG_FreeGridID, SAG_CloseID

AvePlotNames = 0

n = LEN_TRIM(file_prj)

fname = file_prj(1:n-3)//'ave'

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error allocating new SAG grid structure'
  GOTO 9999
END IF

irv = SAG_InitGridID( fname,lun_tmp,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error initializing SAG grid structure'
  GOTO 9999
END IF

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'AvePlotNum'
  eMessage = 'Error reading SAG grid structure'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )

AvePlotNames = 1

im = INDEX( grd%ipblk(1)%name,'Max' )
nm = 1

names(1) = TRIM(grd%ipblk(1)%name(im:))

IF( grd%nblk > 1 )THEN

  DO n = 2,grd%nblk
    i = INDEX( grd%ipblk(n)%name,'Max' )
    IF( i == 0 )CYCLE
    IF( TRIM(grd%ipblk(nm)%name(im:)) /= TRIM(grd%ipblk(n)%name(i:)) )THEN
      AvePlotNames = AvePlotNames + 1
      names(AvePlotNames) = TRIM(grd%ipblk(n)%name(i:))
      nm = n
      im = i
    END IF
  END DO

END IF

9999 CONTINUE

IF( grdI > 0 )THEN
  irv = SAG_CloseID( grdI )
  irv = SAG_FreeGridID( grdI )
END IF

RETURN
END

!==============================================================================

SUBROUTINE AddKindMC( i,ikind )

USE plotlist_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: i       !Multi-component ID
INTEGER, INTENT( INOUT ) :: ikind   !pointer into Kind list

INTEGER mcID, imat

imat = ChoiceMCID(i)
mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL AddChemKindMC( mat_mc%ID(mcID),ikind,KindString,is_kindMC(1:4,i),n_kindMC(1:4,i) )

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'AddKindMC'
    eMessage = 'Multicomponent error'
    WRITE(eMessage,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================
SUBROUTINE SetUnitsConcMC( units,imat )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: units       !Plotted units string for multicomponents
INTEGER,      INTENT( IN  ) :: imat        !Material

INTEGER mcID
INTEGER ID

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )

    ID = mat_mc%ID(mcID)

    SELECT CASE( chemMC(ID,1)%cUnits )

      CASE( UNIT_PPM )
        units = 'ppm'

      CASE DEFAULT
        units = TRIM(material(imat)%unit)//'/m3'

    END SELECT

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetUnitsConcMC'
    eMessage = 'Multicomponent error'
    WRITE(eMessage,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================
SUBROUTINE SetUnitsDepMC( units,imat )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: units       !Plotted units string for multicomponents
INTEGER,      INTENT( IN  ) :: imat        !Material

INTEGER mcID
INTEGER ID

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )

    ID = mat_mc%ID(mcID)
    SELECT CASE( chemMC(ID,1)%oUnits )
      CASE( UNIT_UGM3 )
        units = 'ug/m2'

      CASE( UNIT_PPM )
        units = 'ppm-m'

      CASE DEFAULT
        units = TRIM(material(imat)%unit)//'/m2'

    END SELECT

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetUnitsDepMC'
    eMessage = 'Multicomponent error'
    WRITE(eMessage,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================
SUBROUTINE SetUnitsDosMC( units,imat )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: units       !Plotted units string for multicomponents
INTEGER,      INTENT( IN  ) :: imat        !Material

INTEGER mcID
INTEGER ID

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )

    ID = mat_mc%ID(mcID)
    ! Use concentration units as dose is divided by dt_save
    SELECT CASE( chemMC(ID,1)%oUnits )
      CASE( UNIT_UGM3 )
        units = 'ug/m3'

      CASE( UNIT_PPM )
        units = 'ppm'

      CASE DEFAULT
        units = TRIM(material(imat)%unit)//'/m3'

    END SELECT

  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetUnitsDosMC'
    eMessage = 'Multicomponent error'
    WRITE(eMessage,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999

END SELECT

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ClearPlotLists()

USE plotlist_fi
USE plotmet_fi

IMPLICIT NONE

IF( ALLOCATED( ClassString     ))DEALLOCATE( ClassString     )
IF( ALLOCATED( ChoiceString    ))DEALLOCATE( ChoiceString    )
IF( ALLOCATED( KindString      ))DEALLOCATE( KindString      )
IF( ALLOCATED( CatClassComb    ))DEALLOCATE( CatClassComb    )
IF( ALLOCATED( ClassChoiceComb ))DEALLOCATE( ClassChoiceComb )
IF( ALLOCATED( ClassID         ))DEALLOCATE( ClassID         )
IF( ALLOCATED( ClassOrder      ))DEALLOCATE( ClassOrder      )
IF( ALLOCATED( ChoiceOrder     ))DEALLOCATE( ChoiceOrder     )
IF( ALLOCATED( ClassInterp     ))DEALLOCATE( ClassInterp     )
IF( ALLOCATED( is_kind         ))DEALLOCATE( is_kind         )
IF( ALLOCATED( ChoiceMCID      ))DEALLOCATE( ChoiceMCID      )
IF( ALLOCATED( is_kindMC       ))DEALLOCATE( is_kindMC       )
IF( ALLOCATED(  n_kindMC       ))DEALLOCATE(  n_kindMC       )

IF( ALLOCATED( nMet2D    ))DEALLOCATE( nMet2D    )
IF( ALLOCATED( nMet3D    ))DEALLOCATE( nMet3D    )
IF( ALLOCATED( MetVar2D  ))DEALLOCATE( MetVar2D  )
IF( ALLOCATED( MetVar3D  ))DEALLOCATE( MetVar3D  )
IF( ALLOCATED( MetUnit2D ))DEALLOCATE( MetUnit2D )
IF( ALLOCATED( MetUnit3D ))DEALLOCATE( MetUnit3D )
IF( ALLOCATED( istgMet   ))DEALLOCATE( istgMet   )

RETURN
END

!==============================================================================

SUBROUTINE InitPlotGrid( lun,file,timeID,mxfld,mxgrd,grdI )

USE error_fi
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf
USE sagerr_fd

IMPLICIT NONE

INTEGER,      INTENT( IN    ) :: lun, mxfld, mxgrd
INTEGER,      INTENT( INOUT ) :: timeID
CHARACTER(*), INTENT( IN    ) :: file
INTEGER,      INTENT( OUT   ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER irv, ios, i, pltfld, iloop, last_record, last_ncells
REAL    last_time

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID, SAG_ReadHeaderID

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'InitPlotGrid'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

!------ Initialize SAG structure

IF( mxfld > 0 )THEN
  pltfld = MAX(3,mxfld)          !Plot grids always require at least 3 fields
ELSE
  pltfld = -3
END IF

irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,mxgrd,pltfld,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitPlotGrid'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Open surface file; read nvart for block version

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitPlotGrid'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Position to selected time break

last_time   = grd%time
last_record = grd%record
last_ncells = grd%ncells

iloop  = timeID
timeID = 0
DO i = 1,iloop
  irv = SAG_ReadHeaderID( grdI )
  IF( irv == SAG_ERROR )THEN
    nError   = UK_ERROR
    eRoutine = 'InitPlotGrid'
    eMessage = 'Error reading SAG grid'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
  IF( grd%status == SAG_EOF_REC )THEN
    grd%status = SAG_GRID_REC
    grd%time   = last_time
    grd%record = last_record
    grd%ncells = last_ncells
    EXIT
  END IF
  last_time   = grd%time
  last_record = grd%record
  last_ncells = grd%ncells
  timeID = timeID + 1
END DO

IF( grd%ncells == 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitPlotGrid'
  eMessage = 'Error reading SAG grid'
  eInform  = 'No grid found'
  GOTO 9999
END IF

!------ Allocate grid and data fields for ncells if necessary

IF( mxgrd == 0 )THEN

  grd%mxgrd = grd%ncells

  ALLOCATE( grd%ipgrd(grd%ncells),STAT=ios )
  IF( ios == 0 )ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )

  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'InitPlotGrid'
    eMessage = 'Error allocating surface grid arrays'
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END
