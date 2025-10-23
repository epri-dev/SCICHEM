INTEGER FUNCTION GetProjectPuffs( ToolUserID,puffHead,it,transfer,getMult,flag )

USE localpuf
USE Extract_fi
USE SCIPtool
USE multcomp_fd

IMPLICIT NONE

INTEGER            :: ToolUserID
TYPE( ppuffHeadT ) :: puffHead
INTEGER            :: it
LOGICAL            :: transfer
LOGICAL            :: getMult
INTEGER            :: flag

INTEGER            :: irv, ios
INTEGER            :: i
INTEGER            :: nChem

GetProjectPuffs = SCIPFailure

CALL ClearLocalPuffs()
IF( nError /= NO_ERROR )GOTO 9999

irv = SCIPGetProjectPuffHeader( ToolUserID,puffHead,it )
IF ( irv /= SCIPSuccess )THEN
  CALL toolError( 'Error in GetProjectPuffs' )
  GO TO 9999
END IF

ALLOCATE( puffs(puffHead%puff%nPuff),STAT=ios )
IF( ios == 0 )ALLOCATE( puffAux(puffHead%puff%nAux),STAT=ios )
IF( ios == 0 )ALLOCATE( puffType(puffHead%puff%nType),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'GetProjectPuffs'
  eMessage = 'Error allocating local puff arrays'
  WRITE(eInform,'(A,I0)')'Request=',puffHead%puff%nPuff,'status=',ios
  GOTO 9999
END IF

puffHead%puff%maxPuff = puffHead%puff%nPuff
puffHead%puff%maxAux  = puffHead%puff%nAux
puffHead%puff%maxType = puffHead%puff%nType

IF( getMult )THEN
  mcList%nMCtype = puffHead%puff%nMCtype
  IF( mcList%nMCtype > 0 )THEN
    ALLOCATE( mcList%type(mcList%nMCtype),STAT=ios )
    IF( ios == 0 )ALLOCATE( mcList%ID(mcList%nMCtype),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GetProjectPuffs'
      eMessage = 'Error allocating local material_MClist'
      WRITE(eInform,'(A,I0)')'Request=',mcList%nMCtype,'status=',ios
      GOTO 9999
    END IF
  END IF
ELSE
  mcList%nMCtype = 0
END IF

irv = SCIPGetProjectPuff( ToolUserID,puffHead,it,flag,puffs,puffAux,puffType,mcList )
nChem = 0
IF( getMult )THEN
  DO i = 1,mcList%nMCtype
    SELECT CASE( mcList%type(i) )
      CASE( MC_CHEM )
        hasChem = .TRUE.
        nChem = nChem + 1
      CASE DEFAULT
    END SELECT
  END DO
  IF( hasChem )THEN
    ALLOCATE( chemOut( nChem ),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GetProjectPuffs'
      eMessage = 'Error allocating local chem array'
      WRITE(eInform,'(A,I0)')'Request=',nChem,'status=',ios
      GOTO 9999
    END IF
    DO i = 1,nChem
      NULLIFY(chemOut(i)%species)
    END DO
    irv = SCIPGetProjectPuffChem( ToolUserID,puffHead%project,chemOut,SCIPfalse)
    IF ( irv /= SCIPSuccess )THEN
      CALL toolError( 'Error in GetProjectPuffs' )
      GOTO 9999
    END IF
    DO i = 1,nChem
      ALLOCATE( chemOut(i)%species(chemOut(i)%nspecies),STAT=ios )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'GetProjectPuffs'
        eMessage = 'Error allocating local chem species array'
        WRITE(eInform,'(A,I0)')'Request=',chemOut(i)%nspecies,'status=',ios
        GOTO 9999
      END IF
    END DO
    irv = SCIPGetProjectPuffChem( ToolUserID,puffHead%project,chemOut,SCIPtrue)
    IF ( irv /= SCIPSuccess )THEN
      CALL toolError( 'Error in GetProjectPuffs' )
      GOTO 9999
    END IF
  END IF
END IF
IF ( irv /= SCIPSuccess )THEN
  CALL toolError( 'Error in GetProjectPuffs' )
  GOTO 9999
END IF

IF( transfer )THEN
  IF( nChem == 0 )THEN
    ALLOCATE( chemOut( 1 ),STAT=ios )
  END IF
  CALL transferLocalToScipuff( it,puffHead,puffs,puffAux,puffType,mcList,nChem,chemOut )
  IF( nError /= NO_ERROR )GOTO 9999
  CALL ClearLocalPuffs()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

GetProjectPuffs = SCIPSuccess

9999 CONTINUE

RETURN
END
!==============================================================================
! transferLocalToScipuff
!==============================================================================
SUBROUTINE transferLocalToScipuff( it,puffHead,puffs,puffAux,puffType,mcList,nChem,chemOut )

USE Extract_fi
USE GetTimes_fi
USE SCIPtool
USE scipuff_fi, ONLY: puff, typeID, t, npuf, dynamic, dense_gas, buoy_gas, ntypp, mat_mc, ntypm, material
USE mcstruct_fd
USE multcomp_fd
USE chem_fi, ONLY: chemMC, chem

IMPLICIT NONE

INTEGER                             :: it
TYPE( ppuffHeadT )                  :: puffHead
TYPE( puffT ),     DIMENSION(*)     :: puffs
TYPE( puffTypeT ), DIMENSION(*)     :: puffType
REAL,              DIMENSION(*)     :: puffAux
TYPE( material_MClist )             :: mcList
INTEGER                             :: nChem
TYPE( ChemMC_out    ), DIMENSION(*) :: chemOut

INTEGER i, ios, npaux, iaux, naux
INTEGER j
INTEGER js
INTEGER, EXTERNAL :: allocatePuffs
INTEGER, EXTERNAl :: allocatePuffAux

CALL ClearProjectPuffs()
IF( nError /= NO_ERROR )GOTO 9999

t     = TimePuff(it)%time%runTime
npuf  = puffHead%puff%nPuff
npaux = puffHead%puff%nAux

ntypp = puffHead%puff%nType
ALLOCATE( typeID(puffHead%puff%nType),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'transferLocalToScipuff'
  eMessage = 'Error allocating scipuff typeID  array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',puffHead%puff%nType,'  : Error = ',ios
  GO TO 9999
END IF

ntypm = 0
DO i = 1,puffHead%puff%nType
  typeID(i)%imat  = puffType(i)%imat
  ntypm = MAX(ntypm,typeID(i)%imat)
  typeID(i)%igrp  = puffType(i)%igrp
  typeID(i)%icls  = puffType(i)%icls
  typeID(i)%npaux = puffType(i)%npaux
  typeID(i)%ipmc  = puffType(i)%ipmc
  typeID(i)%mcID  = puffType(i)%mcID
  IF( puffType(i)%ltot == SCIPtrue )THEN
    typeID(i)%ltot = .TRUE.
  ELSE
    typeID(i)%ltot = .FALSE.
  END IF
END DO

ALLOCATE( material(ntypm),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'GetProjectPuffs'
  eMessage = 'Error allocating scipuff material'
  WRITE(eInform,'(A,I0)')'Request=',ntypm,'status=',ios
  GOTO 9999
END IF
DO i = 1,puffHead%puff%nType
  material(typeID(i)%imat)%cmat = puffType(i)%material
END DO

ios = allocatePuffs( npuf )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'transferLocalToScipuff'
  eMessage = 'Error allocating scipuff puff array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',npuf,'  : Error = ',ios
  GOTO 9999
END IF

DO i = 1,npuf
  CALL UnloadPuff( puffs(i),puff(i) ) !iaux (index into puffAux) is in puff%naux
END DO

DO i = 1,npuf
  naux = typeID(puff(i)%ityp)%npaux
  IF( naux > 0 )THEN
    iaux = puff(i)%naux
    puff(i)%naux = naux
    ios = allocatePuffAux( puff(i) )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'transferLocalToScipuff'
      eMessage = 'Error allocating scipuff puff aux'
      WRITE(eInform,'(A,I0,A,I0,A,I0)')'Puff=',i,' : Request=',naux,' : Error = ',ios
      GOTO 9999
    END IF
    puff(i)%aux = puffAux(iaux:(iaux + puff(i)%naux - 1))
  END IF
END DO

IF( puffHead%puff%dynamic == SCIPtrue )THEN
  dynamic = .TRUE.
  IF( puffHead%puff%dense == SCIPtrue )THEN
    dense_gas = .TRUE.
  ELSE
    dense_gas = .FALSE.
  END IF
  IF( puffHead%puff%buoy == SCIPtrue )THEN
    buoy_gas = .TRUE.
  ELSE
    buoy_gas = .FALSE.
  END IF
ELSE
  dynamic   = .FALSE.
  dense_gas = .FALSE.
  buoy_gas  = .FALSE.
END IF

mat_mc%nMCtype = mcList%nMCtype
IF( mcList%nMCtype > 0 )THEN
  ALLOCATE( mat_mc%type(mcList%nMCtype),STAT=ios )
  IF( ios == 0 )ALLOCATE(mat_mc%ID(mcList%nMCtype),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'GetProjectPuffs'
    eMessage = 'Error allocating scipuff material_MClist'
    WRITE(eInform,'(A,I0)')'Request=',mcList%nMCtype,'status=',ios
    GOTO 9999
  END IF
  DO i = 1,mcList%nMCtype
    mat_mc%type(i) = mcList%type(i)
    mat_mc%ID(i) = mcList%ID(i)
  END DO
END IF
IF( nChem > 0 )THEN
  ALLOCATE( chemMC( nChem,1),STAT=ios ) !Assume single thread
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'GetProjectPuffs'
    eMessage = 'Error allocating SCIPUFF chem array'
    WRITE(eInform,'(A,I0)')'Request=',nChem,'status=',ios
    GOTO 9999
  END IF
  DO i = 1,nChem
    chem => chemMC(i,1)
    NULLIFY(chem%species)
    NULLIFY(chem%star)
    chem%nFast        = chemOut(i)%nFast
    chem%nSlow        = chemOut(i)%nSlow
    chem%nParticle    = chemOut(i)%nParticle
    chem%nEquilibrium = chemOut(i)%nEquilibrium
    chem%nStar        = chemOut(i)%nStar
    chem%nSpecies     = chemOut(i)%nSpecies
    ALLOCATE( chem%species(chem%nSpecies),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GetProjectPuffs'
      eMessage = 'Error allocating SCIPUFF chem species array'
      WRITE(eInform,'(A,I0)')'Request=',chem%nSpecies,'status=',ios
      GOTO 9999
    END IF
    DO j = 1,chem%nSpecies
      chem%species(j)%name  = chemOut(i)%species(j)%name
      chem%species(j)%lstar = chemOut(i)%species(j)%lstar
      chem%species(j)%mass  = chemOut(i)%species(j)%mass
      chem%species(j)%conc  = chemOut(i)%species(j)%conc
      chem%species(j)%amb   = chemOut(i)%species(j)%amb
    END DO
    IF( chem%nStar > 0 )THEN
      ALLOCATE( chem%star(chem%nStar),STAT=ios )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'GetProjectPuffs'
        eMessage = 'Error allocating SCIPUFF chem star array'
        WRITE(eInform,'(A,I0)')'Request=',chem%nStar,'status=',ios
        GOTO 9999
      END IF
      DO j = 1,chem%nStar
        NULLIFY( chem%star(j)%s )
      END DO
      js = 0
      DO j = 1,chem%nSpecies
        IF( chem%species(j)%lstar )THEN
          js = js + 1
          chem%star(js)%s => chem%species(j)
        END IF
      END DO
    END IF
  END DO
END IF
9999 CONTINUE

RETURN
END
!==============================================================================
! ClearProjectPuffs
!==============================================================================
SUBROUTINE ClearProjectPuffs()

USE localpuf
USE Extract_fi
USE SCIPtool
USE scipuff_fi, ONLY: typeID, mat_mc, material
USE multcomp_fd
USE chem_fi, ONLY: chemMC

IMPLICIT NONE

INTEGER ios
INTEGER i

ios = 0
IF( ALLOCATED(typeID) )DEALLOCATE( typeID,STAT=ios )
IF( ios == 0 )CALL deallocatePuffs()
IF( ios /= 0 )THEN
  nError  = UK_ERROR
  eRoutine = 'ClearProjectPuffs'
  eMessage = 'Error deallocating SCIPUFF puff arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF

ios = 0
DO i = 1,mat_mc%nMCtype
  SELECT CASE( mat_mc%type(i) )
    CASE( MC_CHEM )
      IF(ios == 0 .AND. ASSOCIATED(chemMC(i,1)%species) )DEALLOCATE( chemMC(i,1)%species,STAT=ios )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'ClearLocalPuffs'
        eMessage = 'Error deallocating SCIPUFF chem species arrays'
        WRITE(eInform,'(A,I0)')'status=',ios
        GOTO 9999
      END IF
    CASE DEFAULT
  END SELECT
END DO
IF( ALLOCATED(chemMC) )DEALLOCATE( chemMC,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ClearLocalPuffs'
  eMessage = 'Error deallocating SCIPUFF chem arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF
IF( ASSOCIATED(mat_mc%type) )DEALLOCATE( mat_mc%type,STAT=ios )
IF(ios == 0 .AND. ASSOCIATED(mat_mc%ID) )DEALLOCATE( mat_mc%ID,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ClearLocalPuffs'
  eMessage = 'Error deallocating SCIPUFF material_mat_mc arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF
NULLIFY(mat_mc%type)
NULLIFY(mat_mc%ID)
mat_mc%nMCtype = 0
IF( ALLOCATED(material) )DEALLOCATE( material,STAT=ios )
9999 CONTINUE

RETURN
END
!==============================================================================
! ClearLocalPuffs
!==============================================================================
SUBROUTINE ClearLocalPuffs()

USE localpuf
USE Extract_fi
USE SCIPtool
USE multcomp_fd

IMPLICIT NONE

INTEGER ios
INTEGER i

ios = 0
IF( ALLOCATED(puffs) )DEALLOCATE( puffs,STAT=ios )
IF(ios == 0 .AND. ALLOCATED(puffAux)  )DEALLOCATE( puffAux, STAT=ios )
IF(ios == 0 .AND. ALLOCATED(puffType) )DEALLOCATE( puffType,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ClearLocalPuffs'
  eMessage = 'Error deallocating local puff arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF

ios = 0
DO i = 1,mcList%nMCtype
  SELECT CASE( mcList%type(i) )
    CASE( MC_CHEM )
      IF(ios == 0 .AND. ASSOCIATED(chemOut(i)%species) )DEALLOCATE( chemOut(i)%species,STAT=ios )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'ClearLocalPuffs'
        eMessage = 'Error deallocating local chem species arrays'
        WRITE(eInform,'(A,I0)')'status=',ios
        GOTO 9999
      END IF
    CASE DEFAULT
  END SELECT
END DO
IF( ALLOCATED(chemOut) )DEALLOCATE( chemOut,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ClearLocalPuffs'
  eMessage = 'Error deallocating local chem arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF
IF( ASSOCIATED(mcList%type) )DEALLOCATE( mcList%type,STAT=ios )
IF(ios == 0 .AND. ASSOCIATED(mcList%ID) )DEALLOCATE( mcList%ID,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ClearLocalPuffs'
  eMessage = 'Error deallocating local material_MClist arrays'
  WRITE(eInform,'(A,I0)')'status=',ios
  GOTO 9999
END IF
NULLIFY(mcList%type)
NULLIFY(mcList%ID)
mcList%nMCtype = 0

hasChem = .FALSE.

9999 CONTINUE

RETURN
END
