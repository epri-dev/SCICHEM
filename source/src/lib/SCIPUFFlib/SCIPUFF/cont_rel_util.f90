MODULE cont_rel_functions

IMPLICIT NONE

  INTERFACE
    SUBROUTINE process_scn_cont( relSpec,crmode,defIN )
      USE cont_rel_fd
      IMPLICIT NONE
      TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
      INTEGER,              INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE
      INTEGER, OPTIONAL,    INTENT( IN    ) :: defIN       !Release ID (for update only)
    END SUBROUTINE process_scn_cont
  END INTERFACE

CONTAINS

!*******************************************************************************
! cont_release_triad utilities
!*******************************************************************************
!*******************************************************************************
! init_contTriad_R8
!*******************************************************************************
SUBROUTINE init_contTriad_R8( triad )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_triad_R8 ), INTENT( OUT ) :: triad

triad%x = 0.0D0
triad%y = 0.0D0
triad%z = 0.0D0

RETURN
END SUBROUTINE
!*******************************************************************************
! init_contTriad_R4
!*******************************************************************************
SUBROUTINE init_contTriad_R4( triad )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_triad_R4 ), INTENT( OUT ) :: triad

triad%x = 0.0
triad%y = 0.0
triad%z = 0.0

RETURN
END SUBROUTINE
!*******************************************************************************
! cont_release_static utilities
!*******************************************************************************
!*******************************************************************************
! init_contStatic
!*******************************************************************************
SUBROUTINE init_contStatic( stat )

USE default_fd
USE cont_rel_fd
USE scipuff_fi, ONLY: static

IMPLICIT NONE

TYPE( cont_release_static ), INTENT( OUT ) :: stat

stat%del   = NOT_SET_R
stat%dur   = NOT_SET_R
stat%step  = NOT_SET_R
stat%tstat = NOT_SET_R
stat%ilev  = 0
stat%tlev  = NOT_SET_I

stat%hasStatics = .FALSE.
stat%doStatics = static

RETURN
END SUBROUTINE
!*******************************************************************************
! cont_release_rel utilities
!*******************************************************************************
!*******************************************************************************
! allocate_contRel_puff
!*******************************************************************************
SUBROUTINE allocate_contRel_puff( rel )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

CALL allocate_puffs( rel%npuff,rel%relPuff )

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_contRel_vapor
!*******************************************************************************
SUBROUTINE allocate_contRel_vapor( rel )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

CALL allocate_puffs( rel%npuff,rel%vapPuff )

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_puffs
!*******************************************************************************
SUBROUTINE allocate_puffs( npuff,puffs )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

INTEGER,                   INTENT( IN ) :: npuff
TYPE( puff_str ), DIMENSION(:), POINTER :: puffs

INTEGER i, ios

IF( npuff > 0 )THEN
  IF( ASSOCIATED(puffs) )THEN
    nError   = UK_ERROR
    eRoutine = 'allocate_contRel_puff'
    eMessage = 'Error allocating source puff array'
    eInform  = 'Source puff array is already allocated'
    GOTO 9999
  END IF
  ALLOCATE( puffs(npuff),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'allocate_contRel_puff'
    eMessage = 'Error allocating source puff array'
    WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' : size =',npuff
    GOTO 9999
  END IF
  DO i = 1,npuff
    NULLIFY( puffs(i)%aux )
    CALL zero_puff( puffs(i) )
  END DO
END IF

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_contRels_aux
!*******************************************************************************
SUBROUTINE allocate_contRels_aux()

!  Allocate source aux of all releases of all cDefinition
!  This assumes that releases have been allocated and have a naux specified
!  This is called after reading the DataHeader from the puff file

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER i, j

IF( numDefinition > 0 )THEN
  DO i = 1,numDefinition
    IF( cDefinition(i)%rSet%nrel > 0 )THEN
      DO j = 1,cDefinition(i)%rSet%nrel
        CALL allocate_contRel_aux( cDefinition(i)%rSet%rels(j) )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END IF
  END DO
END IF

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_contRel_aux
!*******************************************************************************
SUBROUTINE allocate_contRel_aux( rel )

!  Allocate source aux of a release

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

INTEGER ios

IF( ASSOCIATED(rel%saux) )THEN
  nError   = UK_ERROR
  eRoutine = 'allocate_contRel_aux'
  eMessage = 'Error allocating source aux array'
  eInform  = 'Source aux array is already allocated'
ELSE
  IF( rel%naux > 0 )THEN
    ALLOCATE( rel%saux(rel%naux),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'allocate_contRel_aux'
      eMessage = 'Error allocating source aux array'
      WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' : size =',rel%naux
      GOTO 9999
    END IF
    rel%saux = 0.0
  ELSE
    NULLIFY( rel%saux )
  END IF
END IF

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! deallocate_contRel
!*******************************************************************************
INTEGER FUNCTION deallocate_contRel( rel ) RESULT( ios )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

ios = deallocate_contRel_aux( rel )
ios = ios + deallocate_contRel_puff( rel )
ios = ios + deallocate_contRel_vapor( rel )

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_contRel_aux
!*******************************************************************************
INTEGER FUNCTION deallocate_contRel_aux( rel ) RESULT( ios )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

ios = 0

IF( ASSOCIATED(rel%saux) )THEN
  DEALLOCATE( rel%saux,STAT=ios )
  NULLIFY( rel%saux )
END IF

rel%naux = 0

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_contRel_puff
!*******************************************************************************
INTEGER FUNCTION deallocate_contRel_puff( rel ) RESULT( ios )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

ios = deallocate_puffs( rel%relPuff )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'deallocate_contRel_puff'
  eMessage = 'Error deallocating release puffs'
  WRITE(eInform,'(A,I0)')'IOS = ',ios
END IF

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_contRel_vapor
!*******************************************************************************
INTEGER FUNCTION deallocate_contRel_vapor( rel ) RESULT( ios )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

ios = deallocate_puffs( rel%vapPuff )

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_puffs
!*******************************************************************************
INTEGER FUNCTION deallocate_puffs( puffs ) RESULT( ios )

USE cont_rel_fd

IMPLICIT NONE

TYPE( puff_str ), DIMENSION(:), POINTER :: puffs

INTEGER i, n, jos
INTEGER, EXTERNAL :: deallocatePuffAux

ios = 0

IF( ASSOCIATED(puffs) )THEN
  n = SIZE( puffs )
  DO i = 1,n
    ios = ios + deallocatePuffAux( puffs(i) )
  END DO
  DEALLOCATE( puffs,STAT=jos )
  ios = jos + ios
  NULLIFY( puffs )
END IF

RETURN
END FUNCTION
!*******************************************************************************
! init_contRel
!*******************************************************************************
SUBROUTINE init_contRel( rel )

USE cont_rel_fd
USE default_fd

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( OUT ) :: rel

rel%time     = 0.0
rel%plen     = 0.0
rel%fracUP   = 1.0
rel%naux     = 0
rel%npuff    = 0

rel%isStatic = .FALSE.

CALL init_contTriad_R8( rel%loc )

NULLIFY( rel%basePuff%aux )

CALL zero_puff( rel%basePuff )

NULLIFY( rel%saux )
NULLIFY( rel%relPuff )
NULLIFY( rel%vapPuff )

RETURN
END SUBROUTINE
!*******************************************************************************
! staticID
!*******************************************************************************
INTEGER FUNCTION staticID( triad ) RESULT( id )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

TYPE( cont_release_id ), INTENT(IN) :: triad

id = 0

CALL MVBITS( triad%idef,0,LEN_IDDEF ,id,POS_IDDEF  )
CALL MVBITS( triad%irel,0,LEN_IDSET ,id,POS_IDSET  )
CALL MVBITS( triad%ipuf,0,LEN_IDPUFF,id,POS_IDPUFF )

RETURN
END FUNCTION
!*******************************************************************************
! relPuffID
!*******************************************************************************
FUNCTION relPuffID( id ) RESULT( triad )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT(IN)     :: id
TYPE( cont_release_id ) :: triad

triad%idef = IBITS(id,POS_IDDEF ,LEN_IDDEF  )
triad%irel = IBITS(id,POS_IDSET, LEN_IDSET  )
triad%ipuf = IBITS(id,POS_IDPUFF,LEN_IDPUFF )

RETURN
END FUNCTION
!*******************************************************************************
! cont_release_set utilities
!*******************************************************************************
!*******************************************************************************
! deallocate_contSet_rel
!*******************************************************************************
INTEGER FUNCTION deallocate_contSet_rel( set ) RESULT( ios )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_set ), INTENT( INOUT ) :: set

INTEGER i, n, jos

ios = 0

IF( ASSOCIATED(set%rels) )THEN
  n = SIZE(set%rels)
  DO i = 1,n
    ios = ios + deallocate_contRel( set%rels(i) )
  END DO
  DEALLOCATE( set%rels,STAT=jos )
  ios = ios + jos
  NULLIFY( set%rels )
  set%nrel = 0
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
! allocate_contSet_rel
!*******************************************************************************
SUBROUTINE allocate_contSet_rel( set )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_set ), INTENT( INOUT ) :: set

INTEGER i, ios

IF( ASSOCIATED(set%rels) )THEN
  nError   = UK_ERROR
  eRoutine = 'allocate_Definition_rel'
  eMessage = 'Error allocating release specification array'
  eInform  = 'Release specification array is already allocated'
ELSE
  IF( set%nrel > 0 )THEN
    ALLOCATE( set%rels(set%nrel),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'allocate_contSet_rel'
      eMessage = 'Error allocating release specification array'
      WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' : size =',set%nrel
      GOTO 9999
    END IF
    DO i = 1,set%nrel
      CALL init_contRel( set%rels(i) )
    END DO
  ELSE
    NULLIFY( set%rels )
  END IF
END IF

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! init_contSet
!*******************************************************************************
SUBROUTINE init_contSet( set )

USE default_fd
USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_set ), INTENT( OUT ) :: set

set%tlev = 0
set%nrel = 0
NULLIFY( set%rels )

RETURN
END SUBROUTINE
!*******************************************************************************
! cont_release_def utilities
!*******************************************************************************
!*******************************************************************************
! nextDefinition
!*******************************************************************************
INTEGER FUNCTION nextDefinition() RESULT( next )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

next = 0

IF( numDefinition == maxDefinition )THEN
  CALL compress_Definition()
  IF( numDefinition == maxDefinition )THEN
    CALL reallocate_Definition()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

numDefinition = numDefinition + 1

next = numDefinition

IF( cDefinition(next)%ID /= next )THEN
  nError   = UK_ERROR
  eRoutine = 'nextDefinition'
  eMessage = 'Inconsistency in release definition ID'
  WRITE(eInform,'(A,I0,A,I0)')'Definition ID =',cDefinition(next)%ID,' : Expected ID =',next
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
! compress_Definition
!*******************************************************************************
SUBROUTINE compress_Definition()

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER i, inext

inext = 0

DO i = 1,numDefinition
  IF( cDefinition(i)%state == CR_EMPTY )CYCLE
  inext = inext + 1
  IF( inext /= i )THEN
    CALL move_Definition( cDefinition(i),cDefinition(inext) )
    IF( .NOT.cDefinition(inext)%isPool )CALL update_static_rel_pointers( cDefinition(inext) )
  END IF
END DO

numDefinition = inext

RETURN
END SUBROUTINE
!*******************************************************************************
! reallocate_Definition
!*******************************************************************************
SUBROUTINE reallocate_Definition()

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER, PARAMETER :: INC_DEF = 5

INTEGER oldMax, oldNum, ios, jos, i

TYPE( cont_release_def ), DIMENSION(:), ALLOCATABLE, TARGET :: temDef

IF( ALLOCATED(cDefinition) )THEN

  oldMax = maxDefinition
  oldNum = numDefinition   !Don't expect to reallocate unless these are both the same value

  ALLOCATE( temDef(oldMax),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Definition'
    eMessage = 'Error allocating temporary continuous release definition array'
    GOTO 9999
  END IF

  DO i = 1,oldMax
    CALL init_Definition( temDef(i),i )
    CALL move_Definition( cDefinition(i),temDef(i) )
  END DO

  ios = deallocate_Definitions()
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Definition'
    eMessage = 'Error deallocating continuous release definition array'
    GOTO 9999
  END IF

  CALL allocate_Definitions( oldMax+INC_DEF )

  DO i = 1,oldMax
    CALL move_Definition( temDef(i),cDefinition(i) )
  END DO

  numDefinition = oldNum

ELSE

  CALL allocate_Definitions( INC_DEF )

END IF

9999 CONTINUE

IF( ALLOCATED(temDef) )THEN
  ios = 0
  DO i = 1,SIZE(temDef)
    ios = ios + deallocate_Definition( temDef(i) )
  END DO
  DEALLOCATE( temDef,STAT=jos )
  ios = ios + jos
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Definition'
    eMessage = 'Error deallocating temporary continuous release definition array'
  END IF
END IF

RETURN
END SUBROUTINE
!*******************************************************************************
! init_Definition
!*******************************************************************************
SUBROUTINE init_Definition( def,ID )

USE default_fd
USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( OUT ) :: def
INTEGER,                  INTENT( IN  ) :: ID

def%ID      = ID
def%cID     = 0
def%mID     = 0
def%state     = CR_EMPTY
def%isDynamic = .FALSE.
def%isPool    = .FALSE.
def%isMoving  = .FALSE.
def%end       = NOT_SET_R
def%dur       = 0.0
def%dtr       = NOT_SET_R

CALL init_contSet( def%rSet )

CALL init_contTriad_R8( def%loc )
CALL init_contTriad_R4( def%vel )

NULLIFY( def%nextDef )
NULLIFY( def%prevDef )

def%update  = .FALSE.
def%extraUpdate = .FALSE.

CALL InitReleaseSpec( def%relSpec )

RETURN
END SUBROUTINE
!*******************************************************************************
! move_Definition
!*******************************************************************************
SUBROUTINE move_Definition( old,new )

USE cont_rel_fi

IMPLICIT NONE

TYPE( cont_release_def ),         INTENT( INOUT ) :: old
TYPE( cont_release_def ), TARGET, INTENT( INOUT ) :: new

INTEGER ID

ID = new%ID

new = old

new%ID = ID

IF( ASSOCIATED(new%nextDef) )THEN
  new%nextDef%prevDef => new
ELSE
  cCollection(new%cID)%lastDef => new
END IF

IF( ASSOCIATED(new%prevDef) )THEN
  new%prevDef%nextDef => new
ELSE
  cCollection(new%cID)%firstDef => new
END IF

CALL init_Definition( old,old%ID )

RETURN
END SUBROUTINE
!*******************************************************************************
! deallocate_Definition
!*******************************************************************************
INTEGER FUNCTION deallocate_Definition( def ) RESULT( ios )

USE default_fd
USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

ios = deallocate_Definition_rel( def )

NULLIFY( def%nextDef )
NULLIFY( def%prevDef )

IF( ASSOCIATED( def%relSpec%MClist%firstMCRel ) )THEN
  CALL ClearMCrelList(def%relSpec%MClist)
END IF
def%relSpec%release%trel = NOT_SET_R

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_Definition_rel
!*******************************************************************************
INTEGER FUNCTION deallocate_Definition_rel( def ) RESULT( ios )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

ios = deallocate_contSet_rel( def%rSet )

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
! deallocate_Definitions
!*******************************************************************************
INTEGER FUNCTION deallocate_Definitions() RESULT( ios )

USE cont_rel_fi
USE default_fd

IMPLICIT NONE

INTEGER i, n, jos

ios = 0

IF( ALLOCATED( cDefinition ) )THEN
  n = SIZE(cDefinition)
  DO i = 1,n
    ios = ios + deallocate_Definition( cDefinition(i) )
  END DO
  DEALLOCATE( cDefinition,STAT=jos )
  ios = ios + jos
END IF

maxDefinition = 0
numDefinition = 0

RETURN
END FUNCTION
!*******************************************************************************
! allocate_Definitions
!*******************************************************************************
SUBROUTINE allocate_Definitions( size )

USE cont_rel_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: size

INTEGER i, ios

IF( ALLOCATED(cDefinition) )THEN
  ios = -999
ELSE IF( size > 0 )THEN
  ALLOCATE( cDefinition(size),STAT=ios )
  IF( ios == 0 )THEN
    maxDefinition = size
    DO i = 1,maxDefinition
      CALL init_Definition( cDefinition(i),i )
    END DO
  END IF
END IF

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_Definition_rel
!*******************************************************************************
SUBROUTINE allocate_Definition_rel( def )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

CALL allocate_contSet_rel( def%rSet )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! allocate_Definitions_rel
!*******************************************************************************
SUBROUTINE allocate_Definitions_rel( num,defs )

USE cont_rel_fd
USE error_fi

IMPLICIT NONE

INTEGER,                                  INTENT( IN    ) :: num
TYPE( cont_release_def ), DIMENSION(num), INTENT( INOUT ) :: defs

INTEGER i

DO i = 1,num
  CALL allocate_Definition_rel( defs(i) )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! deactivate_Definition
!*******************************************************************************
SUBROUTINE deactivate_Definition( def )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ), TARGET, INTENT( INOUT ) :: def

INTEGER ios, clev

INTEGER, EXTERNAL :: remove_static_puffs_set

IF( validCollectionID(def%cID) )THEN
  IF( cCollection(def%cID)%rStat%hasStatics )clev = remove_static_puffs_set( def%rSet )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL removeFromCollection( def )

ELSE
  nError   = UK_ERROR
  eRoutine = 'deactivate_Definition'
  eMessage = 'Error removing release definition from release collection'
  eInform  = 'Release definition collection ID invalid'
  GOTO 9999
END IF

ios = deallocate_Definition( def )

CALL init_Definition( def,def%ID )

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! validDefID
!*******************************************************************************
LOGICAL FUNCTION validDefID( idef ) RESULT( yes )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: idef

yes = ( idef >= 1 .AND. idef <= numDefinition )

RETURN
END FUNCTION
!*******************************************************************************
! countDefinitions
!*******************************************************************************
INTEGER FUNCTION countDefinitions() RESULT( num )

USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

INTEGER idef

num = 0
DO idef = 1,numDefinition
  IF( cDefinition(idef)%state == CR_EMPTY )CYCLE  !Completed non-Pool but not yet compressed
  IF( cDefinition(idef)%time == DEF_VAL_R )CYCLE  !Completed Pool
  num = num + 1
END DO

RETURN
END FUNCTION
!*******************************************************************************
! readyDefinition
!*******************************************************************************
INTEGER FUNCTION readyDefinition() RESULT( num )

USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

INTEGER idef

num = 0
DO idef = 1,numDefinition
  IF( cDefinition(idef)%state /= CR_READY )CYCLE  !Empty and active non-Pool
  IF( cDefinition(idef)%time == DEF_VAL_R )CYCLE  !Completed Pool
  num = num + 1
END DO

RETURN
END FUNCTION
!*******************************************************************************
! tlevDefinition
!*******************************************************************************
INTEGER FUNCTION tlevDefinition() RESULT( lev )

USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

INTEGER idef

lev = 0
DO idef = 1,numDefinition
  IF( cDefinition(idef)%state /= CR_ACTIVE )CYCLE  !Empty and Ready non-Pool
  IF( cDefinition(idef)%time  == DEF_VAL_R )CYCLE   !Completed Pool
  lev = MAX(lev,cDefinition(idef)%rset%tlev)
END DO

RETURN
END FUNCTION
!*******************************************************************************
! count_nrel
!*******************************************************************************
INTEGER FUNCTION count_nrel() RESULT( num )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER idef

num = 0
DO idef = 1,numDefinition
  num = num + cDefinition(idef)%rSet%nrel
END DO

RETURN
END FUNCTION
!*******************************************************************************
! cont_release_col utilities
!*******************************************************************************
!*******************************************************************************
! next_Collection
!*******************************************************************************
INTEGER FUNCTION next_Collection() RESULT( next )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

next = 0

IF( numCollection == maxCollection )THEN
  next = find_free_Collection()
  IF( next == 0 )THEN
    CALL reallocate_Collection()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

IF( next == 0 )THEN
  numCollection = numCollection + 1
  next          = numCollection
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
! find_free_Collection
!*******************************************************************************
INTEGER FUNCTION find_free_Collection() RESULT( next )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER i

next = 0

DO i = 1,maxCollection
  IF( .NOT.cCollection(i)%isActive )THEN
    next = i
    EXIT
  END IF
END DO

RETURN
END FUNCTION
!*******************************************************************************
! reallocate_Collection
!*******************************************************************************
SUBROUTINE reallocate_Collection()

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER, PARAMETER :: INC_COL = 5

INTEGER oldMax, oldNum, ios, i

TYPE( cont_release_col ), DIMENSION(:), ALLOCATABLE :: temCol

IF( ALLOCATED(cCollection) )THEN

  oldMax = maxCollection
  oldNum = numCollection

  ALLOCATE( temCol(oldMax),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Collection'
    eMessage = 'Error allocating temporary continuous release collection array'
    GOTO 9999
  END IF

  DO i = 1,oldMax
    CALL init_Collection( temCol(i) )
    CALL move_Collection( cCollection(i),temCol(i) )
  END DO

  ios = deallocate_Collections()
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Collection'
    eMessage = 'Error deallocating continuous release collection array'
    GOTO 9999
  END IF

  ios = allocate_Collections( oldMax+INC_COL )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Collection'
    eMessage = 'Error allocating continuous release collection array'
    GOTO 9999
  END IF

  DO i = 1,oldMax
    CALL move_Collection( temCol(i),cCollection(i) )
  END DO

  numCollection = oldNum

ELSE

  ios = allocate_Collections( INC_COL )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Collection'
    eMessage = 'Error allocating continuous release collection array'
    GOTO 9999
  END IF

END IF

9999 CONTINUE

IF( ALLOCATED(temCol) )THEN
  DEALLOCATE( temCol,STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'reallocate_Collection'
    eMessage = 'Error deallocating temporary continuous release collection array'
  END IF
END IF

RETURN
END SUBROUTINE
!*******************************************************************************
! init_Collection
!*******************************************************************************
SUBROUTINE init_Collection( col )

USE default_fd
USE cont_rel_fd
USE scipuff_fi, ONLY: static

IMPLICIT NONE

TYPE( cont_release_col ), INTENT( OUT ) :: col

col%isActive   = .FALSE.
col%isPool     = .FALSE.
col%isDynamic  = .FALSE.
col%update_cc  = .FALSE.

CALL init_contStatic( col%rStat )

NULLIFY( col%firstDef )
NULLIFY( col%lastDef )

RETURN
END SUBROUTINE
!*******************************************************************************
! move_Collection
!*******************************************************************************
SUBROUTINE move_Collection( old,new )

USE cont_rel_fd

IMPLICIT NONE

TYPE( cont_release_col ), INTENT( INOUT ) :: old
TYPE( cont_release_col ), INTENT(   OUT ) :: new

new = old

CALL init_Collection( old )

RETURN
END SUBROUTINE
!*******************************************************************************
! deallocate_Collections
!*******************************************************************************
INTEGER FUNCTION deallocate_Collections() RESULT( ios )

USE cont_rel_fi
USE default_fd

IMPLICIT NONE

ios = 0

IF( ALLOCATED(cCollection) )DEALLOCATE( cCollection,STAT=ios )

numCollection  = 0
maxCollection  = 0

RETURN
END FUNCTION
!*******************************************************************************
! allocate_Collections
!*******************************************************************************
INTEGER FUNCTION allocate_Collections( size ) RESULT( ios )

USE cont_rel_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: size

INTEGER i

IF( ALLOCATED(cCollection) )THEN
  ios = -999
ELSE
  ALLOCATE( cCollection(size),STAT=ios )
  IF( ios == 0 )THEN
    maxCollection = size
    DO i = 1,maxCollection
      CALL init_Collection( cCollection(i) )
    END DO
  END IF
END IF

RETURN
END FUNCTION
!*******************************************************************************
! clear_Collections
!*******************************************************************************
SUBROUTINE clear_Collections()

USE cont_rel_fi
USE default_fd

IMPLICIT NONE

INTEGER i

IF( ALLOCATED(cCollection) )THEN
  DO i = 1,numCollection
    CALL init_Collection( cCollection(i) )
  END DO
END IF

numCollection  = 0

RETURN
END SUBROUTINE
!*******************************************************************************
! AddToCollection
!*******************************************************************************
SUBROUTINE AddToCollection( def )

USE scipuff_fi, ONLY: static
USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

TYPE( cont_release_def ), TARGET, INTENT( INOUT ) :: def

INTEGER icol

!==== Don't add deactivated pools to a collection

IF( def%isPool .AND. def%time == DEF_VAL_R )GOTO 9999

icol = find_Collection( def )
IF( nError /= NO_ERROR )GOTO 9999

IF( icol == 0 )THEN
  icol = next_Collection()
  IF( nError /= NO_ERROR )GOTO 9999
  IF( def%isPool )THEN
    cCollection(icol)%isPool          = .TRUE.
    cCollection(icol)%rStat%doStatics = .FALSE.
  ELSE
    initStatics = static
  END IF
END IF

CALL AddDefinitionToCollection( def,icol )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! AddDefinitionToCollection
!*******************************************************************************
SUBROUTINE AddDefinitionToCollection( def,icol )

USE scipuff_fi, ONLY: static
USE cont_rel_fi
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ), TARGET, INTENT( INOUT ) :: def
INTEGER,                          INTENT( IN    ) :: icol

IF( ALLOCATED(cCollection) )THEN
  IF( icol > 0 .AND. icol <= numCollection )THEN
    IF( def%isPool .NEQV. cCollection(icol)%isPool )THEN
      nError   = UK_ERROR
      eRoutine = 'AddDefinitionToCollection'
      eMessage = 'Error adding release definition to release collection'
      WRITE(eInform,'(A,L,A,L)')'Incompatability in pool setting. col=', &
                                cCollection(icol)%isPool,' : def=',def%isPool
      GOTO 9999
    END IF
    IF( ASSOCIATED(cCollection(icol)%lastDef) )THEN
      IF( .NOT.ASSOCIATED(cCollection(icol)%lastDef%nextDef) )THEN      !End of chain
        cCollection(icol)%lastDef%nextDef => def
        def%prevDef => cCollection(icol)%lastDef
        NULLIFY( def%nextDef )
        cCollection(icol)%lastDef => def
        IF( def%isDynamic )cCollection(icol)%isDynamic = .TRUE.
      ELSE                                                          !corrupt end of chain
        nError   = UK_ERROR
        eRoutine = 'AddDefinitionToCollection'
        eMessage = 'Error adding release definition to release collection'
        eInform  = 'Invalid end to linked list'
        GOTO 9999
      END IF
    ELSE                                                            !First  in chain
      cCollection(icol)%firstDef => def
      cCollection(icol)%lastDef  => def
      cCollection(icol)%isDynamic = def%isDynamic
      cCollection(icol)%isActive = .TRUE.
    END IF
    def%cID = icol
    cCollection(icol)%update_cc = .NOT.cCollection(icol)%isPool
  ELSE
    nError   = UK_ERROR
    eRoutine = 'AddDefinitionToCollection'
    eMessage = 'Error adding release definition to release collection'
    WRITE(eInform,'(A,I0,A,I0)')'Release set id out of range. id=',icol,' : max=',numCollection
    GOTO 9999
  END IF
ELSE
  nError   = UK_ERROR
  eRoutine = 'AddDefinitionToCollection'
  eMessage = 'Error adding release definition to release collection'
  eInform  = 'Release collection array not allocated'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE
!*******************************************************************************
! removeFromCollection
!*******************************************************************************
SUBROUTINE removeFromCollection( def )

USE scipuff_fi, ONLY: static
USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

TYPE( cont_release_def ), TARGET, INTENT( INOUT ) :: def

TYPE( cont_release_def ), POINTER :: pdef

LOGICAL initAll

initALL = .FALSE.

IF( validCollectionID(def%cID) )THEN
  IF( ASSOCIATED(def%prevDef) )THEN
    IF( ASSOCIATED(def%nextDef) )THEN                !Middle of list
      def%prevDef%nextDef => def%nextDef
      def%nextDef%prevDef => def%prevDef
    ELSE                                             !End of list
      NULLIFY( def%prevDef%nextDef )
      cCollection(def%cID)%lastDef => def%prevDef
    END IF
  ELSE
    IF( ASSOCIATED(def%nextDef) )THEN                !Start of list
      NULLIFY( def%nextDef%prevDef )
      cCollection(def%cID)%firstDef => def%nextDef
    ELSE                                             !Only member if list
      CALL init_Collection( cCollection(def%cID) )
      initAll = static
    END IF
  END IF

!==== Reset this collection to update cc

  cCollection(def%cID)%update_cc = cCollection(def%cID)%isActive .AND. .NOT.cCollection(def%cID)%isPool

!==== Reset all collections to redo statics (recompute distance between collections)

  IF( initAll )initStatics = static

ELSE
  nError   = UK_ERROR
  eRoutine = 'removeFromCollection'
  eMessage = 'Error removing release definition from release collection'
  eInform  = 'Release definition collection ID invalid'
  GOTO 9999
END IF

9999 CONTINUE

NULLIFY( pdef )

RETURN
END SUBROUTINE
!*******************************************************************************
! belongsToCollection
!*******************************************************************************
LOGICAL FUNCTION belongsToCollection( icol,def ) RESULT( yes )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER,                  INTENT( IN ) :: icol
TYPE( cont_release_def ), INTENT( IN ) :: def

LOGICAL lxx_col, lxx_def, lzz_col, lzz_def

yes = .FALSE.

!==== Check validity of request

yes = validCollectionID( icol )
IF( .NOT.yes )THEN
  nError   = UK_ERROR
  eRoutine = 'belongsToCollection'
  eMessage = 'Error testing release collection. Set ID out of range'
  WRITE(eInform,'(A,I0,A,I0)')'col ID=',icol,' : num set=',numCollection
  GOTO 9999
END IF

IF( .NOT.cCollection(icol)%isActive )GOTO 9999

!A Collection is a set of Definitions
!One collection contains all Pool releases and does not compute release interactions nor release statics
!Other collections contain definitions that are colocated since they may interact and need to have statics run together
!For consistency with v24, Regular continuous releases and Stack continuous releases are placed in separate sets
!which prevents statics when a Regular release and a Stack release arre colocated.

IF( def%isPool )THEN
!==== Check pool compatability only
  yes = cCollection(icol)%isPool
ELSE
!==== check location
  yes = colocatedWithCollection( cCollection(icol)%FirstDef,def )
!==== Check that both are/are not regular/stack continuous releases
  IF( yes )THEN
    lxx_col = cCollection(icol)%FirstDef%rSet%rels(1)%basePuff%sxx == 0.
    lxx_def = def%rSet%rels(1)%basePuff%sxx == 0.
    lzz_col = cCollection(icol)%FirstDef%rSet%rels(1)%basePuff%szz == 0.
    lzz_def = def%rSet%rels(1)%basePuff%szz == 0.
!==== Check that both are/are not regular continuous releases
    yes = (lxx_col .EQV. lxx_def)
!==== Check that both are/are not stack continuous releases
    yes = yes .AND. (lzz_col .EQV. lzz_def)
!==== check dynamics .OR. material
    IF( yes )THEN
      yes = def%isDynamic .OR. cCollection(icol)%isDynamic .OR. (def%mID == cCollection(icol)%FirstDef%mID)
    END IF
  END IF
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
! colocatedWithCollection
!*******************************************************************************
LOGICAL FUNCTION colocatedWithCollection( col,def ) RESULT( yes )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ),      POINTER :: col
TYPE( cont_release_def ), INTENT( IN ) :: def

REAL(8) del

del = (col%loc%x - def%loc%x)**2 + &
      (col%loc%y - def%loc%y)**2 + &
      (col%loc%z - def%loc%z)**2

IF( del == 0.0D0 )THEN
  del = DBLE((col%vel%x - def%vel%x))**2 + &
        DBLE((col%vel%y - def%vel%y))**2 + &
        DBLE((col%vel%z - def%vel%z))**2
END IF

yes = del == 0.0D0

RETURN
END FUNCTION
!*******************************************************************************
! find_Collection
!*******************************************************************************
INTEGER FUNCTION find_Collection( def ) RESULT( icol )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( IN ) :: def

INTEGER i

icol = 0

IF( numCollection > 0 )THEN
  DO i = 1,numCollection
    IF( cCollection(i)%isActive )THEN
      IF( belongsToCollection(i,def) )THEN
        icol = i
        EXIT
      END IF
    END IF
  END DO
END IF

RETURN
END FUNCTION
!*******************************************************************************
! validCollectionID
!*******************************************************************************
LOGICAL FUNCTION validCollectionID( icol ) RESULT( yes )

USE cont_rel_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icol

yes = ( icol >= 1 .AND. icol <= numCollection )

RETURN
END FUNCTION
!*******************************************************************************
! c_set_tlev
!*******************************************************************************
SUBROUTINE c_set_tlev( lstart )

USE scipuff_fi
USE cont_rel_fi
USE default_fd

IMPLICIT NONE

LOGICAL, INTENT(IN) :: lstart

INTEGER idef, irel, ilev

!------ Check release puffs for time step limits

IF( numDefinition > 0 )THEN

  DO idef = 1,numDefinition
    IF( .NOT.lstart .AND. cDefinition(idef)%state == CR_EMPTY )CYCLE
    IF( cDefinition(idef)%isPool .AND. cDefinition(idef)%time == DEF_VAL_R )CYCLE     !Skip completed pool
    ilev = 0
    DO irel = 1,cDefinition(idef)%rSet%nrel
      ilev = MAX(ilev,cDefinition(idef)%rSet%rels(irel)%basePuff%idtl,cDefinition(idef)%rSet%tlev)
    END DO
    mxtlev = MAX(mxtlev,ilev)
  END DO

END IF

RETURN
END SUBROUTINE

END MODULE cont_rel_functions
