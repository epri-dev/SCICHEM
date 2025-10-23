!*******************************************************************************
!                SAG_OpenID
!*******************************************************************************
INTEGER FUNCTION SAG_OpenID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Open an existing surface grid file

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_Open( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Open
END INTERFACE

SAG_OpenID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_OpenID = SAG_Open( grd )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_Open( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Open an existing surface grid file

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

LOGICAL lOpen
INTEGER irec, nblk, iver, ios, i, naux

CHARACTER(13) file_type

SAG_Open = SAG_ERROR

IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit is already opened

INQUIRE( UNIT=grd%nunit,OPENED=lOpen )
IF( lOpen )THEN
  LastError = SAG_ERR_OPENED
  LastParm  = grd%nunit
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE( FILE=grd%file,OPENED=lOpen )
IF( lOpen )THEN
  LastError = SAG_ERR_OPENED
  LastParm  = 0
  GOTO 9999
END IF

!==== Open file

OPEN( UNIT=grd%nunit,FILE=grd%file,STATUS='OLD',ACCESS='DIRECT', &
      RECL=SAG_RECL*4,IOSTAT=ios )

!==== Check results

IF( ios /= 0 )THEN
  LastError = SAG_ERR_OPEN
  LastParm  = ios
  GOTO 9999
ELSE
  irec = 1
  READ(grd%nunit,rec=irec,IOSTAT=ios) file_type, nblk, iver
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_OPEN
    LastParm  = ios
    GOTO 9999
  END IF
  grd%version = iver
END IF

IF( file_type == 'BLOCK_VERSION' )THEN
  grd%ftype   = nblk
  grd%nblk    = nblk
  ALLOCATE( grd%ipblk(nblk),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = ios
    GOTO 9999
  END IF
  naux = 0
  DO i = 1,nblk
    irec = irec + 1
    READ(grd%nunit,REC=irec,IOSTAT=ios)grd%ipblk(i)%name,grd%ipblk(i)%ifld, &
                                       grd%ipblk(i)%nfld,grd%ipblk(i)%iaux
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_OPEN
      LastParm  = ios
      GOTO 9999
    END IF
    IF( grd%ipblk(i)%iaux > 0 )naux = naux + 1
    IF( grd%ipblk(i)%nfld > 0 )THEN
      ALLOCATE( grd%ipblk(i)%fldnam(grd%ipblk(i)%nfld),STAT=ios )
      IF(ios /= 0)THEN
        LastError = SAG_ERR_MALLOC
        LastParm  = nblk
        GOTO 9999
      END IF
    ELSE
      NULLIFY(grd%ipblk(i)%fldnam)
    END IF
  END DO
  grd%naux = naux
  IF( naux > 0 )THEN
    ALLOCATE( grd%aux(naux),STAT=ios )
    IF(ios /= 0)THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = nblk
      GOTO 9999
    END IF
    DO i = 1,naux
      grd%aux(i)%alloc = .FALSE.
      NULLIFY( grd%aux(i)%srf_data )
    END DO
  ELSE
    NULLIFY( grd%aux )
  END IF
  irec = irec + 1
  READ(grd%nunit,REC=irec,IOSTAT=ios) grd%nvart
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_OPEN
    LastParm  = ios
    GOTO 9999
  END IF
  irec = irec + 1
  IF( grd%nvart >= SAG_RECL )irec = irec + (grd%nvart-SAG_RECL)/SAG_RECL + 1
ELSE
  grd%ftype  = 0
  grd%nblk   = 0
END IF

grd%record = irec
grd%status = SAG_HEAD_REC

SAG_Open = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_Close
!*******************************************************************************
INTEGER FUNCTION SAG_CloseID( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Close an existing surface grid file

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER  :: grd

INTERFACE
  INTEGER FUNCTION SAG_Close( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_Close
END INTERFACE

SAG_CloseID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CloseID = SAG_Close( grd )

9999 CONTINUE

RETURN
END

!================================================================

INTEGER FUNCTION SAG_Close( grd )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi

!     Close an existing surface grid file

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER ios

SAG_Close = SAG_ERROR
IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Close file

CLOSE( UNIT=grd%nunit,IOSTAT=ios )

SAG_Close  = SAG_OK
grd%record = 0
grd%status = SAG_CLOSED

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_Create
!*******************************************************************************
INTEGER FUNCTION SAG_Create( grdI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

!     Open a new surface grid file

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER  :: grd

LOGICAL lOpen
INTEGER irec,nblk,iver,i,ios,j

CHARACTER(13) file_type

INTEGER, EXTERNAL :: SAG_WriteEOFID

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_Create = SAG_ERROR
IF( grd%status == SAG_TEMP )THEN
  LastError = SAG_ERR_TEMP
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if unit is already opened

INQUIRE( UNIT=grd%nunit,OPENED=lOpen )
IF( lOpen )THEN
  LastError = SAG_ERR_OPENED
  LastParm  = grd%nunit
  GOTO 9999
END IF

!==== Check to see if file is already opened

INQUIRE( FILE=grd%file,OPENED=lOpen )
IF( lOpen )THEN
  LastError = SAG_ERR_OPENED
  LastParm  = 0
  GOTO 9999
END IF

!==== Check to see if file already exists

INQUIRE( FILE=grd%file,EXIST=lOpen )
IF( lOpen )THEN
  LastError = SAG_ERR_OPENED
  LastParm  = 0
  GOTO 9999
END IF

!==== Open file

OPEN( UNIT=grd%nunit,FILE=grd%file,STATUS='NEW',ACCESS='DIRECT', &
      RECL=SAG_RECL*4,IOSTAT=ios)

!==== Check results

IF( ios /= 0 )THEN
  LastError = SAG_ERR_OPEN
  LastParm  = ios
  GOTO 9999
ELSE
  irec = 1
END IF

!==== Write Block version header

IF( grd%ftype > 0 )THEN

!====== Check name pointers

  IF( .NOT.ASSOCIATED(grd%ipnam) )THEN
    LastError = SAG_ERR_IPNAM
    LastParm  = 0
    GOTO 9999
  END IF

  IF( .NOT.ASSOCIATED(grd%ipblk) )THEN
    LastError = SAG_ERR_IPBLK
    LastParm  = 0
    GOTO 9999
  END IF

  file_type = 'BLOCK_VERSION'
  nblk      = grd%nblk
  iver      = grd%version
  WRITE(grd%nunit,REC=irec,IOSTAT=ios) file_type, nblk, iver
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_WRITE
    LastParm  = ios
    GOTO 9999
  END IF

  DO i = 1,grd%nblk

    irec = irec + 1
    WRITE(grd%nunit,REC=irec,IOSTAT=ios)grd%ipblk(i)%name,grd%ipblk(i)%ifld, &
                                        grd%ipblk(i)%nfld,grd%ipblk(i)%iaux
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_WRITE
      LastParm  = ios
      GOTO 9999
    END IF

  END DO

  irec = irec + 1
  WRITE(grd%nunit,REC=irec,IOSTAT=ios)grd%nvart, &
                                     (grd%ipnam(i),i=1,MIN(grd%nvart,SAG_RECL-1))
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_WRITE
    LastParm  = ios
    GOTO 9999
  END IF
  IF( grd%nvart >= SAG_RECL )THEN
    j = SAG_RECL - 1
    DO WHILE( j < grd%nvart )
      irec = irec + 1
      WRITE(grd%nunit,REC=irec,IOSTAT=ios) (grd%ipnam(i),i=j+1,MIN(grd%nvart,j+SAG_RECL))
      IF( ios /= 0 )THEN
        LastError = SAG_ERR_WRITE
        LastParm  = ios
        GOTO 9999
      END IF
      j = j + SAG_RECL
    END DO
  END IF

  irec = irec + 1
END IF

grd%record = irec
grd%status = SAG_HEAD_REC

SAG_Create = SAG_WriteEOFID( grdI )

9999 CONTINUE

RETURN
END

