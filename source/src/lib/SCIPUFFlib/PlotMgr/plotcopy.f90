!==============================================================================

SUBROUTINE CopySrfField( Field,srfI,grdI )

USE scipuff_fi
USE sagstr_fd
USE sagdef_fd
USE plotlist_fi
USE error_fi
USE field_fd
USE PtrGrdStrItf
USE SCIPresults_fd

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT  ) :: Field
INTEGER,                INTENT( IN     ) :: srfI
INTEGER,                INTENT( OUT    ) :: grdI

TYPE( SAGgrid_str ), POINTER :: srf

CHARACTER(64) bname  ! block name
CHARACTER(70) vname  ! block name:field name
INTEGER       irv, ivar, imat, ikind, nsg, iCopy

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_FindVariableID
INTEGER, EXTERNAL :: SAG_CopyGridID
INTEGER, EXTERNAL :: output_groups

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CopySrfField'
  eMessage = 'Error creating field grid'
  GOTO 9999
END IF

srf => SAG_PtrGrdStr( srfI )    ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED( srf ) )THEN
  nError = UK_ERROR
  eRoutine = 'CopySrfField'
  eMessage = 'Error associating surface grid'
  GOTO 9999
END IF

!------ Initialize SAG structure

irv = SAG_InitGridID(' ',0,SAG_GRID_BOTH,srf%ncells,3,3,grdI)
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CopySrfField'
  eMessage = 'Error initializing SAG field grid'
  GOTO 9999
END IF

!------ Copy grid

irv = SAG_CopyGridID( srfI,grdI,SAG_COPY_GRID )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CopySrfField'
  eMessage = 'Error copying surface grid to field grid'
  GOTO 9999
END IF

imat = Field%choice

Field%units = TRIM(material(imat)%unit)

IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN

  ikind = Field%kind - is_kind(imat) + 1
  nsg   = output_groups( material(imat) )
  IF( ikind <= nsg )THEN
    CALL set_block_name( material(imat),ikind,bname )
  ELSE
    CALL set_block_name( material(imat),0,bname )
  END IF

ELSE

  CALL set_block_name( material(imat),1,bname )

END IF

vname = TRIM(bname)//':'//'Mean'

irv = SAG_FindVariableID( srfI,vname,ivar )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'CopySrfField'
  eMessage = 'Error finding field name '//TRIM(vname)
  GOTO 9999
END IF

!------ copy specified fields

iCopy = SAG_COPY_FIELD + 1*SAG_TO_FIELD + ivar

irv = SAG_CopyGridID( srfI,grdI,iCopy )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CopySrfField'
  eInform = 'Error copying surface mean to plot field'
  GOTO 9999
END IF

iCopy = SAG_COPY_FIELD + 2*SAG_TO_FIELD + (ivar+1)

irv = SAG_CopyGridID( srfI,grdI,iCopy )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CopySrfField'
  eInform = 'Error copying surface variance to plot field'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
