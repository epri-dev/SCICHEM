!*******************************************************************************
!            Initialize Audit Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_audit( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi

! This routine initializes the audit Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db  !Dialog handle
INTEGER             , INTENT( IN ) ::  id_level !Data level

CHARACTER(128), EXTERNAL :: AddNull

INTEGER irv
TYPE( CMD ) MyCmd !Command Structure

string1 = 'Anonymous'
dbtext(1,id_level) = AddNull( string1 )
CALL SetEditTs(iwnd_db,dbtext(1,id_level),1,1)

CALL ClearList( iwnd_db,IDB_COMBO1 )

string1 = 'UNCLASSIFIED'
CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv )

string2 = 'CONFIDENTIAL'
CALL AddList( iwnd_db,IDB_COMBO1,-1,string2,irv )

string2 = 'SECRET'
CALL AddList( iwnd_db,IDB_COMBO1,-1,string2,irv )

string2 = 'Other'
CALL AddList( iwnd_db,IDB_COMBO1,-1,string2,irv )

CALL SetListSelString( iwnd_db,IDB_COMBO1,string1,irv )

MyCmd%id     = IDB_AUDIT
MyCmd%cntrl  = IDB_COMBO1
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

RETURN
END

!*******************************************************************************
!            Save Audit Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_audit( id_level )

USE resource_fd
USE pcscipuf_fi

! This routine saves the audit Dialog Box

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

CHARACTER(128), EXTERNAL :: StripNull

project(BASE_LEVEL)%audit%Analyst        = StripNull( dbtext(1,id_level) )
project(BASE_LEVEL)%audit%Classification = StripNull( dbcmbo(1,id_level) )

IF( project(BASE_LEVEL)%audit%Classification == 'Other' )THEN
  project(BASE_LEVEL)%audit%Classification = StripNull( dbtext(2,id_level) )
END IF

RETURN
END
