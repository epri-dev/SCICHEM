!*******************************************************************************
!            Initialize Release Definition Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_reldef( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE GUItool_fi
USE create_fi
USE SwinAPI
!
!     This routine initializes the release definition Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER enable(3)
LOGICAL ledit

!==== BASIC Dialog Box

ledit = project(EDIT_LEVEL_2)%Edit .AND. .NOT.project(EDIT_LEVEL_2)%Restart

!     Buttons

IF( ledit )THEN
  enable(1) = TRUE !OK
  enable(2) = TRUE !Load
  enable(3) = TRUE !Clear all
ELSE
  enable(1) = FALSE !OK
  enable(2) = FALSE !Load
  enable(3) = FALSE !Clear all
END IF

CALL EnableButtons( iwnd_db,enable,13,3 )

IF( .NOT.ledit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON13,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON14,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON15,SW_HIDE )
END IF

CALL SetControlFont( iwnd_db,IDB_COMBO1,fixfont )

!==== Set Up Releases - copy to working array

iwnd_dbr = iwnd_db

!==== Release

id_rel = MIN(1,scenario(EDIT_LEVEL_2)%nrel)
CALL init_dialog_releases( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Initialize Release Definition Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_releases( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE create_fi
USE dialog_fi
USE param_fd
!
!     This routine initializes the release definition Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv

TYPE( CMD ) MyCmd !Command Structure

!==== Release

!     Set Coordinate labels

SELECT CASE( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord )
  CASE(I_LATLON )
    CALL SetControlText( iwnd_db,IDB_STATIC21,'Lon (E):' )
    CALL SetControlText( iwnd_db,IDB_STATIC20,'Lat (N):' )

  CASE( I_UTM )
    CALL SetControlText( iwnd_db,IDB_STATIC21,'M. N''ing:' )
    CALL SetControlText( iwnd_db,IDB_STATIC20,'Easting :'  )

  CASE DEFAULT
    CALL SetControlText( iwnd_db,IDB_STATIC20,'X :' )
    CALL SetControlText( iwnd_db,IDB_STATIC21,'Y :' )

END SELECT

!     Clear Release Description

CALL clear_release_edit( iwnd_db,id_level )

!     Combo Boxes - build release list - select first in list

CALL build_release_list( iwnd_db,2,IDB_COMBO1 )

!     Make current selection

IF( scenario(EDIT_LEVEL_2)%nrel > 0 )THEN
  CALL SetListSelString( iwnd_db,IDB_COMBO1,scenario(EDIT_LEVEL_2)%release(id_rel)%string,irv )
  MyCmd%id     = IDB_RELDEF
  MyCmd%cntrl  = IDB_COMBO1
  MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
  MyCmd%button = MyCmd%cntrl - COMBO_BASE
  MyCmd%level  = id_level
  CALL process_combo( iwnd_db,MyCmd )
ELSE
  id_rel = NOT_SET_I
  dbcmbo(1,id_level)  = ' '
  idbcmbo(1,id_level) = 0
END IF

!     Release Buttons - enable/disable buttons

CALL update_release_buttons( iwnd_db )

RETURN
END
!*******************************************************************************
!            Save Release Definition Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_reldef()

USE create_fi
!
!     This routine saves the release definition Dialog Box Parameters
!
IMPLICIT NONE

!---- Turn Off file copy

iwnd_dbr = 0

RETURN
END
!*******************************************************************************
!            Load Releases list into ComboBox
!*******************************************************************************
SUBROUTINE build_release_list( iwnd,ilev,icombo )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE create_fi
!
!     This routine initializes the Releases list ComboBox
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog Handle
INTEGER,              INTENT( IN ) :: ilev !Data level ID
INTEGER,              INTENT( IN ) :: icombo !Combo Box ID

INTEGER ictrl, i, irv, id, jlev
LOGICAL interact

!---- Initialize

ictrl = icombo - COMBO_BASE

!---- Clear List

CALL ClearList( iwnd,icombo )

!---- Load release names into List

CALL FindHwndList( iwnd,id,jlev ) !  Find Dialog ID from List

interact = (id /= IDB_RELNEW)

IF( scenario(ilev)%nrel > 0 )THEN
  DO i = 1,scenario(ilev)%nrel
    IF( interact .OR. scenario(ilev)%release(i)%type(1:1) /= 'X' )THEN
      WRITE(scenario(ilev)%release(i)%string(2:5),'(I4.4)')i-1
      CALL AddList( iwnd,icombo,-999,scenario(ilev)%release(i)%string,irv )
    END IF
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Load Size Bin list into ComboBox
!*******************************************************************************
SUBROUTINE build_group_list( iwnd,ilev,icombo,id )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
USE SwinAPI
!
!     This routine initializes the Releases list ComboBox
!
IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Dialog Handle
INTEGER              ilev !Data level ID
INTEGER              icombo !Combo Box ID
INTEGER              id

INTEGER ictrl, i, irv, n, nsg, iaux, nskp, ioff, idist, max
REAL    x

CHARACTER(32) number

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

!---- Initialize

ictrl = icombo - COMBO_BASE

!---- Clear List

CALL ClearList( iwnd,icombo )

IF( id <= 0 )RETURN

!---- Check for groups

IF( IsGas(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
  CALL EnableControl( iwnd,icombo,FALSE )
  CALL ShowControl( iwnd,icombo,SW_HIDE )
  CALL ShowControl( iwnd,IDB_STATIC31,SW_HIDE )
  idbcmbo(ictrl,ilev) = 1
  dbint(ictrl,ilev)   = -1
ELSE IF( IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
  CALL EnableControl( iwnd,icombo,FALSE )
  CALL ShowControl( iwnd,icombo,SW_HIDE )
  CALL ShowControl( iwnd,IDB_STATIC31,SW_HIDE )
  idbcmbo(ictrl,ilev) = 1
  dbint(ictrl,ilev)   = -1
ELSE

  CALL EnableControl( iwnd,icombo,TRUE )
  CALL ShowControl( iwnd,icombo,SW_SHOWNORMAL )
  CALL ShowControl( iwnd,IDB_STATIC31,SW_SHOWNORMAL )

!---- Load Material bins into List

  string4 = ' '
  iaux  = materials(EDIT_LEVEL_1)%material(id)%iaux
  idist = dbint(ictrl,ilev)
  IF( IsParticle(materials(EDIT_LEVEL_1)%material(id)%icls) &
     .OR. IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    nskp = MAXPMAUX
    ioff = PMAUX_BOUNDS
  ELSE IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    iaux = iaux + MAXGMAUX + MAXLMAUXP
    nskp = MAXLMAUX
    ioff = LMAUX_BOUNDS
  END IF
  nsg  = NINT(materials(EDIT_LEVEL_1)%mat_aux(iaux))
  iaux = iaux + ioff
  IF( nsg > 0 )THEN
    DO i = 1,MAXDISTRIBUTION
      SELECT CASE( i )

        CASE( REL_LOGNORM )
          string1 = 'Log Normal'

        CASE DEFAULT
          string1 = 'Oops - Should never get here'

      END SELECT

      CALL AddList( iwnd,icombo,-1,string1,irv )
      IF( i == idist )string4 = string1
    END DO
    IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      IF( cur_release%type(1:1) == 'I' )THEN
        max = REL_GASPHASE
      ELSE IF( cur_release%type(1:1) == 'X' )THEN
        max = REL_GASPHASE
      ELSE
        IF( cur_release%type(2:2) == 'M' )THEN !disallow moving pools
          max = REL_GASPHASE
        ELSE IF( cur_release%type(2:2) == 'S' )THEN !disallow stack pools
          max = REL_GASPHASE
        ELSE
          max = MAXSPECIALLIQUID
        END IF
      END IF
      DO i = 1,max
        SELECT CASE( i )

          CASE( REL_GASPHASE )
            string1 = 'Gas phase'

          CASE( REL_LIQUIDPOOL )
            string1 = 'Liquid pool'

          CASE DEFAULT
            string1 = 'Oops - Should never get here'

        END SELECT

        CALL AddList( iwnd,icombo,-1,string1,irv )
        IF( MAXDISTRIBUTION+i == idist )string4 = string1
      END DO
    ELSE IF( IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      DO i = 1,MAXSPECIALWETP
        SELECT CASE( i )

          CASE( REL_SLURRY )
            string1 = 'Slurry'

          CASE DEFAULT
            string1 = 'Oops - Should never get here'

        END SELECT

        CALL AddList( iwnd,icombo,-1,string1,irv )
        IF( MAXDISTRIBUTION+i == idist )string4 = string1
      END DO
    END IF
    DO i = 1,nsg
      x = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+(i-1)*nskp)
      CALL c_format( x,n,number )
      string3 = ADJUSTL(number(1:n))//'-'
      x = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+i*nskp)
      CALL c_format( x,n,number )
      string2 = ADJUSTL(number(1:n))//' microns'
      string1 = TRIM(string3)//TRIM(string2)
      CALL AddList( iwnd,icombo,-1,string1,irv )
      IF( i == idbcmbo(ictrl,ilev) )string4 = string1
    END DO
    IF( string4 /= ' ' )THEN
      CALL SetListSelString( iwnd,icombo,string4,irv )
      IF( irv > 0 )THEN
        CALL ListCount( iwnd,icombo,i )
        n = i - nsg
        IF( irv > n )THEN
          idbcmbo(ictrl,ilev) = irv - n
        ELSE
          idbcmbo(ictrl,ilev) = irv + nsg
        END IF
        dbint(ictrl,ilev) = idbcmbo(ictrl,ilev) - nsg
        CALL show_distribution( iwnd,dbint(ictrl,ilev),dbint(2,ilev) )
      ELSE
        idbcmbo(ictrl,ilev) = NOT_SET_I
        dbint(ictrl,ilev)   = NOT_SET_I
        dbcmbo(ictrl,ilev)  = ' '
      END IF
    ELSE
      idbcmbo(ictrl,ilev) = NOT_SET_I
      dbint(ictrl,ilev)   = NOT_SET_I
      dbcmbo(ictrl,ilev)  = ' '
    END IF
  END IF

END IF

RETURN
END
!*******************************************************************************
!            Copy Selection into Current Releases
!*******************************************************************************
SUBROUTINE load_current_release( id )

USE reldef_fd
USE relparam_fd
USE create_fi
USE GUIparam_fd

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: id

id = MIN(id,scenario(EDIT_LEVEL_2)%nrel)

IF( id > 0 )THEN
  CALL CopyRelease( scenario(EDIT_LEVEL_2)%release(id),cur_release )
END IF

id_rel = id

RETURN
END
!*******************************************************************************
!            Delete Current Releasess
!*******************************************************************************
SUBROUTINE delete_current_release( id )

USE reldef_fd
USE relparam_fd
USE create_fi
USE errorParam_fd
USE GUIparam_fd

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: id

INTEGER i
INTEGER ios

IF( ASSOCIATED(scenario(EDIT_LEVEL_2)%release(id)%mc) )THEN
  DEALLOCATE( scenario(EDIT_LEVEL_2)%release(id)%mc,STAT=ios )
  NULLIFY( scenario(EDIT_LEVEL_2)%release(id)%mc )
END IF

DO i = id,scenario(EDIT_LEVEL_2)%nrel-1
 CALL CopyRelease( scenario(EDIT_LEVEL_2)%release(i+1),scenario(EDIT_LEVEL_2)%release(i) )
END DO

scenario(EDIT_LEVEL_2)%nrel = scenario(EDIT_LEVEL_2)%nrel - 1

id = MIN(id,scenario(EDIT_LEVEL_2)%nrel)

RETURN
END
!*******************************************************************************
!            Add Current Release
!*******************************************************************************
SUBROUTINE add_current_release( rel,rel_list,nl,id )

USE create_fi

!     This routine loads the selection into current release

IMPLICIT NONE

TYPE( release_str ),               INTENT( IN    ) :: rel
TYPE( release_str ), DIMENSION(*), INTENT( INOUT ) :: rel_list
INTEGER,                           INTENT( INOUT ) :: nl
INTEGER,                           INTENT( OUT   ) :: id

INTEGER i, ipos

IF( nl <= 0 )THEN
  ipos = 1
ELSE
  IF( rel%time < rel_list(1)%time )THEN
    ipos = 1
  ELSE IF( rel%time >= rel_list(nl)%time )THEN
    ipos = nl + 1
  ELSE
    ipos = 0
    i    = 0
    DO WHILE( ipos == 0 .AND. i <= nl )
      i = i + 1
      IF( rel%time < rel_list(i)%time )ipos = i
    END DO
  END IF
END IF

IF( ipos > nl )THEN
  nl = nl + 1
  CALL CopyRelease( rel,rel_list(nl) )
  id = nl
ELSE
  DO i = nl,ipos,-1
    CALL CopyRelease( rel_list(i),rel_list(i+1) )
  END DO
  CALL CopyRelease( rel,rel_list(ipos) )
  nl = nl + 1
  id = ipos
END IF

RETURN
END
!*******************************************************************************
!            Save Selection from Current Releasess
!*******************************************************************************
SUBROUTINE save_current_release( iwnd_db,id_level )

USE pcscipuf_fi
USE create_fi
USE GUItool_fi
USE SwinAPI

!     This routine saves the selection from current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: id_level

IF( id_rel <= scenario(EDIT_LEVEL_2)%nrel )THEN
  CALL delete_current_release( id_rel )
END IF

CALL add_current_release( cur_release,scenario(EDIT_LEVEL_2)%release, &
                                     scenario(EDIT_LEVEL_2)%nrel,id_rel )
CALL init_dialog_releases( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!                     Initialize Release Description
!*******************************************************************************
SUBROUTINE init_release_edit( iwnd_db,ilev )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUItool_fi
USE GUImatl_fi
USE files_fi
USE dialog_fi
USE SwinAPI
USE errorParam_fd

!     This routine initializes the release description part of the
!     Release Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN ) :: ilev    !Data level ID

TYPE( CMD ) MyCmd !Command Structure

INTEGER irel, irv, id, nr, i
REAL    tfac, rfac, dfac
INTEGER indxt, indxr, indxd, Real2Int
INTEGER nmc, ios

REAL, DIMENSION(:), ALLOCATABLE :: tmpMC

LOGICAL, EXTERNAL :: hasError
CHARACTER(128), EXTERNAL :: AddNull

LOGICAL, EXTERNAL :: IsGas
LOGICAL flashing
LOGICAL, EXTERNAL :: IsAerosol

!     Enable/Show controls

CALL clear_release_edit( iwnd_db,ilev )

releaseEnabled = FALSE

IF( id_rel <= 0 )RETURN

!     Units combo boxes

idbcmbo(5,ilev) = 1
CALL build_release_Lunit( iwnd_db,IDB_COMBO5,ilev )

!     Release Type

flashing = .FALSE.

SELECT CASE( cur_release%type(1:1) )

  CASE( 'C' )
    SELECT CASE( cur_release%type(2:2) )

      CASE( 'M' )
        irel = 2
        lcheck(1,ilev) = .FALSE.
        lcheck(4,ilev) = .FALSE.
        lcheck(3,ilev) = .FALSE.
        CALL show_moving_edit( iwnd_db,ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        CALL SetControlText( iwnd_db,IDB_STATIC40,'Rate :' )
        string2 = 'Moving'

      CASE( 'S' )
        irel = 4
        lcheck(1,ilev) = .FALSE.
        lcheck(4,ilev) = .FALSE.
        lcheck(3,ilev) = .TRUE.
        CALL show_stack_edit( iwnd_db,ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        CALL SetControlText( iwnd_db,IDB_STATIC40,'Rate :' )
        string2 = 'Stack'

      CASE( 'P' )
        irel = 1
        lcheck(1,ilev) = .FALSE.
        lcheck(4,ilev) = .TRUE.
        lcheck(3,ilev) = .FALSE.
        CALL show_continuous_edit( iwnd_db,ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        string2 = 'Pool'

      CASE DEFAULT
        irel = 1
        lcheck(1,ilev) = .FALSE.
        lcheck(4,ilev) = .FALSE.
        lcheck(3,ilev) = .FALSE.
        CALL show_continuous_edit( iwnd_db,ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        string2 = 'Continuous'

    END SELECT

  CASE( 'I' )
    SELECT CASE( cur_release%type(2:2) )

      CASE( 'A' )
        flashing = .TRUE.
        irel = 5
        lcheck(1,ilev) = cur_release%param(REL_RAND_INDX) /= NOT_SET_R
        lcheck(3,ilev) = .FALSE.
        lcheck(4,ilev) = .FALSE.
        CALL show_flash_edit( iwnd_db,cur_release%spec,lcheck(1,ilev),ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        IF( lcheck(1,ilev) )THEN
          CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass/location :' )
        ELSE
          CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass :' )
        END IF
        string2 = 'Flashing Aerosol'

      CASE DEFAULT
        irel = 3
        lcheck(1,ilev) = cur_release%param(REL_RAND_INDX) /= NOT_SET_R
        lcheck(3,ilev) = .FALSE.
        lcheck(4,ilev) = .FALSE.
        CALL show_instantaneous_edit( iwnd_db,cur_release%spec,lcheck(1,ilev),ilev )
        CALL show_liquid_pool( iwnd_db,lcheck(4,ilev),lcheck(3,ilev),ilev,FALSE )
        IF( lcheck(1,ilev) )THEN
          CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass/location :' )
        ELSE
          CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass :' )
        END IF
        string2 = 'Instantaneous'

    END SELECT

  CASE DEFAULT
    irel = 0
    string2 = 'Erroneous'

END SELECT

!     Release ID

string1 = TRIM(string2)//' source description'
CALL SetControlText( iwnd_db,IDB_STATIC11,string1 )
IF( irel == 6 )THEN
  DO i = 11,11
    CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
  END DO
  RETURN
END IF

!     Edit Boxes - Reals

IF( cur_release%time == NOT_SET_R )THEN
  tfac  = 1.
  indxt = 3
ELSE IF( cur_release%time == 0. .OR. cur_release%time >= 1. )THEN
  tfac  = 1.
  indxt = 3
ELSE IF( cur_release%time >= 1./60. )THEN
  tfac  = 60.
  indxt = 2
ELSE
  tfac  = 3600.
  indxt = 1
END IF

dbreal( 1,ilev) = cur_release%time*tfac
idbcmbo(4,ilev) = indxt
CALL build_release_Tunit( iwnd_db,IDB_COMBO4,ilev )

IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
  dbreal8(2,ilev) = cur_release%yRel
  dbreal8(3,ilev) = cur_release%xRel
  CALL compute_DMS8( dbreal8(2,ilev),dbint(4,ilev),dbint(5,ilev),dbreal(15,ilev) )
  CALL compute_DMS8( dbreal8(3,ilev),dbint(6,ilev),dbint(7,ilev),dbreal(16,ilev) )
ELSE
  dbreal8(2,ilev)  = cur_release%xRel
  dbreal8(3,ilev)  = cur_release%yRel
  dbint(4,ilev)   = NOT_SET_I
  dbint(5,ilev)   = NOT_SET_I
  dbreal(15,ilev) = NOT_SET_R
  dbint(6,ilev)   = NOT_SET_I
  dbint(7,ilev)   = NOT_SET_I
  dbreal(16,ilev) = NOT_SET_R
END IF

dbreal(4,ilev) = cur_release%zRel

IF( irel /= 3 .AND. irel /= 5 )THEN

  IF( cur_release%dur == NOT_SET_R .OR. cur_release%dur == DEFERRED_R )THEN
    dfac  = 1.
    indxd = 3
  ELSE IF( cur_release%dur == 0. .OR. cur_release%dur >= 1. )THEN
    dfac  = 1.
    indxd = 3
  ELSE IF( cur_release%dur >= 1./60. )THEN
    dfac  = 60.
    indxd = 2
  ELSE
    dfac  = 3600.
    indxd = 1
  END IF

  dbreal( 6,ilev) = cur_release%dur*dfac
  idbcmbo(7,ilev) = indxd
  CALL build_release_Tunit( iwnd_db,IDB_COMBO7,ilev )

END IF

IF( irel == 3 .OR. irel == 5 .OR. lcheck(4,ilev) )THEN
  rfac  = 1.
  indxr = 1
ELSE
  IF( cur_release%rate == NOT_SET_R .OR. cur_release%rate == DEFERRED_R )THEN
    indxr = 1
  ELSE
    indxr = indxd
  END IF
  SELECT CASE( indxr )

    CASE( 1 )
      rfac = 1.

    CASE( 2 )
      rfac = 60.

    CASE( 3 )
      rfac = 3600.

  END SELECT
END IF

dbreal( 5,ilev) = cur_release%rate*rfac
dbreal( 7,ilev) = cur_release%sig(1)
dbreal( 8,ilev) = cur_release%sig(2)
dbreal( 9,ilev) = cur_release%sig(3)
dbreal(10,ilev) = cur_release%vel(1)
dbreal(11,ilev) = cur_release%vel(2)
dbreal(12,ilev) = cur_release%vel(3)
dbreal(13,ilev) = cur_release%param(REL_MMD_INDX)
dbreal(14,ilev) = cur_release%param(REL_SIGMA_INDX)
dbreal(23,ilev) = cur_release%param(REL_WMFRAC_INDX)
dbreal(26,ilev) = cur_release%param(REL_AFRAC_INDX)

IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  nr = 20
  dbreal(18,ilev) = cur_release%dynam(1)
  dbreal(17,ilev) = cur_release%dynam(2)
  dbreal(19,ilev) = cur_release%dynam(3)
  dbreal(20,ilev) = cur_release%dynam(4)
ELSE
  nr = 16
END IF

CALL SetEditRs( iwnd_db,dbreal(1, ilev),1,1 )
CALL SetEditR8s( iwnd_db,dbreal8(2, ilev),2,2 )
CALL SetEditRs( iwnd_db,dbreal(4, ilev),4,nr-3 )
CALL SetEditRs( iwnd_db,dbreal(23,ilev),23,1 )
CALL SetEditRs( iwnd_db,dbreal(26,ilev),26,1 )
CALL SetEditIs( iwnd_db,dbint (4, ilev),4, 4 )

CALL show_release_Lunit( iwnd_db,idbcmbo(5,ilev) )

!     Check Boxes

CALL SetChecks( iwnd_db,lcheck(1,ilev),1,2 )

IF( lcheck(1,ilev) )THEN
  dbreal(24,ilev) = cur_release%param(REL_RAND_INDX)
  dbreal(25,ilev) = cur_release%param(REL_SPREAD_INDX)
  dbint(8,ilev)   = Real2Int( cur_release%param(REL_SEED_INDX) )
ELSE
  dbreal(24,ilev) = NOT_SET_R
  dbreal(25,ilev) = NOT_SET_R
  CALL new_seed( dbint(8,ilev),ran_seed )
END IF

dbreal(21,ilev) = NOT_SET_R
dbreal(22,ilev) = NOT_SET_R

!     Edit Boxes - Text

dbtext(1,ilev) = AddNull( cur_release%type )
dbtext(4,ilev) = TRIM(cur_release%string(7:))
CALL SetEditTs( iwnd_db,dbtext(4,ilev),4,1 )

!     Radio Buttons

IF( irel == 3 )THEN
  nradio(1,ilev) = 2
ELSE
  nradio(1,ilev) = 1
END IF

ichoice(1,ilev) = cur_release%spec
CALL SetRadios( iwnd_db,ichoice(1,ilev),nradio(1,ilev),1,1 )

nradio(2,ilev) = 2
IF( cur_release%defName )THEN
  ichoice(2,ilev) = 1
ELSE
  ichoice(2,ilev) = 2
END IF
CALL SetRadios( iwnd_db,ichoice(2,ilev),nradio(2,ilev),2,1 )

IF( flashing )THEN
  CALL build_aerosol_material_list( iwnd_db,IDB_COMBO2, &
                            materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl )
ELSE
  CALL build_material_list( iwnd_db,IDB_COMBO2, &
                            materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl )
END IF

CALL ClearList( iwnd_db,IDB_COMBO3 )

IF( cur_release%spec == REL_DATA .AND. irel /= 6 )THEN

  dbtext(2,ilev) = ' '
  dbtext(3,ilev) = ' '
  CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl &
                                       ,cur_release%matl,id )
  dbint(16,ilev) = materials(EDIT_LEVEL_1)%material(id)%nmc

  IF( dbint(16,ilev) /= 0 )THEN

    nmc = dbint(16,ilev)

    IF( nmc > SIZE(dblst(ilev,:)) )THEN
      CALL ReallocateMaxList( nmc )
      IF( hasError() )GOTO 1234
    END IF

    IF( ASSOCIATED(cur_release%mc) )THEN

      IF( nmc > SIZE(cur_release%mc) )THEN
        ALLOCATE( tmpMC(SIZE(cur_release%mc)),STAT=ios )
        IF( ios /= 0 )THEN
          WRITE(string1,*)'Size =',nmc
          CALL SetError( SZ_ERROR,'Allocation Error:Allocating tmpMC',string1,' ','InitReleaseEdit' )
          GOTO 1234
        END IF

        tmpMC = cur_release%mc

        DEALLOCATE( cur_release%mc,STAT=ios )
        ALLOCATE( cur_release%mc(nmc),STAT=ios )
        IF( ios /= 0 )THEN
          WRITE(string1,*)'Size =',nmc
          CALL SetError( SZ_ERROR,'Allocation Error:Reallocating mc',string1,' ','InitReleaseEdit' )
          GOTO 1234
        END IF

        DO i = 1,SIZE(tmpMC)
          cur_release%mc(i) = tmpMC(i)
        END DO

        DO i = SIZE(tmpMC)+1,nmc
          cur_release%mc(i) = 0.0
        END DO
      END IF

    ELSE

      ALLOCATE( cur_release%mc(nmc),STAT=ios )
      IF( ios /= 0 )THEN
        WRITE(string1,*)'Size =',nmc
        CALL SetError( SZ_ERROR,'Allocation Error:Allocating mc',string1,' ','InitReleaseEdit' )
        GOTO 1234
      END IF

      DO i = 1,nmc
        cur_release%mc(i) = 0.0
      END DO

    END IF

    DO i = 1,nmc
      dblst(ilev,i) = cur_release%mc(i)
    END DO

1234 CONTINUE

    IF( hasError() )THEN
      CALL AddErrorAction( 'Multicomponent setting disabled' )
      CALL ShowErrorMessage( iwnd_db )
      dbint(16,ilev) = 0
    END IF

  END IF

  IF( id > 0 )THEN
    CALL SetListSelString( iwnd_db,IDB_COMBO2,cur_release%matl,irv )
    idbcmbo(2,ilev) = id
    dbcmbo(2,ilev)  = AddNull( cur_release%matl )
    dbint(2,ilev)   = materials(EDIT_LEVEL_1)%material(id)%icls
    idbcmbo(3,ilev) = cur_release%indx
    dbint(3,ilev)   = cur_release%distrib
  ELSE
    dbcmbo(2,ilev)  = ' '
    idbcmbo(2,ilev) = NOT_SET_I
    dbint(2,ilev)   = NOT_SET_I
    dbcmbo(3,ilev)  = ' '
    idbcmbo(3,ilev) = NOT_SET_I
    dbint(3,ilev)   = NOT_SET_I
  END IF

  idbcmbo(6,ilev) = indxr

  MyCmd%id     = IDB_RELDEF
  MyCmd%cntrl  = IDB_COMBO2
  MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
  MyCmd%button = MyCmd%cntrl - COMBO_BASE
  MyCmd%level  = ilev
  CALL process_combo( iwnd_db,MyCmd )

  IF( .NOT.IsGas(materials(EDIT_LEVEL_1)%material(id)%icls) .AND. &
      .NOT.IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    MyCmd%id     = IDB_RELDEF
    MyCmd%cntrl  = IDB_COMBO3
    MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
    MyCmd%button = MyCmd%cntrl - COMBO_BASE
    MyCmd%level  = ilev
    CALL process_combo( iwnd_db,MyCmd )
  END IF

ELSE

  CALL SetControlText( iwnd_db,IDB_STATIC34,cur_release%file )
  CALL SetControlText( iwnd_db,IDB_STATIC35,cur_release%path )
  dbtext(2,ilev) = cur_release%file
  dbtext(3,ilev) = cur_release%path
  CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl &
                                       ,cur_release%matl,id )
  IF( id > 0 )THEN
    CALL SetListSelString( iwnd_db,IDB_COMBO2,cur_release%matl,irv )
    idbcmbo(2,ilev) = id
    dbcmbo(2,ilev)  = AddNull( cur_release%matl )
  ELSE
    dbcmbo(2,ilev)  = ' '
    idbcmbo(2,ilev) = NOT_SET_I
  END IF
  dbcmbo(3,ilev)  = ' '
  idbcmbo(3,ilev) = NOT_SET_I
  dbint(3,ilev)   = NOT_SET_I

  MyCmd%id     = IDB_RELDEF
  MyCmd%cntrl  = IDB_COMBO2
  MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
  MyCmd%button = MyCmd%cntrl - COMBO_BASE
  MyCmd%level  = ilev
  CALL process_combo( iwnd_db,MyCmd )

END IF

CALL release_spec_radio( iwnd_db,ilev,FALSE )

RETURN
END
!*******************************************************************************
!                     Save Release Description
!*******************************************************************************
SUBROUTINE save_release_edit( iwnd_db,ilev,lok )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE GUImatl_fi
USE dialog_fi

!     This routine saves the release description part of the
!     Release Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilev    !Data level ID
LOGICAL,              INTENT( OUT ) :: lok

INTEGER  irel, i, id, j
INTEGER  nmc

LOGICAL ldyn, lpool, lran

TYPE( release_str) tmp_release

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL ::  IsLiquid,IsWetParticle
LOGICAL, EXTERNAL ::  IsAerosol
CHARACTER(128), EXTERNAL ::  StripNull

!     Enable/Show controls

IF( id_rel <= 0 )RETURN

NULLIFY( tmp_release%mc )

CALL CopyRelease( cur_release,tmp_release )

!     Release Type

lpool = .FALSE.
SELECT CASE( tmp_release%type(1:1) )

  CASE( 'C' )
    SELECT CASE( tmp_release%type(2:2) )

      CASE( 'M' )
        irel = 2

      CASE( 'S' )
        irel = 4

      CASE DEFAULT
        irel = 1
        IF( lcheck(4,ilev) )THEN
          tmp_release%type(2:2) = 'P'
          lpool = .TRUE.
        ELSE
          tmp_release%type(2:2) = ' '
        END IF

    END SELECT

  CASE( 'I' )
    SELECT CASE( tmp_release%type(2:2) )

      CASE( 'A' )
        irel = 5

      CASE DEFAULT
        irel = 3

    END SELECT

  CASE DEFAULT
    irel = 0

END SELECT

IF( irel == 6 )RETURN

!     Edit Boxes - Reals

tmp_release%time = dbreal(1,ilev)

SELECT CASE( idbcmbo(4,ilev) )
  CASE( 1 )
    tmp_release%time = tmp_release%time/3600.

  CASE( 2 )
    tmp_release%time = tmp_release%time/60.

  CASE DEFAULT

END SELECT

IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN

  tmp_release%xRel = dbreal8(3,ilev)
  tmp_release%yRel = dbreal8(2,ilev)
  tmp_release%zRel = dbreal(4,ilev)

ELSE

  tmp_release%xRel = dbreal8(2,ilev)
  tmp_release%yRel = dbreal8(3,ilev)
  tmp_release%zRel = dbreal(4,ilev)

  IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_CARTESIAN )THEN

    SELECT CASE( idbcmbo(5,ilev) )
      CASE( 2 )
        tmp_release%xRel = tmp_release%xRel/1000.
        tmp_release%yRel = tmp_release%yRel/1000.

      CASE( 3 )
        tmp_release%xRel = tmp_release%xRel*0.0003048
        tmp_release%yRel = tmp_release%yRel*0.0003048

      CASE( 4 )
        tmp_release%xRel = tmp_release%xRel*1.609344
        tmp_release%yRel = tmp_release%yRel*1.609344

      CASE DEFAULT

    END SELECT

  ELSE IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_METERS )THEN

    SELECT CASE( idbcmbo(5,ilev) )
      CASE( 2 )
        tmp_release%xRel = tmp_release%xRel*0.3048
        tmp_release%yRel = tmp_release%yRel*0.3048

      CASE DEFAULT

    END SELECT

  END IF

END IF

IF( lpool )THEN
  tmp_release%zRel = NOT_SET_R
  tmp_release%param(REL_AFRAC_INDX) = NOT_SET_R
ELSE
  tmp_release%param(REL_AFRAC_INDX) = dbreal(26,ilev)
END IF

!     Check Boxes

IF( lcheck(1,ilev) )THEN
  tmp_release%param(REL_RAND_INDX)   = dbreal(24,ilev)
  tmp_release%param(REL_SPREAD_INDX) = dbreal(25,ilev)
  IF( dbint(8,ilev) == NOT_SET_I )CALL new_seed( dbint(8,ilev),ran_seed )
  tmp_release%param(REL_SEED_INDX) = FLOAT(dbint(8,ilev))
ELSE
  tmp_release%param(REL_RAND_INDX) = NOT_SET_R
  tmp_release%param(REL_SPREAD_INDX) = NOT_SET_R
  tmp_release%param(REL_SEED_INDX) = NOT_SET_R
END IF

lran = tmp_release%param(REL_RAND_INDX) /= NOT_SET_R

!     Radio Buttons

tmp_release%spec = ichoice(1,ilev)
IF( tmp_release%spec == REL_DATA )THEN
  tmp_release%rate = dbreal(5,ilev)
  IF( tmp_release%rate /= NOT_SET_R .AND. tmp_release%rate /= DEF_VAL_R )THEN
    SELECT CASE( idbcmbo(6,ilev) )
      CASE( 2 )
        tmp_release%rate = tmp_release%rate/60.

      CASE( 3 )
        tmp_release%rate = tmp_release%rate/3600.

      CASE DEFAULT

    END SELECT
  END IF

  IF( irel == 3 .OR. irel == 5 )THEN
    tmp_release%dur = NOT_SET_R
    j = 3
  ELSE
    IF( lpool )THEN
      tmp_release%dur = DEF_VAL_R
      j = 3
    ELSE
      tmp_release%dur = dbreal(6,ilev)
      IF( tmp_release%dur /= NOT_SET_R .AND. tmp_release%dur /= DEF_VAL_R )THEN
        j = idbcmbo(7,ilev)
      ELSE
        j = 3
      END IF
    END IF
  END IF

  SELECT CASE( j )

    CASE( 1 )
      tmp_release%dur = tmp_release%dur/3600.

    CASE( 2 )
      tmp_release%dur = tmp_release%dur/60.

    CASE DEFAULT

  END SELECT

  IF( irel <= 2 )THEN
    IF( lpool )THEN
      DO i = 1,2
        tmp_release%sig(i) = dbreal(i+6,ilev)
      END DO
      tmp_release%sig(3) = NOT_SET_R
    ELSE
      DO i = 2,3
        tmp_release%sig(i) = dbreal(i+6,ilev)
      END DO
      tmp_release%sig(1) = NOT_SET_R
    END IF
  ELSE IF( irel == 4 )THEN
    tmp_release%sig(1) = dbreal(7,ilev)
    tmp_release%sig(2) = NOT_SET_R
    tmp_release%sig(3) = NOT_SET_R
  ELSE
    DO i = 1,3
      tmp_release%sig(i) = dbreal(i+6,ilev)
    END DO
  END IF

  IF( irel == 2 )THEN
    DO i = 1,3
      tmp_release%vel(i) = dbreal(i+9,ilev)
    END DO
  ELSE
    DO i = 1,3
      tmp_release%vel(i) = NOT_SET_R
    END DO
  END IF

  string1 = StripNull( dbcmbo(2,ilev) )
  tmp_release%matl = TRIM(string1)
  tmp_release%indx = idbcmbo(3,ilev)
  CALL set_release_indx( tmp_release,materials(EDIT_LEVEL_1),.FALSE. )

  IF( dbint(16,ilev) /= 0 )THEN
    IF( dbint(16,ilev) > 0 )THEN
      nmc = dbint(16,ilev)
!    ELSE
!      nmc = MAXRELMC
    END IF
    DO i = 1,nmc
      tmp_release%mc(i) = dblst(ilev,i)
    END DO
  ELSE
    IF( ASSOCIATED(tmp_release%mc) )THEN
      DO i = 1,SIZE(tmp_release%mc)
        tmp_release%mc(i) = scenario(DEFAULT_LEVEL)%release(EDIT_LEVEL_1)%mc(i)
      END DO
    END IF
  END IF
  CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl, &
                           tmp_release%matl,id )
  IF( tmp_release%indx /= NOT_SET_I )THEN
    IF( IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      IF( tmp_release%distrib > 0 .AND. &
          tmp_release%distrib <= MAXDISTRIBUTION+REL_SLURRY )THEN
        tmp_release%param(REL_MMD_INDX)   = dbreal(13,ilev)
        tmp_release%param(REL_SIGMA_INDX) = dbreal(14,ilev)
        IF( tmp_release%distrib == MAXDISTRIBUTION+REL_SLURRY )THEN
          tmp_release%param(REL_WMFRAC_INDX) = dbreal(23,ilev)
        ELSE
          tmp_release%param(REL_WMFRAC_INDX) = NOT_SET_R
        END IF
      ELSE
        tmp_release%param(REL_MMD_INDX)    = NOT_SET_R
        tmp_release%param(REL_SIGMA_INDX)  = NOT_SET_R
        tmp_release%param(REL_WMFRAC_INDX) = dbreal(23,ilev)
      END IF
    ELSE
      IF( tmp_release%distrib > 0 .AND. &
          tmp_release%distrib <= MAXDISTRIBUTION )THEN
        tmp_release%param(REL_MMD_INDX) = dbreal(13,ilev)
        tmp_release%param(REL_SIGMA_INDX) = dbreal(14,ilev)
      ELSE
        tmp_release%param(REL_MMD_INDX) = NOT_SET_R
        tmp_release%param(REL_SIGMA_INDX) = NOT_SET_R
      END IF
      IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
        IF( tmp_release%distrib /= MAXDISTRIBUTION+REL_GASPHASE )THEN
          tmp_release%param(REL_WMFRAC_INDX) = dbreal(23,ilev)
        ELSE
          tmp_release%param(REL_WMFRAC_INDX) = NOT_SET_R
        END IF
      END IF
    END IF
    IF( project(EDIT_LEVEL_2)%Dynamic )THEN
      ldyn = IsGas(materials(EDIT_LEVEL_1)%material(id)%icls)
      ldyn = ldyn .OR. IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls)
      ldyn = ldyn .OR. &
      (IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) .AND. &
       tmp_release%distrib == MAXDISTRIBUTION+REL_GASPHASE .OR. &
       (tmp_release%param(REL_WMFRAC_INDX) /= NOT_SET_R .AND.  &
       tmp_release%param(REL_WMFRAC_INDX) < 1.0) )
      IF( ldyn )THEN
        tmp_release%dynam(1) = dbreal(18,ilev)
        tmp_release%dynam(2) = dbreal(17,ilev)
        tmp_release%dynam(3) = dbreal(19,ilev)
        tmp_release%dynam(4) = dbreal(20,ilev)
      ELSE
        tmp_release%dynam = 0.0 !NOT_SET_R
      END IF
    ELSE
      tmp_release%dynam = 0.0 !NOT_SET_R
    END IF
  ELSE
    tmp_release%param(REL_MMD_INDX) = NOT_SET_R
    tmp_release%param(REL_SIGMA_INDX) = NOT_SET_R
    tmp_release%dynam = 0.0 !NOT_SET_R
  END IF
  tmp_release%file = ' '
  tmp_release%path = ' '
ELSE
  tmp_release%rate = NOT_SET_R
  tmp_release%dur  = NOT_SET_R
  DO i = 1,3
    tmp_release%sig(i) = NOT_SET_R
    tmp_release%vel(i) = NOT_SET_R
  END DO
  tmp_release%param(REL_AFRAC_INDX) = NOT_SET_R
  tmp_release%file = StripNull( TRIM(dbtext(2,ilev)) )
  tmp_release%path = StripNull( TRIM(dbtext(3,ilev)) )
  string1          = StripNull( dbcmbo(2,ilev) )
  tmp_release%matl = TRIM(string1)
END IF

tmp_release%defName = ichoice(2,ilev) == 1 .OR. LEN_TRIM(dbtext(4,ilev)) <= 0
IF( tmp_release%defName )THEN
  tmp_release%string = ' '
ELSE
  tmp_release%string(7:) = TRIM(dbtext(4,ilev))
END IF

tmp_release%param(REL_NXTUPDT_INDX) = DEF_VAL_R

CALL release_string( tmp_release,materials(EDIT_LEVEL_1) )

!--- Check

CALL check_release( iwnd_db,lcheck(1,ilev),lpool,tmp_release,lok )

!--- OK replace original with new

IF( lok )CALL CopyRelease( tmp_release,cur_release )

RETURN
END
!*******************************************************************************
!                     Clear Release Description
!*******************************************************************************
SUBROUTINE clear_release_edit( iwnd_db,ilev )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE SwinAPI

!     This routine clears the release description part of the
!     Release Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN ) :: ilev    !Data level ID

INTEGER i, nr

last_edit = -1

IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  nr = 20
ELSE
  nr = 16
END IF

dbtext(1,ilev) = ' '
dbtext(2,ilev) = ' '
dbtext(3,ilev) = ' '
dbtext(4,ilev) = ' '

dbreal(1,ilev) = NOT_SET_R
dbreal8(2,ilev) = NOT_SET_D
dbreal8(3,ilev) = NOT_SET_D

DO i = 4,nr
  dbreal(i,ilev) = NOT_SET_R
END DO

DO i = 4,7
  dbint(i,ilev) = NOT_SET_I
END DO

ichoice(1,ilev) = 1
ichoice(2,ilev) = 1

DO i = 2,7
  idbcmbo(i,ilev) = NOT_SET_I
  dbcmbo(i,ilev)  = ' '
END DO

dbint(2,ilev) = NOT_SET_I
dbint(3,ilev) = NOT_SET_I

lcheck(1,ilev) = .FALSE.
lcheck(2,ilev) = .FALSE.
lcheck(3,ilev) = .FALSE.
lcheck(4,ilev) = .FALSE.
dbreal(24,ilev) = NOT_SET_R
dbreal(25,ilev) = NOT_SET_R
dbreal(26,ilev) = NOT_SET_R
dbint(8,ilev)   = NOT_SET_I

CALL EnableControl( iwnd_db,IDB_BUTTON7,FALSE   )
CALL ShowControl(   iwnd_db,IDB_BUTTON7,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE   )
CALL ShowControl(   iwnd_db,IDB_BUTTON4,SW_HIDE )
DO i = 11,12
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO
CALL EnableControl( iwnd_db,IDB_BUTTON20,FALSE   )
CALL ShowControl(   iwnd_db,IDB_BUTTON20,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_EDIT1,FALSE   )
CALL ShowControl(   iwnd_db,IDB_EDIT1,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_EDIT4,FALSE )
CALL ShowControl(   iwnd_db,IDB_EDIT4,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_CHECK1, FALSE   )
CALL ShowControl(   iwnd_db,IDB_CHECK1, SW_HIDE )
CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE   )
CALL ShowControl(   iwnd_db,IDB_BUTTON5,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_CHECK2, FALSE   )
CALL ShowControl(   iwnd_db,IDB_CHECK2, SW_HIDE )
CALL EnableControl( iwnd_db,IDB_BUTTON6,FALSE   )
CALL ShowControl(   iwnd_db,IDB_BUTTON6,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_REAL1,FALSE   )
CALL ShowControl(   iwnd_db,IDB_REAL1,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_DOUBLE2,FALSE   )
CALL ShowControl(   iwnd_db,IDB_DOUBLE2,SW_HIDE )
CALL EnableControl( iwnd_db,IDB_DOUBLE3,FALSE   )
CALL ShowControl(   iwnd_db,IDB_DOUBLE3,SW_HIDE )

DO i = 4,20
  CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,REAL_BASE+i,SW_HIDE )
END DO
DO i = 23,24
  CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,REAL_BASE+i,SW_HIDE )
END DO

DO i = 4,7
  CALL EnableControl( iwnd_db,INT_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,INT_BASE+i,SW_HIDE )
END DO

DO i = 2,7
  CALL EnableControl( iwnd_db,COMBO_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,COMBO_BASE+i,SW_HIDE )
END DO

DO i = 1,2
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 11,12
  CALL EnableControl( iwnd_db,RADIO_BASE+i,FALSE   )
  CALL ShowControl(   iwnd_db,RADIO_BASE+i,SW_HIDE )
END DO

DO i = 11,18
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 19,26
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 30,39
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 40,48
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 51,66
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO

RETURN
END
!*******************************************************************************
!            Update Release Definitions buttons
!*******************************************************************************
SUBROUTINE update_release_buttons( iwnd )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUItool_fi
USE files_fi
USE SwinAPI

!     This routine enables/disables the release buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog handle

INTEGER enable(3)
LOGICAL ledit, ltest

!--- Buttons

ledit = project(EDIT_LEVEL_2)%Edit .AND. .NOT. project(EDIT_LEVEL_2)%Restart

IF( ledit )THEN
  ltest = scenario(EDIT_LEVEL_2)%nrel > 0 .AND. id_rel > 0
  IF( ltest .AND. cur_release%type(1:1) /= 'X' )THEN
    enable(1) = TRUE !Edit
  ELSE
    enable(1) = FALSE !Edit
  END IF
  IF( ltest )THEN
    enable(3) = TRUE !Delete
  ELSE
    enable(3) = FALSE !Delete
  END IF
  enable(2) = TRUE !New
ELSE
  enable(1) = FALSE
  enable(2) = FALSE
  enable(3) = FALSE
END IF

CALL EnableButtons( iwnd,enable,1,3 )

!--- Control Buttons

IF( ledit )THEN
  enable(1) = TRUE
  enable(2) = TRUE
ELSE
  enable(1) = FALSE
  enable(2) = FALSE
END IF

CALL EnableButtons( iwnd,enable,14,2 )

IF( .NOT.ledit )THEN
  CALL ShowControl( iwnd,IDB_BUTTON1 ,SW_HIDE )
  CALL ShowControl( iwnd,IDB_BUTTON2 ,SW_HIDE )
  CALL ShowControl( iwnd,IDB_BUTTON3 ,SW_HIDE )
  CALL SetControlText( iwnd,ID_CANCEL,'&OK' )
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_2)%ID%name)//' RELEASES'
  CALL SetControlText( iwnd,IDB_STATIC01,string1 )
END IF

RETURN
END
!*******************************************************************************
!            Enable Release buttons
!*******************************************************************************
SUBROUTINE enable_release_control( iwnd,iflag )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE SwinAPI

!     This routine enables/disables the release buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Dialog handle
INTEGER,              INTENT( IN ) :: iflag !Enable/disable Flag

INTEGER enable(3)

!     Release Buttons

IF( iflag )THEN
  CALL update_release_buttons( iwnd )
ELSE
  enable(1) = FALSE
  enable(2) = FALSE
  enable(3) = FALSE
  CALL EnableButtons( iwnd,enable,1,3 )
END IF

!     Control Buttons

enable(1) = iflag
IF( ( scenario(EDIT_LEVEL_2)%nrel < MAXREL  ) .AND. project(EDIT_LEVEL_2)%Edit )THEN
  enable(2) = iflag
ELSE
  enable(2) = FALSE
END IF
IF( project(EDIT_LEVEL_2)%Edit )THEN
  enable(3) = iflag
ELSE
  enable(3) = FALSE
END IF
CALL EnableButtons( iwnd,enable,13,3 )

!     Cancel/Exit

CALL EnableControl( iwnd,ID_CANCEL,iflag )

!     COMBO Box

CALL EnableControl( iwnd,IDB_COMBO1,iflag )

RETURN
END

!*******************************************************************************
!            Enable Release Editing
!*******************************************************************************
SUBROUTINE enable_release_edit( iwnd,iflag )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE guimatl_fi
USE SwinAPI
USE randef

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Dialog handle
INTEGER,              INTENT( IN ) :: iflag !Enable/Disable

INTEGER irel,i,jflag,kflag,id

LOGICAL lpool, testWet, testActive
INTEGER stack

LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsLiquid

IF( id_rel <= 0 )RETURN

save_flag(1) = iflag
save_flag(2) = iflag
save_flag(3) = iflag
save_flag(4) = iflag

releaseEnabled = iflag

!---  Release Type

lpool = .FALSE.
testWet = .TRUE.
testActive = .TRUE.
stack = FALSE
SELECT CASE( cur_release%type(1:1) )
  CASE( 'C' )
    SELECT CASE( cur_release%type(2:2) )

      CASE( 'M' )
        irel = 2

      CASE( 'S' )
        irel = 4
        stack = iflag

      CASE( 'P' )
        irel  = 1
        lpool = .TRUE.
        testActive = .FALSE.

      CASE DEFAULT
        irel = 1

    END SELECT

  CASE( 'M' )
    irel = 2

  CASE( 'I' )
    irel = 3

  CASE DEFAULT
    irel = 0

END SELECT

!---  Release time

kflag = iflag
CALL EnableControl( iwnd,IDB_REAL1, kflag )
CALL EnableControl( iwnd,IDB_COMBO4,kflag )

!---  Release X,Y Location

CALL EnableControl( iwnd,IDB_DOUBLE2, kflag )
CALL EnableControl( iwnd,IDB_COMBO5,kflag )
CALL EnableControl( iwnd,IDB_INT4,  kflag )
CALL EnableControl( iwnd,IDB_INT5,  kflag )
CALL EnableControl( iwnd,IDB_REAL15,kflag )
CALL EnableControl( iwnd,IDB_DOUBLE3, kflag )
CALL EnableControl( iwnd,IDB_INT6,  kflag )
CALL EnableControl( iwnd,IDB_INT7,  kflag )
CALL EnableControl( iwnd,IDB_REAL16,kflag )
CALL EnableControl( iwnd,IDB_CHECK2,kflag )

CALL EnableControl( iwnd,IDB_RADIO11,kflag )
CALL EnableControl( iwnd,IDB_RADIO12,kflag )
IF( cur_release%defName )THEN
  CALL EnableControl( iwnd,IDB_EDIT4,FALSE )
ELSE
  CALL EnableControl( iwnd,IDB_EDIT4,kflag )
END IF

!---  Continuous/Moving/Pool

IF( irel <= 2 .OR. irel == 4 )THEN

!---  Rate

  CALL EnableControl( iwnd,IDB_REAL5, kflag )
  CALL EnableControl( iwnd,IDB_COMBO6,kflag )

  IF( lpool )THEN

!---  Pool Sigma X,Y

    CALL EnableControl( iwnd,IDB_REAL7,kflag )
    CALL EnableControl( iwnd,IDB_REAL8,kflag )

  ELSE

!---  C/M Height,Duration, Sigma Y,Z

    CALL EnableControl( iwnd,IDB_REAL4, kflag )
    CALL EnableControl( iwnd,IDB_REAL6, kflag )
    CALL EnableControl( iwnd,IDB_COMBO7,kflag )
    IF( irel == 4 )THEN
      CALL EnableControl( iwnd,IDB_REAL7,kflag )
    ELSE
      CALL EnableControl( iwnd,IDB_REAL8,kflag )
      CALL EnableControl( iwnd,IDB_REAL9,kflag )
    END IF

  END IF

!---  Moving Velocity

  IF( irel == 2 )THEN
    CALL EnableControl( iwnd,IDB_REAL10,kflag )
    CALL EnableControl( iwnd,IDB_REAL11,kflag )
    CALL EnableControl( iwnd,IDB_REAL12,kflag )
  END IF

  CALL EnableControl( iwnd,IDB_RADIO01,kflag )

!---  Material

  CALL EnableControl( iwnd,IDB_COMBO2,kflag )
  CALL EnableControl( iwnd,IDB_COMBO3,kflag )

ELSE IF( irel /= 6 )THEN

!---  Instantaneous

!---  Height

  CALL EnableControl( iwnd,IDB_REAL4,kflag )

!---  Release Type

  CALL EnableControl( iwnd,IDB_RADIO01,kflag )
  CALL EnableControl( iwnd,IDB_RADIO02,kflag )

!---  Data Release

  IF( cur_release%spec == REL_DATA )THEN

!---  Mass

    CALL EnableControl( iwnd,IDB_REAL5, kflag )
    CALL EnableControl( iwnd,IDB_COMBO6,kflag )

!---  Size

    CALL EnableControl( iwnd,IDB_REAL7,kflag )
    CALL EnableControl( iwnd,IDB_REAL8,kflag )
    CALL EnableControl( iwnd,IDB_REAL9,kflag )

!---  Material

    CALL EnableControl( iwnd,IDB_COMBO2,kflag )
    CALL EnableControl( iwnd,IDB_COMBO3,kflag )

  ELSE

!--- File / Select

    CALL EnableControl( iwnd,IDB_BUTTON4,kflag )
    testWet = .FALSE.
    testActive = .FALSE.

  END IF

!---  Random

  CALL EnableControl( iwnd,IDB_CHECK1,kflag )

END IF

!--- LogNormal Params

IF( cur_release%distrib == REL_LOGNORM )THEN
  CALL EnableControl( iwnd,IDB_REAL13,kflag )
  CALL EnableControl( iwnd,IDB_REAL14,kflag )
END IF

!---  Slurry Params

IF( testWet )THEN
  CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl &
                          ,cur_release%matl,id )
  IF( IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( cur_release%distrib == MAXDISTRIBUTION+REL_SLURRY )THEN
      CALL EnableControl( iwnd,IDB_REAL13,kflag )
      CALL EnableControl( iwnd,IDB_REAL14,kflag )
      CALL EnableControl( iwnd,IDB_REAL23,kflag )
    ELSE IF( cur_release%distrib /=  REL_LOGNORM )THEN
      CALL EnableControl( iwnd,IDB_REAL23,kflag )
    END IF
  ELSE IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( cur_release%distrib <= REL_LOGNORM )THEN
      CALL EnableControl( iwnd,IDB_REAL23,kflag )
    END IF
  END IF
END IF

!---  Active Fraction

IF( testActive )THEN
  CALL EnableControl( iwnd,IDB_REAL26,kflag )
END IF

!---  Dynamics

IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  CALL EnableControl( iwnd,IDB_REAL17,kflag )
  CALL EnableControl( iwnd,IDB_REAL18,kflag )
  CALL EnableControl( iwnd,IDB_REAL19,stack )
  CALL EnableControl( iwnd,IDB_REAL20,stack )
END IF

!--  Button flag

IF( iflag )THEN
  jflag = SW_SHOWNORMAL
ELSE
  jflag = SW_HIDE
END IF

!---  Buttons

DO i = 11,12
  CALL EnableControl( iwnd,BUTTON_BASE+i,iflag )
  CALL ShowControl(   iwnd,BUTTON_BASE+i,jflag )
END DO

!---  Multicomp button text

IF( iflag )THEN
  string1 = 'Edit &Multi...'
ELSE
  string1 = 'Show &Multi...'
END IF
CALL SetControlText( iwnd,IDB_BUTTON7,string1 )

RETURN
END
!***********************************************************************
!               ReleaseCombo
!***********************************************************************
SUBROUTINE release_combo( iwnd_db,id_button,id_level )

USE resource_fd
USE reldef_fd
USE relparam_fd
USE files_fi
USE pcscipuf_fi
USE GUImatl_fi
USE SwinAPI
USE create_fi

!--- This routine processes COMBOBOXes from the MATDEF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

LOGICAL, EXTERNAL :: IsParticle
!!DEC$ IF DEFINED (AEROSOL)
!LOGICAL IsAerosol
!!DEC$ ENDIF
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
CHARACTER(128) new
INTEGER id, iaux, nsg, n, nex
INTEGER i, ios

CHARACTER(128), EXTERNAL :: StripNull

!---- Select by Button number

SELECT CASE( id_button )

  CASE( 1 ) !Changing Release
    new = StripNull( dbcmbo(1,id_level) )
    CALL find_release_list( new,id )
    CALL load_current_release( id )
    CALL init_release_edit( iwnd_db,id_level )
    CALL update_release_buttons( iwnd_db )

  CASE( 2 ) !Changing Material
    new = StripNull( dbcmbo(2,id_level) )
    CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl,new,id_relMat )
    dbint(2,id_level) = materials(EDIT_LEVEL_1)%material(id_relMat)%icls
    dbint(16,id_level) = materials(EDIT_LEVEL_1)%material(id_relMat)%nmc
    IF( dbint(16,id_level) <= 0 )THEN
      IF( ASSOCIATED(cur_release%mc) )THEN
        DEALLOCATE(cur_release%mc, STAT=ios)
        NULLIFY(cur_release%mc)
      END  IF
    ELSE
      IF( ASSOCIATED(cur_release%mc) )THEN
        IF( SIZE(cur_release%mc) /= dbint(16,id_level) )THEN
          DEALLOCATE(cur_release%mc, STAT=ios)
          NULLIFY(cur_release%mc)
        END IF
      END IF
      IF( .NOT.ASSOCIATED(cur_release%mc) )THEN
        ALLOCATE(cur_release%mc(dbint(16,id_level)), STAT=ios)
        IF( dbint(16,id_level) > SIZE(dblst(id_level,:)) )THEN
          CALL ReallocateMaxList(dbint(16,id_level))
        END IF
        DO i = 1,dbint(16,id_level)
          cur_release%mc(i) = 0.0
          dblst(id_level,i) = cur_release%mc(i)
        END DO
      END IF
    END IF
    CALL build_group_list( iwnd_db,id_level,IDB_COMBO3,id_relMat )
    lcheck(4,id_level) = dbint(3,id_level) == MAXDISTRIBUTION + REL_LIQUIDPOOL
    CALL show_distribution( iwnd_db,dbint(3,id_level),dbint(2,id_level) )
    CALL show_multicomp( iwnd_db,dbint(16,id_level) )
    CALL show_liquid_pool( iwnd_db,lcheck(4,id_level),lcheck(3,id_level),id_level,releaseEnabled )
    CALL build_release_Munit( iwnd_db,IDB_COMBO6,id_level,id_relMat )
    IF( project(EDIT_LEVEL_2)%Dynamic )THEN
      CALL show_dynamic( iwnd_db,dbint(2,id_level),dbint(3,id_level),dbreal(23,id_level) )
    END IF
    IF( lcheck(3,id_level) )THEN
      string1 = 'Diameter (m)'
    ELSE
!!DEC$ IF DEFINED (AEROSOL)
!      IF( IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
!        string1 = 'Entrainment'
!      ELSE
!        string1 = 'Size (m)'
!      END IF
!!DEC$ ELSE
      string1 = 'Size (m)'
!!DEC$ ENDIF
    END IF
    CALL SetControlText( iwnd_db,IDB_STATIC47,TRIM(string1) )

  CASE( 3 ) !Changing Group
    new = StripNull(dbcmbo(2,id_level))
    CALL find_material_list( materials(EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl,new,id )
    CALL ListCount( iwnd_db,IDB_COMBO3,n )
    IF( IsParticle(materials(EDIT_LEVEL_1)%material(id)%icls) &
	   .OR. IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      iaux = materials(EDIT_LEVEL_1)%material(id)%iaux
      nsg  = NINT(materials(EDIT_LEVEL_1)%mat_aux(iaux))
! ADDED
      nex = n - nsg
      IF( idbcmbo(3,id_level) > nex )THEN
        idbcmbo(3,id_level) = idbcmbo(3,id_level) - nex
      ELSE
        idbcmbo(3,id_level) = idbcmbo(3,id_level) + nsg
      END IF
! END ADD
      dbint(3,id_level) = idbcmbo(3,id_level) - nsg
      lcheck(4,id_level)= .FALSE.
    ELSE IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      iaux = materials(EDIT_LEVEL_1)%material(id)%iaux + MAXGMAUX + MAXLMAUXP
      nsg  = NINT(materials(EDIT_LEVEL_1)%mat_aux(iaux))
! ADDED
      nex = n - nsg
      IF( idbcmbo(3,id_level) > nex )THEN
        idbcmbo(3,id_level) = idbcmbo(3,id_level) - nex
      ELSE
        idbcmbo(3,id_level) = idbcmbo(3,id_level) + nsg
      END IF
! END ADD
      dbint(3,id_level) = idbcmbo(3,id_level) - nsg
      lcheck(4,id_level)= dbint(3,id_level) == MAXDISTRIBUTION + REL_LIQUIDPOOL
    ELSE
      dbint(3,id_level) = 0
      lcheck(4,id_level)= .FALSE.
    END IF
    CALL show_distribution( iwnd_db,dbint(3,id_level),materials(EDIT_LEVEL_1)%material(id)%icls )
    CALL show_liquid_pool( iwnd_db,lcheck(4,id_level),lcheck(3,id_level),id_level,releaseEnabled )
    CALL build_release_Munit( iwnd_db,IDB_COMBO6,id_level,id )
    IF( project(EDIT_LEVEL_2)%Dynamic )THEN
      CALL show_dynamic( iwnd_db,dbint(2,id_level),dbint(3,id_level),dbreal(23,id_level) )
    END IF

  CASE( 5 ) !Changing X,Y Axes
    CALL show_release_Lunit( iwnd_db,idbcmbo(5,id_level) )
    CALL SetControlText( iwnd_db,IDB_STATIC23,TRIM(StripNull(dbcmbo(5,id_level))) )

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!            Find Selection in Current Release List
!*******************************************************************************
SUBROUTINE find_release_list( string,id )

USE create_fi
USE GUIparam_fd

!     This routine loads the selection into current release

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: string
INTEGER,      INTENT( OUT ) :: id

INTEGER n,i

n = LEN(scenario(EDIT_LEVEL_2)%release(1)%string)

id = 0
DO i = 1,scenario(EDIT_LEVEL_2)%nrel
  IF( string(1:n) == scenario(EDIT_LEVEL_2)%release(i)%string )id = i
END DO

RETURN
END
!*******************************************************************************
!            Set release Mass/Rate units
!*******************************************************************************
SUBROUTINE build_release_Munit( iwnd,icmbo,ilev,id )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: icmbo
INTEGER,              INTENT( IN ) :: ilev
INTEGER,              INTENT( IN ) :: id

INTEGER irv, indx, jcmbo, imax

CHARACTER(128), EXTERNAL :: AddNull

CALL ClearList( iwnd,icmbo )

IF( id <= 0 )RETURN

imax = 0
SELECT CASE( cur_release%type(1:1) )

  CASE( 'C' )
    IF( lcheck(4,ilev) )THEN
      string1 = TRIM(materials(EDIT_LEVEL_1)%material(id)%unit)
      CALL AddList( iwnd,icmbo,-1,string1,irv )
      imax = imax + 1
    ELSE
      string1 = TRIM(materials(EDIT_LEVEL_1)%material(id)%unit)//'/sec'
      CALL AddList( iwnd,icmbo,-1,string1,irv )
      imax = imax + 1
      string1 = TRIM(materials(EDIT_LEVEL_1)%material(id)%unit)//'/min'
      CALL AddList( iwnd,icmbo,-1,string1,irv )
      imax = imax + 1
      string1 = TRIM(materials(EDIT_LEVEL_1)%material(id)%unit)//'/hr'
      CALL AddList( iwnd,icmbo,-1,string1,irv )
      imax = imax + 1
    END IF

  CASE( 'I' )
    string1 = TRIM(materials(EDIT_LEVEL_1)%material(id)%unit)
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    imax = imax + 1

  CASE DEFAULT
    string1 = 'Units'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    imax = imax + 1

END SELECT

jcmbo = icmbo - COMBO_BASE
indx  = MIN(idbcmbo(jcmbo,ilev),imax) - 1
CALL SetListSel( iwnd,icmbo,indx,irv )
CALL GetListSel( iwnd,icmbo,1,indx,irv )
IF( irv > 0 )THEN
  CALL GetListItem( iwnd,icmbo,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(jcmbo,ilev) = indx + 1
    dbcmbo(jcmbo,ilev)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF

RETURN
END
!*******************************************************************************
!            Set release time/dur units
!*******************************************************************************
SUBROUTINE build_release_Tunit( iwnd,icmbo,ilev )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: icmbo
INTEGER,              INTENT( IN ) :: ilev

INTEGER irv, indx, jcmbo

CHARACTER(128), EXTERNAL :: AddNull

CALL ClearList( iwnd,icmbo )

string1 = 'sec'
CALL AddList( iwnd,icmbo,-1,string1,irv )
string1 = 'min'
CALL AddList( iwnd,icmbo,-1,string1,irv )
string1 = 'hr'
CALL AddList( iwnd,icmbo,-1,string1,irv )

jcmbo = icmbo - COMBO_BASE
indx  = idbcmbo(jcmbo,ilev) - 1
CALL SetListSel( iwnd,icmbo,indx,irv )
CALL GetListSel( iwnd,icmbo,1,indx,irv )
IF( irv > 0 )THEN
  CALL GetListItem( iwnd,icmbo,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(jcmbo,ilev) = indx + 1
    dbcmbo(jcmbo,ilev)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF

RETURN
END
!*******************************************************************************
!            Set release location units
!*******************************************************************************
SUBROUTINE build_release_Lunit( iwnd,icmbo,ilev )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
USE dialog_fi

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: icmbo
INTEGER,              INTENT( IN ) :: ilev

INTEGER irv, indx, jcmbo

CHARACTER(128), EXTERNAL :: AddNull

CALL ClearList( iwnd,icmbo )

SELECT CASE( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord )

  CASE( I_LATLON )
    string1 = 'Degrees'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    string1 = 'Deg/Min/Sec'
    CALL AddList( iwnd,icmbo,-1,string1,irv )

  CASE( I_UTM )
    string1 = 'km'
    CALL AddList( iwnd,icmbo,-1,string1,irv )

  CASE(I_METERS )
    string1 = 'm'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    string1 = 'ft'
    CALL AddList( iwnd,icmbo,-1,string1,irv )

  CASE DEFAULT
    string1 = 'km'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    string1 = 'm'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    string1 = 'ft'
    CALL AddList( iwnd,icmbo,-1,string1,irv )
    string1 = 'mile'
    CALL AddList( iwnd,icmbo,-1,string1,irv )

END SELECT

jcmbo = icmbo - COMBO_BASE
indx  = idbcmbo(jcmbo,ilev) - 1
CALL SetListSel( iwnd,icmbo,indx,irv )
CALL GetListSel( iwnd,icmbo,1,indx,irv )
string2 = 'Not Set'
IF( irv > 0 )THEN
  CALL GetListItem( iwnd,icmbo,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(jcmbo,ilev) = indx + 1
    dbcmbo(jcmbo,ilev)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF
CALL SetControlText( iwnd,IDB_STATIC23,string2(1:irv) )

RETURN
END
!*******************************************************************************
!            Show release location units
!*******************************************************************************
SUBROUTINE show_release_Lunit( iwnd,iflag )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE SwinAPI

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: iflag

INTEGER ideg, iDMS

IF( iflag == 1 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
  ideg = SW_SHOWNORMAL
  iDMS = SW_HIDE
ELSE
  iDMS = SW_SHOWNORMAL
  ideg = SW_HIDE
END IF

CALL ShowControl( iwnd,IDB_DOUBLE2 ,ideg )
CALL ShowControl( iwnd,IDB_DOUBLE3 ,ideg )
CALL ShowControl( iwnd,IDB_INT4  ,iDMS )
CALL ShowControl( iwnd,IDB_INT5  ,iDMS )
CALL ShowControl( iwnd,IDB_INT6  ,iDMS )
CALL ShowControl( iwnd,IDB_INT7  ,iDMS )
CALL ShowControl( iwnd,IDB_REAL15,iDMS )
CALL ShowControl( iwnd,IDB_REAL16,iDMS )

RETURN
END
!*******************************************************************************
!            Release Spec radio changes
!*******************************************************************************
SUBROUTINE release_spec_radio( iwnd,ilev,flag )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: ilev
INTEGER,              INTENT( IN ) :: flag

INTEGER iflag, jflag, kflag, ishow, jshow, kshow, i, mshow, mflag, lshow, lflag
INTEGER nflag, nshow
LOGICAL lstack

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid
CHARACTER(128), EXTERNAL :: StripNull

!     iflag,ishow = Simple release
!     jflag,jshow = CLOUDTRANS release
!     kflag,kshow = Distribution
!     lflag,lshow = Slurry
!     mflag,mshow = Material Subgroup
!     nflag,nshow = Dynamic

IF( ichoice(1,ilev) == REL_DATA )THEN
  iflag = flag
  jflag = FALSE
  ishow = SW_SHOWNORMAL
  jshow = SW_HIDE
  string1 = ' '
  string2 = ' '
  lflag = jflag
  lshow = jshow
  IF( IsWetParticle(dbint(2,ilev)) )THEN
    IF( dbint(3,ilev) > 0 .AND. dbint(3,ilev) <= MAXDISTRIBUTION+REL_SLURRY )THEN
      kflag = iflag
      kshow = ishow
      IF( dbint(3,ilev) == MAXDISTRIBUTION+REL_SLURRY )THEN
        lflag = iflag
        lshow = ishow
      END IF
    ELSE
      kflag = jflag
      kshow = jshow
      lflag = iflag
      lshow = ishow
    END IF
  ELSE IF( IsLiquid(dbint(2,ilev)) )THEN
    IF( dbint(3,ilev) > 0 .AND. dbint(3,ilev) <= MAXDISTRIBUTION )THEN
      kflag = iflag
      kshow = ishow
    ELSE
      kflag = jflag
      kshow = jshow
    END IF
    IF( dbint(3,ilev) <= REL_LOGNORM )THEN
      lflag = iflag
      lshow = ishow
    END IF
  ELSE
    IF( dbint(3,ilev) > 0 .AND. dbint(3,ilev) <= MAXDISTRIBUTION )THEN
      kflag = iflag
      kshow = ishow
    ELSE
      kflag = jflag
      kshow = jshow
    END IF
  END IF
  IF( IsParticle(dbint(2,ilev)) .OR. IsWetParticle(dbint(2,ilev)) )THEN
    mflag = iflag
    mshow = ishow
    nflag = jflag
    nshow = jshow
  ELSE IF( IsLiquid(dbint(2,ilev)) )THEN
    mflag = iflag
    mshow = ishow
    IF( dbint(3,ilev) == MAXDISTRIBUTION+REL_GASPHASE .OR. ((dbint(3,ilev)<=MAXDISTRIBUTION) .AND. &
       (dbreal(23,ilev) >= 0.0 .AND. dbreal(23,ilev) < 1.0)) )THEN
      nflag = iflag
      nshow = ishow
    ELSE
      nflag = jflag
      nshow = jshow
    END IF
  ELSE IF( IsAerosol(dbint(2,ilev)) )THEN
    mflag = jflag
    mshow = jshow
    nflag = iflag
    nshow = ishow
  ELSE
    mflag = jflag
    mshow = jshow
    nflag = iflag
    nshow = ishow
  END IF
  CALL show_liquid_pool( iwnd,lcheck(4,ilev),lcheck(3,ilev),ilev,iflag )
  CALL show_multicomp( iwnd,dbint(16,ilev) )
ELSE
  iflag = FALSE
  jflag = flag
  kflag = iflag
  mflag = iflag
  nflag = iflag
  lflag = iflag
  ishow = SW_HIDE
  jshow = SW_SHOWNORMAL
  kshow = ishow
  mshow = ishow
  nshow = ishow
  lshow = ishow
  string1 = StripNull( dbtext(2,ilev) )
  string2 = StripNull( dbtext(3,ilev) )
  CALL show_multicomp( iwnd,0 )
  CALL ShowControl( iwnd,IDB_STATIC67,SW_HIDE )   !Active Fraction off
  CALL EnableControl( iwnd,IDB_REAL26,FALSE )
  CALL ShowControl( iwnd,IDB_REAL26,SW_HIDE )
  CALL ShowControl( iwnd,IDB_STATIC39,SW_HIDE )   !dry\liquid Fraction off
  CALL EnableControl( iwnd,IDB_REAL23,FALSE )
  CALL ShowControl( iwnd,IDB_REAL23,SW_HIDE )
END IF

CALL EnableControl( iwnd,IDB_BUTTON4,jflag )
CALL SetControlText( iwnd,IDB_STATIC34,string1 )
CALL SetControlText( iwnd,IDB_STATIC35,string2 )
CALL ShowControl( iwnd,IDB_BUTTON4, jshow )
CALL ShowControl( iwnd,IDB_STATIC32,jshow )
CALL ShowControl( iwnd,IDB_STATIC33,jshow )
CALL ShowControl( iwnd,IDB_STATIC34,jshow )
CALL ShowControl( iwnd,IDB_STATIC35,jshow )

CALL EnableControl( iwnd,IDB_COMBO2,iflag )
CALL EnableControl( iwnd,IDB_COMBO3,mflag )
CALL ShowControl( iwnd,IDB_COMBO2,ishow )
CALL ShowControl( iwnd,IDB_COMBO3,mshow )
CALL ShowControl( iwnd,IDB_STATIC30,ishow )
CALL ShowControl( iwnd,IDB_STATIC31,mshow )

CALL EnableControl( iwnd,IDB_REAL13,kflag )
CALL ShowControl(   iwnd,IDB_REAL13,kshow )
CALL ShowControl(   iwnd,IDB_STATIC36,kshow )
CALL ShowControl(   iwnd,IDB_STATIC37,kshow )
CALL EnableControl( iwnd,IDB_REAL14,kflag )
CALL ShowControl(   iwnd,IDB_REAL14,kshow )
CALL ShowControl(   iwnd,IDB_STATIC38,kshow )

CALL EnableControl( iwnd,IDB_REAL23,lflag )
CALL ShowControl( iwnd,IDB_REAL23,lshow )
CALL ShowControl( iwnd,IDB_STATIC39,lshow)

IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  CALL EnableControl( iwnd,IDB_REAL17,nflag )
  CALL ShowControl(   iwnd,IDB_REAL17,nshow )
  CALL EnableControl( iwnd,IDB_REAL18,nflag )
  CALL ShowControl( iwnd,IDB_REAL18  ,nshow )
  CALL ShowControl( iwnd,IDB_STATIC51,nshow )
  CALL ShowControl( iwnd,IDB_STATIC52,nshow )
  CALL ShowControl( iwnd,IDB_STATIC53,nshow )
  CALL ShowControl( iwnd,IDB_STATIC54,nshow )
  CALL ShowControl( iwnd,IDB_STATIC55,nshow )
  CALL ShowControl( iwnd,IDB_STATIC56,nshow )
END IF

lstack = .FALSE.
!      last_edit = 1 + ichoice(1,ilev)
SELECT CASE( dbtext(1,ilev)(1:1) )

  CASE( 'C' )
    SELECT CASE( dbtext(1,ilev)(2:2) )

      CASE( 'S' )
        last_edit = 4
        lstack = .TRUE.

      CASE( 'M' )
        last_edit = 1

      CASE DEFAULT
        last_edit = 0

    END SELECT

  CASE( 'I' )
    SELECT CASE( dbtext(1,ilev)(2:2) )

      CASE( 'A' )
        last_edit = 5
        jflag = flag
        CALL ShowControl( iwnd,IDB_RADIO02,SW_HIDE )

      CASE DEFAULT
        last_edit = 1 + ichoice(1,ilev)
        IF( ichoice(1,ilev) == 1 )THEN
          jflag = flag
        ELSE
          jflag = FALSE
        END IF

    END SELECT
    CALL EnableControl( iwnd,IDB_REAL5,jflag )
    CALL EnableControl( iwnd,IDB_COMBO6,jflag )
    DO i = 7,9
      CALL EnableControl( iwnd,REAL_BASE+i,jflag )
    END DO

  CASE DEFAULT
    last_edit = -1

END SELECT

IF( project(EDIT_LEVEL_2)%Dynamic .AND. lstack )THEN
  CALL EnableControl( iwnd,IDB_REAL19,nflag )
  CALL ShowControl( iwnd,IDB_REAL19  ,nshow )
  CALL EnableControl( iwnd,IDB_REAL20,nflag )
  CALL ShowControl( iwnd,IDB_REAL20  ,nshow )
  CALL ShowControl( iwnd,IDB_STATIC57,nshow )
  CALL ShowControl( iwnd,IDB_STATIC58,nshow )
  CALL ShowControl( iwnd,IDB_STATIC59,nshow )
  CALL ShowControl( iwnd,IDB_STATIC60,nshow )
  CALL ShowControl( iwnd,IDB_STATIC61,nshow )
  CALL ShowControl( iwnd,IDB_STATIC62,nshow )
END IF

CALL show_simple_edit( iwnd,ishow )

RETURN
END
!*******************************************************************************
!            Show  Continuous Editing controls
!*******************************************************************************
SUBROUTINE show_continuous_edit( iwnd_db,ilev )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,               INTENT( IN ) :: ilev !Dialog level

INTEGER i, indx, irv

!==== last_edit =  0 -> Already set to Continuous
!==== last_edit =  1 -> Last set for Moving
!==== last_edit =  2 -> Last set for Instantaneous/Simple
!==== last_edit =  3 -> Last set for Instantaneous/CLOUDTRANS
!==== last_edit =  4 -> Last set for Stack
!==== last_edit =  5 -> Last set for Instantaneous/Aerosol
!==== last_edit = -1 -> last cleared

SELECT CASE( last_edit )
  CASE( -1 )

!---     Labels

    DO i = 11,18
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 19,26
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 30,31
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 40,47
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!---    Time/Location Parameters

    CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
    CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
    IF( indx <= 0 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
      CALL ShowControl( iwnd_db,IDB_DOUBLE2,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_DOUBLE3,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_INT4,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT5,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT6,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT7,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL15,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL16,SW_SHOWNORMAL )
    END IF
    CALL ShowControl( iwnd_db,IDB_REAL4,SW_SHOWNORMAL )

!---     Specifications

    CALL ShowControl( iwnd_db,IDB_RADIO01,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO3,SW_SHOWNORMAL )

    CALL ShowControl( iwnd_db,IDB_RADIO11,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO12,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_EDIT4,SW_SHOWNORMAL )

!---    Parameters

    DO i = 5,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )

  CASE ( 0 )

  CASE ( 1 )

!---     Parameters

    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_HIDE )

  CASE ( 2 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE ( 3 )
    CALL ShowControl( iwnd_db,IDB_STATIC17,SW_SHOWNORMAL )
    DO i = 40,47
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 5,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE ( 4 )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_SHOWNORMAL )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Size (m)' )

  CASE ( 5 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE DEFAULT

END SELECT

last_edit = 0

RETURN
END
!*******************************************************************************
!            Show Stack Editing controls
!*******************************************************************************
SUBROUTINE show_stack_edit( iwnd_db,ilev )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: ilev !Dialog level

INTEGER i, indx, irv

!==== last_edit =  0 -> Last set for Continuous
!==== last_edit =  1 -> Last set for Moving
!==== last_edit =  2 -> Last set for Instantaneous/Simple
!==== last_edit =  3 -> Last set for Instantaneous/CLOUDTRANS
!==== last_edit =  4 -> Already set to Stack
!==== last_edit =  5 -> Last set for Instantaneous/Aerosol
!==== last_edit = -1 -> last cleared

SELECT CASE( last_edit )
  CASE( -1 )

!---     Labels

    DO i = 11,18
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 19,26
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 30,31
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 40,43
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC47,SW_SHOWNORMAL )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

!---     Time/Location Parameters

    CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
    CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
    IF( indx <= 0 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
      CALL ShowControl( iwnd_db,IDB_DOUBLE2,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_DOUBLE3,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_INT4,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT5,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT6,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT7,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL15,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL16,SW_SHOWNORMAL )
    END IF
    CALL ShowControl( iwnd_db,IDB_REAL4,SW_SHOWNORMAL )

!---     Specifications

    CALL ShowControl( iwnd_db,IDB_RADIO01,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO3,SW_SHOWNORMAL )

    CALL ShowControl( iwnd_db,IDB_RADIO11,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO12,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_EDIT4,SW_SHOWNORMAL )

!---     Parameters

    DO i = 5,7
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )

  CASE ( 0 )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_HIDE )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

  CASE ( 1 )

!---     Parameters

    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_HIDE )

    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_HIDE )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

  CASE ( 2 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,  SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6   ,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_HIDE )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

  CASE ( 3 )
    CALL ShowControl( iwnd_db,IDB_STATIC17,SW_SHOWNORMAL )
    DO i = 40,47
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 5,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_HIDE )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

  CASE ( 4 )

  CASE ( 5 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6   ,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_HIDE )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Diameter (m)' )

  CASE DEFAULT

END SELECT

last_edit = 4

RETURN
END
!*******************************************************************************
!            Show  Moving Editing controls
!*******************************************************************************
SUBROUTINE show_moving_edit( iwnd_db,ilev )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: ilev !Dialog level

INTEGER i, indx, irv

!==== last_edit =  0 -> Last set for Continuous
!==== last_edit =  1 -> Already set to Moving
!==== last_edit =  2 -> Last set for Instantaneous/Simple
!==== last_edit =  3 -> Last set for Instantaneous/CLOUDTRANS
!==== last_edit =  4 -> Last set for Stack
!==== last_edit = -1 -> last cleared
!==== last_edit =  5 -> Last set for Instantaneous/Aerosol

SELECT CASE( last_edit )
  CASE( -1 )

!---     Labels

    DO i = 11,18
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 19,26
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 30,31
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 40,48
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!---     Time/Location Parameters

    CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
    CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
    IF( indx <= 0 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
      CALL ShowControl( iwnd_db,IDB_DOUBLE2,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_DOUBLE3,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_INT4,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT5,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT6,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT7,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL15,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL16,SW_SHOWNORMAL )
    END IF
    CALL ShowControl( iwnd_db,IDB_REAL4,SW_SHOWNORMAL )

!---    Specifications

    CALL ShowControl( iwnd_db,IDB_RADIO01,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO3,SW_SHOWNORMAL )

    CALL ShowControl( iwnd_db,IDB_RADIO11,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO12,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_EDIT4,SW_SHOWNORMAL )

!---     Parameters

    DO i = 5,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )

  CASE ( 0 )

!---      Parameters

    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_SHOWNORMAL )

  CASE ( 1 )

  CASE ( 2 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_SHOWNORMAL )
    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE ( 3 )
    CALL ShowControl( iwnd_db,IDB_STATIC17,SW_SHOWNORMAL )
    DO i = 40,48
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 5,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_COMBO6,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE ( 4 )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_SHOWNORMAL )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Size (m)' )

    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_SHOWNORMAL )

  CASE ( 5 )
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_SHOWNORMAL )
    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )

  CASE DEFAULT

END SELECT

last_edit = 1

RETURN
END
!*******************************************************************************
!            Show Instantaneous Editing controls
!*******************************************************************************
SUBROUTINE show_instantaneous_edit( iwnd_db,iflag,lrnd,ilev )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag   !Simple/Complex flag
LOGICAL,              INTENT( IN ) :: lrnd    !Random loc. flag
INTEGER,              INTENT( IN ) :: ilev    !Dialog level

INTEGER i, indx, irv

!==== last_edit =  0 -> Last set for Continuous
!==== last_edit =  1 -> Last set for Moving
!==== last_edit =  2 -> Last set for Instantaneous/Simple
!==== last_edit =  3 -> Last set for Instantaneous/CLOUDTRANS
!==== last_edit =  4 -> Last set for Stack
!==== last_edit =  5 -> Last set for Instantaneous/Aerosol
!==== last_edit = -1 -> last cleared

SELECT CASE( last_edit )
  CASE (-1 )

!---    Labels

    DO i = 11,18
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 19,26
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 30,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    IF( iflag == 1 )CALL show_simple_edit( iwnd_db,SW_SHOWNORMAL )

!---     Time/Location Parameters

    CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
    CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
    IF( indx <= 0 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
      CALL ShowControl( iwnd_db,IDB_DOUBLE2,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_DOUBLE3,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_INT4,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT5,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT6,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT7,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL15,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL16,SW_SHOWNORMAL )
    END IF
    CALL ShowControl( iwnd_db,IDB_REAL4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL )

!---     Specifications

    CALL ShowControl( iwnd_db,IDB_RADIO01,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO3,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )

    CALL ShowControl( iwnd_db,IDB_RADIO11,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO12,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_EDIT4,SW_SHOWNORMAL )

  CASE ( 0 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl(iwnd_db,IDB_CHECK1,SW_SHOWNORMAL)

  CASE ( 1 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_HIDE )
    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL )

  CASE ( 2 )

  CASE ( 3 )

  CASE ( 4 )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_SHOWNORMAL )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Size (m)' )

    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL)
  CASE ( 5 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )

  CASE DEFAULT

END SELECT

IF( iflag == 2 )THEN
  CALL show_simple_edit( iwnd_db,SW_HIDE )
ELSE
  CALL show_simple_edit( iwnd_db,SW_SHOWNORMAL )
END IF


IF( lrnd )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_SHOWNORMAL )
  CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )
ELSE
  CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
  CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
END IF

last_edit = 1 + iflag

RETURN
END
!*******************************************************************************
!            Show Flashing Aerosol Editing controls
!*******************************************************************************
SUBROUTINE show_flash_edit( iwnd_db,iflag,lrnd,ilev )

USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag   !Simple/Complex flag
LOGICAL,              INTENT( IN ) :: lrnd    !Random loc. flag
INTEGER,              INTENT( IN ) :: ilev    !Dialog level

INTEGER i, indx, irv

!==== last_edit =  0 -> Last set for Continuous
!==== last_edit =  1 -> Last set for Moving
!==== last_edit =  2 -> Last set for Instantaneous/Simple
!==== last_edit =  3 -> Last set for Instantaneous/CLOUDTRANS
!==== last_edit =  4 -> Last set for Stack
!==== last_edit =  5 -> Last set for Instantaneous/Aerosol
!==== last_edit = -1 -> last cleared

SELECT CASE( last_edit )
  CASE (-1 )

!---     Labels

    DO i = 11,18
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 19,26
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 30,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    IF( iflag == 1 )CALL show_simple_edit( iwnd_db,SW_SHOWNORMAL )

!---     Time/Location Parameters

    CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO5,SW_SHOWNORMAL )
    CALL GetListSel( iwnd_db,IDB_COMBO5,1,indx,irv )
    IF( indx <= 0 .OR. dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord /= I_LATLON )THEN
      CALL ShowControl( iwnd_db,IDB_DOUBLE2,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_DOUBLE3,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_INT4,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT5,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT6,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_INT7,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL15,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL16,SW_SHOWNORMAL )
    END IF
    CALL ShowControl( iwnd_db,IDB_REAL4,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL )

!---     Specifications

    CALL ShowControl( iwnd_db,IDB_RADIO01,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO2,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_COMBO3,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )

    CALL ShowControl( iwnd_db,IDB_RADIO11,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_RADIO12,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_EDIT4,SW_SHOWNORMAL )

  CASE ( 0 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl(iwnd_db,IDB_CHECK1,SW_SHOWNORMAL)

  CASE ( 1 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_STATIC48,SW_HIDE )
    DO i = 10,12
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
    END DO
    CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL )

  CASE ( 2 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )

  CASE ( 3 )
    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_HIDE )

  CASE ( 4 )
    DO i = 8,9
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC44,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC45,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC46,SW_SHOWNORMAL )
    CALL SetControlText( iwnd_db,IDB_STATIC47,'Size (m)' )

    CALL ShowControl( iwnd_db,IDB_RADIO02,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_BUTTON4,SW_SHOWNORMAL )
    DO i = 32,35
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL ShowControl( iwnd_db,IDB_STATIC42,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_REAL6,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_CHECK1,SW_SHOWNORMAL)

  CASE ( 5 )

  CASE DEFAULT

END SELECT

CALL show_simple_edit( iwnd_db,SW_SHOWNORMAL )

IF( lrnd )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_SHOWNORMAL )
  CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )
ELSE
  CALL ShowControl( iwnd_db,IDB_BUTTON5,SW_HIDE )
  CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE )
END IF

last_edit = 5

RETURN
END
!*******************************************************************************
!            Show Release paramters Editing controls
!*******************************************************************************
SUBROUTINE show_simple_edit( iwnd_db,iflag )

USE resource_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag   !Show/Hide flag

INTEGER i, j, k

IF( last_edit == 4 )THEN
  j = 0
  k = 7
ELSE
  j = 46
  k = 9
END IF

CALL ShowControl( iwnd_db,IDB_STATIC40,iflag )
CALL ShowControl( iwnd_db,IDB_COMBO6,iflag )
DO i = 44,j
  CALL ShowControl( iwnd_db,STATIC_BASE+i,iflag )
END DO
CALL ShowControl( iwnd_db,IDB_STATIC47,iflag )
CALL ShowControl( iwnd_db,IDB_REAL5   ,iflag )
DO i = 7,k
  CALL ShowControl( iwnd_db,REAL_BASE+i,iflag )
END DO

RETURN
END
!***********************************************************************
!               ReleaseButton
!***********************************************************************
SUBROUTINE release_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi
USE cfocus

!     This routine processes PUSHBUTTONs from RELDEF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

INTEGER irv
LOGICAL lok, verify_button

CHARACTER(128)            dString
CHARACTER(PATH_MAXLENGTH) filenam

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull, AddNull

LOGICAL, EXTERNAL :: hasError

!---- initailize

lok = .FALSE.

!---- Select by Button number

SELECT CASE( id_button )

  CASE( 1 ) !EDIT Release
    ifocus = GetFocus()
    CALL enable_release_edit( iwnd_db,TRUE )
    CALL enable_release_control( iwnd_db,FALSE )
    CALL SetFocusControl( iwnd_db,IDB_BUTTON12 )

  CASE( 2 ) !NEW Release
    CALL LetsDialog( iwnd_db,IDB_RELNEW )
    IF( lokbutton )THEN
      ifocus = GetFocus()
      CALL enable_release_edit( iwnd_db,TRUE )
      CALL enable_release_control( iwnd_db,FALSE )
      CALL SetFocusControl( iwnd_db,IDB_REAL1 )
    END IF

  CASE( 3 ) !DELETE Release
    ifocus = GetFocus()
    string1 = 'Delete the current release definition'
    lok = verify_button( iwnd_db,TRIM(string1) )
    IF( lok )THEN
      CALL delete_current_release( id_rel )
      IF( hasError() )CALL ShowErrorMessage( iwnd_db )
      CALL build_release_list( iwnd_db,2,IDB_COMBO1 )
      CALL update_release_buttons( iwnd_db )
      CALL load_current_release( id_rel )
      CALL init_dialog_releases( iwnd_db,id_level )
    END IF
    irv = SetFocus( ifocus )

  CASE( 4 ) !Select CLOUDTRANS file
    lok = .FALSE.
    filenam = StripNull( dbtext(2,id_level) )
    string2 = StripNull( dbtext(3,id_level) )
    CALL AddPath( filenam,string2 )
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL SplitName( filenam,string2,string1 )
      dbtext(2,id_level) = AddNull( TRIM(string2) )
      dbtext(3,id_level) = AddNull( TRIM(string1) )
      CALL ReportFileName( dString,'',string2 )
      CALL SetControlText( iwnd_db,IDB_STATIC34,dString )
      CALL ReportFileName( dString,'',string1 )
      CALL SetControlText( iwnd_db,IDB_STATIC35,dString )
    END IF

  CASE( 5 ) !Random Location parameters
    CALL LetsDialog( iwnd_db,IDB_RNDPARM )

  CASE( 6 )
    CALL LetsDialog( iwnd_db,IDB_UNCPARM )

  CASE( 7 ) !Multicomp parameters
    CALL LetsDialog( iwnd_db,IDB_MULTICOMP )

  CASE( 11 ) !SAVE Edits
    CALL save_release_edit( iwnd_db,id_level,lok )
    IF( lok )THEN
      CALL save_current_release( iwnd_db,id_level )
      CALL enable_release_edit( iwnd_db,FALSE )
      CALL enable_release_control( iwnd_db,TRUE )
      irv = SetFocus( ifocus )
    END IF

  CASE( 12 ) !CANCEL EDITS
    CALL enable_release_edit( iwnd_db,FALSE )
    CALL load_current_release( id_rel )
    CALL init_release_edit( iwnd_db,id_level )
    CALL enable_release_control( iwnd_db,TRUE )
    irv = SetFocus( ifocus )

  CASE( 13 ) !OK - Check Data first
    lok = .FALSE.
    CALL check_release_definition( iwnd_db,scenario(EDIT_LEVEL_2)%nrel, &
                                    scenario(EDIT_LEVEL_2)%release,lok )
    IF( lok )CALL PushButton( iwnd_db,IDB_BUTTON13,ID_OK,irv )

  CASE( 14 ) !LOAD Releases from file
    lok = .FALSE.
    filenam = TRIM(loadfile(5))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL getinfo_scn( iwnd_db,filenam,lok,scenario(EDIT_LEVEL_2),materials(EDIT_LEVEL_1) ) !Read releases
      IF( lok )THEN
        CALL check_buoyant( iwnd_db,2,.TRUE. )
        id_rel = scenario(EDIT_LEVEL_2)%nrel
        CALL update_release_buttons( iwnd_db )
        CALL load_current_release( id_rel )
        CALL init_dialog_releases( iwnd_db,id_level )
        loadfile(5) = TRIM(filenam)
      END IF
    END IF

  CASE( 15 ) !CLEAR ALL Releases
    ifocus = GetFocus()
    string1 = 'Delete ALL Release Definitions'
    lok = verify_button( iwnd_db,TRIM(string1) )
    IF( lok )THEN
      scenario(EDIT_LEVEL_2)%nrel = 0
      id_rel = 0
      CALL init_dialog_releases( iwnd_db,id_level )
    END IF
    irv = SetFocus( ifocus )

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!                     Check Release Description
!*******************************************************************************
SUBROUTINE check_release( iwnd_db,lrnd,lpool,rel,lok )

USE resource_fd
USE relparam_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE errorParam_fd
USE GUImatl_fi
USE UtilMtlAux

!     This routine checks the release description

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db
LOGICAL,              INTENT( IN    ) :: lrnd
LOGICAL,              INTENT( IN    ) :: lpool
TYPE( release_str),   INTENT( INOUT ) :: rel
LOGICAL,              INTENT( OUT   ) :: lok

REAL, PARAMETER :: RHO_WATER  = 1000.0

TYPE( part_material) pmatpart

INTEGER  i, iaux, nsg, j, id
LOGICAL  ltest, ldef, CheckFile
LOGICAL  rateCheck
REAL     frac, relmmd, rat, binl

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckRelease'

lok = .TRUE.

!---- Check for custom name

IF( .NOT.rel%defName )THEN
  IF( LEN_TRIM(rel%string(7:)) > 48 )THEN
    eMessage = 'Custom release name too long'
    WRITE(eInform,'(A,I3,A)')'Current name is',LEN_TRIM(rel%string(7:)),' characters'
    eAction  = 'Please shorten the custom name to be 48 or fewer characters'
    GOTO 9999
  END IF
END IF

!---- Check for time/location

IF( rel%time == NOT_SET_R )THEN
  eMessage = 'No release time'
  eAction  = 'Please specify the release time in hrs from Start'
  GOTO 9999
END IF

IF( rel%time == DEFERRED_R )THEN
  eMessage = 'Deferred release time'
  eAction  = 'Please specify the release time in hrs from Start'
  GOTO 9998
END IF

ltest = .FALSE.
ldef  = .FALSE.
IF( lpool )THEN
  ltest = ltest .OR. rel%xRel == NOT_SET_D
  ldef  = ldef  .OR. rel%xRel == DEFERRED_D
  ltest = ltest .OR. rel%yRel == NOT_SET_D
  ldef  = ldef  .OR. rel%yRel == DEFERRED_D
ELSE
  ltest = ltest .OR. rel%xRel == NOT_SET_D
  ldef  = ldef  .OR. rel%xRel == DEFERRED_D
  ltest = ltest .OR. rel%yRel == NOT_SET_D
  ldef  = ldef  .OR. rel%yRel == DEFERRED_D
  ltest = ltest .OR. rel%zRel == NOT_SET_R
  ldef  = ldef  .OR. rel%zRel == DEFERRED_R
END IF
IF( ltest .OR. ldef )THEN
  eAction  = 'Please specify all three release locations (X,Y,Z)'
  IF( ltest )THEN
    eMessage = 'No release location specified'
    GOTO 9999
  END IF
  IF( ldef )THEN
    eMessage = 'Deferred release location specified'
    GOTO 9998
  END IF
END IF

!---- Complex

IF( rel%spec == REL_FILE )THEN

  IF( rel%file == ' ' .OR. rel%path == ' ' )THEN
    eMessage = 'No CLOUDTRANS file'
    eAction  = 'Please select a CLOUDTRANS file'
    GOTO 9999
  ELSE
    string1 = TRIM(rel%file)
    CALL AddPath(string1,TRIM(rel%path))
    IF( .NOT. CheckFile(string1) )THEN
      eMessage = 'CLOUDTRANS file not found'
      eAction  = 'Please select an existing CLOUDTRANS file'
    END IF
  END IF

!---- Simple

ELSE

  CALL find_material_list(materials( EDIT_LEVEL_1)%material,materials(EDIT_LEVEL_1)%nmatl,rel%matl,id )
  IF( id <= 0 )THEN
    eMessage = 'Release Material Not Found'
    eAction  = 'Please specify a current Material for release'
    GOTO 9999
  END IF

  IF( rel%type(1:2) /= 'CP' .AND. rel%type(1:2) /= 'IF' )THEN
    IF( rel%param(REL_AFRAC_INDX) <= 0.0 .OR. rel%param(REL_AFRAC_INDX) > 1.0 )THEN
      eMessage = 'Invalid active fraction specified'
      eAction  = 'Please specify an active fraction between 0 and 1'
      GOTO 9998
    END IF
  END IF

  IF( IsParticle(materials(EDIT_LEVEL_1)%material(id)%icls) &
     .OR. IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
      IF( rel%indx <= 0 .OR. rel%distrib > MAXDISTRIBUTION+MAXSPECIALWETP )THEN
        eMessage = 'Release Material Size Bin/Distribution Not Found'
        eAction  = 'Please specify a current Size Bin for release'
        GOTO 9999
      END IF
    ELSE
      IF( rel%indx <= 0 .OR. rel%distrib > MAXDISTRIBUTION )THEN
        eMessage = 'Release Material Size Bin/Distribution Not Found'
        eAction  = 'Please specify a current Size Bin for release'
        GOTO 9999
      END IF
    END IF
    IF( rel%distrib > 0 )THEN
      IF( rel%param(REL_MMD_INDX) <= 0.0 .AND. rel%param(REL_MMD_INDX) /= NOT_SET_R )THEN
        eMessage = 'Invalid MMD for Log Normal distribution'
        eAction  = 'Please specify a positive MMD'
        GOTO 9999
      ELSE
        IF( rel%param(REL_MMD_INDX) == NOT_SET_R )THEN
          eMessage = 'No MMD for Log Normal distribution specified'
          eAction  = 'Please specify a positive MMD'
          GOTO 9999
        END IF
        IF( rel%param(REL_MMD_INDX) == DEFERRED_R )THEN
          eMessage = 'Deferred MMD for Log Normal distribution specified'
          eAction  = 'Please specify a positive MMD'
          GOTO 9998
        END IF
      END IF
      IF( rel%param(REL_SIGMA_INDX) <= 1.0 .AND. rel%param(REL_SIGMA_INDX) /= NOT_SET_R )THEN
        eMessage = 'Invalid sigma for Log Normal distribution'
        eAction  = 'Please specify a sigma > 1'
        GOTO 9999
      ELSE
        IF( rel%param(REL_SIGMA_INDX) == NOT_SET_R )THEN
          eMessage = 'No sigma for Log Normal distribution specified'
          eAction  = 'Please specify a sigma > 1'
          GOTO 9999
        END IF
        IF( rel%param(REL_SIGMA_INDX) == DEFERRED_R )THEN
          eMessage = 'Deferred sigma for Log Normal distribution specified'
          eAction  = 'Please specify a sigma > 1'
          GOTO 9998
        END IF
      END IF
      IF( rel%distrib == MAXDISTRIBUTION+REL_SLURRY )THEN
        IF( rel%param(REL_WMFRAC_INDX) /= DEF_VAL_R .AND. rel%param(REL_WMFRAC_INDX) /= DEFERRED_R )THEN
          IF( rel%param(REL_WMFRAC_INDX) == NOT_SET_R )THEN
            eMessage = 'No slurry dry fraction specified'
            eAction  = 'Please specify a dry fraction between 0 and 1'
            GOTO 9999
          END IF
          IF( rel%param(REL_WMFRAC_INDX) <= 0 .OR. rel%param(REL_WMFRAC_INDX) > 1.0 )THEN
            eMessage = 'Invalid slurry dry fraction specified'
            eAction  = 'Please specify a dry fraction between 0 and 1'
            GOTO 9998
          END IF
        ELSE
          IF( rel%param(REL_WMFRAC_INDX) == NOT_SET_R )THEN
            eMessage = 'No slurry dry fraction specified'
            eAction  = 'Please specify a dry fraction between 0 and 1'
            GOTO 9999
          END IF
          IF( rel%param(REL_WMFRAC_INDX) == DEFERRED_R )THEN
            eMessage = 'Deferred slurry dry fraction specified'
            eAction  = 'Please specify a dry fraction between 0 and 1'
            GOTO 9998
          END IF
          IF( rel%param(REL_WMFRAC_INDX) /= DEF_VAL_R )THEN
            IF( rel%param(REL_WMFRAC_INDX) <= 0 .OR. rel%param(REL_WMFRAC_INDX) > 1.0 )THEN
              eMessage = 'Invalid slurry dry fraction specified'
              eAction  = 'Please specify a dry fraction between 0 and 1'
              GOTO 9998
            END IF
          END IF
        END IF
      END IF
      iaux = materials(EDIT_LEVEL_1)%material(id)%iaux
      nsg  = NINT(materials(EDIT_LEVEL_1)%mat_aux(iaux))
      iaux = iaux + PMAUX_BOUNDS
      IF( rel%param(REL_MMD_INDX)  /= NOT_SET_R .AND. &
         rel%param(REL_MMD_INDX)   /= DEF_VAL_R .AND. &
         rel%param(REL_SIGMA_INDX) /= NOT_SET_R .AND. &
         rel%param(REL_SIGMA_INDX) /= DEF_VAL_R )THEN
		    relmmd = rel%param(REL_MMD_INDX)/HCF_M2MICRON
        IF( IsWetParticle(materials(EDIT_LEVEL_1)%material(id)%icls) .AND. &
		       rel%param(REL_WMFRAC_INDX) /= 1.0 .AND. &
           rel%param(REL_WMFRAC_INDX) /= NOT_SET_R)THEN
          CALL GetParticleParam( pmatpart,materials(EDIT_LEVEL_1)%material(id)%iaux,1, &
                                 materials(EDIT_LEVEL_1)%mat_aux )
          rat = pmatpart%rho/RHO_WATER
          rat = (1.0+rat*(1.0/rel%param(REL_WMFRAC_INDX)-1.0))**0.3333333
		      relmmd = relmmd/rat
		      binl = 1.0E-10
		    ELSE
		      rat  = 1.0
		      binl = materials(EDIT_LEVEL_1)%mat_aux(iaux)
		    END IF
		    CALL check_logn( binl,materials(EDIT_LEVEL_1)%mat_aux(iaux+nsg*MAXPMAUX), &
                         relmmd,rel%param(REL_SIGMA_INDX),frac )
        IF( frac > 0.05 )THEN
          nError = WN_ERROR
          WRITE(string1,*) NINT(frac*100.0)
          eMessage = TRIM(ADJUSTL(string1))//'% of the mass is outside the bin range'
          IF( rat /= 1.0 )THEN
		        eInform  = 'This mass will be lumped into the first and last bins'
          ELSE
		        eInform  = 'The mass larger than the last bin will be ignored'
		      END IF
		      eAction = 'Do you want to continue?'
          CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
          CALL ShowWarningMessage( iwnd_db,.TRUE. )
          eRoutine = 'CheckRelease'
          IF( hasError() )THEN
            eMessage = 'More than 5% of the mass is outside the bin range'
            eInform  = 'Please adjust the MMD and/or sigma'
            eAction  = '(or redefine the material size bins)'
            GOTO 9999
          END IF
        END IF
      END IF
    END IF
  ELSE IF( IsLiquid(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( rel%indx <= 0 .OR. rel%distrib > MAXDISTRIBUTION+MAXSPECIALLIQUID )THEN
      eMessage = 'Release Material Size Bin/Distribution Not Found'
      eAction  = 'Please specify a current Size Bin for release'
      GOTO 9999
    END IF
    IF( rel%distrib > 0 .AND. rel%distrib <= MAXDISTRIBUTION )THEN
      IF( rel%param(REL_MMD_INDX) <= 0.0 .AND. &
          rel%param(REL_MMD_INDX) /= NOT_SET_R )THEN
        eMessage = 'Invalid MMD for Log Normal distribution'
        eAction  = 'Please specify a positive MMD'
        GOTO 9999
      ELSE
        IF( rel%param(REL_MMD_INDX) == NOT_SET_R )THEN
          eMessage = 'No MMD for Log Normal distribution specified'
          eAction  = 'Please specify a positive MMD'
          GOTO 9999
        END IF
        IF( rel%param(REL_MMD_INDX) == DEFERRED_R )THEN
          eMessage = 'Deferred MMD for Log Normal distribution specified'
          eAction  = 'Please specify a positive MMD'
          GOTO 9998
        END IF
      END IF
      IF( rel%param(REL_SIGMA_INDX) <= 1.0 .AND. rel%param(REL_SIGMA_INDX) /= NOT_SET_R )THEN
        eMessage = 'Invalid sigma for Log Normal distribution'
        eAction  = 'Please specify a sigma > 1'
        GOTO 9999
      ELSE
        IF( rel%param(REL_SIGMA_INDX) == NOT_SET_R )THEN
          eMessage = 'No sigma for Log Normal distribution specified'
          eAction  = 'Please specify a sigma > 1'
          GOTO 9999
        END IF
        IF( rel%param(REL_SIGMA_INDX) == DEFERRED_R )THEN
          eMessage = 'Deferred sigma for Log Normal distribution specified'
          eAction  = 'Please specify a sigma > 1'
          GOTO 9998
        END IF
      END IF
      iaux = materials(EDIT_LEVEL_1)%material(id)%iaux + MAXGMAUX + MAXLMAUXP
      nsg  = NINT(materials(EDIT_LEVEL_1)%mat_aux(iaux))
      iaux = iaux + LMAUX_BOUNDS
      IF( rel%param(REL_MMD_INDX)   /= NOT_SET_R .AND. &
          rel%param(REL_MMD_INDX)   /= DEF_VAL_R .AND. &
          rel%param(REL_SIGMA_INDX) /= NOT_SET_R .AND. &
          rel%param(REL_SIGMA_INDX) /= DEF_VAL_R )THEN
        CALL check_logn( 1.E-10, &
                         materials(EDIT_LEVEL_1)%mat_aux(iaux+nsg*MAXLMAUX), &
                         rel%param(REL_MMD_INDX)/HCF_M2MICRON, &
                         rel%param(REL_SIGMA_INDX),frac )
        IF( frac > 0.05 )THEN
          nError = WN_ERROR
          WRITE(string1,*) NINT(frac*100.0)
          eMessage = TRIM(ADJUSTL(string1))//'% of the mass is outside the bin range'
          eInform  = 'This mass will be lumped into the first and last bins'
          eAction  = 'Do you want to continue?'
          CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
          CALL ShowWarningMessage( iwnd_db,.TRUE. )
          eRoutine = 'CheckRelease'
          IF( hasError() )THEN
            eMessage = 'More than 5% of the mass is outside the bin range'
            eInform  = 'Please adjust the MMD and/or sigma'
            eAction  = '(or redefine the material size bins)'
            GOTO 9999
          END IF
        END IF
      END IF
    END IF
    IF( project(EDIT_LEVEL_2)%Dynamic .AND. rel%distrib == MAXDISTRIBUTION+REL_GASPHASE )THEN
      ltest = .FALSE.
      ldef  = .FALSE.
      DO i = 1,NUM_DYNAMIC
        ltest = ltest .OR. rel%dynam(i) == NOT_SET_R
        ldef  = ldef  .OR. rel%dynam(i) == DEFERRED_R
      END DO
      IF( ltest .OR. ldef )THEN
        eAction  = 'Please specify the release momentum and buoyancy'
        IF( ltest )THEN
          eMessage = 'No release dynamics specified'
          GOTO 9999
        END IF
        IF( ldef )THEN
          eMessage = 'Deferred release dynamics specified'
          GOTO 9998
        END IF
      END IF
    END IF
  ELSE IF( IsGas(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( project(EDIT_LEVEL_2)%Dynamic )THEN
      ltest = .FALSE.
      ldef  = .FALSE.
      DO i = 1,NUM_DYNAMIC
        ltest = ltest .OR. rel%dynam(i) == NOT_SET_R
        ldef  = ldef  .OR. rel%dynam(i) == DEFERRED_R
      END DO
      IF( ltest .OR. ldef )THEN
        eAction  = 'Please specify the release momentum and buoyancy'
        IF( ltest )THEN
          eMessage = 'No release dynamics specified'
          GOTO 9999
        END IF
        IF( ldef )THEN
          eMessage = 'Deferred release dynamics specified'
          GOTO 9998
        END IF
      END IF
    END IF
  ELSE IF( IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
    IF( project(EDIT_LEVEL_2)%Dynamic )THEN
      ltest = .FALSE.
      ldef  = .FALSE.
      IF( rel%dynam(1) == NOT_SET_R )rel%dynam(1) = DEF_VAL_R
      DO i = 1,NUM_DYNAMIC
        ltest = ltest .OR. rel%dynam(i) == NOT_SET_R
        ldef  = ldef  .OR. rel%dynam(i) == DEFERRED_R
      END DO
      IF( ltest .OR. ldef )THEN
        eAction  = 'Please specify the release momentum and buoyancy'
        IF( ltest )THEN
          eMessage = 'No release dynamics specified'
          GOTO 9999
        END IF
        IF( ldef )THEN
          eMessage = 'Deferred release dynamics specified'
          GOTO 9998
        END IF
      END IF
    END IF
    IF( rel%type(1:1) == 'I' )THEN
      ldef  = .TRUE.
      ltest = .FALSE.
      DO i = 1,3
        IF( rel%sig(i) == NOT_SET_R)rel%sig(i) = DEF_VAL_R
        ltest = ltest .OR. (rel%sig(i) == DEF_VAL_R)
        ldef  = ldef  .AND. (rel%sig(i) == DEF_VAL_R)
      END DO
      IF( ltest .AND. .NOT.ldef )THEN
        eMessage  = 'Inconsistent flashing aerosol definition'
        eInform   = 'If any size is set, all must be set'
        eAction   = 'Pleae enter a consistent definition'
        GOTO 9999
      END IF
    END IF
  END IF

  IF( rel%rate == NOT_SET_R )THEN
    eMessage = 'No release rate(mass)'
    eAction  = 'Please specify the release rate(mass)'
    GOTO 9999
  END IF
  IF( rel%rate == DEFERRED_R )THEN
    eMessage = 'Deferred release rate(mass)'
    eAction  = 'Please specify the release rate(mass)'
    GOTO 9998
  END IF

  rateCheck = .FALSE.
  IF( rel%type(1:1) == 'C' )THEN
    IF( rel%type(2:2) == 'P' )THEN
      IF( rel%rate <= 0.0 .AND. rel%rate /= NOT_SET_R )rateCheck = .TRUE.
    ELSE
      IF( rel%rate < 0.0 .AND. rel%rate /= NOT_SET_R )rateCheck = .TRUE.
    END IF
  ELSE
    IF( rel%rate <= 0.0 .AND. rel%rate /= NOT_SET_R )rateCheck = .TRUE.
  END IF
  IF( rateCheck )THEN
    eMessage = 'Invalid release rate(mass)'
    eAction  = 'Please specify a positive release rate(mass)'
    GOTO 9999
  END IF

  IF( rel%type(1:1) /= 'I' )THEN
!!DEC$ IF DEFINED (AEROSOL)
!    IF( IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
!      eMessage = 'Invalid release material (Aerosol release must be instantaneous)'
!      eAction  = 'Please change the release material'
!      GOTO 9999
!    END IF
!!DEC$ ENDIF
    IF( rel%dur == NOT_SET_R )THEN
      eMessage = 'No release duration'
      eAction  = 'Please specify the release duration in hours'
      GOTO 9999
    END IF
    IF( rel%dur == DEF_VAL_R .AND. .NOT.lpool )THEN
      eMessage = 'Default release duration is not valid'
      eAction  = 'Please specify the release duration in hours'
      GOTO 9999
    END IF
    IF( rel%dur == DEFERRED_R )THEN
      eMessage = 'Deferred release duration'
      eAction  = 'Please specify the release duration in hours'
      GOTO 9998
    END IF
  END IF

  IF( rel%type(1:1) == 'I' )THEN
    j = 1
  ELSE
    j = 2
  END IF
  ltest = .FALSE.
  ldef  = .FALSE.
  IF( lpool )THEN
    DO i = 1,j
      ltest = ltest .OR. rel%sig(i) == NOT_SET_R
      ldef  = ldef  .OR. rel%sig(i) == DEFERRED_R
    END DO
  ELSE IF( rel%type(2:2) == 'S' )THEN
    DO i = 1,1
      ltest = ltest .OR. rel%sig(i) == NOT_SET_R
      ldef  = ldef  .OR. rel%sig(i) == DEFERRED_R
    END DO
  ELSE IF( rel%type(2:2) == 'A' )THEN
    DO i = 1,3
      ldef  = ldef  .OR. rel%sig(i) == DEFERRED_R
    END DO
  ELSE
    DO i = j,3
      ltest = ltest .OR. rel%sig(i) == NOT_SET_R
      ldef  = ldef  .OR. rel%sig(i) == DEFERRED_R
    END DO
  END IF
  IF( ltest .OR. ldef )THEN
    IF( rel%type(2:2) == 'S' )THEN
      eAction  = 'Please specify the stack diameter'
    ELSE
      eAction  = 'Please specify at least 1 release sigma'
    END IF
    IF( ltest )THEN
      eMessage = 'No release size specified'
      GOTO 9999
    END IF
    IF( ldef )THEN
      eMessage = 'Deferred release size specified'
      GOTO 9998
    END IF
  END IF

  IF( rel%type(1:2) == 'CM' )THEN
    ltest = .FALSE.
    ldef  = .FALSE.
    DO i = 1,3
      ltest = ltest .OR. rel%vel(i) == NOT_SET_R
      ldef  = ldef  .OR. rel%vel(i) == DEFERRED_R
    END DO
    IF( ltest .OR. ldef )THEN
      eAction  = 'Please specify at least 1 release velocity'
      IF( ltest )THEN
        eMessage = 'No release velocity specified'
        GOTO 9999
      END IF
      IF( ldef )THEN
        eMessage = 'Deferred release velocity specified'
        GOTO 9998
      END IF
    END IF
    IF( rel%vel(3) < 0.0 )THEN
      ltest = rel%dur*3600.*rel%vel(3) < -rel%zRel
    ELSE
      ltest = .FALSE.
    END IF
    IF( ltest )THEN
      eMessage = 'End point of moving source trajectory below ground'
      eAction  = 'Please adjust velocity,location and/or duration'
      GOTO 9999
    END IF
  END IF

!!DEC$ IF DEFINED (AEROSOL)
!  IF( rel%type(1:1) == 'I' )THEN
!    IF( IsAerosol(materials(EDIT_LEVEL_1)%material(id)%icls) )THEN
!      DO i = 1,3
!        IF( rel%sig(i) /= DEFERRED_R .AND. rel%sig(i) < 0.0 )THEN
!          eMessage = 'Invalid Aerosol entrainment factor'
!          eAction  = 'Please specify an entrainment factor greater than or equal to 1.0'
!          GOTO 9999
!        END IF
!      END DO
!    END IF
!  END IF
!
!!DEC$ ENDIF
END IF

IF( lrnd )THEN
  IF( rel%param(REL_RAND_INDX) == NOT_SET_R )THEN
    eMessage = 'No number of random locations'
    eAction  = 'Please specify the number of random locations to be generated'
    GOTO 9999
  END IF
  IF( rel%param(REL_SPREAD_INDX) == NOT_SET_R )THEN
    eMessage = 'No random locations spread'
    eAction  = 'Please specify the spread of the random locations to be generated'
    GOTO 9999
  END IF
END IF

1000  RETURN

9999 CONTINUE
nError =  IV_ERROR
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )
lok = .FALSE.
GOTO 1000

9998 CONTINUE
nError =  IV_ERROR
eInform  = 'Deferred values are not allowed for analytic releases'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )
lok = .FALSE.
GOTO 1000

END
!*******************************************************************************
!            Initialize New Release Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_relnew( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI
USE GUImatl_fi
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, irv
TYPE( CMD ) MyCmd !Command Structure

INTEGER i
LOGICAL aeroOK
LOGICAL, EXTERNAL :: IsAerosol

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF
CALL clear_release_edit( jwnd_db,jd_level )
!
!     Radio Buttons
!
nradio(1,id_level) = 6 !New Type

IF( scenario(EDIT_LEVEL_2)%nrel > 0 )THEN
  ichoice(1,id_level) = 6 !COPY
  CALL build_release_list( iwnd_db,2,IDB_COMBO1 )
  irv = id_rel
  IF( irv <= scenario(EDIT_LEVEL_2)%nrel .AND. irv > 0 )THEN
    CALL SetListSelString( iwnd_db,IDB_COMBO1,scenario(EDIT_LEVEL_2)%release(irv)%string,irv )
    MyCmd%id     = IDB_RELNEW
    MyCmd%cntrl  = IDB_COMBO1
    MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
    MyCmd%button = MyCmd%cntrl - COMBO_BASE
    MyCmd%level  = id_level
    CALL process_combo( iwnd_db,MyCmd )
  ELSE
    ichoice(1,id_level) = 1 !Continuous
    CALL EnableControl( iwnd_db,IDB_RADIO06,FALSE )
    CALL EnableControl( iwnd_db,IDB_COMBO1 ,FALSE )
  END IF
ELSE
  ichoice(1,id_level) = 1 !Continuous
  CALL EnableControl( iwnd_db,IDB_RADIO06,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO1 ,FALSE )
END IF
aeroOK = .FALSE.
DO i = 1,materials(EDIT_LEVEL_1)%nmatl
  aeroOK = aeroOK .OR. IsAerosol(materials(EDIT_LEVEL_1)%material(i)%icls)
END DO
IF( .NOT.aeroOK )THEN
  CALL EnableControl(iwnd_db,IDB_RADIO05,FALSE)
END IF
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            save new release Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_relnew( id_level )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
!
!     This routine saves the new release Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

CHARACTER(128) new

INTEGER  id, jd_level
INTEGER(POINTER_LEN) jwnd_db
INTEGER  i, ios

CHARACTER(128), EXTERNAL ::StripNull
LOGICAL,        EXTERNAL ::hasError

!---- Get new type

SELECT CASE( ichoice(1,id_level) )

  CASE( 1 )
    cur_release      = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'C   '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1

  CASE( 2 )
    cur_release = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'I   '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1

  CASE( 3 )
    cur_release = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'CM  '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1

  CASE( 4 )
    cur_release = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'CS  '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1

  CASE( 5 )
    cur_release = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'IA  '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1
    cur_release%dynam(1) = DEF_VAL_R
    cur_release%sig(1) = DEF_VAL_R
    cur_release%sig(2) = DEF_VAL_R
    cur_release%sig(3) = DEF_VAL_R

  CASE( 99 )
    cur_release = scenario(DEFAULT_LEVEL)%release(1)
    cur_release%spec = REL_DATA
    cur_release%type = 'X   '
    cur_release%path = TRIM(path_tmp)
    cur_release%matl = materials(EDIT_LEVEL_1)%material(1)%cmat
    cur_release%indx = 1

  CASE DEFAULT
    new = StripNull( dbcmbo(1,id_level) )
    CALL find_release_list( new,id )
    IF( id > 0 .AND. id <= scenario(EDIT_LEVEL_2)%nrel )THEN
      cur_release = scenario(EDIT_LEVEL_2)%release(id)
      IF( cur_release%spec == REL_FILE )THEN
        IF( cur_release%file == '<empty>' )THEN
          cur_release%file = scenario(DEFAULT_LEVEL)%release(1)%file
          cur_release%path = scenario(DEFAULT_LEVEL)%release(1)%path
        END IF
      END IF
      NULLIFY(cur_release%mc)
      IF( ASSOCIATED(scenario(EDIT_LEVEL_2)%release(id)%mc) )THEN
        ALLOCATE(cur_release%mc(SIZE(scenario(EDIT_LEVEL_2)%release(id)%mc)),STAT=ios)
        DO i = 1,SIZE(cur_release%mc)
          cur_release%mc(i) = scenario(EDIT_LEVEL_2)%release(id)%mc(i)
        END DO
      END IF
    ELSE
      GOTO 9999
    END IF

END SELECT

CALL set_release_indx( cur_release,materials(EDIT_LEVEL_1),.FALSE. )
CALL release_string( cur_release,materials(EDIT_LEVEL_1) )

IF( (scenario(EDIT_LEVEL_2)%nrel + 1) > MAXREL )THEN
  CALL ReallocateScenario(scenario(EDIT_LEVEL_2)%nrel + 10)
  IF( hasError() )CALL ShowInfoMessage(jwnd_db)
END IF

id_rel = scenario(EDIT_LEVEL_2)%nrel + 1
CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF
CALL init_release_edit( jwnd_db,jd_level )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Check Release Definitions
!*******************************************************************************
SUBROUTINE check_release_definition( iwnd_db,nrel,rel,lOK )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER(POINTER_LEN),                 INTENT( IN  ) :: iwnd_db
INTEGER,                              INTENT( IN  ) :: nrel
TYPE( release_str ), DIMENSION(nrel), INTENT( IN  ) :: rel
LOGICAL,                              INTENT( OUT ) :: lOK

!     This routine checks the release definitions - very simple for now

INTEGER i
REAL    tlast

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckReleaseDefinition'

lOK = .TRUE.

IF( nrel > 0 )THEN
  lOK = rel(1)%time >= 0
  IF( lOK )THEN
    tlast = NOT_SET_R
    DO i = 1,nrel
      IF( rel(i)%time < tlast )THEN
        lOK = .FALSE.
        nError = IV_ERROR
        eMessage = 'Release times not in sequence'
        WRITE(string1,*)rel(i)%time,tlast
        string1 = ADJUSTL(string1)
        eInform = 'Times='//TRIM(string1)
        GOTO 9999
      ELSE
        tlast = rel(i)%time
      END IF
    END DO
  ELSE
    nError = IV_ERROR
    eMessage = 'First release must be at T=0.0'
  END IF
END IF

9999 CONTINUE

IF( .NOT.lOK )THEN
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowErrorMessage( iwnd_db )
END IF

RETURN
END
!*******************************************************************************
!            Find maximum release height
!*******************************************************************************
SUBROUTINE zmax_release( id_level,zmax )

USE resource_fd
USE pcscipuf_fi
USE create_fi
!
!     This routine saves the new release Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN  ) :: id_level !Data level
REAL ,   INTENT( OUT ) :: zmax

INTEGER i
REAL    z

zmax = 0.
DO i = 1,scenario(id_level)%nrel
  z = scenario(id_level)%release(i)%zRel
  IF( z /= DEF_VAL_R .AND. z /= DEFERRED_R .AND. z /= NOT_SET_R )zmax = MAX(zmax,z)
END DO

RETURN
END
!*******************************************************************************
!                     Set Release String from Data
!*******************************************************************************
SUBROUTINE release_string( rel,matdlg )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fd

IMPLICIT NONE

TYPE( release_str ), INTENT( INOUT ) :: rel
TYPE( matdef_str ),  INTENT( IN    ) :: matdlg

CHARACTER(1), PARAMETER  :: DOT = '.'

INTEGER nch, indx, i, j, ix, iwrt, id, icls
CHARACTER(32) number

LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( rel%defName )THEN
!---- Clear String

  rel%string = ' '

!---- Get material type

  CALL find_material_list( matdlg%material,matdlg%nmatl,rel%matl,id )
  IF( id <= 0 .OR. id > matdlg%nmatl )THEN
    icls = 0
  ELSE
    icls = matdlg%material(id)%icls
  END IF

!---- Add Identifier Slot

  rel%string = 'R0000'
  iwrt = 7

!---- Add Release Type

  IF( rel%type(2:2) /= ' ' )THEN
    rel%string(iwrt:iwrt) = rel%type(2:2)
  ELSE
    rel%string(iwrt:iwrt) = rel%type(1:1)
  END IF
  iwrt = iwrt + 1

!---- Add Material/Group

  IF( rel%file == ' ' .OR. rel%file == ' ' )THEN
    string1 = ' '//TRIM(rel%matl)
    IF( rel%indx > 0 )THEN
      IF( rel%distrib <= 0 )THEN
        WRITE(number,*)rel%indx
        string2 = TRIM(string1)//'('//TRIM(ADJUSTL(number))//')'
      ELSE IF( rel%distrib <= MAXDISTRIBUTION )THEN
        SELECT CASE( rel%distrib )

          CASE( REL_LOGNORM )
            string2 = TRIM(string1)//'(LN)'

          CASE DEFAULT
            string2 = TRIM(string1)//'(??)'

        END SELECT
      ELSE
        IF( IsLiquid(icls) )THEN
          IF( rel%distrib == MAXDISTRIBUTION + REL_GASPHASE )THEN
            string2 = TRIM(string1)//'(G)'
          ELSE IF( rel%distrib == MAXDISTRIBUTION + REL_LIQUIDPOOL )THEN
            string2 = TRIM(string1)//'(P)'
          ELSE
            string2 = TRIM(string1)//'(??)'
          END IF
        ELSE IF( IsWetParticle(icls) )THEN
          IF( rel%distrib == MAXDISTRIBUTION + REL_SLURRY )THEN
            string2 = TRIM(string1)//'(S)'
          ELSE
            string2 = TRIM(string1)//'(??)'
          END IF
        ELSE
          string2 = TRIM(string1)//'(??)'
        END IF
      END IF
      string1 = TRIM(string2)
    END IF
  ELSE
    string1 = ' '//TRIM(rel%file)
  END IF

  j = MIN(LEN(rel%string),LEN(TRIM(string1))+iwrt-1)
  rel%string(iwrt:j) = TRIM(string1)

  iwrt = MAX(iwrt+14,j+1)

!---- Add Release time

  CALL format_time( rel%time,string1,1 )
  indx = INDEX(string1,DOT)
  nch = MIN(LEN_TRIM(string1),6)
  i = MAX(iwrt+2 - indx + 1,iwrt)
  j = MIN(i + nch - 1,iwrt+5)
  rel%string(i:j) = string1(1:nch)
  iwrt = iwrt + 7

!---- Add Release Location

  CALL c_format( SNGL(rel%xRel),nch,number )
  indx = INDEX(number,DOT)
  nch  = MIN(nch,7)
  i = MAX(iwrt + 4 - indx + 1,iwrt)
  j = MIN(i + nch - 1,iwrt+6)
  rel%string(i:j) = number(1:nch)
  iwrt = iwrt + 7

  CALL c_format( SNGL(rel%yRel),nch,number )
  indx = INDEX(number,DOT)
  nch  = MIN(nch,7)
  i = MAX(iwrt + 4 - indx + 1,iwrt)
  j = MIN(i + nch - 1,iwrt+6)
  rel%string(i:j) = number(1:nch)
  iwrt = iwrt + 7

  CALL c_format( rel%zRel,nch,number )
  indx = INDEX(number,DOT)
  nch  = MIN(nch,7)
  i = MAX(iwrt + 4 - indx + 1,iwrt)
  j = MIN(i + nch - 1,iwrt+6)
  rel%string(i:j) = number(1:nch)
  iwrt = iwrt + 7

ELSE

  rel%string(1:6) = 'R0000 '

END IF

RETURN
END
!*******************************************************************************
!            Show Distribution controls
!*******************************************************************************
SUBROUTINE show_distribution( iwnd_db,iflag,icls )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag   !Distribution index
INTEGER,              INTENT( IN ) :: icls

INTEGER ishow, ienable, i, jshow, jenable

LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsLiquid

SELECT CASE( iflag )

  CASE( REL_LOGNORM )
    ishow   = SW_SHOWNORMAL
    ienable = TRUE
    CALL SetControlText( iwnd_db,IDB_STATIC36,'MMD' )
    CALL SetControlText( iwnd_db,IDB_STATIC37,'microns' )
    CALL SetControlText( iwnd_db,IDB_STATIC38,'Sigma' )
    IF( IsLiquid(icls) )THEN
      jshow   = SW_SHOWNORMAL
      jenable = TRUE
      CALL SetControlText( iwnd_db,IDB_STATIC39,'LiqFraction' )
    ELSE
      jshow   = SW_HIDE
      jenable = FALSE
      CALL SetControlText( iwnd_db,IDB_STATIC39,' ' )
    END IF

  CASE( MAXDISTRIBUTION+REL_SLURRY )
    IF( IsWetParticle(icls) )THEN
      ishow   = SW_SHOWNORMAL
      ienable = TRUE
      CALL SetControlText( iwnd_db,IDB_STATIC36,'MMD' )
      CALL SetControlText( iwnd_db,IDB_STATIC37,'microns' )
      CALL SetControlText( iwnd_db,IDB_STATIC38,'Sigma' )
      jshow   = SW_SHOWNORMAL
      jenable = TRUE
      CALL SetControlText( iwnd_db,IDB_STATIC39,'DryFraction' )
    ELSE
      ishow   = SW_HIDE
      ienable = FALSE
      CALL SetControlText( iwnd_db,IDB_STATIC36,' ' )
      CALL SetControlText( iwnd_db,IDB_STATIC37,' ' )
      CALL SetControlText( iwnd_db,IDB_STATIC38,' ' )
      jshow   = SW_HIDE
      jenable = FALSE
      CALL SetControlText (iwnd_db,IDB_STATIC39,' ' )
    END IF

  CASE DEFAULT
    ishow   = SW_HIDE
    ienable = FALSE
    CALL SetControlText( iwnd_db,IDB_STATIC36,' ' )
    CALL SetControlText( iwnd_db,IDB_STATIC37,' ' )
    CALL SetControlText( iwnd_db,IDB_STATIC38,' ' )
    IF( IsWetParticle(icls) )THEN
      jshow   = SW_SHOWNORMAL
      jenable = TRUE
      CALL SetControlText( iwnd_db,IDB_STATIC39,'DryFraction' )
    ELSE IF( IsLiquid(icls) .AND. iflag < 0 )THEN
      jshow   = SW_SHOWNORMAL
      jenable = TRUE
      CALL SetControlText( iwnd_db,IDB_STATIC39,'LiqFraction' )
    ELSE
      jshow   = SW_HIDE
      jenable = FALSE
      CALL SetControlText( iwnd_db,IDB_STATIC39,' ' )
    END IF

END SELECT

CALL EnableControl( iwnd_db,IDB_REAL13,ienable )
CALL EnableControl( iwnd_db,IDB_REAL14,ienable )
CALL ShowControl(   iwnd_db,IDB_REAL13,ishow )
CALL ShowControl(   iwnd_db,IDB_REAL14,ishow )

CALL EnableControl( iwnd_db,IDB_REAL23,jenable )
CALL ShowControl(   iwnd_db,IDB_REAL23,jshow )

DO i = 36,38
  CALL ShowControl( iwnd_db,STATIC_BASE+i,ishow )
END DO

CALL ShowControl( iwnd_db,STATIC_BASE+39,jshow )

RETURN
END
!*******************************************************************************
!            Show Liquid Pool controls
!*******************************************************************************
SUBROUTINE show_liquid_pool( iwnd_db,lpool,lstack,ilev,jenable )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: ilev    !Distribution index
INTEGER,              INTENT( IN ) :: jenable !Display flag
LOGICAL,              INTENT( IN ) :: lpool   !Pool flag
LOGICAL,              INTENT( IN ) :: lstack  !Stack flag

INTEGER ishow, ienable

IF( cur_release%type(1:1) /= 'I' .AND. cur_release%type(1:1) /= 'X' )THEN

  IF( lpool )THEN
    ishow   = SW_HIDE
    ienable = FALSE
    CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass :' )
    string1 = 'N/A'
    CALL EnableControl( iwnd_db,REAL_BASE+9,TRUE )
    CALL SetEditTs( iwnd_db,string1,REAL_BASE-EDIT_BASE+9,1 )
    CALL EnableControl( iwnd_db,REAL_BASE+9,FALSE )
    CALL ShowControl( iwnd_db,IDB_STATIC67,SW_SHOWNORMAL )   !Active Fraction off
    CALL EnableControl( iwnd_db,IDB_REAL26,jenable )
    CALL ShowControl( iwnd_db,IDB_REAL26,SW_SHOWNORMAL )

    CALL EnableControl( iwnd_db,REAL_BASE+7,jenable )
    CALL SetEditRs( iwnd_db,dbreal(7,ilev),7,1 )
  ELSE
    ishow   = SW_SHOWNORMAL
    ienable = jenable
    CALL SetControlText( iwnd_db,IDB_STATIC40,'Rate :' )
    IF( .NOT.lstack )THEN
      string1 = 'N/A'
      CALL EnableControl( iwnd_db,REAL_BASE+7,TRUE )
      CALL SetEditTs( iwnd_db,string1,REAL_BASE-EDIT_BASE+7,1 )
      CALL EnableControl( iwnd_db,REAL_BASE+7,FALSE )
      CALL EnableControl( iwnd_db,REAL_BASE+9,jenable )
      CALL SetEditRs( iwnd_db,dbreal(9,ilev),9,1 )
    END IF
    CALL ShowControl( iwnd_db,IDB_STATIC67,SW_SHOWNORMAL )   !Active Fraction on
    CALL EnableControl( iwnd_db,IDB_REAL26,jenable )
    CALL ShowControl( iwnd_db,IDB_REAL26,SW_SHOWNORMAL )

  END IF

  CALL EnableControl( iwnd_db,IDB_REAL6,ienable )
  CALL EnableControl( iwnd_db,IDB_COMBO7,ienable )
  CALL ShowControl( iwnd_db,IDB_REAL6,ishow )
  CALL ShowControl( iwnd_db,IDB_COMBO7,ishow )

  CALL ShowControl( iwnd_db,IDB_STATIC42,ishow )

ELSE

  ishow   = SW_SHOWNORMAL
  ienable = jenable
  IF( lcheck(1,ilev) )THEN
    CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass/location :' )
  ELSE
    CALL SetControlText( iwnd_db,IDB_STATIC40,'Mass :' )
  END IF
  CALL EnableControl( iwnd_db,REAL_BASE+7,TRUE )
  CALL EnableControl( iwnd_db,REAL_BASE+9,TRUE )
  CALL SetEditRs( iwnd_db,dbreal(7,ilev),7,3 )
  CALL EnableControl( iwnd_db,REAL_BASE+7,jenable )
  CALL EnableControl( iwnd_db,REAL_BASE+9,jenable )
  CALL ShowControl( iwnd_db,IDB_STATIC67,SW_SHOWNORMAL )   !Active Fraction on
  CALL EnableControl( iwnd_db,IDB_REAL26,jenable )
  CALL ShowControl( iwnd_db,IDB_REAL26,SW_SHOWNORMAL )

END IF

CALL EnableControl( iwnd_db,IDB_REAL4,ienable )
CALL ShowControl( iwnd_db,IDB_REAL4,ishow )

CALL ShowControl( iwnd_db,IDB_STATIC22,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC24,ishow )
CALL ShowControl( iwnd_db,IDB_STATIC26,ishow )

RETURN
END
!*******************************************************************************
!            Show Dynamics
!*******************************************************************************
SUBROUTINE show_dynamic( iwnd_db,iflag,jflag,frac )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI
USE relparam_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag !Class
INTEGER,              INTENT( IN ) :: jflag !Distribution index
REAL,                 INTENT( IN ) :: frac  !dry/liqFraction

INTEGER ishow, ienable, i
LOGICAL hasVapor
LOGICAL stack
INTEGER iend

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid

string3 = 'Momentum :'
string4 = 'Buoyancy :'
stack = .FALSE.
iend  = 56
IF( IsGas(iflag) )THEN
  ishow   = SW_SHOWNORMAL
  ienable = TRUE
  IF( cur_release%type(1:1) /= 'I' )THEN
    IF( cur_release%type(2:2) /= 'S' )THEN
      string1 = 'm4/s2'
      string2 = 'C-m3/s'
    ELSE
      string1 = 'm/s'
      string2 = 'C'
      string4 = 'Exit Temp:'
      string3 = 'X Velocity :'
      stack = .TRUE.
      iend  = 62
    END IF
  ELSE
    string1 = 'm4/s'
    string2 = 'C-m3'
  END IF
ELSE IF( IsAerosol(iflag) )THEN
  ishow   = SW_SHOWNORMAL
  ienable = TRUE
  IF( cur_release%type(1:1) /= 'I' )THEN
    IF( cur_release%type(2:2) /= 'S' )THEN
      string1 = 'm4/s2'
      string2 = 'C-m3/s'
    ELSE
      string1 = 'm/s'
      string2 = 'C'
      string4 = 'Exit Temp:'
      string3 = 'X Velocity :'
      stack = .TRUE.
      iend  = 62
    END IF
  ELSE
    IF( cur_release%type(2:2) == 'A' )THEN
      string2 = 'C (Storage)'
      string4 = 'Temp. :'
    ELSE
      string2 = 'C-m3'
    END IF
    string1 = 'm4/s'
  END IF
ELSE IF( IsLiquid(iflag) )THEN
!  SELECT CASE( jflag )
  hasVapor = frac >= 0.0 .AND. jflag <= MAXDISTRIBUTION
  hasVapor = hasVapor .AND. frac < 1.0
  IF( hasVapor .OR. jflag==MAXDISTRIBUTION+REL_GASPHASE )THEN
!    CASE( MAXDISTRIBUTION+REL_GASPHASE )
      ishow   = SW_SHOWNORMAL
      ienable = TRUE
      IF( cur_release%type(1:1) /= 'I' )THEN
        IF( cur_release%type(2:2) /= 'S' )THEN
          string1 = 'm4/s2'
          string2 = 'C-m3/s'
        ELSE
          string1 = 'm/s'
          string2 = 'C'
          string4 = 'Exit Temp:'
          string3 = 'X Velocity :'
          stack = .TRUE.
          iend  = 62
        END IF
      ELSE
        string1 = 'm4/s'
        string2 = 'C-m3'
      END IF
  ELSE
!    CASE DEFAULT
      ishow   = SW_HIDE
      ienable = FALSE
      string1 = ' '
      string2 = ' '
  END IF
!  END SELECT
ELSE
  ishow   = SW_HIDE
  ienable = FALSE
  string1 = ' '
  string2 = ' '
END IF

CALL SetControlText( iwnd_db,IDB_STATIC52,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC54,string2 )
CALL SetControlText( iwnd_db,IDB_STATIC51,string3 )
CALL SetControlText( iwnd_db,IDB_STATIC53,string4 )
IF( stack )THEN
  CALL SetControlText( iwnd_db,IDB_STATIC59,string1 )
  CALL SetControlText( iwnd_db,IDB_STATIC60,string1 )
END IF

CALL EnableControl( iwnd_db,IDB_REAL17,ienable )
CALL EnableControl( iwnd_db,IDB_REAL18,ienable )
CALL ShowControl( iwnd_db,IDB_REAL17,ishow )
CALL ShowControl( iwnd_db,IDB_REAL18,ishow )
IF( stack )THEN
  CALL EnableControl( iwnd_db,IDB_REAL19,ienable )
  CALL EnableControl( iwnd_db,IDB_REAL20,ienable )
  CALL ShowControl( iwnd_db,IDB_REAL19,ishow )
  CALL ShowControl( iwnd_db,IDB_REAL20,ishow )
ELSE
  CALL EnableControl( iwnd_db,IDB_REAL19,FALSE )
  CALL EnableControl( iwnd_db,IDB_REAL20,FALSE )
  CALL ShowControl( iwnd_db,IDB_REAL19,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_REAL20,SW_HIDE )
END IF

DO i = 51,iend
  CALL ShowControl( iwnd_db,STATIC_BASE+i,ishow )
END DO
DO i = iend+1,62
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO

RETURN
END
!*******************************************************************************
!            Show multicomp
!*******************************************************************************
SUBROUTINE show_multicomp( iwnd_db,iflag )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: iflag   !nmc

INTEGER ishow, ienable

IF( iflag /= 0 )THEN
  ishow   = SW_SHOWNORMAL
  ienable = TRUE
ELSE
  ishow   = SW_HIDE
  ienable = FALSE
END IF

CALL EnableControl( iwnd_db,IDB_BUTTON7,ienable )
CALL ShowControl( iwnd_db,IDB_BUTTON7,ishow )

RETURN
END
!*******************************************************************************
!            Compute Deg/Min/Sec
!*******************************************************************************
SUBROUTINE compute_DMS( deg,ideg,imin,sec )

USE resource_fd

!     This routine loads the selection into current release

IMPLICIT NONE

REAL,    INTENT( IN  ) :: deg
INTEGER, INTENT( OUT ) :: ideg
INTEGER, INTENT( OUT ) :: imin
REAL,    INTENT( OUT ) :: sec

IF( deg == NOT_SET_R )THEN
  ideg = NOT_SET_I
  imin = NOT_SET_I
  sec  = NOT_SET_R
ELSE IF( deg == DEF_VAL_R )THEN
  ideg = NOT_SET_I
  imin = NOT_SET_I
  sec  = DEF_VAL_R
ELSE
  ideg = INT(deg)
  imin = INT( 60.*ABS(deg - FLOAT(ideg)) )
  sec  = 60.*(60.*ABS(deg - FLOAT(ideg)) - FLOAT(imin))
END IF

RETURN
END
!*******************************************************************************
!            Compute Deg/Min/Sec
!*******************************************************************************
SUBROUTINE compute_DMS8( deg,ideg,imin,sec )

USE resource_fd

!     This routine loads the selection into current release

IMPLICIT NONE

REAL(8), INTENT( IN  ) :: deg
INTEGER, INTENT( OUT ) :: ideg
INTEGER, INTENT( OUT ) :: imin
REAL,    INTENT( OUT ) :: sec

IF( deg == NOT_SET_D )THEN
  ideg = NOT_SET_I
  imin = NOT_SET_I
  sec  = NOT_SET_R
ELSE IF( deg == DEF_VAL_D )THEN
  ideg = NOT_SET_I
  imin = NOT_SET_I
  sec  = DEF_VAL_R
ELSE
  ideg = INT(deg)
  imin = INT( 60.D0*ABS(deg - DFLOAT(ideg)) )
  sec  = SNGL(60.D0*(60.D0*ABS(deg - DFLOAT(ideg)) - DFLOAT(imin)))
END IF

RETURN
END
!*******************************************************************************
!            Compute Degrees
!*******************************************************************************
SUBROUTINE compute_degrees( ideg,imin,sec,deg )

USE resource_fd

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ideg
INTEGER, INTENT( IN  ) :: imin
REAL,    INTENT( IN  ) :: sec
REAL,    INTENT( OUT ) :: deg

IF( ideg == DEF_VAL_I .OR. imin == DEF_VAL_I .OR. sec == DEF_VAL_R )THEN
  deg = DEF_VAL_R
ELSE IF( ideg == NOT_SET_I .OR. imin == NOT_SET_I .OR. sec == NOT_SET_R )THEN
  deg = NOT_SET_R
ELSE
  deg = FLOAT(ABS(ideg)) + FLOAT(imin)/60. + sec/3600.
  deg = SIGN(deg,FLOAT(ideg))
END IF

RETURN
END
!*******************************************************************************
!            Compute Degrees
!*******************************************************************************
SUBROUTINE compute_degrees8( ideg,imin,sec,deg )

USE resource_fd

!     This routine loads the selection into current release

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ideg
INTEGER, INTENT( IN  ) :: imin
REAL,    INTENT( IN  ) :: sec
REAL(8), INTENT( OUT ) :: deg

IF( ideg == DEF_VAL_I .OR. imin == DEF_VAL_I .OR. sec == DEF_VAL_R )THEN
  deg = DEF_VAL_D
ELSE IF( ideg == NOT_SET_I .OR. imin == NOT_SET_I .OR. sec == NOT_SET_R )THEN
  deg = NOT_SET_D
ELSE
  deg = DFLOAT(ABS(ideg)) + DFLOAT(imin)/60.D0 + sec/3600.D0
  deg = SIGN(deg,DFLOAT(ideg))
END IF

RETURN
END
!*******************************************************************************
!            Initialize Random location parameters Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_random( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE SwinAPI
USE randef
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level
REAL    totMass

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF

IF( dbreal(24,jd_level) == NOT_SET_R )THEN
  dbint(1,id_level) = NOT_SET_I
ELSE IF( dbreal(24,jd_level) == DEF_VAL_R )THEN
  dbint(1,id_level) = DEF_VAL_I
ELSE
  dbint(1,id_level) = NINT(dbreal(24,jd_level))
END IF

dbint(2,id_level) = dbint(8,jd_level)
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,2 )
dbreal(1,id_level) = dbreal(25,jd_level)
CALL SetEditRs(iwnd_db,dbreal( 1,id_level),1,1 )

CALL EnableControl( iwnd_db,IDB_INT1,save_flag(2) )
CALL EnableControl( iwnd_db,IDB_REAL1,save_flag(3) )
CALL EnableControl( iwnd_db,IDB_INT2,save_flag(4) )
IF( save_flag(1) == FALSE )THEN
  CALL EnableControl( iwnd_db,ID_OK,FALSE )
  CALL ShowControl(   iwnd_db,ID_OK,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
END IF

CALL ShowControl( iwnd_db,IDB_STATIC21,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC22,SW_HIDE )
IF( dbreal(5,jd_level) /= NOT_SET_R .AND. &
    dbreal(5,jd_level) /= DEF_VAL_R .AND. &
    dbreal(5,jd_level) /= DEFERRED_R )THEN
  IF( dbint(1,id_level) /= NOT_SET_I .AND. &
      dbint(1,id_level) /= DEF_VAL_I .AND. &
      dbint(1,id_level) /= DEFERRED_I )THEN

    totMass = FLOAT(dbint(1,id_level))*dbreal(5,jd_level)

    WRITE(string1,*)totMass
    CALL SetControlText( iwnd_db,IDB_STATIC22,TRIM(ADJUSTL(string1)) )
    CALL ShowControl( iwnd_db,IDB_STATIC21,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,IDB_STATIC22,SW_SHOWNORMAL )

  END IF
END IF

RETURN
END
!*******************************************************************************
!            Save Random location parameters Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_random( id_level )

USE resource_fd
USE pcscipuf_fi
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF

IF( dbint(1,id_level) == NOT_SET_I )THEN
  dbreal(24,jd_level) = NOT_SET_R
ELSE IF( dbint(1,id_level) == DEF_VAL_I )THEN
  dbreal(24,jd_level) = DEF_VAL_R
ELSE
  dbreal(24,jd_level) = FLOAT(dbint(1,id_level))
END IF
dbreal(25,jd_level) = dbreal(1,id_level)
IF( dbint(2,id_level) == NOT_SET_I )THEN
  CALL new_seed( dbint(8,jd_level),ran_seed )
ELSE
  dbint(8,jd_level) = dbint(2,id_level)
END IF

RETURN
END
!*******************************************************************************
!            Initialize Uncertainty Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_uncertainty( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE SwinAPI
!
!     This routine initializes the Source Uncertainty Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level
LOGICAL EditMode

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF
!
!     Buttons
!
CALL IsControlVisible( jwnd_db,IDB_BUTTON11,EditMode )
IF( .NOT.EditMode )THEN
  CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE )
  CALL ShowControl( iwnd_db,IDB_BUTTON1,SW_HIDE )
  CALL EnableControl( iwnd_db,ID_OK,FALSE )
  CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
END IF
!
!     Edit Boxes - Reals
!
dbreal(1,id_level) = dbreal(21,jd_level) !Horiz
dbreal(2,id_level) = dbreal(22,jd_level) !Verts
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,2 )
IF( .NOT.EditMode )THEN
  CALL EnableControl( iwnd_db,IDB_REAL1,FALSE )
  CALL EnableControl( iwnd_db,IDB_REAL2,FALSE )
END IF

RETURN
END
!*******************************************************************************
!            Save Uncertainty Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_uncertainty( id_level )

USE resource_fd
USE pcscipuf_fi
!
!     This routine initializes the Source Uncertainty Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF
dbreal(21,jd_level) = dbreal(1,id_level) !Horiz
dbreal(22,jd_level) = dbreal(2,id_level) !Verts
!
!     Edit Boxes - Reals
!
RETURN
END
!*******************************************************************************
!            Initialize Multicomponent parameters Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_multicomp( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
USE SwinAPI
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, irv, i
LOGICAL EditMode

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF

!==== Build list if in Edit mode

iEdit = -1
CALL ClearList( iwnd_db,IDB_LIST1 )
nEdit = materials(EDIT_LEVEL_1)%material(id_relMat)%nmc

IF( nEdit > 0 )THEN
  DO i = 1,nEdit
    dblst(id_level,i) = dblst(jd_level,i)
    WRITE(string2,'(1PG10.3)')dblst(id_level,i)
    string2 = ADJUSTL(string2)
    IF( string2(1:1) == '.' )THEN
      string1 = '0'//TRIM(string2)
      string2 = TRIM(string1)
    END IF
    string1 = materials(EDIT_LEVEL_1)%materialMC(id_relMat)%name(i)(1:8)//' '//TRIM(string2)
    CALL AddList( iwnd_db,IDB_LIST1,-1,string1,irv )
  END DO
END IF

CALL SetControlFont( iwnd_db,IDB_LIST1,fixfont )
CALL SetListCurSel( iwnd_db,IDB_LIST1,0 )

!==== Hide Edit

CALL hide_multicomp_edit( iwnd_db )

!==== Enable edits

CALL IsControlVisible( jwnd_db,IDB_BUTTON11,EditMode )
IF( .NOT.EditMode )THEN
  CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE )
  CALL ShowControl( iwnd_db,IDB_BUTTON1,SW_HIDE )
  CALL EnableControl( iwnd_db,ID_OK,FALSE )
  CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
END IF

RETURN
END
!*******************************************************************************
!            Hide Multicomponent parameters Dialog Box
!*******************************************************************************
SUBROUTINE hide_multicomp_edit( iwnd_db )

USE resource_fd
USE SwinAPI
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

mEdit = .FALSE.
CALL SetControlText( iwnd_db,IDB_BUTTON1,'&Edit' )

CALL EnableControl( iwnd_db,IDB_LIST1,TRUE )

CALL EnableControl( iwnd_db,IDB_STATIC02,FALSE )
CALL EnableControl( iwnd_db,IDB_REAL1,FALSE )
CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )

CALL ShowControl( iwnd_db,IDB_STATIC02,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_REAL1,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_BUTTON2,SW_HIDE )

RETURN
END
!*******************************************************************************
!            Show Multicomponent parameters Dialog Box
!*******************************************************************************
SUBROUTINE show_multicomp_edit( iwnd_db )

USE resource_fd
USE SwinAPI
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

mEdit = .TRUE.
CALL SetControlText( iwnd_db,IDB_BUTTON1,'&Save' )

CALL EnableControl( iwnd_db,IDB_LIST1,FALSE )

CALL EnableControl( iwnd_db,IDB_STATIC02,TRUE )
CALL EnableControl( iwnd_db,IDB_REAL1,TRUE )
CALL EnableControl( iwnd_db,IDB_BUTTON2,TRUE )

CALL ShowControl( iwnd_db,IDB_STATIC02,SW_SHOWNORMAL )
CALL ShowControl( iwnd_db,IDB_REAL1,SW_SHOWNORMAL )
CALL ShowControl( iwnd_db,IDB_BUTTON2,SW_SHOWNORMAL )

RETURN
END
!*******************************************************************************
!            Save Multicomponent parameters Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_multicomp( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE create_fi
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, i

CALL FindHwndListId( IDB_RELDEF,jwnd_db,jd_level ) !  Parent = RELDEF

IF( mEdit )THEN
  CALL save_multicomp_edit( iwnd_db,id_level )
  CALL hide_multicomp_edit( iwnd_db )
END IF

DO i = 1,nEdit
  dblst(jd_level,i) = dblst(id_level,i)
END DO

RETURN
END
!*******************************************************************************
!            init Multicomponent edit Dialog Box
!*******************************************************************************
SUBROUTINE init_multicomp_edit( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE errorParam_fd
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

CALL GetListCurSel( iwnd_db,IDB_LIST1,iEdit )

IF( iEdit >= 0 )THEN
  iEdit = iEdit + 1
  iEdit = MAX(iEdit,0)
  iEdit = MIN(iEdit,nEdit)
  dbreal(1,id_level) = dblst(id_level,iEdit)
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )
ELSE
  CALL SetError( NF_ERROR, &
                'No species selected for edit', &
                'Please select a species first',' ', &
                'MulticompEdit' )
  CALL ShowErrorMessage( iwnd_db )
  CALL hide_multicomp_edit( iwnd_db )
END IF

RETURN
END
!*******************************************************************************
!            Save Multicomponent edit Dialog Box
!*******************************************************************************
SUBROUTINE save_multicomp_edit( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE multicmn
!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv, indx

dblst(id_level,iEdit) = dbreal(1,id_level)
WRITE(string2,'(1PG10.3)')dblst(id_level,iEdit)
string2 = ADJUSTL(string2)
IF( string2(1:1) == '.' )THEN
  string1 = '0'//TRIM(string2)
  string2 = TRIM(string1)
END IF
indx = iEdit - 1

CALL GetListItem( iwnd_db,IDB_LIST1,indx,string3,irv )
string1 = string3(1:8)//' '//TRIM(string2)
CALL DeleteList( iwnd_db,IDB_LIST1,indx,irv )
CALL AddList( iwnd_db,IDB_LIST1,indx,string1,irv )
CALL SetListCurSel( iwnd_db,IDB_LIST1,indx )

RETURN
END
!***********************************************************************
!               MulticompButton
!***********************************************************************
SUBROUTINE multicomp_button( iwnd_db,id_button,id_level )

!     This routine processes PUSHBUTTONs from MULTCOMP Dialog Box
USE winAPI_fd, ONLY: POINTER_LEN
USE multicmn

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)

!---- Select by Button number

SELECT CASE( id_button )

  CASE( 1 ) !Edit/Save
    IF( mEdit )THEN
      CALL save_multicomp_edit( iwnd_db,id_level )
      CALL hide_multicomp_edit( iwnd_db )
    ELSE
      CALL show_multicomp_edit( iwnd_db )
      CALL init_multicomp_edit( iwnd_db,id_level )
    END IF

  CASE( 2 ) !Cancel
    CALL hide_multicomp_edit( iwnd_db )

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!               RandomButton
!***********************************************************************
SUBROUTINE random_button( iwnd_db,id_button,id_level )

USE pcscipuf_fi

!     This routine processes PUSHBUTTONs from RNDPARM Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)

!---- Select by Button number

SELECT CASE( id_button )

  CASE( 1 ) !New seed
    CALL new_seed( dbint(2,id_level),ran_seed )
    CALL SetEditIs( iwnd_db,dbint(2,id_level),2,1 )

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!               UncertaintyButton
!***********************************************************************
SUBROUTINE uncertainty_button( iwnd_db,id_button,id_level )

USE pcscipuf_fi

!     This routine processes PUSHBUTTONs from RNDPARM Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

!---- Select by Button number

SELECT CASE( id_button )

  CASE( 1 ) !Default
    dbreal(1,id_level)= 0.
    dbreal(2,id_level)= 0.
    CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,2 )

  CASE DEFAULT

END SELECT

RETURN
END

!===============================================================================

SUBROUTINE check_buoyant( iwnd_db,id_level,l_askUser )

USE resource_fd
USE errorParam_fd
USE pcscipuf_fi
USE create_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: id_level
LOGICAL,              INTENT( IN ) :: l_askUser

LOGICAL ldyn, lneg
INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, i, j

LOGICAL, EXTERNAL :: hasError

!---- If Dynamic flag already set - no action needed

IF( project(id_level)%Dynamic )RETURN

!---- Loop over releases checking for dynamics

ldyn = .FALSE.
lneg = .FALSE.
DO i = 1,scenario(id_level)%nrel
  IF( scenario(id_level)%release(i)%spec == REL_DATA )THEN
    DO j = 1,NUM_DYNAMIC
      ldyn = ldyn .OR. scenario(id_level)%release(i)%dynam(j) /= 0.0
      lneg = lneg .OR. scenario(id_level)%release(i)%dynam(j) < 0.0
    END DO
  END IF
END DO

!---- Found Dynamics - Let user know and ask for action if necessary

IF( ldyn )THEN
  IF( l_askUser )THEN
    CALL SetError( WN_ERROR, &
                  'Found releases with buoyancy and/or momentum.', &
                  'Current project is a passive project.', &
                  'Do you want to change to a dynamic project?', &
                  'CheckDynamics' )
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
    project(id_level)%Dynamic = .NOT.hasError()
    project(id_level)%DenseGas = project(id_level)%DenseGas .OR. &
                          (lneg .AND. project(id_level)%Dynamic)
    CALL InitError()
  ELSE
    project(id_level)%Dynamic  = .TRUE.
    project(id_level)%DenseGas = project(id_level)%DenseGas .OR. lneg
  END IF

!------ Change icon if adding dynamics
  IF( project(id_level)%Dynamic )THEN
    CALL FindHwndListId( IDB_EDTPRJ,jwnd_db,jd_level )
    CALL set_project_icon( jwnd_db,id_level,IDB_STATIC07 )

!------ Clear release dynamics if not
  ELSE
    CALL SetError( WN_ERROR, &
                  'All release dynamics will be ignored', &
                  ' ', &
                  ' ', &
                  'CheckDynamics' )
    CALL ShowInfoMessage( iwnd_db )
    DO i = 1,scenario(id_level)%nrel
      scenario(id_level)%release(i)%dynam = 0.0
    END DO
  END IF
END IF

RETURN
END
!*******************************************************************************
!                     Set GUI Releases indexes from SCIPUFF data
!*******************************************************************************
SUBROUTINE set_release_indx( xrelease,mat,linit )

USE resource_fd
USE create_fi
USE pcscipuf_fi
USE errorParam_fd
USE GUImatl_fi
USE relparam_fd
USE UtilMtlAux

IMPLICIT NONE

TYPE( release_str ), INTENT( INOUT ) :: xrelease
TYPE( matdef_str ),  INTENT( INOUT ) :: mat
LOGICAL,             INTENT( IN    ) :: linit !Flag T->called from dlgstr

INTEGER jtyp, nsg

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

CALL find_material_list( mat%material,mat%nmatl,xrelease%matl,jtyp )
IF( jtyp <= 0 )THEN
  xrelease%indx    = NOT_SET_I
  xrelease%distrib = NOT_SET_I
  IF( xrelease%matl == ' ' )xrelease%matl= 'UNKN'
ELSE
  IF( IsParticle(mat%material(jtyp)%icls) )THEN
    nsg = GetSubgroups( mat%material(jtyp),mat%mat_aux )
    IF( xrelease%indx < 0 )xrelease%indx = nsg + MAXDISTRIBUTION
    IF( linit )THEN
      IF( xrelease%indx > (nsg + MAXDISTRIBUTION) )THEN
        CALL SetError( WN_ERROR, &
                      'Invalid release distribution', &
                      'Possible material definition change', &
                      'Verify releases before continuing', &
                      'SetRelease' )
        CALL ShowErrorMessage( NULL_POINTER )
      END IF
      xrelease%indx = MIN(xrelease%indx,nsg+MAXDISTRIBUTION)
    END IF
    xrelease%distrib = xrelease%indx - nsg
  ELSE IF( IsWetParticle(mat%material(jtyp)%icls) )THEN
    nsg = GetSubgroups( mat%material(jtyp),mat%mat_aux )
    IF( xrelease%indx < 0 )xrelease%indx = nsg + MAXDISTRIBUTION
    IF( linit )THEN
      IF( xrelease%param(REL_WMFRAC_INDX) > 0.0 .AND. xrelease%param(REL_WMFRAC_INDX) <= 1.0 .AND. &
          xrelease%indx == nsg + MAXDISTRIBUTION )THEN
        xrelease%indx = nsg + MAXDISTRIBUTION + REL_SLURRY
      ELSE IF( xrelease%indx > (nsg + MAXDISTRIBUTION) )THEN
        CALL SetError( WN_ERROR, &
                      'Invalid release distribution', &
                      'Possible material definition change', &
                      'Verify releases before continuing', &
                      'SetRelease' )
        CALL ShowErrorMessage( NULL_POINTER )
      END IF
      xrelease%indx = MIN(xrelease%indx,nsg+MAXDISTRIBUTION+MAXSPECIALWETP)
    END IF
    xrelease%distrib = xrelease%indx - nsg
  ELSE IF( IsLiquid(mat%material(jtyp)%icls) )THEN
    nsg = GetSubgroups( mat%material(jtyp),mat%mat_aux )
    IF( xrelease%indx < 0 )xrelease%indx = nsg + MAXDISTRIBUTION
    IF( linit )THEN
      IF( xrelease%dur == DEF_VAL_R )THEN
        xrelease%indx = nsg + MAXDISTRIBUTION + REL_LIQUIDPOOL
      ELSE IF( xrelease%indx == 0 )THEN
        xrelease%indx = nsg + MAXDISTRIBUTION + REL_GASPHASE
      ELSE
        IF( xrelease%indx > (nsg + MAXDISTRIBUTION) )THEN
          CALL SetError( WN_ERROR, &
                        'Invalid release distribution', &
                        'Possible material definition change', &
                        'Verify releases before continuing', &
                        'SetRelease' )
          CALL ShowErrorMessage( NULL_POINTER )
        END IF
        xrelease%indx = MIN(xrelease%indx,nsg+MAXDISTRIBUTION)
      END IF
    END IF
    xrelease%distrib = xrelease%indx - nsg
  ELSE
    xrelease%indx = 0
    xrelease%distrib = 0
  END IF
  xrelease%matl = mat%material(jtyp)%cmat
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************************************
! CopyScenario
!***********************************************************************************************
SUBROUTINE CopyScenario( From,To )

USE create_fi
USE GUIparam_fd

TYPE( reldef_str ), INTENT( IN  ) :: From
TYPE( reldef_str ), INTENT( OUT ) :: To

INTEGER i,j

To%nrel = From%nrel
DO i = 1,From%nrel
  CALL CopyRelease( From%release(i),To%release(i) )
END DO

j = SIZE(To%release)
DO i = To%nrel+1,j
  CALL CopyRelease( scenario(DEFAULT_LEVEL)%release(1),To%release(i) )
END DO

RETURN
END
!***********************************************************************************************
! CopyRelease
!***********************************************************************************************
SUBROUTINE CopyRelease( From,To )

USE release_gui_fd
USE errorParam_fd

TYPE( release_str ), INTENT( IN  ) :: From
TYPE( release_str ), INTENT( OUT ) :: To

INTEGER i, ios

IF( ASSOCIATED(To%mc) )DEALLOCATE( To%mc,STAT=ios )

To = From

NULLIFY(To%mc)
IF( ASSOCIATED(From%mc) )THEN
  ALLOCATE( To%mc(SIZE(From%mc)),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,SIZE(To%mc)
    To%mc(i) = From%mc(i)
  END DO
END IF

1000 CONTINUE

RETURN

9999 CONTINUE
CALL SetError( SZ_ERROR,'Allocation Error','Allocating multicomponent list',' ','CopyRelease' )
GOTO 1000

END
!***********************************************************************************************
! ReallocateScenario
!***********************************************************************************************
SUBROUTINE ReallocateScenario( newSize )

USE create_fi
USE GUItool_fi
USE errorParam_fd
USE pcscipuf_fi

INTEGER, INTENT( IN ) :: newSize

TYPE( GUI_dryFrac ),DIMENSION(:),ALLOCATABLE :: tmpDry
TYPE( release_str ),DIMENSION(:),ALLOCATABLE :: tmpRel

INTEGER i, j, nrel, ios

CHARACTER(48) aString

LOGICAL, EXTERNAL :: hasError

! Increase dryFrac size

IF( ALLOCATED(dryFrac) )THEN
  aString = 'temporary dryFrac'
  ALLOCATE( tmpDry(SIZE(dryFrac)),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,SIZE(dryFrac)
    tmpDry(i) = dryFrac(i)
  END DO
  DEALLOCATE( dryFrac,STAT=ios )
  aString = 'new dryFrac'
  ALLOCATE( dryFrac(newSize),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,SIZE(tmpDry)
    dryFrac(i) = tmpDry(i)
  END DO
  DEALLOCATE( tmpDry,STAT=ios )
END IF

! Increase scenario size

IF( ASSOCIATED(scenario(DEFAULT_LEVEL)%release) )THEN
  nrel = MAX(1,scenario(DEFAULT_LEVEL)%nrel)
  WRITE(aString,'(A,I3)')'temporary releases for level',DEFAULT_LEVEL
  ALLOCATE( tmpRel(nrel),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,nrel
    NULLIFY( tmpRel(i)%mc )
  END DO
  DO i = 1,nrel
    CALL CopyRelease( scenario(DEFAULT_LEVEL)%release(i),tmpRel(i) )
    IF( hasError() )GOTO 1000
  END DO
  DO i = 1,nrel
    IF( ASSOCIATED( scenario(DEFAULT_LEVEL)%release(i)%mc ) )DEALLOCATE( scenario(DEFAULT_LEVEL)%release(i)%mc,STAT=ios )
  END DO
  DEALLOCATE( scenario(DEFAULT_LEVEL)%release,STAT=ios )
  WRITE(aString,'(A,I3)')'new releases for level',DEFAULT_LEVEL
  ALLOCATE( scenario(DEFAULT_LEVEL)%release(newSize),STAT=ios )
  DO i = 1,newSize
    NULLIFY(scenario(DEFAULT_LEVEL)%release(i)%mc )
  END DO
  IF( ios /= 0 )GOTO 9999
  DO i = 1,nrel
    CALL CopyRelease( tmpRel(i),scenario(DEFAULT_LEVEL)%release(i) )
    IF( hasError() )GOTO 1000
  END DO
  DEALLOCATE( tmpRel,STAT=ios )
END IF

DO j = BASE_LEVEL,EDIT_LEVEL_2
  IF( ASSOCIATED(scenario(j)%release) )THEN
    IF( scenario(j)%nrel > 0 )THEN
      nrel = scenario(j)%nrel
      WRITE(aString,'(A,I3)')'temporary releases for level',j
      ALLOCATE( tmpRel(nrel),STAT=ios )
      IF( ios /= 0 )GOTO 9999
      DO i = 1,nrel
        NULLIFY(tmpRel(i)%mc )
      END DO
      DO i = 1,nrel
        CALL CopyRelease( scenario(j)%release(i),tmpRel(i) )
        IF( hasError() )GOTO 1000
      END DO
      DO i = 1,nrel
        IF( ASSOCIATED( scenario(j)%release(i)%mc ) )DEALLOCATE( scenario(j)%release(i)%mc,STAT=ios )
      END DO
      DEALLOCATE( scenario(j)%release,STAT=ios )
      NULLIFY( scenario(j)%release )
      WRITE(aString,'(A,I3)')'new releases for level',j
      ALLOCATE( scenario(j)%release(newSize),STAT=ios )
      IF( ios /= 0 )GOTO 9999
      DO i = 1,newSize
        NULLIFY(scenario(j)%release(i)%mc )
      END DO
      DO i = 1,nrel
        CALL CopyRelease( tmpRel(i),scenario(j)%release(i) )
      END DO
      DO i = nrel+1,newSize
        CALL CopyRelease( scenario(DEFAULT_LEVEL)%release(1),scenario(j)%release(i) )
        IF( hasError() )GOTO 1000
      END DO
      DO i = 1,nrel
        IF( ASSOCIATED( tmpRel(i)%mc ) )DEALLOCATE( tmpRel(i)%mc,STAT=ios )
      END DO
      DEALLOCATE( tmpRel,STAT=ios )
    ELSE
      DO i = 1,scenario(j)%nrel
        IF( ASSOCIATED( scenario(j)%release(i)%mc ) )DEALLOCATE( scenario(j)%release(i)%mc,STAT=ios )
      END DO
      DEALLOCATE( scenario(j)%release,STAT=ios )
      NULLIFY( scenario(j)%release )
      WRITE(aString,'(A,I3)')'Releases for level',j
      ALLOCATE( scenario(j)%release(newSize),STAT=ios )
      IF( ios /= 0 )GOTO 9999
      DO i = 1,newSize
        NULLIFY(scenario(j)%release(i)%mc )
      END DO
      DO i = 1,newSize
        CALL CopyRelease( scenario(DEFAULT_LEVEL)%release(1),scenario(j)%release(i) )
        IF( hasError() )GOTO 1000
      END DO
    END IF
  END IF
END DO

MAXREL = newSize

1000 CONTINUE

IF( ALLOCATED(tmpDry) )DEALLOCATE( tmpDry,STAT=ios )
IF( ALLOCATED(tmpRel) )DEALLOCATE( tmpRel,STAT=ios )
RETURN

9999 CONTINUE
WRITE(string1,*)'new size =',newSize
WRITE(string2,*)'Allocation call sequence ='//TRIM(aString)
CALL SetError( SZ_ERROR,'Allocation Error',string2,string1,'ReallocateScenario' )
GOTO 1000

END
!***********************************************************************************************
! ReallocateMaxList
!***********************************************************************************************
SUBROUTINE ReallocateMaxList( newSize )

USE errorParam_fd
use pcscipuf_fi

INTEGER, INTENT( IN ) :: newSize

REAL,DIMENSION(:,:),ALLOCATABLE :: tmp
INTEGER i,j,ios

LOGICAL, EXTERNAL :: hasError

CHARACTER(48) aString

! Increase dblst size

IF( ALLOCATED(dblst) )THEN
  aString = 'temporary list storage'
  ALLOCATE( tmp(MAX_DLEV,SIZE(dblst(1,:))),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  tmp = dblst
  DEALLOCATE( dblst,STAT=ios )
  aString = 'new list storage'
  ALLOCATE( dblst(MAX_DLEV,newSize),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,MAX_DLEV
    DO j = 1,SIZE(tmp(i,:))
      dblst(i,j) = tmp(i,j)
    END DO
    DO j = SIZE(tmp(i,:))+1,newSize
      dblst(i,j) = 0.0
    END DO
  END DO
  DEALLOCATE( tmp,STAT=ios )
END IF

! Increase xytab size

IF( ALLOCATED(xytab) )THEN
  aString = 'temporary table storage'
  ALLOCATE( tmp(2,SIZE(xytab(1,:))),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  tmp = xytab
  DEALLOCATE( xytab,STAT=ios )
  aString = 'new table storage'
  ALLOCATE( xytab(2,newSize),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,2
    DO j = 1,SIZE(tmp(i,:))
      xytab(i,j) = tmp(i,j)
    END DO
    DO j = SIZE(tmp(i,:))+1,newSize
      xytab(i,j) = 0.0
    END DO
  END DO
  DEALLOCATE( tmp,STAT=ios )
END IF

1000 CONTINUE

maxList = newSize

IF( ALLOCATED(tmp) )DEALLOCATE( tmp,STAT=ios )

RETURN

9999 CONTINUE
WRITE(string1,*)'new size =',newSize
WRITE(string2,*)'Allocation call sequence ='//TRIM(aString)
CALL SetError( SZ_ERROR,'Allocation Error',string2,string1,'ReallocateMaxlist' )
GOTO 1000

END
