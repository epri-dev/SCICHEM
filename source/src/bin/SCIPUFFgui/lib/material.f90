!*******************************************************************************
!            Initialize Material Definition Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_matdef( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE GUImatl_fi
USE GUItool_fi
USE SwinAPI
!
!     This routine initializes the material definition Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER, DIMENSION(3) :: enable
LOGICAL ledit

!==== BASIC Dialog Box

ledit =  project(EDIT_LEVEL_2)%Edit .AND. .NOT.project(EDIT_LEVEL_2)%Restart

!---  Buttons

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

!==== Set Up Materials - copy to working array

iwnd_dbm = iwnd_db
CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )

!==== Material

id_matl = MIN(1,materials(EDIT_LEVEL_2)%nmatl)
CALL init_dialog_materials( iwnd_db,id_level )

RETURN
END
!*******************************************************************************
!            Initialize Material Definition Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_materials( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE GUImatl_fi

!--- This routine initializes the material definition Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv

TYPE( CMD ) MyCmd !Command Structure

!==== Material

!---  Clear Material Description

CALL clear_material_edit( iwnd_db,id_level )

!---  Combo Boxes - build material list - select first in list

CALL build_material_list( iwnd_db,IDB_COMBO1, &
                   materials(EDIT_LEVEL_2)%material,materials(EDIT_LEVEL_2)%nmatl )

!---  Make current selection

IF( materials(EDIT_LEVEL_2)%nmatl > 0 )THEN
  CALL SetListSelString( iwnd_db,IDB_COMBO1, &
                         materials(EDIT_LEVEL_2)%material(id_matl)%cmat,irv )
  MyCmd%id     = IDB_MATDEF
  MyCmd%cntrl  = IDB_COMBO1
  MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
  MyCmd%button = MyCmd%cntrl - COMBO_BASE
  MyCmd%level  = id_level
  CALL process_combo( iwnd_db,MyCmd )
ELSE
  id_matl = NOT_SET_I
  dbcmbo(1,id_level) = ' '
  idbcmbo(1,id_level) = 0
END IF

!---  Material Buttons - enable/disable buttons

CALL update_material_buttons( iwnd_db )

RETURN
END
!*******************************************************************************
!            Save Material Definition Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_matdef()

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
USE files_fi

!---  This routine saves the material definition Dialog Box Parameters

IMPLICIT NONE

INTEGER i


iwnd_dbm = 0

DO i = 1,materials(EDIT_LEVEL_2)%nmatl
END DO

RETURN
END
!*******************************************************************************
!            Load Materials list into ComboBox
!*******************************************************************************
SUBROUTINE build_material_list( iwnd,icombo,matl,nmatl )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi

!--- This routine initializes the materials list ComboBox

IMPLICIT NONE

INTEGER(POINTER_LEN),                   INTENT( IN ) :: iwnd   !Dialog Handle
INTEGER,                                INTENT( IN ) :: icombo !Combo Box ID
INTEGER,                                INTENT( IN ) :: nmatl  !Number of materials
TYPE( material_str ), DIMENSION(nmatl), INTENT( IN ) :: matl   !Material array

INTEGER ictrl, i, irv

!---- Initialize

ictrl = icombo - COMBO_BASE

!---- Clear List

CALL ClearList( iwnd,icombo )

!---- Load Material names into List

IF( nmatl > 0 )THEN
  DO i = 1,nmatl
    CALL AddList( iwnd,icombo,-999,matl(i)%cmat,irv )
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Load Aerosol Materials list into ComboBox
!*******************************************************************************
SUBROUTINE build_aerosol_material_list( iwnd,icombo,matl,nmatl )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi

!--- This routine initializes the materials list ComboBox

IMPLICIT NONE

INTEGER(POINTER_LEN),                    INTENT( IN ) :: iwnd   !Dialog Handle
INTEGER,                                 INTENT( IN ) :: icombo !Combo Box ID
INTEGER,                                 INTENT( IN ) :: nmatl  !Number of materials
TYPE( material_str  ), DIMENSION(nmatl), INTENT( IN ) :: matl   !Material array

INTEGER ictrl, i ,irv

LOGICAL, EXTERNAL :: IsAerosol

!---- Initialize

ictrl = icombo - COMBO_BASE

!---- Clear List

CALL ClearList( iwnd,icombo )

!---- Load aerosol Material names into List

IF( nmatl > 0 )THEN
  DO i = 1,nmatl
    IF( IsAerosol(matl(i)%icls ) )CALL AddList( iwnd,icombo,-999,matl(i)%cmat,irv )
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Copy Selection into Current Materials
!*******************************************************************************
SUBROUTINE load_current_material( id )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE GUImatl_fi
USE files_fi
USE GUIparam_fd

!--- This routine loads the selection into current material

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: id

INTEGER nsg, i ,iaux

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid

IF( id > materials(EDIT_LEVEL_2)%nmatl )id = materials(EDIT_LEVEL_2)%nmatl

IF( id > 0 )THEN
  cur_material = materials(EDIT_LEVEL_2)%material(id)
  cur_material%iaux = 1
  iaux = materials(EDIT_LEVEL_2)%material(id)%iaux
  IF( IsGas(cur_material%icls) )THEN
    nmauxc = MAXGMAUX
    DO i = 1,MAXGMAUX
      cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
    END DO
  ELSE IF( IsParticle(cur_material%icls) .OR. &
           IsWetParticle(cur_material%icls) )THEN
    nsg = NINT(materials(EDIT_LEVEL_2)%mat_aux(iaux))
    nmauxc = MAXPMAUXX + nsg
    DO i = 1,MAXPMAUXX - 1
      cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
    END DO
    DO i = 1,nsg + 1
      cur_aux(MAXPMAUXX-1+i)= &
      materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_BOUNDS+(i-1)*MAXPMAUX)
    END DO
  ELSE IF( IsLiquid(cur_material%icls) )THEN
    nsg = NINT(materials(EDIT_LEVEL_2)%mat_aux(iaux+MAXGMAUX+MAXLMAUXP))
    nmauxc = MAXGMAUX+MAXLMAUXP+MAXLMAUXX + nsg
    DO i = 1,MAXGMAUX+MAXLMAUXP+MAXLMAUXX - 1
      cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
    END DO
    DO i = 1,nsg+1
      cur_aux(MAXGMAUX+MAXLMAUXP+MAXLMAUXX-1+i) = &
                materials(EDIT_LEVEL_2)% &
                mat_aux(iaux+MAXGMAUX+MAXLMAUXP+LMAUX_BOUNDS+(i-1)*MAXLMAUX)
    END DO
  ELSE IF( IsAerosol(cur_material%icls) )THEN
    nmauxc = MAXAMAUXP
    DO i = 1,MAXAMAUXP
      cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
    END DO
  ELSE
    nmauxc = 0
  END IF
END IF

id_matl = id

RETURN
END
!*******************************************************************************
!            Delete Current Materials
!*******************************************************************************
SUBROUTINE delete_current_material( ilev,id )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE GUImatl_fi
USE files_fi

!---  This routine loads the selection into current material

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: id
INTEGER, INTENT( IN    ) :: ilev

INTEGER i, j, iaux, maux, jaux, naux

maux = materials(ilev)%material(id)%iaux

DO i = id,materials(ilev)%nmatl-1
 IF( i < materials(ilev)%nmatl-1 )THEN
   naux = materials(ilev)%material(i+2)%iaux
 ELSE
   naux = materials(ilev)%nmaux + 1
 END IF
 naux = naux - materials(ilev)%material(i+1)%iaux
 jaux = maux - 1
 iaux = materials(ilev)%material(i+1)%iaux - 1
 DO j = 1,naux
   materials(ilev)%mat_aux(jaux+j) = materials(ilev)%mat_aux(iaux+j)
 END DO
 materials(ilev)%material(i) = materials(ilev)%material(i+1)
 materials(ilev)%material(i)%iaux = maux
 maux = maux + naux
END DO

materials(ilev)%nmaux = maux - 1
materials(ilev)%nmatl = materials(ilev)%nmatl - 1
id = MIN(id,materials(ilev)%nmatl)

RETURN
END
!*******************************************************************************
!            Save Selection from Current Materials
!*******************************************************************************
SUBROUTINE save_current_material( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi

!--- This routine saves the selection from current material

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: id_level

INTEGER iaux, i, nsg, maux

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: hasError

IF( id_matl <= materials(EDIT_LEVEL_2)%nmatl )THEN
  CALL delete_current_material( 2,id_matl )
END IF

materials(EDIT_LEVEL_2)%nmatl = materials(EDIT_LEVEL_2)%nmatl + 1
id_matl = materials(EDIT_LEVEL_2)%nmatl

materials(EDIT_LEVEL_2)%material(id_matl) = cur_material
iaux = materials(EDIT_LEVEL_2)%nmaux + 1
materials(EDIT_LEVEL_2)%material(id_matl)%iaux = iaux
IF( IsGas(cur_material%icls) )THEN
  IF( iaux+MAXGMAUX - 1 > MAXMAUX )THEN
    CALL reallocate_mataux( iaux+MAXGMAUX-1 )
    IF( hasError() )GOTO 9999
  END IF
  DO i = 1,MAXGMAUX
    materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1) = cur_aux(i)
  END DO
  maux = MAXGMAUX
ELSE IF( IsParticle(cur_material%icls) .OR. &
         IsWetParticle(cur_material%icls) )THEN
  nsg = NINT(cur_aux(1))
  IF( iaux+PMAUX_MEAN+(nsg-1)*MAXPMAUX > MAXMAUX )THEN
    CALL reallocate_mataux( iaux+PMAUX_MEAN+(nsg-1)*MAXPMAUX )
    IF( hasError() )GOTO 9999
  END IF
  DO i = 1,MAXPMAUXX - 1
    materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1) = cur_aux(i)
  END DO
  DO i = 1,nsg+1
    materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_BOUNDS+(i-1)*MAXPMAUX) = &
                                      cur_aux(MAXPMAUXX-1+i)
  END DO
  DO i = 1,nsg
    materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_MEAN+(i-1)*MAXPMAUX) = &
     0.5*(materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_BOUNDS+(i-1)*MAXPMAUX) + &
          materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_BOUNDS+i*MAXPMAUX))
  END DO
  maux = MAXPMAUXX + nsg*MAXPMAUX
ELSE IF( IsLiquid(cur_material%icls) )THEN
  nsg = NINT(cur_aux(MAXGMAUX+MAXLMAUXP+1))
  IF( iaux + LMAUX_MEAN + (nsg-1)*MAXLMAUX > MAXMAUX )THEN
    CALL reallocate_mataux( iaux+LMAUX_MEAN+(nsg-1)*MAXLMAUX )
    IF( hasError() )GOTO 9999
  END IF
  DO i = 1,MAXGMAUX + MAXLMAUXP + MAXLMAUXX - 1
    materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1) = cur_aux(i)
  END DO
  iaux = iaux + MAXGMAUX+MAXLMAUXP
  DO i = 1,nsg + 1
    materials(EDIT_LEVEL_2)%mat_aux(iaux+LMAUX_BOUNDS+(i-1)*MAXLMAUX) = &
                 cur_aux(MAXGMAUX+MAXLMAUXP+MAXLMAUXX-1+i)
  END DO
  DO i = 1,nsg
    materials(EDIT_LEVEL_2)%mat_aux(iaux+LMAUX_MEAN+(i-1)*MAXLMAUX) = &
        0.5*(materials(EDIT_LEVEL_2)%mat_aux(iaux+LMAUX_BOUNDS+(i-1)*MAXLMAUX) + &
             materials(EDIT_LEVEL_2)%mat_aux(iaux+LMAUX_BOUNDS+i*MAXLMAUX))
  END DO
  maux = MAXGMAUX + MAXLMAUXP + MAXLMAUXX + nsg*MAXLMAUX
ELSE IF( IsAerosol(cur_material%icls) )THEN
  IF( iaux + MAXLMAUXP - 1 > MAXMAUX )THEN
    CALL reallocate_mataux( iaux+MAXLMAUXP-1 )
    IF( hasError() )GOTO 9999
  END IF
  DO i = 1,MAXLMAUXP
    materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1) = cur_aux(i)
  END DO
  maux = MAXLMAUXP
ELSE
  maux = 0
END IF
materials(EDIT_LEVEL_2)%nmaux = materials(EDIT_LEVEL_2)%nmaux + maux

IF( IsMulti(cur_material%icls) )THEN
  CALL SetComponentNamesMC( materials(EDIT_LEVEL_2)%material(id_matl), &
                            materials(EDIT_LEVEL_2)%materialMC(id_matl) )
  IF( hasError() )GOTO 9999
ELSE
  materials(EDIT_LEVEL_2)%material(id_matl)%nmc = 0
END IF

CALL init_dialog_materials( iwnd_db,id_level )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                     Initialize Material Description
!*******************************************************************************
SUBROUTINE init_material_edit( iwnd_db,ilev )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi

!---  This routine initializes the material description part of the
!     Material Definition dialog box

IMPLICIT NONE

REAL, PARAMETER :: RHOAIR = 1.2

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN ) :: ilev    !Data level ID

REAL    fac
INTEGER nsg

CHARACTER(32)  number

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid

CHARACTER(128), EXTERNAL :: AddNull

!---  Enable/Show controls

IF( id_matl <= 0 )THEN
  CALL clear_material_edit( iwnd_db,ilev )
  RETURN
END IF

!---  Show Gas, Particle or Liquid

IF( IsGas(cur_material%icls) )THEN
  CALL show_gas_edit( iwnd_db )
ELSE IF( IsParticle(cur_material%icls) )THEN
  CALL show_particle_edit( iwnd_db )
ELSE IF( IsWetParticle(cur_material%icls) )THEN
  CALL show_particle_edit(iwnd_db)
ELSE IF( IsLiquid(cur_material%icls) )THEN
  CALL show_liquid_edit( iwnd_db )
ELSE IF( IsAerosol(cur_material%icls) )THEN
  CALL show_aerosol_edit( iwnd_db )
ELSE
  CALL show_gas_edit( iwnd_db )
END IF

!---  Check Boxes

lcheck(2,ilev) = cur_material%lsrft !Deposition/Total
lcheck(1,ilev) = cur_material%ldost !Dose/Total
lcheck(4,ilev) = cur_material%lsrfg !Deposition/Group
lcheck(3,ilev) = cur_material%ldosg !Dose/Group
lcheck(5,ilev) = LEN_TRIM(cur_material%file) /= 0
IF( lcheck(5,ilev) )THEN
  string1 = TRIM(cur_material%file)
  CALL AddPath( string1,TRIM(cur_material%path) )
  CALL SetControlText( iwnd_db,IDB_STATIC64,string1 )
  dbtext(20,ilev) = AddNull( string1 )
ELSE
  CALL SetControlText( iwnd_db,IDB_STATIC64,' ' )
END IF
lcheck(6,ilev) = .FALSE.
lcheck(7,ilev) = .FALSE.
lcheck(8,ilev) = .FALSE.
lcheck(9,ilev) = .FALSE.

CALL SetChecks( iwnd_db,lcheck(1,ilev),1,9 )

!---  Edit Boxes - Text

dbtext(1,ilev) = AddNull( cur_material%cmat )
dbtext(2,ilev) = AddNull( cur_material%unit )
dbtext(3,ilev) = AddNull( cur_material%ccls )
CALL SetEditTs( iwnd_db,dbtext(1,ilev),1,3 )
dbint(20,ilev) = cur_material%icls

!---  Edit Boxes - Reals

IF( IsGas(cur_material%icls) )THEN
  dbreal(4,ilev) = cur_aux(1)/RHOAIR !Density
  fac = 100.
  dbreal(6,ilev) = fac*cur_aux(2) !Dep. Velocity
ELSE IF( IsParticle(cur_material%icls) .OR. &
         IsWetParticle(cur_material%icls) )THEN
  dbreal(4,ilev) = cur_aux(2) !Density
  dbreal(6,ilev) = NOT_SET_R
ELSE IF( IsLiquid(cur_material%icls) )THEN
  dbreal(4,ilev) = cur_aux(1)/RHOAIR !Gas Density
  fac = 100.
  dbreal(6, ilev) = fac*cur_aux(2) !Dep. Velocity
  dbreal(7, ilev) = cur_aux(MAXGMAUX+5) !Mol. Weight
  dbreal(8, ilev) = cur_aux(MAXGMAUX+6) !Surface tension
  dbreal(9, ilev) = cur_aux(MAXGMAUX+7) !Spread factor
  dbreal(10,ilev) = cur_aux(MAXGMAUX+2) !Antoine Coeff a
  dbreal(11,ilev) = cur_aux(MAXGMAUX+3) !Antoine Coeff b
  dbreal(12,ilev) = cur_aux(MAXGMAUX+4) !Antoine Coeff c
  dbreal(13,ilev) = cur_aux(MAXGMAUX+MAXLMAUXP+2) !Liq. Density
  dbreal(14,ilev) = cur_aux(MAXGMAUX+1) !Liq. Density Temp. Coeff
  dbreal(15,ilev) = cur_aux(MAXGMAUX+8) !Liq. Viscosity
  dbreal(16,ilev) = cur_aux(MAXGMAUX+9) !Liq. Specifc heat
  dbreal(17,ilev) = cur_aux(MAXGMAUX+10)!Gas Specifc heat
ELSE IF( IsAerosol(cur_material%icls) )THEN
  dbreal(4,ilev) = cur_aux(2) !Density
  fac = 100.
  dbreal(6, ilev) = fac*cur_aux(1) !Dep. Velocity
  dbreal(7, ilev) = cur_aux(6) !Mol. Weight
  dbreal(8, ilev) = cur_aux(7) !Specific Heat (Vapor)
  dbreal(9, ilev) = cur_aux(8) !Specific Heat (Liquid)
  dbreal(10,ilev) = cur_aux(3) !Antoine Coeff a
  dbreal(11,ilev) = cur_aux(4) !Antoine Coeff b
  dbreal(12,ilev) = cur_aux(5) !Antoine Coeff c
END IF
dbreal(1,ilev) = cur_material%prop(1) !Decay Rate
dbreal(2,ilev) = cur_material%prop(2) !Decay Rate
dbreal(3,ilev) = cur_material%prop(4) !NWPN power law
dbreal(5,ilev) = cur_material%prop(3) !Minimum concentration
CALL SetEditRs( iwnd_db,dbreal(1,ilev),1,3 )

!---  List Boxes

IF( IsParticle(cur_material%icls) .OR. &
    IsWetParticle(cur_material%icls) )THEN
  nsg = NINT(cur_aux(1))
  dbint(1,ilev) = nsg
  WRITE(number,*)dbint(1,ilev)
  number = ADJUSTL(number)
  CALL SetControlText( iwnd_db,IDB_STATIC62,number )
  IF( dbint(1,ilev) > 0 )THEN
    CALL build_list_numeric( iwnd_db,IDB_LIST1,ilev, &
                             nsg+1,cur_aux(3),1.E6 )
  ELSE
    nlst(ilev) = 0
  END IF
ELSE IF( IsLiquid(cur_material%icls) )THEN
  nsg = NINT(cur_aux(MAXGMAUX+MAXLMAUXP+1))
  dbint(1,ilev) = nsg
  WRITE(number,*)dbint(1,ilev)
  number = ADJUSTL(number)
  CALL SetControlText( iwnd_db,IDB_STATIC62,number )
  IF( dbint(1,ilev) > 0 )THEN
    CALL build_list_numeric( iwnd_db,IDB_LIST1,ilev, &
                             nsg+1,cur_aux(MAXGMAUX+MAXLMAUXP+3),1.E6 )
  ELSE
    nlst(ilev) = 0
  END IF
END IF

RETURN
END
!*******************************************************************************
!                     Save Material Description
!*******************************************************************************
SUBROUTINE save_material_edit( iwnd_db,ilev,lok )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi
USE files_fi
USE errorParam_fd
USE class_fd

!---  This routine saves the material description part of the
!     Material Definition dialog box

IMPLICIT NONE

REAL, PARAMETER :: RHOAIR = 1.2

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilev    !Data level ID
LOGICAL,              INTENT( OUT ) :: lok

INTEGER i, nmauxt, nsg, nrel, indx, distrib
REAL    tmp_aux(MAXLMAUXX+MAXSGP+MAXGMAUX+MAXLMAUXP)

TYPE( material_str ) tmp_material

LOGICAl, EXTERNAL :: hasError
LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle, IsLiquid
INTEGER, EXTERNAL :: AddClass, ClearClass

CHARACTER(128), EXTERNAL :: StripNull

!---  Enable/Show controls

IF( id_matl <= 0 )RETURN

lok = .TRUE.

tmp_material = cur_material
nmauxt = nmauxc
DO i = 1,nmauxc
  tmp_aux(i) = cur_aux(i)
END DO

!---  Edit Boxes - Text

cur_material%cmat = StripNull( dbtext(1,ilev) ); CALL cupper( cur_material%cmat )
cur_material%unit = StripNull( dbtext(2,ilev) )
cur_material%ccls = StripNull( dbtext(3,ilev) )
cur_material%icls = dbint(20,ilev)

!---  Edit Boxes - Reals

IF( IsGas(cur_material%icls) )THEN
  cur_aux(1) = dbreal(4,ilev)*RHOAIR
  IF( dbreal(6,ilev) /= NOT_SET_R )THEN
    cur_aux(2) = 0.01*dbreal(6,ilev)
  ELSE
    cur_aux(2) = 0.
  END IF
  nsg = 1
ELSE IF( IsParticle(cur_material%icls) .OR. &
         IsWetParticle(cur_material%icls) )THEN
  cur_aux(2) = dbreal(4,ilev)
  nsg = nlst(ilev) - 1
ELSE IF( IsLiquid(cur_material%icls) )THEN
  cur_aux(1) = dbreal(4,ilev)*RHOAIR
  IF( dbreal(6,ilev) /= NOT_SET_R )THEN
    cur_aux(2) = 0.01*dbreal(6,ilev)
  ELSE
    cur_aux(2) = 0.
  END IF
  cur_aux(MAXGMAUX+MAXLMAUXP+2) = dbreal(13,ilev) !Liq. Density
  cur_aux(MAXGMAUX+1 ) = dbreal(14,ilev) !Liq. Density Temp. Coeff
  cur_aux(MAXGMAUX+2 ) = dbreal(10,ilev) !Antoine Coeff a
  cur_aux(MAXGMAUX+3 ) = dbreal(11,ilev) !Antoine Coeff b
  cur_aux(MAXGMAUX+4 ) = dbreal(12,ilev) !Antoine Coeff c
  cur_aux(MAXGMAUX+5 ) = dbreal(7, ilev) !Mol. Weight
  cur_aux(MAXGMAUX+6 ) = dbreal(8, ilev) !Surf Tension
  cur_aux(MAXGMAUX+7 ) = dbreal(9, ilev) !Spread factor
  cur_aux(MAXGMAUX+8 ) = dbreal(15,ilev) !Viscosity
  cur_aux(MAXGMAUX+9 ) = dbreal(16,ilev) !Liq. specifc heat
  cur_aux(MAXGMAUX+10) = dbreal(17,ilev) !Gas Specific Heat
  nsg = nlst(ilev) - 1
ELSE IF( IsAerosol(cur_material%icls) )THEN
  cur_aux(2) = dbreal(4,ilev)
  IF( dbreal(6,ilev) /= NOT_SET_R )THEN
    cur_aux(1) = 0.01*dbreal(6,ilev)
  ELSE
    cur_aux(1) = 0.
  END IF
  cur_aux(3) = dbreal(10,ilev) !Antoine Coeff a
  cur_aux(4) = dbreal(11,ilev) !Antoine Coeff b
  cur_aux(5) = dbreal(12,ilev) !Antoine Coeff c
  cur_aux(6) = dbreal(7, ilev) !Mol. Weight
  cur_aux(7) = dbreal(8, ilev) !Specific Heat (Vapor)
  cur_aux(8) = dbreal(9, ilev) !Specific Heat (Liquid)
  nsg = 1
END IF
IF( dbreal(1,ilev) /= NOT_SET_R )THEN
  cur_material%prop(1) = dbreal(1,ilev)
ELSE
  cur_material%prop(1) = 0.0
END IF
IF( dbreal(2,ilev) /= NOT_SET_R )THEN
  cur_material%prop(2) = dbreal(2,ilev)
ELSE
  cur_material%prop(2) = 0.0
END IF
cur_material%prop(3) = dbreal(5,ilev)
IF( dbreal(5,ilev) == NOT_SET_R .OR. dbreal(5,ilev) == DEF_VAL_R )THEN
  CALL set_conc_min( cur_material )
END IF
  cur_material%prop(4) = NOT_SET_R

!---  List Boxes

IF( IsParticle(cur_material%icls) .OR. IsWetParticle(cur_material%icls) )THEN
  cur_aux(1) = FLOAT(nsg)
ELSE IF( IsLiquid(cur_material%icls) )THEN
  cur_aux(1+MAXGMAUX+MAXLMAUXP) = FLOAT(nsg)
END IF

!---  Check Boxes

IF( .NOT.IsGas(cur_material%icls) .AND. .NOT.IsAerosol(cur_material%icls) )THEN
  cur_material%lsrfg = lcheck(4,ilev) .OR. &
               ( lcheck(2,ilev) .AND. nsg<=1 ) !Deposition/Total
  cur_material%ldosg = lcheck(3,ilev) .OR. &
               ( lcheck(1,ilev) .AND. nsg<=1 ) !Dose/Total
  cur_material%lsrft = lcheck(2,ilev) .AND. nsg>1 !Deposition/Group
  cur_material%ldost = lcheck(1,ilev) .AND. nsg>1 !Dose/Group
ELSE
  cur_material%lsrfg = lcheck(4,ilev) !Deposition/Total
  cur_material%ldosg = lcheck(3,ilev) !Dose/Total
  cur_material%lsrft = .FALSE. !Deposition/Group
  cur_material%ldost = .FALSE. !Dose/Group
END IF

IF( lcheck(5,ilev) )THEN
  string1 = TRIM(dbtext(20,ilev))
  CALL SplitName( string1,cur_material%file,cur_material%path )
  IF( IsGas(cur_material%icls) )cur_material%icls = AddClass( cur_material%icls,MATID_MULTI )
ELSE
  cur_material%icls = ClearClass( cur_material%icls,MATID_MULTI )
  cur_material%file = ' '
  cur_material%path = ' '
END IF

cur_material%effClass = 0

CALL check_material( iwnd_db,cur_material,cur_aux,id_matl,lok )
IF( .NOT.lok )THEN
  cur_material = tmp_material
  nmauxc = nmauxt
  DO i = 1,nmauxc
    cur_aux(i) = tmp_aux(i)
  END DO
ELSE
  IF( IsParticle(cur_material%icls) .OR. IsWetParticle(cur_material%icls) )THEN
    DO i = 1,nsg+1
!lst            cur_aux(2+i) = dblst(i,ilev)/1.e6
      cur_aux(2+i) = dblst(ilev,i)/1.E6
    END DO
  ELSE IF( IsLiquid(cur_material%icls) )THEN
    DO i = 1,nsg+1
!lst            cur_aux(MAXGMAUX+MAXLMAUXP+2+i) = dblst(i,ilev)/1.e6
      cur_aux(MAXGMAUX+MAXLMAUXP+2+i) = dblst(ilev,i)/1.E6
    END DO
  END IF
  IF( scenario(EDIT_LEVEL_2)%nrel > 0 )THEN
    nrel = 0
    DO i = 1,scenario(EDIT_LEVEL_2)%nrel
      IF( scenario(EDIT_LEVEL_2)%release(i)%matl == cur_material%cmat )THEN
        nrel = nrel + 1
      END IF
    END DO
    IF( nrel > 0 )THEN
      CALL Seterror( WN_ERROR, &
                    'Changing material definition may affect ' &
                             //' release subgroup bin',' ', &
                    'Do you want to make the changes?', &
                    'CheckMaterial' )
      CALL ShowWarningMessage( iwnd_db,.FALSE. )
    END IF
    IF( hasError() )THEN
      cur_material = tmp_material
    ELSE
      DO i = 1,scenario(EDIT_LEVEL_2)%nrel
        IF( scenario(EDIT_LEVEL_2)%release(i)%matl == cur_material%cmat )THEN
          indx    = scenario(EDIT_LEVEL_2)%release(i)%indx
          distrib = scenario(EDIT_LEVEL_2)%release(i)%distrib
          IF( scenario(EDIT_LEVEL_2)%release(i)%distrib > 0 )THEN
            scenario(EDIT_LEVEL_2)%release(i)%indx = &
            scenario(EDIT_LEVEL_2)%release(i)%distrib + nsg
          ELSE
            scenario(EDIT_LEVEL_2)%release(i)%distrib = &
                            scenario(EDIT_LEVEL_2)%release(i)%indx - nsg
            IF( scenario(EDIT_LEVEL_2)%release(i)%distrib > 0 )THEN
              scenario(EDIT_LEVEL_2)%release(i)%distrib = 0
              scenario(EDIT_LEVEL_2)%release(i)%indx    = nsg
              WRITE(string1,'(A,''R'',I2.2)') &
              'Unable to reset release index for release ',i
              CALL SetError( NO_ERROR,string1,' ',' ','CheckMaterial' )
              CALL ShowInfoMessage( iwnd_db )
            END IF
          END IF
        END IF
      END DO
    END IF
    CALL InitError()
  END IF
END IF

RETURN
END
!*******************************************************************************
!                     Clear Material Description
!*******************************************************************************
SUBROUTINE clear_material_edit( iwnd_db,ilev )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

!---  This routine clears the material description part of the
!     Material Definition dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Handle
INTEGER,              INTENT( IN ) :: ilev    !Data level ID

INTEGER i

last_edit = -1

DO i = 1,9
  lcheck(i,ilev) = .FALSE.
END DO

DO i = 1,3
  dbtext(i,ilev) = ' '
END DO
dbtext(20,ilev) = ' '

DO i = 1,14
  dbreal(i,ilev) = NOT_SET_R
END DO

CALL clear_list_numeric( iwnd_db,IDB_LIST1,ilev )

DO i = 4,12
  CALL EnableControl( iwnd_db,BUTTON_BASE+i,FALSE )
  CALL ShowControl(   iwnd_db,BUTTON_BASE+i,SW_HIDE )
END DO
CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )

DO i = 1,9
  CALL EnableControl( iwnd_db,CHECK_BASE+i,FALSE )
  CALL ShowControl(   iwnd_db,CHECK_BASE+i,SW_HIDE )
END DO

DO i = 1,3
  CALL EnableControl( iwnd_db,EDIT_BASE+i,FALSE )
  CALL ShowControl(   iwnd_db,EDIT_BASE+i,SW_HIDE )
END DO

DO i = 1,3
  CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
  CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
END DO

!      CALL EnableControl(iwnd_db,IDB_LIST1,FALSE)
CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )

CALL EnableControl( iwnd_db,IDB_STATIC14,TRUE )
string1 = 'Linear decay'
CALL SetControlText( iwnd_db,IDB_STATIC14,string1 )

DO i = 10,18
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 40,44
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 50,53
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO
DO i = 60,64
  CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
END DO

RETURN
END
!*******************************************************************************
!            Update Material Definitions buttons
!*******************************************************************************
SUBROUTINE update_material_buttons( iwnd )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE GUItool_fi
USE create_fi
USE files_fi
USE SwinAPI

!---  This routine enables/disables the material buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog handle

INTEGER enable(3)
LOGICAL ledit, ltest

!---  Buttons

ledit = project(EDIT_LEVEL_2)%Edit .AND. .NOT. project(EDIT_LEVEL_2)%Restart

IF( ledit )THEN
  ltest = materials(EDIT_LEVEL_2)%nmatl > 0 .AND. id_matl > 0
  IF( ltest )THEN
    enable(1) = TRUE !Edit
    enable(3) = TRUE !Delete
  ELSE
    enable(1) = FALSE !Edit
    enable(3) = FALSE !Delete
  END IF
  enable(2) = TRUE !New
ELSE
  enable(1) = FALSE
  enable(2) = FALSE
  enable(3) = FALSE
END IF

CALL EnableButtons( iwnd,enable,1,3 )

!     Control Buttons

enable(1) = project(EDIT_LEVEL_2)%Edit
enable(2) = project(EDIT_LEVEL_2)%Edit
CALL EnableButtons( iwnd,enable,14,2 )

IF( .NOT.ledit )THEN
  CALL ShowControl( iwnd,IDB_BUTTON1 ,SW_HIDE )
  CALL ShowControl( iwnd,IDB_BUTTON2 ,SW_HIDE )
  CALL ShowControl( iwnd,IDB_BUTTON3 ,SW_HIDE )
  CALL SetControlText( iwnd,ID_CANCEL,'&OK' )
  string1 = 'Project '//TRIM(project(EDIT_LEVEL_2)%ID%name)//' MATERIALS'
  CALL SetControlText( iwnd,IDB_STATIC01,string1 )
END IF

RETURN
END
!*******************************************************************************
!            Update Subgroup Definitions buttons
!*******************************************************************************
SUBROUTINE update_subgroup_buttons( iwnd,iflag )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

!     This routine enables/disables the subgroup buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog handle
INTEGER,              INTENT( IN ) :: iflag

INTEGER enable(7), ID, ilev

CHARACTER(32) number

!---  Buttons

CALL FindHwndList( iwnd,ID,ilev )
dbint(1,ilev) = MAX(nlst(ilev)-1,0)
WRITE(number,*)dbint(1,ilev)
number = ADJUSTL(number)
CALL SetControlText( iwnd,IDB_STATIC62,number )

enable(1) = nlst(ilev) > 0 .AND. iflag !Edit
enable(2) = nlst(ilev) < MAXSGP+1 .AND. iflag !New
enable(3) = enable(1) !Delete
enable(4) = enable(1) !Clear all
enable(5) = enable(2) !Compute
enable(6) = enable(2) !Load
enable(7) = dbint(1,ilev) > 0 .AND. iflag !Save

CALL EnableButtons( iwnd,enable,4,7 )

RETURN
END
!*******************************************************************************
!            Update Subgroup Compute buttons
!*******************************************************************************
SUBROUTINE update_compute_buttons( iwnd )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!---  This routine enables/disables the subgroup buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog handle

INTEGER enable

!---  Buttons

enable = TRUE

CALL EnableControl( iwnd,ID_OK,enable )
CALL EnableControl( iwnd,IDB_BUTTON1,enable )

RETURN
END
!*******************************************************************************
!            Enable Material buttons
!*******************************************************************************
SUBROUTINE enable_material_control( iwnd,iflag )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

!---  This routine enables/disables the material buttons

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Dialog handle
INTEGER,              INTENT( IN ) :: iflag !Enable/disable Flag

INTEGER enable(3)

!     Material Buttons

IF( iflag )THEN
  CALL update_material_buttons( iwnd )
ELSE
  enable(1) = FALSE
  enable(2) = FALSE
  enable(3) = FALSE
  CALL EnableButtons( iwnd,enable,1,3 )
END IF

!---  Control Buttons

enable(1) = iflag
IF( materials(EDIT_LEVEL_2)%nmatl < MAXMTYP .AND. project(EDIT_LEVEL_2)%Edit )THEN
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

!---  Cancel

CALL EnableControl( iwnd,ID_CANCEL,iflag )

!---  COMBO Box

CALL EnableControl(iwnd,IDB_COMBO1,iflag)

RETURN
END
!*******************************************************************************
!            Enable Material Editing
!*******************************************************************************
SUBROUTINE enable_material_edit( iwnd,iflag )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi
USE files_fi
USE SwinAPI
USE liqdef

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Dialog handle
INTEGER,              INTENT( IN ) :: iflag !Enable/Disable

INTEGER n1,n2,jflag,i,mrel

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsMulti

IF( id_matl <= 0 )RETURN

save_flag = iflag

!---  Button flag

IF( iflag==TRUE )THEN
  jflag = SW_SHOWNORMAL
ELSE
  jflag = SW_HIDE
END IF

!---  Checks

IF( IsGas(cur_material%icls) .OR. IsAerosol(cur_material%icls) )THEN
  n1 = 3
ELSE
  n1 = 1
END IF

n2 = 4

IF( IsGas(cur_material%icls) )n2 = 5

DO i = n1,n2
  CALL EnableControl( iwnd,CHECK_BASE+i,iflag )
END DO

!---  Edit - Text

DO i = 1,2
  CALL EnableControl( iwnd,EDIT_BASE+i,iflag )
END DO

mrel = 0
IF( scenario(EDIT_LEVEL_2)%nrel > 0 )THEN
  DO i = 1,scenario(EDIT_LEVEL_2)%nrel
    IF( scenario(EDIT_LEVEL_2)%release(i)%matl == cur_material%cmat )THEN
      mrel = mrel + 1
    END IF
  END DO
END IF

IF( mrel > 0 )CALL EnableControl( iwnd,IDB_EDIT1,FALSE )

!---  Edit Real

  CALL EnableControl( iwnd,IDB_REAL3,FALSE )

IF( .FALSE. )THEN
  CALL EnableControl( iwnd,IDB_REAL1,FALSE )
  CALL EnableControl( iwnd,IDB_REAL2,FALSE )
ELSE
  CALL EnableControl( iwnd,IDB_REAL1,iflag )
  CALL EnableControl( iwnd,IDB_REAL2,iflag )
END IF

!---  List

IF( .NOT.IsGas(cur_material%icls) .AND. .NOT.IsAerosol(cur_material%icls) )THEN
  DO i = 4,10
    CALL ShowControl( iwnd,BUTTON_BASE+i,jflag )
  END DO
  CALL update_subgroup_buttons( iwnd,iflag )
END IF

!---  Buttons

DO i = 11,12
  CALL EnableControl( iwnd,BUTTON_BASE+i,iflag )
  CALL ShowControl  ( iwnd,BUTTON_BASE+i,jflag )
END DO
IF( IsGas(cur_material%icls) .AND. IsMulti(cur_material%icls) )THEN
  CALL EnableControl( iwnd,IDB_BUTTON17,iflag )
  CALL ShowControl  ( iwnd,IDB_BUTTON17,jflag )
END IF

!---  Effects
!-- Disable all
CALL EnableControl( iwnd,IDB_CHECK6,FALSE )
CALL EnableControl( iwnd,IDB_CHECK7,FALSE )
CALL EnableControl( iwnd,IDB_CHECK8,FALSE )
CALL EnableControl( iwnd,IDB_CHECK9,FALSE )

RETURN
END
!***********************************************************************
!               MaterialCombo
!***********************************************************************
SUBROUTINE material_combo( iwnd_db,id_button,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE GUImatl_fi

!--- This routine processes COMBOBOXes from the MATDEF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

INTEGER id
CHARACTER(128) new

CHARACTER(128), EXTERNAL :: StripNull

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 ) !Changing Materials
    new = StripNull( dbcmbo(1,id_level) )
    CALL find_material_list( materials(EDIT_LEVEL_2)%material, &
                             materials(EDIT_LEVEL_2)%nmatl,new,id )
    CALL load_current_material( id )
    CALL init_material_edit( iwnd_db,id_level )
    CALL update_material_buttons( iwnd_db )
  CASE DEFAULT
END SELECT

RETURN
END
!*******************************************************************************
!            Find Selection in Current Materials List
!*******************************************************************************
SUBROUTINE find_material_list( matl,nmatl,string,id )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE GUImatl_fi

!     This routine loads the selection into current material

IMPLICIT NONE

INTEGER,                                INTENT( IN  ) :: nmatl
TYPE( material_str ), DIMENSION(nmatl), INTENT( IN  ) :: matl(1)
CHARACTER(*),                           INTENT( IN  ) :: string
INTEGER,                                INTENT( OUT ) :: id

INTEGER n, i

CHARACTER(16) test1, test2

n = MIN(LEN(matl(1)%cmat),LEN(string))

id = 0
test1 = string(1:n)
CALL cupper( test1 )
DO i = 1,nmatl
  test2 = matl(i)%cmat(1:n)
  CALL cupper( test2 )
  IF( test1(1:n) == test2(1:n))THEN
    id = i
    EXIT
  END IF
END DO

RETURN
END
!*******************************************************************************
!            Find Selection in Current Materials List
!*******************************************************************************
SUBROUTINE find_materialMC( matlMC,string,id )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE GUImatl_fi

!---  This routine loads the selection into current material

IMPLICIT NONE

TYPE( matMC_str ), INTENT( IN  ) :: matlMC
CHARACTER(*),      INTENT( IN  ) :: string
INTEGER,           INTENT( OUT ) :: id

INTEGER i

CHARACTER(16) test1, test2

id = 0
test1 = TRIM(string)
CALL cupper( test1 )
IF( ASSOCIATED(matlMC%name) )THEN
  DO i = 1,SIZE(matlMC%name)
    test2 = TRIM(matlMC%name(i))
    CALL cupper( test2 )
    IF( test1 == test2 )THEN
      id = i
      EXIT
    END IF
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Show Gas Material Editing controls
!*******************************************************************************
SUBROUTINE show_gas_edit( iwnd_db )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

INTEGER i, iedit, jedit

LOGICAL, EXTERNAL :: IsMulti

!==== last_edit = -1 -> Last cleared
!==== last_edit =  0 -> Last set for gas
!==== last_edit =  1 -> Last set for particle
!==== last_edit =  2 -> Last set for liquid
!==== last_edit =  3 -> Last set for aerosol
!==== last_edit = 10-13 -> Last set for NFAC
!==== last_edit = 20-23 -> Last set for NWPN
!==== last_edit = 30-33 -> Last set for both NWPN and NFAC - should never happen

IF( last_edit < 0 )THEN
  iedit = last_edit
  jedit = 0
ELSE
  jedit = last_edit/10
  iedit = last_edit - 10*jedit
END IF

SELECT CASE( iedit )
  CASE( -1 )

!--- Labels

    DO i = 10,15
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 41,44
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!---  Physical Parameters

    DO i = 1,3
      CALL ShowControl( iwnd_db,EDIT_BASE+i,SW_SHOWNORMAL )
    END DO

    DO i = 1,2
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO

    CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_SHOWNORMAL ) !Properties Button

!---  Output

    DO i = 3,4
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO

!---  Multicomponents

    CALL ShowControl( iwnd_db,CHECK_BASE+5,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,STATIC_BASE+17,SW_SHOWNORMAL )
    IF( IsMulti(cur_material%icls) )THEN
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )
    END IF
  CASE( 0 )
  CASE( 1 )

!---  Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_HIDE )
    END DO !
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !

!---  Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !


!---  Multicomponents

    CALL ShowControl( iwnd_db,CHECK_BASE+5,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,STATIC_BASE+17,SW_SHOWNORMAL )
    IF( IsMulti(cur_material%icls) )THEN
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
      CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )
    END IF

  CASE( 2 )

!---  Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_HIDE )
    END DO !
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !

!--- Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !


!--- Multicomponents

    CALL ShowControl( iwnd_db,CHECK_BASE+5,SW_SHOWNORMAL )
    CALL ShowControl( iwnd_db,STATIC_BASE+17,SW_SHOWNORMAL )
    IF( IsMulti(cur_material%icls) )THEN
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl(   iwnd_db,IDB_BUTTON17,SW_HIDE )
      CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )
    END IF
  CASE( 3 )

!--- Multicomponents

    CALL ShowControl(   iwnd_db,CHECK_BASE+5,SW_SHOWNORMAL )
    CALL EnableControl( iwnd_db,CHECK_BASE+5,TRUE )
    CALL ShowControl(   iwnd_db,STATIC_BASE+17,SW_SHOWNORMAL )
    IF( IsMulti(cur_material%icls) )THEN
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_SHOWNORMAL )
    ELSE
      CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
      CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )
    END IF

  CASE DEFAULT

END SELECT

last_edit = 10*jedit

RETURN
END
!*******************************************************************************
!            Show Particle Material Editing controls
!*******************************************************************************
SUBROUTINE show_particle_edit( iwnd_db )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

INTEGER i, iedit, jedit

LOGICAL, EXTERNAL :: IsMulti

!==== last_edit = -1 -> Last cleared
!==== last_edit =  0 -> Last set for gas
!==== last_edit =  1 -> Last set for particle
!==== last_edit =  2 -> Last set for liquid
!==== last_edit =  3 -> Last set for aerosol
!==== last_edit = 10-13 -> Last set for NFAC
!==== last_edit = 20-23 -> Last set for NWPN
!==== last_edit = 30-33 -> Last set for both NWPN and NFAC - should never happen

IF( last_edit < 0 )THEN
  iedit = last_edit
  jedit = 0
ELSE
  jedit = last_edit/10
  iedit = last_edit - 10*jedit
END IF

SELECT CASE( iedit )
  CASE( -1 )

!--- Labels

    DO i = 10,16
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 41,44
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!--- Physical Parameters

    DO i = 1,3
      CALL ShowControl( iwnd_db,EDIT_BASE+i,SW_SHOWNORMAL )
    END DO

    DO i = 1,2
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO

    CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_SHOWNORMAL ) !Properties Button

!--- Output

    DO i = 1,4
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL SetControlText( iwnd_db,IDB_STATIC52,'( Size Bin )' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'( Size Bin )' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

!--- SubGroup

    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL ) !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

  CASE( 0 )


!--- Multicomponents

    CALL ShowControl(   iwnd_db,CHECK_BASE+5,SW_HIDE )
    CALL EnableControl( iwnd_db,CHECK_BASE+5,FALSE )
    CALL ShowControl(   iwnd_db,STATIC_BASE+17,SW_HIDE )
    CALL ShowControl(   iwnd_db,IDB_BUTTON17,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
    CALL ShowControl(   iwnd_db,STATIC_BASE+63,SW_HIDE )
    CALL ShowControl(   iwnd_db,STATIC_BASE+64,SW_HIDE )

!--- Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO !
    CALL SetControlText( iwnd_db,IDB_STATIC52,'( Size Bin )' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'( Size Bin )' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

!--- Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_SHOWNORMAL ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !
  CASE( 1 )
  CASE( 2 )

!--- Output

    CALL SetControlText( iwnd_db,IDB_STATIC52,'( Size Bin )' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'( Size Bin )' )

  CASE( 3 )

!--- Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO !
    CALL SetControlText( iwnd_db,IDB_STATIC52,'( Size Bin )' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'( Size Bin )' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

!---  Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_SHOWNORMAL ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

  CASE DEFAULT

END SELECT

last_edit = 10*jedit + 1

RETURN
END
!*******************************************************************************
!            Show Liquid Material Editing controls
!*******************************************************************************
SUBROUTINE show_liquid_edit( iwnd_db )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

INTEGER i, iedit, jedit

LOGICAL, EXTERNAL :: IsMulti

!==== last_edit = -1 -> Last cleared
!==== last_edit =  0 -> Last set for gas
!==== last_edit =  1 -> Last set for particle
!==== last_edit =  2 -> Last set for liquid
!==== last_edit =  3 -> Last set for aerosol
!==== last_edit = 10-13 -> Last set for NFAC
!==== last_edit = 20-23 -> Last set for NWPN
!==== last_edit = 30-33 -> Last set for both NWPN and NFAC - should never happen

IF( last_edit < 0 )THEN
  iedit = last_edit
  jedit = 0
ELSE
  jedit = last_edit/10
  iedit = last_edit - 10*jedit
END IF

SELECT CASE (iedit)
  CASE( -1 )

!--- Labels

    DO i = 10,16
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 41,44
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!--- Physical Parameters

    DO i = 1,3
      CALL ShowControl( iwnd_db,EDIT_BASE+i,SW_SHOWNORMAL )
    END DO

    DO i = 1,2
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO

    CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_SHOWNORMAL )

!---  Output

    DO i = 1,4
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL SetControlText( iwnd_db,IDB_STATIC52,'(Vapor,Liquid)' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'(Vapor,Liquid)' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!---  SubGroup

    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL ) !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
  CASE( 0 )


!---  Multicomponents

    CALL ShowControl( iwnd_db,CHECK_BASE+5,SW_HIDE )
    CALL EnableControl( iwnd_db,CHECK_BASE+5,FALSE )
    CALL ShowControl( iwnd_db,STATIC_BASE+17,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
    CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
    CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )

!--- Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO
    CALL SetControlText( iwnd_db,IDB_STATIC52,'(Vapor,Liquid)' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'(Vapor,Liquid)' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !

!---  Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_SHOWNORMAL ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
  CASE( 1 )

!---  Output

    CALL SetControlText( iwnd_db,IDB_STATIC52,'(Vapor,Liquid)' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'(Vapor,Liquid)' )

  CASE( 2 )

  CASE( 3 )

!--- Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO !
    CALL SetControlText( iwnd_db,IDB_STATIC52,'(Vapor,Liquid)' )
    CALL SetControlText( iwnd_db,IDB_STATIC53,'(Vapor,Liquid)' )
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL)
    END DO !

!--- Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_SHOWNORMAL ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_SHOWNORMAL )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO !
  CASE DEFAULT
END SELECT

last_edit = 10*jedit + 2

RETURN
END
!*******************************************************************************
!            Show Aerosol Material Editing controls
!*******************************************************************************
SUBROUTINE show_aerosol_edit( iwnd_db )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

INTEGER i, iedit,jedit

LOGICAL, EXTERNAL :: IsMulti

!==== last_edit = -1 -> Last cleared
!==== last_edit =  0 -> Last set for gas
!==== last_edit =  1 -> Last set for particle
!==== last_edit =  2 -> Last set for liquid
!==== last_edit =  3 -> Last set for aerosol
!==== last_edit = 10-13 -> Last set for NFAC
!==== last_edit = 20-23 -> Last set for NWPN
!==== last_edit = 30-33 -> Last set for both NWPN and NFAC - should never happen

IF( last_edit < 0 )THEN
  iedit = last_edit
  jedit = 0
ELSE
  jedit = last_edit/10
  iedit = last_edit - 10*jedit
END IF

SELECT CASE( iedit )
  CASE( -1 )

!--- Labels

    DO i = 10,15
      CALL ShowControl (iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO
    DO i = 41,44
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
    END DO

!--- Physical Parameters

    DO i = 1,3
      CALL ShowControl( iwnd_db,EDIT_BASE+i,SW_SHOWNORMAL )
    END DO

    DO i = 1,2
      CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
    END DO

    CALL ShowControl( iwnd_db,IDB_BUTTON16,SW_SHOWNORMAL ) !Properties Button

!--- Output

    DO i = 3,4
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_SHOWNORMAL )
    END DO
  CASE( 0 )

!---  Multicomponents

    CALL ShowControl( iwnd_db,CHECK_BASE+5,SW_HIDE )
    CALL EnableControl( iwnd_db,CHECK_BASE+5,FALSE )
    CALL ShowControl( iwnd_db,STATIC_BASE+17,SW_HIDE )
    CALL ShowControl( iwnd_db,IDB_BUTTON17,SW_HIDE )
    CALL EnableControl( iwnd_db,IDB_BUTTON17,FALSE )
    CALL ShowControl( iwnd_db,STATIC_BASE+63,SW_HIDE )
    CALL ShowControl( iwnd_db,STATIC_BASE+64,SW_HIDE )

  CASE( 1 )

!---  Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_HIDE )
    END DO !
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE)
    END DO !

!--- Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !

  CASE( 2 )

!---  Output

    DO i = 1,2 !Subgroup output
      CALL ShowControl( iwnd_db,CHECK_BASE+i,SW_HIDE )
    END DO !
    DO i = 50,53 !Subgroup/Total labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !

!---  Subgroup

    CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE ) !Subgroup Group
    CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )    !List
    DO i = 60,62 !Subgroup labels
      CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
    END DO !
  CASE( 3 )

  CASE DEFAULT
END SELECT

last_edit = 10*jedit + 3

RETURN
END
!*******************************************************************************
!            Show NFAC/NWPN Material Editing controls
!*******************************************************************************
SUBROUTINE show_NFAC_NWPN( iwnd_db,lNFAC,lNWPN )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ):: iwnd_db !Dialog handle
LOGICAL,              INTENT( IN ):: lNFAC
LOGICAL,              INTENT( IN ):: lNWPN

INTEGER i, iedit, jedit

!==== last_edit = -1 -> Last cleared
!==== last_edit =  0 -> Last set for gas
!==== last_edit =  1 -> Last set for particle
!==== last_edit =  2 -> Last set for liquid
!==== last_edit = 10-12 -> Last set for NFAC
!==== last_edit = 20-22 -> Last set for NWPN
!==== last_edit = 30-32 -> Last set for both NWPN and NFAC - should never happen

IF( last_edit < 0 )THEN
  iedit = last_edit
  jedit = 0
ELSE
  jedit = last_edit/10
  iedit = last_edit - 10*jedit
END IF

!==== Group Box/Label
!          NFAC -> Hide
!     .not.NFAC -> Show
!          NWPN -> label = NWeapon Power Law Decay
!     .not.NWPN -> label = Active fraction

IF( lNFAC )THEN
  SELECT CASE( jedit )
    CASE( 0,2 )
      CALL ShowControl( iwnd_db,IDB_STATIC14,SW_HIDE )
    CASE DEFAULT
  END SELECT
ELSE
  IF( lNWPN )THEN
    string1 = 'NWeapon Power Law Decay Parameter'
  ELSE
    string1 = 'Linear decay'
  END IF
  CALL SetControlText( iwnd_db,IDB_STATIC14,string1 )
  SELECT CASE( jedit )
    CASE( 1,3 )
      CALL ShowControl( iwnd_db,IDB_STATIC14,SW_SHOWNORMAL )
    CASE( 2 )
      IF( .NOT.lNWPN )CALL ShowControl( iwnd_db,IDB_STATIC14,SW_SHOWNORMAL )
    CASE DEFAULT
  END SELECT
END IF

!==== Active fraction input
!          NFAC -> Hide
!          NWPN -> Hide

IF( lNFAC .OR. lNWPN )THEN

  SELECT CASE( jedit )
    CASE( 0 )
      DO i = 41,44
        CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_HIDE )
      END DO
      DO i = 1,2
        CALL ShowControl( iwnd_db,REAL_BASE+i,SW_HIDE )
      END DO

    CASE DEFAULT

  END SELECT

ELSE

  SELECT CASE( jedit )
    CASE( 1,2,3 )
      DO i = 41,44
        CALL ShowControl( iwnd_db,STATIC_BASE+i,SW_SHOWNORMAL )
      END DO
      DO i = 1,2
        CALL ShowControl( iwnd_db,REAL_BASE+i,SW_SHOWNORMAL )
      END DO

    CASE DEFAULT

  END SELECT

END IF

!==== NWPN Power Law
!          NWPN -> Show
!     .not.NWPN -> Hide

IF( lNWPN )THEN

  SELECT CASE( jedit )
    CASE( 0,1 )
      CALL ShowControl( iwnd_db,IDB_STATIC40,SW_SHOWNORMAL )
      CALL ShowControl( iwnd_db,IDB_REAL3,SW_SHOWNORMAL )

    CASE DEFAULT

  END SELECT

ELSE

  SELECT CASE( jedit )
    CASE( 2,3 )
      CALL ShowControl( iwnd_db,IDB_STATIC40,SW_HIDE )
      CALL ShowControl( iwnd_db,IDB_REAL3,SW_HIDE )

    CASE DEFAULT

  END SELECT

END IF

last_edit = iedit

IF( lNFAC )last_edit = last_edit + 10
IF( lNWPN )last_edit = last_edit + 20

RETURN
END
!***********************************************************************
!               MaterialButton
!***********************************************************************
SUBROUTINE material_button( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi
USE GUItool_fi
USE SwinAPI
USE cfocus

!--- This routine processes PUSHBUTTONs from MATDEF Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level  !Dialog level (for data storage)

INTEGER irv

INTEGER nmatl_start
INTEGER iedit, jedit, i, nMtl, nAux
LOGICAL lok, verify_button

TYPE( fileNameT ) fileT
TYPE( ProjectStructure )prjdlg
CHARACTER(128) filenam

LOGICAL, EXTERNAL :: hasError

!---- initailize

lok = .FALSE.

!---- Select by Button number

SELECT CASE( id_button )
  CASE( 1 ) !EDIT Material
    ifocus = GetFocus()
    CALL enable_material_edit( iwnd_db,TRUE )
    CALL enable_material_control( iwnd_db,FALSE )
    CALL SetFocusControl( iwnd_db,IDB_BUTTON12 )

  CASE( 2 ) !NEW Material
    CALL LetsDialog( iwnd_db,IDB_MATNEW )
    IF( lokbutton )THEN
      ifocus = GetFocus()
      CALL enable_material_edit( iwnd_db,TRUE )
      CALL enable_material_control( iwnd_db,FALSE )
      CALL SetFocusControl( iwnd_db,IDB_EDIT1 )
    END IF

  CASE( 3 ) !DELETE material
    ifocus = GetFocus()
    CALL delete_material( iwnd_db,id_level )
    irv = SetFocus( ifocus )

  CASE( 4 ) !EDIT Size Bin
    listedt%dialog = IDB_MATDEF
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_EDTLST )
    CALL update_subgroup_buttons( iwnd_db,TRUE )

  CASE( 5 ) !NEW Size Bin
    listedt%dialog = IDB_MATDEF
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_NEWLST )
    CALL update_subgroup_buttons( iwnd_db,TRUE )

  CASE( 6 ) !DELETE Size Bin
    CALL delete_list_numeric( iwnd_db,IDB_LIST1,id_level )
    CALL update_subgroup_buttons( iwnd_db,TRUE )

  CASE( 7 ) !CLEAR ALL Size Bin
    string1 = 'Delete ALL Size bin definitions'
    lok = verify_button( iwnd_db,TRIM(string1) )
    IF( lok )THEN
      nlst(id_level) = 0
      CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )
      CALL update_subgroup_buttons( iwnd_db,TRUE )
    END IF

  CASE( 8 ) !COMPUTE Size bins
    listedt%dialog = IDB_MATDEF
    listedt%list   = IDB_LIST1
    CALL LetsDialog( iwnd_db,IDB_COMLST )
    CALL update_subgroup_buttons( iwnd_db,TRUE )

  CASE( 9 ) !LOAD Size bins from file
    lok = .FALSE.
    filenam = TRIM(loadfile(10))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL file_list_numeric( iwnd_db,id_dialog,IDB_LIST1,id_level, &
                                       TRIM(filenam),1.E6 ) !  Read Size bins
      CALL update_subgroup_buttons( iwnd_db,TRUE )
      loadfile(10) = TRIM(filenam)
    END IF

  CASE( 10 ) !SAVE Size bins to file
    lok = .FALSE.
    filenam = TRIM(loadfile(10))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      CALL save_list_numeric( iwnd_db,id_dialog,id_level, &
                                       TRIM(filenam),1.E6 ) !  write Size bins
      loadfile(10) = TRIM(filenam)
    END IF

  CASE( 11 ) !SAVE Edits
    CALL save_material_edit( iwnd_db,id_level,lok )
    IF( lok )THEN
      CALL save_current_material( iwnd_db,id_level )
! IAN changed this section 8/11
      IF( hasError() )THEN
        CALL ShowErrorMessage( iwnd_db )
      ELSE
        CALL enable_material_edit( iwnd_db,FALSE )
        CALL enable_material_control( iwnd_db,TRUE )
        irv = SetFocus( ifocus )
      END IF
    END IF

  CASE( 12 ) !CANCEL EDITS
    CALL enable_material_edit( iwnd_db,FALSE )
    CALL load_current_material( id_matl )
    CALL init_material_edit( iwnd_db,id_level )
    CALL enable_material_control( iwnd_db,TRUE )
    irv = SetFocus( ifocus )

  CASE( 13 ) !OK - Check Data first
    lok = .FALSE.
    CALL check_material_definition( lok )
    IF( lok )THEN
      CALL PushButton( iwnd_db,IDB_BUTTON13,ID_OK,irv )
    END IF

  CASE( 14 ) !LOAD Material from file
    lok = .FALSE.
    filenam = TRIM(loadfile(4))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db, &
                                                  filenam ) !  Filename Dialog
    IF( lok )THEN
      nmatl_start = materials(EDIT_LEVEL_2)%nmatl + 1
!==== SCIP Tool
      fileT%string = filenam
      irv = SCIPCountMaterial( ToolCallerID,fileT,nMtl )
      lok = irv == SCIPsuccess
      IF( lok )THEN
        CALL AllocateMtlList( materials(EDIT_LEVEL_2)%nmatl+nMtl )
        prjdlg = project(EDIT_LEVEL_2)
        matdef%control%mode = SCIPnull
        matdef%control%searchName = ' '
        CALL SplitName( filenam,string1,prjdlg%ID%path )
        CALL SplitExtension( string1,prjdlg%ID%name,matdef%control%fileExtension )
        matdef%control%mode = IBSET(matdef%control%mode,HCB_FILE)
        CALL GUI_SCIP_material( prjdlg,materials(EDIT_LEVEL_2),matdef,mtlList,SIZE(mtlList) )
        irv = SCIPLoadMaterialF( ToolCallerID,matdef,mtlList )
        lok = irv == SCIPsuccess
        IF( lok )THEN
          nMtl = matdef%mtlHead%number
          nAux = materials(EDIT_LEVEL_2)%nmaux+30*(matdef%mtlHead%number-nmatl_start+1)
          IF( nMtl > MAXMTYP .OR. nAux > MAXMAUX )THEN
            CALL ReallocateMaterials( MAX(nMtl,MAXMTYP),MAX(nAux,MAXMAUX) )
          END IF
          IF( hasError() )THEN
            CALL ShowErrorMessage( iwnd_db )
          ELSE
            CALL SCIP_GUI_material(materials(EDIT_LEVEL_2),matdef,mtlList)
            IF( hasError() )THEN
              CALL ShowErrorMessage( iwnd_db )
            ELSE
              id_matl = materials(EDIT_LEVEL_2)%nmatl
              DO i = nmatl_start,materials(EDIT_LEVEL_2)%nmatl
                IF( materials(EDIT_LEVEL_2)%material(i)%file /= ' ' )THEN
                 IF( materials(EDIT_LEVEL_2)%material(i)%path == ' ' )THEN
                    materials(EDIT_LEVEL_2)%material(i)%path = TRIM(prjdlg%ID%path)
                  END IF
                END IF
                CALL set_conc_min( materials(EDIT_LEVEL_2)%material(i) )
              END DO
!==== SCIP Tool
              CALL check_dense( iwnd_db,2,.TRUE.,nmatl_start )
              CALL update_material_buttons( iwnd_db )
              CALL load_current_material( id_matl )
              CALL init_dialog_materials( iwnd_db,id_level )
              loadfile(4) = TRIM(filenam)
            END IF
          END IF
        END IF
      END IF
      IF( .NOT.lok )THEN
        CALL GetToolError( 'MaterialButton' )
        CALL ShowErrorMessage( iwnd_db )
      END IF
      CALL DeallocateMtlList()
    END IF

  CASE( 15 ) !CLEAR ALL Materials
    ifocus = GetFocus()
    string1 = 'Delete ALL Material Definitions'
    lok = verify_button(iwnd_db,TRIM(string1))
    IF( lok )THEN
      CALL CopyMaterial( materials(DEFAULT_LEVEL),materials(EDIT_LEVEL_2) )
      id_matl = 0
      CALL init_dialog_materials( iwnd_db,id_level )
    END IF
    irv = SetFocus( ifocus )

  CASE( 16 )
    IF( last_edit < 0 )THEN
      iedit = last_edit
      jedit = 0
    ELSE
      jedit = last_edit/10
      iedit = last_edit - 10*jedit
    END IF
    SELECT CASE( iedit )
    CASE( 3 )       !Aerosol Parameters
      CALL LetsDialog( iwnd_db,IDB_AERPARM )
    CASE( 2 )       !Liquid Parameters
      CALL LetsDialog( iwnd_db,IDB_LIQPARM )
    CASE( 1 )        !Particle Parameters
      CALL LetsDialog( iwnd_db,IDB_PRTPARM )
      CALL SetEditTs( iwnd_db,dbtext(3,id_level),3,1 )
    CASE( 0 )        !Gas Parameters
      CALL LetsDialog( iwnd_db,IDB_GASPARM )
    END SELECT

  CASE( 17 )  !Browse for multicomponent
    filenam = TRIM(dbtext(20,id_level))
    CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,filenam ) !  Filename Dialog
    IF( lok )THEN
      dbtext(20,id_level) = TRIM(filenam)
      CALL SetControlText( iwnd_db,IDB_STATIC64,dbtext(20,id_level) )
    END IF

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!                     Check Material Description
!*******************************************************************************
SUBROUTINE check_material( iwnd_db,mat,mat_aux,id,lok )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE errorParam_fd

!     This routine checks the material description

IMPLICIT NONE

REAL, PARAMETER :: RHOAIR = 1.2

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd_db
TYPE( material_str), INTENT( INOUT ) :: mat
REAL, DIMENSION(*),  INTENT( IN    ) :: mat_aux
LOGICAL,             INTENT( INOUT ) :: lok
INTEGER,             INTENT( IN    ) :: id

INTEGER  i, iaux
CHARACTER(32) number
CHARACTER(4)  munit

INTEGER nError
CHARACTER(128) eMessage, eInform, eAction, eRoutine

LOGICAL IsGas, IsParticle
LOGICAL IsAerosol
LOGICAL IsWetParticle,IsLiquid
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: CheckFile
LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'SaveMaterial'

!---- Check for name

IF( mat%cmat == ' ' .OR. mat%cmat == ' ' )THEN
  eMessage = 'Unnamed material'
  eAction  = 'Please choose a unique name for this material'
  GOTO 9999
END IF

!---- Check for unique name

CALL find_material_list( materials(EDIT_LEVEL_2)%material,materials(EDIT_LEVEL_2)%nmatl, &
                                                mat%cmat,i )
IF( i /= id .AND. i > 0 )THEN
  eMessage = 'Duplicate material name : '//TRIM(mat%cmat)
  eAction  = 'Please choose a unique name for this material'
  GOTO 9999
END IF

iaux = mat%iaux
munit = mat%unit
CALL clower( munit )

!---- Gas

IF( IsGas(mat%icls) )THEN
  IF( mat_aux(iaux) == NOT_SET_R )THEN
    eMessage = 'No gas density ratio'
    eAction  = 'Please specify the gas density ratio'
    GOTO 9999
  END IF
  IF( mat_aux(iaux) <= 0.0 )THEN
    eMessage = 'Invalid gas density ratio'
    eAction  = 'Please specify a gas density ratio > 0'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+1) == NOT_SET_R )THEN
    eMessage = 'No gas deposition velocity'
    eAction  = 'Please specify the gas deposition velocity in cm/s'
    GOTO 9999
  END IF
  IF( project(EDIT_LEVEL_2)%Dynamic .AND. mat_aux(iaux) /= RHOAIR )THEN
    IF( munit /= 'kg' )THEN
      eMessage = 'Mass release units must be kg for buoyant gases'
      eAction  = 'Please set the release mass units to kg'
      GOTO 9999
    END IF
  END IF

!---- Particle

ELSE IF( IsParticle(mat%icls) .OR. IsWetParticle(mat%icls) )THEN
  IF( mat_aux(iaux+1) == NOT_SET_R )THEN
    eMessage = 'No material density'
    eAction  = 'Please specify the material density in kg/m3'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+1) <= 0.0 )THEN
    eMessage = 'Invalid particle density'
    eAction  = 'Please specify a particle density > 0'
    GOTO 9999
  END IF
  IF( NINT(mat_aux(iaux)) <= 0 )THEN
    eMessage = 'No size bins'
    eAction  = 'Please define at least 1 size bin (2 boundaries)'
    GOTO 9999
  END IF
  IF( NINT(mat_aux(iaux)) > MAXSGP )THEN
    eMessage = 'Too many size bins'
    WRITE(number,*)MAXSGP
    number = ADJUSTL(number)
    eAction  = 'Maximum allowed='//number
    GOTO 9999
  END IF

!---- Liquid

ELSE IF( IsLiquid(mat%icls) )THEN
  IF( mat_aux(iaux+MAXGMAUX+MAXLMAUXP+1) == NOT_SET_R )THEN
    eMessage = 'No liquid density'
    eAction  = 'Please specify the material density in kg/m3'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+MAXGMAUX+1) == NOT_SET_R )THEN
    eMessage = 'No material vapor pressure'
    eAction  = 'Please specify the material vapor pressure'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+MAXGMAUX+4) <= 0 )THEN
    IF( mat_aux(iaux+MAXGMAUX+4) == NOT_SET_R )THEN
      eMessage = 'No material molecular weight'
    ELSE
      eMessage = 'Molecular weight must be greater than 0.0'
    END IF
    eAction  = 'Please specify the material molecular weight'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+MAXGMAUX+5) <= 0 )THEN
    IF( mat_aux(iaux+MAXGMAUX+5) == NOT_SET_R )THEN
      eMessage = 'No surface tension'
    ELSE
      eMessage = 'Surface tension must be greater than 0.0'
    END IF
    eAction  = 'Please specify the material surface tension'
    GOTO 9999
  END IF
  IF( NINT(mat_aux(iaux+MAXGMAUX+MAXLMAUXP))<= 0 )THEN
    eMessage = 'No size bins'
    eAction  = 'Please define at least 1 size bin (2 boundaries)'
    GOTO 9999
  END IF
  IF( NINT(mat_aux(iaux+MAXGMAUX+MAXLMAUXP)) > MAXSGP-1 )THEN
    eMessage = 'Too many size bins'
    WRITE(number,*)MAXSGP-1
    number = ADJUSTL(number)
    eAction  = 'Maximum allowed='//number
    GOTO 9999
  END IF
  IF( mat_aux(iaux) == NOT_SET_R )THEN
    eMessage = 'No vapor density ratio'
    eAction  = 'Please specify the vapor density ratio'
    GOTO 9999
  END IF
  IF( mat_aux(iaux) <= 0.0 )THEN
    eMessage = 'Invalid vapor density ratio'
    eAction  = 'Please specify a vapor density ratio > 0'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+1) == NOT_SET_R )THEN
    eMessage = 'No gas deposition velocity'
    eAction  = 'Please specify the gas deposition velocity in cm/s'
    GOTO 9999
  END IF
  IF( munit /= 'kg' )THEN
    eMessage = 'Mass release units must be kg for liquid materials'
    eAction  = 'Please set the release mass units to kg'
    GOTO 9999
  END IF

!---- Aerosol

ELSE IF( IsAerosol(mat%icls) )THEN
  IF( mat_aux(iaux+1) == NOT_SET_R )THEN
    eMessage = 'No liquid phase density'
    eAction  = 'Please specify the liquid phase density in kg/m3'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+1) <= 0.0 )THEN
    eMessage = 'Invalid liquid phase density'
    eAction  = 'Please specify a liquid phase density > 0'
    GOTO 9999
  END IF
  IF( mat_aux(iaux) == NOT_SET_R )THEN
    eMessage = 'No gas phase deposition velocity'
    eAction  = 'Please specify the gas phase deposition velocity in cm/s'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+2) == NOT_SET_R )THEN
    eMessage = 'No material vapor pressure'
    eAction  = 'Please specify the material vapor pressure'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+5) <= 0 )THEN
    IF( mat_aux(iaux+5) == NOT_SET_R )THEN
      eMessage = 'No material molecular weight'
    ELSE
      eMessage = 'Molecular weight must be greater than 0.0'
    END IF
    eAction  = 'Please specify the material molecular weight'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+6) <= 0 )THEN
    IF( mat_aux(iaux+6) == NOT_SET_R )THEN
      eMessage = 'No vapor phase specific heat'
    ELSE
      eMessage = 'Vapor phase specific heat must be greater than 0.0'
    END IF
    eAction  = 'Please specify the vapor phase specific heat'
    GOTO 9999
  END IF
  IF( mat_aux(iaux+7) <= 0 )THEN
    IF( mat_aux(iaux+7) == NOT_SET_R )THEN
      eMessage = 'No liquid phase specific heat'
    ELSE
      eMessage = 'Liquid phase specific heat must be greater than 0.0'
    END IF
    eAction  = 'Please specify the liquid phase specific heat'
    GOTO 9999
  END IF
  IF( munit /= 'kg' )THEN
    eMessage = 'Mass release units must be kg for Aerosols'
    eAction  = 'Please set the release mass units to kg'
    GOTO 9999
  END IF
END IF

IF( IsMulti(mat%icls) )THEN
  string1 = TRIM(mat%file)
  CALL AddPath( string1,TRIM(mat%path) )
  IF( LEN_TRIM(string1) <= 0 )THEN
    nError = IV_ERROR
    eMessage = 'multicomponent description file name not specified'
    eAction  = 'Please specify the multicomponent description file'
    GOTO 9999
  ELSE
    IF( .NOT.CheckFile(TRIM(string1)) )THEN
      nError = IV_ERROR
      eMessage = 'multicomponent description file not found'
      CALL ReportFileName( eInform,'File=',string1 )
      eAction  = 'Please specify an existing multicomponent description file'
      GOTO 9999
    END IF
  END IF
END IF

1000  RETURN

9999 CONTINUE
nError =  IV_ERROR
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( iwnd_db )
lok = .FALSE.
GOTO 1000

END
!*******************************************************************************
!            Initialize New Material Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_matnew( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE SwinAPI

!--- This routine initializes the new material Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,             INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, irv

TYPE( CMD ) MyCmd !Command Structure

CALL FindHwndListId( IDB_MATDEF,jwnd_db,jd_level ) !  Parent = MATDEF
CALL clear_material_edit( jwnd_db,jd_level )

!--- Radio Buttons

nradio(1,id_level)  = 5 !New Type
IF( materials(EDIT_LEVEL_2)%nmatl > 0 )THEN
  ichoice(1,id_level) = 5 !COPY
  CALL build_material_list( iwnd_db,IDB_COMBO1, &
                  materials(EDIT_LEVEL_2)%material,materials(EDIT_LEVEL_2)%nmatl )
  CALL SetListSelString( iwnd_db,IDB_COMBO1, &
                         materials(EDIT_LEVEL_2)%material(id_matl)%cmat,irv )
  MyCmd%id     = IDB_MATNEW
  MyCmd%cntrl  = IDB_COMBO1
  MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
  MyCmd%button = MyCmd%cntrl - COMBO_BASE
  MyCmd%level  = id_level
  CALL process_combo( iwnd_db,MyCmd )
ELSE
  ichoice(1,id_level) = 1 !GAS
  CALL EnableControl( iwnd_db,IDB_RADIO05,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO1 ,FALSE )
END IF
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            save new material Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_matnew( id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi

!--- This routine saves the new material Dialog Box Parameters

IMPLICIT NONE

REAL, PARAMETER :: RHOAIR = 1.2

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER id, jd_level, i, nsg, iaux
INTEGER(POINTER_LEN) jwnd_db

CHARACTER(128) new,StripNull

INTEGER, EXTERNAL :: SetClass
LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid
LOGICAL, EXTERNAL :: hasError

!---- Get new type

SELECT CASE( ichoice(1,id_level) )
  CASE( 1 )
    cur_material = materials(DEFAULT_LEVEL)%material(1)
    cur_material%icls = SetClass( MAT_GAS )
    cur_material%iaux = 1
    nmauxc = MAXGMAUX
    cur_aux(1) = RHOAIR
    cur_aux(2) = 0.0
    cur_material%cmat = ' '
    cur_material%ccls = MAT_GAS

  CASE( 2 )
    cur_material = materials(DEFAULT_LEVEL)%material(1)
    cur_material%icls = SetClass( MAT_PRT )
    cur_material%iaux = 1
    nmauxc = 2
    cur_aux(1) = 0.0
    cur_aux(2) = NOT_SET_R
    cur_material%cmat = ' '
    cur_material%ccls = MAT_PRT

  CASE( 3 )
    cur_material = materials(DEFAULT_LEVEL)%material(1)
    cur_material%icls = SetClass( MAT_LIQ )
    cur_material%iaux = 1
    nmauxc = MAXGMAUX + MAXLMAUXP + 2
    cur_aux(1) = RHOAIR
    cur_aux(2) = 0.0
    DO i = 1,MAXLMAUXP
      cur_aux(i+MAXGMAUX) = NOT_SET_R
    END DO
    cur_aux(MAXGMAUX + MAXLMAUXP+1) = 0.0
    cur_aux(MAXGMAUX + MAXLMAUXP+2) = NOT_SET_R
    cur_material%cmat = ' '
    cur_material%ccls = MAT_LIQ

  CASE( 4 )
    cur_material = materials(DEFAULT_LEVEL)%material(1)
    cur_material%icls = SetClass( MAT_AER )
    cur_material%iaux = 1
    nmauxc = MAXAMAUXP
    cur_aux(1) = 0.0
    DO i = 2,MAXAMAUXP
      cur_aux(i) = NOT_SET_R
    END DO
    cur_material%cmat = ' '
    cur_material%ccls = MAT_AER

  CASE DEFAULT
    new = StripNull( dbcmbo(1,id_level) )
    CALL find_material_list( materials(EDIT_LEVEL_2)%material,materials(EDIT_LEVEL_2)%nmatl &
                                ,new,id )
    IF( id > 0 )THEN
      cur_material = materials(EDIT_LEVEL_2)%material(id)
      cur_material%cmat = ' '
      cur_material%iaux = 1
      iaux = materials(EDIT_LEVEL_2)%material(id)%iaux
      IF( IsGas(cur_material%icls) )THEN
        nmauxc = MAXGMAUX
        cur_aux(1) = materials(EDIT_LEVEL_2)%mat_aux(iaux)
        cur_aux(2) = materials(EDIT_LEVEL_2)%mat_aux(iaux+1)
      ELSE IF( IsParticle(cur_material%icls) .OR. &
	             IsWetParticle(cur_material%icls) )THEN
        nsg = NINT(materials(EDIT_LEVEL_2)%mat_aux(iaux))
        nmauxc = MAXPMAUXX+nsg
        DO i = 1,MAXPMAUXX - 1
          cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
        END DO
        DO i = 1,nsg+1
          cur_aux(MAXPMAUXX+i-1) = &
             materials(EDIT_LEVEL_2)%mat_aux(iaux+PMAUX_BOUNDS+(i-1)*MAXPMAUX)
        END DO
      ELSE IF( IsLiquid(cur_material%icls) )THEN
        nsg = NINT(materials(EDIT_LEVEL_2)%mat_aux(iaux+MAXGMAUX+MAXLMAUXP))
        nmauxc = MAXGMAUX + MAXLMAUXP + MAXLMAUXX + nsg
        DO i = 1,MAXGMAUX + MAXLMAUXP + MAXLMAUXX - 1
          cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
        END DO
        DO i = 1,nsg+1
          cur_aux(MAXGMAUX+MAXLMAUXP+MAXLMAUXX-1+i) = &
              materials(EDIT_LEVEL_2)%mat_aux( &
                 iaux+MAXGMAUX+MAXLMAUXP+LMAUX_BOUNDS+(i-1)*MAXLMAUX)
        END DO
      ELSE IF( IsAerosol(cur_material%icls) )THEN
        nmauxc = MAXAMAUXP
        DO i = 1,MAXAMAUXP
          cur_aux(i) = materials(EDIT_LEVEL_2)%mat_aux(iaux+i-1)
        END DO
      ELSE
        nmauxc = 0
      END IF
    END IF

END SELECT

IF( (materials(EDIT_LEVEL_2)%nmatl + 1) > MAXMTYP .OR. (materials(EDIT_LEVEL_2)%nmaux + nmauxc) > MAXMAUX )THEN
  CALL ReallocateMaterials( (materials(EDIT_LEVEL_2)%nmatl+10),(materials(EDIT_LEVEL_2)%nmaux+300) )
  IF( hasError() )CALL ShowInfoMessage( jwnd_db )
END IF

id_matl = materials(EDIT_LEVEL_2)%nmatl + 1
CALL FindHwndListId( IDB_MATDEF,jwnd_db,jd_level ) !  Parent = MATDEF
CALL init_material_edit( jwnd_db,jd_level )

RETURN
END
!*******************************************************************************
!            Compute Subgroup Definition Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_matcmp( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

!--- This routine initializes the compute subgroup definition Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

dbint(1,id_level) = NOT_SET_I
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )

dbreal(1,id_level) = NOT_SET_R
dbreal(2,id_level) = NOT_SET_R
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,2 )

nradio(1,id_level)  = 2 !Compute Type
ichoice(1,id_level) = 2 !Log
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

CALL update_compute_buttons( iwnd_db )

IF( listedt%dialog == IDB_CONTOUR )THEN
  string1 = 'No. contours :'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
  string1 = 'Minimum contour:'
  CALL SetControlText( iwnd_db,IDB_STATIC02,string1 )
  string1 = 'Maximum contour:'
  CALL SetControlText( iwnd_db,IDB_STATIC03,string1 )
  string1 = 'Contour generator'
  CALL SetControlText( iwnd_db,IDB_STATIC05,string1 )
ELSE IF( listedt%dialog == IDB_TERPARM )THEN
  string1 = 'No. grid points :'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
  string1 = 'Minimum height:'
  CALL SetControlText( iwnd_db,IDB_STATIC02,string1 )
  string1 = 'Maximum height:'
  CALL SetControlText( iwnd_db,IDB_STATIC03,string1 )
  string1 = 'Vertical grid generator'
  CALL SetControlText( iwnd_db,IDB_STATIC05,string1 )
END IF

RETURN
END
!*******************************************************************************
!            compute Subgroup Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_matcmp( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

!--- This routine computes subgroups

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER nn, jd_level
INTEGER(POINTER_LEN) jwnd_db
REAL    bmin, bmax
LOGICAL logb

logb = ichoice(1,id_level) == 2

nn = dbint(1,id_level)
IF( listedt%dialog == IDB_MATDEF )THEN
  nn = nn + 1
END IF

bmin = dbreal(1,id_level)
bmax = dbreal(2,id_level)

IF( lokbutton )THEN !OK Button - Post to MATDEF
  CALL FindHwndListId( listedt%dialog,jwnd_db,jd_level )
ELSE !Show Button - Post to self
  jwnd_db = iwnd_db
  jd_level = id_level
  CALL clear_list_numeric( jwnd_db,listedt%list,jd_level )
END IF

CALL compute_list_numeric( jwnd_db,listedt%list,jd_level, &
                                      bmin,bmax,nn,logb,1.0 )

RETURN
END
!*******************************************************************************
!            Edit List Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_edtlst( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!--- This routine initializes the compute subgroup definition Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level ,indx

CALL FindHwndListId( listedt%dialog,jwnd_db,jd_level )
CALL GetListCurSel( jwnd_db,listedt%list,indx )

indx = MAX(indx,0)

!lst      listedt.value = dblst(indx+1,jd_level)
listedt%value = dblst(jd_level,indx+1)
listedt%indx  = indx

CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE )
CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
CALL ShowControl(   iwnd_db,IDB_BUTTON1,SW_HIDE )
CALL ShowControl(   iwnd_db,IDB_BUTTON2,SW_HIDE )

dbreal(1,id_level) = listedt%value
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            New List Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_newlst( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!--- This routine initializes the compute subgroup definition Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

listedt%indx  = NOT_SET_I
listedt%value = NOT_SET_R

CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
CALL ShowControl( iwnd_db,IDB_BUTTON2,SW_HIDE )

string1 = '&Add'
CALL SetControlText( iwnd_db,IDB_BUTTON1,string1 )

dbreal(1,id_level) = listedt%value
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            Save Edit List Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_edtlst( id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
!
!     This routine initializes the compute subgroup definition Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level
LOGICAL lbad

IF( dbreal(1,id_level) == listedt%value )RETURN

CALL FindHwndListId( listedt%dialog,jwnd_db,jd_level )
CALL SetListCurSel( jwnd_db,listedt%list,listedt%indx )
CALL delete_list_numeric( jwnd_db,listedt%list,jd_level )

IF( dbreal(1,id_level) /= NOT_SET_R )THEN
  CALL add_list_numeric( jwnd_db,listedt%list,jd_level, &
                                    dbreal(1,id_level),lbad )
END IF

RETURN
END
!*******************************************************************************
!            Save New List Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_newlst( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
!
!     This routine initializes the compute subgroup definition Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level
LOGICAL lbad

IF( dbreal(1,id_level) == listedt%value )RETURN

CALL FindHwndListId( listedt%dialog,jwnd_db,jd_level )

IF( dbreal(1,id_level) /= NOT_SET_R )THEN
  CALL add_list_numeric( jwnd_db,listedt%list,jd_level, &
                                    dbreal(1,id_level),lbad )
END IF

dbreal(1,id_level) = listedt%value
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            Check Release Definitions
!*******************************************************************************
SUBROUTINE check_material_definition( lOK )

IMPLICIT NONE

LOGICAL, INTENT( INOUT ):: lOK

!--- This routine checks the material definitions - very simple for now

lOK = .TRUE.

RETURN
END
!*******************************************************************************
!            Initialize Liquid parameters Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_matparam( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE SwinAPI
USE liqdef

!
!     This routine initializes the new material Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, i, id

CHARACTER(128), EXTERNAL :: StripNull

LOGICAL, EXTERNAL :: IsWetParticle

CALL FindHwndList( iwnd_db,id,jd_level )

CALL FindHwndListId( IDB_MATDEF,jwnd_db,jd_level ) !  Parent = MATDEF

IF( id == IDB_AERPARM )THEN

  DO i = 1,9
    dbreal(i,id_level) = dbreal(i+3,jd_level)
  END DO
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,9 )
  IF( save_flag == FALSE )THEN
    DO i = 1,9
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,ID_OK,FALSE )
    CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
    CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  END IF

ELSE IF( id == IDB_LIQPARM )THEN

  DO i = 1,14
    dbreal(i,id_level) = dbreal(i+3,jd_level)
  END DO
  IF( dbreal(9,jd_level) == NOT_SET_R .OR. &
      dbreal(9,jd_level) == DEF_VAL_R )THEN
    dbreal(6,id_level) = dbreal(9,jd_level)
  ELSE
    dbreal(6,id_level) = ABS(dbreal(9,jd_level))
  END IF
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,14 )

  lcheck(1,id_level) = dbreal(9,jd_level) > 0.0
  CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )

  IF( save_flag == FALSE )THEN
    DO i = 1,14
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,CHECK_BASE+1,FALSE )
    CALL EnableControl( iwnd_db,ID_OK,FALSE )
    CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
    CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  ELSE
    IF( .NOT.project(EDIT_LEVEL_2)%Dynamic )THEN
      CALL EnableControl( iwnd_db,IDB_REAL1,FALSE )
    END IF
  END IF
ELSE IF( id == IDB_PRTPARM )THEN
  DO i = 1,2
    dbreal(i,id_level) = dbreal(i+3,jd_level)
  END DO
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,2 )

  lcheck(1,id_level) = IsWetParticle( dbint(20,jd_level) )
  CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )

  IF( save_flag == FALSE )THEN
    DO i = 1,2
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,CHECK_BASE+1,FALSE )
    CALL EnableControl( iwnd_db,ID_OK,FALSE )
    CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
    CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  END IF
ELSE IF( id == IDB_GASPARM )THEN
  DO i = 1,3
    dbreal(i,id_level) = dbreal(i+3,jd_level)
  END DO
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,3 )

  IF( save_flag == FALSE )THEN
    DO i = 1,3
      CALL EnableControl( iwnd_db,REAL_BASE+i,FALSE )
    END DO
    CALL EnableControl( iwnd_db,ID_OK,FALSE )
    CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
    CALL SetControlText( iwnd_db,ID_CANCEL,'&OK' )
  ELSE
    IF( .NOT.project(EDIT_LEVEL_2)%Dynamic )THEN
      CALL EnableControl( iwnd_db,IDB_REAL1,FALSE )
    END IF
  END IF
END IF
string2 = StripNull( dbtext(2,jd_level) )
string1 = '('//TRIM(string2)//'/m3)'
CALL SetControlText( iwnd_db,IDB_STATIC22,TRIM(string1) )

RETURN
END
!*******************************************************************************
!            save liquid parameters Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_matparam( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
!
!     This routine saves the new material Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, i, id

CHARACTER(128), EXTERNAL :: AddNull

CALL FindHwndList( iwnd_db,id,jd_level )

CALL FindHwndListId( IDB_MATDEF,jwnd_db,jd_level ) !  Parent = MATDEF

IF( id == IDB_AERPARM )THEN
  DO i = 1,9
    dbreal(i+3,jd_level) = dbreal(i,id_level)
  END DO
ELSE IF( id == IDB_LIQPARM )THEN
  DO i = 1,14
    dbreal(i+3,jd_level) = dbreal(i,id_level)
  END DO

  IF( .NOT.lcheck(1,id_level) )THEN
    IF( dbreal(6,id_level) /= NOT_SET_R .AND. &
        dbreal(6,id_level) /= DEF_VAL_R )THEN
      dbreal(9,jd_level) = -ABS(dbreal(6,id_level))
    END IF
  END IF
ELSE IF( id == IDB_PRTPARM )THEN
  DO i = 1,2
    dbreal(i+3,jd_level) = dbreal(i,id_level)
  END DO
  IF( lcheck(1,id_level) )THEN
    dbtext(3,jd_level) = AddNull( TRIM(MAT_WET) )
    dbint(20,jd_level) = IBSET(dbint(20,jd_level),MATID_WETP)
    dbint(20,jd_level) = IBCLR(dbint(20,jd_level),MATID_PRT)
  ELSE
    dbtext(3,jd_level) = AddNull( TRIM(MAT_PRT) )
    dbint(20,jd_level) = IBCLR(dbint(20,jd_level),MATID_WETP)
    dbint(20,jd_level) = IBSET(dbint(20,jd_level),MATID_PRT)
  END IF
ELSE IF( id == IDB_GASPARM )THEN
  DO i = 1,3
    dbreal(i+3,jd_level) = dbreal(i,id_level)
  END DO
END IF

RETURN
END
!***********************************************************************
!               DeleteMaterial
!***********************************************************************
SUBROUTINE delete_material( iwnd_db,id_level )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi

!--- This routine deletes a material definition and all associated releases

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)

LOGICAL lok, verify_button
INTEGER mrel, nrel ,mdel, id, i

LOGICAL, EXTERNAL :: hasError
CHARACTER(128) eMessage

mrel = 0

IF( scenario(EDIT_LEVEL_2)%nrel > 0 )THEN
  DO i = 1,scenario(EDIT_LEVEL_2)%nrel
    IF( scenario(EDIT_LEVEL_2)%release(i)%matl == cur_material%cmat )THEN
      mrel = mrel + 1
    END IF
  END DO
END IF

IF( mrel == 0 )THEN
  string1 = 'Delete the '//TRIM(cur_material%cmat)//' definition'
  lok = verify_button( iwnd_db,TRIM(string1) )
ELSE
  WRITE(eMessage,'(A,I2,A)')TRIM(cur_material%cmat)//' is referenced by' &
                ,mrel,' release(s)'
  CALL SetError( WN_ERROR, &
                 eMessage,' ', &
                'Do you want to delete the material and all associated releases?', &
                'DeleteMaterial' )
  CALL ShowWarningMessage( iwnd_db,.FALSE. )
  lok = .NOT.hasError()
  CALL InitError()
END IF

IF( lok )THEN
  IF( mrel > 0 )THEN
    mdel = 0
    nrel = scenario(EDIT_LEVEL_2)%nrel
    DO i = nrel,1,-1
      IF( scenario(EDIT_LEVEL_2)%release(i)%matl == cur_material%cmat )THEN
        id = i
        CALL delete_current_release( id )
        IF( hasError() )THEN
          CALL ShowErrorMessage( iwnd_db )
        ELSE
          mdel = mdel + 1
        END IF
      END IF
    END DO
    IF( mdel /= mrel )THEN
      WRITE(eMessage,'(A,I2,A)')'Only ',mdel,' releases were deleted'
      CALL SetError( UK_ERROR, &
                    'Encountered error deleting associated releases', &
                     eMessage, &
                    'Use the release editor to verify releases', &
                    'DeleteMaterial' )
      CALL ShowErrorMessage( iwnd_db )
    END IF
  END IF
  CALL delete_current_material( 2,id_matl )
  CALL update_material_buttons( iwnd_db )
  CALL load_current_material( id_matl )
  CALL init_dialog_materials( iwnd_db,id_level )
END IF

RETURN
END

!===============================================================================

SUBROUTINE check_dense( iwnd_db,id_level,l_askUser,nmatl_start )

USE resource_fd
USE class_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi

IMPLICIT NONE

REAL, PARAMETER :: RHOAIR = 1.2

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db
INTEGER,              INTENT( IN ) :: id_level
LOGICAL,              INTENT( IN ) :: l_askUser
INTEGER,              INTENT( IN ) :: nmatl_start

INTEGER(POINTER_LEN) jwnd_db
INTEGER jd_level, i, iaux
LOGICAL ldyn

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: hasError

!---- If DenseGas flag already set - no action needed

IF( project(id_level)%DenseGas )RETURN

!---- Check to see if any materials to check

IF( nmatl_start>materials(id_level)%nmatl )RETURN

!---- Loop over releases checking for dynamics

ldyn = .FALSE.
DO i = nmatl_start,materials(id_level)%nmatl
  IF( IsGas(materials(id_level)%material(i)%icls) .OR. &
      IsLiquid(materials(id_level)%material(i)%icls) )THEN
    iaux = materials(id_level)%material(i)%iaux

!=========Sort of a fix to catch old material files that have gas density in kg/cm3
!         that were not used and were really particle type densities.
    IF( materials(id_level)%mat_aux(iaux) > 20.*RHOAIR )THEN
      materials(id_level)%mat_aux(iaux) = RHOAIR
    END IF
!=========

    ldyn = ldyn .OR.  materials(id_level)%mat_aux(iaux) /= RHOAIR
  ELSE IF( IsAerosol(materials(id_level)%material(i)%icls) )THEN
    ldyn = .TRUE.
  END IF
END DO

!---- Found DenseGas - Let user know and ask for action if necessary

IF( ldyn )THEN
  IF( l_askUser )THEN
    CALL SetError( WN_ERROR, &
                  'Found materials with dense gas/vapor phase density', &
                  'Current project does not include dense gas dynamics.', &
                  'Do you want to change to a dense gas project?', &
                  'CheckGasDensity' )
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
    project(id_level)%DenseGas = .NOT.hasError()
    CALL InitError()
  ELSE
    project(id_level)%DenseGas = .TRUE.
  END IF
  project(id_level)%Dynamic = project(id_level)%Dynamic  .OR. &
                              project(id_level)%DenseGas

!------ Change icon if adding dynamics
  IF( project(id_level)%DenseGas )THEN
    CALL FindHwndListId( IDB_EDTPRJ,jwnd_db,jd_level )
    CALL set_project_icon( jwnd_db,id_level,IDB_STATIC07 )

!------ Warn User about ignoring
  ELSE
    CALL SetError( NO_ERROR, &
                  'All dense gas dynamics will be ignored',' ',' ', &
                  'CheckGasDensity' )
    CALL ShowInfoMessage( iwnd_db )
  END IF
END IF

RETURN
END
!***********************************************************************************************
! CopyMaterial
!***********************************************************************************************
SUBROUTINE CopyMaterial( From,To )

USE GUImatl_fi
USE GUIparam_fd

TYPE( matdef_str ), INTENT( IN  ) :: From
TYPE( matdef_str ), INTENT( OUT ) :: To

INTEGER i, j
INTEGER alloc_stat

To%nmatl = From%nmatl
DO i = 1,From%nmatl
  To%material(i) = From%material(i)
  IF( ASSOCIATED(To%materialMC(i)%name) )DEALLOCATE( To%materialMC(i)%name,STAT=alloc_stat )
  NULLIFY( To%materialMC(i)%name )
  IF( ASSOCIATED(From%materialMC(i)%name) )THEN
    ALLOCATE( To%materialMC(i)%name(SIZE(From%materialMC(i)%name)),STAT=alloc_stat )
    DO j = 1,SIZE(From%materialMC(i)%name)
      To%materialMC(i)%name(j) = From%materialMC(i)%name(j)
    END DO
  END IF
  To%materialMC(i)%units = From%materialMC(i)%units
END DO

To%nmaux = From%nmaux
DO i = 1,From%nmaux
  To%mat_aux(i) = From%mat_aux(i)
END DO

j = SIZE(To%material)
DO i = To%nmatl+1,j
  To%material(i) = materials(DEFAULT_LEVEL)%material(1)
  To%materialMC(i)%units = ' '
  IF( ASSOCIATED(To%materialMC(i)%name) )THEN
    DEALLOCATE( To%materialMC(i)%name,STAT=alloc_stat )
    NULLIFY( To%materialMC(i)%name )
  END IF
END DO

j = SIZE(To%mat_aux)
DO i = To%nmaux+1,j
  To%mat_aux(i) = 0.0
END DO

RETURN
END
!==============================================================================
! REALLOCATE_MATAUX
!==============================================================================
SUBROUTINE reallocate_mataux( max )

USE errorParam_fd
USE GUImatl_fi
USE reallocate

IMPLICIT NONE

INTEGER, INTENT( IN ) :: max

INTEGER alloc_stat, i, aux_sz

CHARACTER(128) eInform

IF( max > MAXMAUX )THEN

  DO i = -1,2

    alloc_stat = 0

    IF( ASSOCIATED( materials(i)%mat_aux ))THEN
      aux_sz = SIZE(materials(i)%mat_aux)
      IF( max > aux_sz )THEN
        alloc_stat =  reallocate_real1d( materials(i)%mat_aux,max-aux_sz )
      END IF
    ELSE
      ALLOCATE( materials(i)%mat_aux(max),STAT=alloc_stat )
    END IF
    IF( alloc_stat /= 0 )THEN
      WRITE(eInform,*)'Allocation request =',max,' : Error =',alloc_stat
      eInform = ADJUSTL(eInform)
      CALL SetError( UK_ERROR,'Error reallocating mat auxiliary array', &
                     eInform,' ','reallocate_mataux' )
      GOTO 9999
    END IF

  END DO

  MAXMAUX = max

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_cldtrans_matl( filename,max,nm,matl )

USE errorParam_fd
USE files_fi

! This routine reads a CLOUDTRANS puff file header and returns the
! material names from the file

IMPLICIT NONE

CHARACTER(*),               INTENT( IN  ) :: filename
INTEGER,                    INTENT( IN  ) :: max
INTEGER,                    INTENT( OUT ) :: nm
CHARACTER(*), DIMENSION(*), INTENT( OUT ) :: matl

INTEGER idum,i,ios,narg,nch
LOGICAL lerr

CHARACTER(8)   cdum
CHARACTER(128) eInform, eString
CHARACTER(90)  line
CHARACTER(90)  kwrd

!----- Initialize

nm = 0

!----- Open file

OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( OP_ERROR, &
                'Error opening source release input file', &
                 eString, &
                'Check scenario file for proper specification', &
                'getCLDTRANSmatl' )
  GOTO 9999
END IF

!------ Skip over header

READ(lun_tmp,101,ERR=1000,END=1000) idum

DO i = 1,idum-1
  READ(lun_tmp,100,ERR=1000,END=1000)
END DO

!------ Skip initial data

READ(lun_tmp,101,ERR=1000,END=1000)idum !No. Input puffs

READ(lun_tmp,101,ERR=1000,END=1000)idum !Data fields/puff

READ(lun_tmp,103,ERR=1000,END=1000)(cdum,i=1,idum) !Data field names
READ(lun_tmp,103,ERR=1000,END=1000)(cdum,i=1,idum) !Data units
READ(lun_tmp,101,ERR=1000,END=1000)nm !No. Input types
IF( nm > max )THEN
  WRITE(eInform,'(A,I5)')'Maximum number is ',max
  CALL ReportFileName( eString,'File=',filename )
  CALL SetError( SZ_ERROR, &
                'Too many material types in CLOUDTRANS file', &
                 eInform, &
                 eString, &
                'getCLDTRANSmatl' )
  GOTO 9999
END IF

READ(lun_tmp,101,ERR=1000,END=1000)(idum,i=1,nm)    !No. Subgroups/type
!READ(lun_tmp,103,ERR=1000,END=1000)(matl(i),i=1,nm) !Material names
i = 0
DO
  READ(lun_tmp,'(A)',ERR=1000,END=1000) line               !Material names
  CALL get_next_data( 0,line(24:),nch,kwrd,narg,matl(i+1),nm-i,lerr )
  IF( lerr )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( RD_ERROR, &
                  'Error reading material names on Cloudtrans file', &
                   eString,' ', &
                  'getCLDTRANSmatl' )
    GOTO 9999
  END IF
  i = i + narg
  IF( i == nm )THEN
    EXIT
  ELSE IF( i > nm )THEN
    CALL ReportFileName( eString,'File=',filename )
    CALL SetError( RD_ERROR, &
                  'Too many material names on Cloudtrans file', &
                   eString,' ', &
                  'getCLDTRANSmatl' )
    GOTO 9999
  END IF
END DO

9999 CONTINUE

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN

1000 CONTINUE
CALL ReportFileName( eString,'File=',filename )
CALL SetError( RD_ERROR, &
              'Error reading CLOUDTRANS data', &
               eString,' ', &
              'getCLDTRANSmatl' )
GOTO 9999

100 FORMAT(A)
101 FORMAT((23X,5(I6,10X)))
103 FORMAT((23X,4(2X,A8)))

END
!***********************************************************************************************
! ReallocateMaterials
!***********************************************************************************************
SUBROUTINE ReallocateMaterials( mSize,aSize )

USE create_fi
USE GUIparam_fd
USE GUImatl_fi
USE errorParam_fd
use pcscipuf_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mSize
INTEGER, INTENT( IN ) :: aSize

TYPE( matdef_str ) savMtl
INTEGER i

LOGICAL, EXTERNAL :: hasError

CHARACTER(48) aString

! Increase materials size
NULLIFY(savMtl%material)
NULLIFY(savMtl%mat_aux)
NULLIFY(savMtl%materialMC)

IF( ASSOCIATED(materials(DEFAULT_LEVEL)%material) )THEN
  materials(DEFAULT_LEVEL)%nmatl = nmatl_def
  materials(DEFAULT_LEVEL)%nmaux = nmaux_def
  WRITE(aString,'(A,I3)')'temporary materials for level',DEFAULT_LEVEL
  CALL AllocateMaterial( savMtl,nmatl_def,nmaux_def )
  IF( hasError() )GOTO 9999
  CALL CopyMaterial( materials(DEFAULT_LEVEL),savMtl )
  WRITE(aString,'(A,I3)')'new materials for level',DEFAULT_LEVEL
  CALL AllocateMaterial( materials(DEFAULT_LEVEL),mSize,aSize )
  IF( hasError() )GOTO 9999
  CALL CopyMaterial( savMtl,materials(DEFAULT_LEVEL) )
  materials(DEFAULT_LEVEL)%nmatl = 0
  materials(DEFAULT_LEVEL)%nmaux = 0
  CALL DeallocateMaterial( savMtl )
END IF

DO i = BASE_LEVEL,EDIT_LEVEL_2
  IF( ASSOCIATED(materials(i)%material) )THEN
    WRITE(aString,'(A,I3)')'temporary materials for level',i
    CALL AllocateMaterial( savMtl,materials(i)%nmatl,materials(i)%nmaux )
    IF( hasError() )GOTO 9999
    CALL CopyMaterial( materials(i),savMtl )
    WRITE(aString,'(A,I3)')'new materials for level',i
    CALL AllocateMaterial( materials(i),mSize,aSize )
    CALL CopyMaterial( savMtl,materials(i) )
    IF( hasError() )GOTO 9999
    CALL DeallocateMaterial( savMtl )
  END IF
END DO

MAXMTYP = mSize
MAXMAUX = aSize

1000 CONTINUE

CALL DeallocateMaterial( savMtl )

RETURN

9999 CONTINUE
WRITE(string1,*)'new size =',mSize,aSize
WRITE(string2,*)'Allocation call sequence ='//TRIM(aString)
CALL SetError( SZ_ERROR,'Allocation Error',string2,string1,'ReallocateMaterials' )
GOTO 1000

END
!***********************************************************************************************
! AllocateMaterial
!***********************************************************************************************
SUBROUTINE AllocateMaterial( mtl,mSize,aSize )

USE GUImatl_fd
USE error_fi

IMPLICIT NONE

TYPE( matdef_str ), INTENT( INOUT ) :: mtl
INTEGER,            INTENT( IN    ) :: mSize
INTEGER,            INTENT( IN    ) :: aSize

INTEGER ios
INTEGER i

CALL DeallocateMaterial( mtl )

ALLOCATE( mtl%material(mSize),STAT=ios)
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eRoutine = 'AllocateMaterial'
  eMessage = 'Allocation Error : materials(matl)'
  WRITE(eInform,*)'Request=',mSize,' : ErrorCode=',ios
  GOTO 9999
END IF
ALLOCATE( mtl%materialMC(mSize),STAT=ios)
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eRoutine = 'AllocateMaterial'
  eMessage = 'Allocation Error : materials(matlMC)'
  WRITE(eInform,*)'Request=',mSize,' : ErrorCode=',ios
  GOTO 9999
END IF
DO i = 1,mSize
  NULLIFY( mtl%materialMC(i)%name )
END DO
ALLOCATE( mtl%mat_aux(aSize),STAT=ios )
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eRoutine = 'AllocateMaterial'
  eMessage = 'Allocation Error : materials(aux)'
  WRITE(eInform,*)'Request=',aSize,' : ErrorCode=',ios
  GOTO 9999
END IF

9999 CONTINUE
IF( nError /= NO_ERROR )THEN
  CALL SetError( nError,TRIM(eMessage),TRIM(eInform),' ',TRIM(eRoutine) )
END IF

RETURN
END
!***********************************************************************************************
! DeallocateMaterial
!***********************************************************************************************
SUBROUTINE DeallocateMaterial( mtl )

USE GUImatl_fd

IMPLICIT NONE

TYPE( matdef_str ), INTENT( INOUT ) :: mtl

INTEGER ios
INTEGER i

IF( ASSOCIATED(mtl%material) )DEALLOCATE( mtl%material,STAT=ios )
IF( ASSOCIATED(mtl%materialMC) )THEN
  DO i = 1,SIZE(mtl%materialMC)
    IF( ASSOCIATED(mtl%materialMC(i)%name) )DEALLOCATE( mtl%materialMC(i)%name,STAT=ios )
  END DO
  DEALLOCATE( mtl%materialMC,STAT=ios )
END IF
IF( ASSOCIATED(mtl%mat_aux) )DEALLOCATE( mtl%mat_aux,STAT=ios )

NULLIFY( mtl%material )
NULLIFY( mtl%materialMC )
NULLIFY( mtl%mat_aux )

9999 CONTINUE

RETURN
END
