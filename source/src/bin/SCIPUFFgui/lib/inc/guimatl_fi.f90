MODULE GUImatl_fi

  USE GUImatl_fd
  USE winAPI_fd, ONLY: POINTER_LEN

  SAVE

  INTEGER(POINTER_LEN) iwnd_dbm
  INTEGER id_matl,nmauxc,MAXMTYP,MAXMAUX
  INTEGER id_relMat

  INTEGER nmatl_def, nmaux_def

  TYPE( matdef_str ) materials(-1:2)

  TYPE( material_str ) cur_material
  REAL cur_aux(MAXSGP+MAXGMAUX+MAXLMAUXP+MAXLMAUXX) !Assumes Liquids require the most space

END MODULE GUImatl_fi
