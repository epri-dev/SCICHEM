!==== SAG_MAXVAL MODULE

MODULE sagmaxval_usr
  SAVE
  INTEGER ifld_maxval
  REAL    out_maxval
  REAL    out_minval
END MODULE sagmaxval_usr

!==== Contour functions

MODULE sagcnt_usr
  USE sagcnt_fd
  SAVE
  TYPE( SAGcontour_str ), POINTER :: contour
END MODULE sagcnt_usr

!==== SAG_MERGE

MODULE sagmrg_usr
  SAVE
  INTEGER nfld_mrg
  INTEGER ifld_mrg
  INTEGER jfld_mrg
END MODULE sagmrg_usr

!==== SAG_DRAW

MODULE sagdrw_usr
  USE sagdrw_fd
  SAVE
  TYPE( SAGdrawfld_str )draw
  REAL poly_val
END MODULE sagdrw_usr

!==== SAG_WRT

MODULE sagwrt_usr
  USE sagwrt_fd
  SAVE
  TYPE ( SAGwritefld_str ) uwrite
END MODULE sagwrt_usr

!==== SAG_ADD & SAG_ADDVARIANCE  & SAG_COPY

MODULE sagadd_usr

  SAVE
  INTEGER ifld_add
  INTEGER jfld_add
  REAL    scale_add
  REAL   max_value
  REAL   min_value
  REAL, PARAMETER :: NOT_USED = -1.0e+36

END MODULE sagadd_usr

!==== SAG_NODEVALUE

MODULE sagnod_usr

  USE sagnod_fd
  USE sagstr_fd

  SAVE
  INTEGER nfld_nod
  INTEGER, pointer, dimension(:) :: ifld_nod
  TYPE( SAGnodeT_str ), pointer  :: nodeT
  TYPE( SAGgrid_str ),  pointer  :: grdN

END MODULE sagnod_usr

!==== SAG_GETPOINTSTD

MODULE GetPointStd_usr

  SAVE
  INTEGER ifld_getpt
  INTEGER nvar_getpt

END MODULE GetPointStd_usr

!==== SAG use of thread-safe population dll

MODULE sagpop_fi

  SAVE
  INTEGER threadID
!$OMP THREADPRIVATE( threadID )

END MODULE sagpop_fi
