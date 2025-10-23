MODULE netcdf_fd

!------ Interface block for netCDF calls

INCLUDE 'netcdf.inc'

INTERFACE

  TYPE(C_PTR) FUNCTION nc_inq_libvers() BIND(C)  !C character function
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
  END FUNCTION nc_inq_libvers


  TYPE(C_PTR) FUNCTION nc_strerror( ios ) BIND(C)  !C character function
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_PTR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN )  :: ios
  END FUNCTION nc_strerror


  INTEGER(C_INT) FUNCTION nc_open( fname,omode,fid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(KIND=C_CHAR),                    INTENT( IN )  :: fname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN )  :: omode
    INTEGER(C_INT),                            INTENT( OUT ) :: fid
  END FUNCTION nc_open


  INTEGER(C_INT) FUNCTION nc_close( fid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid
  END FUNCTION nc_close


  INTEGER(C_INT) FUNCTION nc_inq( fid,ndims,nvars,natts,unlimdimID ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN )  :: fid
    INTEGER(C_INT),                            INTENT( OUT ) :: ndims, nvars, natts, unlimdimID
  END FUNCTION nc_inq


  INTEGER(C_INT) FUNCTION nc_inq_var( fid,varid,varnam,ivtype,ndims,dimids,natts ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( OUT ) :: varnam(*)
    INTEGER(C_INT),                            INTENT( OUT ) :: ivtype, ndims, natts
    INTEGER(C_INT), DIMENSION(*),              INTENT( OUT ) :: dimids
  END FUNCTION nc_inq_var


  INTEGER(C_INT) FUNCTION nc_inq_varid( fid,varnam,varid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: varnam(*)
    INTEGER(C_INT),                            INTENT( OUT ) :: varid
  END FUNCTION nc_inq_varid


  INTEGER(C_INT) FUNCTION nc_inq_vartype( fid,varid,ivtype ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INT),                            INTENT( OUT ) :: ivtype
  END FUNCTION nc_inq_vartype


  INTEGER(C_INT) FUNCTION nc_inq_attname( fid,iv,attnum,attname ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, iv, attnum
    CHARACTER(KIND=C_CHAR),                    INTENT( OUT ) :: attname(*)
  END FUNCTION nc_inq_attname


  INTEGER(C_INT) FUNCTION nc_inq_atttype( fid,varid,attname,atttype ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: attname(*)
    INTEGER(C_INT),                            INTENT( OUT ) :: atttype
  END FUNCTION nc_inq_atttype


  INTEGER(C_INT) FUNCTION nc_get_att_text( fid,varid,attname,string ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: attname(*)
    CHARACTER(KIND=C_CHAR),                    INTENT( OUT ) :: string(*)
  END FUNCTION nc_get_att_text


  INTEGER(C_INT) FUNCTION nc_get_att_float( fid,varid,attname,attval ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_FLOAT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: attname(*)
    REAL(C_FLOAT),                             INTENT( OUT ) :: attval
  END FUNCTION nc_get_att_float


  INTEGER(C_INT) FUNCTION nc_get_att_short( fid,varid,attname,attval ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_SHORT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: attname(*)
    INTEGER(C_SHORT),                          INTENT( OUT ) :: attval
  END FUNCTION nc_get_att_short


  INTEGER(C_INT) FUNCTION nc_get_att_int( fid,varid,attname,attval ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: attname(*)
    INTEGER(C_INT),                            INTENT( OUT ) :: attval
  END FUNCTION nc_get_att_int


  INTEGER(C_INT) FUNCTION nc_inq_dim( fid,dimid,dimname,dimlen ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, dimid
    CHARACTER(KIND=C_CHAR),                    INTENT( OUT ) :: dimname(*)
    INTEGER(C_INTPTR_T),                       INTENT( OUT ) :: dimlen
  END FUNCTION nc_inq_dim


  INTEGER(C_INT) FUNCTION nc_inq_dimid( fid,dimname,dimid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: dimname(*)
    INTEGER(C_INT),                            INTENT( OUT ) :: dimid
  END FUNCTION nc_inq_dimid


  INTEGER(C_INT) FUNCTION nc_get_vara_double( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    REAL(C_DOUBLE), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( OUT ) :: array
  END FUNCTION nc_get_vara_double


  INTEGER(C_INT) FUNCTION nc_get_vara_float( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_FLOAT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    REAL(C_FLOAT), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( OUT ) :: array
  END FUNCTION nc_get_vara_float


  INTEGER(C_INT) FUNCTION nc_get_vara_short( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_SHORT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    INTEGER(C_SHORT), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( OUT ) :: array
  END FUNCTION nc_get_vara_short


  INTEGER(C_INT) FUNCTION nc_get_vara_ubyte( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    INTEGER(1), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( OUT ) :: array
  END FUNCTION nc_get_vara_ubyte


  INTEGER(C_INT) FUNCTION nc_get_vara_text( fid,varid,istart,icount,string ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    CHARACTER(KIND=C_CHAR),                    INTENT( OUT ) :: string(*)
  END FUNCTION


  INTEGER(C_INT) FUNCTION nc_create( fname,omode,fid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    CHARACTER(KIND=C_CHAR),                    INTENT( IN )  :: fname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN )  :: omode
    INTEGER(C_INT),                            INTENT( OUT ) :: fid
  END FUNCTION nc_create


  INTEGER(C_INT) FUNCTION nc_def_dim( fid,dimname,dimlen,dimid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: dimname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: dimlen
    INTEGER(C_INT),                            INTENT( OUT ) :: dimid
  END FUNCTION nc_def_dim


  INTEGER(C_INT) FUNCTION nc_def_var( fid,varname,ivtype,ndims,dimids,varid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN  ) :: varname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: ivtype
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: ndims
    INTEGER(C_INT), DIMENSION(*),              INTENT( IN  ) :: dimids
    INTEGER(C_INT),                            INTENT( OUT ) :: varid
  END FUNCTION nc_def_var


  INTEGER(C_INT) FUNCTION nc_redef( fid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid
  END FUNCTION nc_redef


  INTEGER(C_INT) FUNCTION nc_enddef( fid ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid
  END FUNCTION nc_enddef


  INTEGER(C_INT) FUNCTION nc_put_vara_int( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    INTEGER(C_INT), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( IN ) :: array
  END FUNCTION nc_put_vara_int


  INTEGER(C_INT) FUNCTION nc_put_vara_float( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_FLOAT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    REAL(C_FLOAT), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( IN ) :: array
  END FUNCTION nc_put_vara_float


  INTEGER(C_INT) FUNCTION nc_put_vara_double( fid,varid,istart,icount,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, varid
    INTEGER(C_INTPTR_T), DIMENSION(*),         INTENT( IN  ) :: istart, icount
    REAL(C_DOUBLE), DIMENSION(icount(1),icount(2),icount(3),*), INTENT( IN ) :: array
  END FUNCTION nc_put_vara_double


  INTEGER(C_INT) FUNCTION nc_put_var_double( fid,varid,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    REAL(C_DOUBLE), DIMENSION(*),              INTENT( IN ) :: array
  END FUNCTION nc_put_var_double


  INTEGER(C_INT) FUNCTION nc_put_var_int( fid,varid,array ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    INTEGER(C_INT), DIMENSION(*),              INTENT( IN ) :: array
  END FUNCTION nc_put_var_int


  INTEGER(C_INT) FUNCTION nc_put_att_int( fid,varid,attname,alen,ivtype,attval ) BIND(C) !*** attval can be an array ***
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN ) :: attname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: ivtype
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: alen
    INTEGER(C_INT), DIMENSION(*),              INTENT( IN ) :: attval
  END FUNCTION nc_put_att_int


  INTEGER(C_INT) FUNCTION nc_put_att_float( fid,varid,attname,alen,ivtype,attval ) BIND(C) !*** attval can be an array ***
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_FLOAT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN ) :: attname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: ivtype
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: alen
    REAL(C_FLOAT), DIMENSION(*),               INTENT( IN ) :: attval
  END FUNCTION nc_put_att_float


  INTEGER(C_INT) FUNCTION nc_put_att_double( fid,varid,attname,alen,ivtype,attval ) BIND(C) !*** attval can be an array ***
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_CHAR, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN ) :: attname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: ivtype
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: alen
    REAL(C_DOUBLE), DIMENSION(*),              INTENT( IN ) :: attval
  END FUNCTION nc_put_att_double


  INTEGER(C_INT) FUNCTION nc_put_att_text( fid,varid,attname,slen,string ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_CHAR
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: fid, varid
    CHARACTER(KIND=C_CHAR),                    INTENT( IN ) :: attname(*)
    INTEGER(C_INT),                     VALUE, INTENT( IN ) :: slen
    CHARACTER(KIND=C_CHAR),                    INTENT( IN ) :: string(*)
  END FUNCTION nc_put_att_text


  INTEGER(C_INT) FUNCTION nc_inq_dimlen( fid,dimid,nlen ) BIND(C)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT, C_INTPTR_T
    IMPLICIT NONE
    INTEGER(C_INT),                     VALUE, INTENT( IN  ) :: fid, dimid
    INTEGER(C_INTPTR_T),                       INTENT( OUT ) :: nlen
  END FUNCTION nc_inq_dimlen


END INTERFACE

END MODULE netcdf_fd
