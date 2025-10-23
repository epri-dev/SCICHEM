!=======================================================================
!    DEFAULT VALUE definitions
!=======================================================================
MODULE default_fd

  INTEGER, PARAMETER :: DEF_VAL_I  =  (2**16-1)
  INTEGER, PARAMETER :: NOT_SET_I  = -DEF_VAL_I
  INTEGER, PARAMETER :: DEFERRED_I =  DEF_VAL_I - 1

  REAL, PARAMETER :: DEF_VAL_R  =    1.0e+36
  REAL, PARAMETER :: NOT_SET_R  =   -DEF_VAL_R
  REAL, PARAMETER :: DEFERRED_R = 2.*DEF_VAL_R

  REAL(8), PARAMETER :: DEF_VAL_D  =    1.0D+36
  REAL(8), PARAMETER :: NOT_SET_D  =   -DEF_VAL_R
  REAL(8), PARAMETER :: DEFERRED_D = 2.*DEF_VAL_R

  CHARACTER(64), PARAMETER :: NOT_SET_C = 'NOT SET'

END MODULE default_fd
