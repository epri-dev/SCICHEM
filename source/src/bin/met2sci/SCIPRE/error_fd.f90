MODULE MetSCIerrorParam_fd

  INTEGER, PARAMETER :: EOF_ERROR =  -1  !EOF Error
  INTEGER, PARAMETER :: NO_ERROR  =   0  !No error
  INTEGER, PARAMETER :: NF_ERROR  =   1  !Not found error
  INTEGER, PARAMETER :: SZ_ERROR  =   2  !Size error
  INTEGER, PARAMETER :: IV_ERROR  =   3  !Invalid value error
  INTEGER, PARAMETER :: OP_ERROR  =   4  !Open error
  INTEGER, PARAMETER :: RD_ERROR  =   5  !Read error
  INTEGER, PARAMETER :: WR_ERROR  =   7  !Write error
  INTEGER, PARAMETER :: WN_ERROR  =  10  !Warning error
  INTEGER, PARAMETER :: UK_ERROR  =  99  !Unknown error

END MODULE MetSCIerrorParam_fd

MODULE error_fd

  USE MaxChar_fd

  TYPE error_str

    INTEGER         :: Number
    CHARACTER(80)   :: Routine
    CHARACTER(MAXC) :: aString
    CHARACTER(MAXC) :: bString
    CHARACTER(MAXC) :: cString

  END TYPE error_str
  
END MODULE error_fd