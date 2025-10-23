!=======================================================================
!    ERROR definitions
!=======================================================================
MODULE errorParam_fd

  INTEGER, PARAMETER :: EOF_ERROR =  -1  !EOF Error
  INTEGER, PARAMETER :: NO_ERROR  =   0  !No error
  INTEGER, PARAMETER :: NF_ERROR  =   1  !Not found error
  INTEGER, PARAMETER :: SZ_ERROR  =   2  !Size error
  INTEGER, PARAMETER :: IV_ERROR  =   3  !Invalid value error
  INTEGER, PARAMETER :: OP_ERROR  =   4  !Open error
  INTEGER, PARAMETER :: RD_ERROR  =   5  !Read error
  INTEGER, PARAMETER :: S0_ERROR  =   6  !Switch Zero error
  INTEGER, PARAMETER :: WR_ERROR  =   7  !Write error
  INTEGER, PARAMETER :: API_ERROR =   8  !API error
  INTEGER, PARAMETER :: SW_ERROR  =   9  !Switch error
  INTEGER, PARAMETER :: WN_ERROR  =  10  !Warning error
  INTEGER, PARAMETER :: AB_ERROR  =  11  !Abort error
  INTEGER, PARAMETER :: VN_ERROR  =  12  !Version error
  INTEGER, PARAMETER :: DM_ERROR  =  13  !Special Domain error
  INTEGER, PARAMETER :: BZ_ERROR  =  14  !SCIPtool Busy error
  INTEGER, PARAMETER :: OV_ERROR  =  15  !Overlay error
  INTEGER, PARAMETER :: RV_ERROR  =  16  !Met error for reverse calculation
  INTEGER, PARAMETER :: SKP_ERROR =  17  !Skip error - Skip updated release
  INTEGER, PARAMETER :: UK_ERROR  =  99  !Unknown error

END MODULE errorParam_fd
