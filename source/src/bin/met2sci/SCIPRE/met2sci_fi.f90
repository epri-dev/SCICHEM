MODULE met2sci_fi

  USE MetSCIparam_fd
  USE MetSCIerrorParam_fd  
  USE MaxChar_fd
  USE error_fd
  USE fileList_fd
  USE OSread_fd

  TYPE( error_str ) :: error

  CHARACTER(MAXC) inputFile, SFCfile, UAfile, OSfile
  CHARACTER(MAXC) reportFile, messageFile
!  CHARACTER(MAXC) QAfileUA, QAfileSFC  ** use scratch files **

  INTEGER lun, lunSfc, lunUA, lunOS
  INTEGER lunOutSFC, lunOutUA, lunOutOS
  INTEGER lunRpt, lunMsg, lunErr
  INTEGER lunTmp, lunScratch
  INTEGER lunSFQ, lunUAQ, lunOSQ
  INTEGER ios, alloc_stat

  LOGICAL lJob, lUpperAir, lSurface, lOnsite
  LOGICAL lUAQ, lSFQ, lRandUA, lRandSFC, lRandOS
  LOGICAL lexist

  REAL    dRan0, dRanUA, dRanSFC, dRanOS

  TYPE( firstFile ) :: UAlist
  TYPE( firstFile ) :: SFClist
  TYPE( firstFile ) :: OSlist

  TYPE( fileList ), POINTER :: currentFile

  INTEGER yearStartUA,  yearEndUA,  yearStartSFC,  yearEndSFC,  yearStartOS,  yearEndOS
  INTEGER monthStartUA, monthEndUA, monthStartSFC, monthEndSFC, monthStartOS, monthEndOS
  INTEGER dayStartUA,   dayEndUA,   dayStartSFC,   dayEndSFC,   dayStartOS,   dayEndOS

!------ get_next_data variables

  INTEGER, PARAMETER :: MAXN = 40     !Max. no. of arguments on a line

  INTEGER nch, narg, ikwrd
  LOGICAL lerr

  CHARACTER(16)   kwrd
  CHARACTER(1000) line

  CHARACTER(128), DIMENSION(MAXN) :: carg

!------ For parsing onsite data

  INTEGER nOSrecord, nOSformat, nOSrange, nOSheight

  TYPE( firstOSrecord ) :: firstOSread
  TYPE( firstOSrecord ) :: firstOSformat
  TYPE( firstOSrecord ) :: firstOSrange

  TYPE( OSReadRec ), POINTER :: OSread

  CHARACTER(1000) OSdataLine, OSqaoutLine, OSxdatesLine, OSlocationLine, OSheightLine, &
                  OSauditLine, OSobsperhourLine, OSnomissingLine, OSthresholdLine

  INTEGER nvars, nvarp, iPress, iPressSL
  INTEGER iOSDY, iOSMO, iOSYR, iOSHR, iOSMN
  LOGICAL lOSHGT, lOSHTX


  INTEGER, DIMENSION(:), ALLOCATABLE :: OSstype, OSptype
 
END MODULE met2sci_fi
