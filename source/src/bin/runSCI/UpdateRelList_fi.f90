MODULE UpdateRelList_fi

  USE UpdateRelList_fd

  INTEGER :: lun_rel0 = 500  !First unit number for release files

  TYPE( FirstRel ) :: FirstUpdateRel

!------ get_next_data variables

  INTEGER, PARAMETER :: MAXN = 40     !Max. no. of arguments on a line

  INTEGER nch, narg, ikwrd
  LOGICAL lerr

  CHARACTER(16)   kwrd
  CHARACTER(1000) line

  CHARACTER(128), DIMENSION(MAXN) :: carg

END MODULE UpdateRelList_fi
