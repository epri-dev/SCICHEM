MODULE relstruct_fd

  USE release_fd      !Basic release structures
  USE prjstruct_fd    !Basic project structures
  USE list_fd         !Basic list structures

  IMPLICIT NONE

!==== relControlT =============================================================

  TYPE relControlT
    SEQUENCE
    INTEGER       mode          !Indicates file type and search mode
    CHARACTER(32) searchID      !Indicates release ID
    CHARACTER(8)  fileExtension !Indicates file extension if not SCN file
  END TYPE  relControlT

  INTEGER, PARAMETER :: SIZE_relControlT = KIND(1) + 32 + 8

!==== preleaseT ===============================================================

  TYPE preleaseT
    SEQUENCE
    TYPE( projectIDT  ) project
    TYPE( listHeadT   ) scnHead
    TYPE( relControlT ) control
  END TYPE preleaseT

  INTEGER, PARAMETER :: SIZE_preleaseT = SIZE_projectIDT + SIZE_listHeadT + &
                                         SIZE_relControlT

  TYPE poolMassT
    SEQUENCE
    REAL relTime
    REAL Time
    REAL MassRemaining
    CHARACTER(16)   material    !relmat
    CHARACTER(32)   relName     !Release Identifier
    CHARACTER(192)  relDisplay  !Release display string
  END TYPE poolMassT

END MODULE relstruct_fd
