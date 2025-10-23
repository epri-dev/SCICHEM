MODULE message_fd

!==== messageT ================================================================

  TYPE  messageT
    SEQUENCE
    INTEGER        iParm
    INTEGER        jParm
    CHARACTER(128) aString
    CHARACTER(128) bString
    CHARACTER(128) cString
    CHARACTER(80)  routine
  END TYPE  messageT

END MODULE message_fd
