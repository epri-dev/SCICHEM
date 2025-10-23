MODULE OSread_fd

  TYPE OSReadRec
    INTEGER rec
    CHARACTER(1000) line
    TYPE( OSReadRec ), POINTER :: next
  END TYPE

  TYPE firstOSrecord
    INTEGER nRec
    TYPE( OSReadRec ), POINTER :: first
  END TYPE firstOSrecord

END MODULE OSread_fd
