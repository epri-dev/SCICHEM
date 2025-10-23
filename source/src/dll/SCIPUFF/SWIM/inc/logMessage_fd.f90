MODULE logMessage_fd

  TYPE logMessage
    SEQUENCE
    CHARACTER(128)              :: string
    TYPE( LogMessage ), POINTER :: next
  END TYPE logMessage

END MODULE logMessage_fd
