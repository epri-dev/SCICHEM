MODULE GetTimes_fi

  USE tooluser_fd

  INTEGER nTimePuff, nTimeSrf, nTimeMet, nTimeOut
  INTEGER :: nNotUsed

  TYPE( SCIPTimeT ), DIMENSION(:), POINTER :: TimePuff
  TYPE( SCIPTimeT ), DIMENSION(:), POINTER :: TimeSrf
  TYPE( SCIPTimeT ), DIMENSION(:), POINTER :: TimeMet
  TYPE( SCIPTimeT ), DIMENSION(:), POINTER :: TimeOut

  TYPE( limitT ) :: limit

END MODULE GetTimes_fi
