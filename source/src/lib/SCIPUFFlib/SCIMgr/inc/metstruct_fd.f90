MODULE metstruct_fd

  USE weather_fd      !Basic weather structures
  USE prjstruct_fd    !Basic project structures

!==== pweatherT

  TYPE pweatherT
    SEQUENCE
    TYPE( projectIDT ) project
    TYPE( weatherT   ) weather
  END TYPE pweatherT

  INTEGER, PARAMETER :: SIZE_pweatherT = SIZE_projectIDT + SIZE_weatherT

END MODULE metstruct_fd
