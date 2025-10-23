MODULE sensor_fd

!------ Interactive sensor structure; used for queries or to
!       modify sensor location or waypoints

  TYPE  sensorT
    SEQUENCE
    INTEGER id             !Sensor no.
    INTEGER init           !SCIPtrue -> clear waypoint list
    REAL    time
    REAL    x, y, z
    REAL    az, el, dist
    REAL    h
    REAL    mean, var
  END TYPE  sensorT

!------ Interactive sensor structure; used to calculate concentration
!       or LOS at an arbitrary location

  TYPE  InteractiveSensor
    SEQUENCE
    CHARACTER(64) matname    !Material name
    INTEGER isg              !Subgroup no.
    INTEGER iAGL             !Height flag: 1 for AGL, 0 for MSL
    REAL    x, y, z          !Location (project coord; z depends on iAGL)
    REAL    az, el, dist     !LOS azimuth, elevation angles & path length
    REAL    time             !current project time
    REAL    mean, var
  END TYPE  InteractiveSensor

!------ Interactive surface field sensor structure; used to get
!       deposition or dosage at an arbitrary location

  TYPE  InteractiveSrf
    SEQUENCE
    CHARACTER(64) matname    !Material name
    INTEGER isg              !Subgroup no.
    CHARACTER(64) stype      !'DEP' or 'DOS'
    REAL    time
    REAL    x, y             !Location (project coord)
    REAL    mean, var
  END TYPE  InteractiveSrf

END MODULE sensor_fd

