!==== SCIP results parameters

MODULE SCIPresults_fd

  INTEGER, PARAMETER :: SCIPunknown     = -2
  INTEGER, PARAMETER :: SCIPfailure     =  -1
  INTEGER, PARAMETER :: SCIPnull        =   0
  INTEGER, PARAMETER :: SCIPsuccess     =   1
  INTEGER, PARAMETER :: SCIPnegative    = SCIPfailure
  INTEGER, PARAMETER :: SCIPaffirmative = SCIPsuccess
  INTEGER, PARAMETER :: SCIPvalid       = SCIPsuccess
  INTEGER, PARAMETER :: SCIPinvalid     = SCIPfailure
  INTEGER, PARAMETER :: SCIPerror       = SCIPfailure
  INTEGER, PARAMETER :: SCIPon          = SCIPsuccess
  INTEGER, PARAMETER :: SCIPoff         = SCIPnull
  INTEGER, PARAMETER :: SCIPtrue        = SCIPon
  INTEGER, PARAMETER :: SCIPfalse       = SCIPoff

END MODULE SCIPresults_fd
