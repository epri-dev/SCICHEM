MODULE pltopt_fd

  INTEGER OPT_V130 !Add casualty risk level flag, pev
  INTEGER OPT_V090 !Add hazard area flag, pexceed
  INTEGER OPT_V050 !Breakup pltopt into pltopt,pltopt2 and add
  INTEGER OPT_V042 !Add Map on axesdef
  INTEGER OPT_V041 !Add Circles on poptdef
  INTEGER OPT_V040 !Add Terrain on poptdef
  INTEGER OPT_V034 !Add NWPdos,NWIdos,UserTime to pltdef
  INTEGER OPT_V033 !Add Estring,SCIP on pltdef
  INTEGER OPT_V03 !Add Labels on condef
  INTEGER OPT_V0273 !Add Nuc Facility/Text on mapdef
  INTEGER OPT_V027 !Added second condef record
  INTEGER OPT_V023 !Added ticV,ticH on axesdef and scale,Units on condef
  INTEGER OPT_V021 !???
  INTEGER OPT_V02 !Original
  PARAMETER (OPT_V130  = 1924)
  PARAMETER (OPT_V090  = 1920)
  PARAMETER (OPT_V050  = 1912)
  PARAMETER (OPT_V042  = 1764)
  PARAMETER (OPT_V041  = 1760)
  PARAMETER (OPT_V040  = 1756)
  PARAMETER (OPT_V034  = 1752)
  PARAMETER (OPT_V033  = 1740)
  PARAMETER (OPT_V03   = 1720)
  PARAMETER (OPT_V0273 = 1712)
  PARAMETER (OPT_V027  = 1704)
  PARAMETER (OPT_V023  = 1484)
  PARAMETER (OPT_V021  = 1456)
  PARAMETER (OPT_V02   = 1452)

END MODULE pltopt_fd
