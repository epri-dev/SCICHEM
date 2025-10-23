MODULE SWIMinterp_fd

!------ met field interpolating structure

  TYPE  met1dh
    SEQUENCE
    INTEGER i
    REAL    dxr, rat, rm1
  END TYPE met1dh

  TYPE meth
    SEQUENCE
    INTEGER ij, nxyi, nxi
    REAL    dxr, dyr
    REAL    ratx, rxm1
    REAL    raty, rym1
    REAL    cc1, cc2, cc3, cc4
    LOGICAL lter
  END TYPE meth

  TYPE metv
    SEQUENCE
    INTEGER km, kp
    REAL    dzr, ratz, rzm1
    REAL    zp, facp, f2p, zm, facm, f2m
  END TYPE metv

END MODULE SWIMinterp_fd
