
!------ McWIF interface module

MODULE McWIFintrf

  INTERFACE

    INTEGER FUNCTION SetMcWIF( grid,u,v,w,th,p,alpha,csq,DivScale,Method )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN  ) :: grid
      REAL, DIMENSION(:),    POINTER :: u, v, w, th, p, alpha, csq
      REAL,            INTENT( OUT ) :: DivScale
      INTEGER, OPTIONAL, INTENT( IN ) :: Method
    END FUNCTION SetMcWIF

    INTEGER FUNCTION RelaxMcWIF( grid,u,v,w,p,alpha,csq,DivScale,iMG )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN ) :: grid
      REAL, DIMENSION(:),   POINTER :: u, v, w, p, alpha, csq
      REAL,            INTENT( IN ) :: DivScale
      INTEGER,         INTENT( IN ) :: iMG
    END FUNCTION RelaxMcWIF

    INTEGER FUNCTION FFTMcWIF( grid,u,v,w,p,alpha,csq )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN ) :: grid
      REAL, DIMENSION(:),   POINTER :: u, v, w, p, alpha, csq
    END FUNCTION FFTMcWIF

    SUBROUTINE McWIFsmooth3d( grid,fldp,fld,UorV )
      USE SWIM_fi
      TYPE( MetGrid ),        POINTER      :: grid
      REAL, DIMENSION(:),     POINTER      :: fld
      REAL, DIMENSION(:),     POINTER      :: fldp
      CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
    END SUBROUTINE McWIFsmooth3d

    SUBROUTINE McWIFunSmooth3d( grid,fld,fldp,UorV )
      USE SWIM_fi
      TYPE( MetGrid ),        POINTER      :: grid
      REAL, DIMENSION(:),     POINTER      :: fld
      REAL, DIMENSION(:),     POINTER      :: fldp
      CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
    END SUBROUTINE McWIFunSmooth3d

    SUBROUTINE SetOmega( grid,u,v,w,ww,inverse )
      USE SWIMmetField_fd
      TYPE( MetGrid ),   INTENT( IN ) :: grid
      REAL, DIMENSION(:), POINTER     :: u, v, w, ww
      LOGICAL, OPTIONAL, INTENT( IN ) :: inverse
    END SUBROUTINE SetOmega

    INTEGER FUNCTION allocateMGfield( n,lTpot,MGfield ) RESULT( irv )
      USE SWIMmetField_fd
      INTEGER,           INTENT( IN    ) :: n
      LOGICAL,           INTENT( IN    ) :: lTpot
      TYPE( MetMean3D ), INTENT( INOUT ) :: MGfield
    END FUNCTION allocateMGfield

    INTEGER FUNCTION deallocateMGfield( MGfield ) RESULT( irv )
      USE SWIMmetField_fd
      TYPE( MetMean3D ), INTENT( INOUT ) :: MGfield
    END FUNCTION deallocateMGfield

  END INTERFACE

END MODULE McWIFintrf

!==============================================================================

MODULE McWIFsubIntrf

  INTERFACE

    SUBROUTINE StepRelaxP( grid,u,v,w,p,alpha,csq,divmax )
      USE SWIM_fi
      TYPE( MetGrid ),    INTENT( IN  ) :: grid
      REAL, DIMENSION(:), POINTER       :: u, v, w, p, alpha, csq
      REAL,               INTENT( OUT ) :: divmax
    END SUBROUTINE StepRelaxP

    SUBROUTINE StepU( grid,u,v,w,p,alpha )
      USE SWIM_fi
      USE SWIMparam_fd
      TYPE( MetGrid ),    INTENT( IN  ) :: grid
      REAL, DIMENSION(:),       POINTER :: u, v, w, p, alpha, csq
    END SUBROUTINE StepU

    SUBROUTINE SetBC( grid,u,v,w )
      USE SWIM_fi
      USE SWIMparam_fd
      TYPE( MetGrid ), INTENT( IN  ) :: grid
      REAL, DIMENSION(:),    POINTER :: u, v, w
    END SUBROUTINE SetBC

    SUBROUTINE SetFFTsrce( grid,u,v,w,p,psrce )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN ) :: grid
      REAL, DIMENSION(:),   POINTER :: u, v, w, p, psrce
    END SUBROUTINE SetFFTsrce

    INTEGER FUNCTION IterFFT( grid,p,pm,psrce )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN ) :: grid
      REAL, DIMENSION(:),   POINTER :: p, pm, psrce
    END FUNCTION IterFFT

    INTEGER FUNCTION ZeroNetFlux( grid,u,v )
      USE SWIM_fi
      TYPE( MetGrid ), INTENT( IN  ) :: grid
      REAL, DIMENSION(:),    POINTER :: u, v
    END FUNCTION ZeroNetFlux

  END INTERFACE

END MODULE McWIFsubIntrf

MODULE MGfield_fd

  TYPE MGfield_str
    REAL, DIMENSION(:), POINTER :: U, U0
    REAL, DIMENSION(:), POINTER :: V, V0
    REAL, DIMENSION(:), POINTER :: W, W0
    REAL, DIMENSION(:), POINTER :: P
    REAL, DIMENSION(:), POINTER :: Tpot
    REAL, DIMENSION(:), POINTER :: WW
    REAL, DIMENSION(:), POINTER :: div, dP
  END TYPE MGfield_str

END MODULE MGfield_fd

