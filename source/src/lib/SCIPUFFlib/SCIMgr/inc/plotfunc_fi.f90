!*******************************************************************************
!                PlotFunc Module
!*******************************************************************************
MODULE PlotFunc_fd

  INTEGER, PARAMETER :: PLOT_FUNCTION_NULL        =  0
  INTEGER, PARAMETER :: PLOT_FUNCTION_ADD         =  1
  INTEGER, PARAMETER :: PLOT_FUNCTION_MULT        =  2
  INTEGER, PARAMETER :: PLOT_FUNCTION_DIV         =  3
  INTEGER, PARAMETER :: PLOT_FUNCTION_INVERT      =  4
  INTEGER, PARAMETER :: PLOT_FUNCTION_SQRT        =  5
  INTEGER, PARAMETER :: PLOT_FUNCTION_CCOC        =  6
  INTEGER, PARAMETER :: PLOT_FUNCTION_PROB_CLIP   =  7
  INTEGER, PARAMETER :: PLOT_FUNCTION_PROB_LOGN   =  8
  INTEGER, PARAMETER :: PLOT_FUNCTION_CPROB_CLIP  =  9
  INTEGER, PARAMETER :: PLOT_FUNCTION_ICPROB_CLIP = 10
  INTEGER, PARAMETER :: PLOT_FUNCTION_SWITCH      = 11
  INTEGER, PARAMETER :: PLOT_FUNCTION_SPEED       = 12

END MODULE PlotFunc_fd

MODULE PlotFunc_fi

  USE PlotFunc_fd

  SAVE

  INTEGER PlotFunc_nComp
  INTEGER PlotFunc_nfld
  INTEGER PlotFunc_nfun
  REAL    PlotFunc_data
  REAL    PlotFunc_small
  REAL    PlotFunc_spv

END MODULE PlotFunc_fi

MODULE PlotAux_fi

  USE sagstr_fd

  SAVE

  INTEGER AuxType
  TYPE( SAGfield_aux ), POINTER :: AuxData

END MODULE PlotAux_fi

MODULE ArrivalTimeFunc_fi

  INTEGER iDummy

END MODULE ArrivalTimeFunc_fi
