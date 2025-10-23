MODULE contourlist_fd
!==============================================================================
!  Contour definiton structure
!==============================================================================

  TYPE SCIPContourElementT
    SEQUENCE
    REAL          contour 	         !Contour value
    REAL          value              !Auxiliary input data (Hazard Area = exceed vale)
    CHARACTER(16) label              !Display label
    REAL          area               !Contour Area
    REAL          population         !Population associated with contour
  END TYPE SCIPContourElementT

!==============================================================================
!  Contour list definition structure
!==============================================================================

  TYPE SCIPContourHeaderT
    SEQUENCE
    INTEGER                                             number    !Number of contours in list
    REAL                                                scale     !Scale factor (Default=1.0)
    INTEGER                                             labelMode !PLOT_ON/PLOT_OFF/PLOT_NULL
    INTEGER                                             drawMode  !PLOT_FILL/PLOT_DRAW/PLOT_BOTH/PLOT_OFF/PLOT_ON
    CHARACTER(16)                                       unit      !Units        (Default='default')
  END TYPE SCIPContourHeaderT


!==============================================================================
!  Contour list definition structure
!==============================================================================

  TYPE SCIPContourElementList
    SEQUENCE
    TYPE (SCIPContourHeaderT )                        :: listHdr   !List header data
    TYPE (SCIPContourElementT), DIMENSION(:), POINTER :: listPtr   !Pointer to list data
  END TYPE SCIPContourElementList

END MODULE contourlist_fd
