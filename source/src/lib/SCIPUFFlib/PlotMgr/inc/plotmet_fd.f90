MODULE plotmet_fd

  USE domain_fd
  USE charT_fd
  USE plotlist_fd

  INTEGER, PARAMETER :: I_SIGMAZ  =	1
  INTEGER, PARAMETER :: I_SIGMAZM	= 2
  INTEGER, PARAMETER :: I_SIGMAP	= 3
  INTEGER, PARAMETER :: I_Z3D	    = 4

!==== metCoordT ===============================================================

  TYPE metCoordT

    SEQUENCE

    INTEGER type
    INTEGER zone
    TYPE( referenceT ) :: reference
    REAL Lat0, Lon0
    REAL Lat1, Lat2
    REAL Rearth

  END TYPE metCoordT

!==== metVertCoordT ===========================================================

  TYPE metVertCoordT

    SEQUENCE

    INTEGER type
    REAL ztop
    REAL TsrfRef, lapseRef, Tiso
    REAL Ptop, P00

  END TYPE metVertCoordT

!==== metGridT ================================================================

  TYPE metGridT

    SEQUENCE

    TYPE( char64T ) name
    REAL x0, y0
    REAL dx, dy
    INTEGER nx, ny, nz
    TYPE( metCoordT ) coordHoriz
    TYPE( metVertCoordT ) coordVert

  END TYPE metGridT

!==== metGridFieldT ===========================================================

  TYPE metGridFieldT

    SEQUENCE

    INTEGER avail
    INTEGER stgX, stgY, stgZ

  END TYPE metGridFieldT

END MODULE plotmet_fd

!==============================================================================

MODULE metoutput_fd

  INTERFACE

    INTEGER FUNCTION InitMetOutput( userID,Project,n2D,n3D,num2Dfields,num3Dfields, &
                                    numGrids,numTimes,name2D,units2D,name3D,units3D, &
                                    grid,time,field2Dlist,field3Dlist )
      USE prjstruct_fd
      USE plotmet_fd
      USE charT_fd

      INTEGER,            INTENT( IN  ) :: userID
      TYPE( projectIDT ), INTENT( IN  ) :: Project
      INTEGER,            INTENT( IN  ) :: n2D, n3D
      INTEGER,                                     OPTIONAL, INTENT( OUT ) :: num2Dfields, num3Dfields
      INTEGER,                                     OPTIONAL, INTENT( OUT ) :: numGrids, numTimes
      TYPE( char64T ),       DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: name2D, units2D
      TYPE( char64T ),       DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: name3D, units3D
      TYPE( metGridT ),      DIMENSION(*), TARGET, OPTIONAL, INTENT( OUT ) :: grid
      TYPE( SCIPTimeT ),     DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: time
      TYPE( metGridFieldT ), DIMENSION(n2D,*),     OPTIONAL, INTENT( OUT ) :: field2Dlist
      TYPE( metGridFieldT ), DIMENSION(n3D,*),     OPTIONAL, INTENT( OUT ) :: field3Dlist

    END FUNCTION InitMetOutput

  END INTERFACE

END MODULE metoutput_fd

