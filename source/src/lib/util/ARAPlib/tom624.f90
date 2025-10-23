!     ALGORITHM 624 COLLECTED ALGORITHMS FROM ACM.
!     ALGORITHM APPEARED IN ACM-TRANS. MATH. SOFTWARE, VOL.10, NO. 4,
!     DEC., 1984, P. 453.
!==============================================================================
! SUBROUTINE ADNODE
!==============================================================================
      SUBROUTINE ADNODE (KK,X,Y, IADJ,IEND, IER)
      INTEGER KK, IADJ(1), IEND(KK), IER
      REAL    X(KK), Y(KK)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE ADDS NODE KK TO A TRIANGULATION OF A SET
! OF POINTS IN THE PLANE PRODUCING A NEW TRIANGULATION.  A
! SEQUENCE OF EDGE SWAPS IS THEN APPLIED TO THE MESH,
! RESULTING IN AN OPTIMAL TRIANGULATION.  ADNODE IS PART
! OF AN INTERPOLATION PACKAGE WHICH ALSO PROVIDES ROUTINES
! TO INITIALIZE THE DATA STRUCTURE, PLOT THE MESH, AND
! DELETE ARCS.
!
! INPUT PARAMETERS -   KK - INDEX OF THE NODE TO BE ADDED
!                           TO THE MESH.  KK .GE. 4.
!
!                     X,Y - VECTORS OF COORDINATES OF THE
!                           NODES IN THE MESH.  (X(I),Y(I))
!                           DEFINES NODE I FOR I = 1,..,KK.
!
!                    IADJ - SET OF ADJACENCY LISTS OF NODES
!                           1,..,KK-1.
!
!                    IEND - POINTERS TO THE ENDS OF
!                           ADJACENCY LISTS IN IADJ FOR
!                           EACH NODE IN THE MESH.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH.
!
! KK, X, AND Y ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
!                                 OF NODE KK AS THE LAST
!                                 ENTRY.
!
!                           IER - ERROR INDICATOR
!                                 IER = 0 IF NO ERRORS
!                                         WERE ENCOUNTERED.
!                                 IER = 1 IF ALL NODES
!                                         (INCLUDING KK) ARE
!                                         COLLINEAR.
!
! MODULES REFERENCED BY ADNODE - TRFIND, INTADD, BDYADD,
!                                SHIFTD, INDEX, SWPTST,
!                                SWAP
!
!***********************************************************
!
      INTEGER K, KM1, I1, I2, I3, INDKF, INDKL, NABOR1, &
              IO1, IO2, IN1, INDK1, IND2F, IND21
      REAL    XK, YK
      LOGICAL, EXTERNAL :: SWPTST
      INTEGER, EXTERNAL :: TINDEX
!
! LOCAL PARAMETERS -
!
! K =        LOCAL COPY OF KK
! KM1 =      K - 1
! I1,I2,I3 = VERTICES OF A TRIANGLE CONTAINING K
! INDKF =    IADJ INDEX OF THE FIRST NEIGHBOR OF K
! INDKL =    IADJ INDEX OF THE LAST NEIGHBOR OF K
! NABOR1 =   FIRST NEIGHBOR OF K BEFORE ANY SWAPS OCCUR
! IO1,IO2 =  ADJACENT NEIGHBORS OF K DEFINING AN ARC TO
!              BE TESTED FOR A SWAP
! IN1 =      VERTEX OPPOSITE K -- FIRST NEIGHBOR OF IO2
!              WHICH PRECEDES IO1.  IN1,IO1,IO2 ARE IN
!              COUNTERCLOCKWISE ORDER.
! INDK1 =    INDEX OF IO1 IN THE ADJACENCY LIST FOR K
! IND2F =    INDEX OF THE FIRST NEIGHBOR OF IO2
! IND21 =    INDEX OF IO1 IN THE ADJACENCY LIST FOR IO2
! XK,YK =    X(K), Y(K)
!
      IER = 0
      K = KK
!
! INITIALIZATION
!
      KM1 = K - 1
      XK = X(K)
      YK = Y(K)
!
! ADD NODE K TO THE MESH
!
      CALL TRFIND(KM1,XK,YK,X,Y,IADJ,IEND, I1,I2,I3)
      IF (I1 .EQ. 0) GO TO 5
      IF (I3 .EQ. 0) CALL BDYADD(K,I1,I2, IADJ,IEND )
      IF (I3 .NE. 0) CALL INTADD(K,I1,I2,I3, IADJ,IEND )
!
! INITIALIZE VARIABLES FOR OPTIMIZATION OF THE MESH
!
      INDKF = IEND(KM1) + 1
      INDKL = IEND(K)
      NABOR1 = IADJ(INDKF)
      IO2 = NABOR1
      INDK1 = INDKF + 1
      IO1 = IADJ(INDK1)
!
! BEGIN LOOP -- FIND THE VERTEX OPPOSITE K
!
    1 IND2F = 1
      IF (IO2 .NE. 1) IND2F = IEND(IO2-1) + 1
      IND21 = TINDEX(IO2,IO1,IADJ,IEND)
      IF (IND2F .EQ. IND21) GO TO 2
      IN1 = IADJ(IND21-1)
      GO TO 3
!
! IN1 IS THE LAST NEIGHBOR OF IO2
!
    2 IND21 = IEND(IO2)
      IN1 = IADJ(IND21)
      IF (IN1 .EQ. 0) GO TO 4
!
! SWAP TEST -- IF A SWAP OCCURS, TWO NEW ARCS ARE OPPOSITE K
!              AND MUST BE TESTED.  INDK1 AND INDKF MUST BE
!              DECREMENTED.
!
    3 IF ( .NOT. SWPTST(IN1,K,IO1,IO2,X,Y) ) GO TO 4
      CALL SWAPP(IN1,K,IO1,IO2, IADJ,IEND )
      IO1 = IN1
      INDK1 = INDK1 - 1
      INDKF = INDKF - 1
      GO TO 1
!
! NO SWAP OCCURRED.  RESET IO2 AND IO1, AND TEST FOR
!   TERMINATION.
!
    4 IF (IO1 .EQ. NABOR1) RETURN
      IO2 = IO1
      INDK1 = INDK1 + 1
      IF (INDK1 .GT. INDKL) INDK1 = INDKF
      IO1 = IADJ(INDK1)
      IF (IO1 .NE. 0) GO TO 1
      RETURN
!
! ALL NODES ARE COLLINEAR
!
    5 IER = 1
      RETURN
      END
!==============================================================================
! FUNCTION AREA
!==============================================================================
      REAL FUNCTION AREA (X,Y,NB,NODES)
      INTEGER NB, NODES(NB)
      REAL    X(1), Y(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A SEQUENCE OF NB POINTS IN THE PLANE, THIS
! FUNCTION COMPUTES THE AREA BOUNDED BY THE CLOSED POLY-
! GONAL CURVE WHICH PASSES THROUGH THE POINTS IN THE
! SPECIFIED ORDER.  EACH SIMPLE CLOSED CURVE IS POSITIVELY
! ORIENTED (BOUNDS POSITIVE AREA) IF AND ONLY IF THE POINTS
! ARE SPECIFIED IN COUNTERCLOCKWISE ORDER.  THE LAST POINT
! OF THE CURVE IS TAKEN TO BE THE FIRST POINT SPECIFIED, AND
! THUS THIS POINT NEED NOT BE SPECIFIED TWICE.  HOWEVER, ANY
! POINT MAY BE SPECIFIED MORE THAN ONCE IN ORDER TO DEFINE A
! MULTIPLY CONNECTED DOMAIN.
!   THE AREA OF A TRIANGULATION MAY BE COMPUTED BY CALLING
! AREA WITH VALUES OF NB AND NODES DETERMINED BY SUBROUTINE
! BNODES.
!
! INPUT PARAMETERS -   X,Y - N-VECTORS OF COORDINATES OF
!                            POINTS IN THE PLANE FOR N .GE.
!                            NB.  NODE I HAS COORDINATES
!                            (X(I),Y(I)) FOR I = 1, 2, ...,
!                            N.
!
!                       NB - LENGTH OF NODES.
!
!                    NODES - VECTOR OF NODE INDICES IN THE
!                            RANGE 1 TO N DEFINING THE
!                            POLYGONAL CURVE.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER -  AREA - SIGNED AREA BOUNDED BY THE
!                            POLYGONAL CURVE DEFINED
!                            ABOVE.
!
! MODULES REFERENCED BY AREA - NONE
!
!***********************************************************
!
      INTEGER NNB, ND, I
      REAL    A, X0, Y0, DX1, DY1, DX2, DY2
!
! LOCAL PARAMETERS -
!
! NNB =     LOCAL COPY OF NB
! ND =      ELEMENT OF NODES
! I =       DO-LOOP AND NODES INDEX
! A =       PARTIAL SUM OF SIGNED (AND DOUBLED) TRIANGLE
!             AREAS
! X0,Y0 =   X(NODES(1)), Y(NODES(1))
! DX1,DY1 = COMPONENTS OF THE VECTOR NODES(1)-NODES(I) FOR
!             I = 2, 3, ..., NB-1
! DX2,DY2 = COMPONENTS OF THE VECTOR NODES(1)-NODES(I) FOR
!             I = 3, 4, ..., NB
!
      NNB = NB
      A = 0.
      IF (NNB .LT. 3) GO TO 2
!
! INITIALIZATION
!
      ND = NODES(1)
      X0 = X(ND)
      Y0 = Y(ND)
      ND = NODES(2)
      DX1 = X(ND) - X0
      DY1 = Y(ND) - Y0
!
! LOOP ON TRIANGLES (NODES(1),NODES(I),NODES(I+1)),
!   I = 2, 3, ..., NB-1, ADDING TWICE THEIR SIGNED
!   AREAS TO A
!
      DO 1 I = 3,NNB
        ND = NODES(I)
        DX2 = X(ND) - X0
        DY2 = Y(ND) - Y0
        A = A + DX1*DY2 - DX2*DY1
        DX1 = DX2
        DY1 = DY2
    1   CONTINUE
!
! A CONTAINS TWICE THE SIGNED AREA OF THE REGION
!
    2 AREA = A/2.
      RETURN
      END
!==============================================================================
! SUBROUTINE BDYADD
!==============================================================================
      SUBROUTINE BDYADD (KK,I1,I2, IADJ,IEND )
      INTEGER KK, I1, I2, IADJ(1), IEND(KK)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE ADDS A BOUNDARY NODE TO A TRIANGULATION
! OF A SET OF KK-1 POINTS IN THE PLANE.  IADJ AND IEND ARE
! UPDATED WITH THE INSERTION OF NODE KK.
!
! INPUT PARAMETERS -   KK - INDEX OF AN EXTERIOR NODE TO BE
!                           ADDED.  KK .GE. 4.
!
!                      I1 - FIRST (RIGHTMOST AS VIEWED FROM
!                           KK) BOUNDARY NODE IN THE MESH
!                           WHICH IS VISIBLE FROM KK - THE
!                           LINE SEGMENT KK-I1 INTERSECTS
!                           NO ARCS.
!
!                      I2 - LAST (LEFTMOST) BOUNDARY NODE
!                           WHICH IS VISIBLE FROM KK.
!
!                    IADJ - SET OF ADJACENCY LISTS OF NODES
!                           IN THE MESH.
!
!                    IEND - POINTERS TO THE ENDS OF
!                           ADJACENCY LISTS IN IADJ FOR
!                           EACH NODE IN THE MESH.
!
!   IADJ AND IEND MAY BE CREATED BY TRMESH AND MUST CONTAIN
! THE VERTICES I1 AND I2.  I1 AND I2 MAY BE DETERMINED BY
! TRFIND.
!
! KK, I1, AND I2 ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
!                                 OF NODE KK AS THE LAST
!                                 ENTRY.  NODE KK WILL BE
!                                 CONNECTED TO I1, I2, AND
!                                 ALL BOUNDARY NODES BETWEEN
!                                 THEM.  NO OPTIMIZATION OF
!                                 THE MESH IS PERFORMED.
!
! MODULE REFERENCED BY BDYADD - SHIFTD
!
! INTRINSIC FUNCTIONS CALLED BY BDYADD - MIN0, MAX0
!
!***********************************************************
!
      INTEGER K, KM1, NRIGHT, NLEFT, NF, NL, N1, N2, I, &
              IMIN, IMAX, KEND, NEXT, INDX
!
! LOCAL PARAMETERS -
!
! K =            LOCAL COPY OF KK
! KM1 =          K - 1
! NRIGHT,NLEFT = LOCAL COPIES OF I1, I2
! NF,NL =        INDICES OF IADJ BOUNDING THE PORTION OF THE
!                  ARRAY TO BE SHIFTED
! N1 =           IADJ INDEX OF THE FIRST NEIGHBOR OF NLEFT
! N2 =           IADJ INDEX OF THE LAST NEIGHBOR OF NRIGHT
! I =            DO-LOOP INDEX
! IMIN,IMAX =    BOUNDS ON DO-LOOP INDEX -- FIRST AND LAST
!                  ELEMENTS OF IEND TO BE INCREMENTED
! KEND =         POINTER TO THE LAST NEIGHBOR OF K IN IADJ
! NEXT =         NEXT BOUNDARY NODE TO BE CONNECTED TO KK
! INDX =         INDEX FOR IADJ
!
      K = KK
      KM1 = K - 1
      NRIGHT = I1
      NLEFT = I2
!
! INITIALIZE VARIABLES
!
      NL = IEND(KM1)
      N1 = 1
      IF (NLEFT .NE. 1) N1 = IEND(NLEFT-1) + 1
      N2 = IEND(NRIGHT)
      NF = MAX0(N1,N2)
!
! INSERT K AS A NEIGHBOR OF MAX(NRIGHT,NLEFT)
!
      CALL SHIFTD(NF,NL,2, IADJ )
      IADJ(NF+1) = K
      IMIN = MAX0(NRIGHT,NLEFT)
      DO 1 I = IMIN,KM1
        IEND(I) = IEND(I) + 2
    1   CONTINUE
!
! INITIALIZE KEND AND INSERT K AS A NEIGHBOR OF
!   MIN(NRIGHT,NLEFT)
!
      KEND = NL + 3
      NL = NF - 1
      NF = MIN0(N1,N2)
      CALL SHIFTD(NF,NL,1, IADJ )
      IADJ(NF) = K
      IMAX = IMIN - 1
      IMIN = MIN0(NRIGHT,NLEFT)
      DO 2 I = IMIN,IMAX
        IEND(I) = IEND(I) + 1
    2   CONTINUE
!
! INSERT NRIGHT AS THE FIRST NEIGHBOR OF K
!
      IADJ(KEND) = NRIGHT
!
! INITIALIZE INDX FOR LOOP ON BOUNDARY NODES BETWEEN NRIGHT
!   AND NLEFT
!
      INDX = IEND(NRIGHT) - 2
    3 NEXT = IADJ(INDX)
      IF (NEXT .EQ. NLEFT) GO TO 4
!
! CONNECT NEXT AND K
!
      KEND = KEND + 1
      IADJ(KEND) = NEXT
      INDX = IEND(NEXT)
      IADJ(INDX) = K
      INDX = INDX - 1
      GO TO 3
!
! INSERT NLEFT AND 0 AS THE LAST NEIGHBORS OF K
!
    4 IADJ(KEND+1) = NLEFT
      KEND = KEND + 2
      IADJ(KEND) = 0
      IEND(K) = KEND
      RETURN
      END
!==============================================================================
! SUBROUTINE BNODES
!==============================================================================
      SUBROUTINE BNODES (N,IADJ,IEND, NB,NA,NT,NODES)
      INTEGER N, IADJ(1), IEND(N), NB, NA, NT, NODES(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF N POINTS IN THE PLANE, THIS
! ROUTINE RETURNS A VECTOR CONTAINING THE INDICES, IN
! COUNTERCLOCKWISE ORDER, OF THE NODES ON THE BOUNDARY OF
! THE CONVEX HULL OF THE SET OF POINTS.
!
! INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
!
!                     IADJ - SET OF ADJACENCY LISTS OF
!                            NODES IN THE MESH.
!
!                     IEND - POINTERS TO THE ENDS OF
!                            ADJACENCY LISTS IN IADJ FOR
!                            EACH NODE IN THE MESH.
!
!                    NODES - VECTOR OF LENGTH .GE. NB.
!                            (NB .LE. N).
!
!   IADJ AND IEND MAY BE CREATED BY TRMESH AND ARE NOT
! ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS -   NB - NUMBER OF BOUNDARY NODES.
!
!                    NA,NT - NUMBER OF ARCS AND TRIANGLES,
!                            RESPECTIVELY, IN THE MESH.
!
!                    NODES - VECTOR OF NB BOUNDARY NODE
!                            INDICES RANGING FROM 1 TO N.
!
! MODULES REFERENCED BY BNODES - NONE
!
!***********************************************************
!
      INTEGER NST, INDL, K, N0, INDF
!
! LOCAL PARAMETERS -
!
! NST =  FIRST ELEMENT OF NODES -- ARBITRARILY CHOSEN
! INDL = IADJ INDEX OF THE LAST NEIGHBOR OF NST
! K =    NODES INDEX
! N0 =   BOUNDARY NODE TO BE ADDED TO NODES
! INDF = IADJ INDEX OF THE FIRST NEIGHBOR OF N0
!
! SET NST TO THE FIRST BOUNDARY NODE ENCOUNTERED
!
      NST = 1
    1 INDL = IEND(NST)
      IF (IADJ(INDL) .EQ. 0) GO TO 2
      NST = NST + 1
      GO TO 1
!
! INITIALIZATION
!
    2 NODES(1) = NST
      K = 1
      N0 = NST
!
! TRAVERSE THE BOUNDARY IN COUNTERCLOCKWISE ORDER
!
    3 INDF = 1
      IF (N0 .GT. 1) INDF = IEND(N0-1) + 1
      N0 = IADJ(INDF)
      IF (N0 .EQ. NST) GO TO 4
      K = K + 1
      NODES(K) = N0
      GO TO 3
!
! TERMINATION
!
    4 NB = K
      NT = 2*N - NB - 2
      NA = NT + N - 1
      RETURN
      END
!==============================================================================
! SUBROUTINE DELETE
!==============================================================================
      SUBROUTINE DELETE (NN,NOUT1,NOUT2, IADJ,IEND, IER)
      INTEGER NN, NOUT1, NOUT2, IADJ(1), IEND(NN), IER
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE DELETES A BOUNDARY EDGE FROM A TRIANGU-
! LATION OF A SET OF POINTS IN THE PLANE.  IT MAY BE NEC-
! ESSARY TO FORCE CERTAIN EDGES TO BE PRESENT BEFORE CALL-
! ING DELETE (SEE SUBROUTINE EDGE).  NOTE THAT SUBROUTINES
! EDGE, TRFIND, AND THE ROUTINES WHICH CALL TRFIND (ADNODE,
! UNIF, INTRC1, AND INTRC0) SHOULD NOT BE CALLED FOLLOWING
! A DELETION.
!
! INPUT PARAMETERS -    NN - NUMBER OF NODES IN THE TRIAN-
!                            GULATION.
!
!              NOUT1,NOUT2 - PAIR OF ADJACENT NODES ON THE
!                            BOUNDARY DEFINING THE ARC TO
!                            BE REMOVED.  NOUT2 MUST BE THE
!                            LAST NONZERO NEIGHBOR OF NOUT1.
!
! THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
!                IADJ,IEND - DATA STRUCTURE DEFINING THE
!                            TRIANGULATION (SEE SUBROUTINE
!                            TRMESH).
!
! OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE REMOVAL
!                                 OF THE ARC NOUT1-NOUT2
!                                 IF IER .EQ. 0.
!
!                           IER - ERROR INDICATOR
!                                 IER = 0 IF NO ERRORS WERE
!                                         ENCOUNTERED.
!                                 IER = 1 IF NOUT1 OR NOUT2
!                                         IS NOT ON THE
!                                         BOUNDARY.
!                                 IER = 2 IF NOUT1 OR NOUT2
!                                         HAS ONLY 2 NONZERO
!                                         NEIGHBORS.
!                                 IER = 3 IF NOUT2 IS NOT
!                                         THE LAST NEIGHBOR
!                                         OF NOUT1.
!                                 IER = 4 IF A DELETION
!                                         WOULD DIVIDE THE
!                                         MESH INTO TWO
!                                         REGIONS.
!
! MODULES REFERENCED BY DELETE - SHIFTD, INDEX
!
!***********************************************************
!
      INTEGER N, IOUT1, IOUT2, IO1, IO2, IND12, IND21,     &
              ITEMP, IND1F, IND1L, IND2F, IND2L, NEWBD,    &
              INDNF, INDNL, INDN0, INDFP2, INDLM3, NF, NL, &
              I, IMAX
      INTEGER, EXTERNAL :: TINDEX
!
! LOCAL PARAMETERS -
!
! N =           LOCAL COPY OF NN
! IOUT1,IOUT2 = LOCAL COPIES OF NOUT1 AND NOUT2
! IO1,IO2 =     NOUT1,NOUT2 IN ORDER OF INCREASING MAGNITUDE
! IND12 =       INDEX OF IO2 IN THE ADJACENCY LIST FOR IO1
! IND21 =       INDEX OF IO1 IN THE ADJACENCY LIST FOR IO2
! ITEMP =       TEMPORARY STORAGE LOCATION FOR PERMUTATIONS
! IND1F =       IADJ INDEX OF THE FIRST NEIGHBOR OF IO1
! IND1L =       IADJ INDEX OF THE LAST NEIGHBOR OF IO1
! IND2F =       IADJ INDEX OF THE FIRST NEIGHBOR OF IO2
! IND2L =       IADJ INDEX OF THE LAST NEIGHBOR OF IO2
! NEWBD =       THE NEIGHBOR COMMON TO NOUT1 AND NOUT2
! INDNF =       IADJ INDEX OF THE FIRST NEIGHBOR OF NEWBD
! INDNL =       IADJ INDEX OF THE LAST NEIGHBOR OF NEWBD
! INDN0 =       INDEX OF 0 IN THE ADJACENCY LIST FOR NEWBD
!                 BEFORE PERMUTING THE NEIGHBORS
! INDFP2 =      INDNF + 2
! INDLM3 =      INDNL - 3
! NF,NL =       BOUNDS ON THE PORTION OF IADJ TO BE SHIFTED
! I =           DO-LOOP INDEX
! IMAX =        UPPER BOUND ON DO-LOOP FOR SHIFTING IEND
!
      N = NN
      IOUT1 = NOUT1
      IOUT2 = NOUT2
!
! INITIALIZE INDICES
!
      IND1F = 1
      IF (IOUT1 .GT. 1) IND1F = IEND(IOUT1-1) + 1
      IND1L = IEND(IOUT1)
      IND2F = 1
      IF (IOUT2 .GT. 1) IND2F = IEND(IOUT2-1) + 1
      IND2L = IEND(IOUT2)
      NEWBD = IADJ(IND1L-2)
      INDN0 = TINDEX(NEWBD,IOUT2,IADJ,IEND)
      INDNL = IEND(NEWBD)
!
! ORDER VERTICES SUCH THAT THE NEIGHBORS OF IO1 PRECEDE
!   THOSE OF IO2
!
      IF (IOUT1 .GT. IOUT2) GO TO 1
      IO1 = IOUT1
      IO2 = IOUT2
      IND12 = IND1L - 1
      IND21 = IND2F
      GO TO 2
    1 IO1 = IOUT2
      IO2 = IOUT1
      IND12 = IND2F
      IND21 = IND1L - 1
!
! CHECK FOR ERRORS
!
    2 IF ( (IADJ(IND1L) .NE. 0) .OR. (IADJ(IND2L) .NE. 0) ) GO TO 21
      IF ( (IND1L-IND1F .LE. 2) .OR. (IND2L-IND2F .LE. 2) ) GO TO 22
      IF (IADJ(IND1L-1) .NE. IOUT2) GO TO 23
      IF (IADJ(INDNL) .EQ. 0) GO TO 24
!
! DELETE THE EDGE IO1-IO2 AND MAKE NEWBD A BOUNDARY NODE
!
      IF (NEWBD .LT. IO1) GO TO 8
      IF (NEWBD .LT. IO2) GO TO 6
!
! THE VERTICES ARE ORDERED IO1, IO2, NEWBD.
! DELETE IO2 AS A NEIGHBOR OF IO1.
!
      NF = IND12 + 1
      NL = IND21 - 1
      CALL SHIFTD(NF,NL,-1, IADJ )
      IMAX = IO2 - 1
      DO 3 I = IO1,IMAX
        IEND(I) = IEND(I) - 1
    3   CONTINUE
!
! DELETE IO1 AS A NEIGHBOR OF IO2
!
      NF = NL + 2
      NL = INDN0
      CALL SHIFTD(NF,NL,-2, IADJ )
      IMAX = NEWBD - 1
      DO 4 I = IO2,IMAX
        IEND(I) = IEND(I) - 2
    4   CONTINUE
!
! SHIFT THE BOTTOM OF IADJ UP 1 LEAVING ROOM FOR 0 AS A
!   NEIGHBOR OF NEWBD
!
      INDN0 = INDN0 - 1
      NF = NL + 1
      NL = IEND(N)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,-1, IADJ )
      DO 5 I = NEWBD,N
        IEND(I) = IEND(I) - 1
    5   CONTINUE
      GO TO 12
!
! THE VERTICES ARE ORDERED IO1, NEWBD, IO2.
! DELETE IO2 AS A NEIGHBOR OF IO1 LEAVING ROOM FOR 0 AS A
!   NEIGHBOR OF NEWBD.
!
    6 NF = IND12 + 1
      NL = INDN0
      CALL SHIFTD(NF,NL,-1, IADJ )
      IMAX = NEWBD - 1
      DO 7 I = IO1,IMAX
        IEND(I) = IEND(I) - 1
    7   CONTINUE
      GO TO 10
!
! THE VERTICES ARE ORDERED NEWBD, IO1, IO2.
! DELETE IO2 AS A NEIGHBOR OF IO1 LEAVING ROOM FOR 0 AS A
!   NEIGHBOR OF NEWBD.
!
    8 INDN0 = INDN0 + 1
      NF = INDN0
      NL = IND12 - 1
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,1, IADJ )
      IMAX = IO1 - 1
      DO 9 I = NEWBD,IMAX
        IEND(I) = IEND(I) + 1
    9   CONTINUE
!
! DELETE IO1 AS A NEIGHBOR OF IO2
!
   10 NF = IND21 + 1
      NL = IEND(N)
      CALL SHIFTD(NF,NL,-1, IADJ )
      DO 11 I = IO2,N
        IEND(I) = IEND(I) - 1
   11   CONTINUE
!
! PERMUTE THE NEIGHBORS OF NEWBD WITH END-AROUND SHIFTS SO
!   THAT 0 IS THE LAST NEIGHBOR
!
   12 INDNF = 1
      IF (NEWBD .GT. 1) INDNF = IEND(NEWBD-1) + 1
      INDNL = IEND(NEWBD)
      IF (INDN0-INDNF .GE. INDNL-INDN0) GO TO 16
!
! SHIFT UPWARD
!
      IF (INDN0 .GT. INDNF) GO TO 13
      CALL SHIFTD(INDNF+1,INDNL,-1, IADJ )
      GO TO 20
   13 INDFP2 = INDNF + 2
      IF (INDN0 .LT. INDFP2) GO TO 15
      DO 14 I = INDFP2,INDN0
        ITEMP = IADJ(INDNF)
        CALL SHIFTD(INDNF+1,INDNL,-1, IADJ )
        IADJ(INDNL) = ITEMP
   14   CONTINUE
!
! THE LAST SHIFT IS BY 2
!
   15 ITEMP = IADJ(INDNF)
      CALL SHIFTD(INDFP2,INDNL,-2, IADJ )
      IADJ(INDNL-1) = ITEMP
      GO TO 20
!
! SHIFT DOWNWARD
!
   16 IF (INDN0 .EQ. INDNL) GO TO 20
      IF (INDN0 .LT. INDNL-1) GO TO 17
      CALL SHIFTD(INDNF,INDNL-2,1, IADJ )
      IADJ(INDNF) = IADJ(INDNL)
      GO TO 20
   17 INDLM3 = INDNL - 3
      IF (INDN0 .GT. INDLM3) GO TO 19
      DO 18 I = INDN0,INDLM3
        ITEMP = IADJ(INDNL)
        CALL SHIFTD(INDNF,INDNL-1,1, IADJ )
        IADJ(INDNF) = ITEMP
   18   CONTINUE
!
! THE LAST SHIFT IS BY 2
!
   19 ITEMP = IADJ(INDNL-1)
      CALL SHIFTD(INDNF,INDLM3,2, IADJ )
      IADJ(INDNF+1) = IADJ(INDNL)
      IADJ(INDNF) = ITEMP
!
! INSERT 0 AS THE LAST NEIGHBOR OF NEWBD
!
   20 IADJ(INDNL) = 0
      IER = 0
      RETURN
!
! ONE OF THE VERTICES IS NOT ON THE BOUNDARY
!
   21 IER = 1
      RETURN
!
! ONE OF THE VERTICES HAS ONLY TWO NONZERO NEIGHBORS.  THE
!   TRIANGULATION WOULD BE DESTROYED BY A DELETION
!
   22 IER = 2
      RETURN
!
! NOUT2 IS NOT THE LAST NONZERO NEIGHBOR OF NOUT1
!
   23 IER = 3
      RETURN
!
! A DELETION WOULD DIVIDE THE MESH INTO TWO REGIONS
!   CONNECTED AT A SINGLE NODE
!
   24 IER = 4
      RETURN
      END
!==============================================================================
! SUBROUTINE EDGE
!==============================================================================
      SUBROUTINE EDGE (IN1,IN2,X,Y, LWK,IWK,IADJ,IEND, IER)
      INTEGER IN1, IN2, LWK, IWK(2,1), IADJ(1), IEND(1), IER
      REAL    X(1), Y(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF N NODES AND A PAIR OF NODAL
! INDICES IN1 AND IN2, THIS ROUTINE SWAPS ARCS AS NECESSARY
! TO FORCE IN1 AND IN2 TO BE ADJACENT.  ONLY ARCS WHICH
! INTERSECT IN1-IN2 ARE SWAPPED OUT.  IF A THIESSEN TRIANGU-
! LATION IS INPUT, THE RESULTING TRIANGULATION IS AS CLOSE
! AS POSSIBLE TO A THIESSEN TRIANGULATION IN THE SENSE THAT
! ALL ARCS OTHER THAN IN1-IN2 ARE LOCALLY OPTIMAL.
!   A SEQUENCE OF CALLS TO EDGE MAY BE USED TO FORCE THE
! PRESENCE OF A SET OF EDGES DEFINING THE BOUNDARY OF A NON-
! CONVEX REGION.  SUBSEQUENT DELETION OF EDGES OUTSIDE THIS
! REGION (BY SUBROUTINE DELETE) RESULTS IN A NONCONVEX TRI-
! ANGULATION WHICH MAY SERVE AS A FINITE ELEMENT GRID.
! (EDGE SHOULD NOT BE CALLED AFTER A CALL TO DELETE.)  IF,
! ON THE OTHER HAND, INTERPOLATION IS TO BE PERFORMED IN THE
! NONCONVEX REGION, EDGES MUST NOT BE DELETED, BUT IT IS
! STILL ADVANTAGEOUS TO HAVE THE NONCONVEX BOUNDARY PRESENT
! IF IT IS DESIRABLE THAT INTERPOLATED VALUES BE INFLUENCED
! BY THE GEOMETRY.  NOTE THAT SUBROUTINE GETNP WHICH IS USED
! TO SELECT THE NODES ENTERING INTO LOCAL DERIVATIVE ESTI-
! MATES WILL NOT NECESSARILY RETURN CLOSEST NODES IF THE
! TRIANGULATION HAS BEEN RENDERED NONOPTIMAL BY A CALL TO
! EDGE.  HOWEVER, THE EFFECT WILL BE MERELY TO FURTHER EN-
! HANCE THE INFLUENCE OF THE NONCONVEX GEOMETRY ON INTERPO-
! LATED VALUES.
!
! INPUT PARAMETERS - IN1,IN2 - INDICES (OF X AND Y) IN THE
!                              RANGE 1,...,N DEFINING A PAIR
!                              OF NODES TO BE CONNECTED BY
!                              AN ARC.
!
!                        X,Y - N-VECTORS CONTAINING CARTE-
!                              SIAN COORDINATES OF THE
!                              NODES.
!
! THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
!                        LWK - NUMBER OF COLUMNS RESERVED
!                              FOR IWK.  THIS MUST BE AT
!                              LEAST NI -- THE NUMBER OF
!                              ARCS WHICH INTERSECT IN1-IN2.
!                              (NI IS BOUNDED BY N-3).
!
!                        IWK - INTEGER WORK ARRAY DIMENSION-
!                              ED 2 BY LWK (OR VECTOR OF
!                              LENGTH .GE. 2*LWK).
!
!                  IADJ,IEND - DATA STRUCTURE DEFINING THE
!                              TRIANGULATION.  SEE SUBROU-
!                              TINE TRMESH.
!
! OUTPUT PARAMETERS - LWK - NUMBER OF IWK COLUMNS REQUIRED
!                           IF IER = 0 OR IER = 2.  LWK = 0
!                           IFF IN1 AND IN2 WERE ADJACENT
!                           ON INPUT.
!
!                     IWK - CONTAINS THE INDICES OF THE END-
!                           POINTS OF THE NEW ARCS OTHER
!                           THAN IN1-IN2 UNLESS IER .GT. 0
!                           OR LWK = 0.  NEW ARCS TO THE
!                           LEFT OF IN1->IN2 ARE STORED IN
!                           THE FIRST K-1 COLUMNS (LEFT POR-
!                           TION OF IWK), COLUMN K CONTAINS
!                           ZEROS, AND NEW ARCS TO THE RIGHT
!                           OF IN1->IN2 OCCUPY COLUMNS K+1,
!                           ...,LWK.  (K CAN BE DETERMINED
!                           BY SEARCHING IWK FOR THE ZEROS.)
!
!               IADJ,IEND - UPDATED IF NECESSARY TO REFLECT
!                           THE PRESENCE OF AN ARC CONNECT-
!                           ING IN1 AND IN2, UNALTERED IF
!                           IER .NE. 0.
!
!                     IER - ERROR INDICATOR
!                           IER = 0 IF NO ERRORS WERE EN-
!                                   COUNTERED.
!                           IER = 1 IF IN1 .LT. 1, IN2 .LT.
!                                   1, IN1 = IN2, OR LWK
!                                   .LT. 0 ON INPUT.
!                           IER = 2 IF MORE SPACE IS REQUIR-
!                                   ED IN IWK.  SEE LWK.
!                           IER = 3 IF IN1 AND IN2 COULD NOT
!                                   BE CONNECTED DUE TO AN
!                                   INVALID DATA STRUCTURE.
!
! MODULES REFERENCED BY EDGE - SWAP, INDEX, SHIFTD, SWPTST
!
!***********************************************************
!
      INTEGER N1, N2, IWEND, IWL, INDF, INDX, N1LST, NL, NR, &
              NEXT, IWF, LFT, N0, IWC, IWCP1, IWCM1, I, IO1, &
              IO2, INDL
      REAL    X1, Y1, X2, Y2, X0, Y0
      LOGICAL SWP, LEFT
      LOGICAL, EXTERNAL :: SWPTST
!
! LOCAL PARAMETERS -
!
! N1,N2 =   LOCAL COPIES OF IN1 AND IN2 OR NODES OPPOSITE AN
!             ARC IO1-IO2 TO BE TESTED FOR A SWAP IN THE
!             OPTIMIZATION LOOPS
! IWEND =   INPUT OR OUTPUT VALUE OF LWK
! IWL =     IWK (COLUMN) INDEX OF THE LAST (RIGHTMOST) ARC
!             WHICH INTERSECTS IN1->IN2
! INDF =    IADJ INDEX OF THE FIRST NEIGHBOR OF IN1 OR IO1
! INDX =    IADJ INDEX OF A NEIGHBOR OF IN1, NL, OR IO1
! N1LST =   LAST NEIGHBOR OF IN1
! NL,NR =   ENDPOINTS OF AN ARC WHICH INTERSECTS IN1-IN2
!             WITH NL LEFT IN1->IN2
! NEXT =    NODE OPPOSITE NL->NR
! IWF =     IWK (COLUMN) INDEX OF THE FIRST (LEFTMOST) ARC
!             WHICH INTERSECTS IN1->IN2
! LFT =     FLAG USED TO DETERMINE IF A SWAP RESULTS IN THE
!             NEW ARC INTERSECTING IN1-IN2 -- LFT = 0 IFF
!             N0 = IN1, LFT = -1 IMPLIES N0 LEFT IN1->IN2,
!             AND LFT = 1 IMPLIES N0 LEFT IN2->IN1
! N0 =      NODE OPPOSITE NR->NL
! IWC =     IWK INDEX BETWEEN IWF AND IWL -- NL->NR IS
!             STORED IN IWK(1,IWC)->IWK(2,IWC)
! IWCP1 =   IWC + 1
! IWCM1 =   IWC - 1
! I =       DO-LOOP INDEX AND COLUMN INDEX FOR IWK
! IO1,IO2 = ENDPOINTS OF AN ARC TO BE TESTED FOR A SWAP IN
!             THE OPTIMIZATION LOOPS
! INDL =    IADJ INDEX OF THE LAST NEIGHBOR OF IO1
! X1,Y1 =   COORDINATES OF IN1
! X2,Y2 =   COORDINATES OF IN2
! X0,Y0 =   COORDINATES OF N0
! SWP =     FLAG SET TO .TRUE. IFF A SWAP OCCURS IN AN OPTI-
!             MIZATION LOOP
! LEFT =    STATEMENT FUNCTION WHICH RETURNS THE VALUE
!             .TRUE. IFF (XP,YP) IS ON OR TO THE LEFT OF THE
!             VECTOR (XA,YA)->(XB,YB)
!
      LEFT(XA,YA,XB,YB,XP,YP) = (XB-XA)*(YP-YA) .GE. (XP-XA)*(YB-YA)
!
! STORE IN1, IN2, AND LWK IN LOCAL VARIABLES AND CHECK FOR
!   ERRORS.
!
      N1 = IN1
      N2 = IN2
      IWEND = LWK
      IF (N1 .LT. 1  .OR.  N2 .LT. 1  .OR.  N1 .EQ. N2  .OR. IWEND .LT. 0) GO TO 35
!
! STORE THE COORDINATES OF N1 AND N2 AND INITIALIZE IWL.
!
      X1 = X(N1)
      Y1 = Y(N1)
      X2 = X(N2)
      Y2 = Y(N2)
      IWL = 0
!
! SET NR AND NL TO ADJACENT NEIGHBORS OF N1 SUCH THAT
!   NR LEFT N2->N1 AND NL LEFT N1->N2.
!
!   SET INDF AND INDX TO THE INDICES OF THE FIRST AND LAST
!     NEIGHBORS OF N1 AND SET N1LST TO THE LAST NEIGHBOR.
!
      INDF = 1
      IF (N1 .GT. 1) INDF = IEND(N1-1) + 1
      INDX = IEND(N1)
      N1LST = IADJ(INDX)
      IF (N1LST .EQ. 0) INDX = INDX - 1
      IF (N1LST .EQ. 0) GO TO 2
!
!   N1 IS AN INTERIOR NODE.  LOOP THROUGH THE NEIGHBORS NL
!     IN REVERSE ORDER UNTIL NL LEFT N1->N2.
!
      NL = N1LST
    1 IF ( LEFT(X1,Y1,X2,Y2,X(NL),Y(NL)) ) GO TO 2
      INDX = INDX - 1
      NL = IADJ(INDX)
      IF (INDX .GT. INDF) GO TO 1
!
!   NL IS THE FIRST NEIGHBOR OF N1.  SET NR TO THE LAST
!     NEIGHBOR AND TEST FOR AN ARC N1-N2.
!
      NR = N1LST
      IF (NL .EQ. N2) GO TO 34
      GO TO 4
!
!   NL = IADJ(INDX) LEFT N1->N2 AND INDX .GT. INDF.  SET
!     NR TO THE PRECEDING NEIGHBOR OF N1.
!
    2 INDX = INDX - 1
      NR = IADJ(INDX)
      IF ( LEFT(X2,Y2,X1,Y1,X(NR),Y(NR)) ) GO TO 3
      IF (INDX .GT. INDF) GO TO 2
!
!   SET NL AND NR TO THE FIRST AND LAST NEIGHBORS OF N1 AND
!     TEST FOR AN INVALID DATA STRUCTURE (N1 CANNOT BE A
!     BOUNDARY NODE AND CANNOT BE ADJACENT TO N2).
!
      NL = NR
      NR = N1LST
      IF (NR .EQ. 0  .OR.  NR .EQ. N2) GO TO 37
      GO TO 4
!
!   SET NL TO THE NEIGHBOR FOLLOWING NR AND TEST FOR AN ARC
!     N1-N2.
!
    3 NL = IADJ(INDX+1)
      IF (NL .EQ. N2  .OR.  NR .EQ. N2) GO TO 34
!
! STORE THE ORDERED SEQUENCE OF INTERSECTING EDGES NL->NR IN
!   IWK(1,IWL)->IWK(2,IWL).
!
    4 IWL = IWL + 1
      IF (IWL .LE. IWEND) IWK(1,IWL) = NL
      IF (IWL .LE. IWEND) IWK(2,IWL) = NR
!
!   SET NEXT TO THE NEIGHBOR OF NL WHICH FOLLOWS NR.
!
      INDX = IEND(NL)
      IF (IADJ(INDX) .NE. NR) GO TO 5
!
!   NR IS THE LAST NEIGHBOR OF NL.  SET NEXT TO THE FIRST
!     NEIGHBOR.
!
      INDX = 0
      IF (NL .NE. 1) INDX = IEND(NL-1)
      GO TO 6
!
!   NR IS NOT THE LAST NEIGHBOR OF NL.  LOOP THROUGH THE
!     NEIGHBORS IN REVERSE ORDER.
!
    5 INDX = INDX - 1
      IF (IADJ(INDX) .NE. NR) GO TO 5
!
!   STORE NEXT, TEST FOR AN INVALID TRIANGULATION (NL->NR
!     CANNOT BE A BOUNDARY EDGE), AND TEST FOR TERMINATION
!     OF THE LOOP.
!
    6 NEXT = IADJ(INDX+1)
      IF (NEXT .EQ. 0) GO TO 37
      IF (NEXT .EQ. N2) GO TO 8
!
!   SET NL OR NR TO NEXT.
!
      IF ( LEFT(X1,Y1,X2,Y2,X(NEXT),Y(NEXT)) ) GO TO 7
      NR = NEXT
      GO TO 4
    7 NL = NEXT
      GO TO 4
!
! IWL IS THE NUMBER OF ARCS WHICH INTERSECT N1-N2.  STORE
!   LWK AND TEST FOR SUFFICIENT SPACE.
!
    8 LWK = IWL
      IF (IWL .GT. IWEND) GO TO 36
      IWEND = IWL
!
! INITIALIZE FOR EDGE SWAPPING LOOP -- ALL POSSIBLE SWAPS
!   ARE APPLIED (EVEN IF THE NEW ARC AGAIN INTERSECTS
!   N1-N2), ARCS TO THE LEFT OF N1->N2 ARE STORED IN THE
!   LEFT PORTION OF IWK, AND ARCS TO THE RIGHT ARE STORED IN
!   THE RIGHT PORTION.  IWF AND IWL INDEX THE FIRST AND LAST
!   INTERSECTING ARCS.
!
      IER = 0
      IWF = 1
!
! TOP OF LOOP -- SET N0 TO N1 AND NL->NR TO THE FIRST EDGE.
!   IWC POINTS TO THE ARC CURRENTLY BEING PROCESSED.  LFT
!   .LE. 0 IFF N0 LEFT N1->N2.
!
    9 LFT = 0
      N0 = N1
      X0 = X1
      Y0 = Y1
      NL = IWK(1,IWF)
      NR = IWK(2,IWF)
      IWC = IWF
!
!   SET NEXT TO THE NODE OPPOSITE NL->NR UNLESS IWC IS THE
!     LAST ARC.
!
   10 IF (IWC .EQ. IWL) GO TO 21
      IWCP1 = IWC + 1
      NEXT = IWK(1,IWCP1)
      IF (NEXT .NE. NL) GO TO 15
      NEXT = IWK(2,IWCP1)
!
!   NEXT RIGHT N1->N2 AND IWC .LT. IWL.  TEST FOR A POSSIBLE
!     SWAP.
!
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )GO TO 13
      IF (LFT .GE. 0) GO TO 11
      IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )GO TO 13
!
!   REPLACE NL->NR WITH N0->NEXT.
!
      CALL SWAPP(NEXT,N0,NL,NR, IADJ,IEND )
      IWK(1,IWC) = N0
      IWK(2,IWC) = NEXT
      GO TO 14
!
!   SWAP NL-NR FOR N0-NEXT, SHIFT COLUMNS IWC+1,...,IWL TO
!     THE LEFT, AND STORE N0-NEXT IN THE RIGHT PORTION OF
!     IWK.
!
   11 CALL SWAPP(NEXT,N0,NL,NR, IADJ,IEND )
      DO 12 I = IWCP1,IWL
        IWK(1,I-1) = IWK(1,I)
   12   IWK(2,I-1) = IWK(2,I)
      IWK(1,IWL) = N0
      IWK(2,IWL) = NEXT
      IWL = IWL - 1
      NR = NEXT
      GO TO 10
!
!   A SWAP IS NOT POSSIBLE.  SET N0 TO NR.
!
   13 N0 = NR
      X0 = X(N0)
      Y0 = Y(N0)
      LFT = 1
!
!   ADVANCE TO THE NEXT ARC.
!
   14 NR = NEXT
      IWC = IWC + 1
      GO TO 10
!
!   NEXT LEFT N1->N2, NEXT .NE. N2, AND IWC .LT. IWL.
!     TEST FOR A POSSIBLE SWAP.
!
   15 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )GO TO 19
      IF (LFT .LE. 0) GO TO 16
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )GO TO 19
!
!   REPLACE NL->NR WITH NEXT->N0.
!
      CALL SWAPP(NEXT,N0,NL,NR, IADJ,IEND )
      IWK(1,IWC) = NEXT
      IWK(2,IWC) = N0
      GO TO 20
!
!   SWAP NL-NR FOR N0-NEXT, SHIFT COLUMNS IWF,...,IWC-1 TO
!     THE RIGHT, AND STORE N0-NEXT IN THE LEFT PORTION OF
!     IWK.
!
   16 CALL SWAPP(NEXT,N0,NL,NR, IADJ,IEND )
      I = IWC
   17 IF (I .EQ. IWF) GO TO 18
      IWK(1,I) = IWK(1,I-1)
      IWK(2,I) = IWK(2,I-1)
      I = I - 1
      GO TO 17
   18 IWK(1,IWF) = N0
      IWK(2,IWF) = NEXT
      IWF = IWF + 1
      GO TO 20
!
!   A SWAP IS NOT POSSIBLE.  SET N0 TO NL.
!
   19 N0 = NL
      X0 = X(N0)
      Y0 = Y(N0)
      LFT = -1
!
!   ADVANCE TO THE NEXT ARC.
!
   20 NL = NEXT
      IWC = IWC + 1
      GO TO 10
!
!   N2 IS OPPOSITE NL->NR (IWC = IWL).
!
   21 IF (N0 .EQ. N1) GO TO 24
      IF (LFT .LT. 0) GO TO 22
!
!   N0 RIGHT N1->N2.  TEST FOR A POSSIBLE SWAP.
!
      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X2,Y2) ) GO TO 9
!
!   SWAP NL-NR FOR N0-N2 AND STORE N0-N2 IN THE RIGHT
!     PORTION OF IWK.
!
      CALL SWAPP(N2,N0,NL,NR, IADJ,IEND )
      IWK(1,IWL) = N0
      IWK(2,IWL) = N2
      IWL = IWL - 1
      GO TO 9
!
!   N0 LEFT N1->N2.  TEST FOR A POSSIBLE SWAP.
!
   22 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X2,Y2) ) GO TO 9
!
!   SWAP NL-NR FOR N0-N2, SHIFT COLUMNS IWF,...,IWL-1 TO THE
!     RIGHT, AND STORE N0-N2 IN THE LEFT PORTION OF IWK.
!
      CALL SWAPP(N2,N0,NL,NR, IADJ,IEND )
      I = IWL
   23 IWK(1,I) = IWK(1,I-1)
      IWK(2,I) = IWK(2,I-1)
      I = I - 1
      IF (I .GT. IWF) GO TO 23
      IWK(1,IWF) = N0
      IWK(2,IWF) = N2
      IWF = IWF + 1
      GO TO 9
!
! IWF = IWC = IWL.  SWAP OUT THE LAST ARC FOR N1-N2 AND
!   STORE ZEROS IN IWK.
!
   24 CALL SWAPP(N2,N1,NL,NR, IADJ,IEND )
      IWK(1,IWC) = 0
      IWK(2,IWC) = 0
      IF (IWC .EQ. 1) GO TO 29
!
! OPTIMIZATION LOOPS -- OPTIMIZE THE SET OF NEW ARCS TO THE
!   LEFT OF IN1->IN2.  THE LOOP IS REPEATED UNTIL NO SWAPS
!   ARE PERFORMED.
!
      IWCM1 = IWC - 1
   25 SWP = .FALSE.
      DO 28 I = 1,IWCM1
        IO1 = IWK(1,I)
        IO2 = IWK(2,I)
!
!   SET N1 TO THE NEIGHBOR OF IO1 WHICH FOLLOWS IO2 AND SET
!     N2 TO THE NEIGHBOR OF IO1 WHICH PRECEDES IO2.
!
        INDF = 1
        IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
        INDL = IEND(IO1)
        INDX = INDL
        IF (IADJ(INDX) .NE. IO2) GO TO 26
!
!   IO2 IS THE LAST NEIGHBOR OF IO1.
!
        N1 = IADJ(INDF)
        N2 = IADJ(INDX-1)
        GO TO 27
!
!   IO2 IS NOT THE LAST NEIGHBOR OF IO1.  LOOP THROUGH THE
!     NEIGHBORS IN REVERSE ORDER.
!
   26   INDX = INDX - 1
        IF (IADJ(INDX) .NE. IO2) GO TO 26
        N1 = IADJ(INDX+1)
        IF (INDX .NE. INDF) N2 = IADJ(INDX-1)
        IF (INDX .EQ. INDF) N2 = IADJ(INDL)
!
!   TEST IO1-IO2 FOR A SWAP.
!
   27   IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y) ) GO TO 28
        SWP = .TRUE.
        CALL SWAPP(N1,N2,IO1,IO2, IADJ,IEND )
        IWK(1,I) = N1
        IWK(2,I) = N2
   28   CONTINUE
      IF (SWP) GO TO 25
!
! TEST FOR TERMINATION.
!
   29 IF (IWC .EQ. IWEND) RETURN
      IWCP1 = IWC + 1
!
! OPTIMIZE THE SET OF NEW ARCS TO THE RIGHT OF IN1->IN2.
!
   30 SWP = .FALSE.
      DO 33 I = IWCP1,IWEND
        IO1 = IWK(1,I)
        IO2 = IWK(2,I)
!
!   SET N1 AND N2 TO THE NODES OPPOSITE IO1->IO2 AND
!     IO2->IO1, RESPECTIVELY.
!
        INDF = 1
        IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
        INDL = IEND(IO1)
        INDX = INDL
        IF (IADJ(INDX) .NE. IO2) GO TO 31
!
        N1 = IADJ(INDF)
        N2 = IADJ(INDX-1)
        GO TO 32
!
   31   INDX = INDX - 1
        IF (IADJ(INDX) .NE. IO2) GO TO 31
        N1 = IADJ(INDX+1)
        IF (INDX .NE. INDF) N2 = IADJ(INDX-1)
        IF (INDX .EQ. INDF) N2 = IADJ(INDL)
!
   32   IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y) ) GO TO 33
        SWP = .TRUE.
        CALL SWAPP(N1,N2,IO1,IO2, IADJ,IEND )
        IWK(1,I) = N1
        IWK(2,I) = N2
   33   CONTINUE
      IF (SWP) GO TO 30
      RETURN
!
! IN1 AND IN2 WERE ADJACENT ON INPUT.
!
   34 IER = 0
      LWK = 0
      RETURN
!
! PARAMETER OUT OF RANGE
!
   35 IER = 1
      RETURN
!
! INSUFFICIENT SPACE IN IWK
!
   36 IER = 2
      RETURN
!
! INVALID TRIANGULATION DATA STRUCTURE
!
   37 IER = 3
      RETURN
      END
!==============================================================================
! SUBROUTINE GETNP
!==============================================================================
      SUBROUTINE GETNP (X,Y,IADJ,IEND,L, NPTS, DS,IER)
      INTEGER IADJ(1), IEND(1), L, NPTS(L), IER
      REAL    X(1), Y(1), DS
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A THIESSEN TRIANGULATION OF N NODES AND AN ARRAY
! NPTS CONTAINING THE INDICES OF L-1 NODES ORDERED BY
! EUCLIDEAN DISTANCE FROM NPTS(1), THIS SUBROUTINE SETS
! NPTS(L) TO THE INDEX OF THE NEXT NODE IN THE SEQUENCE --
! THE NODE, OTHER THAN NPTS(1),...,NPTS(L-1), WHICH IS
! CLOSEST TO NPTS(1).  THUS, THE ORDERED SEQUENCE OF K
! CLOSEST NODES TO N1 (INCLUDING N1) MAY BE DETERMINED BY
! K-1 CALLS TO GETNP WITH NPTS(1) = N1 AND L = 2,3,...,K
! FOR K .GE. 2.
!   THE ALGORITHM USES THE FACT THAT, IN A THIESSEN TRIAN-
! GULATION, THE K-TH CLOSEST NODE TO A GIVEN NODE N1 IS A
! NEIGHBOR OF ONE OF THE K-1 CLOSEST NODES TO N1.
!
! INPUT PARAMETERS - X,Y - VECTORS OF LENGTH N CONTAINING
!                          THE CARTESIAN COORDINATES OF THE
!                          NODES.
!
!                   IADJ - SET OF ADJACENCY LISTS OF NODES
!                          IN THE TRIANGULATION.
!
!                   IEND - POINTERS TO THE ENDS OF ADJACENCY
!                          LISTS FOR EACH NODE IN THE TRI-
!                          ANGULATION.
!
!                      L - NUMBER OF NODES IN THE SEQUENCE
!                          ON OUTPUT.  2 .LE. L .LE. N.
!
!                   NPTS - ARRAY OF LENGTH .GE. L CONTAIN-
!                          ING THE INDICES OF THE L-1 CLOS-
!                          EST NODES TO NPTS(1) IN THE FIRST
!                          L-1 LOCATIONS.
!
! IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
!
! INPUT PARAMETERS OTHER THAN NPTS ARE NOT ALTERED BY THIS
!   ROUTINE.
!
! OUTPUT PARAMETERS - NPTS - UPDATED WITH THE INDEX OF THE
!                            L-TH CLOSEST NODE TO NPTS(1) IN
!                            POSITION L UNLESS IER = 1.
!
!                       DS - SQUARED EUCLIDEAN DISTANCE BE-
!                            TWEEN NPTS(1) AND NPTS(L)
!                            UNLESS IER = 1.
!
!                      IER - ERROR INDICATOR
!                            IER = 0 IF NO ERRORS WERE EN-
!                                    COUNTERED.
!                            IER = 1 IF L IS OUT OF RANGE.
!
! MODULES REFERENCED BY GETNP - NONE
!
! INTRINSIC FUNCTION CALLED BY GETNP - IABS
!
!***********************************************************
!
      INTEGER LM1, N1, I, NI, NP, INDF, INDL, INDX, NB
      REAL    X1, Y1, DNP, DNB
!
! LOCAL PARAMETERS -
!
! LM1 =     L - 1
! N1 =      NPTS(1)
! I =       NPTS INDEX AND DO-LOOP INDEX
! NI =      NPTS(I)
! NP =      CANDIDATE FOR NPTS(L)
! INDF =    IADJ INDEX OF THE FIRST NEIGHBOR OF NI
! INDL =    IADJ INDEX OF THE LAST NEIGHBOR OF NI
! INDX =    IADJ INDEX IN THE RANGE INDF,...,INDL
! NB =      NEIGHBOR OF NI AND CANDIDATE FOR NP
! X1,Y1 =   COORDINATES OF N1
! DNP,DNB = SQUARED DISTANCES FROM N1 TO NP AND NB,
!             RESPECTIVELY
!
      LM1 = L - 1
      IF (LM1 .LT. 1) GO TO 4
      IER = 0
      N1 = NPTS(1)
      X1 = X(N1)
      Y1 = Y(N1)
!
! MARK THE ELEMENTS OF NPTS
!
      DO 1 I = 1,LM1
        NI = NPTS(I)
        IEND(NI) = -IEND(NI)
    1   CONTINUE
!
! CANDIDATES FOR NP = NPTS(L) ARE THE UNMARKED NEIGHBORS
!   OF NODES IN NPTS.  NP=0 IS A FLAG TO SET NP TO THE
!   FIRST CANDIDATE ENCOUNTERED.
!
      NP = 0
      DNP = 0.
!
! LOOP ON NODES NI IN NPTS
!
      DO 2 I = 1,LM1
        NI = NPTS(I)
        INDF = 1
        IF (NI .GT. 1) INDF = IABS(IEND(NI-1)) + 1
        INDL = -IEND(NI)
!
! LOOP ON NEIGHBORS NB OF NI
!
        DO 2 INDX = INDF,INDL
          NB = IADJ(INDX)
          IF (NB .EQ. 0  .OR.  IEND(NB) .LT. 0) GO TO 2
!
! NB IS AN UNMARKED NEIGHBOR OF NI.  REPLACE NP IF NB IS
!   CLOSER TO N1 OR IS THE FIRST CANDIDATE ENCOUNTERED.
!
          DNB = (X(NB)-X1)**2 + (Y(NB)-Y1)**2
          IF (NP .NE. 0  .AND.  DNB .GE. DNP) GO TO 2
          NP = NB
          DNP = DNB
    2     CONTINUE
      NPTS(L) = NP
      DS = DNP
!
! UNMARK THE ELEMENTS OF NPTS
!
      DO 3 I = 1,LM1
        NI = NPTS(I)
        IEND(NI) = -IEND(NI)
    3   CONTINUE
      RETURN
!
! L IS OUT OF RANGE
!
    4 IER = 1
      RETURN
      END
!==============================================================================
! FUNCTION TINDEX
!==============================================================================
      INTEGER FUNCTION TINDEX (NVERTX,NABOR,IADJ,IEND)
      INTEGER NVERTX, NABOR, IADJ(1), IEND(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS FUNCTION RETURNS THE INDEX OF NABOR IN THE
! ADJACENCY LIST FOR NVERTX.
!
! INPUT PARAMETERS - NVERTX - NODE WHOSE ADJACENCY LIST IS
!                             TO BE SEARCHED.
!
!                     NABOR - NODE WHOSE INDEX IS TO BE
!                             RETURNED.  NABOR MUST BE
!                             CONNECTED TO NVERTX.
!
!                      IADJ - SET OF ADJACENCY LISTS.
!
!                      IEND - POINTERS TO THE ENDS OF
!                             ADJACENCY LISTS IN IADJ.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER -  INDEX - IADJ(INDEX) = NABOR.
!
! MODULES REFERENCED BY INDEX - NONE
!
!***********************************************************
!
      INTEGER NB, INDX
!
! LOCAL PARAMETERS -
!
! NB =   LOCAL COPY OF NABOR
! INDX = INDEX FOR IADJ
!
      NB = NABOR
!
! INITIALIZATION
!
      INDX = IEND(NVERTX) + 1
!
! SEARCH THE LIST OF NVERTX NEIGHBORS FOR NB
!
    1 INDX = INDX - 1
      IF (IADJ(INDX) .NE. NB) GO TO 1
!
      TINDEX = INDX
      RETURN
      END
!==============================================================================
! SUBROUTINE INTADD
!==============================================================================
      SUBROUTINE INTADD (KK,I1,I2,I3, IADJ,IEND )
      INTEGER KK, I1, I2, I3, IADJ(1), IEND(KK)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE ADDS AN INTERIOR NODE TO A TRIANGULATION
! OF A SET OF KK-1 POINTS IN THE PLANE.  IADJ AND IEND ARE
! UPDATED WITH THE INSERTION OF NODE KK IN THE TRIANGLE
! WHOSE VERTICES ARE I1, I2, AND I3.
!
! INPUT PARAMETERS -        KK - INDEX OF NODE TO BE
!                                INSERTED.  KK .GE. 4.
!
!                     I1,I2,I3 - INDICES OF THE VERTICES OF
!                                A TRIANGLE CONTAINING NODE
!                                KK -- IN COUNTERCLOCKWISE
!                                ORDER.
!
!                         IADJ - SET OF ADJACENCY LISTS
!                                OF NODES IN THE MESH.
!
!                         IEND - POINTERS TO THE ENDS OF
!                                ADJACENCY LISTS IN IADJ FOR
!                                EACH NODE IN THE MESH.
!
!   IADJ AND IEND MAY BE CREATED BY TRMESH AND MUST CONTAIN
! THE VERTICES I1, I2, AND I3.  I1,I2,I3 MAY BE DETERMINED
! BY TRFIND.
!
! KK, I1, I2, AND I3 ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
!                                 OF NODE KK AS THE LAST
!                                 ENTRY.  NODE KK WILL BE
!                                 CONNECTED TO NODES I1, I2,
!                                 AND I3.  NO OPTIMIZATION
!                                 OF THE MESH IS PERFORMED.
!
! MODULE REFERENCED BY INTADD - SHIFTD
!
! INTRINSIC FUNCTION CALLED BY INTADD - MOD
!
!***********************************************************
!
      INTEGER K, KM1, N(3), NFT(3), IP1, IP2, IP3, INDX, NF, &
              NL, N1, N2, IMIN, IMAX, I, ITEMP
!
! LOCAL PARAMETERS -
!
! K =           LOCAL COPY OF KK
! KM1 =         K - 1
! N =           VECTOR CONTAINING I1, I2, I3
! NFT =         POINTERS TO THE TOPS OF THE 3 SETS OF IADJ
!                 ELEMENTS TO BE SHIFTED DOWNWARD
! IP1,IP2,IP3 = PERMUTATION INDICES FOR N AND NFT
! INDX =        INDEX FOR IADJ AND N
! NF,NL =       INDICES OF FIRST AND LAST ENTRIES IN IADJ
!                 TO BE SHIFTED DOWN
! N1,N2 =       FIRST 2 VERTICES OF A NEW TRIANGLE --
!                 (N1,N2,KK)
! IMIN,IMAX =   BOUNDS ON DO-LOOP INDEX -- FIRST AND LAST
!                 ELEMENTS OF IEND TO BE INCREMENTED
! I =           DO-LOOP INDEX
! ITEMP =       TEMPORARY STORAGE LOCATION
!
      K = KK
!
! INITIALIZATION
!
      N(1) = I1
      N(2) = I2
      N(3) = I3
!
! SET UP NFT
!
      DO 2 I = 1,3
        N1 = N(I)
        INDX = MOD(I,3) + 1
        N2 = N(INDX)
        INDX = IEND(N1) + 1
!
! FIND THE INDEX OF N2 AS A NEIGHBOR OF N1
!
    1   INDX = INDX - 1
        IF (IADJ(INDX) .NE. N2) GO TO 1
        NFT(I) = INDX + 1
    2   CONTINUE
!
! ORDER THE VERTICES BY DECREASING MAGNITUDE.
!   N(IP(I+1)) PRECEDES N(IP(I)) IN IEND FOR
!   I = 1,2.
!
      IP1 = 1
      IP2 = 2
      IP3 = 3
      IF ( N(2) .LE. N(1) ) GO TO 3
      IP1 = 2
      IP2 = 1
    3 IF ( N(3) .LE. N(IP1) ) GO TO 4
      IP3 = IP1
      IP1 = 3
    4 IF ( N(IP3) .LE. N(IP2) )  GO TO 5
      ITEMP = IP2
      IP2 = IP3
      IP3 = ITEMP
!
! ADD NODE K TO THE ADJACENCY LISTS OF EACH VERTEX AND
!   UPDATE IEND.  FOR EACH VERTEX, A SET OF IADJ ELEMENTS
!   IS SHIFTED DOWNWARD AND K IS INSERTED.  SHIFTING STARTS
!   AT THE END OF THE ARRAY.
!
    5 KM1 = K - 1
      NL = IEND(KM1)
      NF = NFT(IP1)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,3, IADJ )
      IADJ(NF+2) = K
      IMIN = N(IP1)
      IMAX = KM1
      DO 6 I = IMIN,IMAX
        IEND(I) = IEND(I) + 3
    6   CONTINUE
!
      NL = NF - 1
      NF = NFT(IP2)
      CALL SHIFTD(NF,NL,2, IADJ )
      IADJ(NF+1) = K
      IMAX = IMIN - 1
      IMIN = N(IP2)
      DO 7 I = IMIN,IMAX
        IEND(I) = IEND(I) + 2
    7   CONTINUE
!
      NL = NF - 1
      NF = NFT(IP3)
      CALL SHIFTD(NF,NL,1, IADJ )
      IADJ(NF) = K
      IMAX = IMIN - 1
      IMIN = N(IP3)
      DO 8 I = IMIN,IMAX
        IEND(I) = IEND(I) + 1
    8   CONTINUE
!
! ADD NODE K TO IEND AND ITS NEIGHBORS TO IADJ
!
      INDX = IEND(KM1)
      IEND(K) = INDX + 3
      DO 9 I = 1,3
        INDX = INDX + 1
        IADJ(INDX) = N(I)
    9   CONTINUE
      RETURN
      END
!==============================================================================
! SUBROUTINE PERMUT
!==============================================================================
      SUBROUTINE PERMUT (NN,IP, A )
      INTEGER NN, IP(NN)
      REAL    A(NN)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE APPLIES A SET OF PERMUTATIONS TO A VECTOR.
!
! INPUT PARAMETERS - NN - LENGTH OF A AND IP.
!
!                    IP - VECTOR CONTAINING THE SEQUENCE OF
!                         INTEGERS 1,...,NN PERMUTED IN THE
!                         SAME FASHION THAT A IS TO BE PER-
!                         MUTED.
!
!                     A - VECTOR TO BE PERMUTED.
!
! NN AND IP ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - A - REORDERED VECTOR REFLECTING THE
!                         PERMUTATIONS DEFINED BY IP.
!
! MODULES REFERENCED BY PERMUT - NONE
!
!***********************************************************
!
      INTEGER N, K, J, IPJ
      REAL    TEMP
!
! LOCAL PARAMETERS -
!
! N =    LOCAL COPY OF NN
! K =    INDEX FOR IP AND FOR THE FIRST ELEMENT OF A IN A
!          PERMUTATION
! J =    INDEX FOR IP AND A, J .GE. K
! IPJ =  IP(J)
! TEMP = TEMPORARY STORAGE FOR A(K)
!
      N = NN
      IF (N .LT. 2) RETURN
      K = 1
!
! LOOP ON PERMUTATIONS
!
    1 J = K
      TEMP = A(K)
!
! APPLY PERMUTATION TO A.  IP(J) IS MARKED (MADE NEGATIVE)
!   AS BEING INCLUDED IN THE PERMUTATION.
!
    2 IPJ = IP(J)
      IP(J) = -IPJ
      IF (IPJ .EQ. K) GO TO 3
      A(J) = A(IPJ)
      J = IPJ
      GO TO 2
    3 A(J) = TEMP
!
! SEARCH FOR AN UNMARKED ELEMENT OF IP
!
    4 K = K + 1
      IF (K .GT. N) GO TO 5
      IF (IP(K) .GT. 0) GO TO 1
      GO TO 4
!
! ALL PERMUTATIONS HAVE BEEN APPLIED.  UNMARK IP.
!
    5 DO 6 K = 1,N
        IP(K) = -IP(K)
    6   CONTINUE
      RETURN
      END
!==============================================================================
! SUBROUTINE QSORT
!==============================================================================
      SUBROUTINE QSORT (N,X, IND)
      INTEGER N, IND(N)
      REAL    X(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO
! SORT THE REAL ARRAY X INTO INCREASING ORDER.  THE ALGOR-
! ITHM IS AS FOLLOWS.  IND IS INITIALIZED TO THE ORDERED
! SEQUENCE OF INDICES 1,...,N, AND ALL INTERCHANGES ARE
! APPLIED TO IND.  X IS DIVIDED INTO TWO PORTIONS BY PICKING
! A CENTRAL ELEMENT T.  THE FIRST AND LAST ELEMENTS ARE COM-
! PARED WITH T, AND INTERCHANGES ARE APPLIED AS NECESSARY SO
! THAT THE THREE VALUES ARE IN ASCENDING ORDER.  INTER-
! CHANGES ARE THEN APPLIED SO THAT ALL ELEMENTS GREATER THAN
! T ARE IN THE UPPER PORTION OF THE ARRAY AND ALL ELEMENTS
! LESS THAN T ARE IN THE LOWER PORTION.  THE UPPER AND LOWER
! INDICES OF ONE OF THE PORTIONS ARE SAVED IN LOCAL ARRAYS,
! AND THE PROCESS IS REPEATED ITERATIVELY ON THE OTHER
! PORTION.  WHEN A PORTION IS COMPLETELY SORTED, THE PROCESS
! BEGINS AGAIN BY RETRIEVING THE INDICES BOUNDING ANOTHER
! UNSORTED PORTION.
!
! INPUT PARAMETERS -   N - LENGTH OF THE ARRAY X.
!
!                      X - VECTOR OF LENGTH N TO BE SORTED.
!
!                    IND - VECTOR OF LENGTH .GE. N.
!
! N AND X ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETER - IND - SEQUENCE OF INDICES 1,...,N
!                          PERMUTED IN THE SAME FASHION AS X
!                          WOULD BE.  THUS, THE ORDERING ON
!                          X IS DEFINED BY Y(I) = X(IND(I)).
!
! MODULES REFERENCED BY QSORT - NONE
!
! INTRINSIC FUNCTIONS CALLED BY QSORT - IFIX, FLOAT
!
!***********************************************************
!
! NOTE -- IU AND IL MUST BE DIMENSIONED .GE. LOG(N) WHERE
!         LOG HAS BASE 2.
!
!***********************************************************
!
      INTEGER IU(21), IL(21)
      INTEGER M, I, J, K, L, IJ, IT, ITT, INDX
      REAL    R, T
!
! LOCAL PARAMETERS -
!
! IU,IL =  TEMPORARY STORAGE FOR THE UPPER AND LOWER
!            INDICES OF PORTIONS OF THE ARRAY X
! M =      INDEX FOR IU AND IL
! I,J =    LOWER AND UPPER INDICES OF A PORTION OF X
! K,L =    INDICES IN THE RANGE I,...,J
! IJ =     RANDOMLY CHOSEN INDEX BETWEEN I AND J
! IT,ITT = TEMPORARY STORAGE FOR INTERCHANGES IN IND
! INDX =   TEMPORARY INDEX FOR X
! R =      PSEUDO RANDOM NUMBER FOR GENERATING IJ
! T =      CENTRAL ELEMENT OF X
!
      IF (N .LE. 0) RETURN
!
! INITIALIZE IND, M, I, J, AND R
!
      DO 1 I = 1,N
        IND(I) = I
    1   CONTINUE
      M = 1
      I = 1
      J = N
      R = .375
!
! TOP OF LOOP
!
    2 IF (I .GE. J) GO TO 10
      IF (R .GT. .5898437) GO TO 3
      R = R + .0390625
      GO TO 4
    3 R = R - .21875
!
! INITIALIZE K
!
    4 K = I
!
! SELECT A CENTRAL ELEMENT OF X AND SAVE IT IN T
!
      IJ = I + IFIX(R*FLOAT(J-I))
      IT = IND(IJ)
      T = X(IT)
!
! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
!   INTERCHANGE IT WITH T
!
      INDX = IND(I)
      IF (X(INDX) .LE. T) GO TO 5
      IND(IJ) = INDX
      IND(I) = IT
      IT = INDX
      T = X(IT)
!
! INITIALIZE L
!
    5 L = J
!
! IF THE LAST ELEMENT OF THE ARRAY IS LESS THAN T,
!   INTERCHANGE IT WITH T
!
      INDX = IND(J)
      IF (X(INDX) .GE. T) GO TO 7
      IND(IJ) = INDX
      IND(J) = IT
      IT = INDX
      T = X(IT)
!
! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
!   INTERCHANGE IT WITH T
!
      INDX = IND(I)
      IF (X(INDX) .LE. T) GO TO 7
      IND(IJ) = INDX
      IND(I) = IT
      IT = INDX
      T = X(IT)
      GO TO 7
!
! INTERCHANGE ELEMENTS K AND L
!
    6 ITT = IND(L)
      IND(L) = IND(K)
      IND(K) = ITT
!
! FIND AN ELEMENT IN THE UPPER PART OF THE ARRAY WHICH IS
!   NOT LARGER THAN T
!
    7 L = L - 1
      INDX = IND(L)
      IF (X(INDX) .GT. T) GO TO 7
!
! FIND AN ELEMENT IN THE LOWER PART OF THE ARRAY WHCIH IS
!   NOT SMALLER THAN T
!
    8 K = K + 1
      INDX = IND(K)
      IF (X(INDX) .LT. T) GO TO 8
!
! IF K .LE. L, INTERCHANGE ELEMENTS K AND L
!
      IF (K .LE. L) GO TO 6
!
! SAVE THE UPPER AND LOWER SUBSCRIPTS OF THE PORTION OF THE
!   ARRAY YET TO BE SORTED
!
      IF (L-I .LE. J-K) GO TO 9
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 11
!
    9 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 11
!
! BEGIN AGAIN ON ANOTHER UNSORTED PORTION OF THE ARRAY
!
   10 M = M - 1
      IF (M .EQ. 0) RETURN
      I = IL(M)
      J = IU(M)
!
   11 IF (J-I .GE. 11) GO TO 4
      IF (I .EQ. 1) GO TO 2
      I = I - 1
!
! SORT ELEMENTS I+1,...,J.  NOTE THAT 1 .LE. I .LT. J AND
!   J-I .LT. 11.
!
   12 I = I + 1
      IF (I .EQ. J) GO TO 10
      INDX = IND(I+1)
      T = X(INDX)
      IT = INDX
      INDX = IND(I)
      IF (X(INDX) .LE. T) GO TO 12
      K = I
!
   13 IND(K+1) = IND(K)
      K = K - 1
      INDX = IND(K)
      IF (T .LT. X(INDX)) GO TO 13
      IND(K+1) = IT
      GO TO 12
      END
!==============================================================================
! SUBROUTINE REORDR
!==============================================================================
      SUBROUTINE REORDR (N,IFLAG, A,B,C, IND)
      INTEGER N, IFLAG, IND(N)
      REAL    A(N), B(N), C(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO
! REORDER THE REAL ARRAY A INTO INCREASING ORDER.  A RECORD
! OF THE PERMUTATIONS APPLIED TO A IS STORED IN IND, AND
! THESE PERMUTATIONS MAY BE APPLIED TO ONE OR TWO ADDITIONAL
! VECTORS BY THIS ROUTINE.  ANY OTHER VECTOR V MAY BE PER-
! MUTED IN THE SAME FASHION BY CALLING SUBROUTINE PERMUT
! WITH N, IND, AND V AS PARAMETERS.
!   A SET OF NODES (X(I),Y(I)) AND DATA VALUES Z(I) MAY BE
! PREPROCESSED BY REORDR FOR INCREASED EFFICIENCY IN THE
! TRIANGULATION ROUTINE TRMESH.  EFFICIENCY IS INCREASED BY
! A FACTOR OF APPROXIMATELY SQRT(N)/6 FOR RANDOMLY DISTRIB-
! UTED NODES, AND THE PREPROCESSING IS ALSO USEFUL FOR
! DETECTING DUPLICATE NODES.  EITHER X OR Y MAY BE USED AS
! THE SORT KEY (ASSOCIATED WITH A).
!
! INPUT PARAMETERS - N - NUMBER OF NODES.
!
!                IFLAG - NUMBER OF VECTORS TO BE PERMUTED.
!                        IFLAG .LE. 0 IF A, B, AND C ARE TO
!                                     REMAIN UNALTERED.
!                        IFLAG .EQ. 1 IF ONLY A IS TO BE
!                                     PERMUTED.
!                        IFLAG .EQ. 2 IF A AND B ARE TO BE
!                                     PERMUTED.
!                        IFLAG .GE. 3 IF A, B, AND C ARE TO
!                                     BE PERMUTED.
!
!                A,B,C - VECTORS OF LENGTH N TO BE SORTED
!                        (ON THE COMPONENTS OF A), OR DUMMY
!                        PARAMETERS, DEPENDING ON IFLAG.
!
!                  IND - VECTOR OF LENGTH .GE. N.
!
! N, IFLAG, AND ANY DUMMY PARAMETERS ARE NOT ALTERED BY THIS
!   ROUTINE.
!
! OUTPUT PARAMETERS - A,B,C - SORTED OR UNALTERED VECTORS.
!
!                       IND - SEQUENCE OF INDICES 1,...,N
!                             PERMUTED IN THE SAME FASHION
!                             AS THE REAL VECTORS.  THUS,
!                             THE ORDERING MAY BE APPLIED TO
!                             A REAL VECTOR V AND STORED IN
!                             W BY SETTING W(I) = V(IND(I)),
!                             OR V MAY BE OVERWRITTEN WITH
!                             THE ORDERING BY A CALL TO PER-
!                             MUT.
!
! MODULES REFERENCED BY REORDR - QSORT, PERMUT
!
!***********************************************************
!
      INTEGER NN, NV
!
! LOCAL PARAMETERS -
!
! NN = LOCAL COPY OF N
! NV = LOCAL COPY OF IFLAG
!
      NN = N
      NV = IFLAG
      CALL QSORT(NN,A, IND)
      IF (NV .LE. 0) RETURN
      CALL PERMUT(NN,IND, A )
      IF (NV .EQ. 1) RETURN
      CALL PERMUT(NN,IND, B )
      IF (NV .EQ. 2) RETURN
      CALL PERMUT(NN,IND, C )
      RETURN
      END
!==============================================================================
! SUBROUTINE SHIFTD
!==============================================================================
      SUBROUTINE SHIFTD (NFRST,NLAST,KK, IARR )
      INTEGER NFRST, NLAST, KK, IARR(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE SHIFTS A SET OF CONTIGUOUS ELEMENTS OF AN
! INTEGER ARRAY KK POSITIONS DOWNWARD (UPWARD IF KK .LT. 0).
! THE LOOPS ARE UNROLLED IN ORDER TO INCREASE EFFICIENCY.
!
! INPUT PARAMETERS - NFRST,NLAST - BOUNDS ON THE PORTION OF
!                                  IARR TO BE SHIFTED.  ALL
!                                  ELEMENTS BETWEEN AND
!                                  INCLUDING THE BOUNDS ARE
!                                  SHIFTED UNLESS NFRST .GT.
!                                  NLAST, IN WHICH CASE NO
!                                  SHIFT OCCURS.
!
!                             KK - NUMBER OF POSITIONS EACH
!                                  ELEMENT IS TO BE SHIFTED.
!                                  IF KK .LT. 0 SHIFT UP.
!                                  IF KK .GT. 0 SHIFT DOWN.
!
!                           IARR - INTEGER ARRAY OF LENGTH
!                                  .GE. NLAST + MAX(KK,0).
!
! NFRST, NLAST, AND KK ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETER -        IARR - SHIFTED ARRAY.
!
! MODULES REFERENCED BY SHIFTD - NONE
!
!***********************************************************
!
      INTEGER INC, K, NF, NL, NLP1, NS, NSL, I, IBAK, INDX, &
              IMAX
      DATA    INC/5/
!
! LOCAL PARAMETERS -
!
! INC =  DO-LOOP INCREMENT (UNROLLING FACTOR) -- IF INC IS
!          CHANGED, STATEMENTS MUST BE ADDED TO OR DELETED
!          FROM THE DO-LOOPS
! K =    LOCAL COPY OF KK
! NF =   LOCAL COPY OF NFRST
! NL =   LOCAL COPY OF NLAST
! NLP1 = NL + 1
! NS =   NUMBER OF SHIFTS
! NSL =  NUMBER OF SHIFTS DONE IN UNROLLED DO-LOOP (MULTIPLE
!          OF INC)
! I =    DO-LOOP INDEX AND INDEX FOR IARR
! IBAK = INDEX FOR DOWNWARD SHIFT OF IARR
! INDX = INDEX FOR IARR
! IMAX = BOUND ON DO-LOOP INDEX
!
      K = KK
      NF = NFRST
      NL = NLAST
      IF (NF .GT. NL  .OR.  K .EQ. 0) RETURN
      NLP1 = NL + 1
      NS = NLP1 - NF
      NSL = INC*(NS/INC)
      IF ( K .LT. 0) GO TO 4
!
! SHIFT DOWNWARD STARTING FROM THE BOTTOM
!
      IF (NSL .LE. 0) GO TO 2
      DO 1 I = 1,NSL,INC
        IBAK = NLP1 - I
        INDX = IBAK + K
        IARR(INDX) = IARR(IBAK)
        IARR(INDX-1) = IARR(IBAK-1)
        IARR(INDX-2) = IARR(IBAK-2)
        IARR(INDX-3) = IARR(IBAK-3)
        IARR(INDX-4) = IARR(IBAK-4)
    1   CONTINUE
!
! PERFORM THE REMAINING NS-NSL SHIFTS ONE AT A TIME
!
    2 IBAK = NLP1 - NSL
    3 IF (IBAK .LE. NF) RETURN
      IBAK = IBAK - 1
      INDX = IBAK + K
      IARR(INDX) = IARR(IBAK)
      GO TO 3
!
! SHIFT UPWARD STARTING FROM THE TOP
!
    4 IF (NSL .LE. 0) GO TO 6
      IMAX = NLP1 - INC
      DO 5 I = NF,IMAX,INC
        INDX = I + K
        IARR(INDX) = IARR(I)
        IARR(INDX+1) = IARR(I+1)
        IARR(INDX+2) = IARR(I+2)
        IARR(INDX+3) = IARR(I+3)
        IARR(INDX+4) = IARR(I+4)
    5   CONTINUE
!
! PERFORM THE REMAINING NS-NSL SHIFTS ONE AT A TIME
!
    6 I = NSL + NF
    7 IF (I .GT. NL) RETURN
      INDX = I + K
      IARR(INDX) = IARR(I)
      I = I + 1
      GO TO 7
      END
!==============================================================================
! SUBROUTINE SWAPP
!==============================================================================
      SUBROUTINE SWAPP (NIN1,NIN2,NOUT1,NOUT2, IADJ,IEND )
      INTEGER NIN1, NIN2, NOUT1, NOUT2, IADJ(1), IEND(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS SUBROUTINE SWAPS THE DIAGONALS IN A CONVEX QUADRI-
! LATERAL.
!
! INPUT PARAMETERS -  NIN1,NIN2,NOUT1,NOUT2 - NODAL INDICES
!                            OF A PAIR OF ADJACENT TRIANGLES
!                            WHICH FORM A CONVEX QUADRILAT-
!                            ERAL.  NOUT1 AND NOUT2 ARE CON-
!                            NECTED BY AN ARC WHICH IS TO BE
!                            REPLACED BY THE ARC NIN1-NIN2.
!                            (NIN1,NOUT1,NOUT2) MUST BE TRI-
!                            ANGLE VERTICES IN COUNTERCLOCK-
!                            WISE ORDER.
!
! THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
!                IADJ,IEND - TRIANGULATION DATA STRUCTURE
!                            (SEE SUBROUTINE TRMESH).
!
! OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ARC
!                                 REPLACEMENT.
!
! MODULES REFERENCED BY SWAP - INDEX, SHIFTD
!
!***********************************************************
!
      INTEGER IN(2), IO(2), IP1, IP2, J, K, NF, NL, I, IMIN, IMAX
      INTEGER, EXTERNAL :: TINDEX
!
! LOCAL PARAMETERS -
!
! IN =        NIN1 AND NIN2 ORDERED BY INCREASING MAGNITUDE
!               (THE NEIGHBORS OF IN(1) PRECEDE THOSE OF
!               IN(2) IN IADJ)
! IO =        NOUT1 AND NOUT2 IN INCREASING ORDER
! IP1,IP2 =   PERMUTATION OF (1,2) SUCH THAT IO(IP1)
!               PRECEDES IO(IP2) AS A NEIGHBOR OF IN(1)
! J,K =       PERMUTATION OF (1,2) USED AS INDICES OF IN
!               AND IO
! NF,NL =     IADJ INDICES BOUNDARY A PORTION OF THE ARRAY
!               TO BE SHIFTED
! I =         IEND INDEX
! IMIN,IMAX = BOUNDS ON THE PORTION OF IEND TO BE INCRE-
!               MENTED OR DECREMENTED
!
      IN(1) = NIN1
      IN(2) = NIN2
      IO(1) = NOUT1
      IO(2) = NOUT2
      IP1 = 1
!
! ORDER THE INDICES SO THAT IN(1) .LT. IN(2) AND IO(1) .LT.
!   IO(2), AND CHOOSE IP1 AND IP2 SUCH THAT (IN(1),IO(IP1),
!   IO(IP2)) FORMS A TRIANGLE.
!
      IF (IN(1) .LT. IN(2)) GO TO 1
      IN(1) = IN(2)
      IN(2) = NIN1
      IP1 = 2
    1 IF (IO(1) .LT. IO(2)) GO TO 2
      IO(1) = IO(2)
      IO(2) = NOUT1
      IP1 = 3 - IP1
    2 IP2 = 3 - IP1
      IF (IO(2) .LT. IN(1)) GO TO 8
      IF (IN(2) .LT. IO(1)) GO TO 12
!
! IN(1) AND IO(1) PRECEDE IN(2) AND IO(2).  FOR (J,K) =
!   (1,2) AND (2,1), DELETE IO(K) AS A NEIGHBOR OF IO(J)
!   BY SHIFTING A PORTION OF IADJ EITHER UP OR DOWN AND
!   AND INSERT IN(K) AS A NEIGHBOR OF IN(J).
!
      DO 7 J = 1,2
        K = 3 - J
        IF (IN(J) .GT. IO(J)) GO TO 4
!
!   THE NEIGHBORS OF IN(J) PRECEDE THOSE OF IO(J) -- SHIFT
!     DOWN BY 1
!
        NF = 1 + TINDEX(IN(J),IO(IP1),IADJ,IEND)
        NL = -1 + TINDEX(IO(J),IO(K),IADJ,IEND)
        IF (NF .LE. NL) CALL SHIFTD(NF,NL,1, IADJ )
        IADJ(NF) = IN(K)
        IMIN = IN(J)
        IMAX = IO(J)-1
        DO 3 I = IMIN,IMAX
    3     IEND(I) = IEND(I) + 1
        GO TO 6
!
!   THE NEIGHBORS OF IO(J) PRECEDE THOSE OF IN(J) -- SHIFT
!     UP BY 1
!
    4   NF = 1 + TINDEX(IO(J),IO(K),IADJ,IEND)
        NL = -1 + TINDEX(IN(J),IO(IP2),IADJ,IEND)
        IF (NF .LE. NL) CALL SHIFTD(NF,NL,-1, IADJ )
        IADJ(NL) = IN(K)
        IMIN = IO(J)
        IMAX = IN(J) - 1
        DO 5 I = IMIN,IMAX
    5     IEND(I) = IEND(I) - 1
!
!   REVERSE (IP1,IP2) FOR (J,K) = (2,1)
!
    6   IP1 = IP2
        IP2 = 3 - IP1
    7   CONTINUE
      RETURN
!
! THE VERTICES ARE ORDERED (IO(1),IO(2),IN(1),IN(2)).
!   DELETE IO(2) BY SHIFTING UP BY 1
!
    8 NF = 1 + TINDEX(IO(1),IO(2),IADJ,IEND)
      NL = -1 + TINDEX(IO(2),IO(1),IADJ,IEND)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,-1, IADJ )
      IMIN = IO(1)
      IMAX = IO(2)-1
      DO 9 I = IMIN,IMAX
    9   IEND(I) = IEND(I) - 1
!
!   DELETE IO(1) BY SHIFTING UP BY 2 AND INSERT IN(2)
!
      NF = NL + 2
      NL = -1 + TINDEX(IN(1),IO(IP2),IADJ,IEND)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,-2, IADJ )
      IADJ(NL-1) = IN(2)
      IMIN = IO(2)
      IMAX = IN(1)-1
      DO 10 I = IMIN,IMAX
   10   IEND(I) = IEND(I) - 2
!
!   SHIFT UP BY 1 AND INSERT IN(1)
!
      NF = NL + 1
      NL = -1 + TINDEX(IN(2),IO(IP1),IADJ,IEND)
      CALL SHIFTD(NF,NL,-1, IADJ )
      IADJ(NL) = IN(1)
      IMIN = IN(1)
      IMAX = IN(2)-1
      DO 11 I = IMIN,IMAX
   11   IEND(I) = IEND(I) - 1
      RETURN
!
! THE VERTICES ARE ORDERED (IN(1),IN(2),IO(1),IO(2)).
!   DELETE IO(1) BY SHIFTING DOWN BY 1
!
   12 NF = 1 + TINDEX(IO(1),IO(2),IADJ,IEND)
      NL = -1 + TINDEX(IO(2),IO(1),IADJ,IEND)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,1, IADJ )
      IMIN = IO(1)
      IMAX = IO(2) - 1
      DO 13 I = IMIN,IMAX
   13   IEND(I) = IEND(I) + 1
!
!   DELETE IO(2) BY SHIFTING DOWN BY 2 AND INSERT IN(1)
!
      NL = NF - 2
      NF = 1 + TINDEX(IN(2),IO(IP2),IADJ,IEND)
      IF (NF .LE. NL) CALL SHIFTD(NF,NL,2, IADJ )
      IADJ(NF+1) = IN(1)
      IMIN = IN(2)
      IMAX = IO(1) - 1
      DO 14 I = IMIN,IMAX
   14   IEND(I) = IEND(I) + 2
!
!   SHIFT DOWN BY 1 AND INSERT IN(2)
!
      NL = NF - 1
      NF = 1 + TINDEX(IN(1),IO(IP1),IADJ,IEND)
      CALL SHIFTD(NF,NL,1, IADJ )
      IADJ(NF) = IN(2)
      IMIN = IN(1)
      IMAX = IN(2) - 1
      DO 15 I = IMIN,IMAX
   15   IEND(I) = IEND(I) + 1
      RETURN
      END
!==============================================================================
! FUNCTION SWPTST
!==============================================================================
      LOGICAL FUNCTION SWPTST (IN1,IN2,IO1,IO2,X,Y)
      INTEGER IN1, IN2, IO1, IO2
      REAL    X(1), Y(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS FUNCTION DECIDES WHETHER OR NOT TO REPLACE A
! DIAGONAL ARC IN A QUADRILATERAL WITH THE OTHER DIAGONAL.
! THE DETERMINATION IS BASED ON THE SIZES OF THE ANGLES
! CONTAINED IN THE 2 TRIANGLES DEFINED BY THE DIAGONAL.
! THE DIAGONAL IS CHOSEN TO MAXIMIZE THE SMALLEST OF THE
! SIX ANGLES OVER THE TWO PAIRS OF TRIANGLES.
!
! INPUT PARAMETERS -  IN1,IN2,IO1,IO2 - NODE INDICES OF THE
!                              FOUR POINTS DEFINING THE
!                              QUADRILATERAL.  IO1 AND IO2
!                              ARE CURRENTLY CONNECTED BY A
!                              DIAGONAL ARC.  THIS ARC
!                              SHOULD BE REPLACED BY AN ARC
!                              CONNECTING IN1, IN2 IF THE
!                              DECISION IS MADE TO SWAP.
!                              IN1,IO1,IO2 MUST BE IN
!                              COUNTERCLOCKWISE ORDER.
!
!                        X,Y - VECTORS OF NODAL COORDINATES.
!                              (X(I),Y(I)) ARE THE COORD-
!                              INATES OF NODE I FOR I = IN1,
!                              IN2, IO1, OR IO2.
!
! NONE OF THE INPUT PARAMETERS ARE ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETER -  SWPTST - .TRUE. IFF THE ARC CONNECTING
!                              IO1 AND IO2 IS TO BE REPLACED
!
! MODULES REFERENCED BY SWPTST - NONE
!
!***********************************************************
!
      REAL DX11, DX12, DX22, DX21, DY11, DY12, DY22, DY21, &
           SIN1, SIN2, COS1, COS2, SIN12
!
! LOCAL PARAMETERS -
!
! DX11,DY11 = X,Y COORDINATES OF THE VECTOR IN1-IO1
! DX12,DY12 = X,Y COORDINATES OF THE VECTOR IN1-IO2
! DX22,DY22 = X,Y COORDINATES OF THE VECTOR IN2-IO2
! DX21,DY21 = X,Y COORDINATES OF THE VECTOR IN2-IO1
! SIN1 =      CROSS PRODUCT OF THE VECTORS IN1-IO1 AND
!               IN1-IO2 -- PROPORTIONAL TO SIN(T1) WHERE T1
!               IS THE ANGLE AT IN1 FORMED BY THE VECTORS
! COS1 =      INNER PRODUCT OF THE VECTORS IN1-IO1 AND
!               IN1-IO2 -- PROPORTIONAL TO COS(T1)
! SIN2 =      CROSS PRODUCT OF THE VECTORS IN2-IO2 AND
!               IN2-IO1 -- PROPORTIONAL TO SIN(T2) WHERE T2
!               IS THE ANGLE AT IN2 FORMED BY THE VECTORS
! COS2 =      INNER PRODUCT OF THE VECTORS IN2-IO2 AND
!               IN2-IO1 -- PROPORTIONAL TO COS(T2)
! SIN12 =     SIN1*COS2 + COS1*SIN2 -- PROPORTIONAL TO
!               SIN(T1+T2)
!
      SWPTST = .FALSE.
!
! COMPUTE THE VECTORS CONTAINING THE ANGLES T1, T2
!
      DX11 = X(IO1) - X(IN1)
      DX12 = X(IO2) - X(IN1)
      DX22 = X(IO2) - X(IN2)
      DX21 = X(IO1) - X(IN2)
!
      DY11 = Y(IO1) - Y(IN1)
      DY12 = Y(IO2) - Y(IN1)
      DY22 = Y(IO2) - Y(IN2)
      DY21 = Y(IO1) - Y(IN2)
!
! COMPUTE INNER PRODUCTS
!
      COS1 = DX11*DX12 + DY11*DY12
      COS2 = DX22*DX21 + DY22*DY21
!
! THE DIAGONALS SHOULD BE SWAPPED IFF (T1+T2) .GT. 180
!   DEGREES.  THE FOLLOWING TWO TESTS INSURE NUMERICAL
!   STABILITY.
!
      IF (COS1 .GE. 0.  .AND.  COS2 .GE. 0.) RETURN
      IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 1
!
! COMPUTE VECTOR CROSS PRODUCTS
!
      SIN1 = DX11*DY12 - DX12*DY11
      SIN2 = DX22*DY21 - DX21*DY22
      SIN12 = SIN1*COS2 + COS1*SIN2
      IF (SIN12 .GE. 0.) RETURN
    1 SWPTST = .TRUE.
      RETURN
      END
!==============================================================================
! SUBROUTINE TRFIND
!==============================================================================
      SUBROUTINE TRFIND (NST,PX,PY,X,Y,IADJ,IEND, I1,I2,I3)
      INTEGER NST, IADJ(1), IEND(1), I1, I2, I3
      REAL    PX, PY, X(1), Y(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE LOCATES A POINT P IN A THIESSEN TRIANGU-
! LATION, RETURNING THE VERTEX INDICES OF A TRIANGLE WHICH
! CONTAINS P.  TRFIND IS PART OF AN INTERPOLATION PACKAGE
! WHICH PROVIDES SUBROUTINES FOR CREATING THE MESH.
!
! INPUT PARAMETERS -    NST - INDEX OF NODE AT WHICH TRFIND
!                             BEGINS SEARCH.  SEARCH TIME
!                             DEPENDS ON THE PROXIMITY OF
!                             NST TO P.
!
!                     PX,PY - X AND Y-COORDINATES OF THE
!                             POINT TO BE LOCATED.
!
!                       X,Y - VECTORS OF COORDINATES OF
!                             NODES IN THE MESH.  (X(I),Y(I))
!                             DEFINES NODE I FOR I = 1,...,N
!                             WHERE N .GE. 3.
!
!                      IADJ - SET OF ADJACENCY LISTS OF
!                             NODES IN THE MESH.
!
!                      IEND - POINTERS TO THE ENDS OF
!                             ADJACENCY LISTS IN IADJ FOR
!                             EACH NODE IN THE MESH.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - I1,I2,I3 - VERTEX INDICES IN COUNTER-
!                                CLOCKWISE ORDER - VERTICES
!                                OF A TRIANGLE CONTAINING P
!                                IF P IS AN INTERIOR NODE.
!                                IF P IS OUTSIDE OF THE
!                                BOUNDARY OF THE MESH, I1
!                                AND I2 ARE THE FIRST (RIGHT
!                                -MOST) AND LAST (LEFTMOST)
!                                NODES WHICH ARE VISIBLE
!                                FROM P, AND I3 = 0.  IF P
!                                AND ALL OF THE NODES LIE ON
!                                A SINGLE LINE THEN I1 = I2
!                                = I3 = 0.
!
! MODULES REFERENCED BY TRFIND - NONE
!
! INTRINSIC FUNCTION CALLED BY TRFIND - MAX0
!
!***********************************************************
!
      INTEGER N0, N1, N2, N3, N4, INDX, IND, NF, &
              NL, NEXT
      REAL    XP, YP
      LOGICAL LEFT
!
! LOCAL PARAMETERS -
!
! XP,YP =     LOCAL VARIABLES CONTAINING PX AND PY
! N0,N1,N2 =  NODES IN COUNTERCLOCKWISE ORDER DEFINING A
!               CONE (WITH VERTEX N0) CONTAINING P
! N3,N4 =     NODES OPPOSITE N1-N2 AND N2-N1, RESPECTIVELY
! INDX,IND =  INDICES FOR IADJ
! NF,NL =     FIRST AND LAST NEIGHBORS OF N0 IN IADJ, OR
!               FIRST (RIGHTMOST) AND LAST (LEFTMOST) NODES
!               VISIBLE FROM P WHEN P IS OUTSIDE THE
!               BOUNDARY
! NEXT =      CANDIDATE FOR I1 OR I2 WHEN P IS OUTSIDE OF
!               THE BOUNDARY
! LEFT =      STATEMENT FUNCTION WHICH COMPUTES THE SIGN OF
!               A CROSS PRODUCT (Z-COMPONENT).  LEFT(X1,...,
!               Y0) = .TRUE. IFF (X0,Y0) IS ON OR TO THE
!               LEFT OF THE VECTOR FROM (X1,Y1) TO (X2,Y2).
!
      LEFT(X1,Y1,X2,Y2,X0,Y0) = (X2-X1)*(Y0-Y1) .GE. (X0-X1)*(Y2-Y1)
      XP = PX
      YP = PY
!
! INITIALIZE VARIABLES AND FIND A CONE CONTAINING P
!
      N0 = MAX0(NST,1)
    1 INDX = IEND(N0)
      NL = IADJ(INDX)
      INDX = 1
      IF (N0 .NE. 1) INDX = IEND(N0-1) + 1
      NF = IADJ(INDX)
      N1 = NF
      IF (NL .NE. 0) GO TO 3
!
! N0 IS A BOUNDARY NODE.  SET NL TO THE LAST NONZERO
!   NEIGHBOR OF N0.
!
      IND = IEND(N0) - 1
      NL = IADJ(IND)
      IF ( LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) ) GO TO 2
!
! P IS OUTSIDE THE BOUNDARY
!
      NL = N0
      GO TO 16
    2 IF ( LEFT(X(NL),Y(NL),X(N0),Y(N0),XP,YP) ) GO TO 4
!
! P IS OUTSIDE THE BOUNDARY AND N0 IS THE RIGHTMOST
!   VISIBLE BOUNDARY NODE
!
      I1 = N0
      GO TO 18
!
! N0 IS AN INTERIOR NODE.  FIND N1.
!
    3 IF ( LEFT(X(N0),Y(N0),X(N1),Y(N1),XP,YP) ) GO TO 4
      INDX = INDX + 1
      N1 = IADJ(INDX)
      IF (N1 .EQ. NL) GO TO 7
      GO TO 3
!
! P IS TO THE LEFT OF ARC N0-N1.  INITIALIZE N2 TO THE NEXT
!   NEIGHBOR OF N0.
!
    4 INDX = INDX + 1
      N2 = IADJ(INDX)
      IF ( .NOT. LEFT(X(N0),Y(N0),X(N2),Y(N2),XP,YP) )GO TO 8
      N1 = N2
      IF (N1 .NE. NL) GO TO 4
      IF ( .NOT. LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) )GO TO 7
      IF (XP .EQ. X(N0) .AND. YP .EQ. Y(N0)) GO TO 6
!
! P IS LEFT OF OR ON ARCS N0-NB FOR ALL NEIGHBORS NB
!   OF N0.
! ALL POINTS ARE COLLINEAR IFF P IS LEFT OF NB-N0 FOR
!   ALL NEIGHBORS NB OF N0.  SEARCH THE NEIGHBORS OF N0
!   IN REVERSE ORDER.  NOTE -- N1 = NL AND INDX POINTS TO
!   NL.
!
    5 IF ( .NOT. LEFT(X(N1),Y(N1),X(N0),Y(N0),XP,YP) ) GO TO 6
      IF (N1 .EQ. NF) GO TO 20
      INDX = INDX - 1
      N1 = IADJ(INDX)
      GO TO 5
!
! P IS TO THE RIGHT OF N1-N0, OR P=N0.  SET N0 TO N1 AND
!   START OVER.
!
    6 N0 = N1
      GO TO 1
!
! P IS BETWEEN ARCS N0-N1 AND N0-NF
!
    7 N2 = NF
!
! P IS CONTAINED IN A CONE DEFINED BY LINE SEGMENTS N0-N1
!   AND N0-N2 WHERE N1 IS ADJACENT TO N2
!
    8 N3 = N0
    9 IF ( LEFT(X(N1),Y(N1),X(N2),Y(N2),XP,YP) ) GO TO 13
!
! SET N4 TO THE FIRST NEIGHBOR OF N2 FOLLOWING N1
!
      INDX = IEND(N2)
      IF (IADJ(INDX) .NE. N1) GO TO 10
!
! N1 IS THE LAST NEIGHBOR OF N2.
! SET N4 TO THE FIRST NEIGHBOR.
!
      INDX = 1
      IF (N2 .NE. 1) INDX = IEND(N2-1) + 1
      N4 = IADJ(INDX)
      GO TO 11
!
! N1 IS NOT THE LAST NEIGHBOR OF N2
!
   10 INDX = INDX-1
      IF (IADJ(INDX) .NE. N1) GO TO 10
      N4 = IADJ(INDX+1)
      IF (N4 .NE. 0) GO TO 11
!
! P IS OUTSIDE THE BOUNDARY
!
      NF = N2
      NL = N1
      GO TO 16
!
! DEFINE A NEW ARC N1-N2 WHICH INTERSECTS THE LINE
!   SEGMENT N0-P
!
   11 IF ( LEFT(X(N0),Y(N0),X(N4),Y(N4),XP,YP) ) GO TO 12
      N3 = N2
      N2 = N4
      GO TO 9
   12 N3 = N1
      N1 = N4
      GO TO 9
!
! P IS IN THE TRIANGLE (N1,N2,N3) AND NOT ON N2-N3.  IF
!   N3-N1 OR N1-N2 IS A BOUNDARY ARC CONTAINING P, TREAT P
!   AS EXTERIOR.
!
   13 INDX = IEND(N1)
      IF (IADJ(INDX) .NE. 0) GO TO 15
!
! N1 IS A BOUNDARY NODE.  N3-N1 IS A BOUNDARY ARC IFF N3
!   IS THE LAST NONZERO NEIGHBOR OF N1.
!
      IF (N3 .NE. IADJ(INDX-1)) GO TO 14
!
! N3-N1 IS A BOUNDARY ARC
!
      IF ( .NOT. LEFT(X(N1),Y(N1),X(N3),Y(N3),XP,YP) )GO TO 14
!
! P LIES ON N1-N3
!
      I1 = N1
      I2 = N3
      I3 = 0
      RETURN
!
! N3-N1 IS NOT A BOUNDARY ARC CONTAINING P.  N1-N2 IS A
!   BOUNDARY ARC IFF N2 IS THE FIRST NEIGHBOR OF N1.
!
   14 INDX = 1
      IF (N1 .NE. 1) INDX = IEND(N1-1) + 1
      IF (N2 .NE. IADJ(INDX)) GO TO 15
!
! N1-N2 IS A BOUNDARY ARC
!
      IF ( .NOT. LEFT(X(N2),Y(N2),X(N1),Y(N1),XP,YP) )GO TO 15
!
! P LIES ON N1-N2
!
      I1 = N2
      I2 = N1
      I3 = 0
      RETURN
!
! P DOES NOT LIE ON A BOUNDARY ARC.
!
   15 I1 = N1
      I2 = N2
      I3 = N3
      RETURN
!
! NF AND NL ARE ADJACENT BOUNDARY NODES WHICH ARE VISIBLE
!   FROM P.  FIND THE FIRST VISIBLE BOUNDARY NODE.
! SET NEXT TO THE FIRST NEIGHBOR OF NF.
!
   16 INDX = 1
      IF (NF .NE. 1) INDX = IEND(NF-1) + 1
      NEXT = IADJ(INDX)
      IF ( LEFT(X(NF),Y(NF),X(NEXT),Y(NEXT),XP,YP) )GO TO 17
      NF = NEXT
      GO TO 16
!
! NF IS THE FIRST (RIGHTMOST) VISIBLE BOUNDARY NODE
!
   17 I1 = NF
!
! FIND THE LAST VISIBLE BOUNDARY NODE.  NL IS THE FIRST
!   CANDIDATE FOR I2.
! SET NEXT TO THE LAST NEIGHBOR OF NL.
!
   18 INDX = IEND(NL) - 1
      NEXT = IADJ(INDX)
      IF ( LEFT(X(NEXT),Y(NEXT),X(NL),Y(NL),XP,YP) )GO TO 19
      NL = NEXT
      GO TO 18
!
! NL IS THE LAST (LEFTMOST) VISIBLE BOUNDARY NODE
!
   19 I2 = NL
      I3 = 0
      RETURN
!
! ALL POINTS ARE COLLINEAR
!
   20 I1 = 0
      I2 = 0
      I3 = 0
      RETURN
      END
!==============================================================================
! SUBROUTINE TRMESH
!==============================================================================
      SUBROUTINE TRMESH (N,X,Y, IADJ,IEND,IER)
      INTEGER N, IADJ(1), IEND(N), IER
      REAL    X(N), Y(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE CREATES A THIESSEN TRIANGULATION OF N
! ARBITRARILY SPACED POINTS IN THE PLANE REFERRED TO AS
! NODES.  THE TRIANGULATION IS OPTIMAL IN THE SENSE THAT IT
! IS AS NEARLY EQUIANGULAR AS POSSIBLE.  TRMESH IS PART OF
! AN INTERPOLATION PACKAGE WHICH ALSO PROVIDES SUBROUTINES
! TO REORDER THE NODES, ADD A NEW NODE, DELETE AN ARC, PLOT
! THE MESH, AND PRINT THE DATA STRUCTURE.
!   UNLESS THE NODES ARE ALREADY ORDERED IN SOME REASONABLE
! FASHION, THEY SHOULD BE REORDERED BY SUBROUTINE REORDR FOR
! INCREASED EFFICIENCY BEFORE CALLING TRMESH.
!
! INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
!                            N .GE. 3.
!
!                      X,Y - N-VECTORS OF COORDINATES.
!                            (X(I),Y(I)) DEFINES NODE I.
!
!                     IADJ - VECTOR OF LENGTH .GE. 6*N-9.
!
!                     IEND - VECTOR OF LENGTH .GE. N.
!
! N, X, AND Y ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - IADJ - ADJACENCY LISTS OF NEIGHBORS IN
!                            COUNTERCLOCKWISE ORDER.  THE
!                            LIST FOR NODE I+1 FOLLOWS THAT
!                            FOR NODE I WHERE X AND Y DEFINE
!                            THE ORDER.  THE VALUE 0 DENOTES
!                            THE BOUNDARY (OR A PSEUDO-NODE
!                            AT INFINITY) AND IS ALWAYS THE
!                            LAST NEIGHBOR OF A BOUNDARY
!                            NODE.  IADJ IS UNCHANGED IF IER
!                            .NE. 0.
!
!                     IEND - POINTERS TO THE ENDS OF
!                            ADJACENCY LISTS (SETS OF
!                            NEIGHBORS) IN IADJ.  THE
!                            NEIGHBORS OF NODE 1 BEGIN IN
!                            IADJ(1).  FOR K .GT. 1, THE
!                            NEIGHBORS OF NODE K BEGIN IN
!                            IADJ(IEND(K-1)+1) AND K HAS
!                            IEND(K) - IEND(K-1) NEIGHBORS
!                            INCLUDING (POSSIBLY) THE
!                            BOUNDARY.  IADJ(IEND(K)) .EQ. 0
!                            IFF NODE K IS ON THE BOUNDARY.
!                            IEND IS UNCHANGED IF IER = 1.
!                            IF IER = 2 IEND CONTAINS THE
!                            INDICES OF A SEQUENCE OF N
!                            NODES ORDERED FROM LEFT TO
!                            RIGHT WHERE LEFT AND RIGHT ARE
!                            DEFINED BY ASSUMING NODE 1 IS
!                            TO THE LEFT OF NODE 2.
!
!                      IER - ERROR INDICATOR
!                            IER = 0 IF NO ERRORS WERE
!                                    ENCOUNTERED.
!                            IER = 1 IF N .LT. 3.
!                            IER = 2 IF N .GE. 3 AND ALL
!                                    NODES ARE COLLINEAR.
!
! MODULES REFERENCED BY TRMESH - SHIFTD, ADNODE, TRFIND,
!                                INTADD, BDYADD, SWPTST,
!                                SWAP, INDEX
!
!***********************************************************
!
      INTEGER NN, K, KM1, NL, NR, IND, INDX, N0, ITEMP,   &
              IERR, KM1D2, KMI, I, KMIN
      REAL    XL, YL, XR, YR, DXR, DYR, XK, YK, DXK, DYK, &
              CPROD, SPROD
!
! LOCAL PARAMETERS -
!
! NN =          LOCAL COPY OF N
! K =           NODE (INDEX) TO BE INSERTED INTO IEND
! KM1 =         K-1 - (VARIABLE) LENGTH OF IEND
! NL,NR =       IEND(1), IEND(KM1) -- LEFTMOST AND RIGHTMOST
!                 NODES IN IEND AS VIEWED FROM THE RIGHT OF
!                 1-2 WHEN IEND CONTAINS THE INITIAL ORDERED
!                 SET OF NODAL INDICES
! XL,YL,XR,YR = X AND Y COORDINATES OF NL AND NR
! DXR,DYR =     XR-XL, YR-YL
! XK,YK =       X AND Y COORDINATES OF NODE K
! DXK,DYK =     XK-XL, YK-YL
! CPROD =       VECTOR CROSS PRODUCT OF NL-NR AND NL-K --
!                 USED TO DETERMINE THE POSITION OF NODE K
!                 WITH RESPECT TO THE LINE DEFINED BY THE
!                 NODES IN IEND
! SPROD =       SCALAR PRODUCT USED TO DETERMINE THE
!                 INTERVAL CONTAINING NODE K WHEN K IS ON
!                 THE LINE DEFINED BY THE NODES IN IEND
! IND,INDX =    INDICES FOR IEND AND IADJ, RESPECTIVELY
! N0,ITEMP =    TEMPORARY NODES (INDICES)
! IERR =        DUMMY PARAMETER FOR CALL TO ADNODE
! KM1D2,KMI,I = KM1/2, K-I, DO-LOOP INDEX -- USED IN IEND
!                 REORDERING LOOP
! KMIN =        FIRST NODE INDEX SENT TO ADNODE
!
      NN = N
      IER = 1
      IF (NN .LT. 3) RETURN
      IER = 0
!
! INITIALIZE IEND, NL, NR, AND K
!
      IEND(1) = 1
      IEND(2) = 2
      XL = X(1)
      YL = Y(1)
      XR = X(2)
      YR = Y(2)
      K = 2
!
! BEGIN LOOP ON NODES 3,4,...
!
    1 DXR = XR-XL
      DYR = YR-YL
!
! NEXT LOOP BEGINS HERE IF NL AND NR ARE UNCHANGED
!
    2 IF (K .EQ. NN) GO TO 13
      KM1 = K
      K = KM1 + 1
      XK = X(K)
      YK = Y(K)
      DXK = XK-XL
      DYK = YK-YL
      CPROD = DXR*DYK - DXK*DYR
      IF (CPROD .GT. 0.) GO TO 6
      IF (CPROD .LT. 0.) GO TO 8
!
! NODE K LIES ON THE LINE CONTAINING NODES 1,2,...,K-1.
!   SET SPROD TO (NL-NR,NL-K).
!
      SPROD = DXR*DXK + DYR*DYK
      IF (SPROD .GT. 0.) GO TO 3
!
! NODE K IS TO THE LEFT OF NL.  INSERT K AS THE FIRST
!   (LEFTMOST) NODE IN IEND AND SET NL TO K.
!
      CALL SHIFTD(1,KM1,1, IEND )
      IEND(1) = K
      XL = XK
      YL = YK
      GO TO 1
!
! NODE K IS TO THE RIGHT OF NL.  FIND THE LEFTMOST NODE
!   N0 WHICH LIES TO THE RIGHT OF K.
!   SET SPROD TO (N0-NL,N0-K).
!
    3 DO 4 IND = 2,KM1
        N0 = IEND(IND)
        SPROD = (XL-X(N0))*(XK-X(N0)) + (YL-Y(N0))*(YK-Y(N0))
        IF (SPROD .GE. 0.) GO TO 5
    4   CONTINUE
!
! NODE K IS TO THE RIGHT OF NR.  INSERT K AS THE LAST
!   (RIGHTMOST) NODE IN IEND AND SET NR TO K.
!
      IEND(K) = K
      XR = XK
      YR = YK
      GO TO 1
!
! NODE K LIES BETWEEN IEND(IND-1) AND IEND(IND).  INSERT K
!   IN IEND.
!
    5 CALL SHIFTD(IND,KM1,1, IEND )
      IEND(IND) = K
      GO TO 2
!
! NODE K IS TO THE LEFT OF NL-NR.  REORDER IEND SO THAT NL
!   IS THE LEFTMOST NODE AS VIEWED FROM K.
!
    6 KM1D2 = KM1/2
      DO 7 I = 1,KM1D2
        KMI = K-I
        ITEMP = IEND(I)
        IEND(I) = IEND(KMI)
        IEND(KMI) = ITEMP
    7   CONTINUE
!
! NODE K IS TO THE RIGHT OF NL-NR.  CREATE A TRIANGULATION
!   CONSISTING OF NODES 1,2,...,K.
!
    8 NL = IEND(1)
      NR = IEND(KM1)
!
! CREATE THE ADJACENCY LISTS FOR THE FIRST K-1 NODES.
!   INSERT NEIGHBORS IN REVERSE ORDER.  EACH NODE HAS FOUR
!   NEIGHBORS EXCEPT NL AND NR WHICH HAVE THREE.
!
      DO 9 IND = 1,KM1
        N0 = IEND(IND)
        INDX = 4*N0
        IF (N0 .GE. NL) INDX = INDX-1
        IF (N0 .GE. NR) INDX = INDX-1
        IADJ(INDX) = 0
        INDX = INDX-1
        IF (IND .LT. KM1) IADJ(INDX) = IEND(IND+1)
        IF (IND .LT. KM1) INDX = INDX-1
        IADJ(INDX) = K
        IF (IND .EQ. 1) GO TO 9
        IADJ(INDX-1) = IEND(IND-1)
    9   CONTINUE
!
! CREATE THE ADJACENCY LIST FOR NODE K
!
      INDX = 5*KM1 - 1
      IADJ(INDX) = 0
      DO 10 IND = 1,KM1
        INDX = INDX-1
        IADJ(INDX) = IEND(IND)
   10   CONTINUE
!
! REPLACE IEND ELEMENTS WITH POINTERS TO IADJ
!
      INDX = 0
      DO 11 IND = 1,KM1
        INDX = INDX + 4
        IF (IND .EQ. NL  .OR.  IND .EQ. NR) INDX = INDX-1
        IEND(IND) = INDX
   11   CONTINUE
      INDX = INDX + K
      IEND(K) = INDX
!
! ADD THE REMAINING NODES TO THE TRIANGULATION
!
      IF (K .EQ. NN) RETURN
      KMIN = K+1
      DO 12 K = KMIN,NN
        CALL ADNODE(K,X,Y, IADJ,IEND, IERR)
   12   CONTINUE
      RETURN
!
! ALL NODES ARE COLLINEAR
!
   13 IER = 2
      RETURN
      END
!==============================================================================
! SUBROUTINE TRPRNT
!==============================================================================
      SUBROUTINE TRPRNT (N,LUNIT,X,Y,IADJ,IEND,IFLAG)
      INTEGER N, LUNIT, IADJ(1), IEND(N), IFLAG
      REAL    X(N), Y(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
! THIS ROUTINE PRINTS THE ADJACENCY LISTS AND, OPTIONALLY,
! THE NODAL COORDINATES.  THE NUMBERS OF BOUNDARY NODES,
! TRIANGLES, AND ARCS ARE ALSO PRINTED.
!
! INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
!                            3 .LE. N .LE. 9999.
!
!                    LUNIT - LOGICAL UNIT FOR OUTPUT.  1
!                            .LE. LUNIT .LE. 99.  OUTPUT IS
!                            PRINTED ON UNIT 6 IF LUNIT IS
!                            OUT OF RANGE.
!
!                      X,Y - VECTORS OF COORDINATES OF THE
!                            NODES IN THE MESH.  NOT USED
!                            UNLESS IFLAG = 0.
!
!                     IADJ - SET OF ADJACENCY LISTS OF NODES
!                            IN THE MESH.
!
!                     IEND - POINTERS TO THE ENDS OF
!                            ADJACENCY LISTS IN IADJ FOR
!                            EACH NODE IN THE MESH.
!
!                    IFLAG - OPTION INDICATOR
!                            IFLAG = 0 IF X AND Y ARE TO BE
!                                      PRINTED (TO 6 DECIMAL
!                                      PLACES).
!                            IFLAG = 1 IF X AND Y ARE NOT
!                                      TO BE PRINTED.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH.
!
! NONE OF THE PARAMETERS ARE ALTERED BY THIS ROUTINE.
!
! MODULES REFERENCED BY TRPRNT - NONE
!
!***********************************************************
!
      INTEGER NN, NMAX, LUN, NODE, INDF, INDL, NL, NLMAX, &
              INC, I, NB, NT, NA
      DATA    NMAX/9999/, NLMAX/60/
!
! LOCAL PARAMETERS -
!
! NN =        LOCAL COPY OF N
! NMAX =      UPPER BOUND ON N
! LUN =       LOCAL COPY OF LUNIT
! NODE =      INDEX OF A NODE
! INDF,INDL = IADJ INDICES OF THE FIRST AND LAST NEIGHBORS
!               OF NODE
! NL =        NUMBER OF LINES PRINTED ON A PAGE
! NLMAX =     MAXIMUM NUMBER OF PRINT LINES PER PAGE EXCEPT
!               FOR THE LAST PAGE WHICH HAS 3 ADDITIONAL
!               LINES
! INC =       INCREMENT FOR NL
! I =         IADJ INDEX FOR IMPLIED DO-LOOP
! NB =        NUMBER OF BOUNDARY NODES
! NT =        NUMBER OF TRIANGLES
! NA =        NUMBER OF ARCS (UNDIRECTED EDGES)
!
      NN = N
      LUN = LUNIT
      IF (LUN .LT. 1  .OR.  LUN .GT. 99) LUN = 6
!
! PRINT HEADING AND INITIALIZE NL
!
      WRITE (LUN,100) NN
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) GO TO 5
      NL = 6
      IF (IFLAG .EQ. 0) GO TO 2
!
! PRINT IADJ ONLY
!
      WRITE (LUN,101)
      NB = 0
      INDF = 1
      DO 1 NODE = 1,NN
        INDL = IEND(NODE)
        IF (IADJ(INDL) .EQ. 0) NB = NB + 1
        INC = (INDL - INDF)/14 + 2
        NL = NL + INC
        IF (NL .GT. NLMAX) WRITE (LUN,106)
        IF (NL .GT. NLMAX) NL = INC
        WRITE (LUN,103) NODE, (IADJ(I), I = INDF,INDL)
        IF (INDL-INDF .NE. 13) WRITE (LUN,105)
        INDF = INDL + 1
    1   CONTINUE
      GO TO 4
!
! PRINT X, Y, AND IADJ
!
    2 WRITE (LUN,102)
      NB = 0
      INDF = 1
      DO 3 NODE = 1,NN
        INDL = IEND(NODE)
        IF (IADJ(INDL) .EQ. 0) NB = NB + 1
        INC = (INDL - INDF)/8 + 2
        NL = NL + INC
        IF (NL .GT. NLMAX) WRITE (LUN,106)
        IF (NL .GT. NLMAX) NL = INC
        WRITE (LUN,104) NODE, X(NODE), Y(NODE),(IADJ(I), I = INDF,INDL)
        IF (INDL-INDF .NE. 7) WRITE (LUN,105)
        INDF = INDL + 1
    3   CONTINUE
!
! PRINT NB, NA, AND NT
!
    4 NT = 2*NN - NB - 2
      NA = NT + NN - 1
      WRITE (LUN,107) NB, NA, NT
      RETURN
!
! N IS OUT OF RANGE
!
    5 WRITE (LUN,108)
      RETURN
!
! PRINT FORMATS
!
  100 FORMAT (1H1,26X,23HADJACENCY SETS,    N = ,I5//)
  101 FORMAT (1H ,4HNODE,32X,17HNEIGHBORS OF NODE//)
  102 FORMAT (1H ,4HNODE,5X,7HX(NODE),8X,7HY(NODE),20X,17HNEIGHBORS OF NODE//)
  103 FORMAT (1H ,I4,5X,14I5/(1H ,9X,14I5))
  104 FORMAT (1H ,I4,2E15.6,5X,8I5/(1H ,39X,8I5))
  105 FORMAT (1H )
  106 FORMAT (1H1)
  107 FORMAT (/1H ,5HNB = ,I4,15H BOUNDARY NODES,10X, &
              5HNA = ,I5,5H ARCS,10X,5HNT = ,I5,      &
              10H TRIANGLES)
  108 FORMAT (1H ,10X,25H*** N IS OUT OF RANGE ***)
      END
!==============================================================================
! SUBROUTINE COORDS
!==============================================================================
      SUBROUTINE COORDS (X,Y,X1,X2,X3,Y1,Y2,Y3, R,IER)
      INTEGER IER
      REAL    X, Y, X1, X2, X3, Y1, Y2, Y3, R(3)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE COMPUTES THE THREE BARYCENTRIC COORDINATES
! OF A POINT IN THE PLANE FOR A GIVEN TRIANGLE.
!
! INPUT PARAMETERS - X,Y - X AND Y COORDINATES OF THE POINT
!                          WHOSE BARYCENTRIC COORDINATES ARE
!                          DESIRED.
!
!      X1,X2,X3,Y1,Y2,Y3 - COORDINATES OF THE VERTICES OF
!                          THE TRIANGLE.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS -  R - 3-VECTOR OF BARYCENTRIC COORDI-
!                          NATES UNLESS IER = 1.  NOTE THAT
!                          R(I) .LT. 0. IFF (X,Y) IS TO THE
!                          RIGHT OF THE VECTOR FROM VERTEX
!                          I+1 TO VERTEX I+2 (CYCLICAL
!                          ARITHMETIC).
!
!                    IER - ERROR INDICATOR
!                          IER = 0 IF NO ERRORS WERE
!                                  ENCOUNTERED.
!                          IER = 1 IF THE VERTICES OF THE
!                                  TRIANGLE ARE COLLINEAR.
!
! MODULES REFERENCED BY COORDS - NONE
!
!***********************************************************
!
      REAL    U(3), V(3), AREA, XP, YP
!
! LOCAL PARAMETERS -
!
! U(K),V(K) = X AND Y COMPONENTS OF THE VECTOR REPRESENTING
!               THE SIDE OPPOSITE VERTEX K FOR K = 1,2,3.
! AREA =      TWICE THE AREA OF THE TRIANGLE.
! XP,YP =     X-X1, Y-Y1
!
      U(1) = X3-X2
      U(2) = X1-X3
      U(3) = X2-X1
!
      V(1) = Y3-Y2
      V(2) = Y1-Y3
      V(3) = Y2-Y1
!
! AREA = 3-1 X 3-2
!
      AREA = U(1)*V(2) - U(2)*V(1)
      IF (AREA .EQ. 0.) GO TO 1
!
! R(1) = (2-3 X 2-(X,Y))/AREA, R(2) = (1-(X,Y) X 1-3)/AREA,
!   R(3) = (1-2 X 1-(X,Y))/AREA
!
      R(1) = (U(1)*(Y-Y2) - V(1)*(X-X2))/AREA
      XP = X - X1
      YP = Y - Y1
      R(2) = (U(2)*YP - V(2)*XP)/AREA
      R(3) = (U(3)*YP - V(3)*XP)/AREA
      IER = 0
      RETURN
!
! VERTICES ARE COLLINEAR
!
    1 IER = 1
      RETURN
      END
!==============================================================================
! SUBROUTINE GIVENS
!==============================================================================
      SUBROUTINE GIVENS ( A,B, C,S)
      REAL A, B, C, S
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE CONSTRUCTS THE GIVENS PLANE ROTATION --
!     ( C  S)
! G = (     ) WHERE C*C + S*S = 1 -- WHICH ZEROS THE SECOND
!     (-S  C)
! ENTRY OF THE 2-VECTOR (A B)-TRANSPOSE.  A CALL TO GIVENS
! IS NORMALLY FOLLOWED BY A CALL TO ROTATE WHICH APPLIES
! THE TRANSFORMATION TO A 2 BY N MATRIX.  THIS ROUTINE WAS
! TAKEN FROM LINPACK.
!
! INPUT PARAMETERS - A,B - COMPONENTS OF THE 2-VECTOR TO BE
!                          ROTATED.
!
! OUTPUT PARAMETERS -  A - OVERWRITTEN BY R = +/-SQRT(A*A
!                          + B*B)
!
!                      B - OVERWRITTEN BY A VALUE Z WHICH
!                          ALLOWS C AND S TO BE RECOVERED
!                          AS FOLLOWS -
!                          C = SQRT(1-Z*Z), S=Z IF ABS(Z)
!                              .LE. 1.
!                          C = 1/Z, S = SQRT(1-C*C) IF
!                              ABS(Z) .GT. 1.
!
!                      C - +/-(A/R)
!
!                      S - +/-(B/R)
!
! MODULES REFERENCED BY GIVENS - NONE
!
! INTRINSIC FUNCTIONS CALLED BY GIVENS - ABS, SQRT
!
!***********************************************************
!
      REAL AA, BB, R, U, V
!
! LOCAL PARAMETERS -
!
! AA,BB = LOCAL COPIES OF A AND B
! R =     C*A + S*B = +/-SQRT(A*A+B*B)
! U,V =   VARIABLES USED TO SCALE A AND B FOR COMPUTING R
!
      AA = A
      BB = B
      IF (ABS(AA) .LE. ABS(BB)) GO TO 1
!
! ABS(A) .GT. ABS(B)
!
      U = AA + AA
      V = BB/U
      R = SQRT(.25 + V*V) * U
      C = AA/R
      S = V * (C + C)
!
! NOTE THAT R HAS THE SIGN OF A, C .GT. 0, AND S HAS
!   SIGN(A)*SIGN(B)
!
      B = S
      A = R
      RETURN
!
! ABS(A) .LE. ABS(B)
!
    1 IF (BB .EQ. 0.) GO TO 2
      U = BB + BB
      V = AA/U
!
! STORE R IN A
!
      A = SQRT(.25 + V*V) * U
      S = BB/A
      C = V * (S + S)
!
! NOTE THAT R HAS THE SIGN OF B, S .GT. 0, AND C HAS
!   SIGN(A)*SIGN(B)
!
      B = 1.
      IF (C .NE. 0.) B = 1./C
      RETURN
!
! A = B = 0.
!
    2 C = 1.
      S = 0.
      RETURN
      END
!==============================================================================
! SUBROUTINE GRADG
!==============================================================================
      SUBROUTINE GRADG (N,X,Y,Z,IADJ,IEND,EPS, NIT, ZXZY, IER)
      INTEGER N, IADJ(1), IEND(N), NIT, IER
      REAL    X(N), Y(N), Z(N), EPS, ZXZY(2,N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF N NODES IN THE PLANE WITH
! ASSOCIATED DATA VALUES, THIS ROUTINE USES A GLOBAL METHOD
! TO COMPUTE ESTIMATED GRADIENTS AT THE NODES.  THE METHOD
! CONSISTS OF MINIMIZING A QUADRATIC FUNCTIONAL Q(G) OVER
! THE N-VECTOR G OF GRADIENTS WHERE Q APPROXIMATES THE LIN-
! EARIZED CURVATURE OF AN INTERPOLANT F OVER THE TRIANGULA-
! TION.  THE RESTRICTION OF F TO AN ARC OF THE TRIANGULATION
! IS TAKEN TO BE THE HERMITE CUBIC INTERPOLANT OF THE DATA
! VALUES AND TANGENTIAL GRADIENT COMPONENTS AT THE END-
! POINTS OF THE ARC, AND Q IS THE SUM OF THE LINEARIZED
! CURVATURES OF F ALONG THE ARCS -- THE INTEGRALS OVER THE
! ARCS OF D2F(T)**2 WHERE D2F(T) IS THE SECOND DERIVATIVE
! OF F WITH RESPECT TO DISTANCE T ALONG THE ARC.  THIS MIN-
! IMIZATION PROBLEM CORRESPONDS TO AN ORDER 2N SYMMETRIC
! POSITIVE-DEFINITE SPARSE LINEAR SYSTEM WHICH IS SOLVED FOR
! THE X AND Y PARTIAL DERIVATIVES BY THE BLOCK GAUSS-SEIDEL
! METHOD WITH 2 BY 2 BLOCKS.
!   AN ALTERNATIVE METHOD, SUBROUTINE GRADL, COMPUTES A
! LOCAL APPROXIMATION TO THE PARTIALS AT A SINGLE NODE AND
! MAY BE MORE ACCURATE, DEPENDING ON THE DATA VALUES AND
! DISTRIBUTION OF NODES (NEITHER METHOD EMERGED AS SUPERIOR
! IN TESTS FOR ACCURACY).  HOWEVER, IN TESTS RUN ON AN IBM
! 370, GRADG WAS FOUND TO BE ABOUT 3.6 TIMES AS FAST FOR
! NIT = 4.
!
! INPUT PARAMETERS - N - NUMBER OF NODES.  N .GE. 3.
!
!                  X,Y - CARTESIAN COORDINATES OF THE NODES.
!
!                    Z - DATA VALUES AT THE NODES.  Z(I) IS
!                        ASSOCIATED WITH (X(I),Y(I)).
!
!            IADJ,IEND - DATA STRUCTURE DEFINING THE TRIAN-
!                        GULATION.  SEE SUBROUTINE TRMESH.
!
!                  EPS - NONNEGATIVE CONVERGENCE CRITERION.
!                        THE METHOD IS TERMINATED WHEN THE
!                        MAXIMUM CHANGE IN A GRADIENT COMPO-
!                        NENT BETWEEN ITERATIONS IS AT MOST
!                        EPS.  EPS = 1.E-2 IS SUFFICIENT FOR
!                        EFFECTIVE CONVERGENCE.
!
! THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
!                  NIT - MAXIMUM NUMBER OF GAUSS-SEIDEL
!                        ITERATIONS TO BE APPLIED.  THIS
!                        MAXIMUM WILL LIKELY BE ACHIEVED IF
!                        EPS IS SMALLER THAN THE MACHINE
!                        PRECISION.  OPTIMAL EFFICIENCY WAS
!                        ACHIEVED IN TESTING WITH EPS = 0
!                        AND NIT = 3 OR 4.
!
!                 ZXZY - 2 BY N ARRAY WHOSE COLUMNS CONTAIN
!                        INITIAL ESTIMATES OF THE PARTIAL
!                        DERIVATIVES (ZERO VECTORS ARE
!                        SUFFICIENT).
!
! OUTPUT PARAMETERS - NIT - NUMBER OF GAUSS-SEIDEL ITERA-
!                           TIONS EMPLOYED.
!
!                    ZXZY - ESTIMATED X AND Y PARTIAL DERIV-
!                           ATIVES AT THE NODES WITH X PAR-
!                           TIALS IN THE FIRST ROW.  ZXZY IS
!                           NOT CHANGED IF IER = 2.
!
!                     IER - ERROR INDICATOR
!                           IER = 0 IF THE CONVERGENCE CRI-
!                                   TERION WAS ACHIEVED.
!                           IER = 1 IF CONVERGENCE WAS NOT
!                                   ACHIEVED WITHIN NIT
!                                   ITERATIONS.
!                           IER = 2 IF N OR EPS IS OUT OF
!                                   RANGE OR NIT .LT. 0 ON
!                                   INPUT.
!
! MODULES REFERENCED BY GRADG - NONE
!
! INTRINSIC FUNCTIONS CALLED BY GRADG - SQRT, AMAX1, ABS
!
!***********************************************************
!
      INTEGER NN, MAXIT, ITER, K, INDF, INDL, INDX, NB
      REAL    TOL, DGMAX, XK, YK, ZK, ZXK, ZYK, A11, A12,  &
              A22, R1, R2, DELX, DELY, DELXS, DELYS, DSQ,  &
              DCUB, T, DZX, DZY
!
! LOCAL PARAMETERS -
!
! NN =          LOCAL COPY OF N
! MAXIT =       INPUT VALUE OF NIT
! ITER =        NUMBER OF ITERATIONS USED
! K =           DO-LOOP AND NODE INDEX
! INDF,INDL =   IADJ INDICES OF THE FIRST AND LAST NEIGHBORS
!                 OF K
! INDX =        IADJ INDEX IN THE RANGE INDF,...,INDL
! NB =          NEIGHBOR OF K
! TOL =         LOCAL COPY OF EPS
! DGMAX =       MAXIMUM CHANGE IN A GRADIENT COMPONENT BE-
!                 TWEEN ITERATIONS
! XK,YK,ZK =    X(K), Y(K), Z(K)
! ZXK,ZYK =     INITIAL VALUES OF ZXZY(1,K) AND ZXZY(2,K)
! A11,A12,A22 = MATRIX COMPONENTS OF THE 2 BY 2 BLOCK A*DG
!                 = R WHERE A IS SYMMETRIC, DG = (DZX,DZY)
!                 IS THE CHANGE IN THE GRADIENT AT K, AND R
!                 IS THE RESIDUAL
! R1,R2 =       COMPONENTS OF THE RESIDUAL -- DERIVATIVES OF
!                 Q WITH RESPECT TO THE COMPONENTS OF THE
!                 GRADIENT AT NODE K
! DELX,DELY =   COMPONENTS OF THE ARC NB-K
! DELXS,DELYS = DELX**2, DELY**2
! DSQ =         SQUARE OF THE DISTANCE D BETWEEN K AND NB
! DCUB =        D**3
! T =           FACTOR OF R1 AND R2
! DZX,DZY =     SOLUTION OF THE 2 BY 2 SYSTEM -- CHANGE IN
!                 DERIVATIVES AT K FROM THE PREVIOUS ITERATE
!
      NN = N
      TOL = EPS
      MAXIT = NIT
!
! ERROR CHECKS AND INITIALIZATION
!
      IF (NN .LT. 3  .OR.  TOL .LT. 0.  .OR.  MAXIT .LT. 0)GO TO 5
      ITER = 0
!
! TOP OF ITERATION LOOP
!
    1 IF (ITER .EQ. MAXIT) GO TO 4
      DGMAX = 0.
      INDL = 0
      DO 3 K = 1,NN
        XK = X(K)
        YK = Y(K)
        ZK = Z(K)
        ZXK = ZXZY(1,K)
        ZYK = ZXZY(2,K)
!
!   INITIALIZE COMPONENTS OF THE 2 BY 2 SYSTEM
!
        A11 = 0.
        A12 = 0.
        A22 = 0.
        R1 = 0.
        R2 = 0.
!
!   LOOP ON NEIGHBORS NB OF K
!
        INDF = INDL + 1
        INDL = IEND(K)
        DO 2 INDX = INDF,INDL
          NB = IADJ(INDX)
          IF (NB .EQ. 0) GO TO 2
!
!   COMPUTE THE COMPONENTS OF ARC NB-K
!
          DELX = X(NB) - XK
          DELY = Y(NB) - YK
          DELXS = DELX*DELX
          DELYS = DELY*DELY
          DSQ = DELXS + DELYS
          DCUB = DSQ*SQRT(DSQ)
!
!   UPDATE THE SYSTEM COMPONENTS FOR NODE NB
!
          A11 = A11 + DELXS/DCUB
          A12 = A12 + DELX*DELY/DCUB
          A22 = A22 + DELYS/DCUB
          T = ( 1.5*(Z(NB)-ZK) - ((ZXZY(1,NB)/2.+ZXK)*DELX + (ZXZY(2,NB)/2.+ZYK)*DELY) )/DCUB
          R1 = R1 + T*DELX
          R2 = R2 + T*DELY
    2     CONTINUE
!
!   SOLVE THE 2 BY 2 SYSTEM AND UPDATE DGMAX
!
        DZY = (A11*R2 - A12*R1)/(A11*A22 - A12*A12)
        DZX = (R1 - A12*DZY)/A11
        DGMAX = AMAX1(DGMAX,ABS(DZX),ABS(DZY))
!
!   UPDATE THE PARTIALS AT NODE K
!
        ZXZY(1,K) = ZXK + DZX
    3   ZXZY(2,K) = ZYK + DZY
!
!   INCREMENT ITER AND TEST FOR CONVERGENCE
!
      ITER = ITER + 1
      IF (DGMAX .GT. TOL) GO TO 1
!
! METHOD CONVERGED
!
      NIT = ITER
      IER = 0
      RETURN
!
! METHOD FAILED TO CONVERGE WITHIN NIT ITERATIONS
!
    4 IER = 1
      RETURN
!
! PARAMETER OUT OF RANGE
!
    5 NIT = 0
      IER = 2
      RETURN
      END
!==============================================================================
! SUBROUTINE GRADL
!==============================================================================
      SUBROUTINE GRADL (N,K,X,Y,Z,IADJ,IEND, DX,DY,IER)
      INTEGER N, K, IADJ(1), IEND(N), IER
      REAL    X(N), Y(N), Z(N), DX, DY
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A THIESSEN TRIANGULATION OF N POINTS IN THE PLANE
! WITH ASSOCIATED DATA VALUES Z, THIS SUBROUTINE ESTIMATES
! X AND Y PARTIAL DERIVATIVES AT NODE K.  THE DERIVATIVES
! ARE TAKEN TO BE THE PARTIALS AT K OF A QUADRATIC FUNCTION
! WHICH INTERPOLATES Z(K) AND FITS THE DATA VALUES AT A SET
! OF NEARBY NODES IN A WEIGHTED LEAST SQUARES SENSE. A MAR-
! QUARDT STABILIZATION FACTOR IS USED IF NECESSARY TO ENSURE
! A WELL-CONDITIONED SYSTEM AND A LINEAR FITTING FUNCTION IS
! USED IF N .LT. 6.  THUS, A UNIQUE SOLUTION EXISTS UNLESS
! THE NODES ARE COLLINEAR.
!   AN ALTERNATIVE ROUTINE, GRADG, EMPLOYS A GLOBAL METHOD
! TO COMPUTE THE PARTIAL DERIVATIVES AT ALL OF THE NODES AT
! ONCE.  THAT METHOD IS MORE EFFICIENT (WHEN ALL PARTIALS
! ARE NEEDED) AND MAY BE MORE ACCURATE, DEPENDING ON THE
! DATA.
!
! INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
!                        TION.  N .GE. 3.
!
!                    K - NODE AT WHICH DERIVATIVES ARE
!                        SOUGHT.  1 .LE. K .LE. N.
!
!                  X,Y - N-VECTORS CONTAINING THE CARTESIAN
!                        COORDINATES OF THE NODES.
!
!                    Z - N-VECTOR CONTAINING THE DATA VALUES
!                        ASSOCIATED WITH THE NODES.
!
!                 IADJ - SET OF ADJACENCY LISTS.
!
!                 IEND - POINTERS TO THE ENDS OF ADJACENCY
!                        LISTS FOR EACH NODE.
!
! IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS - DX,DY - ESTIMATED PARTIAL DERIVATIVES
!                             AT NODE K UNLESS IER .LT. 0.
!
!                       IER - ERROR INDICATOR
!                             IER .GT. 0 IF NO ERRORS WERE
!                                      ENCOUNTERED.  IER
!                                      CONTAINS THE NUMBER
!                                      OF NODES (INCLUDING
!                                      K) USED IN THE FIT.
!                                      IER = 3, 4, OR 5 IM-
!                                      PLIES A LINEAR FIT.
!                             IER = -1 IF N OR K IS OUT OF
!                                      RANGE.
!                             IER = -2 IF ALL NODES ARE
!                                      COLLINEAR.
!
! MODULES REFERENCED BY GRADL - GETNP, SETUP, GIVENS,
!                               ROTATE
!
! INTRINSIC FUNCTIONS CALLED BY GRADL - MIN0, FLOAT, SQRT,
!                                       AMIN1, ABS
!
!***********************************************************
!
      INTEGER NN, KK, LMN, LMX, LMIN, LMAX, LM1, LNP,     &
              NPTS(30), IERR, NP, I, J, IM1, JP1, IP1, L
      REAL    SUM, DS, R, RS, RTOL, AVSQ, AV, XK, YK, ZK, &
              A(6,6), C, S, DMIN, DTOL, SF
      DATA    LMN/10/
      DATA    LMX/30/, RTOL/1.E-5/, DTOL/.01/, SF/1./
!
! LOCAL PARAMETERS -
!
! NN,KK =     LOCAL COPIES OF N AND K
! LMN,LMX =   MINIMUM AND MAXIMUM VALUES OF LNP FOR N
!               SUFFICIENTLY LARGE.  IN MOST CASES LMN-1
!               NODES ARE USED IN THE FIT.  4 .LE. LMN .LE.
!               LMX.
! LMIN,LMAX = MIN(LMN,N), MIN(LMX,N)
! LM1 =       LMIN-1 OR LNP-1
! LNP =       LENGTH OF NPTS
! NPTS =      ARRAY CONTAINING THE INDICES OF A SEQUENCE OF
!               NODES ORDERED BY DISTANCE FROM K.  NPTS(1)=K
!               AND THE FIRST LNP-1 ELEMENTS OF NPTS ARE
!               USED IN THE LEAST SQUARES FIT.  UNLESS LNP
!               EXCEEDS LMAX, NPTS(LNP) DETERMINES R.
! IERR =      ERROR FLAG FOR CALLS TO GETNP (NOT CHECKED)
! NP =        ELEMENT OF NPTS TO BE ADDED TO THE SYSTEM
! I,J =       DO-LOOP INDICES
! IM1,JP1 =   I-1, J+1
! IP1 =       I+1
! L =         NUMBER OF COLUMNS OF A**T TO WHICH A ROTATION
!               IS APPLIED
! SUM =       SUM OF SQUARED EUCLIDEAN DISTANCES BETWEEN
!               NODE K AND THE NODES USED IN THE LEAST
!               SQUARES FIT
! DS =        SQUARED DISTANCE BETWEEN NODE K AND AN ELE-
!               MENT OF NPTS
! R =         DISTANCE BETWEEN NODE K AND NPTS(LNP) OR SOME
!               POINT FURTHER FROM K THAN NPTS(LMAX) IF
!               NPTS(LMAX) IS USED IN THE FIT.  R IS A
!               RADIUS OF INFLUENCE WHICH ENTERS INTO THE
!               WEIGHTS (SEE SUBROUTINE SETUP).
! RS =        R*R
! RTOL =      TOLERANCE FOR DETERMINING R.  IF THE RELATIVE
!               CHANGE IN DS BETWEEN TWO ELEMENTS OF NPTS IS
!               NOT GREATER THAN RTOL THEY ARE TREATED AS
!               BEING THE SAME DISTANCE FROM NODE K
! AVSQ =      AV*AV
! AV =        ROOT-MEAN-SQUARE DISTANCE BETWEEN K AND THE
!               NODES (OTHER THAN K) IN THE LEAST SQUARES
!               FIT.  THE FIRST 3 COLUMNS OF THE SYSTEM ARE
!               SCALED BY 1/AVSQ, THE NEXT 2 BY 1/AV.
! XK,YK,ZK =  COORDINATES AND DATA VALUE ASSOCIATED WITH K
! A =         TRANSPOSE OF THE AUGMENTED REGRESSION MATRIX
! C,S =       COMPONENTS OF THE PLANE ROTATION DETERMINED
!               BY SUBROUTINE GIVENS
! DMIN =      MINIMUM OF THE MAGNITUDES OF THE DIAGONAL
!               ELEMENTS OF THE REGRESSION MATRIX AFTER
!               ZEROS ARE INTRODUCED BELOW THE DIAGONAL
! DTOL =      TOLERANCE FOR DETECTING AN ILL-CONDITIONED
!               SYSTEM.  THE SYSTEM IS ACCEPTED WHEN DMIN
!               .GE. DTOL
! SF =        MARQUARDT STABILIZATION FACTOR USED TO DAMP
!               OUT THE FIRST 3 SOLUTION COMPONENTS (SECOND
!               PARTIALS OF THE QUADRATIC) WHEN THE SYSTEM
!               IS ILL-CONDITIONED.  AS SF INCREASES, THE
!               FITTING FUNCTION APPROACHES A LINEAR
!
      NN = N
      KK = K
!
! CHECK FOR ERRORS AND INITIALIZE LMIN, LMAX
!
      IF (NN .LT. 3  .OR.  KK .LT. 1  .OR.  KK .GT. NN)GO TO 16
      LMIN = MIN0(LMN,NN)
      LMAX = MIN0(LMX,NN)
!
! COMPUTE NPTS, LNP, AVSQ, AV, AND R.
!   SET NPTS TO THE CLOSEST LMIN-1 NODES TO K.
!
      SUM = 0.
      NPTS(1) = KK
      LM1 = LMIN - 1
      DO 1 LNP = 2,LM1
        CALL GETNP (X,Y,IADJ,IEND,LNP, NPTS, DS,IERR)
    1   SUM = SUM + DS
!
! ADD ADDITIONAL NODES TO NPTS UNTIL THE RELATIVE INCREASE
!   IN DS IS AT LEAST RTOL.
!
      DO 2 LNP = LMIN,LMAX
        CALL GETNP (X,Y,IADJ,IEND,LNP, NPTS, RS,IERR)
        IF ((RS-DS)/DS .LE. RTOL) GO TO 2
        IF (LNP .GT. 6) GO TO 3
    2   SUM = SUM + RS
!
! USE ALL LMAX NODES IN THE LEAST SQUARES FIT.  RS IS
!   ARBITRARILY INCREASED BY 10 PER CENT.
!
      RS = 1.1*RS
      LNP = LMAX + 1
!
! THERE ARE LNP-2 EQUATIONS CORRESPONDING TO NODES NPTS(2),
!   ...,NPTS(LNP-1).
!
    3 AVSQ = SUM/FLOAT(LNP-2)
      AV = SQRT(AVSQ)
      R = SQRT(RS)
      XK = X(KK)
      YK = Y(KK)
      ZK = Z(KK)
      IF (LNP .LT. 7) GO TO 12
!
! SET UP THE FIRST 5 EQUATIONS OF THE AUGMENTED REGRESSION
!   MATRIX (TRANSPOSED) AS THE COLUMNS OF A, AND ZERO OUT
!   THE LOWER TRIANGLE (UPPER TRIANGLE OF A) WITH GIVENS
!   ROTATIONS
!
      DO 5 I = 1,5
        NP = NPTS(I+1)
        CALL SETUP (XK,YK,ZK,X(NP),Y(NP),Z(NP),AV,AVSQ, R, A(1,I))
        IF (I .EQ. 1) GO TO 5
        IM1 = I - 1
        DO 4 J = 1,IM1
          JP1 = J + 1
          L = 6 - J
          CALL GIVENS (A(J,J),A(J,I),C,S)
    4     CALL ROTATE (L,C,S,A(JP1,J),A(JP1,I))
    5   CONTINUE
!
! ADD THE ADDITIONAL EQUATIONS TO THE SYSTEM USING
!   THE LAST COLUMN OF A -- I .LE. LNP.
!
      I = 7
    6   IF (I .EQ. LNP) GO TO 8
        NP = NPTS(I)
        CALL SETUP (XK,YK,ZK,X(NP),Y(NP),Z(NP),AV,AVSQ, R, A(1,6))
        DO 7 J = 1,5
          JP1 = J + 1
          L = 6 - J
          CALL GIVENS (A(J,J),A(J,6),C,S)
    7     CALL ROTATE (L,C,S,A(JP1,J),A(JP1,6))
        I = I + 1
        GO TO 6
!
! TEST THE SYSTEM FOR ILL-CONDITIONING
!
    8 DMIN = AMIN1( ABS(A(1,1)),ABS(A(2,2)),ABS(A(3,3)),ABS(A(4,4)),ABS(A(5,5)) )
      IF (DMIN .GE. DTOL) GO TO 15
      IF (LNP .GT. LMAX) GO TO 9
!
! ADD ANOTHER NODE TO THE SYSTEM AND INCREASE R --
!   I .EQ. LNP
!
      LNP = LNP + 1
      IF (LNP .LE. LMAX) CALL GETNP (X,Y,IADJ,IEND,LNP, NPTS, RS,IERR)
      R = SQRT(1.1*RS)
      GO TO 6
!
! STABILIZE THE SYSTEM BY DAMPING SECOND PARTIALS --ADD
!   MULTIPLES OF THE FIRST THREE UNIT VECTORS TO THE FIRST
!   THREE EQUATIONS.
!
    9 DO 11 I = 1,3
        A(I,6) = SF
        IP1 = I + 1
        DO 10 J = IP1,6
   10     A(J,6) = 0.
        DO 11 J = I,5
          JP1 = J + 1
          L = 6 - J
          CALL GIVENS (A(J,J),A(J,6),C,S)
   11     CALL ROTATE (L,C,S,A(JP1,J),A(JP1,6))
      GO TO 14
!
! 4 .LE. LNP .LE. 6 (2, 3, OR 4 EQUATIONS) -- FIT A PLANE TO
!   THE DATA USING THE LAST 3 COLUMNS OF A.
!
   12 NP = NPTS(2)
      CALL SETUP (XK,YK,ZK,X(NP),Y(NP),Z(NP),AV,AVSQ, R, A(1,4))
      NP = NPTS(3)
      CALL SETUP (XK,YK,ZK,X(NP),Y(NP),Z(NP),AV,AVSQ, R, A(1,5))
      CALL GIVENS (A(4,4),A(4,5),C,S)
      CALL ROTATE (2,C,S,A(5,4),A(5,5))
      IF (LNP .EQ. 4) GO TO 14
!
      LM1 = LNP - 1
      DO 13 I = 4,LM1
        NP = NPTS(I)
        CALL SETUP (XK,YK,ZK,X(NP),Y(NP),Z(NP),AV,AVSQ, R, A(1,6))
        CALL GIVENS (A(4,4),A(4,6),C,S)
        CALL ROTATE (2,C,S,A(5,4),A(5,6))
        CALL GIVENS (A(5,5),A(5,6),C,S)
   13   CALL ROTATE (1,C,S,A(6,5),A(6,6))
!
! TEST THE LINEAR FIT FOR ILL-CONDITIONING
!
   14 DMIN = AMIN1( ABS(A(4,4)),ABS(A(5,5)) )
      IF (DMIN .LT. DTOL) GO TO 17
!
! SOLVE THE 2 BY 2 TRIANGULAR SYSTEM FOR THE DERIVATIVES
!
   15 DY = A(6,5)/A(5,5)
      DX = (A(6,4) - A(5,4)*DY)/A(4,4)/AV
      DY = DY/AV
      IER = LNP - 1
      RETURN
!
! N OR K IS OUT OF RANGE
!
   16 IER = -1
      RETURN
!
! NO UNIQUE SOLUTION DUE TO COLLINEAR NODES
!
   17 IER = -2
      RETURN
      END
!==============================================================================
! SUBROUTINE INTRC0
!==============================================================================
      SUBROUTINE INTRC0 (N,PX,PY,X,Y,Z,IADJ,IEND, IST, PZ, IER)
      INTEGER N, IADJ(1), IEND(N), IST, IER
      REAL    PX, PY, X(N), Y(N), Z(N), PZ
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
! THIS ROUTINE COMPUTES THE VALUE AT (PX,PY) OF A PIECEWISE
! LINEAR SURFACE WHICH INTERPOLATES DATA VALUES AT THE
! VERTICES OF THE TRIANGLES.  THE SURFACE IS EXTENDED IN A
! CONTINUOUS FASHION BEYOND THE BOUNDARY OF THE TRIANGULAR
! MESH, ALLOWING EXTRAPOLATION.  INTRC0 IS PART OF AN
! INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES TO GENERATE,
! UPDATE, AND PLOT THE MESH.
!
! INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
!                            N .GE. 3.
!
!                    PX,PY - POINT AT WHICH THE INTERPOLATED
!                            VALUE IS DESIRED.
!
!                      X,Y - VECTORS OF COORDINATES OF THE
!                            NODES IN THE MESH.
!
!                        Z - VECTOR OF DATA VALUES AT THE
!                            NODES.
!
!                     IADJ - SET OF ADJACENCY LISTS OF NODES
!                            IN THE MESH.
!
!                     IEND - POINTERS TO THE ENDS OF
!                            ADJACENCY LISTS IN IADJ FOR
!                            EACH NODE IN THE MESH.
!
!                      IST - INDEX OF THE STARTING NODE IN
!                            THE SEARCH FOR A TRIANGLE CON-
!                            TAINING (PX,PY).  1 .LE. IST
!                            .LE. N.  THE OUTPUT VALUE OF
!                            IST FROM A PREVIOUS CALL MAY
!                            BE A GOOD CHOICE.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH.
!
! INPUT PARAMETERS OTHER THAN IST ARE NOT ALTERED BY THIS
!   ROUTINE.
!
! OUTPUT PARAMETERS -  IST - INDEX OF ONE OF THE VERTICES OF
!                            THE TRIANGLE CONTAINING (PX,PY)
!                            UNLESS IER .LT. 0.
!
!                       PZ - VALUE OF THE INTERPOLATORY
!                            SURFACE AT (PX,PY) OR ZERO
!                            IF IER .LT. 0.
!
!                      IER - ERROR INDICATOR
!                            IER = 0 IF NO ERRORS WERE
!                                    ENCOUNTERED.
!                            IER = 1 IF NO ERRORS WERE EN-
!                                    COUNTERED AND EXTRAPO-
!                                    LATION WAS PERFORMED.
!                            IER = -1 IF N OR IST IS OUT OF
!                                     RANGE.
!                            IER = -2 IF THE NODES ARE COL-
!                                     LINEAR.
!
! MODULES REFERENCED BY INTRC0 - TRFIND, COORDS
!
!***********************************************************
!
      INTEGER I1, I2, I3, N1, N2, INDX
      REAL    XP, YP, R(3), X1, Y1, X2, Y2, DP
!
! LOCAL PARAMETERS -
!
! I1,I2,I3 = VERTEX INDICES RETURNED BY TRFIND
! N1,N2 =    ENDPOINTS OF THE CLOSEST BOUNDARY EDGE TO P
!              WHEN P IS OUTSIDE OF THE MESH BOUNDARY
! INDX =     IADJ INDEX OF N1 AS A NEIGHBOR OF N2
! XP,YP =    LOCAL COPIES OF THE COORDINATES OF P=(PX,PY)
! R =        BARYCENTRIC COORDINATES
! X1,Y1 =    X,Y COORDINATES OF N1
! X2,Y2 =    X,Y COORDINATES OF N2
! DP =       INNER PRODUCT OF N1-N2 AND P-N2
!
      IF (N .LT. 3  .OR.  IST .LT. 1  .OR.  IST .GT. N)GO TO 5
      XP = PX
      YP = PY
!
! FIND A TRIANGLE CONTAINING P IF P IS WITHIN THE MESH
!   BOUNDARY
!
      CALL TRFIND(IST,XP,YP,X,Y,IADJ,IEND, I1,I2,I3)
      IF (I1 .EQ. 0) GO TO 6
      IST = I1
      IF (I3 .EQ. 0) GO TO 1
!
! COMPUTE BARYCENTRIC COORDINATES
!
      CALL COORDS(XP,YP,X(I1),X(I2),X(I3),Y(I1),Y(I2), Y(I3), R,IER)
      IF (IER .NE. 0) GO TO 6
      PZ = R(1)*Z(I1) + R(2)*Z(I2) + R(3)*Z(I3)
      RETURN
!
! P IS OUTSIDE OF THE MESH BOUNDARY.  EXTRAPOLATE TO P BY
!   EXTENDING THE INTERPOLATORY SURFACE AS A CONSTANT
!   BEYOND THE BOUNDARY.  THUS PZ IS THE SURFACE FUNCTION
!   VALUE AT Q WHERE Q IS THE CLOSEST BOUNDARY POINT TO P.
!
! DETERMINE Q BY TRAVERSING THE BOUNDARY STARTING FROM THE
!   RIGHTMOST VISIBLE NODE I1.
!
    1 N2 = I1
!
! SET N1 TO THE LAST NONZERO NEIGHBOR OF N2 AND COMPUTE DP
!
    2 INDX = IEND(N2) - 1
      N1 = IADJ(INDX)
      X1 = X(N1)
      Y1 = Y(N1)
      X2 = X(N2)
      Y2 = Y(N2)
      DP = (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2)
      IF (DP .LE. 0.) GO TO 3
      IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) .GT. 0.) GO TO 4
      N2 = N1
      GO TO 2
!
! N2 IS THE CLOSEST BOUNDARY POINT TO P
!
    3 PZ = Z(N2)
      IER = 1
      RETURN
!
! THE CLOSEST BOUNDARY POINT TO P LIES ON N2-N1.  COMPUTE
!   ITS COORDINATES WITH RESPECT TO N2-N1.
!
    4 R(1) = DP/( (X2-X1)**2 + (Y2-Y1)**2 )
      R(2) = 1. - R(1)
      PZ = R(1)*Z(N1) + R(2)*Z(N2)
      IER = 1
      RETURN
!
! N .LT. 3 OR IST IS OUT OF RANGE
!
    5 PZ = 0.
      IER = -1
      RETURN
!
! NODES ARE COLLINEAR
!
    6 PZ = 0.
      IER = -2
      RETURN
      END
!==============================================================================
! SUBROUTINE INTRC1
!==============================================================================
      SUBROUTINE INTRC1 (N,PX,PY,X,Y,Z,IADJ,IEND,IFLAG, ZXZY, IST, PZ,IER)
      INTEGER N, IADJ(1), IEND(N), IFLAG, IST, IER
      REAL    PX, PY, X(N), Y(N), Z(N), ZXZY(2,N), PZ
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
! THIS ROUTINE DETERMINES A PIECEWISE CUBIC FUNCTION F(X,Y)
! WHICH INTERPOLATES A SET OF DATA VALUES AND PARTIAL
! DERIVATIVES AT THE VERTICES.  F HAS CONTINUOUS FIRST
! DERIVATIVES OVER THE MESH AND EXTENDS BEYOND THE MESH
! BOUNDARY ALLOWING EXTRAPOLATION.  INTERPOLATION IS EXACT
! FOR QUADRATIC DATA.  THE VALUE OF F AT (PX,PY) IS
! RETURNED.  INTRC1 IS PART OF AN INTERPOLATION PACKAGE
! WHICH PROVIDES ROUTINES TO GENERATE, UPDATE AND PLOT THE
! MESH.
!
! INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
!                            N .GE. 3.
!
!                    PX,PY - COORDINATES OF A POINT AT WHICH
!                            F IS TO BE EVALUATED.
!
!                      X,Y - VECTORS OF COORDINATES OF THE
!                            NODES IN THE MESH.
!
!                        Z - VECTOR OF DATA VALUES AT THE
!                            NODES.
!
!                     IADJ - SET OF ADJACENCY LISTS OF NODES
!                            IN THE MESH.
!
!                     IEND - POINTERS TO THE ENDS OF
!                            ADJACENCY LISTS IN IADJ FOR
!                            EACH NODE IN THE MESH.
!
!                    IFLAG - OPTION INDICATOR
!                            IFLAG = 0 IF INTRC1 IS TO
!                                      PROVIDE DERIVATIVE
!                                      ESTIMATES (FROM
!                                      GRADL).
!                            IFLAG = 1 IF DERIVATIVES ARE
!                                      USER PROVIDED.
!
!                     ZXZY - 2 BY N ARRAY WHOSE COLUMNS
!                            CONTAIN ESTIMATED PARTIAL DER-
!                            IVATIVES AT THE NODES (X PAR-
!                            TIALS IN THE FIRST ROW) IF
!                            IFLAG = 1, NOT USED IF IFLAG
!                            = 0.
!
!                      IST - INDEX OF THE STARTING NODE IN
!                            THE SEARCH FOR A TRIANGLE CON-
!                            TAINING (PX,PY).  1 .LE. IST
!                            .LE. N.  THE OUTPUT VALUE OF
!                            IST FROM A PREVIOUS CALL MAY
!                            BE A GOOD CHOICE.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH AND DERIVATIVE
!   ESTIMATES MAY BE COMPUTED BY GRADL OR GRADG.
!
! INPUT PARAMETERS OTHER THAN IST ARE NOT ALTERED BY THIS
!   ROUTINE.
!
! OUTPUT PARAMETERS - IST - INDEX OF ONE OF THE VERTICES OF
!                           THE TRIANGLE CONTAINING (PX,PY)
!                           UNLESS IER .LT. 0.
!
!                      PZ - VALUE OF F AT (PX,PY), OR 0 IF
!                           IER .LT. 0.
!
!                     IER - ERROR INDICATOR
!                           IER = 0 IF NO ERRORS WERE
!                                   ENCOUNTERED.
!                           IER = 1 IF NO ERRORS WERE EN-
!                                   COUNTERED AND EXTRAPOLA-
!                                   TION WAS PERFORMED.
!                           IER = -1 IF N, IFLAG, OR IST IS
!                                    OUT OF RANGE.
!                           IER = -2 IF THE NODES ARE COL-
!                                    LINEAR.
!
! MODULES REFERENCED BY INTRC1 - TRFIND, TVAL,
!             (AND OPTIONALLY)   GRADL, GETNP, SETUP,
!                                GIVENS, ROTATE
!
!***********************************************************
!
      INTEGER NN, I1, I2, I3, IERR, N1, N2, INDX
      REAL    XP, YP, ZX1, ZY1, ZX2, ZY2, ZX3, ZY3, X1, Y1,   &
              X2, Y2, X3, Y3, Z1, Z2, Z3, DUM, DP, U, V, XQ,  &
              YQ, R1, R2, A1, A2, B1, B2, C1, C2, F1, F2
!
! LOCAL PARAMETERS -
!
! NN =                      LOCAL COPY OF N
! I1,I2,I3 =                VERTICES DETERMINED BY TRFIND
! IERR =                    ERROR FLAG FOR CALLS TO GRADL
!                             AND TVAL
! N1,N2 =                   ENDPOINTS OF THE CLOSEST BOUND-
!                             ARY EDGE TO P WHEN P IS OUT-
!                             SIDE OF THE MESH BOUNDARY
! INDX =                    IADJ INDEX OF N1 AS A NEIGHBOR
!                             OF N2
! XP,YP =                   LOCAL COPIES OF THE COORDINATES
!                             OF P=(PX,PY)
! ZX1,ZY1,ZX2,ZY2,ZX3,ZY3 = X AND Y DERIVATIVES AT THE
!                             VERTICES OF A TRIANGLE T WHICH
!                             CONTAINS P OR AT N1 AND N2
! X1,Y1,X2,Y2,X3,Y3 =       X,Y COORDINATES OF THE VERTICES
!                             OF T OR OF N1 AND N2
! Z1,Z2,Z3 =                DATA VALUES AT THE VERTICES OF T
! DUM =                     DUMMY VARIABLE FOR CALL TO TVAL
! DP =                      INNER PRODUCT OF N1-N2 AND P-N2
! U,V =                     X,Y COORDINATES OF THE VECTOR
!                             N2-N1
! XQ,YQ =                   X,Y COORDINATES OF THE CLOSEST
!                             BOUNDARY POINT TO P WHEN P IS
!                             OUTSIDE OF THE MESH BOUNDARY
! R1,R2 =                   BARYCENTRIC COORDINATES OF Q
!                             WITH RESPECT TO THE LINE SEG-
!                             MENT N2-N1 CONTAINING Q
! A1,A2,B1,B2,C1,C2 =       CARDINAL FUNCTIONS FOR EVALUAT-
!                             ING THE INTERPOLATORY SURFACE
!                             AT Q
! F1,F2 =                   CUBIC FACTORS USED TO COMPUTE
!                             THE CARDINAL FUNCTIONS
!
      NN = N
      PZ = 0.
      IF (NN .LT. 3  .OR.  IFLAG .LT. 0  .OR.  IFLAG .GT. 1  &
         .OR.  IST .LT. 1  .OR.  IST .GT. NN) GO TO 11
      XP = PX
      YP = PY
!
! FIND A TRIANGLE CONTAINING P IF P IS WITHIN THE MESH
!   BOUNDARY
!
      CALL TRFIND(IST,XP,YP,X,Y,IADJ,IEND, I1,I2,I3)
      IF (I1 .EQ. 0) GO TO 12
      IST = I1
      IF (I3 .EQ. 0) GO TO 3
      IF (IFLAG .NE. 1) GO TO 1
!
! DERIVATIVES ARE USER PROVIDED
!
      ZX1 = ZXZY(1,I1)
      ZX2 = ZXZY(1,I2)
      ZX3 = ZXZY(1,I3)
      ZY1 = ZXZY(2,I1)
      ZY2 = ZXZY(2,I2)
      ZY3 = ZXZY(2,I3)
      GO TO 2
!
! COMPUTE DERIVATIVE ESTIMATES AT THE VERTICES
!
    1 CALL GRADL(NN,I1,X,Y,Z,IADJ,IEND, ZX1,ZY1,IERR)
      CALL GRADL(NN,I2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR)
      CALL GRADL(NN,I3,X,Y,Z,IADJ,IEND, ZX3,ZY3,IERR)
!
! SET LOCAL PARAMETERS FOR CALL TO TVAL
!
    2 X1 = X(I1)
      Y1 = Y(I1)
      X2 = X(I2)
      Y2 = Y(I2)
      X3 = X(I3)
      Y3 = Y(I3)
      Z1 = Z(I1)
      Z2 = Z(I2)
      Z3 = Z(I3)
      CALL TVAL(XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2, &
                ZX3,ZY1,ZY2,ZY3,0, PZ,DUM,DUM,IERR)
      IF (IERR .NE. 0) GO TO 12
      IER = 0
      RETURN
!
! P IS OUTSIDE OF THE MESH BOUNDARY.  EXTRAPOLATE TO P BY
!   PASSING A LINEAR FUNCTION OF ONE VARIABLE THROUGH THE
!   VALUE AND DIRECTIONAL DERIVATIVE (IN THE DIRECTION
!   P-Q) OF THE INTERPOLATORY SURFACE (TVAL) AT Q WHERE
!   Q IS THE CLOSEST BOUNDARY POINT TO P.
!
! DETERMINE Q BY TRAVERSING THE BOUNDARY STARTING FROM
!   THE RIGHTMOST VISIBLE NODE I1.
!
    3 N2 = I1
!
! SET N1 TO THE LAST NONZERO NEIGHBOR OF N2 AND COMPUTE DP
!
    4 INDX = IEND(N2) - 1
      N1 = IADJ(INDX)
      X1 = X(N1)
      Y1 = Y(N1)
      X2 = X(N2)
      Y2 = Y(N2)
      DP = (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2)
      IF (DP .LE. 0.) GO TO 5
      IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) .GT. 0.) GO TO 8
      N2 = N1
      GO TO 4
!
! N2 IS THE CLOSEST BOUNDARY POINT TO P.  COMPUTE PARTIAL
!   DERIVATIVES AT N2.
!
    5 IF (IFLAG .NE. 1) GO TO 6
      ZX2 = ZXZY(1,N2)
      ZY2 = ZXZY(2,N2)
      GO TO 7
    6 CALL GRADL(NN,N2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR)
!
! COMPUTE EXTRAPOLATED VALUE AT P
!
    7 PZ = Z(N2) + ZX2*(XP-X2) + ZY2*(YP-Y2)
      IER = 1
      RETURN
!
! THE CLOSEST BOUNDARY POINT Q LIES ON N2-N1.  COMPUTE
!   PARTIALS AT N1 AND N2.
!
    8 IF (IFLAG .NE. 1) GO TO 9
      ZX1 = ZXZY(1,N1)
      ZY1 = ZXZY(2,N1)
      ZX2 = ZXZY(1,N2)
      ZY2 = ZXZY(2,N2)
      GO TO 10
    9 CALL GRADL(NN,N1,X,Y,Z,IADJ,IEND, ZX1,ZY1,IERR)
      CALL GRADL(NN,N2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR)
!
! COMPUTE Q, ITS BARYCENTRIC COORDINATES, AND THE CARDINAL
!   FUNCTIONS FOR EXTRAPOLATION
!
   10 U = X2-X1
      V = Y2-Y1
      R1 = DP/(U**2 + V**2)
      R2 = 1. - R1
      XQ = R1*X1 + R2*X2
      YQ = R1*Y1 + R2*Y2
      F1 = R1*R1*R2
      F2 = R1*R2*R2
      A1 = R1 + (F1-F2)
      A2 = R2 - (F1-F2)
      B1 = U*F1
      B2 = -U*F2
      C1 = V*F1
      C2 = -V*F2
!
! COMPUTE THE VALUE OF THE INTERPOLATORY SURFACE (TVAL)
!   AT Q
!
      PZ = A1*Z(N1) + A2*Z(N2) + B1*ZX1 + B2*ZX2 + C1*ZY1 + C2*ZY2
!
! COMPUTE THE EXTRAPOLATED VALUE AT P
!
      PZ = PZ + (R1*ZX1 + R2*ZX2)*(XP-XQ) + (R1*ZY1 + R2*ZY2)*(YP-YQ)
      IER = 1
      RETURN
!
! N, IFLAG, OR IST OUT OF RANGE
!
   11 IER = -1
      RETURN
!
! NODES ARE COLLINEAR
!
   12 IER = -2
      RETURN
      END
!==============================================================================
! SUBROUTINE ROTATE
!==============================================================================
      SUBROUTINE ROTATE (N,C,S, X,Y )
      INTEGER N
      REAL    C, S, X(N), Y(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!                                            ( C  S)
!   THIS ROUTINE APPLIES THE GIVENS ROTATION (     ) TO THE
!                                            (-S  C)
!               (X(1) ... X(N))
! 2 BY N MATRIX (             ).  THIS ROUTINE WAS TAKEN
!               (Y(1) ... Y(N))
! LINPACK.
!
! INPUT PARAMETERS -   N - NUMBER OF COLUMNS TO BE ROTATED.
!
!                    C,S - ELEMENTS OF THE GIVENS ROTATION.
!                          THESE MAY BE DETERMINED BY
!                          SUBROUTINE GIVENS.
!
!                    X,Y - VECTORS OF LENGTH .GE. N
!                          CONTAINING THE 2-VECTORS TO BE
!                          ROTATED.
!
!   THE PARAMETERS N, C, AND S ARE NOT ALTERED BY THIS
! ROUTINE.
!
! OUTPUT PARAMETERS - X,Y - ROTATED VECTORS
!
! MODULES REFERENCED BY ROTATE - NONE
!
!***********************************************************
!
      INTEGER I
      REAL    XI, YI
!
! LOCAL PARAMETERS -
!
! I =     DO-LOOP INDEX
! XI,YI = X(I), Y(I)
!
      IF (N .LE. 0 .OR. (C .EQ. 1. .AND. S .EQ. 0.)) RETURN
      DO 1 I = 1,N
        XI = X(I)
        YI = Y(I)
        X(I) = C*XI + S*YI
        Y(I) = -S*XI + C*YI
    1   CONTINUE
      RETURN
      END
!==============================================================================
! SUBROUTINE SETUP
!==============================================================================
      SUBROUTINE SETUP (XK,YK,ZK,XI,YI,ZI,S1,S2,R, ROW)
      REAL XK, YK, ZK, XI, YI, ZI, S1, S2, R, ROW(6)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE SETS UP THE I-TH ROW OF AN AUGMENTED RE-
! GRESSION MATRIX FOR A WEIGHTED LEAST-SQUARES FIT OF A
! QUADRATIC FUNCTION Q(X,Y) TO A SET OF DATA VALUES Z WHERE
! Q(XK,YK) = ZK.  THE FIRST 3 COLUMNS (QUADRATIC TERMS) ARE
! SCALED BY 1/S2 AND THE FOURTH AND FIFTH COLUMNS (LINEAR
! TERMS) ARE SCALED BY 1/S1.  THE WEIGHT IS (R-D)/(R*D) IF
! R .GT. D AND 0 IF R .LE. D, WHERE D IS THE DISTANCE
! BETWEEN NODES I AND K.
!
! INPUT PARAMETERS - XK,YK,ZK - COORDINATES AND DATA VALUE
!                               AT NODE K -- INTERPOLATED
!                               BY Q.
!
!                    XI,YI,ZI - COORDINATES AND DATA VALUE
!                               AT NODE I.
!
!                       S1,S2 - INVERSE SCALE FACTORS.
!
!                           R - RADIUS OF INFLUENCE ABOUT
!                               NODE K DEFINING THE WEIGHT.
!
!                         ROW - VECTOR OF LENGTH 6.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETER - ROW - VECTOR CONTAINING A ROW OF THE
!                          AUGMENTED REGRESSION MATRIX.
!
! MODULES REFERENCED BY SETUP - NONE
!
! INTRINSIC FUNCTION CALLED BY SETUP - SQRT
!
!***********************************************************
!
      INTEGER I
      REAL    DX, DY, DXSQ, DYSQ, D, W, W1, W2
!
! LOCAL PARAMETERS -
!
! I =    DO-LOOP INDEX
! DX =   XI - XK
! DY =   YI - YK
! DXSQ = DX*DX
! DYSQ = DY*DY
! D =    DISTANCE BETWEEN NODES K AND I
! W =    WEIGHT ASSOCIATED WITH THE ROW
! W1 =   W/S1
! W2 =   W/S2
!
      DX = XI - XK
      DY = YI - YK
      DXSQ = DX*DX
      DYSQ = DY*DY
      D = SQRT(DXSQ + DYSQ)
      IF (D .LE. 0.  .OR.  D .GE. R) GO TO 1
      W = (R-D)/R/D
      W1 = W/S1
      W2 = W/S2
      ROW(1) = DXSQ*W2
      ROW(2) = DX*DY*W2
      ROW(3) = DYSQ*W2
      ROW(4) = DX*W1
      ROW(5) = DY*W1
      ROW(6) = (ZI - ZK)*W
      RETURN
!
! NODES K AND I COINCIDE OR NODE I IS OUTSIDE OF THE RADIUS
!   OF INFLUENCE.  SET ROW TO THE ZERO VECTOR.
!
    1 DO 2 I = 1,6
    2   ROW(I) = 0.
      RETURN
      END
!==============================================================================
! FUNCTION TRVOL
!==============================================================================
      REAL FUNCTION TRVOL (X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3)
      REAL X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS FUNCTION COMPUTES THE INTEGRAL OVER A TRIANGLE OF
! THE PLANAR SURFACE WHICH INTERPOLATES DATA VALUES AT THE
! VERTICES.
!
! INPUT PARAMETERS - X1,X2,X3 - X COORDINATES OF THE
!                               VERTICES OF THE TRIANGLE IN
!                               COUNTERCLOCKWISE ORDER.
!
!                    Y1,Y2,Y3 - Y COORDINATES OF THE
!                               VERTICES OF THE TRIANGLE IN
!                               THE SAME ORDER AS THE X
!                               COORDINATES.
!
!                    Z1,Z2,Z3 - DATA VALUES AT THE VERTICES
!                               IN THE SAME ORDER AS THE
!                               COORDINATES.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER -    TRVOL - VOLUME UNDER THE INTERPOLA-
!                               TORY SURFACE ABOVE THE
!                               TRIANGLE OR ZERO IF THE
!                               COORDINATES ARE INCORRECTLY
!                               ORDERED OR COLLINEAR.
!
! MODULES REFERENCED BY TRVOL - NONE
!
!***********************************************************
!
      REAL T1, T2, T3, AREA

      T1 = X2*Y3 - X3*Y2
      T2 = X3*Y1 - X1*Y3
      T3 = X1*Y2 - X2*Y1
      AREA = T1 + T2 + T3
      IF (AREA .LT. 0.) AREA = 0.
!
! AREA IS TWICE THE AREA OF THE TRIANGLE.  TRVOL IS THE MEAN
!   OF THE DATA VALUES TIMES THE AREA OF THE TRIANGLE.
!
      TRVOL = (Z1 + Z2 + Z3)*AREA/6.
      RETURN
      END
!==============================================================================
! SUBROUTINE TVAL
!==============================================================================
      SUBROUTINE TVAL (X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1, &
                       ZX2,ZX3,ZY1,ZY2,ZY3,IFLAG, W,WX,WY, &
                       IER)
      INTEGER IFLAG, IER
      REAL    X, Y, X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3, &
              ZX1, ZX2, ZX3, ZY1, ZY2, ZY3, W, WX, WY
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN FUNCTION VALUES AND FIRST PARTIAL DERIVATIVES AT
! THE THREE VERTICES OF A TRIANGLE, THIS ROUTINE DETERMINES
! A FUNCTION W WHICH AGREES WITH THE GIVEN DATA, RETURNING
! THE VALUE AND (OPTIONALLY) FIRST PARTIAL DERIVATIVES OF W
! AT A POINT (X,Y) IN THE TRIANGLE.  THE INTERPOLATION
! METHOD IS EXACT FOR QUADRATIC POLYNOMIAL DATA.  THE
! TRIANGLE IS PARTITIONED INTO THREE SUBTRIANGLES WITH
! EQUAL AREAS.  W IS CUBIC IN EACH SUBTRIANGLE AND ALONG
! THE EDGES, BUT HAS ONLY ONE CONTINUOUS DERIVATIVE ACROSS
! EDGES.  THE NORMAL DERIVATIVE OF W VARIES LINEARLY ALONG
! EACH OUTER EDGE.  THE VALUES AND PARTIAL DERIVATIVES OF W
! ALONG A TRIANGLE EDGE DEPEND ONLY ON THE DATA VALUES AT
! THE ENDPOINTS OF THE EDGE.  THUS THE METHOD YIELDS C-1
! CONTINUITY WHEN USED TO INTERPOLATE OVER A TRIANGULAR
! GRID.  THIS ALGORITHM IS DUE TO C. L. LAWSON.
!
! INPUT PARAMETERS -   X,Y - COORDINATES OF A POINT AT WHICH
!                            W IS TO BE EVALUATED.
!
!        X1,X2,X3,Y1,Y2,Y3 - COORDINATES OF THE VERTICES OF
!                            A TRIANGLE CONTAINING (X,Y).
!
!                 Z1,Z2,Z3 - FUNCTION VALUES AT THE VERTICES
!                            TO BE INTERPOLATED.
!
!              ZX1,ZX2,ZX3 - X-DERIVATIVE VALUES AT THE
!                            VERTICES.
!
!              ZY1,ZY2,ZY3 - Y-DERIVATIVE VALUES AT THE
!                            VERTICES.
!
!                    IFLAG - OPTION INDICATOR
!                            IFLAG = 0 IF ONLY W IS TO BE
!                                      COMPUTED.
!                            IFLAG = 1 IF W, WX, AND WY ARE
!                                      TO BE RETURNED.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS -   W - ESTIMATED VALUE OF THE INTERP-
!                           OLATORY FUNCTION AT (X,Y) IF
!                           IER = 0.  OTHERWISE W = 0.
!
!                   WX,WY - PARTIAL DERIVATIVES OF W AT
!                           (X,Y) IF IER = 0 AND IFLAG = 1,
!                           UNCHANGED IF IFLAG .NE. 1, ZERO
!                           IF IER .NE. 0 AND IFLAG = 1.
!
!                     IER - ERROR INDICATOR
!                           IER = 0 IF NO ERRORS WERE
!                                   ENCOUNTERED.
!                           IER = 1 IF THE VERTICES OF THE
!                                   TRIANGLE ARE COLLINEAR.
!
! MODULES REFERENCED BY TVAL - NONE
!
! INTRINSIC FUNCTION CALLED BY TVAL - AMIN1
!
!***********************************************************
!
      INTEGER I, IP1, IP2, IP3
      REAL    U(3), V(3), SL(3), AREA, XP, YP, R(3), RX(3),   &
              RY(3), PHI(3), PHIX(3), PHIY(3), RMIN, C1, C2,  &
              RO(3), ROX(3), ROY(3), F(3), G(3), GX(3),       &
              GY(3), P(3), PX(3), PY(3), Q(3), QX(3), QY(3),  &
              A(3), AX(3), AY(3), B(3), BX(3), BY(3), C(3),   &
              CX(3), CY(3)
!
! LOCAL PARAMETERS -
!
! I =               DO-LOOP INDEX
! IP1,IP2,IP3 =     PERMUTED INDICES FOR COMPUTING RO, ROX,
!                     AND ROY
! U(K) =            X-COMPONENT OF THE VECTOR REPRESENTING
!                     THE SIDE OPPOSITE VERTEX K
! V(K) =            Y-COMPONENT OF THE VECTOR REPRESENTING
!                     THE SIDE OPPOSITE VERTEX K
! SL(K) =           SQUARE OF THE LENGTH OF THE SIDE
!                     OPPOSITE VERTEX K
! AREA =            TWICE THE AREA OF THE TRIANGLE
! XP,YP =           X-X1, Y-Y1
! R(K) =            K-TH BARYCENTRIC COORDINATE
! RX(K),RY(K) =     X,Y PARTIAL DERIVATIVES OF R(K)
! PHI(K)            R(K-1)*R(K+1) -- QUADRATIC
! PHIX(K),PHIY(K) = X,Y PARTIALS OF PHI(K)
! RMIN =            MIN(R1,R2,R3)
! C1,C2 =           FACTORS FOR COMPUTING RO
! RO(K) =           FACTORS FOR COMPUTING G -- CUBIC
!                     CORRECTION TERMS
! ROX(K),ROY(K) =   X,Y PARTIALS OF RO(K)
! F(K) =            FACTORS FOR COMPUTING G, GX, AND GY --
!                     CONSTANT
! G(K) =            FACTORS FOR COMPUTING THE CARDINAL
!                     FUNCTIONS -- CUBIC
! GX(K),GY(K) =     X,Y PARTIALS OF G(K)
! P(K) =            G(K) + PHI(K)
! PX(K),PY(K) =     X,Y PARTIALS OF P(K)
! Q(K) =            G(K) - PHI(K)
! QX(K),QY(K) =     X,Y PARTIALS OF Q(K)
! A(K) =            CARDINAL FUNCTION WHOSE COEFFICIENT IS
!                     Z(K)
! AX(K),AY(K) =     X,Y PARTIALS OF A(K) -- CARDINAL
!                     FUNCTIONS FOR WX AND WY
! B(K) =            TWICE THE CARDINAL FUNCTION WHOSE
!                     COEFFICIENT IS ZX(K)
! BX(K),BY(K) =     X,Y PARTIALS OF B(K)
! C(K) =            TWICE THE CARDINAL FUNCTION WHOSE
!                     COEFFICIENT IS ZY(K)
! CX(K),CY(K) =     X,Y PARTIALS OF C(K)
!
      U(1) = X3 - X2
      U(2) = X1 - X3
      U(3) = X2 - X1
!
      V(1) = Y3 - Y2
      V(2) = Y1 - Y3
      V(3) = Y2 - Y1
!
      DO 1 I = 1,3
        SL(I) = U(I)*U(I) + V(I)*V(I)
    1   CONTINUE
!
! AREA = 3-1 X 3-2
!
      AREA = U(1)*V(2) - U(2)*V(1)
      IF (AREA .EQ. 0.) GO TO 9
!
! R(1) = (2-3 X 2-(X,Y))/AREA, R(2) = (1-(X,Y) X 1-3)/AREA,
!   R(3) = (1-2 X 1-(X,Y))/AREA
!
      R(1) = (U(1)*(Y-Y2) - V(1)*(X-X2))/AREA
      XP = X - X1
      YP = Y - Y1
      R(2) = (U(2)*YP - V(2)*XP)/AREA
      R(3) = (U(3)*YP - V(3)*XP)/AREA
      IER = 0
!
      PHI(1) = R(2)*R(3)
      PHI(2) = R(3)*R(1)
      PHI(3) = R(1)*R(2)
!
      RMIN = AMIN1(R(1),R(2),R(3))
      IF (RMIN .NE. R(1)) GO TO 3
      IP1 = 1
      IP2 = 2
      IP3 = 3
      GO TO 5
    3 IF (RMIN .NE. R(2)) GO TO 4
      IP1 = 2
      IP2 = 3
      IP3 = 1
      GO TO 5
    4 IP1 = 3
      IP2 = 1
      IP3 = 2
!
    5 C1 = RMIN*RMIN/2.
      C2 = RMIN/3.
      RO(IP1) = (PHI(IP1) + 5.*C1/3.)*R(IP1) - C1
      RO(IP2) = C1*(R(IP3) - C2)
      RO(IP3) = C1*(R(IP2) - C2)
!
      F(1) = 3.*(SL(2)-SL(3))/SL(1)
      F(2) = 3.*(SL(3)-SL(1))/SL(2)
      F(3) = 3.*(SL(1)-SL(2))/SL(3)
!
      G(1) = (R(2)-R(3))*PHI(1) + F(1)*RO(1) - RO(2) + RO(3)
      G(2) = (R(3)-R(1))*PHI(2) + F(2)*RO(2) - RO(3) + RO(1)
      G(3) = (R(1)-R(2))*PHI(3) + F(3)*RO(3) - RO(1) + RO(2)
!
      DO 6 I = 1,3
        P(I) = G(I) + PHI(I)
        Q(I) = G(I) - PHI(I)
    6   CONTINUE
!
      A(1) = R(1) + G(3) - G(2)
      A(2) = R(2) + G(1) - G(3)
      A(3) = R(3) + G(2) - G(1)
!
      B(1) = U(3)*P(3) + U(2)*Q(2)
      B(2) = U(1)*P(1) + U(3)*Q(3)
      B(3) = U(2)*P(2) + U(1)*Q(1)
!
      C(1) = V(3)*P(3) + V(2)*Q(2)
      C(2) = V(1)*P(1) + V(3)*Q(3)
      C(3) = V(2)*P(2) + V(1)*Q(1)
!
! W IS A LINEAR COMBINATION OF THE CARDINAL FUNCTIONS
!
      W = A(1)*Z1 + A(2)*Z2 + A(3)*Z3 + (B(1)*ZX1 + B(2)*ZX2 &
          + B(3)*ZX3 + C(1)*ZY1 + C(2)*ZY2 + C(3)*ZY3)/2.
      IF (IFLAG .NE. 1) RETURN
!
! COMPUTE WX AND WY
!
      DO 7 I = 1,3
        RX(I) = -V(I)/AREA
        RY(I) = U(I)/AREA
    7   CONTINUE
      PHIX(1) = R(2)*RX(3) + RX(2)*R(3)
      PHIY(1) = R(2)*RY(3) + RY(2)*R(3)
      PHIX(2) = R(3)*RX(1) + RX(3)*R(1)
      PHIY(2) = R(3)*RY(1) + RY(3)*R(1)
      PHIX(3) = R(1)*RX(2) + RX(1)*R(2)
      PHIY(3) = R(1)*RY(2) + RY(1)*R(2)
!
      ROX(IP1) = RX(IP1)*(PHI(IP1) + 5.*C1) + R(IP1)*(PHIX(IP1) - RX(IP1))
      ROY(IP1) = RY(IP1)*(PHI(IP1) + 5.*C1) + R(IP1)*(PHIY(IP1) - RY(IP1))
      ROX(IP2) = RX(IP1)*(PHI(IP2) - C1) + C1*RX(IP3)
      ROY(IP2) = RY(IP1)*(PHI(IP2) - C1) + C1*RY(IP3)
      ROX(IP3) = RX(IP1)*(PHI(IP3) - C1) + C1*RX(IP2)
      ROY(IP3) = RY(IP1)*(PHI(IP3) - C1) + C1*RY(IP2)
!
      GX(1) = (RX(2) - RX(3))*PHI(1) + (R(2) - R(3))*PHIX(1) &
              + F(1)*ROX(1) - ROX(2) + ROX(3)
      GY(1) = (RY(2) - RY(3))*PHI(1) + (R(2) - R(3))*PHIY(1) &
              + F(1)*ROY(1) - ROY(2) + ROY(3)
      GX(2) = (RX(3) - RX(1))*PHI(2) + (R(3) - R(1))*PHIX(2) &
              + F(2)*ROX(2) - ROX(3) + ROX(1)
      GY(2) = (RY(3) - RY(1))*PHI(2) + (R(3) - R(1))*PHIY(2) &
              + F(2)*ROY(2) - ROY(3) + ROY(1)
      GX(3) = (RX(1) - RX(2))*PHI(3) + (R(1) - R(2))*PHIX(3) &
              + F(3)*ROX(3) - ROX(1) + ROX(2)
      GY(3) = (RY(1) - RY(2))*PHI(3) + (R(1) - R(2))*PHIY(3) &
              + F(3)*ROY(3) - ROY(1) + ROY(2)
!
      DO 8 I = 1,3
        PX(I) = GX(I) + PHIX(I)
        PY(I) = GY(I) + PHIY(I)
        QX(I) = GX(I) - PHIX(I)
        QY(I) = GY(I) - PHIY(I)
    8   CONTINUE
!
      AX(1) = RX(1) + GX(3) - GX(2)
      AY(1) = RY(1) + GY(3) - GY(2)
      AX(2) = RX(2) + GX(1) - GX(3)
      AY(2) = RY(2) + GY(1) - GY(3)
      AX(3) = RX(3) + GX(2) - GX(1)
      AY(3) = RY(3) + GY(2) - GY(1)
!
      BX(1) = U(3)*PX(3) + U(2)*QX(2)
      BY(1) = U(3)*PY(3) + U(2)*QY(2)
      BX(2) = U(1)*PX(1) + U(3)*QX(3)
      BY(2) = U(1)*PY(1) + U(3)*QY(3)
      BX(3) = U(2)*PX(2) + U(1)*QX(1)
      BY(3) = U(2)*PY(2) + U(1)*QY(1)
!
      CX(1) = V(3)*PX(3) + V(2)*QX(2)
      CY(1) = V(3)*PY(3) + V(2)*QY(2)
      CX(2) = V(1)*PX(1) + V(3)*QX(3)
      CY(2) = V(1)*PY(1) + V(3)*QY(3)
      CX(3) = V(2)*PX(2) + V(1)*QX(1)
      CY(3) = V(2)*PY(2) + V(1)*QY(1)
!
! WX AND WY ARE LINEAR COMBINATIONS OF THE CARDINAL
!   FUNCTIONS
!
      WX = AX(1)*Z1 + AX(2)*Z2 + AX(3)*Z3 + (BX(1)*ZX1 +   &
           BX(2)*ZX2 + BX(3)*ZX3 + CX(1)*ZY1 + CX(2)*ZY2 + &
           CX(3)*ZY3)/2.
      WY = AY(1)*Z1 + AY(2)*Z2 + AY(3)*Z3 + (BY(1)*ZX1 +   &
           BY(2)*ZX2 + BY(3)*ZX3 + CY(1)*ZY1 + CY(2)*ZY2 + &
           CY(3)*ZY3)/2.
      RETURN
!
! VERTICES ARE COLLINEAR
!
    9 IER = 1
      W = 0.
      IF (IFLAG .NE. 1) RETURN
      WX = 0.
      WY = 0.
      RETURN
      END
!==============================================================================
! SUBROUTINE UNIF
!==============================================================================
      SUBROUTINE UNIF (N,X,Y,Z,IADJ,IEND,NROW,NX,NY,PX,PY,IFLAG, ZXZY, ZZ,IER)
      INTEGER N, IADJ(1), IEND(N), NROW, NX, NY, IFLAG, IER
      REAL    X(N), Y(N), Z(N), PX(NX), PY(NY), ZXZY(2,N), ZZ(NROW,NY)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A THIESSEN TRIANGULATION OF A SET OF POINTS IN THE
! PLANE WITH CORRESPONDING DATA VALUES, THIS ROUTINE INTERP-
! OLATES THE DATA VALUES TO A SET OF RECTANGULAR GRID POINTS
! FOR SUCH APPLICATIONS AS CONTOURING.  THE INTERPOLANT IS
! ONCE CONTINUOUSLY DIFFERENTIABLE.  EXTRAPOLATION IS PER-
! FORMED AT GRID POINTS EXTERIOR TO THE TRIANGULATION.
!
! INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
!                        TION.  N .GE. 3
!
!                X,Y,Z - N-VECTORS OF NODAL COORDINATES AND
!                        DATA VALUES.
!
!            IADJ,IEND - TRIANGULATION DATA STRUCTURE -- MAY
!                        BE CREATED BY TRMESH.
!
!                 NROW - NUMBER OF ROWS IN THE DIMENSION
!                        STATEMENT OF ZZ.
!
!                NX,NY - NUMBER OF ROWS AND COLUMNS, RESPEC-
!                        TIVELY, IN THE RECTANGULAR GRID.
!                        1 .LE. NX .LE. NROW AND 1 .LE. NY.
!
!                PX,PY - VECTORS OF LENGTH NX AND NY, RE-
!                        SPECTIVELY, CONTAINING THE COORDI-
!                        NATES OF THE GRID LINES.
!
!                IFLAG - OPTION INDICATOR
!                        IFLAG = 0 IF DERIVATIVE ESTIMATES
!                                  AT THE VERTICES OF A
!                                  TRIANGLE ARE TO BE RECOM-
!                                  PUTED FOR EACH GRID POINT
!                                  IN THE TRIANGLE AND NOT
!                                  SAVED.
!                        IFLAG = 1 IF DERIVATIVE ESTIMATES
!                                  ARE INPUT IN ZXZY.
!                        IFLAG = 2 IF DERIVATIVE ESTIMATES
!                                  ARE TO BE COMPUTED ONCE
!                                  FOR EACH NODE (BY GRADL)
!                                  AND SAVED IN ZXZY.
!
!                 ZXZY - NOT USED IF IFLAG = 0, 2 BY N ARRAY
!                        WHOSE COLUMNS CONTAIN THE X AND Y
!                        PARTIAL DERIVATIVE ESTIMATES (X
!                        PARTIALS IN THE FIRST ROW) IF
!                        IFLAG = 1, OR 2 BY N ARRAY (OR WORK
!                        SPACE OF LENGTH .GE. 2*N) IF IFLAG
!                        = 2.
!
! DERIVATIVE ESTIMATES MAY BE COMPUTED BY GRADL OR GRADG.
!
!                   ZZ - NROW BY NCOL ARRAY WITH NROW .GE.
!                        NX AND NCOL .GE. NY.
!
! NONE OF THE INPUT PARAMETERS ARE ALTERED EXCEPT AS
!   NOTED BELOW.
!
! OUTPUT PARAMETERS - ZXZY - 2 BY N ARRAY WHOSE COLUMNS CON-
!                            TAIN X AND Y PARTIAL DERIVATIVE
!                            ESTIMATES AT THE NODES IF IFLAG
!                            = 2 AND IER .GE. 0, NOT ALTERED
!                            IF IFLAG .NE. 2.
!
!                       ZZ - INTERPOLATED VALUES AT THE GRID
!                            POINTS IF IER .GE. 0.
!                            ZZ(I,J) = F(PX(I),PY(J)) FOR
!                            I = 1,...,NX AND J = 1,...,NY.
!
!                      IER - ERROR INDICATOR
!                            IER .GE. 0 IF NO ERRORS WERE
!                                       ENCOUNTERED.  IER
!                                       CONTAINS THE NUMBER
!                                       OF GRID POINTS EXT-
!                                       ERIOR TO THE TRIAN-
!                                       GULATION BOUNDARY.
!                            IER  =  -1 IF N, NX, NY, OR
!                                       IFLAG IS OUT OF
!                                       RANGE.
!                            IER  =  -2 IF THE NODES ARE
!                                       COLLINEAR.
!
! MODULES REFERENCED BY UNIF - INTRC1, TRFIND, TVAL,
!           (AND OPTIONALLY)   GRADL, GETNP, SETUP, GIVENS,
!                                AND ROTATE
!
!***********************************************************
!
      INTEGER NST, IST, NN, NI, NJ, IFL, I, J, IERR, NEX
      DATA    NST/1/
!
! LOCAL PARAMETERS -
!
! IST =   PARAMETER FOR INTRC1
! NST =   INITIAL VALUE FOR IST
! NN =    LOCAL COPY OF N
! NI,NJ = LOCAL COPIES OF NX AND NY
! IFL =   LOCAL COPY OF IFLAG FOR INTRC1
! I,J =   DO-LOOP INDICES
! IERR =  ERROR FLAG FOR CALLS TO GRADL AND INTRC1
! NEX =   NUMBER OF GRID POINTS EXTERIOR TO THE TRIANGULA-
!           TION BOUNDARY (NUMBER OF EXTRAPOLATED VALUES)
!
      NN = N
      NI = NX
      NJ = NY
      IFL = IFLAG
      IF (NN .LT. 3  .OR.  NI .LT. 1  .OR.  NI .GT. NROW &
         .OR.  NJ .LT. 1  .OR.  IFL .LT. 0  .OR.         &
         IFL .GT. 2) GO TO 4
      IST = NST
      IF (IFL .NE. 2) GO TO 2
!
! COMPUTE DERIVATIVE ESTIMATES AT THE NODES.
!
      IFL = 1
      DO 1 I = 1,NN
        CALL GRADL(NN,I,X,Y,Z,IADJ,IEND, ZXZY(1,I), ZXZY(2,I),IERR)
        IF (IERR .LT. 0) GO TO 5
    1   CONTINUE
!
! COMPUTE INTERPOLATED VALUES
!
    2 NEX = 0
      DO 3 J = 1,NJ
        DO 3 I = 1,NI
          CALL INTRC1(NN,PX(I),PY(J),X,Y,Z,IADJ,IEND,IFL, ZXZY, IST, ZZ(I,J),IERR)
          IF (IERR .LT. 0) GO TO 5
          NEX = NEX + IERR
    3     CONTINUE
      IER = NEX
      RETURN
!
! N, NX, NY, OR IFLAG IS OUT OF RANGE
!
    4 IER = -1
      RETURN
!
! TRIANGULATION NODES ARE COLLINEAR
!
    5 IER = -2
      RETURN
      END
!==============================================================================
! FUNCTION VOLUME
!==============================================================================
      REAL FUNCTION VOLUME (N,X,Y,Z,IADJ,IEND)
      INTEGER N, IADJ(1), IEND(N)
      REAL    X(N), Y(N), Z(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A SET OF N DATA POINTS (X(I),Y(I)) AND FUNCTION
! VALUES Z(I)=F(X(I),Y(I)) AND A TRIANGULATION COVERING THE
! CONVEX HULL H OF THE DATA POINTS, THIS FUNCTION APPROXI-
! MATES THE INTEGRAL OF F OVER H BY INTEGRATING THE PIECE-
! WISE LINEAR INTERPOLANT OF THE DATA VALUES.  VOLUME IS
! PART OF AN INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES
! TO CREATE, UPDATE, AND PLOT THE TRIANGULAR MESH.
!
! INPUT PARAMETERS -      N - NUMBER OF NODES IN THE MESH.
!                             N .GE. 3.
!
!                       X,Y - VECTORS OF COORDINATES OF
!                             THE NODES IN THE MESH.
!
!                         Z - VECTOR OF DATA VALUES AT THE
!                             NODES.
!
!                      IADJ - SET OF ADJACENCY LISTS OF
!                             NODES IN THE MESH.
!
!                      IEND - POINTERS TO THE ENDS OF
!                             ADJACENCY LISTS IN IADJ FOR
!                             EACH NODE IN THE MESH.
!
! IADJ AND IEND MAY BE CREATED BY TRMESH.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER - VOLUME - SUM OF THE VOLUMES OF THE
!                             LINEAR INTERPOLANTS ON THE
!                             TRIANGLES.
!
! MODULE REFERENCED BY VOLUME - TRVOL
!
!***********************************************************
!
      INTEGER NN, NM2, N1, N2, N3, INDF, INDL, INDX
      REAL    SUM, XN1, YN1, ZN1
      REAL, EXTERNAL :: TRVOL
!
! LOCAL PARAMETERS -
!
! NN =          LOCAL COPY OF N
! NM2 =         N-2
! N1,N2,N3 =    VERTICES OF A TRIANGLE IN COUNTERCLOCKWISE
!                 ORDER
! INDF =        IADJ INDEX OF THE FIRST NEIGHBOR OF N1
! INDL =        IADJ INDEX OF THE LAST NEIGHBOR OF N1
! INDX =        IADJ INDEX VARYING FROM INDF TO INDL
! SUM =         TEMPORARY STORAGE FOR ACCUMULATED VOLUME
! XN1,YN1,ZN1 = X(N1), Y(N1), Z(N1) -- STORED LOCALLY FOR
!                 EFFICIENCY
!
      NN = N
      IF (NN .LT. 3) GO TO 3
!
! INITIALIZATION
!
      NM2 = NN-2
      INDF = 1
      SUM = 0.
!
! LOOP ON TRIANGLES (N1,N2,N3) SUCH THAT N2 AND N3 ARE
!   ADJACENT NEIGHBORS OF N1 WHICH ARE BOTH LARGER THAN N1
!
      DO 2 N1 = 1,NM2
        XN1 = X(N1)
        YN1 = Y(N1)
        ZN1 = Z(N1)
        INDL = IEND(N1)
        DO 1 INDX = INDF,INDL
          N2 = IADJ(INDX)
          N3 = IADJ(INDX+1)
          IF (INDX .EQ. INDL) N3 = IADJ(INDF)
          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 1
          SUM = SUM + TRVOL(XN1,X(N2),X(N3),YN1,Y(N2),Y(N3), ZN1,Z(N2),Z(N3))
    1     CONTINUE
        INDF = INDL + 1
    2   CONTINUE
!
      VOLUME = SUM
      RETURN
!
! N IS OUT OF RANGE
!
    3 VOLUME = 0.
      RETURN
      END
!==============================================================================
! SUBROUTINE ARCTST
!==============================================================================
      SUBROUTINE ARCTST (N,X,Y,IADJ,IEND, LL, LIST,IER)
      INTEGER N, IADJ(1), IEND(N), LL, LIST(2,1), IER
      REAL    X(N), Y(N)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE DETERMINES THE NUMBER OF ARCS IN A TRIANGU-
! LATION WHICH ARE NOT LOCALLY OPTIMAL AS DEFINED BY LOGICAL
! FUNCTION SWPTST.  A THIESSEN TRIANGULATION (SEE SUBROUTINE
! TRMESH) IS CHARACTERIZED BY THE PROPERTY THAT ALL ARCS ARE
! LOCALLY OPTIMAL.  THE ALGORITHM CONSISTS OF APPLYING THE
! SWAP TEST TO ALL INTERIOR ARCS.
!
! INPUT PARAMETERS -
!
!       N - NUMBER OF NODES IN THE TRIANGULATION.  N .GE. 3.
!
!       X,Y - COORDINATES OF THE NODES.
!
!       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
!                   TION.  SEE SUBROUTINE TRMESH.
!
! THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
!       LL - NUMBER OF COLUMNS RESERVED FOR LIST (SEE THE
!            OUTPUT VALUE OF LIST).  LL .GE. 0.
!
!       LIST - INTEGER ARRAY DIMENSIONED 2 BY LL (OR VECTOR
!              OF LENGTH .GE. 2*LL) IF LL .GT. 0.
!
! OUTPUT PARAMETERS -
!
!       LL - NUMBER OF ARCS WHICH ARE NOT LOCALLY OPTIMAL
!            UNLESS IER = 1.
!
!       LIST - COLUMNS CONTAIN THE ENDPOINT NODAL INDICES
!              (SMALLER INDEX IN THE FIRST ROW) OF THE FIRST
!              K NONOPTIMAL ARCS ENCOUNTERED, WHERE K IS THE
!              MINIMUM OF THE INPUT AND OUTPUT VALUES OF LL,
!              IF IER = 0 AND K .GT. 0.  THE NUMBER OF INTE-
!              RIOR ARCS IS 3N-2NB-3 .LE. 3(N-3) WHERE NB IS
!              THE NUMBER OF BOUNDARY NODES.  BOUNDARY ARCS
!              ARE OPTIMAL BY DEFINITION.
!
!       IER - ERROR INDICATOR
!             IER = 0 IF NO ERRORS WERE ENCOUNTERED.
!             IER = 1 IF N .LT. 3 OR LL .LT. 0.
!
! MODULES REQUIRED BY ARCTST - SWPTST
!
!***********************************************************
!
      INTEGER NM1, LMAX, L, INDF, INDL, IO1, IN2, INDX, IO2, IN1
      LOGICAL, EXTERNAL :: SWPTST

      NM1 = N - 1
      LMAX = LL
!
! TEST FOR ERRORS AND INITIALIZE FOR LOOP ON INTERIOR ARCS.
!
      IF (NM1 .LT. 2  .OR.  LMAX .LT. 0) GO TO 3
      IER = 0
      L = 0
      INDF = 1
!
! OUTER LOOP ON NODES IO1
!
      DO 2 IO1 = 1,NM1
        INDL = IEND(IO1)
        IN2 = IADJ(INDL)
!
! INNER LOOP ON NEIGHBORS IO2 OF IO1 SUCH THAT IO2 .GT. IO1
!   AND IO1-IO2 IS AN INTERIOR ARC -- (IO1,IO2,IN1) AND
!   (IO2,IO1,IN2) ARE TRIANGLES.
!
        DO 1 INDX = INDF,INDL
          IO2 = IADJ(INDX)
          IN1 = IADJ(INDX+1)
          IF (INDX .EQ. INDL) IN1 = IADJ(INDF)
          IF (IO2 .LT. IO1  .OR.  IN1 .EQ. 0  .OR. IN2 .EQ. 0) GO TO 1
!
! TEST FOR A SWAP.
!
          IF (.NOT. SWPTST(IN1,IN2,IO1,IO2,X,Y)) GO TO 1
          L = L + 1
          IF (L .GT. LMAX) GO TO 1
          LIST(1,L) = IO1
          LIST(2,L) = IO2
    1     IN2 = IO2
    2   INDF = INDL + 1
      LL = L
      RETURN
!
! N OR LL OUT OF RANGE
!
    3 IER = 1
      RETURN
      END
!==============================================================================
! SUBROUTINE CIRCUM
!==============================================================================
      SUBROUTINE CIRCUM (X1,X2,X3,Y1,Y2,Y3, CX,CY,IER)
      INTEGER IER
      REAL    X1, X2, X3, Y1, Y2, Y3, CX, CY
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS SUBROUTINE COMPUTES THE COORDINATES OF THE CENTER
! OF A CIRCLE DEFINED BY THREE POINTS IN THE PLANE.
!
! INPUT PARAMETERS -
!
!       X1,...,Y3 - X AND Y COORDINATES OF THREE POINTS IN
!                   THE PLANE.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETERS -
!
!       CX,CY - COORDINATES OF THE CENTER OF THE CIRCLE
!               UNLESS IER = 1.
!
!       IER - ERROR INDICATOR
!             IER = 0 IF NO ERRORS WERE ENCOUNTERED.
!             IER = 1 IF THE POINTS ARE COLLINEAR.
!
! MODULES REQUIRED BY CIRCUM - NONE
!
!***********************************************************
!
      INTEGER I
      REAL U(3), V(3), DS(3), A, FX, FY
!
! SET U(K) AND V(K) TO THE X AND Y COMPONENTS OF THE EDGE
!   OPPOSITE VERTEX K, TREATING THE POINTS AS VERTICES OF
!   A TRIANGLE.
!
      U(1) = X3 - X2
      U(2) = X1 - X3
      U(3) = X2 - X1
      V(1) = Y3 - Y2
      V(2) = Y1 - Y3
      V(3) = Y2 - Y1
!
! SET A TO TWICE THE SIGNED AREA OF THE TRIANGLE.  A .GT. 0
!   IFF (X3,Y3) IS STRICTLY TO THE LEFT OF THE EDGE FROM
!   (X1,Y1) TO (X2,Y2).
!
      A = U(1)*V(2) - U(2)*V(1)
      IF (A .EQ. 0.) GO TO 2
!
! SET DS(K) TO THE SQUARED DISTANCE FROM THE ORIGIN TO
!   VERTEX K.
!
      DS(1) = X1**2 + Y1**2
      DS(2) = X2**2 + Y2**2
      DS(3) = X3**2 + Y3**2
!
! COMPUTE FACTORS OF CX AND CY.
!
      FX = 0.
      FY = 0.
      DO 1 I = 1,3
        FX = FX - DS(I)*V(I)
    1   FY = FY + DS(I)*U(I)
      CX = FX/2./A
      CY = FY/2./A
      IER = 0
      RETURN
!
! COLLINEAR POINTS
!
    2 IER = 1
      RETURN
      END
!==============================================================================
! FUNCTION LOPTST
!==============================================================================
      INTEGER FUNCTION LOPTST (N1,N2,X,Y,IADJ,IEND)
      INTEGER N1, N2, IADJ(1), IEND(1)
      REAL    X(1), Y(1)
!
!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   GIVEN A PAIR OF INDICES DEFINING A TRIANGULATION ARC,
! THIS FUNCTION DETERMINES WHETHER OR NOT THE ARC IS LOCALLY
! OPTIMAL AS DEFINED BY LOGICAL FUNCTION SWPTST.
!
! INPUT PARAMETERS -
!
!       N1,N2 - X,Y INDICES OF A PAIR OF ADJACENT NODES.
!
!       X,Y - NODAL COORDINATES.
!
!       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
!                   TION.  SEE SUBROUTINE TRMESH.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER -
!
!       LOPTST = -2 IF N1 AND N2 ARE NOT ADJACENT,
!              = -1 IF N1-N2 IS AN INTERIOR ARC WHICH IS
!                   NOT LOCALLY OPTIMAL,
!              =  0 IF N1-N2 SATISFIES THE NEUTRAL CASE (THE
!                   VERTICES OF THE CORRESPONDING QUADRILAT-
!                   ERAL LIE ON A COMMON CIRCLE),
!              =  1 IF N1-N2 IS A LOCALLY OPTIMAL INTERIOR
!                   ARC,
!              =  2 IF N1-N2 IS A BOUNDARY ARC.
!      NOTE THAT N1-N2 IS LOCALLY OPTIMAL IFF LOPTST .GE. 0.
!
! MODULES REQUIRED BY LOPTST - NONE
!
!***********************************************************
!
      INTEGER IO1, IO2, INDF, INDL, INDX, IN1, IN2
      REAL DX11, DX12, DX22, DX21, DY11, DY12, DY22, DY21
      REAL COS1, COS2, SIN12

      IO1 = N1
      IO2 = N2
!
! FIND THE INDEX OF IO2 AS A NEIGHBOR OF IO1
!
      INDF = 1
      IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
      INDL = IEND(IO1)
    1 INDX = INDX - 1
      IF (IADJ(INDX) .EQ. IO2) GO TO 2
      IF (INDX .NE. INDF) GO TO 1
!
! N1 AND N2 ARE NOT ADJACENT
!
      LOPTST = -2
      RETURN
!
! DETERMINE IN1 AND IN2 SUCH THAT (IO1,IO2,IN1) AND
!   (IO2,IO1,IN2) ARE TRIANGLES.
!
    2 IF (INDX .NE. INDL) IN1 = IADJ(INDX+1)
      IF (INDX .EQ. INDL) IN1 = IADJ(INDF)
      IF (INDX .NE. INDF) IN2 = IADJ(INDX-1)
      IF (INDX .EQ. INDF) IN2 = IADJ(INDL)
      IF (IN1 .NE. 0  .AND.  IN2 .NE. 0) GO TO 3
!
! N1-N2 IS A BOUNDARY ARC
!
      LOPTST = 2
      RETURN
!
! COMPUTE COMPONENTS OF THE QUADRILATERAL SIDES.
!
    3 DX11 = X(IO1) - X(IN1)
      DX12 = X(IO2) - X(IN1)
      DX22 = X(IO2) - X(IN2)
      DX21 = X(IO1) - X(IN2)
!
      DY11 = Y(IO1) - Y(IN1)
      DY12 = Y(IO2) - Y(IN1)
      DY22 = Y(IO2) - Y(IN2)
      DY21 = Y(IO1) - Y(IN2)
!
! COMPUTE INNER PRODUCTS.
!
      COS1 = DX11*DX12 + DY11*DY12
      COS2 = DX22*DX21 + DY22*DY21
!
! IO1-IO2 IS LOCALLY OPTIMAL IFF A1+A2 .LE. 180 DEGREES
!   WHERE A1 AND A2 DENOTE THE ANGLES AT IN1 AND IN2.
!
      IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 4
      IF (COS1 .GT. 0.  .AND.  COS2 .GT. 0.) GO TO 5
!
! COMPUTE A QUANTITY WITH THE SIGN OF SIN(A1+A2).
!
      SIN12 = (DX11*DY12 - DX12*DY11)*COS2 + (DX22*DY21 - DX21*DY22)*COS1
      IF (SIN12 .LT. 0.) GO TO 4
      IF (SIN12 .GT. 0.) GO TO 5
!
! NEUTRAL CASE
!
      LOPTST = 0
      RETURN
!
! N1-N2 NOT LOCALLY OPTIMAL
!
    4 LOPTST = -1
      RETURN
!
! N1-N2 LOCALLY OPTIMAL
!
    5 LOPTST = 1
      RETURN
      END
!==============================================================================
!==============================================================================
! FUNCTION STORE
!==============================================================================
!==============================================================================
      REAL FUNCTION STORE (X)
      REAL X

!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS FUNCTION FORCES ITS ARGMENT X TO BE STORED IN MAIN
! MEMORY.  THIS IS USEFUL FOR COMPUTING MACHINE DEPENDENT
! PARAMETERS (SUCH AS THE MACHINE PRECISION) WHERE IT IS
! NECESSARY TO AVOID COMPUTATION IN HIGH PRECISION REG-
! ISTERS.
!
! INPUT PARAMETER -
!
!       X - VALUE TO BE STORED.
!
! X IS NOT ALTERED BY THIS FUNCTION.
!
! OUTPUT PARAMETER -
!
!       STORE - VALUE OF X AFTER IT HAS BEEN STORED AND
!               (POSSIBLY) TRUNCATED OR ROUNDED TO THE
!               MACHINE WORD LENGTH.
!
! MODULES REQUIRED BY STORE - NONE
!
!***********************************************************

      REAL Y

      COMMON/STCOM/Y
      Y = X
      STORE = Y
      RETURN
      END
!==============================================================================
!==============================================================================
! SUBROUTINE TRMTST
!==============================================================================
!==============================================================================
      SUBROUTINE TRMTST (N,X,Y,IADJ,IEND,TOL,LUN, IER)
      INTEGER N, IADJ(1), IEND(N), LUN, IER
      REAL    X(N), Y(N), TOL

!***********************************************************
!
!                                               ROBERT RENKA
!                                       OAK RIDGE NATL. LAB.
!                                             (615) 576-5139
!
!   THIS ROUTINE TESTS THE VALIDITY OF THE DATA STRUCTURE
! REPRESENTING A THIESSEN TRIANGULATION CREATED BY SUBROU-
! TINE TRMESH.  THE FOLLOWING PROPERTIES ARE TESTED --
!   1)  IEND(1) .GE. 3 AND IEND(K) .GE. IEND(K-1)+3 FOR K =
!       2,...,N (EACH NODE HAS AT LEAST THREE NEIGHBORS).
!   2)  0 .LE. IADJ(K) .LE. N FOR K = 1,...,IEND(N) (IADJ
!       ENTRIES ARE NODAL INDICES OR ZEROS REPRESENTING THE
!       BOUNDARY).
!   3)  NB .GE. 3, NT = 2N-NB-2, AND NA = 3N-NB-3 WHERE NB,
!       NT, AND NA ARE THE NUMBERS OF BOUNDARY NODES, TRI-
!       ANGLES, AND ARCS, RESPECTIVELY.
!   4)  EACH CIRCUMCIRCLE DEFINED BY THE VERTICES OF A TRI-
!       ANGLE CONTAINS NO NODES IN ITS INTERIOR.  THIS PROP-
!       ERTY DISTINGUISHES A THIESSEN TRIANGULATION FROM AN
!       ARBITRARY TRIANGULATION OF THE NODES.
! NOTE THAT NO TEST IS MADE FOR THE PROPERTY THAT A TRIANGU-
! LATION COVERS THE CONVEX HULL OF THE NODES, AND THUS A
! TEST ON A DATA STRUCTURE ALTERED BY SUBROUTINE DELETE
! SHOULD NOT RESULT IN AN ERROR.
!
! INPUT PARAMETERS -
!
!       N - NUMBER OF NODES.  N .GE. 3.
!
!       X,Y - NODAL COORDINATES.
!
!       IADJ,IEND - TRIANGULATION DATA STRUCTURE.  SEE SUB-
!                   ROUTINE TRMESH.
!
!       TOL - NONNEGATIVE TOLERANCE TO ALLOW FOR FLOATING-
!             POINT ERRORS IN THE CIRCUMCIRCLE TEST.  AN
!             ERROR SITUATION IS DEFINED AS R**2 - D**2 .GT.
!             TOL WHERE R IS THE RADIUS OF A CIRCUMCIRCLE
!             AND D IS THE DISTANCE FROM THE CIRCUMCENTER
!             TO THE NEAREST NODE.  A REASONABLE VALUE
!             FOR TOL IS 10*EPS WHERE EPS IS THE MACHINE
!             PRECISION.  THE TEST IS EFFECTIVELY BYPASSED
!             BY MAKING TOL LARGER THAN THE DIAMETER OF THE
!             CONVEX HULL OF THE NODES.
!
!       LUN - LOGICAL UNIT NUMBER FOR PRINTING ERROR MES-
!             SAGES.  IF LUN .LT. 1 OR LUN .GT. 99, NO MES-
!             SAGES ARE PRINTED.
!
! INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
!
! OUTPUT PARAMETER -
!
!       IER - ERROR INDICATOR
!             IER = -1 IF ONE OR MORE NULL TRIANGLES (AREA =
!                      0) ARE PRESENT BUT NO (OTHER) ERRORS
!                      WERE ENCOUNTERED.  A NULL TRIANGLE IS
!                      AN ERROR ONLY IF IT OCCURS IN THE
!                      THE INTERIOR.
!             IER = 0 IF NO ERRORS OR NULL TRIANGLES WERE
!                     ENCOUNTERED.
!             IER = 1 IF N .LT. 3 OR TOL .LT. 0.
!             IER = 2 IF AN IEND OR IADJ ENTRY IS OUT OF
!                     RANGE.
!             IER = 3 IF THE TRIANGULATION PARAMETERS (NB,
!                     NT, AND NA) ARE INCONSISTENT.
!             IER = 4 IF A TRIANGLE CONTAINS A NODE INTERIOR
!                     TO ITS CIRCUMCIRCLE.
!             THE ERROR SITUATIONS ARE TESTED IN THE ORDER
!               DEFINED BY THE (POSITIVE) IER VALUES.
!
! MODULE REQUIRED BY TRMTST - CIRCUM
!
!***********************************************************

      LOGICAL RITE
      INTEGER NN, NB, NT, NULL, NFAIL, INDF, INDL, INDX, N1, N2, N3, NA
      INTEGER IERR, I
      REAL    RTOL, CX, CY, R

!==== SET LOCAL VARIABLES, TEST FOR ERRORS IN INPUT, AND
!     INITIALIZE COUNTS.

      NN = N
      RTOL = TOL
      RITE = LUN .GE. 1  .AND.  LUN .LE. 99

!==== N OR TOL IS OUT OF RANGE ERROR.

      IF (NN .LT. 3  .OR.  RTOL .LT. 0.)THEN
        IER = 1
        IF (RITE) WRITE (LUN,110) N, RTOL
        GOTO 9999
      END IF

      NB = 0
      NT = 0
      NULL = 0
      NFAIL = 0

!==== LOOP ON TRIANGLES (N1,N2,N3) SUCH THAT N2 AND N3 INDEX
!       ADJACENT NEIGHBORS OF N1 AND ARE BOTH LARGER THAN N1
!       (TRIANGLES ARE ASSOCIATED WITH THEIR SMALLEST INDEX).

      INDF = 1
      DO N1 = 1,NN

        INDL = IEND(N1)

!====== IEND ENTRY OUT OF RANGE ERROR

        IF (INDL .LT. INDF+2)THEN
          IER = 2
          IF (RITE) WRITE (LUN,120) N1
          GOTO 9999
        END IF

        IF (IADJ(INDL) .EQ. 0) NB = NB + 1

!====== LOOP ON NEIGHBORS OF N1

        DO INDX = INDF,INDL

          N2 = IADJ(INDX)

!======== IADJ ENTRY OUT OF RANGE ERROR

          IF(N2 .LT. 0  .OR.  N2 .GT. NN  .OR. (INDX .LT. INDL  .AND.  N2 .EQ. 0))THEN
            IER = 2
            IF (RITE) WRITE (LUN,130) INDX
            GOTO 9999
          END IF

          IF (INDX .LT. INDL) N3 = IADJ(INDX+1)
          IF (INDX .EQ. INDL) N3 = IADJ(INDF)

          IF (N2 .LT. N1  .OR.  N3 .LT. N1) CYCLE
          NT = NT + 1

!======== COMPUTE THE COORDINATES OF THE CIRCUMCENTER OF
!         (N1,N2,N3).

          CALL CIRCUM (X(N1),X(N2),X(N3),Y(N1),Y(N2),Y(N3), CX,CY,IERR)
          IF (IERR .NE. 0) THEN
            NULL = NULL + 1
            CYCLE
          END IF

!======== TEST FOR NODES WITHIN THE CIRCUMCIRCLE.

          R = (CX-X(N1))**2 + (CY-Y(N1))**2 - RTOL
          IF (R .LE. 0.) CYCLE

          DO I = 1,NN

            IF (I .EQ. N1  .OR.  I .EQ. N2  .OR. I .EQ. N3)CYCLE

!========== NODE I IS INTERIOR TO THE CIRCUMCIRCLE OF (N1,N2,N3).

            IF ((CX-X(I))**2 + (CY-Y(I))**2 .LT. R)THEN
              NFAIL = NFAIL + 1
              EXIT
            END IF

          END DO

        END DO

        INDF = INDL + 1

      END DO

!==== CHECK PARAMETERS FOR CONSISTENCY AND TEST FOR NFAIL = 0.

      NA = (IEND(NN)-NB)/2

!==== INCONSISTENT TRIANGULATION PARAMETERS ERROR

      IF (NB .LT. 3  .OR.  NT .NE. 2*NN-NB-2  .OR. NA .NE. 3*NN-NB-3)THEN
        IER = 3
        IF (RITE) WRITE (LUN,140) NB, NT, NA
        GOTO 9999
      END IF

!==== CIRCUMCIRCLE TEST FAILURE ERROR

      IF (NFAIL .NE. 0)THEN
        IER = 4
        IF (RITE) WRITE (LUN,150) NFAIL
        GOTO 9999
      END IF

!==== NO ERRORS WERE ENCOUNTERED.

      IF (NULL .EQ. 0)THEN
        IER = -1
        IF (RITE) WRITE (LUN,100) NULL
      ELSE
        IER = 0
      END IF

9999  CONTINUE
      RETURN

  100 FORMAT (1H ,10HTRMTST -- ,I5,16H NULL TRIANGLES ,   &
              11HARE PRESENT/1H ,10X,16H(NULL TRIANGLES , &
              32HON THE BOUNDARY ARE UNAVOIDABLE)//)
  110 FORMAT (1H ,33HTRMTST -- INVALID INPUT PARAMETER/ &
              1H ,10X,4HN = ,I5,8H, TOL = ,E8.1)
  120 FORMAT (1H ,15HTRMTST -- NODE ,I5,26H HAS LESS THAN 3 NEIGHBORS)
  130 FORMAT (1H ,33HTRMTST -- IADJ(K) IS NOT A VALID ,14HINDEX FOR K = ,I5)
  140 FORMAT (1H ,33HTRMTST -- INCONSISTENT PARAMETERS/ &
              1H ,10X,I5,15H BOUNDARY NODES,3X,I5,      &
              10H TRIANGLES,3X,I5,5H ARCS)
  150 FORMAT (1H ,10HTRMTST -- ,I5,15H CIRCUMCIRCLES , &
              32HCONTAIN NODES IN THEIR INTERIORS)
      END
