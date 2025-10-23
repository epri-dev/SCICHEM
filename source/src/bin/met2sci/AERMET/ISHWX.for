      SUBROUTINE ISHWX( W1, W2, IWX)


c     PURPOSE:    Interprets the TD-3505 (ISHD) present weather codes and
c                 returns a value for the integer variable IWX indicating
c                 the type of precipitation if any.

c     INPUTS:     ISHD 2-digit present weather codes

c     OUTPUTS:    IWX       0 = none
c                           1 = liquid precipitation
c                           2 = frozen precipitation
c                           3 = both liquid and frozen
c                           9 = missing

c     REVISION HISTORY:
c
c                 Modified to use correct codes based on latest ISHD
c                 (TD-3505) documentation.  Currently only looking at
c                 first reported weather code (W1).
c                 R. W. Brode, EPA, AQMG, December 2006
c


      IMPLICIT NONE
      

      INTEGER      IWX, IWX1, IWX2, W1, W2

      IWX  = 0

C---- Evaluate first weather code                          ! rwb #526  06341
      IF(W1 .LE. 19)THEN                        ! None
         IWX1 = 0

      ELSEIF(W1.EQ.22 .OR.
     &      (W1.GE.36 .AND. W1.LE.39) .OR.
     &      (W1.GE.70 .AND. W1.LE.79) .OR.
     &       W1.EQ.85 .OR.  W1.EQ.86)THEN       ! Frozen
         IWX1 = 2

      ELSEIF(W1.EQ.21 .OR. W1.EQ.25)THEN        ! Liquid
         IWX1 = 1

      ELSEIF(W1 .LE. 28)THEN                    ! Mixed
         IWX1 = 3

      ELSEIF(W1 .LE. 49)THEN                    ! None
         IWX1 = 0

      ELSEIF(W1.EQ.56 .OR.  W1.EQ.57  .OR.
     &      (W1.GE.66 .AND. W1.LE.69) .OR.
     &      (W1.GE.83 .AND. W1.LE.90) .OR.
     &       W1.GE.93               )THEN      ! Mixed
         IWX1 = 3

      ELSE                                      ! Liquid
         IWX1 = 1

      ENDIF


C---- Look at second weather code
      IF(W2 .LE. 19)THEN                        ! None
         IWX2 = 0

      ELSEIF(W2.EQ.22 .OR.
     &      (W2.GE.36 .AND. W2.LE.39) .OR.
     &      (W2.GE.70 .AND. W2.LE.79) .OR.
     &       W2.EQ.85 .OR.  W2.EQ.86)THEN       ! Frozen
         IWX2 = 2

      ELSEIF(W2.EQ.21 .OR. W2.EQ.25)THEN        ! Liquid
         IWX2 = 1

      ELSEIF(W2 .LE. 28)THEN                    ! Mixed
         IWX2 = 3

      ELSEIF(W2 .LE. 49)THEN                    ! None
         IWX2 = 0

      ELSEIF(W2.EQ.56 .OR.  W2.EQ.57  .OR.
     &      (W2.GE.66 .AND. W2.LE.69) .OR.
     &      (W2.GE.83 .AND. W2.LE.90) .OR.
     &       W2.GE.93               )THEN      ! Mixed
         IWX2 = 3

      ELSE                                      ! Liquid
         IWX2 = 1

      ENDIF

C---- Combine first and second weather codes
      IF(IWX1.EQ.0 .AND. IWX2.EQ.0)THEN         ! None
         IWX = 0
      ELSEIF(IWX1.EQ.1 .AND. IWX2.EQ.1)THEN     ! Liquid
         IWX = 1
      ELSEIF(IWX1.EQ.2 .AND. IWX2.EQ.2)THEN     ! Frozen
         IWX = 2
      ELSEIF((IWX1.EQ.1 .AND. IWX2.EQ.2) .OR.
     &       (IWX1.EQ.2 .AND. IWX2.EQ.1))THEN   ! Mixed
         IWX = 3
      ELSE
         IWX = MAX(IWX1,IWX2)
      ENDIF

      RETURN
      END

