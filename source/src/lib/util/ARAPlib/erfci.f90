MODULE erfci_fi

  SAVE

  INTEGER, PARAMETER :: MAX  = 1001
  REAL(8), PARAMETER :: DLIM = 4.8D0

  LOGICAL erfc_init

  REAL(8), DIMENSION(MAX) ::  xx,yy

END MODULE erfci_fi

!==============================================================================

REAL FUNCTION erfci( arg )

USE erfci_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: arg

REAL(8)  rate,y,dconv
INTEGER  i

IF( .NOT.erfc_init )CALL init_erfci()

y = DBLE(arg)

IF( y < 0.0D0 .OR. y > 1.0D0 )THEN

  erfci = 1.E+36

ELSE IF( y >= yy(1) )THEN

  erfci = -SNGL(DLIM)

ELSE IF( y <= yy(MAX) )THEN

  erfci = SNGL(DLIM)

ELSE

  DO i = 2,MAX
    IF( yy(i) < y )THEN
      rate  = (y - yy(i-1))/(yy(i)-yy(i-1))
      dconv = xx(i-1) + rate*(xx(i)-xx(i-1))
      erfci = SNGL(dconv)
      EXIT
    END IF
  END DO

END IF

RETURN
END

!==============================================================================

SUBROUTINE init_erfci()

USE erfci_fi

IMPLICIT NONE

REAL(8)  dxx,sq2
INTEGER  i

REAL(8), EXTERNAL :: derfc

erfc_init = .TRUE.

sq2 = SQRT(2.0D0)
dxx = 2.D0*DLIM/DBLE(MAX-1)

DO i = 1,MAX
  xx(i) = -DLIM + dxx*DBLE(i-1)
  yy(i) = 0.5D0*derfc( xx(i)/sq2 )
END DO

RETURN
END
