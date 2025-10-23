SUBROUTINE vsorti(x,ix,n)

IMPLICIT NONE

REAL,    DIMENSION(*), INTENT( INOUT ) :: x
INTEGER, DIMENSION(*), INTENT( INOUT ) :: ix
INTEGER,               INTENT( IN )    :: n

INTEGER, DIMENSION(21) :: il,iu

INTEGER nn, i, j, k, l, m, ij, it, itt
REAL    r, t, tt

nn = n
IF( nn <= 1 )RETURN

m = 1
i = 1
j = nn
r = 0.375

125 CONTINUE
k = i

!                       select a central element of the
!                       array and save it in location t

ij = i + IFIX (FLOAT (j-i) * r)
t  = x(ij)
it = ix(ij)

!                       if first element of array is greater
!                       than t, interchange with t

IF( x(i) > t )THEN
  x(ij) = x(i)
  x(i)  = t
  t     = x(ij)
  ix(ij) = ix(i)
  ix(i)  = it
  it     = ix(ij)
END IF
l = j

!                       if last element of array is less than
!                       t, interchange with t

IF( x(j) < t )THEN
  x(ij) = x(j)
  x(j)  = t
  t     = x(ij)
  ix(ij) = ix(j)
  ix(j)  = it
  it     = ix(ij)

!                       if first element of array is greater
!                       than t, interchange with t

  IF( x(i) > t )THEN
    x(ij) = x(i)
    x(i)  = t
    t     = x(ij)
    ix(ij) = ix(i)
    ix(i)  = it
    it     = ix(ij)
  END IF
END IF

!                       find an element in the second half of
!                       the array which is smaller than t

140 CONTINUE
l = l - 1
DO WHILE( x(l) > t )
  l = l - 1
END DO

!                       find an element in the first half of
!                       the array which is greater than t

k = k + 1
DO WHILE( x(k) < t )
  k = k + 1
END DO

!                       interchange these elements

IF( k < l )THEN
  IF( x(k) > x(l) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
    itt   = ix(l)
    ix(l) = ix(k)
    ix(k) = itt
  END IF
  GOTO 140
ELSE IF( k == l )THEN
  k = k + 1
  l = l - 1
END IF

!                       save upper and lower subscripts of
!                       the array yet to be sorted

IF( l-i > j-k )THEN
  il(m) = i
  iu(m) = l
  i = k
  m = m + 1
ELSE
  il(m) = k
  iu(m) = j
  j = l
  m = m + 1
END IF

!                       begin again on another portion of
!                       the unsorted array

155 CONTINUE
IF( j-i > 1 )THEN
  GOTO 125
ELSE IF( j-i == 1 )THEN
  k = j
  l = i
  IF( x(i) > x(j) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
    itt   = ix(l)
    ix(l) = ix(k)
    ix(k) = itt
  END IF
END IF

i = i - 1

165 CONTINUE
i = i + 1
IF( i == j )THEN
  m = m - 1
  IF( m == 0 )GOTO 300
  i = il(m)
  j = iu(m)
  GOTO 155
END IF

t = x(i+1)
it = ix(i+1)
IF( x(i) <= t )GOTO 165
k = i

170 CONTINUE
x(k+1) = x(k)
ix(k+1) = ix(k)
k = k - 1
IF( t < x(k) )GOTO 170
x(k+1)  = t
ix(k+1) = it
GOTO 165

300 CONTINUE

RETURN
END

!==============================================================================
!==============================================================================

SUBROUTINE vsortr(x,ix,n)

IMPLICIT NONE

REAL,    DIMENSION(*), INTENT( INOUT ) :: x
REAL,    DIMENSION(*), INTENT( INOUT ) :: ix
INTEGER,               INTENT( IN )    :: n

INTEGER, DIMENSION(21) :: il,iu

INTEGER nn, i, j, k, l, m, ij
REAL    r, t, tt, it, itt

nn = n
IF( nn <= 1 )RETURN

m = 1
i = 1
j = nn
r = 0.375

125 CONTINUE
k = i

!                       select a central element of the
!                       array and save it in location t

ij = i + IFIX (FLOAT (j-i) * r)
t  = x(ij)
it = ix(ij)

!                       if first element of array is greater
!                       than t, interchange with t

IF( x(i) > t )THEN
  x(ij) = x(i)
  x(i)  = t
  t     = x(ij)
  ix(ij) = ix(i)
  ix(i)  = it
  it     = ix(ij)
END IF
l = j

!                       if last element of array is less than
!                       t, interchange with t

IF( x(j) < t )THEN
  x(ij) = x(j)
  x(j)  = t
  t     = x(ij)
  ix(ij) = ix(j)
  ix(j)  = it
  it     = ix(ij)

!                       if first element of array is greater
!                       than t, interchange with t

  IF( x(i) > t )THEN
    x(ij) = x(i)
    x(i)  = t
    t     = x(ij)
    ix(ij) = ix(i)
    ix(i)  = it
    it     = ix(ij)
  END IF
END IF

!                       find an element in the second half of
!                       the array which is smaller than t

140 CONTINUE
l = l - 1
DO WHILE( x(l) > t )
  l = l - 1
END DO

!                       find an element in the first half of
!                       the array which is greater than t

k = k + 1
DO WHILE( x(k) < t )
  k = k + 1
END DO

!                       interchange these elements

IF( k < l )THEN
  IF( x(k) > x(l) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
    itt   = ix(l)
    ix(l) = ix(k)
    ix(k) = itt
  END IF
  GOTO 140
ELSE IF( k == l )THEN
  k = k + 1
  l = l - 1
END IF

!                       save upper and lower subscripts of
!                       the array yet to be sorted

IF( l-i > j-k )THEN
  il(m) = i
  iu(m) = l
  i = k
  m = m + 1
ELSE
  il(m) = k
  iu(m) = j
  j = l
  m = m + 1
END IF

!                       begin again on another portion of
!                       the unsorted array

155 CONTINUE
IF( j-i > 1 )THEN
  GOTO 125
ELSE IF( j-i == 1 )THEN
  k = j
  l = i
  IF( x(i) > x(j) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
    itt   = ix(l)
    ix(l) = ix(k)
    ix(k) = itt
  END IF
END IF

i = i - 1

165 CONTINUE
i = i + 1
IF( i == j )THEN
  m = m - 1
  IF( m == 0 )GOTO 300
  i = il(m)
  j = iu(m)
  GOTO 155
END IF

t = x(i+1)
it = ix(i+1)
IF( x(i) <= t )GOTO 165
k = i

170 CONTINUE
x(k+1) = x(k)
ix(k+1) = ix(k)
k = k - 1
IF( t < x(k) )GOTO 170
x(k+1)  = t
ix(k+1) = it
GOTO 165

300 CONTINUE

RETURN
END

!==============================================================================
!==============================================================================

SUBROUTINE usort(x,n)

IMPLICIT NONE

REAL,    DIMENSION(*), INTENT( INOUT ) :: x
INTEGER,               INTENT( IN )    :: n

INTEGER, DIMENSION(21) :: il,iu

INTEGER nn, i, j, k, l, m, ij
REAL    r, t, tt

nn = n
IF( nn <= 1 )RETURN

m = 1
i = 1
j = nn
r = 0.375

125 CONTINUE
k = i

!                       select a central element of the
!                       array and save it in location t

ij = i + IFIX (FLOAT (j-i) * r)
t  = x(ij)

!                       if first element of array is greater
!                       than t, interchange with t

IF( x(i) > t )THEN
  x(ij) = x(i)
  x(i)  = t
  t     = x(ij)
END IF
l = j

!                       if last element of array is less than
!                       t, interchange with t

IF( x(j) < t )THEN
  x(ij) = x(j)
  x(j)  = t
  t     = x(ij)

!                       if first element of array is greater
!                       than t, interchange with t

  IF( x(i) > t )THEN
    x(ij) = x(i)
    x(i)  = t
    t     = x(ij)
  END IF
END IF

!                       find an element in the second half of
!                       the array which is smaller than t

140 CONTINUE
l = l - 1
DO WHILE( x(l) > t )
  l = l - 1
END DO

!                       find an element in the first half of
!                       the array which is greater than t

k = k + 1
DO WHILE( x(k) < t )
  k = k + 1
END DO

!                       interchange these elements

IF( k < l )THEN
  IF( x(k) > x(l) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
  END IF
  GOTO 140
ELSE IF( k == l )THEN
  k = k + 1
  l = l - 1
END IF

!                       save upper and lower subscripts of
!                       the array yet to be sorted

IF( l-i > j-k )THEN
  il(m) = i
  iu(m) = l
  i = k
  m = m + 1
ELSE
  il(m) = k
  iu(m) = j
  j = l
  m = m + 1
END IF

!                       begin again on another portion of
!                       the unsorted array

155 CONTINUE
IF( j-i > 1 )THEN
  GOTO 125
ELSE IF( j-i == 1 )THEN
  k = j
  l = i
  IF( x(i) > x(j) )THEN
    tt   = x(l)
    x(l) = x(k)
    x(k) = tt
  END IF
END IF

i = i - 1

165 CONTINUE
i = i + 1
IF( i == j )THEN
  m = m - 1
  IF( m == 0 )GOTO 300
  i = il(m)
  j = iu(m)
  GOTO 155
END IF

t = x(i+1)
IF( x(i) <= t )GOTO 165
k = i

170 CONTINUE
x(k+1) = x(k)
k = k - 1
IF( t < x(k) )GOTO 170
x(k+1)  = t
GOTO 165

300 CONTINUE

RETURN
END
