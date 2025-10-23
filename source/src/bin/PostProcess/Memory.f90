SUBROUTINE ClearMemory()

USE Extract_fi
USE GetTimes_fi
USE SCIPtool

IMPLICIT NONE

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'clearMemory'

INTEGER :: irv, i, ios

IF( nFields > 0 )THEN
  DO i = 1,nFields
    IF( fieldIDs(i) <= 0 )CYCLE
    irv = SCIPDeleteField( callerID, fieldIDs(i) )
    IF( irv /= SCIPsuccess )THEN
      WRITE(6,*)'Error deleting plot field : ',TRIM(ClassStr(Fields(i)%class)%string)
      CALL toolError('Error deleting plot field : '//TRIM(ClassStr(Fields(i)%class)%string))
      CALL showError()
    END IF
  END DO
END IF

IF( doFld )THEN
  IF( ALLOCATED(fieldIDs) )DEALLOCATE( FieldIds,STAT=ios )
  IF( ALLOCATED(Fields)   )DEALLOCATE( Fields,  STAT=ios )
END IF

! Clean up : Deallocate grid memory

IF( ALLOCATED( ClassStr )         )DEALLOCATE( ClassStr,         STAT=irv )
IF( ALLOCATED( ChoiceStr )        )DEALLOCATE( ChoiceStr,        STAT=irv )
IF( ALLOCATED( KindStr )          )DEALLOCATE( KindStr,          STAT=irv )
IF( ALLOCATED( CatClassArray )    )DEALLOCATE( CatClassArray,    STAT=irv )
IF( ALLOCATED( ClassChoiceArray ) )DEALLOCATE( ClassChoiceArray, STAT=irv )

IF( ASSOCIATED( TimePuff )        )DEALLOCATE( TimePuff,         STAT=irv )
IF( ASSOCIATED( TimeSrf  )        )DEALLOCATE( TimeSrf,          STAT=irv )
IF( ASSOCIATED( TimeMet  )        )DEALLOCATE( TimeMet,          STAT=irv )
NULLIFY( TimePuff )
NULLIFY( TimeSrf )
NULLIFY( TimeMet )

IF( ALLOCATED( cPoints      )    )DEALLOCATE( cPoints,           STAT=irv )
IF( ALLOCATED( cLines       )    )DEALLOCATE( cLines,            STAT=irv )
IF( ALLOCATED( contourList  )    )DEALLOCATE( contourList,       STAT=irv )

IF( ALLOCATED( fNodes       )    )DEALLOCATE( fNodes,            STAT=irv )
IF( ALLOCATED( fTriangles   )    )DEALLOCATE( fTriangles,        STAT=irv )

IF( ALLOCATED( xGrd     )        )DEALLOCATE( xGrd,              STAT=irv )
IF( ALLOCATED( yGrd     )        )DEALLOCATE( yGrd,              STAT=irv )
IF( ALLOCATED( mFldGrd  )        )DEALLOCATE( mFldGrd,           STAT=irv )
IF( ALLOCATED( vFldGrd  )        )DEALLOCATE( vFldGrd,           STAT=irv )
IF( ALLOCATED( dFldGrd  )        )DEALLOCATE( dFldGrd,           STAT=irv )
IF( ALLOCATED( zGrd     )        )DEALLOCATE( zGrd,              STAT=irv )

IF( ALLOCATED( ClassData    )    )DEALLOCATE( ClassData,         STAT=irv )
IF( ALLOCATED( MetClassData )    )DEALLOCATE( MetClassData,      STAT=irv )

IF( ALLOCATED( profiles     )    )DEALLOCATE( profiles,          STAT=irv )
IF( ALLOCATED( profileNames )    )DEALLOCATE( profileNames,      STAT=irv )

RETURN
END

!******************************************************************************
!******************************************************************************

SUBROUTINE allocatePlotLists( )

USE Extract_fi

IMPLICIT NONE

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'allocatePlotLists'

INTEGER irv

ALLOCATE( ClassStr(nClass), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating plot class array'
  nError = irv
  GO TO 9999
END IF

ALLOCATE( ChoiceStr(nChoice), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating plot choice array'
  nError = irv
  GO TO 9999
END IF

ALLOCATE( KindStr(nKind), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating plot kind array'
  nError = irv
  GO TO 9999
END IF

ALLOCATE( CatClassArray(HP_NUMCAT,nClass), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating plot category/class array'
  nError = irv
  GO TO 9999
END IF

ALLOCATE( ClassChoiceArray(nClass,nChoice), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating plot class/choice array'
  nError = irv
  GO TO 9999
END IF

9999 CONTINUE

IF( nError /= NO_ERROR )eRoutine=ROUTINE_NAME


RETURN
END
!******************************************************************************
!******************************************************************************

SUBROUTINE allocatePlotTimes( )

USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'allocatePlotTimes'

INTEGER irv, nArray

IF( ASSOCIATED( TimePuff )        )DEALLOCATE( TimePuff,         STAT=irv )
IF( ASSOCIATED( TimeSrf  )        )DEALLOCATE( TimeSrf,          STAT=irv )
IF( ASSOCIATED( TimeMet  )        )DEALLOCATE( TimeMet,          STAT=irv )
NULLIFY( TimePuff )
NULLIFY( TimeSrf )
NULLIFY( TimeMet )

nArray = MAX(1,nTimeSrf)
ALLOCATE( TimeSrf(nArray), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating surface time array'
  nError = irv
  GO TO 9999
END IF

nArray = MAX(1,nTimePuff)
ALLOCATE( TimePuff(nArray), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating puff time array'
  nError = irv
  GO TO 9999
END IF

nArray = MAX(1,nTimeMet)
ALLOCATE( TimeMet(nArray), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating met time array'
  nError = irv
  GO TO 9999
END IF

9999 CONTINUE

IF( nError /= NO_ERROR )eRoutine=ROUTINE_NAME

RETURN
END
!******************************************************************************
!******************************************************************************

SUBROUTINE allocateFields( iDim )

USE Extract_fi

IMPLICIT NONE

INTEGER iDim

INTEGER ios

ALLOCATE( FieldIds(iDim), Fields(iDim) ,STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateFields'
  eMessage = 'Error : allocating FieldIds, Fields arrays'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',nFields,' : error =',ios
  GOTO 9999
END IF

nFields = iDim

9999 CONTINUE

RETURN
END

!******************************************************************************
!******************************************************************************

SUBROUTINE allocateContour( iDim,jDim )

USE Extract_fi

IMPLICIT NONE

INTEGER iDim
INTEGER jDim

INTEGER irv

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'allocateContour'

ALLOCATE( cPoints(iDim), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating contour point array'
  nError = irv
  GOTO 9999
END IF

ALLOCATE( cLines(jDim), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating contour line array'
  nError = irv
  GOTO 9999
END IF

9999 CONTINUE

IF( nError /= NO_ERROR )eRoutine=ROUTINE_NAME

RETURN
END

!******************************************************************************
!******************************************************************************

SUBROUTINE allocateNodes( iDim, jDim)

USE Extract_fi

IMPLICIT NONE

CHARACTER(32), PARAMETER :: ROUTINE_NAME = 'allocateNodes'

INTEGER iDim
INTEGER jDim

INTEGER irv

ALLOCATE( fNodes(iDim), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating output node array'
  nError = irv
  GOTO 9999
END IF

ALLOCATE( fTriangles(jDim), STAT=irv )
IF( irv /= 0 )THEN
  eMessage = 'Error allocating output triangle array'
  nError = irv
  GOTO 9999
END IF

9999 CONTINUE
IF( nError /= NO_ERROR )eRoutine=ROUTINE_NAME

RETURN
END
!==============================================================================
!==============================================================================
! allocateFldGrd
!==============================================================================
!==============================================================================
SUBROUTINE allocateFldGrd(iDim,jDim)

USE Extract_fi

IMPLICIT NONE

INTEGER iDim
INTEGER jDim

INTEGER irv

ALLOCATE( mFldGrd(iDim), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateFldGrid'
  eMessage = 'Error allocating mFldGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',iDim,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( vFldGrd(iDim), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateFldGrid'
  eMessage = 'Error allocating vFldGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',iDim,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( dFldGrd(iDim,jDim,2), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateFldGrid'
  eMessage = 'Error allocating dFldGrd'
  WRITE(eInform,'(A,I0,I0,A,I0)')'Request =',iDim,jDim,' : Error=',irv
  GO TO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
! allocateGrid
!==============================================================================
!==============================================================================
SUBROUTINE allocateGrid(iDim)

USE Extract_fi

IMPLICIT NONE

INTEGER iDim

INTEGER irv

ALLOCATE( xGrd(iDim), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateGrid'
  eMessage = 'Error allocating xGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',iDim,' : Error=',irv
  GO TO 9999
END IF

ALLOCATE( yGrd(iDim), STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'allocateGrid'
  eMessage = 'Error allocating yGrd'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',iDim,' : Error=',irv
  GO TO 9999
END IF

9999 CONTINUE

RETURN
END
!==============================================================================
!==============================================================================
! DeallocateTable
!==============================================================================
!==============================================================================
SUBROUTINE DeallocateTable

USE Extract_fi

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(TableTitle) )DEALLOCATE(TableTitle,STAT=ios)
IF( ALLOCATED(ColTitle)   )DEALLOCATE(ColTitle  ,STAT=ios)
IF( ALLOCATED(RowTitle)   )DEALLOCATE(RowTitle  ,STAT=ios)
IF( ALLOCATED(Table)      )DEALLOCATE(Table     ,STAT=ios)

RETURN
END

!==============================================================================
!==============================================================================
! AllocateTable
!==============================================================================
!==============================================================================
SUBROUTINE AllocateTable(mTable,mCol,mRow)

USE Extract_fi

IMPLICIT NONE

INTEGER mTable
INTEGER mCol
INTEGER mRow

CHARACTER(24) string
INTEGER       na
INTEGER       i
INTEGER       ios

CALL DeallocateTable

IF( mTable <= 0 )GOTO 9998
IF( mCol   <= 0 )GOTO 9998
IF( mRow   <= 0 )GOTO 9998

ALLOCATE( TableTitle(mTable), STAT=ios )
IF( ios /= 0 )THEN
  string = 'TableTitle'
  na     = mTable
  GOTO 9997
END IF
DO i = 1,mTable
  TableTitle(i)%string = 'Testing'
END DO

ALLOCATE( ColTitle(mCol), STAT=ios )
IF( ios /= 0 )THEN
  string = 'ColTitle'
  na     = mCol
  GOTO 9997
END IF
DO i = 1,mCol
  ColTitle(i)%string = 'Column'
END DO

ALLOCATE( RowTitle(mRow), STAT=ios )
IF( ios /= 0 )THEN
  string = 'RowTitle'
  na     = mRow
  GOTO 9997
END IF
DO i = 1,mRow
  RowTitle(i)%string = 'Row'
END DO

ALLOCATE( Table(mCol,mRow,mTable), STAT=ios )
IF( ios /= 0 )THEN
  string = 'Table'
  na     = mCol*mRow*mTable
  GOTO 9997
END IF
Table = 0

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = SZ_ERROR
eRoutine = 'AllocateTable'
eMessage = 'Invalid table array sizes'
WRITE(string,*)mTable
eInform = 'nTable='//TRIM(ADJUSTL(string))
WRITE(string,*)mRow
eInform = TRIM(eInform)//' : nRow='//TRIM(ADJUSTL(string))
WRITE(string,*)mCol
eInform = TRIM(eInform)//' : nCol='//TRIM(ADJUSTL(string))
GOTO 9999

9997 CONTINUE
nError   = SZ_ERROR
eRoutine = 'AllocateTable'
eMessage = 'Error allocating '//TRIM(string)//' array'
WRITE(string,*)ios
eInform = 'IOS='//TRIM(ADJUSTL(string))
WRITE(string,*)na
eInform = TRIM(eInform)//' : Size='//TRIM(ADJUSTL(string))
GOTO 9999

END

