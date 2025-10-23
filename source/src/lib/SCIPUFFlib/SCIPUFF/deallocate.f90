SUBROUTINE scipuff_deallocate()

USE met_fi
USE surface_fi
USE srfaux_fi
USE puff_fi
USE ipgrd_fi
USE matl_fi
USE cont_rel_fi
USE nextRel_fi
USE type_fi
USE cont_rel_functions
USE error_fi
USE sciprime_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER alloc_stat, ios

TYPE( StoreReleaseT ), POINTER :: nextRel

!----- Surface structures

IF( ALLOCATED(srfnam)     )DEALLOCATE( srfnam,STAT=alloc_stat )
IF( ALLOCATED(srftyp)     )DEALLOCATE( srftyp,STAT=alloc_stat )
IF( ALLOCATED(srf_block)  )DEALLOCATE( srf_block,STAT=alloc_stat )
IF( ALLOCATED(srf_effect) )DEALLOCATE( srf_effect,STAT=alloc_stat )
CALL deallocateSrfPuff()
IF( ALLOCATED(evapTemp)     )DEALLOCATE( evapTemp,    STAT=alloc_stat )
IF( ALLOCATED(evapFld)      )DEALLOCATE( evapFld,     STAT=alloc_stat )
IF( ALLOCATED(evapTempID)   )DEALLOCATE( evapTempID,  STAT=alloc_stat )
IF( ASSOCIATED( ibaux_srf ) )THEN
  DEALLOCATE( ibaux_srf )
  NULLIFY( ibaux_srf )
END IF
IF( ASSOCIATED( cmaux_srf ) )THEN
  DEALLOCATE( cmaux_srf )
  NULLIFY( cmaux_srf )
END IF
IF( ASSOCIATED( cvaux_srf ) )THEN
  DEALLOCATE( cvaux_srf )
  NULLIFY( cvaux_srf )
END IF

!----- Puff structures

CALL deallocatePuffs()

IF( ALLOCATED(ipgrd)     )DEALLOCATE( ipgrd,STAT=alloc_stat )
IF( ALLOCATED(npgrd)     )DEALLOCATE( npgrd,STAT=alloc_stat )
IF( ALLOCATED(mxlev_grd) )DEALLOCATE( mxlev_grd,STAT=alloc_stat )

!----- Release structures

ios = deallocate_Collections()
ios = deallocate_Definitions()

!----- Clean up linked list of instantaneous releases

DO WHILE( .TRUE. )

  nextRel => InstReleaseList%NextRelease

  IF( ASSOCIATED(nextRel) )THEN
    IF( ASSOCIATED(nextRel%NextRelease) )THEN
      InstReleaseList%NextRelease => nextRel%NextRelease
    ELSE
      CALL ClearReleaseSpec( InstReleaseList%NextRelease%relSpec )
      NULLIFY( InstReleaseList%NextRelease )
    END IF
    CALL ClearReleaseSpec( nextRel%relSpec )
    DEALLOCATE( nextRel,STAT=alloc_stat )
  ELSE
    EXIT
  END IF

END DO

!----- Washout arrays

IF( ALLOCATED(vwash) )DEALLOCATE( vwash,STAT=alloc_stat )
IF( ALLOCATED(twash) )DEALLOCATE( twash,STAT=alloc_stat )
IF( ALLOCATED(dwash) )DEALLOCATE( dwash,STAT=alloc_stat )

!----- Arrays allocated in read_prj (including MetGrid)

CALL deallocate_read_prj()

!----- McWif vertical grid

IF( ALLOCATED(zMC) )DEALLOCATE( zMC,STAT=alloc_stat )

!----- Sensor structures

CALL ClearSensor()

!----- Materials for checking interactive sources

CALL ClearReleaseCheck()

!----- Arrays for PRIME sources
IF( ALLOCATED(srcid) )THEN
  DEALLOCATE( srcid, iwrk2,isrural,STAT=alloc_stat )
  DEALLOCATE( adsbh,adsbw,adsbl,STAT=alloc_stat )
  DEALLOCATE( adsxadj,adsyadj,STAT=alloc_stat )
ENDIF
CALL exitMC()

CALL ExitAdjointMat()

!----- Surface SAG structures

IF( srfdep > 0 )CALL close_surface( srfdep )
IF( srfdos > 0 )CALL close_surface( srfdos )
IF( srfados > 0 .AND. multicomp )CALL close_surface( srfados )

!---- OpenMP Error structures
IF( ALLOCATED(omp_error))DEALLOCATE( omp_error,STAT=alloc_stat )

RETURN
END

!***********************************************************************
!               DeallocateSrfPuff
!***********************************************************************
SUBROUTINE DeallocateSrfPuff()

USE surface_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, n, m

IF( ALLOCATED(srf_puff) )THEN
  n = SIZE(srf_puff,1)
  m = SIZE(srf_puff,2)
  DO i = 1,n
    DO j = 1,m
      IF( ASSOCIATED( srf_puff(i,j)%iblk ) )THEN
        DEALLOCATE( srf_puff(i,j)%iblk,STAT=alloc_stat )
        NULLIFY(srf_puff(i,j)%iblk)
      END IF
    END DO
  END DO
  DEALLOCATE( srf_puff,STAT=alloc_stat )
END IF

RETURN
END

!=======================================================================

SUBROUTINE close_surface( srf )

USE scipuff_fi
USE sagdef_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: srf

INTEGER irv

INTEGER, EXTERNAL :: SAG_CloseID, SAG_RmvGrdStr

irv = SAG_CloseID( srf )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'close_surface'
  eMessage = 'Error closing surface file'
  GOTO 9999
END IF

irv = SAG_RmvGrdStr( srf )
IF( irv /= SAG_OK )THEN
  nError = UK_ERROR
  eRoutine = 'close_surface'
  eMessage = 'Error deallocating surface structure'
  GOTO 9999
END IF

srf = 0

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE exitMC()

USE scipuff_fi

!Deallocate multicomponent storage

CALL ExitChemMC()

CALL ClearMClist()

RETURN
END

!=======================================================================

SUBROUTINE ClearSensor()

!------ Deallocate sensor array

USE sampler_fi

IMPLICIT NONE

INTEGER i, alloc_stat

INTERFACE

  SUBROUTINE ClearWayPointsList( Point )
    USE sampler_fi, ONLY: WayPoint
    TYPE( WayPoint ), POINTER :: Point
  END SUBROUTINE ClearWayPointsList

  RECURSIVE SUBROUTINE ClearSampClassList( SampList )
    USE sampler_fd
    TYPE( SampClassT ), POINTER :: SampList
  END SUBROUTINE ClearSampClassList

END INTERFACE

IF( ALLOCATED(smp_vname) )DEALLOCATE( smp_vname,STAT=alloc_stat )
IF( ALLOCATED(smp_units) )DEALLOCATE( smp_units,STAT=alloc_stat )
IF( ASSOCIATED(nsmp_dep) )DEALLOCATE( nsmp_dep, STAT=alloc_stat )
IF( ASSOCIATED(nsmp_dos) )DEALLOCATE( nsmp_dos, STAT=alloc_stat )
IF( ALLOCATED(asmp_vname) )DEALLOCATE( asmp_vname,STAT=alloc_stat )

IF( ALLOCATED(depblk_smp) )THEN
  DO i = 1,ndepblk_smp
    IF( ASSOCIATED( depblk_smp(i)%ifld ) )THEN
      DEALLOCATE( depblk_smp(i)%ifld,STAT=alloc_stat )
      NULLIFY( depblk_smp(i)%ifld )
    END IF
  END DO
  DEALLOCATE( depblk_smp,STAT=alloc_stat )
END IF

IF( ALLOCATED(dosblk_smp) )THEN
  DO i = 1,ndosblk_smp
    IF( ASSOCIATED( dosblk_smp(i)%ifld ) )THEN
      DEALLOCATE( dosblk_smp(i)%ifld,STAT=alloc_stat )
      NULLIFY( dosblk_smp(i)%ifld )
    END IF
  END DO
  DEALLOCATE( dosblk_smp,STAT=alloc_stat )
END IF

IF( ALLOCATED(smp) )THEN

  DO i = 1,nsmp
    CALL ClearWayPointsList( smp(i)%nextpoint )
    NULLIFY( smp(i)%nextpoint )
    IF( ASSOCIATED(smp(i)%dsmp) )THEN
      DEALLOCATE(  smp(i)%dsmp,STAT=alloc_stat )
      NULLIFY(     smp(i)%dsmp )
    END IF
    IF( ASSOCIATED(smp(i)%asmp) )THEN
      DEALLOCATE(  smp(i)%asmp,STAT=alloc_stat )
      NULLIFY(     smp(i)%asmp )
    END IF
    IF( ASSOCIATED(smp(i)%mcID) )THEN
      DEALLOCATE(  smp(i)%mcID,STAT=alloc_stat )
      NULLIFY(     smp(i)%mcID )
    END IF
    IF( ASSOCIATED(smp(i)%bin) )THEN
      DEALLOCATE(  smp(i)%bin,STAT=alloc_stat )
      NULLIFY(     smp(i)%bin )
    END IF
    IF( ASSOCIATED(smp(i)%FieldName) )THEN
      DEALLOCATE(  smp(i)%FieldName,STAT=alloc_stat )
      NULLIFY(     smp(i)%FieldName )
    END IF
    IF( ASSOCIATED(smp(i)%FieldUnits) )THEN
      DEALLOCATE(  smp(i)%FieldUnits,STAT=alloc_stat )
      NULLIFY(     smp(i)%FieldUnits )
    END IF
    IF( ASSOCIATED(smp(i)%SizeBin) )THEN
      DEALLOCATE(  smp(i)%SizeBin,STAT=alloc_stat )
      NULLIFY(     smp(i)%SizeBin )
    END IF
  END DO

  DEALLOCATE( smp,STAT=alloc_stat )

END IF

IF( ASSOCIATED(FirstSampClass) )THEN
  CALL ClearSampClassList( FirstSampClass )
END IF

IF( ALLOCATED(intSPSMaterials) )THEN
  DEALLOCATE( intSPSMaterials,STAT=alloc_stat )
END IF

nsmp = 0

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE ClearSampClassList( SampClass )
USE sampler_fd

TYPE( SampClassT ), POINTER :: SampClass

INTEGER alloc_stat

IF( ASSOCIATED(SampClass%next) )THEN
  CALL ClearSampClassList( SampClass%next )
END IF

DEALLOCATE( SampClass,STAT=alloc_stat )

RETURN
END

!=======================================================================

SUBROUTINE ClearWayPointsList( FirstPoint )

USE sampler_fi, ONLY: WayPoint

IMPLICIT NONE

TYPE( WayPoint ), POINTER :: FirstPoint
TYPE( WayPoint ), POINTER :: Point

INTEGER alloc_stat

IF( ASSOCIATED(FirstPoint) )THEN !Do only if list is associated

!------ Go to beginning of list (since may currently
!       be in the middle or end of list)

  point => FirstPoint%prev
  DO WHILE( ASSOCIATED(point) )
    FirstPoint => point
    point => point%prev
  END DO

!------ Go down list and deallocate

  DO WHILE( ASSOCIATED(FirstPoint) )

    Point => FirstPoint%next
    DEALLOCATE( FirstPoint,STAT=alloc_stat )
    NULLIFY( FirstPoint )
    FirstPoint => Point

  END DO

END IF

RETURN
END

!=======================================================================

SUBROUTINE ExitAdjointMat()

USE adjoint_fi
USE surface_fi

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(AdjMat)    )DEALLOCATE( AdjMat,   STAT=ios )
IF( ALLOCATED(srfdosAdj) )DEALLOCATE( srfdosAdj,STAT=ios )

RETURN
END
!***********************************************************************
!               ClearReleaseCheck
!***********************************************************************
SUBROUTINE ClearReleaseCheck()

USE ReleaseCheck_fi

! This cleans up materials used for checking interactive sources

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(mtlList) )THEN
  DEALLOCATE( mtlList,STAT=ios )
  mtlHead%max = 0
END IF

RETURN
END
