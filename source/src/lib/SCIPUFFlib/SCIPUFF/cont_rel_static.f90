MODULE createStatics

CONTAINS

!*******************************************************************************
!  CreateStaticPuff
!*******************************************************************************
FUNCTION CreateStaticPuff( def,irel,rel,ipuf,ilev,lev1,lev2,dt0,vapor,nlev ) RESULT( spid )

USE scipuff_fi
USE error_fi
USE met_fi
USE step_p_fi
USE cont_rel_fd
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def        !definition
INTEGER,                  INTENT( IN    ) :: irel       !release index - needed for static ID
TYPE( cont_release_rel ), INTENT( INOUT ) :: rel        !release
INTEGER,                  INTENT( IN    ) :: ipuf
INTEGER,                  INTENT( IN    ) :: ilev
INTEGER,                  INTENT( IN    ) :: lev1
INTEGER,                  INTENT( INOUT ) :: lev2
REAL,                     INTENT( IN    ) :: dt0
LOGICAL,                  INTENT( IN    ) :: vapor
INTEGER,                  INTENT( OUT   ) :: nlev

TYPE( static_release ), POINTER :: spid

INTEGER idtl, sid, ios
REAL    ur, vr, wr

TYPE( puff_dynamics ) pd
TYPE( puff_static   ) ps

INTEGER, EXTERNAL :: reallocatePuffAux
INTEGER, EXTERNAL :: CreateContinuousPuff

NULLIFY(spid)

!----- Release velocity

IF( def%isMoving )THEN
  ur = def%vel%x
  vr = def%vel%y
  wr = def%vel%z
ELSE
  ur = 0.0
  vr = 0.0
  wr = 0.0
END IF

!----- Set release dynamic vertical velocity

IF( dynamic )THEN
  CALL get_dynamics( rel%relPuff(ipuf),pd )
  IF( rel%relPuff(ipuf)%c > SMALL )THEN
  udyn = pd%ucb/rel%relPuff(ipuf)%c
  vdyn = pd%vcb/rel%relPuff(ipuf)%c
  wdyn = pd%wcb/rel%relPuff(ipuf)%c
  ELSE
  udyn = 0.0
  vdyn = 0.0
  wdyn = 0.0
  END IF
ELSE
  udyn = 0.0
  vdyn = 0.0
  wdyn = 0.0
END IF

!----- Create the initial static puff

idtl = CreateContinuousPuff( def,rel%relPuff(ipuf),.FALSE.,.TRUE., &
                             dt0,0.0,lev1,lev2,rel%plen,(ipuf==1) )

IF( idtl < 0 )GOTO 9999

nlev = idtl

ALLOCATE( spid,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateStaticPuff'
  eMessage = 'Error allocating static release group'
  WRITE(eInform,'(A,I0)')'IOS=',ios
  GOTO 9999
END IF

spid%sp%ipuf = npuf

puff(npuf)%idtl = ilev

CALL siginv( puff(npuf) )

!----- Setup statics

ios = reallocatePuffAux( puff(npuf),puff(npuf)%naux+NAUX_STATIC )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateStaticPuff'
  eMessage = 'Error reallocating puff aux array to include statics'
  WRITE(eInform,'(A,I0)')'IOS=',ios
  GOTO 9999
END IF

spid%sp%rid%idef = def%ID
spid%sp%rid%irel = irel
spid%sp%rid%ipuf = ipuf

sid = staticID(spid%sp%rid)

ps%sr    = 0.0
ps%isnxt = -sid
ps%isprv = -sid
CALL put_static( puff(npuf),ps )

!----- Save velocites for stepping

spid%sp%vel%x = 0.5*(ub-ur+udyn)*dt0            !NB - uses velocities for "real" releases
spid%sp%vel%y = 0.5*(vb-vr+vdyn)*dt0            !for additional nliq vapor releases
spid%sp%vel%z = 0.5*(wb-wr+wdyn)*dt0

spid%sp%qqx = qqb*dt0*dt0

!----- Zero release puff, in case release is depleted during initialization

rel%relPuff(ipuf)%c = 0.0

!----- Set static release puff pointers

rel%relPuff(ipuf)%iprv = npuf    !Last puff in static list
rel%relPuff(ipuf)%inxt = npuf    !First puff in static list

IF( vapor )THEN

!----- Create the initial static puff for vapor

  CALL check_newpuff()
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'CreateStaticPuff'
    GOTO 9999
  END IF

  spid%liquid = .TRUE.

  npuf = npuf + 1

  spid%vp%ipuf = npuf

  CALL copy_puff( rel%vapPuff(ipuf),puff(npuf) )

  puff(npuf)%idtl = ilev

  CALL siginv( puff(npuf) )

  !----- Setup statics

  ios = reallocatePuffAux( puff(npuf),puff(npuf)%naux+NAUX_STATIC )

  spid%vp%rid%idef = def%ID
  spid%vp%rid%irel = irel
  spid%vp%rid%ipuf = ipuf + rel%npuff

  sid = staticID(spid%vp%rid)

  ps%sr    = 0.0
  ps%isnxt = -sid
  ps%isprv = -sid
  CALL put_static( puff(npuf),ps )

  !----- Save velocities - use associated real puff values for the vapor puff initial values

  spid%vp%vel = spid%sp%vel
  spid%vp%qqx = spid%sp%qqx

  !----- Zero release puff, in case release is depleted during initialization

  rel%vapPuff(ipuf)%c = 0.0

  !----- Set static release puff pointers

  rel%vapPuff(ipuf)%iprv = npuf    !Last puff in static list
  rel%vapPuff(ipuf)%inxt = npuf    !First puff in static list

ELSE
  spid%liquid      = .FALSE.
  spid%vp%rid%idef = 0
  spid%vp%rid%irel = 0
  spid%vp%rid%ipuf = 0
  spid%vp%ipuf     = 0
  spid%vp%vel%x    = 0.0
  spid%vp%vel%y    = 0.0
  spid%vp%vel%z    = 0.0
  spid%vp%qqx      = 0.0
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
!  newListEntry
!*******************************************************************************
FUNCTION newListEntry( stat ) RESULT( next )

USE error_fi
USE cont_rel_fd

IMPLICIT NONE

TYPE( static_release      ), POINTER :: stat

TYPE( static_release_list ), POINTER :: next

INTEGER ios

ALLOCATE( next,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'newListEntry'
  eMessage = 'Error allocating new element in static linked list'
  WRITE(eInform,'(A,I0)')'IOS=',ios
  GOTO 9999
END IF
NULLIFY( next%stat )
NULLIFY( next%next )
NULLIFY( next%prev )

IF( ASSOCIATED(stat) )next%stat => stat

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
!  addListEntry
!*******************************************************************************
FUNCTION addListEntry( list,stat ) RESULT( next )

USE error_fi
USE cont_rel_fd

IMPLICIT NONE

TYPE( static_release_list ), POINTER :: list
TYPE( static_release      ), POINTER :: stat

TYPE( static_release_list ), POINTER :: next

next => newListEntry( stat )
IF( nError /= NO_ERROR )GOTO 9999

list%next => next
next%prev => list

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
!  removeListEntry
!*******************************************************************************
FUNCTION removeListEntry( list ) RESULT( last )

USE error_fi
USE cont_rel_fd

IMPLICIT NONE

TYPE( static_release_list ), POINTER :: list

TYPE( static_release_list ), POINTER :: last

INTEGER ios

NULLIFY( last )

IF( ASSOCIATED(list) )THEN
  IF( ASSOCIATED(list%stat) )THEN
    DEALLOCATE( list%stat,STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'removeListEntry'
      eMessage = 'Error deallocating static in static linked list'
      WRITE(eInform,'(A,I0)')'IOS=',ios
      GOTO 9999
    END IF
    NULLIFY( list%stat )
  END IF

  IF( ASSOCIATED(list%prev) )THEN
    last => list%prev
    NULLIFY( last%next )
  END IF

  DEALLOCATE( list,STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'removeListEntry'
    eMessage = 'Error deallocating element in static linked list'
    WRITE(eInform,'(A,I0)')'IOS=',ios
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END FUNCTION
!*******************************************************************************
!  getRelPuff
!*******************************************************************************
FUNCTION getRelPuff( icls,sid ) RESULT( p )

USE puffstruct_fd
USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icls
INTEGER, INTENT( IN ) :: sid

TYPE( cont_release_id ) :: rid

TYPE( puff_str         ), POINTER :: p
TYPE( cont_release_rel ), POINTER :: rel

NULLIFY( rel )
NULLIFY( p )

rid = relPuffID(sid)

rel => getRel(icls,rid)

IF( rid%ipuf > rel%npuff )THEN
  p => rel%vapPuff(rid%ipuf-rel%npuff)
ELSE
  p => rel%relPuff(rid%ipuf)
END IF

9999 CONTINUE

NULLIFY( rel )

RETURN
END FUNCTION
!*******************************************************************************
!  getRel
!*******************************************************************************
FUNCTION getRel( icls,rid ) RESULT( rel )

USE puffstruct_fd
USE error_fi
USE cont_rel_fi

IMPLICIT NONE

INTEGER                , INTENT(IN) :: icls
TYPE( cont_release_id ), INTENT(IN) :: rid

TYPE( cont_release_rel ), POINTER :: rel
TYPE( cont_release_set ), POINTER :: set

NULLIFY( set )
NULLIFY( rel )

set => cDefinition(rid%idef)%rSet

rel => set%rels(rid%irel)

9999 CONTINUE

NULLIFY( set )

RETURN
END FUNCTION

END MODULE createStatics
!*******************************************************************************
!
!*******************************************************************************
!*******************************************************************************
!  CheckStaticContinuousReleases
!*******************************************************************************
SUBROUTINE CheckStaticContinuousReleases( tstep,lev1 )

USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,    INTENT( IN    ) :: tstep      ! end time for current large timestep
INTEGER, INTENT( IN    ) :: lev1       ! current time level

INTEGER icol

INTERFACE
  SUBROUTINE CreateStaticContinuousRelease( tstep,col,lev )
    USE cont_rel_fd
    REAL,                             INTENT( IN    ) :: tstep      ! time
    TYPE( cont_release_col ), TARGET, INTENT( INOUT ) :: col        ! Collection
    INTEGER,                          INTENT( IN    ) :: lev        ! current time level
  END SUBROUTINE CreateStaticContinuousRelease
END INTERFACE

!==== Init statics

IF( initStatics )THEN
  initStatics = .FALSE.
  CALL InitStaticContinuousReleases()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Run statics

DO icol = 1,numCollection
  IF( cCollection(icol)%isPool )CYCLE
  IF( cCollection(icol)%isActive )THEN
    IF( cCollection(icol)%rStat%doStatics )THEN
      CALL CreateStaticContinuousRelease( tstep,cCollection(icol),lev1 )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  InitStaticContinuousReleases
!*******************************************************************************
SUBROUTINE InitStaticContinuousReleases()

USE scipuff_fi
USE surface_fi
USE error_fi
USE cont_rel_fi
USE met_fi
USE cont_rel_functions
USE default_fd

IMPLICIT NONE

INTEGER icol
REAL    delmax, xmap, ymap

!----- Set large distance for interacting releases

CALL mapfac( 0.5*(xmin+xmax),0.5*(ymin+ymax),xmap,ymap )
delmax = SQRT( ((xmax-xmin)/xmap)**2 + ((ymax-ymin)/ymap)**2 )
delmax = 100.*delmax

!==== Initialize del for all Collections with doStatic=T

DO icol = 1,numCollection
  IF( .NOT.cCollection(icol)%isActive )CYCLE
  IF(      cCollection(icol)%isPool   )CYCLE
  cCollection(icol)%rStat%doStatics = .TRUE.
  cCollection(icol)%rStat%del       = delmax
END DO

RETURN
END
!*******************************************************************************
!  TestStaticContinuousReleases
!*******************************************************************************
SUBROUTINE TestStaticContinuousReleases( tstep,col )

USE scipuff_fi
USE surface_fi
USE error_fi
USE cont_rel_fi
USE met_fi
USE cont_rel_functions
USE default_fd

IMPLICIT NONE

REAL,    INTENT( IN    )                          :: tstep      ! end time for current large timestep
TYPE( cont_release_col ), TARGET, INTENT( INOUT ) :: col        ! Collection

INTEGER irel
INTEGER icls, ifld, ityp
INTEGER ipuf
REAL    vel, velmax, cpuff
REAL    h, hx, hy, ur, vr, wr, dtx, diffTime, firstTime, diffT
LOGICAL lsrf, lsrfm
LOGICAL ldyn
LOGICAL lextraUpdate

TYPE( cont_release_def ),    POINTER :: def
TYPE( cont_release_set ),    POINTER :: set
TYPE( cont_release_static ), POINTER :: stat
TYPE( puff_str ),            POINTER :: p
TYPE( puff_dynamics ) pd

INTEGER, EXTERNAL :: getPuffifld
LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid

!---- Initialize pointers

NULLIFY( def )
NULLIFY( set )
NULLIFY( stat )
NULLIFY( p )

!==== Loop collection and compute static times

stat => col%rStat
firstTime = NOT_SET_R
diffTime  = DEF_VAL_R
IF( stat%doStatics )THEN
  stat%tstat = 0.25*delt
  stat%dur   = DEF_VAL_R
  stat%step  = delt
  velmax     = 0.0
  lsrf  = .FALSE.
  lsrfm = .FALSE.
  ldyn  = .FALSE.
  lextraUpdate = .FALSE.
  def => col%firstDef
  DO WHILE( ASSOCIATED(def) )
    IF( def%state /= CR_EMPTY )THEN
      IF( firstTime == NOT_SET_R )THEN
        firstTime = def%time
      ELSE
        diffT = ABS(def%time-firstTime)
        IF( diffT /= 0.0 )diffTime = MIN(diffTime,diffT)
      END IF
    END IF
    IF( def%state == CR_ACTIVE )THEN
      lextraUpdate = lextraUpdate .OR. def%extraUpdate
      IF( .NOT. lextraUpdate )THEN
        set => def%rSet
        IF( set%nrel > 0 )THEN
          stat%tstat = MIN(stat%tstat,0.1*def%dur)
          DO irel = 1,set%nrel
            p => set%rels(irel)%basePuff
            ityp = p%ityp
            icls = typeID(ityp)%icls
            lsrf = lsrf .OR. srf_puff(ityp,1)%nblocks > 0 .OR. srf_puff(ityp,2)%nblocks > 0

            stat%step = MIN(stat%step,tstep-set%rels(irel)%time)
            stat%dur  = MIN(stat%dur,def%end-set%rels(irel)%time)

            ifld = getPuffifld( set%rels(irel)%basePuff )
            IF( lter )THEN
              CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h,hx,hy,ifld )
              IF( nError /= NO_ERROR )GOTO 9999
            ELSE
              h = 0.
            END IF

            IF( def%isMoving )lsrfm = lsrfm .OR. p%zbar-h < 3.*SQRT(p%szz)

            CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,p%szz,0.0,0,inField=ifld )
            IF( lsv_oper )CALL reset_lsv( p%si )
            ur  = ub - def%vel%x
            vr  = vb - def%vel%y
            wr  = wb - def%vel%z
            qqb = uub + vvb + 2.*(uubl+vvbl) + wwbh + 1.E-6
            vel = SQRT(ur*ur + vr*vr + wr*wr + qqb)
            velmax = MAX(velmax,vel)

!------ No statics if dynamic jet velocity is near to reversal

            IF( dynamic )THEN
              CALL get_dynamics( set%rels(irel)%basePuff,pd )
              cpuff = set%rels(irel)%basePuff%c
             IF( cpuff > SMALL )THEN
              IF( ub*(ub+pd%ucb/cpuff)+vb*(vb+pd%vcb/cpuff) < 0.0 )THEN
                ldyn = ldyn .OR. ((ub+pd%ucb/cpuff)**2+(vb+pd%vcb/cpuff)**2 < 0.5*(ub*ub+vb*vb))
              END IF
             END IF
            END IF
            NULLIFY( p )
          END DO
        END IF
        NULLIFY( set )
      END IF
    END IF
    def => def%nextDef
  END DO
  NULLIFY( def )
  dtx = delt/(2.**stat%ilev)
  stat%tstat = MIN(stat%tstat,0.5*(stat%del/velmax))
  stat%tstat = MIN(stat%tstat,0.25*stat%step)
  IF( diffTime /= DEF_VAL_R .AND. diffTime > 0.0 )stat%tstat = MIN(stat%tstat,0.5*diffTime)
  stat%doStatics = (dtx <= 0.1*stat%tstat) .AND. .NOT.(lsrf .AND. lsrfm)
  IF( ldyn )stat%doStatics = .FALSE.
  IF( lextraUpdate )stat%doStatics = .FALSE.

!------ create vapor puffs for definitions of liquid materials that will do statics

  IF( stat%doStatics )THEN
    def => col%firstDef
    DO WHILE( ASSOCIATED(def) )
      IF( def%state == CR_ACTIVE )THEN
        IF( IsLiquid(material(def%mID)%icls) )THEN
          set => def%rSet
          set%tlev = stat%ilev
          IF( set%nrel > 0 )THEN
            DO irel = 1,set%nrel
              set%rels(irel)%basePuff%idtl = stat%ilev
              DO ipuf = 1,set%rels(irel)%npuff
                set%rels(irel)%relPuff(ipuf)%idtl = stat%ilev
              END DO
              IF( IsLiquid(typeID(set%rels(irel)%basePuff%ityp)%icls) )THEN
                CALL allocate_contRel_vapor( set%rels(irel) )
                IF( nError /= NO_ERROR )GOTO 9999
                DO ipuf = 1,set%rels(irel)%npuff
                  CALL create_StaticVaporPuff( def%mID,set%rels(irel)%relPuff(ipuf),set%rels(irel)%vapPuff(ipuf) )
                  IF( nError /= NO_ERROR )GOTO 9999
                END DO
              END IF
            END DO
          END IF
        END IF
      END IF
      def => def%nextDef
    END DO
    NULLIFY( def )
  END IF
END IF

NULLIFY( stat )

9999 CONTINUE

NULLIFY( set )
NULLIFY( def )
NULLIFY( stat )
NULLIFY( p )

RETURN
END
!*******************************************************************************
!  create_StaticVaporPuff
!*******************************************************************************
SUBROUTINE create_StaticVaporPuff( imat,prel,pvap )

USE scipuff_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: imat
TYPE( puff_str ), INTENT( IN    ) :: prel
TYPE( puff_str ), INTENT( OUT   ) :: pvap

INTEGER ityp, naux, ios

INTEGER, EXTERNAL :: allocatePuffAux

ityp = material(imat)%ioffp + 1
naux = typeID(ityp)%npaux

CALL copy_puff_noaux( prel,pvap )

pvap%naux = naux
ios = allocatePuffAux( pvap )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'create_StaticVaporPuff'
  eMessage = 'Error allocating scipuff puff auxiliary array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',pvap%naux,'  : Error = ',ios
  GOTO 9999
END IF

!----- Reset type

pvap%ityp = ityp

!----- Zero mass

pvap%c  = 0.0
pvap%cc = 0.0

!----- Set unit sigmas for interactions

pvap%sxx = 1.0
pvap%sxy = 0.0
pvap%sxz = 0.0
pvap%syy = 1.0
pvap%syz = 0.0
pvap%szz = 1.0

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  CreateStaticContinuousRelease
!*******************************************************************************
SUBROUTINE CreateStaticContinuousRelease( tstep,col,lev )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE met_fi
USE step_p_fi
USE surface_fi
USE files_fi
USE cont_rel_functions
USE createStatics
USE omp_lib
USE grdlock_fi
USE ipgrd_fi

IMPLICIT NONE

REAL,                             INTENT( IN    ) :: tstep      ! time
TYPE( cont_release_col ), TARGET, INTENT( INOUT ) :: col        ! Collection
INTEGER,                          INTENT( IN    ) :: lev        ! current time level

INTEGER irel, ipuf, ilev, jpuf, nsrel, ios, ilev0, ityp, jlev
INTEGER icls, sid, ilevm, ifld, is, lev1, lev2, isprv
INTEGER npuf0, jpufs, jtyp, npufr, iprv, inxt, k, icell
REAL    dt0, rat, tr, velm, um, vm, wm, ux, uy, uz, uxo, uyo, uzo
REAL    fac_srf, dtm, sigm, dt, t_srf, chkx, dt_rel
REAL    s1, s2, arg
REAL(8) betx, bety, betz
LOGICAL lRmvPUff, lwet, l1, l2
LOGICAL lsplit, lrealizable, lfirst, lpuf, lmov
LOGICAL vapor
LOGICAL myOK
REAL    rat1, rat2, rat3, sly

TYPE( static_release        ), DIMENSION(:), ALLOCATABLE :: staticRel
TYPE( cont_release_triad_R4 )                            :: vel

TYPE ( puff_static   ) ps, pso
TYPE ( puff_dynamics ) pd

TYPE( cont_release_static ), POINTER :: stat
TYPE( cont_release_def    ), POINTER :: def
TYPE( cont_release_set    ), POINTER :: set
TYPE( cont_release_rel    ), POINTER :: rel

TYPE( puff_str            ), POINTER :: prel

TYPE( static_release_list ), POINTER :: listFirst
TYPE( static_release_list ), POINTER :: listLast
TYPE( static_release_list ), POINTER :: listNext
TYPE( static_release      ), POINTER :: thisStatic

INTEGER, EXTERNAL :: time_level, getPuffifld, deallocatePuffAux, getPuffirel, getPuffiskew
REAL,    EXTERNAL :: DzSplit, DxSplit, overlp_merge
LOGICAL, EXTERNAL :: PuffRealizable, IsWetParticle, check_wetpart_merge
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: dense_effect

INTERFACE
  SUBROUTINE TestStaticContinuousReleases( tstep,col )
    USE cont_rel_fd
    REAL,    INTENT( IN    )                          :: tstep      ! end time for current large timestep
    TYPE( cont_release_col ), TARGET, INTENT( INOUT ) :: col        ! Collection
  END SUBROUTINE TestStaticContinuousReleases
END INTERFACE

NULLIFY( stat )
NULLIFY( def )
NULLIFY( set )
NULLIFY( rel )

NULLIFY( prel )

NULLIFY( listFirst )
NULLIFY( listLast )
NULLIFY( listNext )
NULLIFY( thisStatic )

stat => col%rStat

IF( stat%tlev < lev )GOTO 9999

lRmvPuff = (t <= t_rst_static .AND. delt_static < delt) !Check if need to remove overlapping puffs with restart
npuf0 = npuf

CALL TestStaticContinuousReleases( tstep,col )
IF( nError /= NO_ERROR )GOTO 9999

IF( .NOT.stat%doStatics )GOTO 9999

stat%doStatics = .FALSE.

lev1 = 0
lev2 = 0
ilev = stat%ilev
dt0 = delt/2.**ilev

stat%ilev = 0
def => col%firstDef
DO WHILE( ASSOCIATED(def) )
  IF( def%state == CR_ACTIVE )THEN

    set => def%rSet
    DO irel = 1,set%nrel
      vapor = ASSOCIATED(set%rels(irel)%vapPuff)
      set%rels(irel)%isStatic = .TRUE.
      CALL setPuffipgrd( set%rels(irel)%basePuff,2 )  !Still needed?
      DO ipuf = 1,set%rels(irel)%npuff
        thisStatic => CreateStaticPuff( def,irel,set%rels(irel),ipuf,ilev,lev1,lev2,dt0,vapor,jlev )
        IF( ASSOCIATED(thisStatic) )THEN
          IF( ASSOCIATED(listLast) )THEN
            listLast => addListEntry( listLast,thisStatic )
            IF( nError /= NO_ERROR )GOTO 9999
          ELSE
            listFirst => newListEntry( thisStatic )
            IF( nError /= NO_ERROR )GOTO 9999
            listLast => listFirst
          END IF
          stat%ilev = MAX(stat%ilev,jlev)
        ELSE
          set%rels(irel)%isStatic = .FALSE.
          DO jpuf = 1,ipuf-1
            listLast => removeListEntry( listLast )
            IF( nError /= NO_ERROR )GOTO 9999
          END DO
          EXIT
        END IF
        NULLIFY( thisStatic )
      END DO
    END DO
    NULLIFY( set )
  END IF
  def => def%nextDef
END DO
NULLIFY( def )

nsrel = 0
listNext => listFirst
DO WHILE( ASSOCIATED(listNext) )
  nsrel = nsrel + 1
  listNext => listNext%next
END DO
NULLIFY( listNext )

IF( nsrel == 0 )GOTO 9999

stat%hasStatics = .TRUE.

ALLOCATE( staticRel(nsrel),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateStaticContinuousRelease'
  eMessage = 'Error allocating static release array'
  WRITE(eInform,'(A,I0)')'IOS=',ios
  GOTO 9999
END IF

ipuf = 0
listNext => listFirst
DO WHILE( ASSOCIATED(listNext) )
  ipuf = ipuf + 1
  staticRel(ipuf) = listNext%stat
  listNext => listNext%next
END DO
NULLIFY( listNext )

DO WHILE( ASSOCIATED(listLast) )
  listLast => removeListEntry( listLast )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

!----- Set variables for static creation loop

tr    = dt0
rat   = 1.0
ilev0 = ilev

lsplit      = .FALSE.
lrealizable = .TRUE.

IF( col%firstDef%isMoving )THEN  !If one is moving, they all are
  lmov = .TRUE.
  um   = col%firstDef%vel%x
  vm   = col%firstDef%vel%y
  wm   = col%firstDef%vel%z
  velm = SQRT(um**2 + vm**2 + wm**2)
ELSE
  lmov = .FALSE.
  um   = 0.0
  vm   = 0.0
  wm   = 0.0
  velm = 0.0
END IF

t_srf = MIN(stat%dur,stat%step)

myTID = 1
myOK  = ManageGrdLock( nthreads,2,INIT_GRDLOCK )

IF( nEvapBlk > 0 )CALL ClearEvapTemp()

!----- Loop creating static puffs

StaticCreate: DO WHILE( tr <= stat%tstat .AND. .NOT.lsplit .AND. lrealizable )

  CALL InteractStatics( nsrel,staticRel )
  IF( nError /= NO_ERROR )GOTO 9999

  lfirst = .TRUE.

  RelLoop:  DO is = 1,nsrel
    lpuf = staticRel(is)%liquid
    IF( lpuf )THEN
      ipuf = staticRel(is)%vp%ipuf
      sid  = staticID(staticRel(is)%vp%rid)
      uxo  = staticRel(is)%vp%vel%x
      uyo  = staticRel(is)%vp%vel%y
      uzo  = staticRel(is)%vp%vel%z
    ELSE
      ipuf = staticRel(is)%sp%ipuf
      sid  = staticID(staticRel(is)%sp%rid)
      uxo  = staticRel(is)%sp%vel%x
      uyo  = staticRel(is)%sp%vel%y
      uzo  = staticRel(is)%sp%vel%z
    END IF
    DO WHILE( ipuf > 0 )

      IF( puff(ipuf)%c <= 0.0 )THEN
        puff(ipuf)%idtl = ilev0
        IF( lpuf )THEN
          lpuf = .FALSE.
          ipuf = staticRel(is)%sp%ipuf
          sid  = staticID(staticRel(is)%sp%rid)
          uxo  = staticRel(is)%sp%vel%x
          uyo  = staticRel(is)%sp%vel%y
          uzo  = staticRel(is)%sp%vel%z
        ELSE
          ipuf = 0
        END IF
        CYCLE
      END IF

      ityp = puff(ipuf)%ityp
      icls = typeID(ityp)%icls

      CALL check_newpuff()
      IF( nError /= NO_ERROR )THEN
        eRoutine = 'CreateStaticContinuousRelease'
        GOTO 9999
      END IF

      npuf = npuf + 1

!-------- set static time level for old puff

      puff(ipuf)%idtl = I_STATIC

!-------- create new puff

      CALL copy_puff( puff(ipuf),puff(npuf) )

      CALL get_static( puff(ipuf),ps )
      ps%isnxt = npuf
      CALL put_static( puff(ipuf),ps )

      ps%isprv = ipuf
      ps%isnxt = -sid
      CALL put_static( puff(npuf),ps )

!----- Advance static puff

      IF( lfirst )THEN
        dt      = delt/2.**ilev
        lev1    = MAX(0,ilev-1)
        fac_srf = rat*t_srf/dt
      END IF

      jpuf = npuf
      CALL step_p( dt,puff(jpuf),jpuf,lev1,lev2,fac_srf )
      IF( nError /= NO_ERROR )GOTO 9999

      ux = 0.5*(ub-um+udyn)*dt
      uy = 0.5*(vb-vm+vdyn)*dt
      uz = 0.5*(wb-wm+wdyn)*dt

!----- Reset type and class since step_p can change wet particles to dry

      ityp = puff(jpuf)%ityp
      icls = typeID(ityp)%icls

!----- Merge any vapor puffs into temporary

      IF( IsLiquid(icls) .AND. jpuf < npuf )THEN
        CALL ScaleStaticPuff( puff(npuf),rat )
        IF( nError /= NO_ERROR )GOTO 9999
        puff(npuf)%sxx = puff(npuf)%sxx + ux*ux - uxo*uxo
        puff(npuf)%sxy = puff(npuf)%sxy + ux*uy - uxo*uyo
        puff(npuf)%sxz = puff(npuf)%sxz + ux*uz - uxo*uzo
        puff(npuf)%syy = puff(npuf)%syy + uy*uy - uyo*uyo
        puff(npuf)%syz = puff(npuf)%syz + uy*uz - uyo*uzo
        puff(npuf)%szz = puff(npuf)%szz + uz*uz - uzo*uzo
        vel%x = ux
        vel%y = uy
        vel%z = uz
        CALL MergeVaporPuff( puff(npuf),puff(staticRel(is)%vp%ipuf),vel,staticRel(is)%vp%vel )
        ios = deallocatePuffAux( puff(npuf) )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'CreateStaticContinuousRelease'
          eMessage = 'Error deallocating vapor puff aux array'
          WRITE(eInform,'(A,I0)')'IOS=',ios
          GOTO 9999
        END IF
        npuf = npuf - 1
      END IF

!---- If puff mass is reduced to zero, terminate statics

      prel => getRelPuff(icls,sid)
      IF( .NOT. lpuf .AND. puff(npuf)%c == 0.0 )THEN
        prel%iprv = ipuf
        prel%idtl = 0
        CALL get_static( puff(ipuf),ps )
        ps%isnxt = -sid
        CALL put_static( puff(ipuf),ps )
        ios = deallocatePuffAux( puff(npuf) )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'CreateStaticContinuousRelease'
          eMessage = 'Error deallocating zero mass puff aux array'
          WRITE(eInform,'(A,I0)')'IOS=',ios
          GOTO 9999
        END IF
        npuf = npuf - 1
        lfirst = .FALSE.
        staticRel(is)%sp%ipuf  = 0
        ipuf = 0
        CYCLE
      ELSE
        prel%iprv = npuf
      END IF
      NULLIFY( prel )

!----- Reset all puffs to same time level as first puff

      IF( lfirst )THEN
        ilev0 = puff(npuf)%idtl
      ELSE
        ilev0 = MAX(puff(npuf)%idtl,ilev0)
      END IF

!----- Set sigmas based on velocity

      IF( puff(npuf)%c > 0.0 )THEN
        puff(npuf)%sxx = puff(npuf)%sxx + ux*ux - uxo*uxo
        puff(npuf)%sxy = puff(npuf)%sxy + ux*uy - uxo*uyo
        puff(npuf)%sxz = puff(npuf)%sxz + ux*uz - uxo*uzo
        puff(npuf)%syy = puff(npuf)%syy + uy*uy - uyo*uyo
        puff(npuf)%syz = puff(npuf)%syz + uy*uz - uyo*uzo
        puff(npuf)%szz = puff(npuf)%szz + uz*uz - uzo*uzo
      END IF

!------ Check if puff is realizable; if not reset sigmas

      lrealizable = PuffRealizable( puff(npuf)%sxx,puff(npuf)%sxy,puff(npuf)%sxz, &
                                    puff(npuf)%syy,puff(npuf)%syz,puff(npuf)%szz )

      IF( .NOT.lrealizable )THEN
        puff(npuf)%sxx = puff(npuf)%sxx - ux*ux + uxo*uxo
        puff(npuf)%sxy = puff(npuf)%sxy - ux*uy + uxo*uyo
        puff(npuf)%sxz = puff(npuf)%sxz - ux*uz + uxo*uzo
        puff(npuf)%syy = puff(npuf)%syy - uy*uy + uyo*uyo
        puff(npuf)%syz = puff(npuf)%syz - uy*uz + uyo*uzo
        puff(npuf)%szz = puff(npuf)%szz - uz*uz + uzo*uzo
      END IF

!------ set composite horizontal scale for limit to si2

      rat1 = 2.*uubl/(uubt+vvbt+SMALL)
      rat2 = 2.*vvbl/(uubt+vvbt+SMALL)
      rat3 = (uub+vvb)/(uubt+vvbt+SMALL)
      IF( rat1+rat2+rat3 > SMALL )THEN
        sly = rat1*sbls + rat2*sbl + rat3*sby
      ELSE ! for bl_type = none
        sly = MAX(sbls,sbl,sby)
      END IF

      IF( puff(npuf)%si2 < sly )puff(npuf)%si2 = puff(npuf)%si2 + 2.*SQRT(ux*ux+uy*uy)

      CALL siginv( puff(npuf) )

!---- Check time level for moving source

      IF( lmov .AND. lfirst )THEN

!----- Make sure length is not too long compared to lateral width
!----- since inversion of sigma matrix becomes ill-conditioned

        sigm = MAX(puff(npuf)%axx,puff(npuf)%ayy,puff(npuf)%azz)
        dtm  = SQRT(1.0/sigm)/(0.003*velm)
        ilevm = time_level( dtm )
        IF( nError /= NO_ERROR )THEN
          eRoutine = 'CreateStaticContinuousRelease'
          WRITE(lun_log,*,ERR=9998)'******* TIME LEVEL ERROR ********'
          WRITE(lun_log,*,ERR=9998)TRIM(eRoutine)
          WRITE(lun_log,*,ERR=9998)TRIM(eInform)
          WRITE(lun_log,*,ERR=9998)'DT=',dtm
          WRITE(lun_log,*,ERR=9998)'DELT=',delt
          WRITE(lun_log,*,ERR=9998)'LEVEL=',ilevm,MAXTLV
          WRITE(lun_log,*,ERR=9998)'VEL=',vel
          CALL dump_puff( npuf,puff(npuf) )
          GOTO 9999
        END IF

        puff(npuf)%idtl = MAX(puff(npuf)%idtl,ilevm)
        ilev0           = MAX(ilev0,puff(npuf)%idtl)

      END IF

!-------- Scale puff integral variables for change in timestep

      CALL ScaleStaticPuff( puff(npuf),rat )
      IF( nError /= NO_ERROR )GOTO 9999

!------ Terminate if vertically impacting ground

      IF( dynamic )THEN
        CALL get_dynamics( puff(npuf),pd )
        ldense = dense_effect( puff(npuf)%zbar-hp,sv,MIN(pd%wcp,pd%wcb),puff(npuf)%c )
        IF( wdyn < -SQRT(ub*ub+vb*vb) .AND. ldense )THEN
          lsplit = .TRUE.
        ELSE
          ldense = ((pd%dudx+pd%dvdy)*puff(npuf)%c*(sv**2) > puff(npuf)%zwc) .AND. ldense
        END IF
      END IF

!------ Check for termination due to split

      ifld = getPuffifld( puff(npuf) )
      chkx = DxSplit( puff(npuf),ifld,ldense )
      lsplit = lsplit .OR. (puff(npuf)%szz > DzSplit( puff(npuf),ifld,.FALSE. ))
      IF( chkx > 0. )lsplit = lsplit .OR. (puff(npuf)%sxx > chkx) &
                                     .OR. (puff(npuf)%syy > chkx)

!------ Terminate if dynamic velocity is near to reversal

      IF( ub*(ub+udyn)+vb*(vb+vdyn) < 0.0 )THEN
        IF( (ub+udyn)**2+(vb+vdyn)**2 < 0.5*(ub*ub+vb*vb) )lsplit = .TRUE.
      END IF

      IF( lmov )THEN

!------ Terminate on surface interaction if moving source

        IF( srf_puff(ityp,1)%nblocks > 0 .OR. srf_puff(ityp,2)%nblocks > 0 )THEN
          lsplit = lsplit .OR. (puff(npuf)%zbar-hp < 3.*SQRT(puff(npuf)%szz))
        END IF

!------ Update previous puffs for a moving source

        isprv = ps%isprv
        DO WHILE( isprv > 0 )
          puff(isprv)%xbar = puff(isprv)%xbar + DBLE(um*dt*xmap)
          puff(isprv)%ybar = puff(isprv)%ybar + DBLE(vm*dt*ymap)
          puff(isprv)%zbar = puff(isprv)%zbar + wm*dt
          CALL get_static( puff(isprv),pso )
          isprv = pso%isprv
        END DO

      END IF

      lfirst = .FALSE.

      IF( lpuf )THEN
        staticRel(is)%vp%ipuf  = npuf
        staticRel(is)%vp%vel%x = ux
        staticRel(is)%vp%vel%y = uy
        staticRel(is)%vp%vel%z = uz
        lpuf = .FALSE.
        ipuf = staticRel(is)%sp%ipuf
        sid  = staticID(staticRel(is)%sp%rid)
        uxo  = staticRel(is)%sp%vel%x
        uyo  = staticRel(is)%sp%vel%y
        uzo  = staticRel(is)%sp%vel%z
      ELSE
        staticRel(is)%sp%ipuf  = npuf
        staticRel(is)%sp%vel%x = ux
        staticRel(is)%sp%vel%y = uy
        staticRel(is)%sp%vel%z = uz
        ipuf = 0
      END IF
    END DO
  END DO RelLoop

  tr  = tr + dt
  rat = 2.**(ilev-ilev0)

  ilev = ilev0

  IF( lRmvPuff .AND. tr <= 0.25*delt_static )jpufs = npuf

END DO StaticCreate

IF( nEvapBlk > 0 )CALL MoveEvapTemp()

myOK = ManageGrdLock( 0,0,CLR_GRDLOCK )

!------ Remove existing overlapping puffs if resuming with longer time step

IF( lRmvPuff )THEN
  npufr = 0

  RstPuffLoop: DO ipuf = 1,npuf0
    IF( puff(ipuf)%idtl < 0 )CYCLE RstPuffLoop
    ityp = puff(ipuf)%ityp
    irel  = getPuffirel( puff(ipuf) )
    iSkew = getPuffiskew( puff(ipuf) )
    lwet = IsWetParticle( typeID(ityp)%icls )

    DO jpuf = jpufs,npuf
      IF( puff(jpuf)%idtl /=  I_STATIC )CYCLE
      jtyp = puff(jpuf)%ityp
      CALL mapfac( SNGL(puff(jpuf)%xbar),SNGL(puff(jpuf)%ybar),xmap,ymap )

!------ check type

      IF( ityp == jtyp .AND. irel == getPuffirel(puff(jpuf)) &
                      .AND. iSkew == getPuffiskew(puff(jpuf)) )THEN

!------ check for wet particles

        IF( lwet )THEN
          IF( .NOT.check_wetpart_merge(puff(ipuf),puff(jpuf)) )CYCLE
        END IF

!------ check scale

        s1 = puff(ipuf)%si
        s2 = puff(jpuf)%si
        IF( s1 <= simrge*s2 .AND. s2 <= simrge*s1 )THEN

!------ check for location relative to inversion height

          s1 = puff(ipuf)%zbar - puff(ipuf)%zi
          s2 = puff(jpuf)%zbar - puff(jpuf)%zi

          l1 = puff(ipuf)%zc <= simrge*puff(jpuf)%zc
          l2 = puff(jpuf)%zc <= simrge*puff(ipuf)%zc

          IF( s1*s2 >= 0.0 .AND. l1 .AND. l2 )THEN

  !------ check overlap distance

            betx = (puff(jpuf)%xbar - puff(ipuf)%xbar)/DBLE(xmap)
            bety = (puff(jpuf)%ybar - puff(ipuf)%ybar)/DBLE(ymap)
            betz =  DBLE(puff(jpuf)%zbar) - DBLE(puff(ipuf)%zbar)
            arg = overlp_merge( puff(ipuf),puff(jpuf),betx,bety,betz )

            IF( arg <= rrmrge )THEN
              iprv = puff(ipuf)%iprv
              inxt = puff(ipuf)%inxt
              IF( inxt > 0 )THEN
                puff(inxt)%iprv = iprv
              END IF
              IF( iprv < 0 )THEN
                isprv = -iprv
                k = isprv - INT(isprv/1000)*1000
                icell = (isprv - k)/1000
                ipgrd(icell,2,k) = inxt
              ELSE IF( iprv > 0 )THEN
                puff(iprv)%inxt = inxt
              END IF
              puff(ipuf)%idtl = I_REMOVE
              npufr = npufr + 1
              CYCLE RstPuffLoop
            END IF

          END IF
        END IF
      END IF

    END DO
  END DO RstPuffLoop

  IF( npufr > 0 )CALL reset_tlev() !Reset time-level lists if puffs have been removed

END IF

!----- At end of static list, create the effective release puffs

!----- Define a time level so one is available even if nrels = 0
!      N.B. Ignore any error from time_level as t_srf can be arbitrarily small

ilev = time_level( 0.125*t_srf )
IF( nError /= NO_ERROR )CALL init_error()

dt_rel = dt

def => col%firstDef
DO WHILE( ASSOCIATED(def) )
  IF( def%state == CR_ACTIVE )THEN

    set => def%rSet
    DO irel = 1,set%nrel
      IF( .NOT.set%rels(irel)%isStatic )CYCLE
      def%dtr = 2.*delt
      vapor = ASSOCIATED(set%rels(irel)%vapPuff)
      DO ipuf = 1,set%rels(irel)%npuff
        dt = dt_rel
        CALL create_static_release( def%isMoving,def%vel,set%rels(irel)%relPuff(ipuf),ilev,dt )
        IF( nError /= NO_ERROR )GOTO 9999
        IF( vapor )THEN
          dt = dt_rel
          CALL create_static_release( def%isMoving,def%vel,set%rels(irel)%vapPuff(ipuf),ilev,dt )
          IF( nError /= NO_ERROR )GOTO 9999
        END IF
      END DO
    END DO
    NULLIFY( set )
  END IF
  def => def%nextDef
END DO
NULLIFY( def )

!----- Make sure all static releases use the same time level

stat%tlev = ilev
def => col%firstDef
DO WHILE( ASSOCIATED(def) )
  IF( def%state == CR_ACTIVE )THEN

    set => def%rSet
    DO irel = 1,set%nrel
      IF( .NOT.set%rels(irel)%isStatic )CYCLE
      vapor = ASSOCIATED(set%rels(irel)%vapPuff)
      set%rels(irel)%basePuff%idtl = ilev
      DO ipuf = 1,set%rels(irel)%npuff
        set%rels(irel)%relPuff(ipuf)%idtl = ilev
        IF( vapor )THEN
          set%rels(irel)%vapPuff(ipuf)%idtl = ilev
        END IF
      END DO
    END DO
    NULLIFY( set )
  END IF
  def => def%nextDef
END DO
NULLIFY( def )

9999 CONTINUE

IF( ALLOCATED(staticRel) )DEALLOCATE( staticRel,STAT=ios )

NULLIFY( stat )
NULLIFY( def )
NULLIFY( set )
NULLIFY( rel )

NULLIFY( prel )

NULLIFY( listFirst )
NULLIFY( listLast )
NULLIFY( listNext )
NULLIFY( thisStatic )

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error writing to log file'
GOTO 9999

END
!*******************************************************************************
!  InteractStatics
!*******************************************************************************
SUBROUTINE InteractStatics( nsrel,srel )

USE scipuff_fi
USE error_fi
use inter_fi
USE cont_rel_fd
USE files_fi

IMPLICIT NONE

INTEGER,                                  INTENT( IN ) :: nsrel
TYPE( static_release ), DIMENSION(nsrel), INTENT( IN ) :: srel

TYPE ( puff_dynamics ) pdi
TYPE ( puff_totalcc  ) pti
TYPE( puff_liquid    ) pqi, pqj
TYPE ( puff_static   ) ps

INTEGER is, minPuff, maxPuff
INTEGER ipuf, jpuf, ityp, imat, jtyp, jmat, alloc_stat
REAL    sigl, facl, xl, ux, uy, uz, qqx
LOGICAL lpuf

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid, IsAerosol
LOGICAL, EXTERNAL :: IsMulti

minPuff = npuf
maxPuff = 1

DO is = 1,nsrel
  IF( srel(is)%sp%ipuf > 0 )THEN
    minPuff = MIN(minPuff,srel(is)%sp%ipuf)
    maxPuff = MAX(maxPuff,srel(is)%sp%ipuf)
  END IF
  IF( srel(is)%liquid .AND. srel(is)%vp%ipuf > 0 )THEN
    minPuff = MIN(minPuff,srel(is)%vp%ipuf)
    maxPuff = MAX(maxPuff,srel(is)%vp%ipuf)
  END IF
END DO

ALLOCATE( ptmp(minPuff:maxPuff),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ptmp2(minPuff:maxPuff),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ccb(minPuff:maxPuff),cctb(minPuff:maxPuff),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ccbg(minPuff:maxPuff),STAT=alloc_stat )
IF( dynamic )THEN
  IF( alloc_stat == 0 )ALLOCATE( ucb(minPuff:maxPuff),vcb(minPuff:maxPuff),wcb(minPuff:maxPuff),STAT=alloc_stat )
  IF( alloc_stat == 0 )ALLOCATE( ctb(minPuff:maxPuff),bcb(minPuff:maxPuff),ctp(minPuff:maxPuff),STAT=alloc_stat )
  IF( dense_gas )THEN
    IF( alloc_stat == 0 )ALLOCATE( udense(minPuff:maxPuff),vdense(minPuff:maxPuff),STAT=alloc_stat )
    IF( alloc_stat == 0 )ALLOCATE( dudx_d(minPuff:maxPuff),dudy_d(minPuff:maxPuff),STAT=alloc_stat )
    IF( alloc_stat == 0 )ALLOCATE( dvdx_d(minPuff:maxPuff),dvdy_d(minPuff:maxPuff),STAT=alloc_stat )
  END IF
END IF
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'inter_static'
  eMessage = 'Error allocating interaction arrays'
  WRITE(eInform,'(A,I10)') 'Requested size =',npuf
  GOTO 9999
END IF

ipufMin = -1    !Normal interaction

DO ipuf = minPuff,maxPuff
  CALL clear_inter_omp( ipuf )
END DO

IF( multicomp )THEN
  CALL InitInterMC( minPuff,maxPuff )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

DO ipuf = minPuff,maxPuff

  IF( puff(ipuf)%c <= 0.0 )CYCLE

  ityp  = puff(ipuf)%ityp
  imat  = typeID(ityp)%imat
  ltot  = typeID(ityp)%ltot
  lmc   = IsMulti(typeID(ityp)%icls)
  lliqm = IsLiquid(material(imat)%icls)
  lliqi = IsLiquid(typeID(ityp)%icls)
  laeri = IsAerosol(typeID(ityp)%icls)
  IF( lliqi )THEN
    CALL get_liquid( puff(ipuf),pqi )
    ldiami = pqi%d
    lgrpi = typeID(ityp)%igrp - 1
    CALL get_liquid_bin( material(imat),lgrpi,lbini,puff(ipuf) )
  END IF
  ldyni = IsGas(typeID(ityp)%icls) .AND. dynamic

!--- Load auxiliary structures

  IF( dynamic )CALL get_dynamics( puff(ipuf),pdi )

  IF( typeID(ityp)%ltot )CALL get_totalcc( puff(ipuf),pti )

  IF( lmc )CALL GetInterMC1(  puff(ipuf) )
!---  Set interaction parameters for ipuf

  CALL mapfac( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),xmap_i,ymap_i )

  CALL test_refl( puff(ipuf),lrfl_ipuf,zp,h,hx,hy )

  IF( lrfl_ipuf )THEN
    CALL inter_asig( puff(ipuf) )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  DO jpuf = ipuf,maxPuff

    IF( puff(jpuf)%c <= 0.0 )CYCLE

    lstatic = (ipuf == jpuf)

    jtyp  = puff(jpuf)%ityp
    jmat  = typeID(jtyp)%imat
    ldynj = IsGas( typeID(jtyp)%icls ) .AND. dynamic
    lliqj = IsLiquid( typeID(jtyp)%icls ) .AND. (typeID(jtyp)%igrp > 1)
    IF( lliqj )THEN
      CALL get_liquid( puff(ipuf),pqj )
      ldiamj = pqj%d
      lgrpj = typeID(jtyp)%igrp - 1
      CALL get_liquid_bin( material(imat),lgrpj,lbinj,puff(jpuf) )
    END IF

    lmat  = (imat == jmat) .AND. (ltot .OR. lmc)
    IF( lliqi )THEN
      ltyp = lmat .AND. (typeID(jtyp)%igrp > 1)
    ELSE
      ltyp = ityp == jtyp
    END IF
    lsame = ltyp .OR. lmat

    lprocess = lsame .OR. ldynj .OR. ldyni

!------ compute interactions if necessary

    IF( lprocess )THEN
      CALL inter_proc( ipuf,jpuf,pdi,pti,pqi )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

  END DO
END DO

!--- Adjustment factor for particle/liquids

DO ipuf = minPuff,maxPuff
  CALL set_inter_omp( ipuf )
END DO

DO ipuf = minPuff,maxPuff
  CALL set_cc_liq( puff(ipuf),ptmp(ipuf),ptmp2(ipuf) )
END DO

IF( multicomp )THEN
  DO ipuf = minPuff,maxPuff
    CALL ResetInterMC( ipuf,puff(ipuf) )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO
  CALL ExitInterMC()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!--- Scale factor

DO is = 1,nsrel

  ipuf = srel(is)%sp%ipuf

  ux = srel(is)%sp%vel%x
  uy = srel(is)%sp%vel%y
  uz = srel(is)%sp%vel%z

  qqx = srel(is)%sp%qqx

  IF( ipuf /= 0 )THEN
    lpuf = .FALSE.
  ELSE
    IF( srel(is)%liquid )THEN
      ipuf = srel(is)%vp%ipuf
      lpuf = .TRUE.

      ux = srel(is)%vp%vel%x
      uy = srel(is)%vp%vel%y
      uz = srel(is)%vp%vel%z

      qqx = srel(is)%vp%qqx
    END IF
  END IF
  DO WHILE( ipuf > 0 )

    sigl = SQRT( ux**2*puff(ipuf)%sxx + 2.0*ux*uy*puff(ipuf)%sxy + &
                uy**2*puff(ipuf)%syy + 2.0*ux*uz*puff(ipuf)%sxz + &
                uz**2*puff(ipuf)%szz + 2.0*uy*uz*puff(ipuf)%syz )

    xl = ux**2 + uy**2 + uz**2

    IF( qqx < xl )THEN
      facl = SQRTPI*sigl/xl
      facl = (1.-facl)*(qqx/xl) + facl
    ELSE
      facl = 1.
    END IF

!--- Set adjusted interaction values

    puff(ipuf)%ccb = facl*puff(ipuf)%ccb

    IF( typeID(puff(ipuf)%ityp)%ltot )THEN
      CALL get_totalcc( puff(ipuf),pti )
      pti%cctb = facl*pti%cctb
      CALL put_totalcc( puff(ipuf),pti )
    END IF

    IF( IsMulti(typeID(puff(ipuf)%ityp)%icls) )THEN
      CALL ScaleStaticMC( puff(ipuf),facl )
    END IF

    IF( dynamic )THEN
      CALL get_dynamics( puff(ipuf),pdi )
      IF( dense_gas )THEN
        CALL get_static( puff(ipuf),ps )
        pdi%dudx = facl*pdi%dudx
        pdi%dudy = facl*pdi%dudy
        pdi%dvdx = facl*pdi%dvdx
        pdi%dvdy = facl*pdi%dvdy
      END IF
      pdi%ucb = facl*pdi%ucb
      pdi%vcb = facl*pdi%vcb
      pdi%wcb = facl*pdi%wcb
      pdi%ctb = facl*pdi%ctb
      IF( buoy_gas )pdi%bcb = facl*pdi%bcb
      CALL put_dynamics( puff(ipuf),pdi )
    END IF

    IF( srel(is)%liquid .AND. .NOT.lpuf )THEN
      ipuf = srel(is)%vp%ipuf
      lpuf = .TRUE.

      ux = srel(is)%vp%vel%x
      uy = srel(is)%vp%vel%y
      uz = srel(is)%vp%vel%z

      qqx = srel(is)%vp%qqx
    ELSE
      ipuf = 0
    END IF

  END DO

END DO

9999 CONTINUE

IF( ALLOCATED(ptmp) )DEALLOCATE( ptmp,STAT=alloc_stat )
IF( ALLOCATED(ptmp2) )DEALLOCATE( ptmp2,STAT=alloc_stat )
IF( ALLOCATED(ccb)  )DEALLOCATE( ccb, STAT=alloc_stat )
IF( ALLOCATED(cctb) )DEALLOCATE( cctb,STAT=alloc_stat )
IF( ALLOCATED(ccbg) )DEALLOCATE( ccbg,STAT=alloc_stat )
IF( dynamic )THEN
  IF( ALLOCATED(ucb) )DEALLOCATE( ucb,STAT=alloc_stat )
  IF( ALLOCATED(vcb) )DEALLOCATE( vcb,STAT=alloc_stat )
  IF( ALLOCATED(wcb) )DEALLOCATE( wcb,STAT=alloc_stat )
  IF( ALLOCATED(ctb) )DEALLOCATE( ctb,STAT=alloc_stat )
  IF( ALLOCATED(bcb) )DEALLOCATE( bcb,STAT=alloc_stat )
  IF( ALLOCATED(ctp) )DEALLOCATE( ctp,STAT=alloc_stat )
  IF( dense_gas )THEN
    IF( ALLOCATED(udense) )DEALLOCATE( udense,STAT=alloc_stat )
    IF( ALLOCATED(vdense) )DEALLOCATE( vdense,STAT=alloc_stat )
    IF( ALLOCATED(dudx_d) )DEALLOCATE( dudx_d,STAT=alloc_stat )
    IF( ALLOCATED(dudy_d) )DEALLOCATE( dudy_d,STAT=alloc_stat )
    IF( ALLOCATED(dvdx_d) )DEALLOCATE( dvdx_d,STAT=alloc_stat )
    IF( ALLOCATED(dvdy_d) )DEALLOCATE( dvdy_d,STAT=alloc_stat )
  END IF
END IF

RETURN
END
!*******************************************************************************
!  ScaleStaticPuff
!*******************************************************************************
SUBROUTINE ScaleStaticPuff( p,rat )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: rat

INTEGER icls

TYPE ( puff_dynamics ) pd
TYPE ( puff_totalcc  ) pt
TYPE ( puff_liquid   ) pq
TYPE ( puff_aerosol  ) pa

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsMulti

!IF( rat == 1.0 )GOTO 9999

!----- Set auxiliary variables

icls = typeID(p%ityp)%icls

IF( dynamic )CALL get_dynamics( p,pd )
IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL get_liquid( p,pq )
IF( IsAerosol(icls) )CALL get_aerosol( p,pa )

!-------- Scale puff integral variables for change in timestep

CALL scale_psum( p,rat )

!-------- Scale dynamic variables for change in mass
!         N.B. dense variables are not mass integrals but
!              must be reset due to change in puff shape

IF( dynamic )THEN
  CALL scale_dynamics( pd,rat )
  IF( dense_gas .AND. IsGas(icls) )CALL set_dense_gas( p,pd )
  CALL put_dynamics( p,pd )
END IF

!-------- Scale multi-group total variance

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc( p,pt )
  pt%cctb = rat*pt%cctb
  pt%cct  = rat*pt%cct
  CALL put_totalcc( p,pt )
END IF

IF( IsAerosol(icls) )CALL put_aerosol( p,pa )

!-------- Scale liquid auxiliary variables

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )THEN
  pq%ccs = rat*pq%ccs
  CALL put_liquid( p,pq )
END IF

!-------- Scale multi-components

IF( IsMulti(icls) )THEN
  CALL InitMCcont( p,rat,1.0 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF


9999 CONTINUE

RETURN
END
!*******************************************************************************
!  MergeVaporPuff
!*******************************************************************************
SUBROUTINE MergeVaporPuff( p,pvap,v,vvap )

USE scipuff_fi
USE cont_rel_fd

IMPLICIT NONE

TYPE( puff_str ),              INTENT( IN    ) :: p
TYPE( puff_str ),              INTENT( INOUT ) :: pvap
TYPE( cont_release_triad_R4 ), INTENT( IN    ) :: v
TYPE( cont_release_triad_R4 ), INTENT( INOUT ) :: vvap

REAL(8) xbar, ybar
REAL    tot, r1, r2, zbar, xmap, ymap
REAL    ddx1, ddx2, ddy1, ddy2, ddz1, ddz2
REAL    pzc, vzc

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

tot = p%c + pvap%c
IF( tot > SMALL )THEN
  r1 = p%c/tot
  r2 = pvap%c/tot
ELSE
  r1 = 0.5
  r2 = 0.5
END IF

vvap%x = r1*v%x + r2*vvap%x
vvap%y = r1*v%y + r2*vvap%y
vvap%z = r1*v%z + r2*vvap%z

xbar = DBLE(r1)*p%xbar + DBLE(r2*pvap%xbar)
ybar = DBLE(r1)*p%ybar + DBLE(r2*pvap%ybar)
zbar = r1*p%zbar + r2*pvap%zbar

ddx1 = SNGL(p%xbar    - xbar)/xmap
ddx2 = SNGL(pvap%xbar - xbar)/xmap
ddy1 = SNGL(p%ybar    - ybar)/ymap
ddy2 = SNGL(pvap%ybar - ybar)/ymap
ddz1 =  p%zbar    - zbar
ddz2 =  pvap%zbar - zbar

pvap%sxx = r1*(p%sxx + ddx1*ddx1) + r2*(pvap%sxx + ddx2*ddx2)
pvap%syy = r1*(p%syy + ddy1*ddy1) + r2*(pvap%syy + ddy2*ddy2)
pvap%szz = r1*(p%szz + ddz1*ddz1) + r2*(pvap%szz + ddz2*ddz2)
pvap%sxy = r1*(p%sxy + ddx1*ddy1) + r2*(pvap%sxy + ddx2*ddy2)
pvap%syz = r1*(p%syz + ddy1*ddz1) + r2*(pvap%syz + ddy2*ddz2)
pvap%sxz = r1*(p%sxz + ddx1*ddz1) + r2*(pvap%sxz + ddx2*ddz2)

pzc = p%zc
vzc = pvap%zc

CALL pmerge_psum( p,pvap,r1,r2 )

IF( pzc == 0. .OR. vzc == 0. )pvap%zc = MAX(vzc,pzc)

IF( pvap%zc > 0. )pvap%zc = MAX(pvap%zc,zbar+0.01)

pvap%xbar = xbar
pvap%ybar = ybar
pvap%zbar = zbar

!----- Puff auxiliary space

CALL sum_paux( p,pvap )

CALL siginv( pvap )

RETURN
END
!*******************************************************************************
!  create_static_release
!*******************************************************************************
SUBROUTINE create_static_release( moving,vel,prel,ilev,dt )

USE scipuff_fi
USE cont_rel_fd
USE met_fi
USE error_fi

IMPLICIT NONE

LOGICAL,                       INTENT( IN    ) :: moving
TYPE( cont_release_triad_R4 ), INTENT( IN    ) :: vel
TYPE( puff_str ),              INTENT( INOUT ) :: prel
INTEGER,                       INTENT( INOUT ) :: ilev
REAL,                          INTENT( INOUT ) :: dt

INTEGER jpuf, ityp, icls, inxt, iprv, ifld, i
REAL    dtr, dtx, ux, vx, wx, rat
LOGICAL ltot

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc  ) pt
TYPE( puff_liquid   ) pq
TYPE( puff_aerosol  ) pa

INTEGER, EXTERNAL :: getPuffifld
LOGICAL, EXTERNAL :: PuffRealizable
INTEGER, EXTERNAl :: allocatePuffAux
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsMulti

!--- Check to see if this is a real release

jpuf = prel%iprv     !Final static puff

IF( jpuf <= 0 )THEN
  prel%idtl = -1
  prel%c    = 0.0
  GOTO 9999
END IF

IF( puff(jpuf)%idtl == I_STATIC )GOTO 9999   !Only happens if a Liquid puff completely evaporates

!--- Save integer pointers

iprv = prel%iprv
inxt = prel%inxt

!--- Need to copy aux separately from the rest of the puff becuase
!    The jpuf has static aux (even though it is not yet marked as static)
!    prel should already have the correct number of aux allocated

CALL copy_puff_noaux( puff(jpuf),prel )

DO i = 1,prel%naux
  prel%aux(i) = puff(jpuf)%aux(i)
END DO

!--- Restore integer pointers

prel%iprv = iprv
prel%inxt = inxt

rat = 1.0/dt

ityp = prel%ityp
icls = typeID(ityp)%icls
ltot = typeID(ityp)%ltot

IF( dynamic )CALL get_dynamics( prel,pd )

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL get_liquid( prel,pq )

IF( IsAerosol(icls) )CALL get_aerosol( prel,pa )

CALL scale_psum( prel,rat )

IF( dynamic )THEN
  CALL scale_dynamics( pd,rat )
  CALL put_dynamics( prel,pd )
END IF

!---- Just scale total variance

IF( ltot )THEN
  CALL get_totalcc ( prel,pt )
  pt%cctb = rat*pt%cctb
  pt%cct  = rat*pt%cct
  CALL put_totalcc( prel,pt )
END IF

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )THEN
  pq%ccs = rat*pq%ccs
  CALL put_liquid( prel,pq )
END IF

IF( IsAerosol(icls) )CALL put_aerosol( prel,pa )

IF( IsMulti(icls) )THEN
  CALL InitMCcont( prel,rat,1.0 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

dtr = delt/(2.**prel%idtl)

dtx = 0.25*(dtr*dtr-dt*dt)

ilev = MAX(ilev,prel%idtl)

ifld = getPuffifld( prel )
CALL get_met( SNGL(prel%xbar),SNGL(prel%ybar),prel%zbar,prel%szz,0.0,0,inField=ifld )

!------ Pass release timestep back

dt = dtr

IF( moving )THEN
  ux = ub - vel%x
  vx = vb - vel%y
  wx = wb - vel%z
ELSE
  ux = ub
  vx = vb
  wx = wb
END IF

prel%sxx = prel%sxx + ux*ux*dtx
prel%sxy = prel%sxy + ux*vx*dtx
prel%sxz = prel%sxz + ux*wx*dtx
prel%syy = prel%syy + vx*vx*dtx
prel%syz = prel%syz + vx*wx*dtx
prel%szz = prel%szz + wx*wx*dtx

IF( .NOT.PuffRealizable(prel%sxx,prel%sxy,prel%sxz, &
                        prel%syy,prel%syz,prel%szz) )THEN
  prel%sxx = prel%sxx - ux*ux*dtx
  prel%sxy = prel%sxy - ux*vx*dtx
  prel%sxz = prel%sxz - ux*wx*dtx
  prel%syy = prel%syy - vx*vx*dtx
  prel%syz = prel%syz - vx*wx*dtx
  prel%szz = prel%szz - wx*wx*dtx
END IF

prel%uo = vel%x
prel%vo = vel%y
prel%wo = vel%z

CALL setPuffipgrd( prel,1 )  !Still needed?

!------ Set the final static puff to be static

puff(jpuf)%idtl = I_STATIC

9999 CONTINUE

RETURN
END
