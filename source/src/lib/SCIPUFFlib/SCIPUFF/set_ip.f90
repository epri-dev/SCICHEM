SUBROUTINE set_ip( n1,n2 )

USE scipuff_fi

!------ setup ipgrd and linked lists

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n1,n2

INTEGER ipuf, igrd, ip, k, jpuf
REAL    xx, yy, xmap, ymap

INTEGER, EXTERNAL :: PuffGridLevel

!------ loop over puffs

DO ipuf = n1,n2

!------ find appropriate ipgrd location

  CALL mapfac( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),xmap,ymap )

  igrd = PuffGridLevel( puff(ipuf)%sxx,puff(ipuf)%syy,xmap,ymap )
  IF( nError /= NO_ERROR )THEN
    eRoutine ='set_ip'
    CALL dump_puff( ipuf,puff(ipuf) )
    GOTO 9999
  END IF

  CALL puff_grid( puff(ipuf),xx,yy,k )

!------- check for other puffs in grid

  CALL get_grid_cell( xx,yy,k,igrd,ip )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL get_ipgrd( ip,2,k,jpuf )

!------- no other puff in box

  IF( jpuf == 0 )THEN

    puff(ipuf)%inxt = 0
    puff(ipuf)%iprv = -(1000*ip + k)
    CALL setPuffipgrd( puff(ipuf),igrd )

!------- puff(s) in box already;  add to top of list

  ELSE

    puff(ipuf)%inxt = jpuf
    puff(ipuf)%iprv = -(1000*ip + k)
    CALL setPuffipgrd( puff(ipuf),igrd )
    puff(jpuf)%iprv = ipuf

  END IF

  CALL set_ipgrd( ip,2,k,ipuf )

  CALL step_clock()

END DO

9999 CONTINUE

RETURN
END

