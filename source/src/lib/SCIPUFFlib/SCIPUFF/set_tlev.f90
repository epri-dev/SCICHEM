SUBROUTINE set_tlev( lStart )

USE scipuff_fi
USE cont_rel_functions

!------ setup multi-timestep linked lists

IMPLICIT NONE

LOGICAL, INTENT(IN) :: lstart

INTEGER ilev

!----- Set linked list

CALL reset_tlev()

!----- Find maximum time level

mxtlev = 0

IF( npuf > 0 )THEN

  DO ilev = 1,MAXTLV
    IF( ntlev(ilev) > 0 )mxtlev = ilev
  END DO

END IF

!----- Set levels for continuous releases

CALL c_set_tlev( lstart )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE reset_tlev()

USE scipuff_fi

!------ setup multi-timestep linked lists

IMPLICIT NONE

INTEGER ilev

!----- Initialize lists to zero

DO ilev = 0,MAXTLV
  itfrst(ilev) = 0
  itlast(ilev) = 0
  ntlev(ilev)  = 0
END DO

!----- Add all puffs to lists

CALL add_tlev( 1,npuf )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE add_tlev( n1,n2 )

USE scipuff_fi

!------ add new puffs (from n1 to n2) to multi-timestep linked lists

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n1, n2

INTEGER ipuf, ilev

!----- Add puffs from n1 to n2

DO ipuf = n1,n2

  ilev = puff(ipuf)%idtl

!----- Only add positive levels - negatives are REMOVE or STATIC

  IF( ilev >= 0 )THEN
    puff(ipuf)%idtn = 0              !This puff will be at the end of the list so no next puff
    IF( itfrst(ilev) == 0 )THEN
      itfrst(ilev) = ipuf            !This is the first in the list
    ELSE
      puff(itlast(ilev))%idtn = ipuf !Previous last points to this puff as next in list
    END IF
    itlast(ilev) = ipuf              !Set last pointer
    ntlev(ilev)  = ntlev(ilev) + 1   !Increment counter
  END IF

END DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE remove_tlev( lev,iprv,ipufn )

USE scipuff_fi

!------ remove a puff from multi-timestep linked list on level lev
!       iprv and ipufn are "previous" and "next" pointers

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lev, iprv, ipufn

!----- If no previous then "next" puff becomes the first, else "previous" points to "next"

IF( iprv == 0 )THEN
  itfrst(lev) = ipufn
ELSE
  puff(iprv)%idtn = ipufn
END IF

!----- If no "next", the "previous" becomes the last puff

IF( ipufn == 0 )itlast(lev) = iprv

!----- Decrement counter

ntlev(lev) = ntlev(lev) - 1

RETURN
END
