module time_tools

implicit none

contains

!
!*************************************************************************
!
  LOGICAL FUNCTION leap_yr(Yr) ! Y2K correct.

    IMPLICIT NONE

    integer yr

! If integer divide and float divide give the same result, then it's
! evenly divisible.  Could have used mod() here, but I think this is
! more portable.  Might give problems on some old Pentium systems
! with that pesky 4./2. = 1.9999998 error.

! The year is 365.2422 days long.
! Rules for leap years: if year is evenly divisible by 4, then it's
! a leap year, except those evenly divisible by 100, but there is a
! leap year in those evenly divisible by 400.  This will give a mean
! year 365.2425 days long.  Error is .0003 days (25.92 seconds) so
! it will take 3333 years for the calendar to get off by one day.

    if ((float(Yr/4) .eq. Yr/4.) .and. ((float(Yr/100) .ne. Yr/100.) &
         .or. (float(Yr/400) .eq. Yr/400.))) then
       leap_yr = .true.
    else
       leap_yr = .false.
    end if

    RETURN

  END FUNCTION leap_yr
!
!------------------------------------------------------------------------------
!
  subroutine legal_timestamp(iy,im,id,ih,iMaxHr)
!
!-----Makes sure the time is a legal time, either 0-23 (if iMaxHr = 23)
!     or 1-24 (if iMaxHr = 24).
!
    IMPLICIT NONE

    integer :: iy,im,id,ih,iMaxHr

    do while (ih < iMaxHr-23)
       ih = ih + 24
       id = id - 1
       if (id < 1) then
          im = im - 1
          if (im == 0) then
             im = 12
             iy = iy - 1
             if (iy < 0) iy = iy + 100    ! May be 2-digit year
          end if
          id = id + iDaysInMth(im,iy)
       end if
    end do

    do while (ih > iMaxHr)
       ih = ih - 24
       id = id + 1
       if (id > iDaysInMth(im,iy)) then
          im = im + 1
          if (im > 12) then
             im = 1
             iy = iy + 1
             if (iy == 100) iy = 0        ! May be 2-digit year
          end if
          id = 1
       end if
    end do
    return

  end subroutine legal_timestamp
!
!------------------------------------------------------------------------------
!
  integer function iymdh2idate(iy,im,id,ih)
!
!-----Returns YYMMDDHH stamp from year, month, day, hour input
!
    integer iy,im,id,ih

    call legal_timestamp(iy,im,id,ih,23)
    iymdh2idate = iy*1000000 + im*10000 + id*100 + ih
    return

  end function iymdh2idate
!
!------------------------------------------------------------------------------
!
  subroutine idate2ymdh(idate,iy,im,id,ih)
!
!-----Returns year, month, day, hour from YYMMDDHH or YYYYMMDDHH input
!
    integer iy,im,id,ih, idate

    iy = idate/1000000
    im = (idate - iy*1000000)/10000
    id = (idate - iy*1000000 - im*10000)/100
    ih = (idate - iy*1000000 - im*10000 - id*100)
    return

  end subroutine idate2ymdh
!
!------------------------------------------------------------------------------
!
subroutine jul2dat(ijul,iy,im,id)
!
!-----Convert Julian day to calendar date
!
  integer mon(12,2),ijul,iy,im,id, il,i,ii
  data mon /1,32,60,91,121,152,182,213,244,274,305,335, &
            1,32,61,92,122,153,183,214,245,275,306,336/

  il = 1
  if (Leap_yr(iy)) il = 2
  do ii = 1,12
     i = 12 - ii + 1
     if (ijul.ge.mon(i,il)) then
        im = i
        id = ijul - mon(i,il) + 1
        return
     end if
  end do
  write(*,*) 'jul2dat (in timesubs.f90): Bad julian day'
  write(*,'(a,i4,a,i3)') 'Year = ',iy,' Julian = ',ijul
  stop

end subroutine jul2dat
!
!------------------------------------------------------------------------------
!
subroutine dat2jul(iy,im,id,ijul)
!
!-----Convert calendar date to Julian date
!
  integer iy,im,id,ijul,jday(12)
  data jday /0,31,59,90,120,151,181,212,243,273,304,334/

  ijul = jday(im) + id
  if (im <= 2) return
  if (Leap_yr(iy)) ijul = ijul + 1

  return
end subroutine dat2jul
!
!------------------------------------------------------------------------------
!
integer function julian(iy,im,id)
!
!-----Convert calendar date to Julian date
!
  integer iy,im,id
  integer mon(12)
  data mon/0,31,59,90,120,151,181,212,243,273,304,334/

  if (Leap_yr(iy) .and. im > 2) then
     julian = mon(im) + id + 1
  else
     julian = mon(im) + id
  end if

  return
end function julian
!
!------------------------------------------------------------------------------
!
  integer function iDaysInMth(im,iy)
!
!-----Sets the number of days in the month
!
    integer           :: mon(12),iy,im
    data mon/31,28,31,30,31,30,31,31,30,31,30,31/

    if (Leap_yr(iy) .and. im == 2) then
       iDaysInMth = 29
    else
       iDaysInMth = mon(im)
    end if
    return

  end function iDaysInMth
!
!------------------------------------------------------------------------------
!
  subroutine TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)
!
!------Returns the number of hours between two time-stamps
!
    integer, intent(in)  :: iy1,im1,id1,ih1, iy2,im2,id2,ih2
    integer, intent(out) :: hrs
    integer              :: i,minyr, hrs1,hrs2 ! local variales
!
!-----Calculate the number of hours since the beginning of the earliest year:
!
    minyr = min(iy1,iy2) - 1

    hrs1 = 24*(id1-1) + ih1 ! days of this month, plus hours for today
    do i = minyr, iy1-1     ! possibly zero times through the loop
       hrs1 = hrs1 + 8760   ! add the hours for all the years until this year
       if (Leap_yr(i)) hrs1 = hrs1 + 24
    end do
    if (im1 > 1) then       ! add the hours for all the months until last month
       do i = 1, im1-1
          hrs1 = hrs1 + iDaysInMth(i,iy1) * 24
       end do
    end if

    hrs2 = 24*(id2-1) + ih2 ! days of this month, plus hours for today
    do i = minyr, iy2-1     ! possibly zero times through the loop
       hrs2 = hrs2 + 8760   ! add the hours for all the years until this year
       if (Leap_yr(i)) hrs2 = hrs2 + 24
    end do
    if (im2 > 1) then       ! add the hours for all the months until last month
       do i = 1, im2-1
          hrs2 = hrs2 + iDaysInMth(i,iy2) * 24
       end do
    end if

    hrs = hrs2 - hrs1       ! answer is the difference

  end subroutine TimeDiff
!
!------------------------------------------------------------------------------
!
end module time_tools


