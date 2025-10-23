module structv13_fd
  type  material_V13
    sequence
    integer   icls     !Class index
    integer   iaux     !Matl Auxiliary pointer
    integer   nmc      !Matl No. Multi-Comp
    integer   ipmc     !Matl Multi-Comp pointer
    integer   ioffp    !Puff ityp Offset
    integer   ioffs    !Srf Deposition Offset
    integer   ioffd    !Srf Dose Offset
    integer   ioffs_mc !Multi-Comp Srf Deposition Offset
    integer   ioffd_mc !Multi-Comp Srf Dose Offset

    logical   lsrfg    !Srf Deposition by group
    logical   lsrft    !Srf Deposition total
    logical   ldosg    !Srf Dose by group
    logical   ldost    !Srf Dose total
    logical   lsrf_mc  !Mutli-Comp Srf Deposition total
    logical   ldos_mc  !Multi-Comp Srf Dose total

    real      prop(10) !decay parameters

    character*16 cmat  !Material Name
    character*16 ccls  !Class Name
    character*16 unit  !Mass units
    character*16 file  !Material file name
    character*64 path  !Material file path

  end type  material_V13

end module structv13_fd
