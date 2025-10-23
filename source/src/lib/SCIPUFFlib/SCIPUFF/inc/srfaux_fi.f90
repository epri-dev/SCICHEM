MODULE srfaux_fi

  USE struct_fd

  SAVE

  INTEGER nbaux_srf  !No. of blocks needing auxiliary processing

  INTEGER, DIMENSION(:), POINTER :: ibaux_srf  !Pointer to blocks
  REAL,    DIMENSION(:), POINTER :: cmaux_srf  !Puff mean contribution
  REAL,    DIMENSION(:), POINTER :: cvaux_srf  !Puff variance contribution

  COMMON / srfaux_fi_cmn / nbaux_srf
  COMMON / srfaux_fi_cmn / ibaux_srf
  COMMON / srfaux_fi_cmn / cmaux_srf
  COMMON / srfaux_fi_cmn / cvaux_srf
  !$OMP THREADPRIVATE (/srfaux_fi_cmn/)

  TYPE( puff_liquid  )  pq
  COMMON / srfaux_fi_pq / pq
!$OMP THREADPRIVATE (/srfaux_fi_pq/)

END MODULE srfaux_fi
