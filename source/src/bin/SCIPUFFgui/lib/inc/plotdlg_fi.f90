MODULE plotdlg_fi
  USE plotdlg_fd

  SAVE

  TYPE( MAP_DLG ) mapdef(-1:1)

  TYPE( POPT2_DLG ) popt2def(-1:1)

  TYPE( POPT_DLG ) poptdef(-1:1)

  TYPE( TITLE_DLG ) ttldef(-1:1)

  TYPE( AXES_DLG ) axesdef(-1:1)

  INTEGER Nmap_scales(4)
  INTEGER Smap_scales(10,4)
  REAL    Fmap_scales(4)
END MODULE plotdlg_fi
