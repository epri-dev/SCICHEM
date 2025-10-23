MODULE script_fi

  USE script_fd
  USE DefSize_fd

  SAVE

  INTEGER     pcscipuf_mode
  INTEGER     script_level
  INTEGER     script_matl
  INTEGER     script_group
  INTEGER     script_value
  REAL        script_time
  INTEGER     script_fill
  INTEGER     script_draw
  CHARACTER(PATH_MAXLENGTH) script_file
  CHARACTER(PATH_MAXLENGTH) script_input
  CHARACTER(PATH_MAXLENGTH) script_table_input

END MODULE script_fi
