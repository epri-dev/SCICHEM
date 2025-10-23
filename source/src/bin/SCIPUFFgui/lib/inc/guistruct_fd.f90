MODULE GUIstruct_fd

  USE winAPI_fd, ONLY: POINTER_LEN

!     Structure for passing Dialog Box command Information

  TYPE CMD
    SEQUENCE
     INTEGER(POINTER_LEN)  hwnd   !Window Handle of DialogBox sending the command
     INTEGER               id     !DialogBox ID number
     INTEGER               level  !Dialog data level
     INTEGER(POINTER_LEN)  cntrl  !Handle of command window
     INTEGER               type   !Type of command
     INTEGER               button !Command ID
  END TYPE CMD

!     Structure for dealing with numeric lists (Single-selection List boxes)

  TYPE LIST_STR
    SEQUENCE
    INTEGER dialog !Dialog ID Number
    INTEGER list   !List Box ID Number
    INTEGER indx   !List Box Selection index
    REAL    value  !List Box Selection value
  END TYPE LIST_STR

END MODULE GUIstruct_fd
