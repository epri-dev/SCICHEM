MODULE saglst_fd

  USE sagstr_fd
  USE sagtri_fd

  SAVE

  TYPE :: SAGlist_str
    SEQUENCE
    INTEGER                          :: id
    TYPE (SAGgrid_str), POINTER      :: grd
    TYPE (SAGtriangleT_str ),POINTER :: triT
    TYPE (SAGlist_str), POINTER      :: prev
    TYPE (SAGlist_str), POINTER      :: next
  END TYPE SAGlist_str

  TYPE :: SAGlist_def
    TYPE (SAGlist_str), POINTER :: First
    TYPE (SAGlist_str), POINTER :: Last
  END TYPE SAGlist_def

  TYPE (SAGlist_def) :: GrdList

END MODULE saglst_fd

MODULE PtrGrdStrItf

  INTERFACE

    FUNCTION SAG_PtrGrdStr( id ) RESULT( grd )
      USE sagstr_fd
      INTEGER,            INTENT( IN ) :: id
      TYPE (SAGgrid_str), POINTER      :: grd  !function result is a pointer
    END FUNCTION SAG_PtrGrdStr

    FUNCTION SAG_PtrTriStr( id ) RESULT( triT )
      USE sagtri_fd
      INTEGER,                 INTENT( IN ) :: id
      TYPE (SAGtriangleT_str ),POINTER      :: triT  !function result is a pointer
    END FUNCTION SAG_PtrTriStr

    FUNCTION SAG_PtrAuxStr( id ) RESULT( grd_aux )
      USE sagstr_fd
      INTEGER                          ,INTENT( IN ) :: id
      TYPE (SAGfield_aux ),DIMENSION(:),POINTER      :: grd_aux  !function result is a pointer
    END FUNCTION SAG_PtrAuxStr

  END INTERFACE

END MODULE PtrGrdStrItf
