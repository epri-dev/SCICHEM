MODULE project_fd

  USE prjstruct_fd

  TYPE  AuditStructure
    SEQUENCE
    CHARACTER(32)  Classification
    CHARACTER(32)  Analyst
    CHARACTER(32)  Version
    CHARACTER(32)  CreateDate
  END TYPE  AuditStructure

  TYPE  ProjectStructure
    SEQUENCE
    LOGICAL OK
    LOGICAL Run
    LOGICAL Plot
    LOGICAL Edit
    LOGICAL Puff
    LOGICAL Grid
    LOGICAL Dynamic
    LOGICAL DenseGas
    LOGICAL Terrain
    LOGICAL StaticPuffs
    LOGICAL SourceNests
    INTEGER Mode
    INTEGER MapCoord
    LOGICAL Ref
    INTEGER Weather
    LOGICAL Restart
    INTEGER RestartTimeIndx
    CHARACTER(128) Title
    CHARACTER(PATH_MAXLENGTH) RestartFile
    TYPE( AuditStructure ) audit
    TYPE( projectIDT ) ID
  END TYPE  ProjectStructure

END MODULE project_fd
