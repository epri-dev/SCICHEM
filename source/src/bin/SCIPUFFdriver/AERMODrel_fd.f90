MODULE AERMODrel_fd

  TYPE AERMODrel
    CHARACTER(32)   relName     !Release Identifier
    CHARACTER(192)  relDisplay  !Release display string
    CHARACTER(16)   material    !relmat
    INTEGER         type        !reltyp
    REAL            tRel        !trel/3600.
    REAL            xRel
    REAL            yRel
    REAL            zRel
    REAL            distribution
    REAL            diameter
    REAL            MMD
    REAL            sigma
    LOGICAL         lEmiFile

  END TYPE AERMODrel

END MODULE AERMODrel_fd
