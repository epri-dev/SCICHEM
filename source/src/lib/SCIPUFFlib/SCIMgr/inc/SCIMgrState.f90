MODULE SCIMgrState

  LOGICAL,EXTERNAL :: SCIMgrIsBusy      !State /= HS_IDLE
  LOGICAL,EXTERNAL :: SCIMgrIsIdle      !State == HS_IDLE
  LOGICAL,EXTERNAL :: SCIMgrIsRun       !State == HS_RUN  bit is set

  LOGICAL,EXTERNAL :: SCIMgrIsSync      !State == HS_SYNC bit is set

  LOGICAL,EXTERNAL :: SCIMgrIsWait      !State == HS_WAIT bit is set
  LOGICAL,EXTERNAL :: SCIMgrHasProgress !State == HS_PBOX bit is set
  LOGICAL,EXTERNAL :: SCIMgrCheckState  !Checks State against argument
  INTEGER,EXTERNAL :: SCIMgrSetState    !Sets the current SCIP tool state
  INTEGER,EXTERNAL :: SCIMgrGetState    !Gets the current SCIP tool state

END MODULE SCIMgrState
