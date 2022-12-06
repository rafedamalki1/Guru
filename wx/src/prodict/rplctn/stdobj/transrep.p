DEFINE INPUT PARAMETER hParent AS HANDLE NO-UNDO.

DEF SHARED TEMP-TABLE ttRepProcs 
    FIELD table-name AS CHAR
    FIELD proc-hdl AS HANDLE                   
    FIELD iLastLocalRep AS INTEGER
    FIELD iLastRemoteRep AS INTEGER
    INDEX table-name AS PRIMARY UNIQUE table-name.

DEF TEMP-TABLE ttTrans
    FIELD trans-id AS INTEGER.

{prodict/rplctn/stdobj/repdef.i}


  FOR EACH {&Local-LDB}.replicate-chng NO-LOCK 
     BREAK BY {&Local-LDB}.replicate-chng.trans-id:
     
     IF FIRST-OF({&Local-LDB}.replicate-chng.trans-id) THEN
       DO:                                     
        FIND FIRST ttRepProcs 
          WHERE ttRepProcs.table-name = {&Local-LDB}.replicate-chng.table-name.
        IF {&Local-LDB}.replicate-chng.gmt-julian >= ttRepProcs.iLastLocalRep THEN
          DO:
           CREATE ttTrans.
           ASSIGN ttTrans.trans-id = {&Local-LDB}.replicate-chng.trans-id.
          END.
       END.
  END.


  FOR EACH ttTrans TRANSACTION:
     RUN show-message IN hParent ("Replicating Trans - " + string(ttTrans.trans-id)).
     FOR EACH {&Local-LDB}.replicate-chng 
        WHERE {&Local-LDB}.replicate-chng.trans-id = ttTrans.trans-id NO-LOCK:     
        
        FIND FIRST ttRepProcs 
           WHERE ttRepProcs.table-name = {&Local-LDB}.replicate-chng.table-name.

        IF NOT AVAILABLE ttRepProcs THEN
          RETURN ERROR.

        RUN apply-local-records IN ttRepProcs.proc-hdl
           (INPUT {&Local-LDB}.replicate-chng.data-record,
            INPUT {&Local-LDB}.replicate-chng.event).
        
        IF RETURN-VALUE = "ERROR" THEN
           UNDO,LEAVE.
     END.
  END.
  
  FOR EACH {&Remote-LDB}.replicate-chng NO-LOCK
     BREAK BY {&Remote-LDB}.replicate-chng.trans-id:
     
     IF FIRST-OF({&Remote-LDB}.replicate-chng.trans-id) THEN
       DO:
        FIND FIRST ttRepProcs 
          WHERE ttRepProcs.table-name = {&Remote-LDB}.replicate-chng.table-name.
        IF {&Remote-LDB}.replicate-chng.gmt-julian >= ttRepProcs.iLastremoteRep THEN
          DO:
           CREATE ttTrans.
           ASSIGN ttTrans.trans-id = {&Remote-LDB}.replicate-chng.trans-id.
          END.
       END.
  END.
  
  FOR EACH ttTrans TRANSACTION:
     FOR EACH {&Remote-LDB}.replicate-chng 
        WHERE {&Remote-LDB}.replicate-chng.trans-id = ttTrans.trans-id NO-LOCK:     
        
        FIND FIRST ttRepProcs 
           WHERE ttRepProcs.table-name = {&Remote-LDB}.replicate-chng.table-name.

        IF NOT AVAILABLE ttRepProcs THEN
          RETURN ERROR.
          
        RUN apply-remote-records IN ttRepProcs.proc-hdl
           (INPUT {&Remote-LDB}.replicate-chng.data-record,
            INPUT {&Remote-LDB}.replicate-chng.event).
        
        IF RETURN-VALUE = "ERROR" THEN
           UNDO,LEAVE.
     END.
  END.
     

