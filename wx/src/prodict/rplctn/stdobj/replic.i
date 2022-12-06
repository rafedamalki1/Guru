&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11
/* Procedure Description
"Replication master include file"
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


{prodict/rplctn/stdobj/repdef.i}

/* Repl-Table is the table the procedure will replicate */
DEF TEMP-TABLE ttRemote LIKE {&Local-LDB}.{&Repl-Table}.
DEF TEMP-TABLE ttLocal LIKE {&Local-LDB}.{&Repl-Table}.
DEF TEMP-TABLE ttCharFld
    FIELD fld AS CHAR.
DEF TEMP-TABLE ttLocLkup
    FIELD trans-id AS INTEGER
    FIELD chng-row-id AS CHAR
    FIELD loc-row-id AS CHAR.
DEF TEMP-TABLE ttRemLkup
    FIELD trans-id AS INTEGER
    FIELD chng-row-id AS CHAR
    FIELD rem-row-id AS CHAR.
DEF TEMP-TABLE ttTemp LIKE {&Local-LDB}.{&Repl-Table}.
DEF VAR iRemoteNew AS INTEGER INIT 0 NO-UNDO.
DEF VAR iRemoteUpdate AS INTEGER INIT 0 NO-UNDO.
DEF VAR iRemoteDelete AS INTEGER INIT 0 NO-UNDO.
DEF VAR iCollisionCount AS INTEGER INIT 0 NO-UNDO.
DEF VAR iLastLocalRep AS INTEGER NO-UNDO.
DEF VAR iLastRemoteRep AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 4.08
         WIDTH              = 40.
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

IF THIS-PROCEDURE:PERSISTENT THEN
  ASSIGN THIS-PROCEDURE:PRIVATE-DATA = '{&Repl-Table}'.

RUN get-local-records.
RUN get-remote-records.
RUN check-collision.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-local-records Include 
PROCEDURE apply-local-records :
/*------------------------------------------------------------------------------
  Purpose: Apply records to from the local database to the remote database    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER rRaw AS RAW NO-UNDO.
DEFINE INPUT PARAMETER sEvent AS CHAR NO-UNDO.

FOR EACH ttTemp:
 DELETE ttTemp.
END.

RAW-TRANSFER rRaw TO ttTemp.
RELEASE ttTemp.

FIND FIRST ttTemp.

/* need to get out because record was not found */
IF NOT AVAILABLE ttTemp THEN
  RETURN "ERROR".
  
DISABLE TRIGGERS FOR LOAD OF {&Remote-LDB}.{&repl-table}.

FIND FIRST {&Remote-LDB}.{&Repl-Table} OF ttTemp EXCLUSIVE-LOCK NO-ERROR.

  CASE sEvent:
  WHEN "WRITE" THEN 
  DO:  
    IF NOT AVAILABLE {&Remote-LDB}.{&Repl-Table} THEN
      DO:
        RAW-TRANSFER ttTemp TO {&Remote-LDB}.{&Repl-Table}.
        RELEASE {&Remote-LDB}.{&Repl-Table}.
        ASSIGN iRemoteNew = iRemoteNew + 1.
      END.
    ELSE
      DO:
        RAW-TRANSFER ttTemp TO {&Remote-LDB}.{&Repl-Table}.
        RELEASE {&Remote-LDB}.{&Repl-Table}.
        ASSIGN iRemoteUpdate = iRemoteUpdate + 1.
      END.    
   END.
  WHEN "DELETE" THEN
    DO:
      IF AVAILABLE {&Remote-LDB}.{&Repl-Table} THEN
        DELETE {&Remote-LDB}.{&Repl-Table}.
        ASSIGN iRemoteDelete = iRemoteDelete + 1.
    END.
  END CASE.
  
RETURN "OK".  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-remote-records Include 
PROCEDURE apply-remote-records :
/*------------------------------------------------------------------------------
  Purpose: Apply records from the remote databse to the local database
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER rRaw AS RAW NO-UNDO.
DEFINE INPUT PARAMETER sEvent AS CHAR NO-UNDO.

FOR EACH ttTemp:
 DELETE ttTemp.
END.

RAW-TRANSFER rRaw TO ttTemp.
RELEASE ttTemp.

FIND FIRST ttTemp.

DISABLE TRIGGERS FOR LOAD of {&Local-LDB}.{&Repl-Table}.

  FIND FIRST {&Local-LDB}.{&Repl-Table} OF ttTemp EXCLUSIVE-LOCK NO-ERROR.
  CASE sEvent:
  WHEN "WRITE" THEN
  DO:
  IF NOT AVAILABLE {&Local-LDB}.{&Repl-Table} THEN
    DO:
      RAW-TRANSFER ttTemp TO {&Local-LDB}.{&Repl-Table}.
      RELEASE {&Local-LDB}.{&Repl-Table}.
    END.
  ELSE
    DO:
      RAW-TRANSFER ttTemp TO {&Local-LDB}.{&Repl-Table}.
      RELEASE {&Local-LDB}.{&Repl-Table}.
    END.    
  END.
  WHEN "DELETE" THEN
    DO:
      IF AVAILABLE {&Remote-LDB}.{&Repl-Table} THEN
        DELETE {&Remote-LDB}.{&Repl-Table}.
        ASSIGN iRemoteDelete = iRemoteDelete + 1.
    END.
  END CASE.
  
RETURN "OK".  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-collision Include 
PROCEDURE check-collision :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR iJulian AS INTEGER NO-UNDO.
DEFINE VAR v_coll_id AS INTEGER NO-UNDO.
DEFINE VAR sFields AS CHAR NO-UNDO.
ASSIGN 
iJulian = (((integer(today) - integer(date("1/1/90"))) * 24) * 60 * 60)
iJulian = iJulian + (integer(time)).

FOR EACH ttLocal:
  FOR EACH ttRemote OF ttLocal:
    IF AVAILABLE ttRemote THEN
      DO:
        BUFFER-COMPARE ttLocal TO ttRemote SAVE RESULT IN sFields NO-ERROR.
        
        IF sFields <> ? THEN
          DO:
            ASSIGN v_coll_id = NEXT-VALUE(collision-seq, {&Coll-Master}).
                  
            FIND ttLocLkup WHERE ttLocLkup.loc-row-id = STRING(ROWID(ttLocal)).
            FIND ttRemLkup WHERE ttRemLkup.rem-row-id = STRING(ROWID(ttRemote)).
            
            FOR EACH {&Local-LDB}.replicate-chng 
                      WHERE {&Local-LDB}.replicate-chng.trans-id = ttLocLkup.trans-id:
                       
               CREATE {&Coll-Master}.replicate-coll.
               FIND FIRST {&Local-LDB}.system-config NO-ERROR.
               ASSIGN {&Coll-Master}.replicate-coll.table-name = '{&Repl-Table}'
                      {&Coll-Master}.replicate-coll.trans-id = ttLocLkup.trans-id
                      {&Coll-Master}.replicate-coll.CollisionId = v_coll_id
                      {&Coll-Master}.replicate-coll.event = {&Local-LDB}.replicate-chng.event
                      {&Coll-Master}.replicate-coll.system-id = {&Local-LDB}.system-config.system-id
                      {&Coll-Master}.replicate-coll.CollisionDate = TODAY
                      {&Coll-Master}.replicate-coll.CollisionTime = STRING(TIME,"HH:MM:SS")
                      {&Coll-Master}.replicate-coll.DataRecord = {&Local-LDB}.replicate-chng.data-record.
               IF ttLocLkup.chng-row-id = STRING(ROWID({&Local-LDB}.replicate-chng))
               THEN ASSIGN {&Coll-Master}.replicate-coll.Collided = true.
               ELSE ASSIGN {&Coll-Master}.replicate-coll.Collided = false.
               RELEASE {&Coll-Master}.replicate-coll.
            END.   
            
            FOR EACH {&Remote-LDB}.replicate-chng 
                      WHERE {&Remote-LDB}.replicate-chng.trans-id = ttRemLkup.trans-id:
                       
               CREATE {&Coll-Master}.replicate-coll.
               FIND FIRST {&Remote-LDB}.system-config NO-ERROR.
               ASSIGN {&Coll-Master}.replicate-coll.table-name = '{&Repl-Table}'
                      {&Coll-Master}.replicate-coll.trans-id = ttRemLkup.trans-id
                      {&Coll-Master}.replicate-coll.CollisionId = v_coll_id
                      {&Coll-Master}.replicate-coll.event = {&Remote-LDB}.replicate-chng.event
                      {&Coll-Master}.replicate-coll.system-id = {&Remote-LDB}.system-config.system-id
                      {&Coll-Master}.replicate-coll.CollisionDate = TODAY
                      {&Coll-Master}.replicate-coll.CollisionTime = STRING(TIME,"HH:MM:SS")
                      {&Coll-Master}.replicate-coll.DataRecord = {&Remote-LDB}.replicate-chng.data-record.
               IF ttRemLkup.chng-row-id = STRING(ROWID({&Remote-LDB}.replicate-chng))
               THEN ASSIGN {&Coll-Master}.replicate-coll.Collided = true.
               ELSE ASSIGN {&Coll-Master}.replicate-coll.Collided = false.
               RELEASE {&Coll-Master}.replicate-coll.
            END.
          END.
    
        run show-message ("Collision detected; refer Collision log for details").
        DELETE ttLocal.
        DELETE ttRemote.
        ASSIGN iCollisionCount = iCollisionCount + 1.
      END.
   END.
END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-log-rec Include 
PROCEDURE check-for-log-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lLocalLog AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR lRemoteLog AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR lAnswer AS LOGICAL NO-UNDO.

FIND FIRST {&Local-LDB}.replicate-log 
     WHERE {&Local-LDB}.replicate-log.rep-with = sCurrSysId NO-LOCK NO-ERROR.
     
IF AVAILABLE {&Local-LDB}.{&Repl-Table} THEN
  ASSIGN lLocalLog = TRUE.
  
FIND FIRST {&Remote-LDB}.replicate-log 
     WHERE {&Remote-LDB}.replicate-log.rep-with = sSystemId NO-LOCK NO-ERROR.
     
IF AVAILABLE {&Remote-LDB}.{&Repl-Table} THEN
  ASSIGN lRemoteLog = TRUE.


IF NOT lLocalLog AND NOT lRemoteLog THEN
  DO:
    MESSAGE "There is no replication log for table {&Repl-Table}~n" +
            "and remote system " + sSystemId + "~n" +
            "do you wish to replicate entire table ?" VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO-CANCEL SET lAnswer.
    IF lAnswer THEN
       DO:
         RUN create-remote-log.
         RUN create-local-log.
         FIND FIRST {&Local-LDB}.replicate-log
              WHERE {&Local-LDB}.replicate-log.rep-table = '{&Repl-Table}'
                AND {&Local-LDB}.replicate-log.rep-with = sSystemId
                EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE {&Local-LDB}.replicate-log THEN
            ASSIGN {&Local-LDB}.replicate-log.gmt-julian = 0.
         ELSE
            DO:
              MESSAGE "Error creating replication log!!!" VIEW-AS ALERT-BOX.
              RETURN ERROR.
            END.
       END.     
     ELSE
       DO:
         RUN show-message ("Terminating Replication ....").
         RETURN "EXIT".
       END.
  END.
  
IF NOT lLocalLog AND lRemoteLog THEN
  DO:
    MESSAGE "There is no replication log for table {&Repl-Table}~n" +
            "on the local system " + sSystemId + "~n" + 
            "but a record was found on the remote server.~n"
            "Do you wish to replicate using information from~n" +
            "remote server (Suggested!)?" VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO-CANCEL SET lAnswer.
    IF lAnswer THEN
       DO:
          CREATE {&Local-LDB}.Replicate-Log.
          RAW-TRANSFER {&Remote-LDB}.Replicate-Log TO {&Local-LDB}.replicate-log.         
       END.     
     ELSE
       DO:
         RUN show-message ("Terminating Replication ....").
         RETURN "EXIT".
       END.

  END.
  
IF lLocalLog AND NOT lRemoteLog THEN
  DO:
    MESSAGE "There is no replication log for table {&Repl-Table}~n" +
            "on the remote system " + sCurrSysId + "~n" + 
            "but a record was found on the local database.~n"
            "Do you wish to replicate using information from~n" +
            "the local database (Suggested!)?" VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO-CANCEL SET lAnswer.
    IF lAnswer THEN
       DO:
          CREATE {&Remote-LDB}.Replicate-Log.
          RAW-TRANSFER {&Local-LDB}.Replicate-Log TO {&Remote-LDB}.replicate-log.         
       END.     
     ELSE
       DO:
         RUN show-message ("Terminating Replication ....").
         RETURN "EXIT".
       END.
  END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-local-log Include 
PROCEDURE create-local-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER dDate AS DATE NO-UNDO.
DEFINE VAR iJulian AS INTEGER NO-UNDO.

ASSIGN 
iJulian = (((integer(dDate) - integer(date("1/1/90"))) * 24) * 60 * 60)
iJulian = iJulian + (integer(time)).
 
FIND FIRST {&Local-LDB}.system-config  NO-LOCK NO-ERROR.

CREATE {&Local-LDB}.replicate-log.
ASSIGN
{&Local-LDB}.replicate-log.rep-table = '{&Repl-Table}'
{&Local-LDB}.replicate-log.rep-length = 0
{&Local-LDB}.replicate-log.rep-with = sSystemId
{&Local-LDB}.replicate-log.stat = "COM"
{&Local-LDB}.replicate-log.num-repl-sent = 0
{&Local-LDB}.replicate-log.num-repl-rec = 0
{&Local-LDB}.replicate-log.error-message = ""
{&Local-LDB}.replicate-log.mod-dt = dDate
{&Local-LDB}.replicate-log.mod-tm = string(time,"HH:MM:SS")
{&Local-LDB}.replicate-log.mod-user = "test"
{&Local-LDB}.replicate-log.gmt-mod-dt = dDate
{&Local-LDB}.replicate-log.gmt-mod-tm = string(time,"HH:MM:SS")
{&Local-LDB}.replicate-log.rec-owner = ""
{&Local-LDB}.replicate-log.gmt-julian =  iJulian.
   
             
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-remote-log Include 
PROCEDURE create-remote-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER dDate AS DATE NO-UNDO.
DEFINE VAR iJulian AS INTEGER NO-UNDO.

ASSIGN 
iJulian = (((integer(dDate) - integer(date("1/1/90"))) * 24) * 60 * 60)
iJulian = iJulian + (integer(time)).
 
FIND FIRST {&Local-LDB}.system-config  NO-LOCK NO-ERROR.

CREATE {&Remote-LDB}.replicate-log.
ASSIGN
{&Remote-LDB}.replicate-log.rep-table = '{&Repl-Table}'
{&Remote-LDB}.replicate-log.rep-length = 0
{&Remote-LDB}.replicate-log.rep-with = sCurrSysId
{&Remote-LDB}.replicate-log.stat = "COM"
{&Remote-LDB}.replicate-log.num-repl-sent = iRemoteNew + iRemoteUpdate
{&Remote-LDB}.replicate-log.num-repl-rec = 0
{&Remote-LDB}.replicate-log.error-message = ""
{&Remote-LDB}.replicate-log.mod-dt = dDate
{&Remote-LDB}.replicate-log.mod-tm = string(time,"HH:MM:SS")
{&Remote-LDB}.replicate-log.mod-user = "test"
{&Remote-LDB}.replicate-log.gmt-mod-dt = dDate
{&Remote-LDB}.replicate-log.gmt-mod-tm = string(time,"HH:MM:SS")
{&Remote-LDB}.replicate-log.rec-owner = ""
{&Remote-LDB}.replicate-log.gmt-julian =  iJulian.
            
 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-last-rep Include 
PROCEDURE get-last-rep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER iLocal AS INTEGER NO-UNDO.
DEF OUTPUT PARAMETER iRemote AS INTEGER NO-UNDO.

ASSIGN
iLocal = iLastLocalRep
iRemote = iLastRemoteRep.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-local-records Include 
PROCEDURE get-local-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b_localChng FOR {&Local-LDB}.replicate-chng.

FIND FIRST {&Local-LDB}.replicate-log 
   WHERE {&Local-LDB}.replicate-log.rep-table = '{&Repl-Table}'
    AND {&Local-LDB}.replicate-log.rep-with = sSystemId NO-LOCK NO-ERROR.


IF NOT AVAILABLE {&Local-LDB}.replicate-log THEN
  DO:
    RUN show-message ("ERROR -> No Replication Log Found For Table {&repl-table}").
    RUN create-local-log (date("1/1/90")).    
    FOR EACH {&Local-LDB}.replicate-chng
       WHERE {&Local-LDB}.replicate-chng.table-name =  '{&Repl-Table}' NO-LOCK:
       RAW-TRANSFER {&Local-LDB}.replicate-chng.data-record TO ttLocal NO-ERROR.
                &if "{&fldex}" <> ""
                    &then
       FIND FIRST b_localChng
         WHERE b_localChng.table-name =  '{&Repl-Table}|2'
         AND   b_localChng.create-tm  = {&Local-LDB}.replicate-chng.create-tm
       IF AVAILABLE b_localChng THEN
         RAW-TRANSFER b_localChng.data-record TO ttCharFld.
         ASSIGN ttLocal.{&fldex} = ttCharFld.fld.
       END.
                    &endif
       CREATE ttLocLkup.
       ASSIGN ttLocLkup.trans-id = {&Local-LDB}.replicate-chng.trans-id
              ttLocLkup.chng-row-id = STRING(ROWID({&Local-LDB}.replicate-chng))
              ttLocLkup.loc-row-id = STRING(ROWID(ttLocal)).
       RELEASE ttLocal.
    END.  
  END. 
ELSE
  DO:
    RUN show-message ("Last Local Rep of {&Repl-Table} was on " +
                       string({&Local-LDB}.replicate-log.mod-dt) + " - " +
                       {&Local-LDB}.replicate-log.mod-tm + " - " +
                       string({&Local-LDB}.replicate-log.gmt-julian)).
    FOR EACH {&Local-LDB}.replicate-chng
       WHERE {&Local-LDB}.replicate-chng.gmt-julian >= {&Local-LDB}.replicate-log.gmt-julian
         AND {&Local-LDB}.replicate-chng.table-name =  '{&Repl-Table}' NO-LOCK:
       RAW-TRANSFER {&Local-LDB}.replicate-chng.data-record TO ttLocal NO-ERROR.
                &if "{&fldex}" <> ""
                    &then
       FIND FIRST b_localChng
         WHERE b_localChng.table-name =  '{&Repl-Table}|2'
         AND   b_localChng.create-tm  = {&Local-LDB}.replicate-chng.create-tm
       IF AVAILABLE b_localChng THEN
         RAW-TRANSFER b_localChng.data-record TO ttCharFld.
         ASSIGN ttLocal.{&fldex} = ttCharFld.fld.
       END.
                    &endif
       CREATE ttLocLkup.
       ASSIGN ttLocLkup.trans-id = {&Local-LDB}.replicate-chng.trans-id
              ttLocLkup.chng-row-id = STRING(ROWID({&Local-LDB}.replicate-chng))
              ttLocLkup.loc-row-id = STRING(ROWID(ttLocal)).
       RELEASE ttLocal.
    END.  

  END.    

FIND FIRST {&Local-LDB}.replicate-log 
  WHERE {&Local-LDB}.replicate-log.rep-table = '{&Repl-Table}'
    AND {&Local-LDB}.replicate-log.rep-with = sSystemId NO-LOCK NO-ERROR.
    
IF AVAILABLE {&Local-LDB}.replicate-log  THEN
  ASSIGN iLastLocalRep = {&Local-LDB}.replicate-log.gmt-julian.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-remote-records Include 
PROCEDURE get-remote-records :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b_remoteChng FOR {&remote-LDB}.replicate-chng.

FIND FIRST {&Remote-LDB}.replicate-log 
   WHERE {&Remote-LDB}.replicate-log.rep-table = '{&Repl-Table}'
     AND {&Remote-LDB}.replicate-log.rep-with = sCurrSysId NO-LOCK NO-ERROR.

IF NOT AVAILABLE {&Remote-LDB}.replicate-log THEN
  DO:
    RUN show-message ("ERROR -> No Replication Log Found For Table {&repl-tabel}").
    RUN create-remote-log (date("1/1/90")).
    FOR EACH {&Remote-LDB}.replicate-chng
       WHERE {&Remote-LDB}.replicate-chng.table-name =  '{&Repl-Table}' NO-LOCK:
       RAW-TRANSFER {&Remote-LDB}.replicate-chng.data-record TO ttRemote NO-ERROR.
                &if "{&fldex}" <> ""
                    &then
       FIND FIRST b_remoteChng
         WHERE b_remoteChng.table-name =  '{&Repl-Table}|2'
         AND   b_remoteChng.create-tm  = {&Remote-LDB}.replicate-chng.create-tm
       IF AVAILABLE b_remoteChng THEN
         RAW-TRANSFER b_remoteChng.data-record TO ttCharFld.
         ASSIGN ttremote.{&fldex} = ttCharFld.fld.
       END.
                    &endif
       CREATE ttRemLkup.
       ASSIGN ttRemLkup.trans-id = {&Remote-LDB}.replicate-chng.trans-id
              ttRemLkup.chng-row-id = STRING(ROWID({&Remote-LDB}.replicate-chng))
              ttRemLkup.rem-row-id = STRING(ROWID(ttRemote)).
       RELEASE ttRemote.
    END.  
  END. 
ELSE
  DO:
    RUN show-message ("Last Remote Rep of {&Repl-Table} was on " +
                       string({&Remote-LDB}.replicate-log.mod-dt) + " - " +
                       {&Remote-LDB}.replicate-log.mod-tm + " - " +
                       string({&Remote-LDB}.replicate-log.gmt-julian)).
    FOR EACH {&Remote-LDB}.replicate-chng
       WHERE {&Remote-LDB}.replicate-chng.gmt-julian >= {&Remote-LDB}.replicate-log.gmt-julian
         AND {&Remote-LDB}.replicate-chng.table-name =  '{&Repl-Table}' NO-LOCK:
       RAW-TRANSFER {&Remote-LDB}.replicate-chng.data-record TO ttRemote NO-ERROR.
                &if "{&fldex}" <> ""
                    &then
       FIND FIRST b_remoteChng
         WHERE b_remoteChng.table-name =  '{&Repl-Table}|2'
         AND   b_remoteChng.create-tm  = {&Remote-LDB}.replicate-chng.create-tm
       IF AVAILABLE b_remoteChng THEN
         RAW-TRANSFER b_remoteChng.data-record TO ttCharFld.
         ASSIGN ttremote.{&fldex} = ttCharFld.fld.
       END.
                    &endif
       CREATE ttRemLkup.
       ASSIGN ttRemLkup.trans-id = {&Remote-LDB}.replicate-chng.trans-id
              ttRemLkup.chng-row-id = STRING(ROWID({&Remote-LDB}.replicate-chng))
              ttRemLkup.rem-row-id = STRING(ROWID(ttRemote)).
       RELEASE ttRemote.
    END.  

  END.    

FIND FIRST {&Remote-LDB}.replicate-log 
  WHERE {&Remote-LDB}.replicate-log.rep-table = '{&Repl-Table}'
    AND {&Remote-LDB}.replicate-log.rep-with = sSystemId NO-LOCK NO-ERROR.
    
IF AVAILABLE {&Remote-LDB}.replicate-log  THEN
  ASSIGN iLastRemoteRep = {&Remote-LDB}.replicate-log.gmt-julian.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-data-table Include 
PROCEDURE load-data-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replicate Include 
PROCEDURE replicate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
icollisionCount = 0.
RUN check-collision.
RUN apply-local-records.
RUN apply-remote-records.
RUN create-remote-log.
RUN create-local-log.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-system-ids Include 
PROCEDURE set-system-ids :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER sLocal AS CHAR NO-UNDO.
DEF INPUT PARAMETER sRemote AS CHAR NO-UNDO.

ASSIGN
sCurrSysId = sLocal
sSystemId = sRemote.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-message Include 
PROCEDURE show-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER sText AS CHAR NO-UNDO.
DEF VAR lOk AS LOGICAL NO-UNDO.

ASSIGN
lOk = eStatus:insert-string(sText + "~n").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



