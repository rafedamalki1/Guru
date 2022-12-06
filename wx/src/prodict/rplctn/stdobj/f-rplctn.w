&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR lOk AS LOGICAL NO-UNDO. 
DEF VAR sSystemId AS CHAR NO-UNDO.
DEF VAR sCurrSysId AS CHAR NO-UNDO.

DEF NEW SHARED TEMP-TABLE ttRepProcs 
    FIELD table-name AS CHAR
    FIELD proc-hdl AS HANDLE
    FIELD iLastLocalRep AS INTEGER
    FIELD iLastRemoteRep AS INTEGER
    INDEX table-name AS PRIMARY UNIQUE table-name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 eStat COMBO-BOX-1 BUTTON-2 BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Begin Replication" 
     SIZE 21.14 BY .85.

DEFINE BUTTON BUTTON-3 
     LABEL "Cancel Replication" 
     SIZE 21.14 BY .85.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Replicate With" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 48.57 BY 1 NO-UNDO.

DEFINE VARIABLE eStat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 64.57 BY 7.85 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67.14 BY 11.85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     eStat AT ROW 1.77 COL 2.72 NO-LABEL
     COMBO-BOX-1 AT ROW 10.08 COL 11.57 COLON-ALIGNED
     BUTTON-2 AT ROW 11.15 COL 14.14
     BUTTON-3 AT ROW 11.15 COL 35.86
     "Full Replication" VIEW-AS TEXT
          SIZE 10.86 BY .5 AT ROW 1 COL 1.57
          FGCOLOR 9 
     RECT-6 AT ROW 1.31 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.14 BY 12.15
         FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 12.15
         WIDTH              = 67.29.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Default                                                  */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-1 IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR EDITOR eStat IN FRAME F-Main
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{prodict/rplctn/stdobj/repdef.i}
{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 F-Frame-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Begin Replication */
DO:
  RUN connect-to-remote.
  DEF VAR hProc AS HANDLE.

  FOR EACH {&Local-LDB}._File WHERE
     {&Local-LDB}._File._Fil-misc2[6] <> ? 
      AND {&Local-LDB}._File._Fil-misc2[6] <> "" NO-LOCK:
       DO: 
         RUN check-for-rep-proc (INPUT {&Local-LDB}._File._Fil-misc2[6], OUTPUT hProc).
       END.
  END.
  
  RUN prodict/rplctn/stdobj/transrep.p (THIS-PROCEDURE).
  
  FOR EACH ttRepProcs:
     RUN create-local-log IN ttRepProcs.proc-hdl (today).
     RUN create-remote-log IN ttRepProcs.proc-hdl (today).
  END.
  
  RUN disconnect-remote.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-rep-proc F-Frame-Win 
PROCEDURE check-for-rep-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER sTable AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER hHandle AS HANDLE NO-UNDO.
DEFINE VAR iLocal AS INTEGER NO-UNDO.
DEFINE VAR iRemote AS INTEGER NO-UNDO.

FIND FIRST ttRepProcs 
  WHERE ttRepProcs.table-name = sTable NO-ERROR.

IF NOT AVAILABLE ttRepProcs THEN
  DO:
    IF AVAILABLE {&Local-LDB}._File THEN
      DO:
         RUN VALUE({&Local-LDB}._File._Fil-misc2[6])
             PERSISTENT SET hHandle (INPUT sSystemId,
                                     INPUT sCurrSysId,
                                     INPUT eStat:HANDLE IN FRAME {&FRAME-NAME})
                                     NO-ERROR.

         RUN get-last-rep IN hHandle (OUTPUT iLocal,OUTPUT iRemote).
                  
         CREATE ttRepProcs.
         ASSIGN ttRepProcs.table-name = {&Local-LDB}._File._File-Name
                ttRepProcs.proc-hdl = hHandle
                ttRepProcs.iLastLocalRep = iLocal
                ttRepProcs.iLastRemoteRep = iRemote.

         RELEASE ttRepProcs.
         
      END.
    ELSE
      DO:
         MESSAGE "Error Running Replication Procedure" view-as alert-box.
         RETURN ERROR.
      END.   
  END.
ELSE
  ASSIGN
   hHandle = ttRepProcs.proc-hdl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connect-to-remote F-Frame-Win 
PROCEDURE connect-to-remote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR sDb AS CHAR NO-UNDO.
DEF VAR sDesc AS CHAR NO-UNDO.

ASSIGN
sDB = entry(lookup(COMBO-BOX-1:screen-value IN FRAME {&FRAME-NAME},COMBO-BOX-1:LIST-ITEMS),COMBO-BOX-1:PRIVATE-DATA)
sDesc = COMBO-BOX-1:screen-value IN FRAME {&FRAME-NAME}.
RUN show-message ("Connecting To Database - " + sDesc + " (" + sDb + ")").

FIND FIRST {&Local-LDB}.system-config NO-LOCK NO-ERROR.
IF AVAILABLE {&Local-LDB}.system-config THEN
  DO:
    ASSIGN
    sCurrSysId = {&Local-LDB}.system-config.system-id.
  END.
ELSE
  RETURN ERROR.

FIND FIRST {&Local-LDB}.replicate-mstr
  WHERE {&Local-LDB}.replicate-mstr.system-id = sDb NO-LOCK.

run prodict/rplctn/stdobj/dbconn.p ({&Local-LDB}.replicate-mstr.connect-string,"remote").

IF ERROR-STATUS:ERROR THEN
  DO:
     RUN show-message ("ERROR -> " + ERROR-STATUS:GET-MESSAGE(1)).
  END.
ELSE
  DO:
    RUN show-message ("Datbase - " + sDesc + " Connected!").
    ASSIGN
    sSystemId = sDb.
   END.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-rep-procs F-Frame-Win 
PROCEDURE delete-rep-procs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttRepProcs:
  DELETE PROCEDURE ttRepProcs.proc-hdl NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
     DELETE ttRepProcs.
  ELSE
     MESSAGE ERROR-STATUS:GET-MESSAGE(ERROR-STATUS:NUM-MESSAGES) VIEW-AS ALERT-BOX.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disconnect-remote F-Frame-Win 
PROCEDURE disconnect-remote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN delete-rep-procs.
DISCONNECT remote.
RUN Show-message ("Remote Database Disconnected...").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE RECT-6 eStat COMBO-BOX-1 BUTTON-2 BUTTON-3 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-last-log-rec F-Frame-Win 
PROCEDURE get-last-log-rec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lLocalLog AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR lRemoteLog AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR lAnswer AS LOGICAL NO-UNDO.

/*
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
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-replicate-with F-Frame-Win 
PROCEDURE load-replicate-with :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR sText AS CHAR NO-UNDO.
DEF VAR sdb AS CHAR NO-UNDO.

DEF VAR lOk AS LOGICAL NO-UNDO.
FOR EACH {&Local-LDB}.replicate-mstr NO-LOCK:
  ASSIGN
  sText = IF sText = "" 
          THEN {&Local-LDB}.replicate-mstr.description 
          ELSE sText + "," + {&Local-LDB}.replicate-mstr.description
  sDb = IF sDb = "" 
          THEN {&Local-LDB}.replicate-mstr.system-id
          ELSE sDb + "," + {&Local-LDB}.replicate-mstr.system-id.

END.

ASSIGN
COMBO-BOX-1:LIST-ITEMS IN FRAME {&FRAME-NAME} = sText
COMBO-BOX-1:PRIVATE-DATA = sDb.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy F-Frame-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  IF CONNECTED("remote") THEN
    RUN disconnect-remote.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable F-Frame-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN load-replicate-with.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
  COMBO-BOX-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,COMBO-BOX-1:LIST-ITEMS).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replicate-by-transaction F-Frame-Win 
PROCEDURE replicate-by-transaction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartFrame, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-message F-Frame-Win 
PROCEDURE show-message :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER sText AS CHAR NO-UNDO.

  ASSIGN
  lOk = eStat:insert-string(sText + "~n") IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


