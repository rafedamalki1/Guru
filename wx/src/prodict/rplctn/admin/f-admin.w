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
DEFINE VAR sSystemId AS CHAR NO-UNDO.
DEFINE VAR sOrigID AS CHAR NO-UNDO.
DEFINE VAR sOrigConnect AS CHAR NO-UNDO.
DEFINE VAR sOrigLDB AS CHAR NO-UNDO.

ASSIGN
sOrigLDB = LDBNAME(1).

&SCOPED-DEFINE Local-LDB sports
&SCOPED-DEFINE Remote-LDB remote

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame

&Scoped-define ADM-CONTAINER FRAME

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 COMBO-BOX-1 cbConnect RECT-4 ~
cbSysConfig cbReplicate cbRemote cbLog cbColl cbDel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON cbColl 
     LABEL "Collision Resolution" 
     SIZE 30 BY .85.

DEFINE BUTTON cbConnect 
     LABEL "Connect Admin DB" 
     SIZE 18.29 BY .85.

DEFINE BUTTON cbDel 
     LABEL "Deletion Resolution" 
     SIZE 30 BY .85.

DEFINE BUTTON cbDis 
     LABEL "Disconnect Admin DB" 
     SIZE 18.29 BY .85.

DEFINE BUTTON cbLog 
     LABEL "Replication Log" 
     SIZE 30 BY .85.

DEFINE BUTTON cbRemote 
     LABEL "Remote System Connect Parms" 
     SIZE 30 BY .85.

DEFINE BUTTON cbReplicate 
     LABEL "Current Records To Replicate" 
     SIZE 30 BY .85.

DEFINE BUTTON cbSysConfig 
     LABEL "System Configuration" 
     SIZE 30 BY .85.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Connect To" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 53.14 BY 1 NO-UNDO.

DEFINE VARIABLE flLogic AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logical DB" 
     VIEW-AS FILL-IN 
     SIZE 34.29 BY .81 NO-UNDO.

DEFINE VARIABLE flPhysical AS CHARACTER FORMAT "X(256)":U 
     LABEL "Physical DB" 
     VIEW-AS FILL-IN 
     SIZE 34.29 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 8.31.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.72 BY 12.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-1 AT ROW 1.77 COL 9.86 COLON-ALIGNED
     cbConnect AT ROW 2.85 COL 2.72
     flPhysical AT ROW 2.85 COL 28.72 COLON-ALIGNED
     cbDis AT ROW 3.77 COL 2.72
     flLogic AT ROW 3.77 COL 28.72 COLON-ALIGNED
     cbSysConfig AT ROW 5.85 COL 3
     cbReplicate AT ROW 5.85 COL 34
     cbRemote AT ROW 6.92 COL 3
     cbLog AT ROW 6.92 COL 34
     cbColl AT ROW 8 COL 3
     cbDel AT ROW 8 COL 34
     "Replication Administration" VIEW-AS TEXT
          SIZE 18.29 BY .5 AT ROW 1 COL 1.57
          FGCOLOR 9 
     RECT-5 AT ROW 1.31 COL 1
     RECT-4 AT ROW 5 COL 1.57
     "Admin Functions" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 5 COL 2.72
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 65.72 BY 13.23
         FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 13.5
         WIDTH              = 69.14.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Default                                                  */
/* SETTINGS FOR BUTTON cbDis IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-1 IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN flLogic IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN flPhysical IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
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

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME cbColl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbColl F-Frame-Win
ON CHOOSE OF cbColl IN FRAME F-Main /* Collision Resolution */
DO:
  RUN admin-collision.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbConnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbConnect F-Frame-Win
ON CHOOSE OF cbConnect IN FRAME F-Main /* Connect Admin DB */
DO:
  DEF VAR lOk AS LOGICAL NO-UNDO.
  RUN connect-to-remote NO-ERROR.
  IF RETURN-VALUE = "" OR RETURN-VALUE = ? THEN
    DO:
      RUN show-stats.
      ENABLE cbDis with frame {&FRAME-NAME}.
      APPLY "ENTRY" TO cbDis.
      DISABLE cbConnect with frame {&FRAME-NAME}.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDis F-Frame-Win
ON CHOOSE OF cbDis IN FRAME F-Main /* Disconnect Admin DB */
DO:

  RUN disconnect-db.
  RUN show-stats.
  DISABLE cbDis WITH FRAME {&FRAME-NAME}.
  RUN check-if-local. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLog F-Frame-Win
ON CHOOSE OF cbLog IN FRAME F-Main /* Replication Log */
DO:
 RUN admin-replication-log.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbRemote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbRemote F-Frame-Win
ON CHOOSE OF cbRemote IN FRAME F-Main /* Remote System Connect Parms */
DO:
  RUN admin-remote-setup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbReplicate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbReplicate F-Frame-Win
ON CHOOSE OF cbReplicate IN FRAME F-Main /* Current Records To Replicate */
DO:
 RUN admin-change-log.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSysConfig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSysConfig F-Frame-Win
ON CHOOSE OF cbSysConfig IN FRAME F-Main /* System Configuration */
DO:
 RUN admin-system-config.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-1 F-Frame-Win
ON VALUE-CHANGED OF COMBO-BOX-1 IN FRAME F-Main /* Connect To */
DO:
  RUN check-if-local.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-change-log F-Frame-Win 
PROCEDURE admin-change-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN adecomm/_setcurs.p ("WAIT").
  RUN prodict/rplctn/admin/d-chngs.w.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-collision F-Frame-Win 
PROCEDURE admin-collision :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN adecomm/_setcurs.p ("WAIT").
  RUN prodict/rplctn/admin/d-collis.w.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-delete F-Frame-Win 
PROCEDURE admin-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE "This function not enable!" view-as alert-box.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-remote-setup F-Frame-Win 
PROCEDURE admin-remote-setup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iIndex AS INTEGER NO-UNDO.

  RUN adecomm/_setcurs.p ("WAIT").  
  RUN prodict/rplctn/admin/d-systms.w.
  RUN adecomm/_setcurs.p ("WAIT").  
  IF cbConnect:sensitive IN FRAME {&FRAME-NAME} THEN
    DO:
      iIndex = LOOKUP(COMBO-BOX-1:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                      COMBO-BOX-1:LIST-ITEMS).
      RUN load-replicate-with. 
      ASSIGN
      COMBO-BOX-1:SCREEN-VALUE = ENTRY(iIndex,COMBO-BOX-1:LIST-ITEMS).
    END.
  RUN adecomm/_setcurs.p ("").  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-replication-log F-Frame-Win 
PROCEDURE admin-replication-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN adecomm/_setcurs.p ("WAIT").
    RUN prodict/rplctn/admin/d-log.w.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE admin-system-config F-Frame-Win 
PROCEDURE admin-system-config :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN adecomm/_setcurs.p ("WAIT").
  RUN prodict/rplctn/admin/d-syscfg.w.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-if-local F-Frame-Win 
PROCEDURE check-if-local :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR sId AS CHAR NO-UNDO.
  DEF VAR iIndex AS INTEGER NO-UNDO.
  
  ASSIGN
  iIndex = COMBO-BOX-1:lookup(COMBO-BOX-1:screen-value) IN FRAME {&FRAME-NAME}
  sId = entry(iIndex,COMBO-BOX-1:private-data).
  

  IF sId = sSystemId THEN
    DISABLE cbConnect WITH FRAME {&FRAME-NAME}.
  ELSE
    DO:
      IF NOT cbDis:sensitive IN FRAME {&FRAME-NAME} THEN
        ENABLE  cbConnect WITH FRAME {&FRAME-NAME}.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connect-db F-Frame-Win 
PROCEDURE connect-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER sConnect AS CHAR NO-UNDO.

run prodict/rplctn/stdobj/dbconn.p (sConnect,"temp").

CREATE ALIAS admin FOR DATABASE temp.

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
DEF VAR sConnect AS CHAR NO-UNDO.

ASSIGN
sDB = entry(lookup(COMBO-BOX-1:screen-value IN FRAME {&FRAME-NAME},COMBO-BOX-1:LIST-ITEMS),COMBO-BOX-1:PRIVATE-DATA)
sDesc = COMBO-BOX-1:screen-value IN FRAME {&FRAME-NAME}.

FIND FIRST admin.replicate-mstr
  WHERE admin.replicate-mstr.system-id = sDb NO-LOCK.

 
ASSIGN
sSystemId = admin.replicate-mstr.system-id
sConnect = admin.replicate-mstr.connect-string.

IF sOrigId = sSystemId THEN
  RETURN.

RUN connect-db (sConnect).

IF NOT ERROR-STATUS:ERROR THEN
   MESSAGE "Connected Remote Database as ADMIN" VIEW-AS ALERT-BOX.
ELSE
   DO:
     MESSAGE "ERROR -> " + ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
     RETURN "ERROR".
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disconnect-db F-Frame-Win 
PROCEDURE disconnect-db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* disconnect and set the admin alias to the original database */

IF sOrigID = sSystemId THEN
  RETURN.
ELSE
  DO:
    RUN prodict/rplctn/stdobj/discon.p (sOrigLDB).
    ASSIGN
    sSystemId = sOrigId.
  END.

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
  ENABLE RECT-5 COMBO-BOX-1 cbConnect RECT-4 cbSysConfig cbReplicate cbRemote 
         cbLog cbColl cbDel 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-local-id F-Frame-Win 
PROCEDURE get-local-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST admin.system-config NO-LOCK NO-ERROR.

IF NOT AVAILABLE admin.system-config THEN
   RETURN "ERROR".
ELSE
  ASSIGN sOrigId = admin.system-config.system-id.

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
FOR EACH admin.replicate-mstr NO-LOCK:
  ASSIGN
  sText = IF sText = "" 
          THEN admin.replicate-mstr.description 
          ELSE sText + "," + admin.replicate-mstr.description
  sDb = IF sDb = "" 
          THEN admin.replicate-mstr.system-id
          ELSE sDb + "," + admin.replicate-mstr.system-id.
.
END.

ASSIGN
COMBO-BOX-1:LIST-ITEMS IN FRAME {&FRAME-NAME} = sText
COMBO-BOX-1:PRIVATE-DATA = sDb.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects F-Frame-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  CREATE ALIAS admin FOR DATABASE VALUE(LDBNAME(1)).
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy F-Frame-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN disconnect-db.
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

  RUN get-local-id.
  ASSIGN sSystemId = sOrigId.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
  COMBO-BOX-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,COMBO-BOX-1:LIST-ITEMS).
  RUN show-stats.
  RUN check-if-local.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize F-Frame-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-stats F-Frame-Win 
PROCEDURE show-stats :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
display pdbname("admin") @ flPhysical
        ldbname("admin") @ flLogic with frame {&FRAME-NAME}.
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
  
  CASE p-state:
  WHEN "run-sys-config" THEN
    RUN admin-system-config.
  WHEN "run-rep-log" THEN
    RUN admin-replication-log.
  WHEN "run-chng-log" THEN
    RUN admin-change-log.
  WHEN "run-collision" THEN
    RUN admin-collision.
  WHEN "run-delete" THEN
    RUN admin-delete.
  WHEN "run-remote-parms" THEN
    RUN admin-remote-setup.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


