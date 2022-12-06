&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          admin            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: d-chngs.w

  Description: SmartDialog for Changes not yet Replicated

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

 DEFINE VAR hColl AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES admin.replicate-chng

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 admin.replicate-chng.trans-id admin.replicate-chng.table-name admin.replicate-chng.event admin.replicate-chng.gmt-mod-dt admin.replicate-chng.gmt-mod-tm admin.replicate-chng.gmt-julian admin.replicate-chng.rec-owner 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH admin.replicate-chng NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 admin.replicate-chng
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 admin.replicate-chng


/* Definitions for DIALOG-BOX D-Dialog                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 tgDesc BROWSE-2 cbDelete cbDelTrans cbDelAll Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 tgDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_f-dbinfo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON cbDelAll 
     LABEL "Delete All Records" 
     SIZE 18 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON cbDelete 
     LABEL "Delete Record" 
     SIZE 18 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON cbDelTrans 
     LABEL "Delete Transaction" 
     SIZE 18 BY 1.08
     BGCOLOR 8 .

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Transaction Id", 1,
"Julian Date", 2,
"Table Name", 3
     SIZE 45.72 BY .77 NO-UNDO.

DEFINE VARIABLE tgDesc AS LOGICAL INITIAL yes 
     LABEL "Descending" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.72 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      admin.replicate-chng SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      admin.replicate-chng.trans-id
      admin.replicate-chng.table-name
      admin.replicate-chng.event
      admin.replicate-chng.gmt-mod-dt
      admin.replicate-chng.gmt-mod-tm
      admin.replicate-chng.gmt-julian
      admin.replicate-chng.rec-owner
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 74.43 BY 8.77
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     RADIO-SET-1 AT ROW 1.31 COL 3.86 NO-LABEL
     tgDesc AT ROW 1.31 COL 54.72
     BROWSE-2 AT ROW 2.23 COL 2.57
     cbDelete AT ROW 11.23 COL 22
     cbDelTrans AT ROW 11.23 COL 40.57
     cbDelAll AT ROW 11.23 COL 59
     Btn_OK AT ROW 12.58 COL 40
     Btn_Cancel AT ROW 12.58 COL 52.57
     Btn_Help AT ROW 12.58 COL 65.14
     SPACE(1.71) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Record Changes Not Replicated".

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "admin.replicate-chng"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = admin.replicate-chng.trans-id
     _FldNameList[2]   = admin.replicate-chng.table-name
     _FldNameList[3]   = admin.replicate-chng.event
     _FldNameList[4]   = admin.replicate-chng.gmt-mod-dt
     _FldNameList[5]   = admin.replicate-chng.gmt-mod-tm
     _FldNameList[6]   = admin.replicate-chng.gmt-julian
     _FldNameList[7]   = admin.replicate-chng.rec-owner
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Record Changes Not Replicated */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help D-Dialog
ON CHOOSE OF Btn_Help IN FRAME D-Dialog /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDelAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDelAll D-Dialog
ON CHOOSE OF cbDelAll IN FRAME D-Dialog /* Delete All Records */
DO:
  DEF VAR lButton AS LOGICAL NO-UNDO.

  IF AVAILABLE admin.replicate-chng THEN
    DO:
       MESSAGE "Do you want to delete all records for replication?" VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL SET lButton.
       IF lButton THEN
         DO:
           FOR EACH admin.replicate-chng:
              DELETE admin.replicate-chng.
           END.
           RUN local-open-query.
         END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDelete D-Dialog
ON CHOOSE OF cbDelete IN FRAME D-Dialog /* Delete Record */
DO:
  DEF VAR lButton AS LOGICAL NO-UNDO.

  IF AVAILABLE admin.replicate-chng THEN
    DO:
       MESSAGE "Do you want to delete this record ?" VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL SET lButton.
       IF lButton THEN
         DO:
           FIND CURRENT admin.replicate-chng EXCLUSIVE-LOCK NO-ERROR.
           IF AVAILABLE admin.replicate-chng THEN
           DELETE admin.replicate-chng.
           RUN local-open-query.
         END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDelTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDelTrans D-Dialog
ON CHOOSE OF cbDelTrans IN FRAME D-Dialog /* Delete Transaction */
DO:
  DEF VAR lButton AS LOGICAL NO-UNDO.
  DEF VAR iTrans AS INTEGER NO-UNDO.
  
  ASSIGN iTrans = admin.replicate-chng.trans-id NO-ERROR.

  IF AVAILABLE admin.replicate-chng THEN
    DO:
       MESSAGE "Do you want to delete all records in this transaction?" 
                      VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL SET lButton.
       IF lButton THEN
         DO:
           FOR EACH admin.replicate-chng
               WHERE admin.replicate-chng.trans-id = iTrans:
              DELETE admin.replicate-chng.
           END.
           RUN local-open-query.
         END. 
    END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 D-Dialog
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME D-Dialog
DO:
  RUN local-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgDesc D-Dialog
ON VALUE-CHANGED OF tgDesc IN FRAME D-Dialog /* Descending */
DO:
  RUN local-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ASSIGN SESSION:APPL-ALERT-BOX = TRUE.
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'rplctn/stdobj/f-dbinfo.w':U ,
             INPUT  FRAME D-Dialog:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_f-dbinfo ).
       RUN set-position IN h_f-dbinfo ( 12.85 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 0.96 , 25.00 ) */

    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-1 tgDesc 
      WITH FRAME D-Dialog.
  ENABLE RADIO-SET-1 tgDesc BROWSE-2 cbDelete cbDelTrans cbDelAll Btn_OK 
         Btn_Cancel Btn_Help 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN local-open-query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query D-Dialog 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       Open query with different sort depending on value of radio-set
------------------------------------------------------------------------------*/
 CASE RADIO-SET-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
   WHEN "1" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
          DO:
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.trans-id DESC.
          END.
        ELSE
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.trans-id.
      END.
   WHEN "2" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.gmt-julian DESC.
        ELSE
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.gmt-julian.
      END.
   WHEN "3" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.table-name DESC.
        ELSE
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-chng NO-LOCK
              BY admin.replicate-chng.table-name.
      END.

 END CASE.
 
 GET FIRST {&BROWSE-NAME} NO-LOCK.
 APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "admin.replicate-chng"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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


