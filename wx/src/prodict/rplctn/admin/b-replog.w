&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          admin            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  b-replog.w

  Description: Browser for Data Replication Logs

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES admin.replicate-log

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table admin.replicate-log.rep-with admin.replicate-log.rep-table admin.replicate-log.gmt-mod-dt admin.replicate-log.gmt-mod-tm admin.replicate-log.stat admin.replicate-log.error-message 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define FIELD-PAIRS-IN-QUERY-br_table
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH admin.replicate-log NO-LOCK ~
    BY admin.replicate-log.rep-with ~
       BY admin.replicate-log.rep-table ~
        BY admin.replicate-log.gmt-mod-dt ~
         BY admin.replicate-log.gmt-mod-tm.
&Scoped-define TABLES-IN-QUERY-br_table admin.replicate-log
&Scoped-define FIRST-TABLE-IN-QUERY-br_table admin.replicate-log


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 tgDesc br_table cbClear cbDelete 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 tgDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON cbClear 
     LABEL "Clear Log" 
     SIZE 20 BY 1.

DEFINE BUTTON cbDelete 
     LABEL "Delete Record" 
     SIZE 20 BY 1.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "System Id", 1,
"Date", 2,
"Table Name", 3
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE tgDesc AS LOGICAL INITIAL no 
     LABEL "Descending" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.72 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      admin.replicate-log SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      admin.replicate-log.rep-with FORMAT "X(15)"
      admin.replicate-log.rep-table FORMAT "X(20)"
      admin.replicate-log.gmt-mod-dt
      admin.replicate-log.gmt-mod-tm
      admin.replicate-log.stat
      admin.replicate-log.error-message
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 67 BY 9.96
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RADIO-SET-1 AT ROW 1.27 COL 2 NO-LABEL
     tgDesc AT ROW 1.27 COL 50
     br_table AT ROW 2.08 COL 1
     cbClear AT ROW 12.31 COL 3
     cbDelete AT ROW 12.31 COL 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 12.62
         WIDTH              = 67.57.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Default                                      */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "admin.replicate-log"
     _Options          = "NO-LOCK"
     _OrdList          = "admin.replicate-log.rep-with|yes,admin.replicate-log.rep-table|yes,admin.replicate-log.gmt-mod-dt|yes,admin.replicate-log.gmt-mod-tm|yes"
     _FldNameList[1]   > admin.replicate-log.rep-with
"admin.replicate-log.rep-with" ? "X(15)" "character" ? ? ? ? ? ? no ?
     _FldNameList[2]   > admin.replicate-log.rep-table
"admin.replicate-log.rep-table" ? "X(20)" "character" ? ? ? ? ? ? no ?
     _FldNameList[3]   = admin.replicate-log.gmt-mod-dt
     _FldNameList[4]   = admin.replicate-log.gmt-mod-tm
     _FldNameList[5]   = admin.replicate-log.stat
     _FldNameList[6]   = admin.replicate-log.error-message
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbClear B-table-Win
ON CHOOSE OF cbClear IN FRAME F-Main /* Clear Log */
DO:
  DEF VAR lButton AS LOGICAL NO-UNDO.

  MESSAGE "Purge Replication Log ?" 
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL SET lButton.
  
  IF lButton THEN DO:
    FOR EACH admin.replicate-log:
      DELETE admin.replicate-log.
    END.
    RUN dispatch IN THIS-PROCEDURE ("open-query").
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbDelete B-table-Win
ON CHOOSE OF cbDelete IN FRAME F-Main /* Delete Record */
DO:
  DEF VAR lButton AS LOGICAL NO-UNDO.

  MESSAGE "Delete Replication Log Entry?" 
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL SET lButton.
  
  IF lButton THEN DO:
    FIND CURRENT admin.replicate-log EXCLUSIVE-LOCK.
    IF AVAILABLE admin.replicate-log THEN
      DELETE admin.replicate-log.
    RUN dispatch IN THIS-PROCEDURE ("open-query").
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 B-table-Win
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME F-Main
DO:
  RUN local-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgDesc B-table-Win
ON VALUE-CHANGED OF tgDesc IN FRAME F-Main /* Descending */
DO:
  RUN local-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       Open query with different sorts based on the radio-set value &
               check box value
------------------------------------------------------------------------------*/
  CASE RADIO-SET-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
    WHEN "1" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
          OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.rep-with DESC.
        ELSE
          OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.rep-with.
      END.
    WHEN "2" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
          OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.gmt-mod-dt
              BY admin.replicate-log.gmt-mod-tm DESC.
        ELSE
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.gmt-mod-dt
              BY admin.replicate-log.gmt-mod-tm.
      END.
    WHEN "3" THEN
      DO:
        CLOSE QUERY {&BROWSE-NAME}.
        IF INPUT tgDesc THEN
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.rep-table DESC.
        ELSE
           OPEN QUERY {&BROWSE-NAME} FOR EACH admin.replicate-log NO-LOCK
              BY admin.replicate-log.rep-table.
      END.
  END CASE.
 
  GET FIRST {&BROWSE-NAME} NO-LOCK.
  APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "admin.replicate-log"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


