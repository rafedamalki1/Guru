&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/28 - 12:36 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW  
{GLOBVAR2DEL1.I}
DEFINE VARIABLE houtlook AS Com-handle NO-UNDO.
DEFINE VARIABLE hnamespace AS Com-handle NO-UNDO.
DEFINE VARIABLE hfolder AS Com-handle NO-UNDO.
DEFINE VARIABLE hitem AS Com-handle NO-UNDO.
DEFINE VARIABLE num AS integer NO-UNDO.
DEFINE VARIABLE my1hand AS HANDLE NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE SHARED TEMP-TABLE tempepost NO-UNDO
   FIELD NAMN AS CHARACTER
   FIELD EPOST AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_VAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tempepost

/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL tempepost.NAMN tempepost.EPOST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH tempepost NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH tempepost NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL tempepost
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL tempepost


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_VAL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 BRW_VAL BTN_OK BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 12 BY 1.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 26.5
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VAL FOR 
      tempepost SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL DIALOG-1 _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      tempepost.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U WIDTH 25
      tempepost.EPOST COLUMN-LABEL "Epost" FORMAT "X(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 68.5 BY 23.25
         TITLE "Välj epostadress" ROW-HEIGHT-CHARS .38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_VAL AT ROW 2 COL 3
     BTN_OK AT ROW 25.75 COL 23.38
     BTN_AVB AT ROW 25.75 COL 39.38
     RECT-21 AT ROW 1 COL 1.5
     SPACE(0.74) SKIP(0.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj mottagare från listan":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: tempepost T "?" NO-UNDO temp-db tempepost
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_VAL RECT-21 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.tempepost"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempepost.NAMN
"tempepost.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tempepost.EPOST
"tempepost.EPOST" "Epost" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Välj mottagare från listan */
DO:
  {BORTBRWPROC.I}
   RELEASE OBJECT hfolder NO-ERROR.
   RELEASE OBJECT hnamespace NO-ERROR.
   RELEASE OBJECT houtlook NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Välj mottagare från listan */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL DIALOG-1
ON VALUE-CHANGED OF BRW_VAL IN FRAME DIALOG-1 /* Välj epostadress */
DO:
   &Scoped-define BROWSE-NAME BRW_VAL     
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   RUN enable_UI.       
   {FRMSIZED.I}
   
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE "outlook.application" houtlook NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO: 
   END.
   ELSE DO:
      hnamespace = houtlook:getnamespace("MAPI").
      hfolder = hnamespace:addresslists:ITEM(1).
   END.  
   IF VALID-HANDLE(hfolder) THEN DO:
      DO num = 1 TO hfolder:addressentries:COUNT():
         hitem = hfolder:addressentries:ITEM(num).
         CREATE TEMPEPOST.
         ASSIGN
         tempepost.NAMN = hitem:NAME
         tempepost.EPOST = hitem:address.
      END.
      RELEASE OBJECT hitem.
   END.         
/*    OUTPUT TO C:\EPOST.TXT.                                         */
/*    FOR EACH tempepost:                                             */
/*       PUT UNFORMATTED tempepost.NAMN + " " + tempepost.EPOST SKIP. */
/*    END.                                                            */
/*    OUTPUT CLOSE.                                                   */
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  ENABLE RECT-21 BRW_VAL BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

