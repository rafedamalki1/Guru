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
DEFINE INPUT PARAMETER epostnamn AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER epostvar AS CHARACTER NO-UNDO.
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
   FIELD EPOST AS CHARACTER
   INDEX NAMN NAMN
   INDEX EPOST EPOST.

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
&Scoped-Define ENABLED-OBJECTS RECT-49 IMAGE-1 BRW_VAL FILL-IN_SFORNAMN ~
FILL-IN_SEFTERNAMN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .83.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER/sokpa.gif":U
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68.5 BY 2.42
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
      tempepost.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 68.5 BY 23.25
         TITLE "Välj epostadress" ROW-HEIGHT-CHARS .38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_VAL AT ROW 1.46 COL 1.5
     FILL-IN_SFORNAMN AT ROW 25.17 COL 17.5 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 26.29 COL 17.5 COLON-ALIGNED
     BTN_OK AT ROW 27.71 COL 56.13
     BTN_AVB AT ROW 27.71 COL 71.13
     RECT-49 AT ROW 24.96 COL 1.5
     IMAGE-1 AT ROW 25.17 COL 2.13
     SPACE(75.36) SKIP(2.91)
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
/* BROWSE-TAB BRW_VAL IMAGE-1 DIALOG-1 */
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
"tempepost.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
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
   epostvar = "".
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
ON LEFT-MOUSE-DBLCLICK OF BRW_VAL IN FRAME DIALOG-1 /* Välj epostadress */
DO:
   APPLY "CHOOSE" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL DIALOG-1
ON VALUE-CHANGED OF BRW_VAL IN FRAME DIALOG-1 /* Välj epostadress */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE tempepost THEN epostvar = tempepost.EPOST.
   ELSE epostvar = "".
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


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* OK */
DO:
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
  {BORTBRWPROC.I}
   RELEASE OBJECT hfolder NO-ERROR.
   RELEASE OBJECT hnamespace NO-ERROR.
   RELEASE OBJECT houtlook NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON LEAVE OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* E-post */
DO:
  FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON LEAVE OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Namn */
DO:
  FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
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
   epostvar = "".
   {ALLSTARTDYN.I}
   RUN enable_UI.       
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].
   FIND FIRST tempepost WHERE tempepost.NAMN = epostnamn NO-LOCK NO-ERROR.
   IF AVAILABLE tempepost THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tempepost)).
   END.
   RUN lastselectdyn_UI IN brwproc[1].
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
   FIND FIRST tempepost NO-LOCK NO-ERROR.
   IF AVAILABLE tempepost THEN DO:
   END.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF Guru.Konstanter:appcon THEN DO:      
            RUN PERSEPOST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT TABLE tempepost).
         END.
         ELSE DO:
            RUN PERSEPOST.P (OUTPUT TABLE tempepost).
         END.
      END.
      ELSE DO:
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
               CREATE tempepost.
               ASSIGN
               tempepost.NAMN = hitem:NAME
               tempepost.EPOST = hitem:address.
            END.
            RELEASE OBJECT hitem NO-ERROR.
         END.       
      END.
   /*    OUTPUT TO C:\EPOST.TXT.                                         */
   /*    FOR EACH tempepost:                                             */
   /*       PUT UNFORMATTED tempepost.NAMN + " " + tempepost.EPOST SKIP. */
   /*    END.                                                            */
   /*    OUTPUT CLOSE.                                                   */
   END.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SFORNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "NAMN").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SEFTERNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "EPOST"). 
   
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
  DISPLAY FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-49 IMAGE-1 BRW_VAL FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK 
         BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

