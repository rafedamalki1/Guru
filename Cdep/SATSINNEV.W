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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SMTRL.I}
{MTRLTEMP.I}
DEFINE SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE VARIABLE levvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.


{LISTMTRL.I}

DEFINE BUFFER listbuff FOR list_mtrl.
DEFINE BUFFER satsbuff FOR satstemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_SATS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES satstemp

/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp.ENR2 satstemp.BENAMNING2 ~
satstemp.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS 
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp ~
      WHERE satstemp.SATS = FALSE NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp ~
      WHERE satstemp.SATS = FALSE NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_SATS btn_ok FILL-IN-KOD FILL-IN-BEN ~
FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 
&Scoped-Define DISPLAYED-OBJECTS TOG_VAL FILL-IN-KOD FILL-IN-BEN ~
FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_ok 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kod" 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Pris" 
      VIEW-AS TEXT 
     SIZE 13 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Satsinnehållet till beredningen" 
     VIEW-AS TOGGLE-BOX
     SIZE 52.25 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS DIALOG-1 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp.ENR2 FORMAT "X(11)":U
      satstemp.BENAMNING2 FORMAT "x(40)":U
      satstemp.ANTAL FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 52.5 BY 6.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_VAL AT ROW 10.38 COL 1.5
     BRW_SATS AT ROW 11.5 COL 1.5
     btn_ok AT ROW 18.38 COL 40
     FILL-IN-KOD AT ROW 1.88 COL 10.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.42 COL 10.25 COLON-ALIGNED
     FILL-IN-ENR AT ROW 4.96 COL 10.25 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 6.5 COL 10.25 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 8 COL 10.25 COLON-ALIGNED
     SPACE(29.24) SKIP(10.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Satsinnehåll":L
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_SATS TOG_VAL DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.satstemp.SATS = FALSE"
     _FldNameList[1]   = Temp-Tables.satstemp.ENR2
     _FldNameList[2]   = Temp-Tables.satstemp.BENAMNING2
     _FldNameList[3]   = Temp-Tables.satstemp.ANTAL
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Satsinnehåll */
DO:
  {BORTBRWPROC.I}
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Satsinnehåll */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* OK */
DO:    
   IF TOG_VAL = TRUE THEN DO:
      satsinn = TRUE.
      FOR EACH satstemp WHERE satstemp.KOD = kodvar AND
      satstemp.LEVKOD = levvar AND satstemp.SATS = FALSE USE-INDEX KOD NO-LOCK:         
         CREATE listbuff.
         ASSIGN  
         listbuff.NUM = list_mtrl.NUM  
         listbuff.SKAPNUM = list_mtrl.SKAPNUM        
         listbuff.ENR = satstemp.ENR2
         listbuff.BENAMNING = satstemp.BENAMNING2
         listbuff.PRIS = satstemp.PRIS2           
         listbuff.LEVKOD = satstemp.LEVKOD
         listbuff.ANTAL = satstemp.ANTAL               
         listbuff.ENHET = satstemp.ENHET2
         listbuff.LINKAB = FALSE
         listbuff.SATS = FALSE.
         FIND FIRST satsbuff WHERE satsbuff.KOD = satstemp.ENR2 AND
         satsbuff.LEVKOD = levvar AND satsbuff.SATS = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE satsbuff THEN listbuff.SATS = TRUE.
      END.
      FIND list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid EXCLUSIVE-LOCK NO-ERROR.
      DELETE list_mtrl.
   END.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL DIALOG-1
ON VALUE-CHANGED OF TOG_VAL IN FRAME DIALOG-1 /* Satsinnehållet till beredningen */
DO:
   TOG_VAL = INPUT TOG_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS
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
   IF satstrue = TRUE THEN DO:      
      FIND satstemp WHERE ROWID(satstemp) = mtrl_rowid NO-LOCK NO-ERROR.      
   END.
   ELSE IF satsmtrl = TRUE THEN DO:
      FIND mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-LOCK NO-ERROR.
      FIND FIRST satstemp WHERE satstemp.KOD = mtrltemp.ENR AND satstemp.LEVKOD = mtrltemp.LEVKOD
      AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.      
      /*IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         IF mtrltemp.LEVKOD = "11" THEN DO:
            FIND FIRST satstemp WHERE satstemp.KOD = mtrltemp.ENR AND satstemp.LEVKOD = "1"
            AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.      
         END.
         ELSE DO:
            FIND FIRST satstemp WHERE satstemp.KOD = mtrltemp.ENR AND satstemp.LEVKOD = mtrltemp.LEVKOD
            AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.      
         END.
      END.
      ELSE DO:
         FIND FIRST satstemp WHERE satstemp.KOD = mtrltemp.ENR AND satstemp.LEVKOD = mtrltemp.LEVKOD
         AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.      
      END.    */  
   END.
   ELSE DO:
      FIND list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
      FIND FIRST satstemp WHERE satstemp.KOD = list_mtrl.ENR AND satstemp.LEVKOD = list_mtrl.LEVKOD
      AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.
      /*IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:         
         IF list_mtrl.LEVKOD = "11" THEN DO:
            FIND FIRST satstemp WHERE satstemp.KOD = list_mtrl.ENR AND satstemp.LEVKOD = "1"
            AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST satstemp WHERE satstemp.KOD = list_mtrl.ENR AND satstemp.LEVKOD = list_mtrl.LEVKOD
            AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.
         END.
      END.
      ELSE DO:
         FIND FIRST satstemp WHERE satstemp.KOD = list_mtrl.ENR AND satstemp.LEVKOD = list_mtrl.LEVKOD
         AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.
      END.            */
   END.
   IF AVAILABLE satstemp THEN DO:   
      ASSIGN
      satstemp.ENR2:LABEL IN BROWSE BRW_SATS = Guru.Konstanter:genk
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-KOD = satstemp.KOD
      FILL-IN-BEN = satstemp.BENAMNING
      FILL-IN-ENR = satstemp.ENR
      FILL-IN-ENHET = satstemp.ENHET
      FILL-IN-PRIS = satstemp.PRIS
      levvar = satstemp.LEVKOD
      kodvar = satstemp.KOD.   
      OPEN QUERY BRW_SATS FOR EACH satstemp WHERE satstemp.KOD = kodvar AND
      satstemp.LEVKOD = levvar AND satstemp.SATS = FALSE USE-INDEX KOD NO-LOCK.    
   END.
   ELSE DO:
      MESSAGE "Information om satsen saknas"
      VIEW-AS ALERT-BOX TITLE "Meddelande".        
   END.   
   RUN enable_UI.       
   {FRMSIZED.I}
   IF satstrue = TRUE THEN TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   ELSE IF satsmtrl = TRUE THEN TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   ELSE DO:
      ENABLE TOG_VAL WITH FRAME {&FRAME-NAME}.
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.  
   END.
   {musarrow.i}   
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_SATS:HANDLE IN FRAME {&FRAME-NAME}).
   
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
  DISPLAY TOG_VAL FILL-IN-KOD FILL-IN-BEN FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  ENABLE BRW_SATS btn_ok FILL-IN-KOD FILL-IN-BEN FILL-IN-ENR FILL-IN-ENHET 
         FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

