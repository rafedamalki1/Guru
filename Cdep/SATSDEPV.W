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
DEFINE INPUT PARAMETER kodvar AS CHARACTER.
DEFINE INPUT PARAMETER levvar AS CHARACTER.
DEFINE INPUT-OUTPUT PARAMETER satsinn AS LOGICAL.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SMTRL.I}
{HOPPSEK2W.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}

DEFINE VARIABLE dxtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-OK AS LOGICAL NO-UNDO.


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
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Satsinnehållet till materiellistan" 
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
     TOG_VAL AT ROW 9.63 COL 1.5
     BRW_SATS AT ROW 10.75 COL 1.5
     btn_ok AT ROW 17.63 COL 40
     FILL-IN-KOD AT ROW 1.88 COL 10.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.21 COL 10.25 COLON-ALIGNED
     FILL-IN-ENR AT ROW 4.54 COL 10.25 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 5.88 COL 10.25 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 7.25 COL 10.25 COLON-ALIGNED
     SPACE(29.87) SKIP(10.57)
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
  IF VALID-HANDLE(dxtapph) THEN DELETE PROCEDURE dxtapph.
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
         FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = satstemp.ENR2 AND 
         spec_mtrl.LEVKOD = satstemp.LEVKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE spec_mtrl THEN DO:         
            CREATE spec_mtrl.
            ASSIGN           
            spec_mtrl.ENR = satstemp.ENR2
            spec_mtrl.BENAMNING = satstemp.BENAMNING2
            spec_mtrl.NPRIS = satstemp.PRIS2           
            spec_mtrl.BPRIS = satstemp.PRIS2           
            spec_mtrl.LEVKOD = satstemp.LEVKOD
            spec_mtrl.SALDO = satstemp.ANTAL               
            spec_mtrl.ENHET = satstemp.ENHET2
            spec_mtrl.MARK = TRUE.                  
         END.
         ELSE DO:
            spec_mtrl.SALDO = spec_mtrl.SALDO + satstemp.ANTAL.
         END.
      END.      
   END.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL DIALOG-1
ON VALUE-CHANGED OF TOG_VAL IN FRAME DIALOG-1 /* Satsinnehållet till materiellistan */
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
   EMPTY TEMP-TABLE satstemp NO-ERROR.          
   RUN satskolehmt_UI IN dxtapph (INPUT kodvar ,INPUT levvar, OUTPUT TABLE satstemp).             
   
   
   FIND FIRST satstemp WHERE satstemp.KOD = kodvar AND satstemp.LEVKOD = levvar
   AND satstemp.SATS = TRUE NO-LOCK NO-ERROR.   
   IF AVAILABLE satstemp THEN DO:   
      ASSIGN
      satstemp.ENR2:LABEL IN BROWSE BRW_SATS = Guru.Konstanter:genk
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-KOD = satstemp.KOD
      FILL-IN-BEN = satstemp.BENAMNING
      FILL-IN-ENR = satstemp.ENR
      FILL-IN-ENHET = satstemp.ENHET
      FILL-IN-PRIS = satstemp.PRIS.      
      kommandoquery = "satstemp.KOD = " + "'" + STRING(kodvar) + "'" + " AND satstemp.LEVKOD = " + "'" + STRING(levvar) + "'" + "  AND satstemp.SATS = FALSE".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).      
      RUN openbdynspec_UI IN brwproc[1].
      /*OPEN QUERY BRW_SATS FOR EACH satstemp WHERE satstemp.KOD = kodvar AND
      satstemp.LEVKOD = levvar AND satstemp.SATS = FALSE USE-INDEX KOD NO-LOCK.    */
   END.
   ELSE DO:
      MESSAGE "Information om satsen saknas"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      status-OK = SESSION:SET-WAIT-STATE("").
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      LEAVE MAIN-BLOCK.
   END.   
   RUN enable_UI.       
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   ENABLE TOG_VAL WITH FRAME {&FRAME-NAME}.
   TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.  
   {FRMSIZED.I}   
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
      
   IF Guru.Konstanter:appcon THEN DO:
      RUN DXTAPP.P PERSISTENT SET dxtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DXTAPP.P PERSISTENT SET dxtapph.
   END.   
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

