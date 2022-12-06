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

  Created: 95/05/10 -  1:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{ANMARKTEMP.I}
{HOPALLA.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE brow AS ROWID NO-UNDO.
DEFINE VARIABLE anmarkapph AS HANDLE NO-UNDO.
DEFINE VARIABLE vilkavar AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastanmtemp

/* Definitions for BROWSE BRW_ANM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANM fastanmtemp.ANVANDARE ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANM fastanmtemp.ANVANDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define QUERY-STRING-BRW_ANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANM OPEN QUERY BRW_ANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANM fastanmtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 EDITOR_ANM BTN_KONTO BRW_ANM BTN_NY ~
BTN_UPP BTN_BORT BTN_VISAO FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN ~
FILL-IN_ARBBESKED FILL-IN_UTFARDAT BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS EDITOR_ANM FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN FILL-IN_ARBBESKED FILL-IN_UTFARDAT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KONTO 
     IMAGE-UP FILE "BILDER\upp-u":U
     LABEL "UPP":L 
     SIZE 7 BY 2.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 12 BY 1.

DEFINE VARIABLE EDITOR_ANM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 71 BY 6.

DEFINE VARIABLE FILL-IN_ARBBESKED AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Arbetsbesked" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Anm." 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY .83.

DEFINE VARIABLE FILL-IN_UTFARDAT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdat av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANM FOR 
      fastanmtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANM DIALOG-1 _STRUCTURED
  QUERY BRW_ANM NO-LOCK DISPLAY
      fastanmtemp.ANVANDARE FORMAT "x(12)":U
      fastanmtemp.ANMARK COLUMN-LABEL "Anmärkning" FORMAT "X(50)":U
  ENABLE
      fastanmtemp.ANVANDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 11.17
         TITLE "Fasta anmärkningar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     EDITOR_ANM AT ROW 3.25 COL 1.5 NO-LABEL
     BTN_KONTO AT ROW 9.5 COL 33.75
     BRW_ANM AT ROW 11.71 COL 1.5
     BTN_NY AT ROW 23.08 COL 15.88
     BTN_UPP AT ROW 23.08 COL 28.75
     BTN_BORT AT ROW 23.08 COL 41.63
     BTN_VISAO AT ROW 23.08 COL 54.5
     FILL-IN_SPERSONALKOD AT ROW 24.83 COL 21.5 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 24.83 COL 42 COLON-ALIGNED
     FILL-IN_ARBBESKED AT ROW 26.63 COL 13 COLON-ALIGNED
     FILL-IN_UTFARDAT AT ROW 26.63 COL 32.5 COLON-ALIGNED
     BTN_OK AT ROW 26.67 COL 58.5
     "Anmärkning:" VIEW-AS TEXT
          SIZE 16.5 BY 1 AT ROW 2.08 COL 25.63
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 8.5 BY .83 AT ROW 24.88 COL 2.63
     RECT-21 AT ROW 24.63 COL 1.5
     SPACE(0.99) SKIP(1.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Anmärkning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ANM BTN_KONTO DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ANVANDARE
"ANVANDARE" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.fastanmtemp.ANMARK
"ANMARK" "Anmärkning" "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR,ENDKEY OF FRAME DIALOG-1 /* Anmärkning */
DO:
   {muswait.i}
   FRAME DIALOG-1:HIDDEN = TRUE.   
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTO DIALOG-1
ON CHOOSE OF BTN_KONTO IN FRAME DIALOG-1 /* UPP */
DO:
   RUN anm_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   RUN ny_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   {muswait.i}    
   DEFINE VARIABLE numrader AS INTEGER NO-UNDO.
   EDITOR_ANM = INPUT EDITOR_ANM. 
   RUN btnok IN anmarkapph (INPUT aonrrec,INPUT EDITOR_ANM).
   FRAME DIALOG-1:HIDDEN = TRUE.
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISAO DIALOG-1
ON CHOOSE OF BTN_VISAO IN FRAME DIALOG-1 /* Visa */
DO:
   RUN visao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_ANM DIALOG-1
ON LEAVE OF EDITOR_ANM IN FRAME DIALOG-1
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ARBBESKED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBBESKED DIALOG-1
ON LEAVE OF FILL-IN_ARBBESKED IN FRAME DIALOG-1 /* Arbetsbesked */
DO:
   ASSIGN uppaonrtemp.ARBBESKED = INPUT FILL-IN_ARBBESKED.
   IF uppaonrtemp.ARBBESKED = TRUE THEN FILL-IN_UTFARDAT:HIDDEN = FALSE.
   IF uppaonrtemp.ARBBESKED = FALSE THEN FILL-IN_UTFARDAT:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBBESKED DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_ARBBESKED IN FRAME DIALOG-1 /* Arbetsbesked */
DO:
   IF INPUT FILL-IN_ARBBESKED = FALSE THEN ASSIGN uppaonrtemp.ARBBESKED = TRUE.
   IF INPUT FILL-IN_ARBBESKED = TRUE THEN ASSIGN uppaonrtemp.ARBBESKED = FALSE.
   IF uppaonrtemp.ARBBESKED = TRUE THEN FILL-IN_UTFARDAT:HIDDEN = FALSE.
   IF uppaonrtemp.ARBBESKED = FALSE THEN FILL-IN_UTFARDAT:HIDDEN = TRUE.
   DISPLAY FILL-IN_ARBBESKED  WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Anm. */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Anm. */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SFORNAMN + '*'.   
   FIND fastanmtemp WHERE ROWID(fastanmtemp) = brow NO-LOCK NO-ERROR.
   FIND NEXT fastanmtemp WHERE fastanmtemp.PROGRAM = "AONR" AND fastanmtemp.ANMARK MATCHES posok       
   NO-LOCK NO-ERROR.        
   IF NOT AVAILABLE fastanmtemp THEN DO:     
      FIND FIRST fastanmtemp WHERE fastanmtemp.PROGRAM = "AONR" AND fastanmtemp.ANMARK MATCHES posok       
      NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE fastanmtemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE fastanmtemp THEN DO:      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].            
      
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Användare */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Användare */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   brow = ROWID(fastanmtemp).
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SPERSONALKOD + '*'.   
   FIND fastanmtemp WHERE ROWID(fastanmtemp) = brow NO-LOCK NO-ERROR.
   FIND NEXT fastanmtemp WHERE fastanmtemp.PROGRAM = "AONR" AND
   fastanmtemp.ANVANDARE MATCHES posok       
   NO-LOCK NO-ERROR.
     
   IF NOT AVAILABLE fastanmtemp THEN DO:     
      FIND FIRST fastanmtemp WHERE fastanmtemp.PROGRAM = "AONR" AND 
      fastanmtemp.ANVANDARE MATCHES posok       
      NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE fastanmtemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE fastanmtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].            
      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_UTFARDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_UTFARDAT DIALOG-1
ON LEAVE OF FILL-IN_UTFARDAT IN FRAME DIALOG-1 /* Utfärdat av */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANM
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
   {musarrow.i}
   {ALLSTARTDYN.I} 

   FIND FIRST uppaonrtemp NO-LOCK NO-ERROR.
   ASSIGN EDITOR_ANM = uppaonrtemp.ANM[1].
   RUN hmtfastanm IN anmarkapph (INPUT "AONR",OUTPUT TABLE fastanmtemp).
   RUN openbdyn_UI IN brwproc[1] (INPUT vilkavar).     
   FIND FIRST fastanmtemp NO-LOCK NO-ERROR.
   IF AVAILABLE fastanmtemp THEN ASSIGN brow = fastanmtemp.FASTANVROW.
   RUN enable_UI.       
   {FRMSIZED.I}
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   IF NOT AVAILABLE fastanmtemp THEN DO:
      ASSIGN        
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.     
   ELSE DO:
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      IF AVAILABLE fastanmtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
         RUN lastselectdyn_UI IN brwproc[1].            
      END.
   END.
   IF FILL-IN_ARBBESKED = FALSE THEN FILL-IN_UTFARDAT:HIDDEN = TRUE.
   FILL-IN_ARBBESKED = FALSE.
   FILL-IN_UTFARDAT = "".
   FILL-IN_ARBBESKED:HIDDEN = TRUE.
   FILL-IN_UTFARDAT:HIDDEN = TRUE.
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
   ASSIGN
   vilkavar = ' WHERE fastanmtemp.PROGRAM = "AONR" '.   
   fastanmtemp.ANVANDARE:READ-ONLY IN BROWSE BRW_ANM = TRUE.
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANM:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv THEN DO:
      {muswait.i}
      brow = fastanmtemp.FASTANVROW.
      RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT fastanmtemp.FASTANVROW).
      RUN hmtfastanm IN anmarkapph (INPUT "AONR",OUTPUT TABLE fastanmtemp).
      RUN openbdyn_UI IN brwproc[1] (INPUT vilkavar).     
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].            
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ändra andras sparade anmärkningar!"
      VIEW-AS ALERT-BOX.            
   END.   
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anm_UI DIALOG-1 
PROCEDURE anm_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EDITOR_ANM = INPUT FRAME {&FRAME-NAME} EDITOR_ANM.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF EDITOR_ANM = "" THEN EDITOR_ANM = fastanmtemp.ANMARK.
   ELSE EDITOR_ANM = EDITOR_ANM + CHR(10) + fastanmtemp.ANMARK.
   DISPLAY EDITOR_ANM WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv OR Guru.Konstanter:hoppsekvar[9] = TRUE THEN DO:
      /*Har man tillgång till Register är man administratör och får ta bort andras anmärkningar*/
      MESSAGE "Vill du verkligen ta bort denna fasta anmärkning?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE fastanmtemp.ANMARK
      UPDATE answer AS LOGICAL.
      IF answer THEN DO TRANSACTION:
         {muswait.i}
         RUN bortanm IN anmarkapph (INPUT fastanmtemp.FASTANVROW).    
         DELETE fastanmtemp.      
         RUN selnextprevrow_UI IN brwproc[1].
         RUN refreshbrw_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].              
         IF NOT AVAILABLE fastanmtemp THEN DO:
            ASSIGN        
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
         
      END.
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ta bort andras sparade anmärkningar!"
      VIEW-AS ALERT-BOX.      
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
  DISPLAY EDITOR_ANM FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_ARBBESKED 
          FILL-IN_UTFARDAT 
      WITH FRAME DIALOG-1.
  ENABLE RECT-21 EDITOR_ANM BTN_KONTO BRW_ANM BTN_NY BTN_UPP BTN_BORT BTN_VISAO 
         FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_ARBBESKED 
         FILL-IN_UTFARDAT BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}  
   RUN nyanm IN anmarkapph (INPUT "",INPUT Guru.Konstanter:globanv,
                               INPUT Guru.Konstanter:globomr,INPUT "AONR",OUTPUT brow,
                               OUTPUT TABLE fastanmtemp APPEND).
   FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR. 
   
   RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT fastanmtemp.FASTANVROW).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      ASSIGN        
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN hmtfastanm IN anmarkapph (INPUT "AONR",OUTPUT TABLE fastanmtemp).
      RUN openbdyn_UI IN brwproc[1] (INPUT vilkavar).     
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].            
   END.    
   ELSE DO TRANSACTION:
      RUN bortanm IN anmarkapph (INPUT fastanmtemp.FASTANVROW). 
      DELETE fastanmtemp.       
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visao_UI DIALOG-1 
PROCEDURE visao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.    
   {muswait.i}
   RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT ?).   
   {musarrow.i}       
   musz = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

