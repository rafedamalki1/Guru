&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE avdnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE foreproch AS HANDLE NO-UNDO. /*Handle till FOREAPP.P*/
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE avdtempnr AS INTEGER NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{STRTEMP.I}

{AVDELNINGTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AVD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avdelningtemp

/* Definitions for BROWSE BRW_AVD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD avdelningtemp.AVDELNINGNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define QUERY-STRING-BRW_AVD FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AVD OPEN QUERY BRW_AVD FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD avdelningtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AVD BTN_NY BTN_AND BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVD FOR 
      avdelningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD C-Win _STRUCTURED
  QUERY BRW_AVD NO-LOCK DISPLAY
      avdelningtemp.AVDELNINGNR COLUMN-LABEL "Nr" FORMAT ">>>>9":U
      avdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Namn" FORMAT "x(35)":U
  ENABLE
      avdelningtemp.AVDELNINGNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 43.75 BY 11.58
         TITLE "Företag".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_AVD AT ROW 1.58 COL 1.5
     BTN_NY AT ROW 13.46 COL 2.38
     BTN_AND AT ROW 13.46 COL 17.38
     BTN_BORT AT ROW 13.46 COL 32.38
     BTN_AVB AT ROW 13.46 COL 47.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.38 BY 17.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration företag"
         HEIGHT             = 13.88
         WIDTH              = 61.13
         MAX-HEIGHT         = 22.96
         MAX-WIDTH          = 81.38
         VIRTUAL-HEIGHT     = 22.96
         VIRTUAL-WIDTH      = 81.38
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_AVD 1 DEFAULT-FRAME */
ASSIGN 
       BRW_AVD:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD
/* Query rebuild information for BROWSE BRW_AVD
     _TblList          = "Temp-Tables.avdelningtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.avdelningtemp.AVDELNINGNR
"avdelningtemp.AVDELNINGNR" "Nr" ">>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.avdelningtemp.AVDELNINGNAMN
"avdelningtemp.AVDELNINGNAMN" "Namn" "x(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVD */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administration företag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Administration företag */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVD
&Scoped-define SELF-NAME BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVD C-Win
ON VALUE-CHANGED OF BRW_AVD IN FRAME DEFAULT-FRAME /* Företag */
DO:
   avdnr = avdelningtemp.AVDELNINGNR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   {muswait.i}          
   andra = TRUE.
   
   APPLY "VALUE-CHANGED" TO BRW_AVD IN FRAME DEFAULT-FRAME.  
   RUN NYSFORE.W.
   
   andra = FALSE.
   IF musz = FALSE THEN DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = avdnr NO-LOCK NO-ERROR.
      IF AVAILABLE avdelningtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avdelningtemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.      
   END.
   ELSE DO:
      musz = FALSE.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   FIND FIRST avdelningtemp NO-LOCK NO-ERROR.
   IF AVAILABLE avdelningtemp THEN APPLY "CLOSE":U TO THIS-PROCEDURE.
   ELSE DO: 
      MESSAGE "Ni måste lägga upp ett företag innan ni avslutar."
         VIEW-AS ALERT-BOX TITLE "Meddelande!".
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   {muswait.i}
   IF AVAILABLE avdelningtemp THEN DO:
      ASSIGN
      avdtempnr = avdelningtemp.AVDELNINGNR
      counter = 0.
      FOR EACH avdelningtemp.
         counter = counter + 1.
      END.
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = avdtempnr NO-LOCK NO-ERROR.
      IF counter = 1 THEN DO:
         MESSAGE "Det måste finnas minst ett företag upplagt!" SKIP
            "Skapa ett nytt företag före ni tar bort detta" 
            VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      ELSE DO:
         MESSAGE "Vill du ta bort " + avdelningtemp.AVDELNINGNAMN + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            avdnr = avdelningtemp.AVDELNINGNR.
            RUN storforebtnbort_UI IN foreproch
               (INPUT avdnr).
            FIND avdelningtemp WHERE avdelningtemp.AVDELNINGNR = avdnr EXCLUSIVE-LOCK NO-ERROR.
            OPEN QUERY oq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = avdelningtemp.AVDELNINGNR NO-LOCK.
            GET FIRST oq EXCLUSIVE-LOCK.
            DO WHILE AVAILABLE(stordistemp):
               OPEN QUERY dq FOR EACH vstorntemp WHERE vstorntemp.DISTRIKTID = stordistemp.DISTRIKTID NO-LOCK.
               GET FIRST dq EXCLUSIVE-LOCK.
               DO WHILE AVAILABLE(vstorntemp):
                  OPEN QUERY bq FOR EACH bortkoppladtemp WHERE bortkoppladtemp.STORNUMMERID =
                  vstorntemp.STORNUMMERID NO-LOCK.
                  GET FIRST bq EXCLUSIVE-LOCK.
                  DO WHILE AVAILABLE(bortkoppladtemp):
                     DELETE bortkoppladtemp.
                     GET NEXT bq EXCLUSIVE-LOCK.
                  END.
                  CLOSE QUERY bq.
                  FIND FIRST urstorntemp WHERE urstorntemp.DISTRIKTID = vstorntemp.DISTRIKTID AND 
                     urstorntemp.STORNUMMERID = vstorntemp.STORNUMMERID EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE urstorntemp THEN DELETE urstorntemp.
                  DELETE vstorntemp.
                  GET NEXT dq EXCLUSIVE-LOCK.
               END.
               CLOSE QUERY dq.
               DELETE stordistemp.
               GET NEXT oq EXCLUSIVE-LOCK.
            END.
            DELETE avdelningtemp NO-ERROR.
            RUN selnextprevrow_UI IN brwproc[1].
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.    
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   {muswait.i}  
   musz = FALSE.
   
   RUN NYSFORE.W.
   
   IF musz = FALSE THEN DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = avdnr NO-LOCK.
      IF AVAILABLE avdelningtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avdelningtemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.   
   END.
   ELSE DO:
      musz = FALSE.
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(foreproch) THEN DELETE PROCEDURE foreproch.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   {WIN_M_START.I}
   {ALLSTARTDYN.I}    
   {muswait.i}
   andra = FALSE.
   RUN enable_UI.
   {FRMSIZE.I}
   RUN openbdynspec_UI IN brwproc[1].
   BRW_AVD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE avdelningtemp THEN avdnr = avdelningtemp.AVDELNINGNR.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   avdelningtemp.AVDELNINGNR:READ-ONLY IN BROWSE BRW_AVD = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_AVD:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FOREAPP.P PERSISTENT SET foreproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN FOREAPP.P PERSISTENT SET foreproch.   
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE BRW_AVD BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI C-Win 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.   
   &Scoped-define BROWSE-NAME BRW_AVD
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

