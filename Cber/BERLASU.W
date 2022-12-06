&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW                       
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE anamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE berlasapph AS HANDLE NO-UNDO.
DEFINE VARIABLE val AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE lasberpers
   FIELD BERNR AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD EXPBER AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BERANVROW AS ROWID
   INDEX BERNR BERNR OMRADE.

DEFINE TEMP-TABLE elasberpers NO-UNDO LIKE lasberpers.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_BER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lasberpers

/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER lasberpers.BERNR lasberpers.OMRADE ~
lasberpers.NAMN lasberpers.BENAMNING lasberpers.EXPBER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER 
&Scoped-define QUERY-STRING-BRW_BER FOR EACH lasberpers NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH lasberpers NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_BER lasberpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER lasberpers


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BER BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Lås upp" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BER FOR 
      lasberpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER Dialog-Frame _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      lasberpers.BERNR COLUMN-LABEL "Beredning" FORMAT "x(8)":U
      lasberpers.OMRADE FORMAT "x(256)":U WIDTH 8
      lasberpers.NAMN COLUMN-LABEL "Låses av" FORMAT "x(256)":U
            WIDTH 30
      lasberpers.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 50
      lasberpers.EXPBER COLUMN-LABEL "Låst pga" FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 80.75 BY 15.04
         TITLE "Beredningar som används eller är låsta".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_BER AT ROW 1.83 COL 3.75
     BTN_BORT AT ROW 2 COL 86
     BTN_AVB AT ROW 16.04 COL 86
     SPACE(1.24) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Beredningar som används eller är låsta".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: lasberpers T "?" NO-UNDO temp-db lasberpers
      TABLE: ? T "?" NO-UNDO temp-db vispers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_BER 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.lasberpers"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.lasberpers.BERNR
"BERNR" "Beredning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.lasberpers.OMRADE
"OMRADE" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.lasberpers.NAMN
"NAMN" "Låses av" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.lasberpers.BENAMNING
"BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.lasberpers.EXPBER
"EXPBER" "Låst pga" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Beredningar som används eller är låsta */
DO:
   {BORTBRWPROC.I}
  IF VALID-HANDLE(berlasapph) THEN DELETE PROCEDURE berlasapph.
  APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Beredningar som används eller är låsta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Beredningar som används eller är låsta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BER
&Scoped-define SELF-NAME BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BER Dialog-Frame
ON VALUE-CHANGED OF BRW_BER IN FRAME Dialog-Frame /* Beredningar som används eller är låsta */
DO:
   status-ok = BRW_BER:SELECT-FOCUSED-ROW() NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avsluta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Lås upp */
DO:
   IF lasberpers.EXPBER = ""  OR lasberpers.EXPBER = "Beredning" THEN DO:   
      MESSAGE "Vill du ta bort låsningen av beredning nr: " + " " +
      STRING(lasberpers.BERNR) " " STRING(lasberpers.OMRADE) + " som enlig uppgift används av " + lasberpers.NAMN + "? OBS! Du måste vara helt säker på att beredningen inte används." 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "OBS EXPORTERADE BEREDNING!! Vill du ta bort låsningen av beredning nr: " + " " +
      STRING(lasberpers.BERNR) " " STRING(lasberpers.OMRADE) + " som enlig uppgift är exporterad av " + lasberpers.NAMN + "?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.
   END.
   IF svar THEN DO:  
      EMPTY TEMP-TABLE elasberpers NO-ERROR. 
      CREATE elasberpers.
      BUFFER-COPY lasberpers TO elasberpers.
      val = FALSE.
      RUN bort_UI IN berlasapph (INPUT TABLE elasberpers,OUTPUT val).
      IF val = FALSE THEN DO:
         DELETE lasberpers.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN refreshbrw_UI IN brwproc[1].
         GET FIRST BRW_BER NO-LOCK.
         IF NOT AVAILABLE lasberpers THEN DO:
            MESSAGE "Det finns inga fler låsta beredningar." VIEW-AS ALERT-BOX.
            APPLY "GO" TO BTN_AVB.
         END.
         RUN lastselectdyn_UI IN brwproc[1].   
      END.
      ELSE DO:
         MESSAGE "Det gick inte att låsa upp beredningen, kontakta Elpool på tfn:090-184540." VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.                
   END.
   ELSE DO:
      musz = musz.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ  ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   RUN laddaberlas_UI IN berlasapph (OUTPUT anamn,OUTPUT TABLE lasberpers).   
   RUN enable_UI.       
   {FRMSIZED.I}   
   RUN setcolsortvar_UI IN brwproc[1] ("").
   RUN openbdynspec_UI IN brwproc[1].  
   FIND FIRST lasberpers WHERE lasberpers.BERNR = STRING(valbernr) AND 
   lasberpers.OMRADE = valomrade NO-ERROR.
   IF AVAILABLE lasberpers THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(lasberpers)).
      RUN lastselectdyn_UI IN brwproc[1].     
   END.
   ELSE DO:
      FIND FIRST lasberpers NO-LOCK NO-ERROR.
      IF AVAILABLE lasberpers THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(lasberpers)).
         RUN lastselectdyn_UI IN brwproc[1].     
      END.
      ELSE DO:
         MESSAGE "Det finns inga låsta beredningar." VIEW-AS ALERT-BOX.
         musz = TRUE.   
      END.     
   END.
   RUN title_UI IN brwproc[1].
   {musarrow.i}
   {DIA_M_SLUT.I}
   IF musz = TRUE THEN DO:
      musz = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERLASAPP.P PERSISTENT SET berlasapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERLASAPP.P PERSISTENT SET berlasapph.
   END. 
  
   RUN globanvin_UI IN berlasapph (INPUT Guru.Konstanter:globanv).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE BRW_BER BTN_BORT BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

