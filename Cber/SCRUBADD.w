&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */




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
/*          This .W file was created with the Progress AppBuilder.      */
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
{SCADMIN.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{PTEMP.I}
{LOPTEMP.I}
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER antalcol AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER grundkloid AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER grundkloidnamn AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR hdrubtemp.
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_FOR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tempforlagg temphandelse loptemp ptemp ~
hdrubtemp tempytbelagg

/* Definitions for BROWSE BRW_FOR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FOR tempforlagg.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FOR 
&Scoped-define QUERY-STRING-BRW_FOR FOR EACH tempforlagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FOR OPEN QUERY BRW_FOR FOR EACH tempforlagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FOR tempforlagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FOR tempforlagg


/* Definitions for BROWSE BRW_HAND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HAND temphandelse.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HAND 
&Scoped-define QUERY-STRING-BRW_HAND FOR EACH temphandelse NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HAND OPEN QUERY BRW_HAND FOR EACH temphandelse NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HAND temphandelse
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HAND temphandelse


/* Definitions for BROWSE BRW_LOP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP loptemp.LOPNR loptemp.ENHET ~
loptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP 
&Scoped-define QUERY-STRING-BRW_LOP FOR EACH loptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LOP OPEN QUERY BRW_LOP FOR EACH loptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LOP loptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP loptemp


/* Definitions for BROWSE BRW_P                                         */
&Scoped-define FIELDS-IN-QUERY-BRW_P ptemp.ARBKOD ptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P 
&Scoped-define QUERY-STRING-BRW_P FOR EACH ptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P OPEN QUERY BRW_P FOR EACH ptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P ptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P ptemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL hdrubtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH hdrubtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH hdrubtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VAL hdrubtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL hdrubtemp


/* Definitions for BROWSE BRW_YT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_YT tempytbelagg.YTBELAGG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_YT 
&Scoped-define QUERY-STRING-BRW_YT FOR EACH tempytbelagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_YT OPEN QUERY BRW_YT FOR EACH tempytbelagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_YT tempytbelagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_YT tempytbelagg


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FOR BRW_VAL BRW_P BRW_HAND BRW_LOP ~
BRW_YT BTN_ADD BTN_OK BTN_AVBRYT BTN_BORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ADD 
     IMAGE-UP FILE "BILDER/next-u.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Lägg till" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     IMAGE-UP FILE "BILDER/prev-u.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Ta Bort" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FOR FOR 
      tempforlagg SCROLLING.

DEFINE QUERY BRW_HAND FOR 
      temphandelse SCROLLING.

DEFINE QUERY BRW_LOP FOR 
      loptemp SCROLLING.

DEFINE QUERY BRW_P FOR 
      ptemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      hdrubtemp SCROLLING.

DEFINE QUERY BRW_YT FOR 
      tempytbelagg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FOR C-Win _STRUCTURED
  QUERY BRW_FOR NO-LOCK DISPLAY
      tempforlagg.BENAMNING COLUMN-LABEL "Förl.sätt" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 52 BY 9.

DEFINE BROWSE BRW_HAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HAND C-Win _STRUCTURED
  QUERY BRW_HAND NO-LOCK DISPLAY
      temphandelse.BENAMNING COLUMN-LABEL "Förl.Händelse" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 52 BY 9.

DEFINE BROWSE BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP C-Win _STRUCTURED
  QUERY BRW_LOP NO-LOCK DISPLAY
      loptemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U COLUMN-FGCOLOR 12
      loptemp.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 56 BY 17.42
         TITLE "Löpnr kopplade till arbetskoden" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P C-Win _STRUCTURED
  QUERY BRW_P NO-LOCK DISPLAY
      ptemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U COLUMN-FGCOLOR 12
      ptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 56 BY 8.38
         TITLE "Arbetskoder".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      hdrubtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 52 BY 20.25.

DEFINE BROWSE BRW_YT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_YT C-Win _STRUCTURED
  QUERY BRW_YT NO-LOCK DISPLAY
      tempytbelagg.YTBELAGG COLUMN-LABEL "Ytbeläggning" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 52 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_FOR AT ROW 1.5 COL 5
     BRW_VAL AT ROW 1.5 COL 72.38
     BRW_P AT ROW 1.63 COL 5 WIDGET-ID 200
     BRW_HAND AT ROW 10.79 COL 5
     BRW_LOP AT ROW 11.58 COL 5 WIDGET-ID 100
     BRW_YT AT ROW 20.08 COL 5
     BTN_ADD AT ROW 11.46 COL 62.75
     BTN_OK AT ROW 27.58 COL 93.38
     BTN_AVBRYT AT ROW 27.58 COL 109.88
     BTN_BORT AT ROW 16.46 COL 62.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: hdrubtemp T "?" NO-UNDO temp-db hdrubtemp
      TABLE: loptemp T "?" NO-UNDO temp-db loptemp
      TABLE: ptemp T "?" NO-UNDO temp-db ptemp
      TABLE: tempforlagg T "?" NO-UNDO temp-db tempforlagg
      TABLE: temphandelse T "?" NO-UNDO temp-db temphandelse
      TABLE: tempytbelagg T "?" NO-UNDO temp-db tempytbelagg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Schaktrubriker"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FOR 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_VAL BRW_FOR DEFAULT-FRAME */
/* BROWSE-TAB BRW_P BRW_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_HAND BRW_P DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP BRW_HAND DEFAULT-FRAME */
/* BROWSE-TAB BRW_YT BRW_LOP DEFAULT-FRAME */
ASSIGN 
       BRW_LOP:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_LOP:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_P:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_P:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FOR
/* Query rebuild information for BROWSE BRW_FOR
     _TblList          = "Temp-Tables.tempforlagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempforlagg.BENAMNING
"tempforlagg.BENAMNING" "Förl.sätt" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FOR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HAND
/* Query rebuild information for BROWSE BRW_HAND
     _TblList          = "Temp-Tables.temphandelse"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.temphandelse.BENAMNING
"temphandelse.BENAMNING" "Förl.Händelse" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HAND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP
/* Query rebuild information for BROWSE BRW_LOP
     _TblList          = "Temp-Tables.loptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.loptemp.LOPNR
"loptemp.LOPNR" "Löpnr" ? "integer" ? 12 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.loptemp.ENHET
"loptemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.loptemp.BENAMNING
"loptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P
/* Query rebuild information for BROWSE BRW_P
     _TblList          = "Temp-Tables.ptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ptemp.ARBKOD
"ptemp.ARBKOD" "Kod" ? "character" ? 12 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ptemp.BENAMNING
"ptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.hdrubtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.hdrubtemp.BENAMNING
"hdrubtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_YT
/* Query rebuild information for BROWSE BRW_YT
     _TblList          = "Temp-Tables.tempytbelagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempytbelagg.YTBELAGG
"tempytbelagg.YTBELAGG" "Ytbeläggning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_YT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Schaktrubriker */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Schaktrubriker */
DO:
   EMPTY TEMP-TABLE hdrubtemp.
  /* This event will close the window and terminate the procedure.  */
   {BORTBRWPROC.I}             
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.   
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P
&Scoped-define SELF-NAME BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P C-Win
ON VALUE-CHANGED OF BRW_P IN FRAME DEFAULT-FRAME /* Arbetskoder */
DO:
  RUN changearb_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ADD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ADD C-Win
ON CHOOSE OF BTN_ADD IN FRAME DEFAULT-FRAME /* Lägg till */
DO:  
  RUN addrub_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT C-Win
ON CHOOSE OF BTN_AVBRYT IN FRAME DEFAULT-FRAME /* Avbryt */
DO:   
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      APPLY "CHOOSE" TO BTN_OK.         
  END.
  ELSE IF NOT svar THEN DO:
      EMPTY TEMP-TABLE hdrubtemp.                  
  END.                    
  ELSE DO:
     RETURN NO-APPLY.
  END. 

  APPLY "CLOSE":U TO THIS-PROCEDURE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta Bort */
DO:
  RUN remrub_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FOR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
      {WIN_M_START.I}
      {muswait.i} 
      {ALLSTARTDYN.I}
      {FRMSIZE.I}
      IF Guru.GlobalaVariabler:KalkyUppIschakt = TRUE THEN DO:         
         IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
         RUN Ptempklog_UI IN KalkClasserStart    (INPUT 2,INPUT grundkloid, OUTPUT TABLE ptemp).
         RUN Loptempnklog_UI IN KalkClasserStart (INPUT 2, INPUT grundkloid, OUTPUT TABLE loptemp).
      END.    
      RUN ladda_UI.      
      RUN enable_UI.
      IF Guru.GlobalaVariabler:KalkyUppIschakt = TRUE THEN DO:
         RUN setorgtitle_UI IN brwproc[5] (INPUT "Arbetskoder i katalog : " + grundkloidnamn).
         ASSIGN
         BRW_FOR:HIDDEN = TRUE 
         BRW_YT:HIDDEN = TRUE
         BRW_HAND:HIDDEN = TRUE 
         BRW_P:HIDDEN = FALSE
         BRW_LOP:HIDDEN = FALSE.  
      END.   
      ELSE DO:
         ASSIGN
         BRW_FOR:HIDDEN = FALSE 
         BRW_YT:HIDDEN = FALSE
         BRW_HAND:HIDDEN = FALSE 
         BRW_P:HIDDEN = TRUE
         BRW_LOP:HIDDEN = TRUE.
      END.   
      {musarrow.i}
      {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addrub_UI C-Win 
PROCEDURE addrub_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE antal_lopnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_valdafor AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_valdahand AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_valdayt AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE hjben AS CHARACTER NO-UNDO.
   ASSIGN
   antal_lopnr = BRW_LOP:NUM-SELECTED-ROWS IN FRAME DEFAULT-FRAME
   antal_valdafor = BRW_FOR:NUM-SELECTED-ROWS IN FRAME DEFAULT-FRAME   
   antal_valdahand = BRW_HAND:NUM-SELECTED-ROWS IN FRAME DEFAULT-FRAME
   antal_valdayt = BRW_YT:NUM-SELECTED-ROWS IN FRAME DEFAULT-FRAME. 
   RUN getnumresult_UI IN brwproc[4](OUTPUT antal_raknare).
  
   IF antal_lopnr + antalcol + antal_raknare + antal_valdafor + antal_valdahand + antal_valdayt > 42 THEN DO:
      MESSAGE "Du kan max ha 40 rubriker!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END. 
   antal_raknare = 1.  
   DO WHILE antal_raknare LE antal_lopnr :
      BRW_LOP:FETCH-SELECTED-ROW(antal_raknare) IN FRAME DEFAULT-FRAME.      
      IF loptemp.ARBKOD = "137" THEN DO:
         FIND FIRST hdrubtemp WHERE hdrubtemp.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") + " " NO-ERROR.      
         IF NOT AVAILABLE hdrubtemp THEN DO:
            FIND FIRST tempforlagg  WHERE tempforlagg.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") + " " NO-LOCK NO-ERROR.
            IF AVAILABLE tempforlagg THEN DO:
               CREATE hdrubtemp.
               ASSIGN
               hdrubtemp.ID = tempforlagg.ID
               hdrubtemp.TYP = "f"
               hdrubtemp.BENAMNING = tempforlagg.BENAMNING.
            END.
            ELSE DO:
               FIND FIRST temphandelse  WHERE temphandelse.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") + " " NO-LOCK NO-ERROR.
               IF AVAILABLE temphandelse THEN DO:
                  CREATE hdrubtemp.
                  ASSIGN
                  hdrubtemp.ID = temphandelse.ID                  
                  hdrubtemp.TYP = temphandelse.TYP
                  hdrubtemp.BENAMNING = temphandelse.BENAMNING.
               END.
               ELSE DO:
                  FIND FIRST tempytbelagg  WHERE tempytbelagg.YTBELAGG BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") + " " NO-LOCK NO-ERROR.
                  IF AVAILABLE tempytbelagg THEN DO:
                     CREATE hdrubtemp.
                     ASSIGN
                     hdrubtemp.ID = tempytbelagg.ID
                     hdrubtemp.TYP = "y"
                     hdrubtemp.BENAMNING = tempytbelagg.YTBELAGG.  
                  END.
               END.   
            END.            
         END.
      END.
      ELSE IF loptemp.ARBKOD = "137G" THEN DO:
         hjben = SUBSTRING(loptemp.ARBKOD,1,3) + STRING(loptemp.LOPNR,"99") + "G". 
         FIND FIRST hdrubtemp WHERE hdrubtemp.BENAMNING BEGINS  hjben  NO-ERROR.      
         IF NOT AVAILABLE hdrubtemp THEN DO:
            FIND FIRST tempforlagg  WHERE tempforlagg.BENAMNING BEGINS hjben NO-LOCK NO-ERROR.
            IF AVAILABLE tempforlagg THEN DO:
               CREATE hdrubtemp.
               ASSIGN
               hdrubtemp.ID = tempforlagg.ID
               hdrubtemp.TYP = "f"
               hdrubtemp.BENAMNING = tempforlagg.BENAMNING.
            END.
            ELSE DO:
               FIND FIRST temphandelse  WHERE temphandelse.BENAMNING BEGINS hjben NO-LOCK NO-ERROR.
               IF AVAILABLE temphandelse THEN DO:
                  CREATE hdrubtemp.
                  ASSIGN
                  hdrubtemp.ID = temphandelse.ID                  
                  hdrubtemp.TYP = temphandelse.TYP
                  hdrubtemp.BENAMNING = temphandelse.BENAMNING.
               END.
               ELSE DO:
                  FIND FIRST tempytbelagg  WHERE tempytbelagg.YTBELAGG BEGINS hjben NO-LOCK NO-ERROR.
                  IF AVAILABLE tempytbelagg THEN DO:
                     CREATE hdrubtemp.
                     ASSIGN
                     hdrubtemp.ID = tempytbelagg.ID
                     hdrubtemp.TYP = "y"
                     hdrubtemp.BENAMNING = tempytbelagg.YTBELAGG.  
                  END.
               END.   
            END.            
         END.
      END.
      ELSE DO:
         IF loptemp.LOPNR > 99 THEN DO:
            FIND FIRST hdrubtemp WHERE hdrubtemp.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"999") NO-ERROR.      
            IF NOT AVAILABLE hdrubtemp THEN DO:
               FIND FIRST temphandelse  WHERE temphandelse.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"999") NO-LOCK NO-ERROR.
               IF AVAILABLE temphandelse THEN DO:
                  CREATE hdrubtemp.
                  ASSIGN
                  hdrubtemp.ID = temphandelse.ID                     
                  hdrubtemp.TYP = temphandelse.TYP
                  hdrubtemp.BENAMNING = temphandelse.BENAMNING.
               END.                          
            END. 
         END.
         ELSE DO:        
            FIND FIRST hdrubtemp WHERE hdrubtemp.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") NO-ERROR.      
            IF NOT AVAILABLE hdrubtemp THEN DO:
               FIND FIRST tempforlagg  WHERE tempforlagg.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") NO-LOCK NO-ERROR.
               IF AVAILABLE tempforlagg THEN DO:
                  CREATE hdrubtemp.
                  ASSIGN
                  hdrubtemp.ID = tempforlagg.ID
                  hdrubtemp.TYP = "f"
                  hdrubtemp.BENAMNING = tempforlagg.BENAMNING.
               END.
               ELSE DO:
                  FIND FIRST temphandelse  WHERE temphandelse.BENAMNING BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") NO-LOCK NO-ERROR.
                  IF AVAILABLE temphandelse THEN DO:
                     CREATE hdrubtemp.
                     ASSIGN
                     hdrubtemp.ID = temphandelse.ID               
                     hdrubtemp.TYP = temphandelse.TYP
                     hdrubtemp.BENAMNING = temphandelse.BENAMNING.
                  END.
                  ELSE DO:
                     FIND FIRST tempytbelagg  WHERE tempytbelagg.YTBELAGG BEGINS loptemp.ARBKOD + STRING(loptemp.LOPNR,"99") NO-LOCK NO-ERROR.
                     IF AVAILABLE tempytbelagg THEN DO:
                        CREATE hdrubtemp.
                        ASSIGN
                        hdrubtemp.ID = tempytbelagg.ID
                        hdrubtemp.TYP = "y"
                        hdrubtemp.BENAMNING = tempytbelagg.YTBELAGG.  
                     END.
                  END.   
               END.            
            END.
         END.   
      END.   
      antal_raknare = antal_raknare + 1.
   END.   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valdafor :
      BRW_FOR:FETCH-SELECTED-ROW(antal_raknare) IN FRAME DEFAULT-FRAME.      
      FIND FIRST hdrubtemp WHERE hdrubtemp.ID = tempforlagg.ID AND hdrubtemp.TYP = "f" NO-ERROR.      
      IF NOT AVAILABLE hdrubtemp THEN DO:
         CREATE hdrubtemp.
         ASSIGN
         hdrubtemp.ID = tempforlagg.ID
         hdrubtemp.TYP = "f"
         hdrubtemp.BENAMNING = tempforlagg.BENAMNING.      
      END.
      antal_raknare = antal_raknare + 1.
   END.   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valdahand :
      BRW_HAND:FETCH-SELECTED-ROW(antal_raknare) IN FRAME DEFAULT-FRAME.
      FIND FIRST hdrubtemp WHERE hdrubtemp.ID = temphandelse.ID 
      AND (hdrubtemp.TYP = "fh" OR hdrubtemp.TYP = "ph") NO-ERROR.      
      IF NOT AVAILABLE hdrubtemp THEN DO:      
         CREATE hdrubtemp.
         ASSIGN
         hdrubtemp.ID = temphandelse.ID    
         hdrubtemp.TYP = temphandelse.TYP
         hdrubtemp.BENAMNING = temphandelse.BENAMNING.
      END.
      antal_raknare = antal_raknare + 1.     
   END.   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valdayt :
      BRW_YT:FETCH-SELECTED-ROW(antal_raknare) IN FRAME DEFAULT-FRAME.            
      FIND FIRST hdrubtemp WHERE hdrubtemp.ID = tempytbelagg.ID AND hdrubtemp.TYP = "y" NO-ERROR.      
      IF NOT AVAILABLE hdrubtemp THEN DO:
         CREATE hdrubtemp.
         ASSIGN
         hdrubtemp.ID = tempytbelagg.ID
         hdrubtemp.TYP = "y"
         hdrubtemp.BENAMNING = tempytbelagg.YTBELAGG.                  
      END.
      antal_raknare = antal_raknare + 1.
   END.
   
   RUN openbdynspec_UI IN brwproc[4].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changearb_UI C-Win 
PROCEDURE changearb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   RUN selectrowid_UI IN brwproc[5] (OUTPUT brwrowid).
   FIND FIRST ptemp WHERE ROWID(ptemp) = brwrowid NO-LOCK NO-ERROR.
   IF AVAILABLE ptemp THEN DO:  
      RUN setcolsortvar_UI IN brwproc[6] (INPUT "TYP = " + STRING(2) + " AND ARBKOD = '" + ptemp.ARBKOD + "'").
      RUN openbdynspec_UI IN brwproc[6].
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
  ENABLE BRW_FOR BRW_VAL BRW_P BRW_HAND BRW_LOP BRW_YT BTN_ADD BTN_OK 
         BTN_AVBRYT BTN_BORT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
               (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.     
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_FOR:HANDLE IN FRAME DEFAULT-FRAME).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_YT:HANDLE IN FRAME DEFAULT-FRAME).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_HAND:HANDLE IN FRAME DEFAULT-FRAME).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_VAL:HANDLE IN FRAME DEFAULT-FRAME).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_P:HANDLE IN FRAME DEFAULT-FRAME).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_LOP:HANDLE IN FRAME DEFAULT-FRAME).               
   tthandle = TEMP-TABLE tempytbelagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "YTBELAGG", INPUT " WHERE YTBELAGG.BORT = FALSE").
   tthandle = TEMP-TABLE tempforlagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FORLAGG", INPUT " WHERE FORLAGG.BORT = FALSE").
   tthandle = TEMP-TABLE temphandelse:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDHANDELSE", INPUT " WHERE HDHANDELSE.BORT = FALSE").   
   FOR EACH temphandelse NO-LOCK:      
      temphandelse.TYP = temphandelse.SORTCHAR.
   END.
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[3] (INPUT "ORDNING").      
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[3].
   RUN openbdynspec_UI IN brwproc[5].
   RUN openbdynspec_UI IN brwproc[6].
   RUN changearb_UI.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remrub_UI C-Win 
PROCEDURE remrub_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.   
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME DEFAULT-FRAME.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      BRW_VAL:FETCH-SELECTED-ROW(antal_raknare) IN FRAME DEFAULT-FRAME.      
      DELETE hdrubtemp.
      antal_raknare = antal_raknare + 1.
   END.
   RUN openbdynspec_UI IN brwproc[4].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

