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
DEFINE NEW SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.

DEFINE NEW SHARED VARIABLE distproch AS HANDLE NO-UNDO. /*Handle till DISTAPP.P*/
DEFINE NEW SHARED VARIABLE distnr AS INTEGER NO-UNDO.

DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE natvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

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
&Scoped-define BROWSE-NAME BRW_P1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES stordistemp

/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT ~
stordistemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_P1 stordistemp
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 stordistemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_FOR CMB_ARTAL BRW_P1 BTN_KUND BTN_LEDN ~
BTN_STN BTN_STN2 BTN_KOPI BTN_NY BTN_AND BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_FOR CMB_ARTAL 

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

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera gr.data" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KUND 
     LABEL "Gr.data kunder" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LEDN 
     LABEL "Gr.data ledningar" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_STN 
     LABEL "Gr.data stationer" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_STN2 
     LABEL "Gr.data stn.app" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FOR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 28.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_P1 FOR 
      stordistemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 C-Win _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      stordistemp.VIDISTRIKT COLUMN-LABEL "Nr" FORMAT "X(3)":U
      stordistemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(30)":U WIDTH 40.5
  ENABLE
      stordistemp.VIDISTRIKT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 46.75 BY 11.58
         TITLE "Distrikt".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_FOR AT ROW 1.79 COL 1
     CMB_ARTAL AT ROW 3.13 COL 3
     BRW_P1 AT ROW 4.46 COL 1.5
     BTN_KUND AT ROW 6.04 COL 49.38
     BTN_LEDN AT ROW 7.13 COL 49.38
     BTN_STN AT ROW 8.25 COL 49.38
     BTN_STN2 AT ROW 9.33 COL 49.38
     BTN_KOPI AT ROW 10.46 COL 49.38
     BTN_NY AT ROW 16.29 COL 3.25
     BTN_AND AT ROW 16.29 COL 18.25
     BTN_BORT AT ROW 16.29 COL 33.25
     BTN_AVB AT ROW 16.29 COL 49.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.88 BY 16.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: stordistemp T "?" NO-UNDO temp-db stordistemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration distrikt"
         HEIGHT             = 16.71
         WIDTH              = 63.63
         MAX-HEIGHT         = 23.5
         MAX-WIDTH          = 101.25
         VIRTUAL-HEIGHT     = 23.5
         VIRTUAL-WIDTH      = 101.25
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
/* BROWSE-TAB BRW_P1 CMB_ARTAL DEFAULT-FRAME */
ASSIGN 
       BRW_P1:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_FOR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.stordistemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.stordistemp.VIDISTRIKT
"stordistemp.VIDISTRIKT" "Nr" "X(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.stordistemp.NAMN
"stordistemp.NAMN" "Namn" "X(30)" "character" ? ? ? ? ? ? no ? no no "40.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administration distrikt */
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
ON WINDOW-CLOSE OF C-Win /* Administration distrikt */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
&Scoped-define SELF-NAME BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P1 C-Win
ON VALUE-CHANGED OF BRW_P1 IN FRAME DEFAULT-FRAME /* Distrikt */
DO:
   IF AVAILABLE stordistemp THEN distnr = stordistemp.DISTRIKTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:      
      {muswait.i}                
      
      andra = TRUE.
      RUN NYDIST.W.
      
      andra = FALSE.
      IF musz = FALSE THEN DO:      
         RUN refreshbrw_UI IN brwproc[1].
      END.
      ELSE DO:
         musz = FALSE.
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns inget distrikt att ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   {muswait.i}
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN      
      distnr = stordistemp.DISTRIKTID.      
      MESSAGE "Vill du ta bort markerat distrikt? OBS! Alla störningar och grunddata som är kopplat till distriktet tas också bort."
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.
      IF svar THEN DO:
         RUN stordistbtnbort_UI IN distproch (INPUT distnr).
         FIND stordistemp WHERE stordistemp.DISTRIKTID = distnr EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE stordistemp THEN DO:
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
         END.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns inget distrikt att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPI C-Win
ON CHOOSE OF BTN_KOPI IN FRAME DEFAULT-FRAME /* Kopiera gr.data */
DO:
   {muswait.i}  
   {AVBGOM.I}
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN
      distnr = stordistemp.DISTRIKTID.      
      MESSAGE "Vill Ni kopiera grundata från något annat år? Om det finns befintliga grunddata kommer dessa att skrivas över."
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Kopiera?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         RUN KOPIGRD.W.
      END.
      ELSE DO:
         musz = musz.
      END.
   END.  
   ELSE DO:
      MESSAGE "Det finns inget distrikt att kopiera grunddata till."
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KUND C-Win
ON CHOOSE OF BTN_KUND IN FRAME DEFAULT-FRAME /* Gr.data kunder */
DO:
   {muswait.i}  
   {AVBGOM.I}
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN
      prec = RECID(stordistemp). 
      RUN GRDKUND.W.
   END.  
   ELSE DO:
      MESSAGE "Det finns inget distrikt att lägga upp grunddata för."
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEDN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEDN C-Win
ON CHOOSE OF BTN_LEDN IN FRAME DEFAULT-FRAME /* Gr.data ledningar */
DO:
   {muswait.i}  
   {AVBGOM.I}
/*    FIND FIRST stordistemp WHERE stordistemp.AVDELNINGNR = forvar */
/*    AND stordistemp.ARTAL = franar NO-LOCK NO-ERROR.              */
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN
      prec = RECID(stordistemp)
      natvar = 2. 
      RUN GRDLED.W.
   END.  
   ELSE DO:
      MESSAGE "Det finns inget distrikt att lägga upp grunddata för."
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
   END.
   {AVBFRAM.I}
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
   
   RUN NYDIST.W.
   
   IF musz = FALSE THEN DO:
      FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = distnr NO-LOCK NO-ERROR.
      IF AVAILABLE stordistemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(stordistemp)).
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


&Scoped-define SELF-NAME BTN_STN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STN C-Win
ON CHOOSE OF BTN_STN IN FRAME DEFAULT-FRAME /* Gr.data stationer */
DO:
   {muswait.i}  
   {AVBGOM.I}
/*    FIND FIRST stordistemp WHERE stordistemp.AVDELNINGNR = forvar */
/*    AND stordistemp.ARTAL = franar NO-LOCK NO-ERROR.              */
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN      
      prec = RECID(stordistemp)
      natvar = 3. 
      RUN GRDLED.W.
   END.  
   ELSE DO:
      MESSAGE "Det finns inget distrikt att lägga upp grunddata för."
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_STN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STN2 C-Win
ON CHOOSE OF BTN_STN2 IN FRAME DEFAULT-FRAME /* Gr.data stn.app */
DO:
   {muswait.i}  
   {AVBGOM.I}
/*    FIND FIRST stordistemp WHERE stordistemp.AVDELNINGNR = forvar */
/*    AND stordistemp.ARTAL = franar NO-LOCK NO-ERROR.              */
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN
      prec = RECID(stordistemp)
      natvar = 4. 
      RUN GRDLED.W.
   END.  
   ELSE DO:
      MESSAGE "Det finns inget distrikt att lägga upp grunddata för."
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.     
   /*IF franar = (YEAR(TODAY) + 1) THEN DO:*/
      FIND FIRST stordistemp WHERE stordistemp.ARTAL = franar AND 
      stordistemp.AVDELNINGNR = forvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE stordistemp THEN DO:
         FIND FIRST stordistemp WHERE stordistemp.ARTAL = (franar - 1) AND
         stordistemp.AVDELNINGNR = forvar NO-LOCK NO-ERROR.
         IF AVAILABLE stordistemp THEN DO:               
            MESSAGE "Vill du kopiera distrikt från föregående år?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Kopiera distrikt?" UPDATE svarartal AS LOGICAL.
            IF svarartal = TRUE THEN DO:            
               RUN kopidist_UI IN distproch
               (INPUT franar, INPUT forvar, INPUT-OUTPUT TABLE stordistemp).
               MESSAGE "Vill du kopiera grunddata från föregående år?" VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO TITLE "Kopiera grunddata?" UPDATE svardata AS LOGICAL.         
               IF svarartal = TRUE THEN DO:                                 
                  RUN kopidata_UI IN distproch
                  (INPUT franar, INPUT forvar).
               END.
            END.                        
         END.
      END.
   /*END.*/
   RUN changequery_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FOR C-Win
ON VALUE-CHANGED OF CMB_FOR IN FRAME DEFAULT-FRAME /* Företag */
DO:
   CMB_FOR = INPUT CMB_FOR.   
   FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_FOR NO-LOCK NO-ERROR.
   forvar = avdelningtemp.AVDELNINGNR.
   RUN changequery_UI.
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
   IF VALID-HANDLE(distproch) THEN DELETE PROCEDURE distproch.
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
   {muswait.i}
   {ALLSTARTDYN.I} 
   DEBUGGER:SET-BREAK().
   CMB_FOR:DELIMITER = "$".
   OPEN QUERY aq FOR EACH avdelningtemp  NO-LOCK BY avdelningtemp.AVDELNINGNAMN.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(avdelningtemp): 
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF avdelningtemp.AVDELNINGNR >= 64 THEN DO:
            status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
         END.
      END.
      ELSE DO:
         status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
      END.      
      GET NEXT aq NO-LOCK.
   END.
   CLOSE QUERY aq.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR >= 64 
      USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.
   ELSE DO:   
      FIND FIRST avdelningtemp USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.
   ASSIGN
   forvar = avdelningtemp.AVDELNINGNR
   CMB_FOR:SCREEN-VALUE = avdelningtemp.AVDELNINGNAMN.   
   andra = FALSE.
   FIND FIRST stordistemp WHERE stordistemp.ARTAL > 0
   USE-INDEX ARTAL NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:      
      ASSIGN  
      uppar = stordistemp.ARTAL.
      FIND LAST stordistemp 
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
      ASSIGN
      slutar = stordistemp.ARTAL + 2.
   END.   
   ELSE DO:
      ASSIGN
      uppar = YEAR(TODAY)
      slutar = YEAR(TODAY) + 1.
   END.
   /*LADDAR ÅR I CMB_ARTAL*/ 
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")  
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(YEAR(TODAY),"9999")
   franar = YEAR(TODAY)
   CMB_ARTAL = INPUT CMB_ARTAL.
   RUN enable_UI.   
   {FRMSIZE.I}       
   RUN changequery_UI.
   {musarrow.i}
   {WIN_M_SLUT.I}
   ASSIGN
      /*
   BTN_KOPI:LABEL IN FRAME {&FRAME-NAME} = "Kopiera~ngrunddata" 
   BTN_KUND:LABEL IN FRAME {&FRAME-NAME} = "Grunddata~n kunder" 
   BTN_LEDN:LABEL IN FRAME {&FRAME-NAME} = "Grunddata~nledningar"
   BTN_STN:LABEL IN FRAME {&FRAME-NAME} = "Grunddata~nstationer"
   BTN_STN2:LABEL IN FRAME {&FRAME-NAME} = "  Grunddata~n stationsapp.".
   */
   BTN_KOPI:LABEL IN FRAME {&FRAME-NAME} = "Kopiera grunddata" 
   BTN_KUND:LABEL IN FRAME {&FRAME-NAME} = "Grunddata kunder" 
   BTN_LEDN:LABEL IN FRAME {&FRAME-NAME} = "Grunddata ledningar"
   BTN_STN:LABEL IN FRAME {&FRAME-NAME} = "Grunddata stationer"
   BTN_STN2:LABEL IN FRAME {&FRAME-NAME} = "Grunddata stationsapp.".
   /*
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_KOPI:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_KOPI:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_KUND:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_KUND:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_LEDN:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_LEDN:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_STN:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_STN:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_STN2:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_STN2:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   */
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
   stordistemp.VIDISTRIKT:READ-ONLY IN BROWSE BRW_P1 = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN DISTAPP.P PERSISTENT SET distproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN DISTAPP.P PERSISTENT SET distproch.   
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changequery_UI C-Win 
PROCEDURE changequery_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   kommandoquery = "stordistemp.AVDELNINGNR = " + STRING(forvar) + 
      " AND stordistemp.ARTAL = " + STRING(franar).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE stordistemp THEN distnr = stordistemp.DISTRIKTID.
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
  DISPLAY CMB_FOR CMB_ARTAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_FOR CMB_ARTAL BRW_P1 BTN_KUND BTN_LEDN BTN_STN BTN_STN2 BTN_KOPI 
         BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

