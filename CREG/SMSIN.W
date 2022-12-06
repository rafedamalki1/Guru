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
&Scoped-define NEW 
&Scoped-define SHARED 
{TELENAMN.I}
DEFINE SHARED VARIABLE smsproch AS HANDLE NO-UNDO.       /* SMSAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE intele AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE VARIABLE datvar AS DATE FORMAT "99999999" NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

DEFINE NEW SHARED TEMP-TABLE tele_temp 
   FIELD DATUM AS DATE FORMAT "9999/99/99"
   FIELD TID AS CHARACTER FORMAT "X(8)"  
   FIELD TELNR AS CHARACTER FORMAT "X(12)"
   FIELD MED AS CHARACTER FORMAT "X(160)" LABEL "Meddelande"  
   FIELD NAMN AS CHARACTER FORMAT "X(40)" LABEL "Namn".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TEL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tele_temp

/* Definitions for BROWSE BRW_TEL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TEL tele_temp.TID tele_temp.NAMN ~
tele_temp.MED tele_temp.TELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TEL 
&Scoped-define QUERY-STRING-BRW_TEL FOR EACH tele_temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TEL OPEN QUERY BRW_TEL FOR EACH tele_temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TEL tele_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TEL tele_temp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 BTN_UPDAT1 FILL-IN-STARTDAT ~
BTN_MINDAT1 BRW_TEL FBTN_VISA FBTN_SKRIV BTN_HAMTA FILL-IN-TEL FILL-IN-NAMN ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-STARTDAT FILL-IN-TEL FILL-IN-NAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_HAMTA 
     LABEL "Hämta meddelanden" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MINDAT1 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_UPDAT1 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 33.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL AS CHARACTER FORMAT "X(12)":U 
     LABEL "Telenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93.5 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TEL FOR 
      tele_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TEL C-Win _STRUCTURED
  QUERY BRW_TEL NO-LOCK DISPLAY
      tele_temp.TID COLUMN-LABEL "Tid" FORMAT "X(5)":U
      tele_temp.NAMN FORMAT "X(33)":U
      tele_temp.MED FORMAT "X(65)":U
      tele_temp.TELNR COLUMN-LABEL "Telefonnr." FORMAT "X(14)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 93.5 BY 12.83
         TITLE "Meddelanden".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_UPDAT1 AT ROW 1.38 COL 44
     FILL-IN-STARTDAT AT ROW 1.92 COL 31.88 COLON-ALIGNED
     BTN_MINDAT1 AT ROW 2.5 COL 44
     BRW_TEL AT ROW 3.42 COL 1.5
     FBTN_VISA AT ROW 6.42 COL 97.5
     FBTN_SKRIV AT ROW 7.5 COL 97.5
     BTN_HAMTA AT ROW 8.63 COL 97.5
     FILL-IN-TEL AT ROW 16.75 COL 21.5 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 16.75 COL 45.5 COLON-ALIGNED
     BTN_AVB AT ROW 16.75 COL 97.5
     "Sök på:" VIEW-AS TEXT
          SIZE 8 BY .83 AT ROW 16.92 COL 3.13
     RECT-3 AT ROW 16.54 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.63 BY 17.46.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tele_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.5
         WIDTH              = 113
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 113.5
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 113.5
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
/* BROWSE-TAB BRW_TEL BTN_MINDAT1 DEFAULT-FRAME */
ASSIGN 
       BRW_TEL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TEL
/* Query rebuild information for BROWSE BRW_TEL
     _TblList          = "Temp-Tables.tele_temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tele_temp.TID
"tele_temp.TID" "Tid" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tele_temp.NAMN
"tele_temp.NAMN" ? "X(33)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tele_temp.MED
"tele_temp.MED" ? "X(65)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tele_temp.TELNR
"tele_temp.TELNR" "Telefonnr." "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TEL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win
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
ON WINDOW-CLOSE OF C-Win
DO:
   /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
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


&Scoped-define SELF-NAME BTN_HAMTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMTA C-Win
ON CHOOSE OF BTN_HAMTA IN FRAME DEFAULT-FRAME /* Hämta meddelanden */
DO:
   {muswait.i}
   datvar = INPUT FILL-IN-STARTDAT.
   IF intele = TRUE THEN DO:
      RUN in_UI.
      RUN namn_UI.
   END.
   ELSE DO:
      RUN ut_UI.
      RUN namn_UI.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINDAT1 C-Win
ON CHOOSE OF BTN_MINDAT1 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPDAT1 C-Win
ON CHOOSE OF BTN_UPDAT1 IN FRAME DEFAULT-FRAME /* + */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON CHOOSE OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   {muswait.i}   
   skrivut = TRUE.
   RUN SKRIVVAL.W (INPUT FALSE).   
   IF musz = TRUE THEN DO:
       musz = FALSE.
       skrivut = FALSE.  
   END.                 
   IF skrivut = FALSE THEN DO:
      skrivut = skrivut.
   END.   
   ELSE DO:       
      {AVBGOM.I}
      IF musz = FALSE THEN RUN VSMSIN.W.
      ELSE musz = FALSE. 
      {AVBFRAM.I}
   END.                       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   skrivut = FALSE.
   musz = FALSE.   
   {AVBGOM.I}
   IF musz = FALSE THEN RUN VSMSIN.W.
   ELSE musz = FALSE.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Datum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.   
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TEL
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
   ASSIGN
   datvar = TODAY
   FILL-IN-STARTDAT = TODAY.
   IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "settings\legalin.txt".
      END.  
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "settings\legalin.txt".
      END.          
   END.     
   RUN legaltxt_UI IN smsproch (INPUT infil, OUTPUT TABLE tele_namn).   
   IF intele = TRUE THEN DO:
      {&WINDOW-NAME}:TITLE = "Inkommna SMS".
      RUN in_UI.
      RUN namn_UI.
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Skickade SMS".
      IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
            ASSIGN 
            infil = {GRANSMS.I} + "settings\address.txt".         
         END.  
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            ASSIGN 
            infil = {GRANSMS.I} + "Settings\address.txt".        
         END.          
      END.
      RUN adresstxt_UI IN smsproch (INPUT infil, OUTPUT TABLE tele_namn).
      RUN ut_UI.
      RUN namn_UI.
   END.   
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN openbdynspec_UI IN brwproc[1].  
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_TEL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[1]
      (INPUT FILL-IN-NAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "NAMN").
   RUN addfillin_UI IN brwproc[1]
      (INPUT FILL-IN-TEL:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TELNR").
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
  DISPLAY FILL-IN-STARTDAT FILL-IN-TEL FILL-IN-NAMN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 BTN_UPDAT1 FILL-IN-STARTDAT BTN_MINDAT1 BRW_TEL FBTN_VISA 
         FBTN_SKRIV BTN_HAMTA FILL-IN-TEL FILL-IN-NAMN BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE in_UI C-Win 
PROCEDURE in_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tele_temp:
      DELETE tele_temp.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.
   IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "Logfiles\Normal\" + STRING(datvar,"99999999") + ".IN".         
      END.  
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "Logfiles\Normal\" + STRING(datvar,"99999999") + ".IN". 
      END.          
   END.  
   RUN skapa_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE namn_UI C-Win 
PROCEDURE namn_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tele_temp:
      FIND FIRST tele_namn WHERE tele_namn.TELNR = tele_temp.TELNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE tele_namn THEN DO:         
         ASSIGN
         tele_temp.NAMN = tele_namn.NAMN.         
      END.   
      ELSE DO:
         ASSIGN
         tele_temp.NAMN = "Saknas".
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI C-Win 
PROCEDURE skapa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN smsskapa_UI IN smsproch (INPUT infil, OUTPUT TABLE tele_temp).
   FIND FIRST tele_temp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tele_temp THEN DO:
      MESSAGE "Det finns inga meddelanden datum:" + STRING(datvar,"99/99/99") + "."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN.
   END.                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI C-Win 
PROCEDURE ut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tele_temp:
      DELETE tele_temp.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.
   IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "Logfiles\Normal\" + STRING(datvar,"99999999") + ".OUT".         
      END.  
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         ASSIGN 
         infil = {GRANSMS.I} + "Logfiles\Normal\" + STRING(datvar,"99999999") + ".OUT".        
      END.          
   END.    
   RUN skapa_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

