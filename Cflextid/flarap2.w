&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: flarap2.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE manad AS INTEGER NO-UNDO.
DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO.   
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
DEFINE VARIABLE utvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sign AS LOGICAL NO-UNDO.
{TIDUTTTNEW.I}

DEFINE TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(132)".    
   
DEFINE TEMP-TABLE fltid
   FIELD DATUM AS DATE  
   FIELD PLUS AS DECIMAL
   FIELD KONTROLL AS CHARACTER                
   FIELD FELMED AS CHARACTER FORMAT "X(25)"
   FIELD FELOK AS LOGICAL
   FIELD INTID AS DECIMAL 
   FIELD INKNAPP AS CHARACTER
   FIELD INAUTO AS CHARACTER
   FIELD UTTID AS DECIMAL
   FIELD UTKNAPP AS CHARACTER
   FIELD UTAUTO AS CHARACTER
   FIELD LUUT AS DECIMAL
   FIELD LUIN AS DECIMAL
   FIELD FLUTFM AS DECIMAL
   FIELD FLUTEM AS DECIMAL
   FIELD FLUTFM2 AS DECIMAL
   FIELD FLUTEM2 AS DECIMAL
   FIELD FLINFM AS DECIMAL
   FIELD FLINFM2 AS DECIMAL
   FIELD FLINEM AS DECIMAL
   FIELD FLINEM2 AS DECIMAL
   FIELD ORSAK AS CHARACTER
   FIELD DAG AS CHARACTER
   INDEX DATUM IS PRIMARY DATUM ASCENDING.

DEFINE TEMP-TABLE ftid
   FIELD DATUM AS DATE
   FIELD KNAPP AS CHARACTER
   FIELD TID AS DECIMAL
   FIELD KOM AS LOGICAL
   FIELD GICK AS LOGICAL
   FIELD KORD AS DATE
   FIELD AUTO AS CHARACTER
   FIELD ORSAK AS CHARACTER
   INDEX DATUM IS PRIMARY DATUM ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_NAAVB BTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NAAVB 
     LABEL "Nästa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 39.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 63.5 BY 1
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 124.63 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.38
     FILL-IN-REGIS AT ROW 13.63 COL 4.25 COLON-ALIGNED NO-LABEL
     FILL-IN_FORNAMN AT ROW 13.63 COL 24.25 COLON-ALIGNED NO-LABEL
     FILL-IN_NAMN AT ROW 13.75 COL 24 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     RAD_ALLVAL AT ROW 15.13 COL 2.38 NO-LABEL
     BTN_NAAVB AT ROW 28 COL 81.5
     BTN_SKRIV AT ROW 28 COL 96.5
     BTN_AVS AT ROW 28 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Flextidsrapport"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDS                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME FRAME-TIDS
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-REGIS:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME FRAME-TIDS
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME FRAME-TIDS
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK "
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB WINDOW-2
ON CHOOSE OF BTN_NAAVB IN FRAME FRAME-TIDS /* Nästa */
DO:
   {muswait.i}   
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE ftid NO-ERROR. 
   EMPTY TEMP-TABLE fltid  NO-ERROR.    
   IF NOT AVAILABLE tidpers THEN DO:      
      APPLY "WINDOW-CLOSE":U TO WINDOW-2. 
   END.
   ELSE DO:                       
      FIND NEXT tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidpers THEN DO:     
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.         
      END.             
      IF musz = TRUE THEN DO:
         musz = musz.
      END.   
      ELSE DO:
         persrec = tidpers.TIDPERSREC.
         RUN tolk_UI.       
         OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
         {musarrow.i}            
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "WINDOW-CLOSE":U TO WINDOW-2.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGL.P.                               
    {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   BRW_UT:FONT = 24.
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN DO:  
      FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   END.
   IF NOT AVAILABLE tidpers THEN DO:
      MESSAGE "Det finns ingen tid registrerad under denna period." VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.
   persrec = tidpers.TIDPERSREC.              
   EMPTY TEMP-TABLE tidut  NO-ERROR.    
   MESSAGE "Vill du visa rapporten på skärmen svara Ja." SKIP
           "Vill du skriva ut rapporten svara Nej."     
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Flexrapport"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO: 
      skrivut = FALSE.
   END.
   ELSE skrivut = TRUE.                      
   IF skrivut = FALSE THEN DO:
      RUN tolk_UI.    
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      BRW_UT:HIDDEN = FALSE.   
      RUN enable_UI.   
      {FRMSIZE.I}         
      ASSIGN
      Guru.GlobalaVariabler:collefth = ?.
      Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      ASSIGN
      Guru.GlobalaVariabler:colrighth = BTN_NAAVB:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ELSE DO:
      MESSAGE "Vill skriva ut tryck 'Ja'" SKIP
      "Vill du skicka som E-post tryck 'Nej'"   
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skrivare eller E-post"
      UPDATE answer2 AS LOGICAL.
      IF answer2 THEN DO:
         RUN skrivligg_UI (INPUT FALSE).
         IF utskriv = TRUE THEN musz = FALSE. 
         ELSE musz = TRUE. 
         IF musz = TRUE THEN musz = FALSE.
         ELSE DO:         
            FOR EACH tidpers :
               EMPTY TEMP-TABLE tidut NO-ERROR. 
               EMPTY TEMP-TABLE ftid  NO-ERROR. 
               EMPTY TEMP-TABLE fltid NO-ERROR.                
               persrec = tidpers.TIDPERSREC.
               RUN tolk_UI.    
               RUN ut_UI.
            END.
         END.
         
      END.
      ELSE DO:
         utvar = TRUE.           
         FOR EACH tidpers :
            EMPTY TEMP-TABLE tidut NO-ERROR. 
            EMPTY TEMP-TABLE ftid  NO-ERROR. 
            EMPTY TEMP-TABLE fltid NO-ERROR.                
            persrec = tidpers.TIDPERSREC.
            RUN tolk_UI.
            FOR EACH tidut:
               CREATE tidut2.
               ASSIGN tidut2.UT = tidut.UT.
            END.   
         END.   
         EMPTY TEMP-TABLE tidut NO-ERROR.          
         FOR EACH tidut2:
            CREATE tidut.
            ASSIGN tidut.UT = tidut2.UT.
         END.
         RUN EMEDDS.W (INPUT utvar).
         musz = TRUE.                          
      END.   
      LEAVE MAIN-BLOCK.
   END.   
   RUN enable_UI.   
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BTN_NAAVB BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivut_UI WINDOW-2 
PROCEDURE skrivligg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER liggande AS LOGICAL NO-UNDO.
   {SKRIVLS.I}   
END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivut_UI WINDOW-2 
PROCEDURE skrivut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UTSKRIFT*/              
   RUN tolk_UI.    
END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tolk_UI WINDOW-2 
PROCEDURE tolk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*tidtabrec = RECID(TIDREGITAB). */
   {muswait.i}
   sign = TRUE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPFL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT tidpers.PERSONALKOD, INPUT Guru.Konstanter:globforetag, INPUT manad , INPUT sign, OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN SKAPFL.P 
      (INPUT tidpers.PERSONALKOD, INPUT Guru.Konstanter:globforetag, INPUT manad, INPUT sign, OUTPUT TABLE tidut).
   END.
   {musarrow.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGL.P.               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

