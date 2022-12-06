&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{EXECLIN.I}
&Scoped-define NEW 
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE allaspann AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valfore AS LOGICAL NO-UNDO.
DEFINE VARIABLE antalomr AS INTEGER NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE distvar AS INTEGER NO-UNDO.
DEFINE VARIABLE driftvar AS INTEGER NO-UNDO.
DEFINE VARIABLE spannvar AS INTEGER NO-UNDO.
DEFINE VARIABLE driftant AS INTEGER NO-UNDO.
DEFINE VARIABLE planant AS INTEGER NO-UNDO.
DEFINE VARIABLE hspant AS INTEGER NO-UNDO.
DEFINE VARIABLE lspant AS INTEGER NO-UNDO.
DEFINE VARIABLE totavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE totbortfall AS DECIMAL NO-UNDO.
DEFINE VARIABLE bortbelast AS DECIMAL NO-UNDO.
DEFINE VARIABLE totkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE Tdriftant AS INTEGER NO-UNDO.
DEFINE VARIABLE Tplanant AS INTEGER NO-UNDO.
DEFINE VARIABLE Thspant AS INTEGER NO-UNDO.
DEFINE VARIABLE Tlspant AS INTEGER NO-UNDO.
DEFINE VARIABLE Ttotavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE Ttotbortfall AS DECIMAL NO-UNDO.
DEFINE VARIABLE Tbortbelast AS DECIMAL NO-UNDO.
DEFINE VARIABLE Ttotkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE endsum AS LOGICAL NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE antaletkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}

{TIDUTTTNEW.I}
   
DEFINE SHARED TEMP-TABLE omr_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY AVDELNINGNR DISTRIKTID.

DEFINE SHARED TEMP-TABLE avd_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR.
   
DEFINE SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID. 

DEFINE TEMP-TABLE summ_omr   
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL.

DEFINE TEMP-TABLE drift_omr   
   FIELD SAVBROTT AS DECIMAL   
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE lev_omr   
   FIELD PAVBROTT AS DECIMAL
   FIELD PLANTIM AS DECIMAL.

DEFINE TEMP-TABLE summ_allt   
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL.

DEFINE TEMP-TABLE drift_allt   
   FIELD SAVBROTT AS DECIMAL   
   FIELD HTIM AS DECIMAL.

DEFINE TEMP-TABLE lev_allt   
   FIELD PAVBROTT AS DECIMAL 
   FIELD PLANTIM AS DECIMAL.


DEFINE TEMP-TABLE summ_temp
   FIELD STORTYPID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE lev_temp   
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE drift_temp
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE plan_temp
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE lev_temp2   
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE drift_temp2
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE plan_temp2
   FIELD ADELID AS INTEGER
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE lev_temp3   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE drift_temp3   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE plan_temp3
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE lev_temp4   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE drift_temp4   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE plan_temp4
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE lev_temp5   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE drift_temp5   
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

DEFINE TEMP-TABLE plan_temp5
   FIELD SAVBROTT AS DECIMAL
   FIELD PAVBROTT AS DECIMAL
   FIELD KUNDER AS INTEGER
   FIELD TOTTIM AS DECIMAL
   FIELD PLANTIM AS DECIMAL
   FIELD HTIM AS DECIMAL
   FIELD MAXTIM AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV FBTN_EXCEL BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Aktivera investering i Excel format".

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 27.88 COL 81
     FBTN_EXCEL AT ROW 27.88 COL 96 WIDGET-ID 2
     BTN_AVS AT ROW 27.88 COL 111
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
         TITLE              = "Kundkonsekvenser"
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = temp-db.tidut.ut
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:   
   {BORTBRWPROC.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT TRUE).          
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      RUN EKLOGL.P.          
   END.
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


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-TIDS /* Till Excel */
DO:
   
   RUN Kundkonexcel_UI.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
   
      
str="====================================================================================================================================".
   
   tant = 0.
   sidlangd = Guru.GlobalaVariabler:globsidl - 3.   
   RUN huvud_UI.
   RUN huvud2_UI.
   RUN fore_UI.
   FIND FIRST tidut  NO-LOCK NO-ERROR. 
   IF AVAILABLE tidut THEN DO:    
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      BRW_UT:HIDDEN = FALSE.
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
   END.
   ELSE DO:
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  FBTN_EXCEL.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   {musarrow.i} 
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bryt_UI WINDOW-2 
PROCEDURE bryt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF kant > sidlangd THEN tant = (kant - (sidlangd - tant)).
   ELSE DO:
      IF kant > sidlangd - tant THEN DO:                  
         FIND tidut WHERE RECID(tidut) = utrec NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,132) = "$"
         tant = kant.
      END.
      ELSE DO:
         tant = tant + kant.
         IF tant = sidlangd THEN tant = 0.
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE BTN_SKRIV FBTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fore_UI WINDOW-2 
PROCEDURE fore_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   EMPTY TEMP-TABLE drift_omr NO-ERROR. 
   EMPTY TEMP-TABLE lev_omr NO-ERROR. 
   EMPTY TEMP-TABLE summ_omr NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp4 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp4 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp4 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp5 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp5 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp5 NO-ERROR. 
   EMPTY TEMP-TABLE drift_allt NO-ERROR. 
   EMPTY TEMP-TABLE lev_allt NO-ERROR. 
   EMPTY TEMP-TABLE summ_allt NO-ERROR. 
   IF valfore = TRUE THEN DO:
      RUN valelalla_UI.
   END.
   ELSE DO:
      IF alla = TRUE THEN DO: 
         RUN valelalla_UI.         
      END.
      ELSE DO:
         IF alla2 = TRUE THEN DO:
            FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = 2 NO-LOCK NO-ERROR.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
            CREATE tidut.
            tant = tant + 2.         
            FOR EACH spann_temp2:
               EMPTY TEMP-TABLE summ_temp NO-ERROR. 
               ASSIGN
               spannvar = spann_temp2.SPANID
               driftvar = stordriftomrtemp.STDRIFTID
               endsum = FALSE.
               OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
               AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
               GET FIRST kq NO-LOCK.
               DO WHILE AVAILABLE(stordistemp):
                  ASSIGN
                  distvar = stordistemp.DISTRIKTID.
                  RUN storning_UI.
                  GET NEXT kq NO-LOCK.
               END.
               CLOSE QUERY kq.
               RUN storning3_UI.
               RUN bryt_UI.
            END.
            RUN str_UI.
            RUN slutomr_UI.
            CREATE tidut.
            tant = tant + 1.
            FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:            
               CREATE tidut.
               tant = tant + 1.
               SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
               FOR EACH spann_temp2 USE-INDEX SPAN:
                  EMPTY TEMP-TABLE summ_temp NO-ERROR.                   
                  ASSIGN
                  spannvar = spann_temp2.SPANID
                  driftvar = stordriftomrtemp.STDRIFTID
                  endsum = TRUE.
                  OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
                  AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
                  GET FIRST kq NO-LOCK.
                  DO WHILE AVAILABLE(stordistemp):
                     ASSIGN
                     distvar = stordistemp.DISTRIKTID.
                     RUN storning_UI.
                     GET NEXT kq NO-LOCK.
                  END.
                  CLOSE QUERY kq.
                  RUN storning3_UI.
               END.
               RUN slutomr_UI.
            END.
            RUN str_UI.
            RUN slutallt_UI.
            RUN str2_UI.
         END.
         ELSE DO:
            FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = 2 NO-LOCK NO-ERROR.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
            CREATE tidut.         
            FOR EACH spann_temp2:
               EMPTY TEMP-TABLE summ_temp NO-ERROR. 
               
               ASSIGN
               spannvar = spann_temp2.SPANID
               driftvar = stordriftomrtemp.STDRIFTID
               endsum = FALSE.
               FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
                  ASSIGN
                  distvar = omr_temp.DISTRIKTID.       
                  RUN storning_UI.                           
               END.
               RUN storning3_UI.
            END.
            RUN str_UI.
            RUN slutomr_UI.
            CREATE tidut.
            FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:            
               CREATE tidut.
               SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
               FOR EACH spann_temp2 USE-INDEX SPAN:
                  EMPTY TEMP-TABLE summ_temp NO-ERROR.                   
                  ASSIGN
                  spannvar = spann_temp2.SPANID
                  driftvar = stordriftomrtemp.STDRIFTID
                  endsum = TRUE.
                  FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
                     ASSIGN
                     distvar = omr_temp.DISTRIKTID.       
                     RUN storning_UI.                           
                  END.
                  RUN storning3_UI.            
               END.
               RUN slutomr_UI.
            END.
            RUN str_UI.
            RUN slutallt_UI.
            RUN str2_UI.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI WINDOW-2 
PROCEDURE huvud2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD2*/
   RUN nrcol_UI.    
   
   CREATE tidut.                     
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = str.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "---------- Leverans avbrottstid ----------".      
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "************* I medeltal pga *************".    
      
   CREATE tidut.
   ASSIGN      
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Antal"
                                         
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Kund.avb.tid/"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "Kund.avb.tid/".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "Driftst."
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Planer."
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Totalt"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Max pga"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "drabbade"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "lev.avb. pga"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "lev.avb. pga".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "huvuddel"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "avbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "drifts."
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "driftstörn"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "plan.avbr.".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "(min)"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "(min)"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "(min)"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "(tim)"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "(st)"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "(tim)"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "(tim)".   
   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "---------"   
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "----------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "-------------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "-------------".
 
   tant = tant + 8.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/       
   DO TRANSACTION:              
      CREATE tidut.                     
      SUBSTRING(tidut.UT,1) = "Kundkonsekvenser".      
      SUBSTRING(tidut.UT,60) = STRING(TODAY). 
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Urval:".
      CREATE tidut.
      IF uttyp = 1 THEN
      SUBSTRING(tidut.UT,1) = "Driftstörningar".
      ELSE IF uttyp = 2 THEN
      SUBSTRING(tidut.UT,1) = "Planerade avbrott".
      ELSE
      SUBSTRING(tidut.UT,1) = "Driftstörningar och planerade avbrott".
      CREATE tidut.
      IF period = 1 THEN
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(YEAR(bdatum)).
      ELSE
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(bdatum,"9999/99/99") + " - " + STRING(avdatum,"9999/99/99").
      CREATE tidut.
      tant = tant + 6.
      IF valfore = TRUE THEN DO:
         SUBSTRING(tidut.UT,1) = "Valda företag:".
         CREATE tidut.  
         tant = tant + 1.
         FOR EACH avd_temp USE-INDEX AVD:
            SUBSTRING(tidut.UT,10) = avd_temp.NAMN.
            CREATE tidut.
            tant = tant + 1.
         END.
      END.
      ELSE DO:
         IF alla = TRUE THEN DO:
            SUBSTRING(tidut.UT,1) = "Alla företag".
         END.
         ELSE DO:
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = forvar NO-LOCK NO-ERROR.
            IF alla2 = TRUE THEN DO:
               SUBSTRING(tidut.UT,1) = "Valt företag:" + avdelningtemp.AVDELNINGNAMN.
            END.
            ELSE DO:
               SUBSTRING(tidut.UT,1) = "Valt företag:" + avdelningtemp.AVDELNINGNAMN.
               CREATE tidut.
               tant = tant + 1.
               SUBSTRING(tidut.UT,1) = "Distrikt:".
               FOR EACH omr_temp USE-INDEX OMR:
                  SUBSTRING(tidut.UT,10) = omr_temp.NAMN.
                  CREATE tidut.
                  tant = tant + 1.
               END.
            END.
         END.
      END.
      IF allaspann = TRUE THEN DO:
         CREATE tidut.
         CREATE tidut.
         tant = tant + 2.
         SUBSTRING(tidut.UT,1) = "Alla spänningsnivåer".
      END.
      ELSE DO:
         CREATE tidut.
         CREATE tidut.
         tant = tant + 2.
         SUBSTRING(tidut.UT,1) = "Spänningsnivåer:".
         FOR EACH spann_temp2 USE-INDEX SPAN:
            SUBSTRING(tidut.UT,17) = spann_temp2.NAMN.
            CREATE tidut.
            tant = tant + 1.
         END.
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KUNDSTOR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT TABLE omr_temp, INPUT alla, INPUT bdatum,
         INPUT TABLE spann_temp2, OUTPUT antaletkunder, INPUT 1).      
      END.
      ELSE DO:
         RUN KUNDSTOR.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT TABLE omr_temp, INPUT alla, INPUT bdatum,
         INPUT TABLE spann_temp2, OUTPUT antaletkunder, INPUT 1).
      END.    
      CREATE tidut.
      CREATE tidut.
      tant = tant + 3.
      SUBSTRING(tidut.UT,1) = "Antalet kunder som ingår i beräkningen:" + STRING(antaletkunder).
      CREATE tidut.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kundkonexcel_UI WINDOW-2 
PROCEDURE Kundkonexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN nrcol_UI.
   ASSIGN
   slutbredd = 12   
   bredd[1] = 27
   bredd[2] = 10
   bredd[3] = 10
   bredd[4] = 10
   bredd[5] = 10
   bredd[6] = 10
   bredd[7] = 14
   bredd[8] = 16 
   bredd[9] = 16.
   
   i = 9.
   /*
   REPEAT:
      IF i > 50 THEN LEAVE.
      ASSIGN
      bredd[i] = 5.
      i = i + 1.        
   END.
   */
   bladvar = 0.
   iRad = 1.
   RUN colbredd_UI.  
   RUN startexcel_UI.
  
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                                     
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "============" THEN DO:
         LEAVE.
      END.     
   END.
   /*Poster*/ 
   raknare = 1.
   
   REPEAT:
      
/*
          
      MESSAGE 
      SUBSTRING(tidut.UT,estartnr[2],estartnr[3] - estartnr[2]]) SKIP
      SUBSTRING(tidut.UT,estartnr[3],estartnr[4] - estartnr[3]]) SKIP
      SUBSTRING(tidut.UT,estartnr[4],estartnr[5] - estartnr[4]]) SKIP
      SUBSTRING(tidut.UT,estartnr[5],estartnr[6] - estartnr[5]]) SKIP
      SUBSTRING(tidut.UT,estartnr[6],estartnr[7] - estartnr[6]]) SKIP
      SUBSTRING(tidut.UT,estartnr[7],estartnr[8] - estartnr[7]]) SKIP
      SUBSTRING(tidut.UT,estartnr[8],estartnr[9] - estartnr[8]]) SKIP
      VIEW-AS ALERT-BOX.
      */
      
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.   
      ELSE IF SUBSTRING(tidut.UT,utnr[nrcol[2]],8) = "========" THEN DO:         
      END.
                             /*
                              123456789012345678901234567890
                             "                         ---------- Leverans avbrottstid ----------"
                             */                                               
      ELSE IF SUBSTRING(tidut.UT,utnr[nrcol[2]],42) = "---------- Leverans avbrottstid ----------" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF  SUBSTRING(tidut.UT,utnr[nrcol[2]],42) = "************* I medeltal pga *************" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.                                            
      ELSE IF  SUBSTRING(tidut.UT,utnr[nrcol[2]],8) = "Driftst." THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF  SUBSTRING(tidut.UT,utnr[nrcol[2]],8) = "Huvuddel" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF  SUBSTRING(tidut.UT,utnr[nrcol[2]],5) = "(min)" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF  SUBSTRING(tidut.UT,utnr[nrcol[2]],5) = "-----" THEN DO:
         
          RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
   RUN slutexcel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nrcol_UI WINDOW-2 
PROCEDURE nrcol_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   breddantal = 8   /*antal kolumner*/
   bredd[1] = 27
   bredd[2] = 9
   bredd[3] = 9
   bredd[4] = 9
   bredd[5] = 9
   bredd[6] = 9
   bredd[7] = 13
   bredd[8] = 13.   
   
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 2.
             
      i = i + 1.
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutallt_UI WINDOW-2 
PROCEDURE slutallt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*SUMMERING ALLT*/
   FIND FIRST drift_allt NO-LOCK NO-ERROR.
   IF AVAILABLE drift_allt THEN DO:   
      FOR EACH drift_allt:      
         ACCUMULATE drift_allt.HTIM (AVERAGE).     
         ACCUMULATE drift_allt.SAVBROTT (AVERAGE).                                   
      END.   
      CREATE drift_temp5.
      ASSIGN                                  
      drift_temp5.HTIM = (ACCUM AVERAGE drift_allt.HTIM)   
      drift_temp5.SAVBROTT = (ACCUM AVERAGE drift_allt.SAVBROTT).
   END.
   FIND FIRST lev_allt NO-LOCK NO-ERROR.
   IF AVAILABLE lev_allt THEN DO:   
      FOR EACH lev_allt:      
         ACCUMULATE lev_allt.PLANTIM (AVERAGE).      
         ACCUMULATE lev_allt.PAVBROTT (AVERAGE).                               
      END.   
      CREATE lev_temp5.
      ASSIGN                                   
      lev_temp5.PLANTIM = (ACCUM AVERAGE lev_allt.PLANTIM)        
      lev_temp5.PAVBROTT = (ACCUM AVERAGE lev_allt.PAVBROTT).
   END.
   FIND FIRST summ_allt NO-LOCK NO-ERROR.
   IF AVAILABLE summ_allt THEN DO:
      FOR EACH summ_allt:      
         ACCUMULATE summ_allt.KUNDER (TOTAL).      
         ACCUMULATE summ_allt.TOTTIM  (AVERAGE).                                                
      END.   
      CREATE plan_temp5.
      ASSIGN                                   
      plan_temp5.KUNDER = (ACCUM TOTAL summ_allt.KUNDER)
      plan_temp5.TOTTIM = (ACCUM AVERAGE summ_allt.TOTTIM).
   END.

   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "TOTALSUMMA".   
   FIND FIRST drift_temp5 NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp5 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp5.HTIM * 60,">>>>>>>>9")            
      SUBSTRING(tidut.UT,59) = "*"
      SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp5.SAVBROTT,">>>>>>>>>>9.9").
      FIND FIRST plan_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp5 THEN DO:     
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp5.TOTTIM * 60,">>>>>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp5.KUNDER,">>>>>>>>>9").
      END.
      FIND FIRST lev_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE lev_temp5 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp5.PLANTIM * 60,">>>>>>>>9")               
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp5.PAVBROTT,">>>>>>>>>>9.9").
      END.      
   END.
   ELSE DO:
      FIND FIRST lev_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE lev_temp5 THEN DO:
         CREATE tidut.
/*          SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20). */
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp5.PLANTIM * 60,">>>>>>>>9")               
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp5.PAVBROTT,">>>>>>>>>>9.9").
         FIND FIRST plan_temp5 NO-LOCK NO-ERROR.
         IF AVAILABLE plan_temp5 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp5.TOTTIM * 60,">>>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp5.KUNDER,">>>>>>>>>9").
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutomr_UI WINDOW-2 
PROCEDURE slutomr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*SUMMERING DRFITOMRADE*/
   FIND FIRST drift_omr NO-LOCK NO-ERROR.
   IF AVAILABLE drift_omr THEN DO:   
      FOR EACH drift_omr:      
         ACCUMULATE drift_omr.HTIM (AVERAGE).     
         ACCUMULATE drift_omr.SAVBROTT (AVERAGE).
         ACCUMULATE drift_omr.MAXTIM (MAXIMUM).
      END.   
      CREATE drift_temp4.
      ASSIGN                                  
      drift_temp4.HTIM = (ACCUM AVERAGE drift_omr.HTIM)   
      drift_temp4.SAVBROTT = (ACCUM AVERAGE drift_omr.SAVBROTT)
      drift_temp4.MAXTIM = (ACCUM MAXIMUM drift_omr.MAXTIM).
   END.
   FIND FIRST lev_omr NO-LOCK NO-ERROR.
   IF AVAILABLE lev_omr THEN DO:   
      FOR EACH lev_omr:      
         ACCUMULATE lev_omr.PLANTIM (AVERAGE).      
         ACCUMULATE lev_omr.PAVBROTT (AVERAGE).                               
      END.   
      CREATE lev_temp4.
      ASSIGN                                   
      lev_temp4.PLANTIM = (ACCUM AVERAGE lev_omr.PLANTIM)        
      lev_temp4.PAVBROTT = (ACCUM AVERAGE lev_omr.PAVBROTT).
   END.
   FIND FIRST summ_omr NO-LOCK NO-ERROR.
   IF AVAILABLE summ_omr THEN DO:   
      FOR EACH summ_omr:      
         ACCUMULATE summ_omr.KUNDER (TOTAL).      
         ACCUMULATE summ_omr.TOTTIM  (AVERAGE).                                                
      END.   
      CREATE plan_temp4.
      ASSIGN                                   
      plan_temp4.KUNDER = (ACCUM TOTAL summ_omr.KUNDER)
      plan_temp4.TOTTIM = (ACCUM AVERAGE summ_omr.TOTTIM).
   END.
   
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SA:" + SUBSTRING(stordriftomrtemp.NAMN,1,20).   
   END.
   FIND FIRST drift_temp4 NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp4 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp4.HTIM * 60,">>>>>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(drift_temp4.MAXTIM,">>>>>>9.9")
      SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp4.SAVBROTT,">>>>>>>>>>9.9").
      FIND FIRST plan_temp4 NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp4 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp4.TOTTIM * 60,">>>>>>>>9")
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp4.KUNDER,">>>>>>>>>9").
      END.
      FIND FIRST lev_temp4 NO-LOCK NO-ERROR.
      IF AVAILABLE lev_temp4 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp4.PLANTIM * 60,">>>>>>>>9")               
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp4.PAVBROTT,">>>>>>>>>>9.9").
      END.     
   END.
   ELSE DO:
      FIND FIRST lev_temp4 NO-LOCK NO-ERROR.
      IF AVAILABLE lev_temp4 THEN DO:
         CREATE tidut.
/*          SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20). */
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp4.PLANTIM * 60,">>>>>>>>9")               
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp4.PAVBROTT,">>>>>>>>>>9.9").
         FIND FIRST plan_temp4 NO-LOCK NO-ERROR.
         IF AVAILABLE plan_temp4 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp4.TOTTIM * 60,">>>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp4.KUNDER,">>>>>>>>>9").
         END.
      END.
   END.
   EMPTY TEMP-TABLE drift_omr NO-ERROR. 
   EMPTY TEMP-TABLE lev_omr NO-ERROR. 
   EMPTY TEMP-TABLE summ_omr NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp4 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp4 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp4 NO-ERROR.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spann_UI WINDOW-2 
PROCEDURE spann_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      utrec = RECID(tidut).
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = spann_temp2.NAMN.
      CREATE tidut.
      kant = 2.
      OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(anlaggningsdeltemp):
         FIND FIRST drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
         USE-INDEX ADELID NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp2 THEN DO:            
            CREATE tidut.
            kant = kant + 1.
            SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20).
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp2.HTIM * 60,">>>>>>>>9")            
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(drift_temp2.MAXTIM,">>>>>>9.9")
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp2.SAVBROTT,">>>>>>>>>>9.9").
            FIND FIRST plan_temp2 WHERE plan_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE plan_temp2 THEN DO:               
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp2.TOTTIM * 60,">>>>>>>>9")
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp2.KUNDER,">>>>>>>>>9").
            END.
            
            FIND FIRST lev_temp2 WHERE lev_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE lev_temp2 THEN DO:               
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp2.PLANTIM * 60,">>>>>>>>9")               
               SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp2.PAVBROTT,">>>>>>>>>>9.9").
            END.            
         END.
         ELSE DO:
            FIND FIRST lev_temp2 WHERE lev_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE lev_temp2 THEN DO:
               CREATE tidut.
               ASSIGN
               kant = kant + 1
               SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20)                              
               SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp2.PLANTIM * 60,">>>>>>>>9")               
               SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp2.PAVBROTT,">>>>>>>>>>9.9").
               FIND FIRST plan_temp2 WHERE plan_temp2.ADELID = anlaggningsdeltemp.ADELID
               USE-INDEX ADELID NO-LOCK NO-ERROR.
               IF AVAILABLE plan_temp2 THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp2.TOTTIM * 60,">>>>>>9.9")
                  SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp2.KUNDER,">>>>>>>>>9").
               END.
            END.
         END.
         GET NEXT aq NO-LOCK.
      END.
      CLOSE QUERY aq.
      RUN str_UI.
      kant = kant + 1.
      RUN slutspann_UI.
   END.
   ELSE DO:
      RUN slutspann_UI.
   END.
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutspann_UI WINDOW-2 
PROCEDURE slutspann_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      kant = kant + 1.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SA: " + SUBSTRING(spann_temp2.NAMN,1,20).   
      FIND FIRST drift_temp3 NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp3 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp3.HTIM * 60,">>>>>>>>9")            
         SUBSTRING(tidut.UT,59) = "*"
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp3.SAVBROTT,">>>>>>>>>>9.9").
         FIND FIRST plan_temp3 NO-LOCK NO-ERROR.
         IF AVAILABLE plan_temp3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp3.TOTTIM * 60,">>>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp3.KUNDER,">>>>>>>>>9").
         END.
         FIND FIRST lev_temp3 NO-LOCK NO-ERROR.
         IF AVAILABLE lev_temp3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp3.PLANTIM * 60,">>>>>>>>9")               
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp3.PAVBROTT,">>>>>>>>>>9.9").
         END.        
      END.
      ELSE DO:
         FIND FIRST lev_temp3 NO-LOCK NO-ERROR.
         IF AVAILABLE lev_temp3 THEN DO:  
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lev_temp3.PLANTIM * 60,">>>>>>>>9")               
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(lev_temp3.PAVBROTT,">>>>>>>>>>9.9").
            FIND FIRST plan_temp3 NO-LOCK NO-ERROR.
            IF AVAILABLE plan_temp3 THEN DO:            
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(plan_temp3.TOTTIM * 60,">>>>>>9.9")
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(plan_temp3.KUNDER,">>>>>>>>>9").
            END.
         END.
      END.
      CREATE tidut.
      kant = kant + 1.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storning3_UI WINDOW-2 
PROCEDURE storning3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   EMPTY TEMP-TABLE drift_temp NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp3 NO-ERROR.    
   FOR EACH summ_temp:
      IF summ_temp.STORTYPID = 1 THEN DO:         
         CREATE drift_temp.
         ASSIGN
         drift_temp.ADELID = summ_temp.ADELID
         drift_temp.SAVBROTT = summ_temp.SAVBROTT
         drift_temp.KUNDER = summ_temp.KUNDER
         drift_temp.HTIM = summ_temp.HTIM
         drift_temp.MAXTIM = summ_temp.MAXTIM.         
      END.
      ELSE DO:
         CREATE lev_temp.
         ASSIGN
         lev_temp.ADELID = summ_temp.ADELID
         lev_temp.PAVBROTT = summ_temp.PAVBROTT
         lev_temp.KUNDER = summ_temp.KUNDER
         lev_temp.HTIM = summ_temp.HTIM
         lev_temp.TOTTIM = summ_temp.TOTTIM
         lev_temp.PLANTIM = summ_temp.PLANTIM.
      END.
   END.

   /*SUMMERING ANLÄGGNINGSDEL*/
   FOR EACH drift_temp BREAK BY drift_temp.ADELID:      
      ACCUMULATE drift_temp.HTIM (AVERAGE BY drift_temp.ADELID).
      ACCUMULATE drift_temp.MAXTIM (MAXIMUM BY drift_temp.ADELID).
      ACCUMULATE drift_temp.SAVBROTT (AVERAGE BY drift_temp.ADELID).
      IF LAST-OF(drift_temp.ADELID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                 
         drift_temp2.ADELID = drift_temp.ADELID
         drift_temp2.HTIM = (ACCUM AVERAGE BY drift_temp.ADELID drift_temp.HTIM)
         drift_temp2.MAXTIM = (ACCUM MAXIMUM BY drift_temp.ADELID drift_temp.MAXTIM)
         drift_temp2.SAVBROTT = (ACCUM AVERAGE BY drift_temp.ADELID drift_temp.SAVBROTT).                       
      END.     
   END.
   FOR EACH lev_temp BREAK BY lev_temp.ADELID:      
      ACCUMULATE lev_temp.PLANTIM (AVERAGE BY lev_temp.ADELID).      
      ACCUMULATE lev_temp.PAVBROTT (AVERAGE BY lev_temp.ADELID).
      IF LAST-OF(lev_temp.ADELID) THEN DO TRANSACTION:
         CREATE lev_temp2.
         ASSIGN                                 
         lev_temp2.ADELID = lev_temp.ADELID
         lev_temp2.PLANTIM = (ACCUM AVERAGE BY lev_temp.ADELID lev_temp.PLANTIM)        
         lev_temp2.PAVBROTT = (ACCUM AVERAGE BY lev_temp.ADELID lev_temp.PAVBROTT).                       
      END.     
   END.
   FOR EACH summ_temp BREAK BY summ_temp.ADELID:      
      ACCUMULATE summ_temp.KUNDER (TOTAL BY summ_temp.ADELID).      
      ACCUMULATE summ_temp.TOTTIM  (AVERAGE BY summ_temp.ADELID).
      IF LAST-OF(summ_temp.ADELID) THEN DO TRANSACTION:
         CREATE plan_temp2.
         ASSIGN                                 
         plan_temp2.ADELID = summ_temp.ADELID
         plan_temp2.KUNDER = (ACCUM TOTAL BY summ_temp.ADELID summ_temp.KUNDER)
         plan_temp2.TOTTIM = (ACCUM AVERAGE BY summ_temp.ADELID summ_temp.TOTTIM).                                
      END.     
   END.

   /*SUMMERING SPÄNNINGSNIVÅ*/
   FIND FIRST drift_temp NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp THEN DO:   
      FOR EACH drift_temp:      
         ACCUMULATE drift_temp.HTIM (AVERAGE).
         ACCUMULATE drift_temp.MAXTIM (MAXIMUM).
         ACCUMULATE drift_temp.SAVBROTT (AVERAGE).                                   
      END.      
      CREATE drift_temp3.
      ASSIGN                                    
      drift_temp3.HTIM = (ACCUM AVERAGE drift_temp.HTIM)
      drift_temp3.MAXTIM = (ACCUM MAXIMUM drift_temp.MAXTIM)
      drift_temp3.SAVBROTT = (ACCUM AVERAGE drift_temp.SAVBROTT).
   END.
   FIND FIRST lev_temp NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp THEN DO:   
      FOR EACH lev_temp:      
         ACCUMULATE lev_temp.PLANTIM (AVERAGE).      
         ACCUMULATE lev_temp.PAVBROTT (AVERAGE).                               
      END.     
      CREATE lev_temp3.
      ASSIGN                                    
      lev_temp3.PLANTIM = (ACCUM AVERAGE lev_temp.PLANTIM)        
      lev_temp3.PAVBROTT = (ACCUM AVERAGE lev_temp.PAVBROTT).
   END.
   FIND FIRST summ_temp NO-LOCK NO-ERROR.
   IF AVAILABLE summ_temp THEN DO:   
      FOR EACH summ_temp:      
         ACCUMULATE summ_temp.KUNDER (TOTAL).      
         ACCUMULATE summ_temp.TOTTIM  (AVERAGE).                                                
      END.   
      CREATE plan_temp3.
      ASSIGN                                   
      plan_temp3.KUNDER = (ACCUM TOTAL summ_temp.KUNDER)
      plan_temp3.TOTTIM = (ACCUM AVERAGE summ_temp.TOTTIM).
   END.

   /*SUMMERAS VID SLUTOMR_UI*/
   FOR EACH summ_temp:
      CREATE summ_omr.
      ASSIGN
      summ_omr.KUNDER = summ_temp.KUNDER
      summ_omr.TOTTIM = summ_temp.TOTTIM.
      CREATE summ_allt.
      ASSIGN
      summ_allt.KUNDER = summ_temp.KUNDER
      summ_allt.TOTTIM = summ_temp.TOTTIM.
   END.
   FOR EACH drift_temp:
      CREATE drift_omr.
      ASSIGN
      drift_omr.HTIM = drift_temp.HTIM
      drift_omr.SAVBROTT = drift_temp.SAVBROTT
      drift_omr.MAXTIM = drift_temp.MAXTIM.
      CREATE drift_allt.
      ASSIGN
      drift_allt.HTIM = drift_temp.HTIM
      drift_allt.SAVBROTT = drift_temp.SAVBROTT.
   END.
   FOR EACH lev_temp:
      CREATE lev_omr.
      ASSIGN
      lev_omr.PLANTIM = lev_temp.PLANTIM
      lev_omr.PAVBROTT = lev_temp.PAVBROTT.
      CREATE lev_allt.
      ASSIGN
      lev_allt.PLANTIM = lev_temp.PLANTIM
      lev_allt.PAVBROTT = lev_temp.PAVBROTT.
   END.
   RUN spann_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storning_UI WINDOW-2 
PROCEDURE storning_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN STORSUM3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
      INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).      
   END.
   ELSE DO:
      RUN STORSUM3.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
      INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).
   END.    
    /*slå ihop spänning*/
   IF spannvar = 421 THEN DO:
      spannvar = 20.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN STORSUM3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
         INPUT avdatum, INPUT period, 
         INPUT uttyp, INPUT driftvar, INPUT alla, 
         INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).      
      END.
      ELSE DO:
         RUN STORSUM3.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
         INPUT avdatum, INPUT period,
         INPUT uttyp, INPUT driftvar, INPUT alla, 
         INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).
      END.    
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE str2_UI WINDOW-2 
PROCEDURE str2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "========="
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "========="
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "========="
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "========="   
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "=========="
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "============="
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "=============".         
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE str_UI WINDOW-2 
PROCEDURE str_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "---------"  
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "----------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "-------------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "-------------".         
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valelalla_UI WINDOW-2 
PROCEDURE valelalla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = 2 NO-LOCK NO-ERROR.
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = stordriftomrtemp.NAMN.
   CREATE tidut.
   tant = tant + 2.      
   FOR EACH spann_temp2 USE-INDEX SPAN:
      EMPTY TEMP-TABLE summ_temp NO-ERROR.       
      ASSIGN
      spannvar = spann_temp2.SPANID
      driftvar = stordriftomrtemp.STDRIFTID
      endsum = FALSE.
      RUN storning_UI.   
      RUN storning3_UI.
      RUN bryt_UI.
   END.
   RUN str_UI.
   RUN slutomr_UI.
   CREATE tidut.
   tant = tant + 1.
   FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:         
      CREATE tidut.
      tant = tant + 1.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = stordriftomrtemp.NAMN.
      FOR EACH spann_temp2 USE-INDEX SPAN:
         EMPTY TEMP-TABLE summ_temp NO-ERROR.          
         ASSIGN
         spannvar = spann_temp2.SPANID
         driftvar = stordriftomrtemp.STDRIFTID
         endsum = TRUE.
         RUN storning_UI.
         RUN storning3_UI.
      END.
      RUN slutomr_UI.
   END.
   RUN str_UI.
   RUN slutallt_UI.
   RUN str2_UI.   
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

