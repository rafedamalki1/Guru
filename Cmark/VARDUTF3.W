&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



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
{MAGA.I}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER valvard AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER fbeteckn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER markarec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR maga.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}

{REGVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE omrrec AS RECID NO-UNDO.
/*DEFINE SHARED VARIABLE kto LIKE KONTO.KONTO NO-UNDO.*/
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE allnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE allpersnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjfore AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjfore2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE skprotapph AS HANDLE NO-UNDO.

&Scoped-define SHARED SHARED  
{MARKVAL.I}                               
{TIDUTTTNEW.I}
DEFINE NEW SHARED TEMP-TABLE uppfoltemp   
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER  
   FIELD ALLAMA AS LOGICAL
   FIELD VALVARD AS CHARACTER
   FIELD FORSTA AS LOGICAL
   FIELD STAMP AS LOGICAL.  

{SUMTEMP.I}
{allavardtemp.I}
     
DEFINE TEMP-TABLE fastaga         
   FIELD MARKNR AS INTEGER
   FIELD MARKAGARE AS CHARACTER
   FIELD PERSONNUMMER AS CHARACTER     
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING.      

                       
DEFINE {&NEW} SHARED VARIABLE TOG_ALLAMA AS LOGICAL INITIAL no 
     LABEL "ALLA MARKÄGARE":L 
     VIEW-AS TOGGLE-BOX
     SIZE 24.25 BY 1 NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE TOG_FORSTA AS LOGICAL INITIAL no 
     LABEL "ENBART SAMMANSTÄLLNING":L 
     VIEW-AS TOGGLE-BOX
     SIZE 26.38 BY 1 NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE TOG_STAMP AS LOGICAL INITIAL ? 
     LABEL "ENBART STÄMPLINSLÄNGD":L 
     VIEW-AS TOGGLE-BOX
     SIZE 26.38 BY 1 NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE TOG_VAGHALLARE AS LOGICAL INITIAL no 
     LABEL "Väghållare" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .79 NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE TOG_TILLFSK AS LOGICAL INITIAL no 
     LABEL "Visa enbart tillfällig skada" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.5 BY .79 NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
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


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_EXCEL BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 114.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28.13 COL 71.5
     BTN_EXCEL AT ROW 28.13 COL 86.5
     BTN_AVS AT ROW 28.13 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
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
         TITLE              = "Visa markvärdering"
         HEIGHT             = 28.42
         WIDTH              = 116
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 116
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
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa i Excel */
DO: 
   excellista = 0.
  
   IF hjfore = "UMEA" OR hjfore = "UMBR"  THEN DO:
      RUN EXPROTOU.P (INPUT excellista , INPUT hjfore, INPUT TABLE tidut).        
   END.
   ELSE DO:   
      RUN EXPROTO.P (INPUT excellista , INPUT hjfore, INPUT TABLE tidut).        
   END.
   ASSIGN
   Guru.Konstanter:globforetag = hjfore2
   hjfore = hjfore2.
   /*IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "cELPA"  THEN DO:
      RUN EXPROTOU.P (INPUT excellista , INPUT Guru.Konstanter:globforetag, INPUT TABLE tidut).        
   END.
   ELSE DO:   
      RUN EXPROTO.P (INPUT excellista , INPUT Guru.Konstanter:globforetag, INPUT TABLE tidut).        
   END.*/
   {musarrow.i}       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa i Excel */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
     RUN ut_UI.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
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
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(skprotapph) THEN DELETE PROCEDURE skprotapph.
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

   hjfore = Guru.Konstanter:globforetag.
   hjfore2 = Guru.Konstanter:globforetag.   
   /*IF Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:
      MESSAGE "Vill du ta ut värderingsprotokoll för Picab - svara Ja" SKIP
              "Vill du ta ut värderingsprotokoll för Umeå Energi - svara Nej"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
      IF val = TRUE THEN hjfore = "PICA".      
      IF val = FALSE THEN hjfore = "UMEA".
   END.*/
   EMPTY TEMP-TABLE uppfoltemp NO-ERROR.    
   CREATE uppfoltemp.
   ASSIGN
   uppfoltemp.FORETAG = hjfore
   /*uppfoltemp.FORETAG = Guru.Konstanter:globforetag*/
   /*uppfoltemp.ANVANDARE = Guru.Konstanter:globanv  */
   uppfoltemp.ALLAMA = TOG_ALLAMA
   uppfoltemp.VALVARD = valvard
   uppfoltemp.FORSTA = TOG_FORSTA
   uppfoltemp.STAMP = TOG_STAMP.      
   IF TOG_VAGHALLARE = TRUE THEN DO:
      uppfoltemp.ANVANDARE = "JA".
   END.
   ELSE uppfoltemp.ANVANDARE = "NEJ".            
   IF TOG_TILLFSK = TRUE THEN DO:
      RUN vptillfsk_UI IN skprotapph
      (INPUT valvard ,INPUT fbeteckn,INPUT markarec,INPUT TABLE uppfoltemp,INPUT TABLE markval,INPUT TABLE maga,OUTPUT TABLE tidut).
   END.
   ELSE DO:   
      RUN varderingsprotokoll_UI IN skprotapph
      (INPUT valvard ,INPUT fbeteckn,INPUT markarec,INPUT TABLE uppfoltemp,INPUT TABLE markval,INPUT TABLE maga,OUTPUT TABLE tidut).
      
   END.
   ASSIGN
   Guru.Konstanter:globforetag = hjfore2.      
      
   FIND LAST tidut NO-LOCK NO-ERROR.  
   IF skrivut = FALSE THEN DO:
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.       
   END.
   ELSE DO:   
      RUN ut_UI.
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK.     
   END.
   RUN enable_UI.   
   {FRMSIZE.I}     
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  BTN_EXCEL.
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
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph.
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
  ENABLE BTN_SKRIV BTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
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
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

