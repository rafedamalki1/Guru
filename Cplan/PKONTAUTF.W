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

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pansv AS CHARACTER NO-UNDO.
{ALLDEF.I}
/* Local Variable Definitions ---                                       */ 
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{AVTPLANTEMP.I}
{OMRTEMPW.I}                                   
 

DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE tillar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.    


DEFINE VARIABLE ktotea AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotber AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotarb AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotmask AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotmtrl AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotovr AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE kbertim AS DECIMAL NO-UNDO.
DEFINE VARIABLE kmsktim AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotutr AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotutrtim AS DECIMAL NO-UNDO.

DEFINE VARIABLE karbtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ktotalt3 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ktotalt4 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ktotalt5 AS DECIMAL NO-UNDO. 

DEFINE VARIABLE ototea AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototber AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototarb AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototmask AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototmtrl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototovr AS DECIMAL NO-UNDO. 
DEFINE VARIABLE obertim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE oarbtim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE omsktim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototutr AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ototutrtim AS DECIMAL NO-UNDO. 

DEFINE VARIABLE oarbtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt5 AS DECIMAL NO-UNDO.

DEFINE VARIABLE planvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE xtypmtrl AS INTEGER NO-UNDO.
DEFINE VARIABLE monpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tempupp   
    FIELD totea      AS DECIMAL    
    FIELD totarb     AS DECIMAL    
    FIELD totmask    AS DECIMAL    
    FIELD totmtrl    AS DECIMAL    
    FIELD totovr     AS DECIMAL    
    FIELD totalt     AS DECIMAL    
    FIELD arbtim     AS DECIMAL    
    FIELD bertim     AS DECIMAL    
    FIELD msktim     AS DECIMAL    
    FIELD totutr     AS DECIMAL    
    FIELD totutrtim  AS DECIMAL    
    FIELD ktotea     AS DECIMAL   
    FIELD ktotber     AS DECIMAL   
    FIELD ktotarb    AS DECIMAL          
    FIELD ktotmask   AS DECIMAL    
    FIELD ktotmtrl   AS DECIMAL    
    FIELD ktotovr    AS DECIMAL    
    FIELD ktotalt    AS DECIMAL    
    FIELD karbtim    AS DECIMAL    
    FIELD kbertim    AS DECIMAL    
    FIELD kmsktim    AS DECIMAL    
    FIELD ktotutr    AS DECIMAL    
    FIELD ktotutrtim AS DECIMAL    
    FIELD ototea     AS DECIMAL 
    FIELD ototber     AS DECIMAL
    FIELD ototarb    AS DECIMAL         
    FIELD ototmask   AS DECIMAL    
    FIELD ototmtrl   AS DECIMAL    
    FIELD ototovr    AS DECIMAL    
    FIELD ototalt    AS DECIMAL    
    FIELD oarbtim    AS DECIMAL    
    FIELD obertim    AS DECIMAL    
    FIELD omsktim    AS DECIMAL    
    FIELD ototutr    AS DECIMAL    
    FIELD ototutrtim AS DECIMAL    
    FIELD oarbtim1   AS DECIMAL    
    FIELD ototalt1   AS DECIMAL    
    FIELD oarbtim2   AS DECIMAL    
    FIELD ototalt2   AS DECIMAL          
    FIELD oarbtim3   AS DECIMAL    
    FIELD ototalt3   AS DECIMAL    
    FIELD oarbtim4   AS DECIMAL    
    FIELD ototalt4   AS DECIMAL    
    FIELD oarbtim5   AS DECIMAL    
    FIELD ototalt5   AS DECIMAL    
    FIELD karbtim1   AS DECIMAL    
    FIELD ktotalt1   AS DECIMAL    
    FIELD karbtim2   AS DECIMAL    
    FIELD ktotalt2   AS DECIMAL   
    FIELD karbtim3   AS DECIMAL
    FIELD ktotalt3   AS DECIMAL
    FIELD karbtim4   AS DECIMAL
    FIELD ktotalt4   AS DECIMAL
    FIELD karbtim5   AS DECIMAL
    FIELD ktotalt5   AS DECIMAL.

DEFINE SHARED TEMP-TABLE omrkonto    
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMREC AS RECID  
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.
   
DEFINE SHARED TEMP-TABLE okod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.    
   
DEFINE SHARED TEMP-TABLE kkod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.


/*BEREDNING*/
{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}
/*SLUT BEREDNING*/
   
   
{TIDUTTTNEW.I}

DEFINE NEW SHARED TEMP-TABLE tidut2
   FIELD UT AS CHARACTER
   FIELD LONGNAMN AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut2

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut2.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut2


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
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Till excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut2.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28 COL 81
     BTN_EXCEL AT ROW 28 COL 96
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
      TABLE: tidut2 T "?" NO-UNDO temp-db tidut2
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Följesedel"
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
     _TblList          = "Temp-Tables.tidut2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut2.ut
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
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Till excel */
DO: 
   {muswait.i}
   RUN EXPKONT.P (INPUT period).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_EXCEL IN FRAME FRAME-VINST /* Till excel */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:    
   RUN SKRIVVAL.W (INPUT TRUE).
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
     {muswait.i}
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
   {ALLSTARTDYN.I}
   IF vallista = 1 THEN DO:  
      {&WINDOW-NAME}:TITLE = "Rapport: " + Guru.Konstanter:gpll + " - konto".   
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Rapport: Organisation - objekt".
   END.   
   offert = FALSE.
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   EMPTY TEMP-TABLE tidut2 NO-ERROR.    
   RUN huvud_UI.
   IF vallista = 1 THEN DO:
      RUN lista1_UI.       
   END.
   ELSE DO:
      RUN lista3_UI.
   END.      
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      ELSE DO:   
         RUN ut_UI.           
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.      
   RUN enable_UI.   
   {FRMSIZE.I}     
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/                                
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                           
         CREATE tidut2.
         IF vallista = 1 THEN DO:
            SUBSTRING(tidut2.UT,1) = "RAPPORT: " + CAPS(Guru.Konstanter:gplk) + " - KONTO".        
         END.
         ELSE DO:
            SUBSTRING(tidut2.UT,1) = "RAPPORT: ORGANISATION - OBJEKT".
         END.   
         ASSIGN         
         SUBSTRING(tidut2.UT,40) = STRING(TODAY)
         SUBSTRING(tidut2.UT,50) = STRING(TIME,"HH:MM:SS").
         CREATE tidut2.
         SUBSTRING(tidut2.UT,1) = "PERIOD :".
         IF period = 1 THEN DO:
            SUBSTRING(tidut2.UT,10) = STRING(franar).
         END.
         ELSE DO:
            SUBSTRING(tidut2.UT,10) = STRING(franar) + " - " + STRING(tillar). 
         END.       
         CREATE tidut2.
         CREATE tidut2.
         IF period = 1 THEN
         SUBSTRING(tidut2.UT,1) = "==================================================================================================================================".
         ELSE SUBSTRING(tidut2.UT,1) = "==========================================================================================================".
      END.   
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI WINDOW-2 
PROCEDURE ladda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
CREATE tempupp.
ASSIGN
tempupp.totea     =   totea
tempupp.totarb    =   totarb     
tempupp.totmask   =   totmask   
tempupp.totmtrl   =   totmtrl   
tempupp.totovr    =   totovr    
tempupp.totalt    =   totalt    
tempupp.arbtim    =   arbtim    
tempupp.bertim    =   bertim    
tempupp.msktim    =   msktim    
tempupp.totutr    =   totutr    
tempupp.totutrtim =  totutrtim 
tempupp.ktotea    =   ktotea    
tempupp.ktotarb   =   ktotarb   
tempupp.ktotmask  =   ktotmask  
tempupp.ktotmtrl  =   ktotmtrl  
tempupp.ktotovr   =   ktotovr   
tempupp.ktotalt   =   ktotalt   
tempupp.karbtim   =   karbtim   
tempupp.kbertim   =   kbertim   
tempupp.kmsktim   =   kmsktim   
tempupp.ktotutr   =   ktotutr   
tempupp.ktotutrtim =   ktotutrtim
tempupp.ototea    =   ototea    
tempupp.ototarb   =   ototarb   
tempupp.ototmask  =   ototmask  
tempupp.ototmtrl  =   ototmtrl  
tempupp.ototovr   =   ototovr   
tempupp.ototalt   =   ototalt   
tempupp.oarbtim   =   oarbtim   
tempupp.obertim   =   obertim   
tempupp.omsktim   =   omsktim   
tempupp.ototutr   =   ototutr   
tempupp.ototutrtim =   ototutrtim
tempupp.oarbtim1  =   oarbtim1  
tempupp.ototalt1  =   ototalt1  
tempupp.oarbtim2  =   oarbtim2  
tempupp.ototalt2  =   ototalt2  
tempupp.oarbtim3  =   oarbtim3  
tempupp.ototalt3  =   ototalt3  
tempupp.oarbtim4  =   oarbtim4  
tempupp.ototalt4  =   ototalt4  
tempupp.oarbtim5  =   oarbtim5  
tempupp.ototalt5  =   ototalt5  
tempupp.karbtim1  =   karbtim1  
tempupp.ktotalt1  =   ktotalt1  
tempupp.karbtim2  =   karbtim2  
tempupp.ktotalt2  =   ktotalt2  
tempupp.karbtim3  =   karbtim3  
tempupp.ktotalt3  =   ktotalt3  
tempupp.karbtim4  =   karbtim4  
tempupp.ktotalt4  =   ktotalt4  
tempupp.karbtim5  =   karbtim5  
tempupp.ktotalt5  =   ktotalt5.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lista1_UI WINDOW-2 
PROCEDURE lista1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN omrade_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lista3_UI WINDOW-2 
PROCEDURE lista3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN omrade_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrade_UI WINDOW-2 
PROCEDURE omrade_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   RUN ladda_UI.
   IF Guru.Konstanter:appcon THEN DO:
      RUN PKONTUTFAAPPC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT pansv,INPUT period,INPUT planvar,INPUT franar,INPUT tillar,
      INPUT monpris,INPUT xtypmtrl,INPUT vallista,INPUT TABLE tempupp,
      INPUT TABLE okod,INPUT TABLE kkod,INPUT TABLE omrkonto,
      OUTPUT TABLE tidut2, INPUT Guru.Konstanter:globforetag).
             
   END.
   ELSE DO:
      RUN PKONTUTFAAPPC.P
      (INPUT pansv,INPUT period,INPUT planvar,INPUT franar,INPUT tillar,
       INPUT monpris,INPUT xtypmtrl,INPUT vallista,INPUT TABLE tempupp,
      INPUT TABLE okod,INPUT TABLE kkod,INPUT TABLE omrkonto,
      OUTPUT TABLE tidut2, INPUT Guru.Konstanter:globforetag).
              
   END.

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
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FOR EACH tidut2:
      CREATE tidut.
      BUFFER-COPY tidut2 TO tidut.
   END.
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGL.P. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

