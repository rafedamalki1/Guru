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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}  
{ANMARKD.I}
{LISTDEF.I}
DEFINE INPUT PARAMETER selupp AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE edmedd AS CHARACTER NO-UNDO.                              
DEFINE SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.      
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str4 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalsum AS DECIMAL NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE totant AS INTEGER NO-UNDO.
DEFINE VARIABLE temptant AS INTEGER NO-UNDO.
DEFINE VARIABLE kvaravsidan    AS INTEGER NO-UNDO.
DEFINE VARIABLE exkant        AS INTEGER NO-UNDO.
DEFINE VARIABLE extant        AS INTEGER NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE sidlangdex AS INTEGER NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE upput AS LOGICAL NO-UNDO.
DEFINE VARIABLE berlistapph AS HANDLE NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE printername AS CHARACTER NO-UNDO.
DEFINE VARIABLE standardskriv AS LOGICAL NO-UNDO.
DEFINE VARIABLE ppvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vartprospar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tamedtext AS LOGICAL NO-UNDO. 
DEFINE VARIABLE bestdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
{WHANDLTEMP.I}
IF selupp = "Materiel/upplag/artikel inklusive texter" THEN tamedtext = TRUE.
ELSE IF selupp = "Förenklat Byggprotokoll" THEN tamedtext = FALSE.
ELSE tamedtext = FALSE.
{TIDUTTTNEW.I}                       
   
DEFINE TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}
{FLBERTEMP.I}
DEFINE TEMP-TABLE emtrl_temp2 LIKE mtrl_temp2.
DEFINE NEW SHARED TEMP-TABLE extemp
    {EXTEMPDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE excoltemp
   FIELD COLNAME AS CHARACTER
   FIELD COLNUM AS INTEGER
   FIELD WDTH AS INTEGER.

&Scoped-define SHARED SHARED
{KONVALTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-DYN
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
&Scoped-Define ENABLED-OBJECTS BTN_AVS-DYN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS-DYN AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HTML 
     LABEL "Visa i IE":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PDF 
     LABEL "Visa i PDF":L 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 98.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.5 BY 28.42 WIDGET-ID 200.

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 27.54 COL 26.13
     BTN_EXCEL AT ROW 27.54 COL 41.13
     BTN_HTML AT ROW 27.54 COL 56.13
     BTN_PDF AT ROW 27.54 COL 71.13
     BTN_AVS AT ROW 27.54 COL 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99 BY 28 WIDGET-ID 300.

DEFINE FRAME FRAME-DYN
     BTN_AVS-DYN AT ROW 27.54 COL 86 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99 BY 28 WIDGET-ID 100.


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
         TITLE              = "Lista materiel/upplag/byggid/artikel"
         HEIGHT             = 28.42
         WIDTH              = 99.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 166.13
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 166.13
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-DYN:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-VINST:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-DYN:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-VINST:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-DYN
   FRAME-NAME                                                           */
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

&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DYN
&Scoped-define SELF-NAME BTN_AVS-DYN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS-DYN WINDOW-2
ON CHOOSE OF BTN_AVS-DYN IN FRAME FRAME-DYN /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa i Excel */
DO: 
   {muswait.i}      
   RUN BERLIST2EX.P (INPUT sidlangdex, INPUT tamedtext, INPUT selupp).
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HTML WINDOW-2
ON CHOOSE OF BTN_HTML IN FRAME FRAME-VINST /* Visa i IE */
DO: 
   {muswait.i}
   RUN UTBER2HTM.P (INPUT TABLE tidut).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PDF WINDOW-2
ON CHOOSE OF BTN_PDF IN FRAME FRAME-VINST /* Visa i PDF */
DO: 
   {muswait.i}
   RUN UTBER2PDF.P (INPUT TABLE tidut).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   IF tamedtext = TRUE THEN  RUN SKRIVVAL.W (INPUT TRUE).       
   ELSE  RUN SKRIVVAL.W (INPUT FALSE).    
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


&Scoped-define FRAME-NAME FRAME-DYN
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
   
   &Scoped-define FRAME-NAME FRAME-A
  
   {WIN_M_START.I}
   ON 'VALUE-CHANGED' OF FRAME FRAME-A PERSISTENT RUN fixastorlek_UI IN THIS-PROCEDURE.
   ON 'VALUE-CHANGED' OF WINDOW-2 PERSISTENT RUN fixastorlek_UI IN THIS-PROCEDURE.
   {muswait.i} 
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   ASSIGN
   musz = FALSE
   sidlangd = Guru.GlobalaVariabler:globsids - 3.
   
   RUN tamedtext_UI IN berlistapph (INPUT tamedtext).
   IF selupp = "Förenklat Byggprotokoll" THEN DO:
      RUN skapalistabyggupp_UI IN berlistapph (INPUT valaonr,INPUT valomrade,INPUT valort,INPUT Guru.Konstanter:globanvpkod,
                                        INPUT prisvar,INPUT edmedd,INPUT Guru.GlobalaVariabler:globsids,INPUT valkonst, INPUT TABLE kon_val,
                                        OUTPUT musz2,OUTPUT TABLE extemp,
                                        OUTPUT TABLE tidut).
   END.
   ELSE IF selupp = "Materiel/upplag/byggid/artikel" THEN DO:
      
      RUN skapalistabygguppdyn_UI IN berlistapph (INPUT valaonr,INPUT valomrade,INPUT valort,INPUT Guru.Konstanter:globanvpkod,
                                        INPUT prisvar,INPUT edmedd,INPUT Guru.GlobalaVariabler:globsids,INPUT valkonst, INPUT TABLE kon_val,OUTPUT musz2, OUTPUT TABLE emtrl_temp2).                            
   END.
   ELSE DO:   
      RUN skapalista_UI IN berlistapph (INPUT valaonr,INPUT valomrade,INPUT valort,INPUT Guru.Konstanter:globanvpkod,
                                        INPUT prisvar,INPUT edmedd,INPUT Guru.GlobalaVariabler:globsids,INPUT valkonst, INPUT TABLE kon_val,
                                        OUTPUT musz2,OUTPUT TABLE extemp,
                                        OUTPUT TABLE tidut).
   END.                                     
   musz = musz2.
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
   {FRMSIZEF.I}   
   IF selupp = "Materiel/upplag/byggid/artikel" THEN DO:      
      RUN BYGGVISDYN.P PERSISTENT SET bestdynh (INPUT prisvar, INPUT THIS-PROCEDURE, INPUT framesizeh,INPUT TABLE whandltemp).
      RUN skapabrw_UI IN bestdynh (INPUT 2, TEMP-TABLE emtrl_temp2:DEFAULT-BUFFER-HANDLE).
      RUN fixastorlek_UI IN bestdynh.
      ASSIGN FRAME FRAME-VINST:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-DYN:HIDDEN = FALSE.  
      
       IF visvalvar > 1 THEN DO:
         RUN fixastorlek_UI.
         RUN excelproc_UI IN bestdynh.  
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         Guru.GlobalaVariabler:retvalkoll = FALSE.
         LEAVE MAIN-BLOCK.
      END.    
   END.
   ELSE DO:   
      ASSIGN FRAME FRAME-VINST:HIDDEN = FALSE.
      ASSIGN FRAME FRAME-DYN:HIDDEN = TRUE.      
      ASSIGN     
      Guru.GlobalaVariabler:collefth = BTN_AVS:HANDLE.           
      Guru.GlobalaVariabler:colrighth = BTN_PDF:HANDLE.            
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
      BTN_HTML:HIDDEN = TRUE.
      Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      IF visvalvar > 1 THEN DO:
         IF visvalvar = 2 THEN APPLY "CHOOSE" TO  BTN_EXCEL.
         IF visvalvar = 3 THEN APPLY "CHOOSE" TO  BTN_HTML.
         IF visvalvar = 4 THEN APPLY "CHOOSE" TO  BTN_PDF.
         APPLY "CHOOSE" TO BTN_AVS IN FRAME FRAME-VINST. 
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
         Guru.GlobalaVariabler:retvalkoll = FALSE.
         LEAVE MAIN-BLOCK.
      END.
   END.      
   {musarrow.i}                                                
   {WIN_M_SLUT.I}
   RUN fixastorlek_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/   
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 1.   
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-DYN:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_AVS-DYN:HANDLE IN FRAME FRAME-DYN). 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME FRAME-VINST).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERLISTAPP2.P PERSISTENT SET berlistapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERLISTAPP2.P PERSISTENT SET berlistapph.
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
  VIEW FRAME FRAME-A IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  ENABLE BTN_AVS-DYN 
      WITH FRAME FRAME-DYN IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DYN}
  ENABLE BTN_SKRIV BTN_EXCEL BTN_HTML BTN_PDF BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixastorlek_UI WINDOW-2 
PROCEDURE fixastorlek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(bestdynh) THEN RUN fixastorlek_UI IN bestdynh.   
   Guru.GlobalaVariabler:StartKolumnForKnappar = FRAME FRAME-VINST:WIDTH-CHARS.  
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_AVS:HANDLE).
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_PDF:HANDLE).
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_HTML:HANDLE).
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_EXCEL:HANDLE).
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_SKRIV:HANDLE).
   Guru.GlobalaVariabler:StartKolumnForKnappar = FRAME FRAME-DYN:WIDTH-CHARS.
   Guru.Konstanter:PlaceraKnapparVagrattFranHoger(BTN_AVS-DYN:HANDLE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musa WINDOW-2 
PROCEDURE musa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musw WINDOW-2 
PROCEDURE musw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     {muswait.i} 
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
   IF tamedtext = TRUE THEN RUN EKLOGL.P.    
   ELSE RUN EKLOGS.P.    
    
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI WINDOW-2 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

