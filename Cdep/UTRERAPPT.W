&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}


&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DIRDEF.I}
{LEVTEMP.I}
{DEPATEMP.I}
{LAGERTRUM.I}
{SPECMTRLTEMP.I}
&Scoped-define NEW

DEFINE TEMP-TABLE trkoll NO-UNDO
   FIELD ENR AS CHARACTER 
   FIELD TRUMMANR AS INTEGER
   FIELD trummaplats AS character   
   FIELD totmeter AS INTEGER    
   FIELD saldo AS INTEGER
   FIELD diff AS integer
   FIELD difftot AS integer
   FIELD finns AS LOGICAL  
   FIELD DEPNR AS integer
   FIELD obokad AS INTEGER
   FIELD bokad AS INTEGER 
   INDEX ENR ENR. 
                                          
DEFINE SHARED VARIABLE akval AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  

DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE dbenamning AS CHARACTER NO-UNDO.       

DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.    
DEFINE VARIABLE lagplats AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE difftot AS INTEGER NO-UNDO.
{TIDUTTTNEW.I}
   
DEFINE INPUT PARAMETER rapptyp AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR elagertrum.
DEFINE INPUT PARAMETER TABLE FOR lagertrum.
DEFINE INPUT PARAMETER TABLE FOR aotrum.



   /*DEFINE QUERY utq FOR BERBEST.*/


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_AVS 

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
     LABEL "Till Excel":L 
     SIZE 14 BY 1 .

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 101.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28 COL 58.5
     BTN_EXCEL AT ROW 28 COL 73.5
     BTN_AVS AT ROW 28 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103 BY 28.42.


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
         TITLE              = "Lista Arbetsorder - Uttag/Returer"
         HEIGHT             = 28.42
         WIDTH              = 103
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 103
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 103
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
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-VINST         = 1000.

/* SETTINGS FOR BUTTON BTN_EXCEL IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXCEL:HIDDEN IN FRAME FRAME-VINST           = TRUE.

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
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   {muswait.i}
   RUN EXCUTRE4V.P (INPUT rapptyp,INPUT TABLE elagertrum, INPUT TABLE lagertrum, INPUT TABLE aotrum, INPUT TABLE trkoll).  
   {musarrow.i}    
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
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}   
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
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
       
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE sok_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE ej_mtrl NO-ERROR.  
   FIND FIRST lagertrum WHERE NO-LOCK NO-ERROR.
   
   RUN mtrlehmt_UI IN mtrlbapph (INPUT lagertrum.DEPNR,INPUT Guru.Konstanter:globforetag ,OUTPUT vald_kundlev, OUTPUT dbenamning, OUTPUT TABLE spec_mtrl, OUTPUT TABLE ej_mtrl).
    
   IF rapptyp = 1 THEN DO: 
      RUN rubrik1_UI.
      RUN rapp1_UI.
   END.
   IF rapptyp = 2 THEN DO: 
      RUN rubrik2_UI.
      RUN rapp2_UI.
   END.
   IF rapptyp = 3 OR rapptyp = 4 OR rapptyp = 5 THEN DO: 
      RUN rubrik3_UI.
      RUN rapp3_UI.
   END.
   IF rapptyp = 6  THEN DO: 
      RUN rubrik6_UI.
      RUN rapp6_UI.
   END.
      
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").
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
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}      
   BTN_EXCEL:HIDDEN = FALSE.   
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF BTN_EXCEL:VISIBLE = TRUE THEN DO:  
      Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
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
  ENABLE BTN_SKRIV BTN_AVS BTN_EXCEL
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp1_UI WINDOW-2 
PROCEDURE rapp1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   FIND FIRST elagertrum NO-LOCK NO-ERROR.   
   OPEN QUERY utlq FOR EACH lagertrum WHERE lagertrum.TRUMMANR = elagertrum.TRUMMANR USE-INDEX TRUMMAPLATS.
   GET FIRST utlq NO-LOCK.
   DO WHILE AVAILABLE(lagertrum):                   
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = lagertrum.TRUMMAPLATS
      SUBSTRING(tidut.UT,8) = SUBSTRING(lagertrum.BENAMNING,1,15)
      SUBSTRING(tidut.UT,24) = STRING(lagertrum.STMETER)
      SUBSTRING(tidut.UT,31) = STRING(lagertrum.LAGMETER)
      SUBSTRING(tidut.UT,38) = STRING(lagertrum.UTMETER)
      SUBSTRING(tidut.UT,45) = STRING(lagertrum.AOKOPP)
      SUBSTRING(tidut.UT,50) = lagertrum.AONR
      SUBSTRING(tidut.UT,57) = STRING(lagertrum.DELNR).
      IF lagertrum.DATUM NE ? THEN SUBSTRING(tidut.UT,61) = STRING(lagertrum.DATUM) .
      ASSIGN      
      SUBSTRING(tidut.UT,70) = lagertrum.ENR
      SUBSTRING(tidut.UT,80) = lagertrum.ANVANDARE
      SUBSTRING(tidut.UT,90) = STRING(lagertrum.TRUMMANR) .
      
      /*IF mellanrum = TRUE THEN DO: 
         CREATE tidut.      
      END.*/
      GET NEXT utlq NO-LOCK.
   END.                    
   CLOSE QUERY utlq.  
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp2_UI WINDOW-2 
PROCEDURE rapp2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   lagplats = "".
   OPEN QUERY utlq FOR EACH lagertrum  USE-INDEX TRUMMAPLATS.
   GET FIRST utlq NO-LOCK.
   DO WHILE AVAILABLE(lagertrum):              
      IF lagplats NE "" AND lagplats NE lagertrum.TRUMMAPLATS THEN DO:
         CREATE tidut.      
      END.
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = lagertrum.TRUMMAPLATS
      SUBSTRING(tidut.UT,8) = SUBSTRING(lagertrum.BENAMNING,1,15)
      SUBSTRING(tidut.UT,24) = STRING(lagertrum.STMETER)
      SUBSTRING(tidut.UT,31) = STRING(lagertrum.LAGMETER)
      SUBSTRING(tidut.UT,38) = STRING(lagertrum.UTMETER)
      SUBSTRING(tidut.UT,45) = STRING(lagertrum.AOKOPP)
      SUBSTRING(tidut.UT,50) = lagertrum.AONR
      SUBSTRING(tidut.UT,57) = STRING(lagertrum.DELNR).
      IF lagertrum.DATUM NE ? THEN SUBSTRING(tidut.UT,61) = STRING(lagertrum.DATUM) .
      ASSIGN      
      SUBSTRING(tidut.UT,70) = lagertrum.ENR
      SUBSTRING(tidut.UT,80) = lagertrum.ANVANDARE
      SUBSTRING(tidut.UT,90) = STRING(lagertrum.TRUMMANR). 
      lagplats = lagertrum.TRUMMAPLATS .     
      GET NEXT utlq NO-LOCK.
   END.                    
   CLOSE QUERY utlq.  
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp3_UI WINDOW-2 
PROCEDURE rapp3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   ASSIGN
   aonummer = ""
   dnr = 0.
   IF rapptyp = 3 THEN DO:
      OPEN QUERY utlq FOR EACH aotrum WHERE aotrum.AONR = valaonr AND aotrum.DELNR = valdelnr USE-INDEX TRUMMAPLATS.
      GET FIRST utlq NO-LOCK.
   END.
   IF rapptyp = 4 THEN DO:
      OPEN QUERY utlq FOR EACH aotrum  USE-INDEX TRUMMAPLATS.
      GET FIRST utlq NO-LOCK.
   END.
   IF rapptyp = 5 THEN DO:
      OPEN QUERY utlq FOR EACH aotrum  USE-INDEX AONR2.
      GET FIRST utlq NO-LOCK.
   END.
   
   DO WHILE AVAILABLE(aotrum):              
      IF rapptyp = 5 THEN DO:
         IF aonummer = aotrum.AONR AND dnr = aotrum.DELNR THEN.      
         ELSE CREATE tidut.
      END.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = aotrum.TRUMMAplats
      SUBSTRING(tidut.UT,8) = aotrum.AONR
      SUBSTRING(tidut.UT,15) = STRING(aotrum.DELNR)      
      SUBSTRING(tidut.UT,19) = STRING(aotrum.STMETER).      
      IF aotrum.DATUMUT NE ? THEN SUBSTRING(tidut.UT,26) = STRING(aotrum.DATUMUT) . 
      ASSIGN
      SUBSTRING(tidut.UT,35) = aotrum.ENR
      SUBSTRING(tidut.UT,44) = SUBSTRING(aotrum.BENAMNING,1,15)
      SUBSTRING(tidut.UT,61) = STRING(aotrum.BESTALLARE).
      SUBSTRING(tidut.UT,71) = STRING(aotrum.TRUMMANR).
      ASSIGN aonummer = aotrum.AONR  dnr = aotrum.DELNR.      
      GET NEXT utlq NO-LOCK.
   END.                    
   CLOSE QUERY utlq.  
 END PROCEDURE.
 
 /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp6_UI WINDOW-2 
PROCEDURE rapp6_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE trkoll NO-ERROR.  
   FOR EACH lagertrum WHERE lagertrum.TRUMMA = "s" NO-LOCK:
      FIND FIRST trkoll WHERE trkoll.enr = lagertrum.enr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE trkoll THEN DO:   
         CREATE trkoll.
         assign
         trkoll.enr = lagertrum.enr
         trkoll.DEPNR = lagertrum.DEPNR
         trkoll.finns = FALSE.
      
      END.   
      assign
      trkoll.totmeter = trkoll.totmeter  + lagertrum.LAGMETER.
      IF lagertrum.AOKOPP = FALSE AND lagertrum.LAGMETER > 0 THEN DO:
         trkoll.obokad = trkoll.obokad  + lagertrum.LAGMETER.
      END.
      IF lagertrum.AOKOPP = TRUE  AND lagertrum.LAGMETER > 0 THEN DO:
         trkoll.bokad = trkoll.bokad  + lagertrum.LAGMETER.
      END.   
      
   END.   
   
   
   FOR EACH trkoll WHERE NO-LOCK:
      FIND FIRST spec_mtrl WHERE spec_mtrl.DEPNR = trkoll.DEPNR AND spec_mtrl.IBDATUM = ?  AND  spec_mtrl.ENR = trkoll.enr NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         trkoll.saldo = spec_mtrl.SALDO.
         trkoll.finns = TRUE.
         trkoll.diff = trkoll.totmeter - trkoll.saldo.
         IF trkoll.diff > 0 THEN DO:
            difftot = difftot + trkoll.diff.
         END.   
         trkoll.difftot = difftot.   
      END.   
   END.

      
   OPEN QUERY utlq FOR EACH lagertrum WHERE lagertrum.TRUMMA = "s" USE-INDEX ENR.
   GET FIRST utlq NO-LOCK.
   DO WHILE AVAILABLE(lagertrum):              
      FIND FIRST trkoll  WHERE trkoll.ENR = lagertrum.ENR NO-LOCK NO-ERROR.
      
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = lagertrum.TRUMMAPLATS
      SUBSTRING(tidut.UT,8) = SUBSTRING(lagertrum.BENAMNING,1,15)
      SUBSTRING(tidut.UT,24) = STRING(lagertrum.STMETER)
      SUBSTRING(tidut.UT,31) = STRING(lagertrum.LAGMETER)
      SUBSTRING(tidut.UT,38) = STRING(lagertrum.UTMETER)
      SUBSTRING(tidut.UT,45) = STRING(lagertrum.AOKOPP)           
      SUBSTRING(tidut.UT,70) = lagertrum.ENR.
      IF lagertrum.DATUM NE ? THEN SUBSTRING(tidut.UT,61) = STRING(lagertrum.DATUM) .
      IF lagertrum.AOKOPP = TRUE THEN DO:
         FIND FIRST aotrum WHERE aotrum.TRUMMANR = lagertrum.TRUMMANR NO-LOCK NO-ERROR.
         IF AVAILABLE aotrum THEN DO:   
            ASSIGN 
            SUBSTRING(tidut.UT,50) = aotrum.AONR
            SUBSTRING(tidut.UT,57) = STRING(aotrum.DELNR).
         END.
      END.      
      
      IF AVAILABLE trkoll THEN DO:
         SUBSTRING(tidut.UT,80) = STRING(trkoll.totmeter) .
         SUBSTRING(tidut.UT,86) = STRING(trkoll.obokad) .
         SUBSTRING(tidut.UT,92) = STRING(trkoll.bokad) .
         
         SUBSTRING(tidut.UT,98) = STRING(trkoll.SALDO) .
         SUBSTRING(tidut.UT,104) = STRING(trkoll.DIFF) .         
      END.
      SUBSTRING(tidut.UT,110) = STRING(lagertrum.ANVANDARE) .
      SUBSTRING(tidut.UT,120) = STRING(lagertrum.TRUMMANR) .
      GET NEXT utlq NO-LOCK.
   END.                    
   
   
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik1_UI WINDOW-2 
PROCEDURE rubrik1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = " VALD TRUMMA"
   SUBSTRING(tidut.UT,47) = STRING(TODAY)
   SUBSTRING(tidut.UT,57) = STRING(TIME,"HH:MM:SS").                                                                       
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "===========".
   
   CREATE tidut.
   ASSIGN
   str=                                                                    
"======.===============.======.======.======.====.==========.========.=======.=========.======".                  
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "TRUMMA"
   SUBSTRING(tidut.UT,24) = "START".
   
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "NAMN"
   SUBSTRING(tidut.UT,8) = "BENÄMNING" 
   SUBSTRING(tidut.UT,24) = "LAGER"
   SUBSTRING(tidut.UT,31) = "LAGER"
   SUBSTRING(tidut.UT,38) = "UTGÅTT"
   SUBSTRING(tidut.UT,45) = "KOPP"
   SUBSTRING(tidut.UT,50) = CAPS(Guru.Konstanter:gaok)   
   SUBSTRING(tidut.UT,61) = "DATUM"  
   SUBSTRING(tidut.UT,70) = "ENR"
   SUBSTRING(tidut.UT,80) = "ANV".
   SUBSTRING(tidut.UT,90) = "ID".

   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik2_UI WINDOW-2 
PROCEDURE rubrik2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "ALLA TRUMMOR"
   SUBSTRING(tidut.UT,47) = STRING(TODAY)
   SUBSTRING(tidut.UT,57) = STRING(TIME,"HH:MM:SS").                                                                       
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "===========".
   
   CREATE tidut.
   ASSIGN
   str=                                                                    
"======.===============.======.======.======.====.==========.========.========.=========.======.".                  
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "TRUMMA"
   SUBSTRING(tidut.UT,24) = "START".
   
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "NAMN"
   SUBSTRING(tidut.UT,8) = "BENÄMNING" 
   SUBSTRING(tidut.UT,24) = "LAGER"
   SUBSTRING(tidut.UT,31) = "LAGER"
   SUBSTRING(tidut.UT,38) = "UTGÅTT"
   SUBSTRING(tidut.UT,45) = "KOPP"
   SUBSTRING(tidut.UT,50) = CAPS(Guru.Konstanter:gaok)   
   SUBSTRING(tidut.UT,61) = "DATUM"  
   SUBSTRING(tidut.UT,70) = "ENR"
   SUBSTRING(tidut.UT,80) = "ANV".
   SUBSTRING(tidut.UT,90) = "ID".

   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik3_UI WINDOW-2 
PROCEDURE rubrik3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.
   IF rapptyp = 3  THEN DO: 
      ASSIGN
      SUBSTRING(tidut.UT,20) = "BOKADE TRUMMOR PÅ " + CAPS(Guru.Konstanter:gaok) + " " + valaonr + " " + STRING(valdelnr)
      SUBSTRING(tidut.UT,70) = STRING(TODAY)
      SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").                                                                       
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,20) = "==================================".
   END.
   IF rapptyp = 4 THEN DO: 
      ASSIGN
      SUBSTRING(tidut.UT,20) = "ALLA BOKADE TRUMMOR SORTERAD PÅ TRUMMA NAMN"
      SUBSTRING(tidut.UT,70) = STRING(TODAY)
      SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").                                                                       
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,20) = "=============================================".
   END.
   IF rapptyp =  5 THEN DO: 
      ASSIGN
      SUBSTRING(tidut.UT,20) = "ALLA BOKADE TRUMMOR SORTERAD PÅ " + CAPS(Guru.Konstanter:gaok)
      SUBSTRING(tidut.UT,70) = STRING(TODAY)
      SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").                                                                       
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,20) = "==============================================".
   END.
   
   CREATE tidut.
   ASSIGN
   str=                                                                    
"======.==========.=====.=========.=======.===============.=========.======".                  
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "TRUMMA".
      
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "NAMN"
   SUBSTRING(tidut.UT,8) = CAPS(Guru.Konstanter:gaok)   
   SUBSTRING(tidut.UT,19) = "LAGER"
   SUBSTRING(tidut.UT,26) = "BOKAD"
   SUBSTRING(tidut.UT,35) = "ENR"
   SUBSTRING(tidut.UT,44) = "BENÄMNING"
   SUBSTRING(tidut.UT,61) = "ANV"
   SUBSTRING(tidut.UT,71) = "ID".     
   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                                                            
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik6_UI WINDOW-2 
PROCEDURE rubrik6_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Alla trummor- ej bokade- bokade-lagersaldo sorterad på enr"
   
   SUBSTRING(tidut.UT,70) = STRING(TODAY)
   SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").                                                                       
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "============================================================".
   
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "För varje trumma ser man totala saldot för enr och uppdelat på ej bokade trummor och bokade trummor.".
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Desstom ser man det totala lagersaldot för enr och om det är en diff mellan summa alla trummor och lagersaldo.".
   CREATE tidut.
   ASSIGN
   str=                                                                    
"======.===============.======.======.======.====.==========.========.=======.======.=====.=====.=====.=====.=========.=====".                  
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "TRUMMA"
   SUBSTRING(tidut.UT,24) = "START"
   SUBSTRING(tidut.UT,78) = "SUMMA"
   SUBSTRING(tidut.UT,86) = "EJ"
   SUBSTRING(tidut.UT,92) = ""
   
   SUBSTRING(tidut.UT,98) = "LAGER"
   SUBSTRING(tidut.UT,104) = "DIFF".   
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "NAMN"
   SUBSTRING(tidut.UT,8) = "BENÄMNING" 
   SUBSTRING(tidut.UT,24) = "LAGER"
   SUBSTRING(tidut.UT,31) = "LAGER"
   SUBSTRING(tidut.UT,38) = "UTGÅTT"
   SUBSTRING(tidut.UT,45) = "KOPP"
   SUBSTRING(tidut.UT,50) = CAPS(Guru.Konstanter:gaok)   
   SUBSTRING(tidut.UT,61) = "DATUM"  
   SUBSTRING(tidut.UT,70) = "ENR"
   SUBSTRING(tidut.UT,78) = "TRUMMOR"
   SUBSTRING(tidut.UT,86) = "BOK"
   SUBSTRING(tidut.UT,92) = "BOK"
   
   SUBSTRING(tidut.UT,98) = "SALDO"
   SUBSTRING(tidut.UT,110) = "ANV"   
   SUBSTRING(tidut.UT,120) = "ID".

   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                                                            
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
   IF rapptyp = 6  THEN RUN EKLOGL.P. 
   ELSE RUN EKLOGS.P.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

