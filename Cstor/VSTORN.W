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
DEFINE SHARED VARIABLE distvar AS INTEGER FORMAT "->>>>>>9" NO-UNDO.
DEFINE SHARED VARIABLE stornrid AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE anmtyp AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
{ANMARKD.I}

&SCOPED-DEFINE NEW 
&SCOPED-DEFINE SHARED SHARED
{AVDELNINGTEMP.I} 
{STRTEMP.I}
{STORTEMP.I}
DEFINE TEMP-TABLE extrastorntemp NO-UNDO LIKE urstorntemp.

{TIDUTTTNEW.I}


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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UT BTN_SKRIV BTN_AVB 

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
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28 COL 96.5
     BTN_AVB AT ROW 28 COL 111
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
         TITLE              = "Visa arbetsorder"
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

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.        
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
   {ALLSTARTDYN.I}
   FIND FIRST vstorntemp WHERE vstorntemp.STORNUMMERID = stornrid
      AND vstorntemp.DISTRIKTID = distvar NO-LOCK NO-ERROR.
   ASSIGN {&WINDOW-NAME}:TITLE = "Visning av störning".
   {muswait.i}    
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
      
str="====================================================================================================================================".
str="------------------------------------------------------------------------------------------------------------------------------------".

   RUN huvud_UI.
   FIND FIRST tidut NO-LOCK NO-ERROR.    
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
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anmark_UI WINDOW-2 
PROCEDURE anmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 2 THEN DO: 
      IF anmtyp = FALSE THEN DO:
         ASSIGN            
         SUBSTRING(tidut.UT,23) = ":"   
         SUBSTRING(tidut.UT,24) = tidtext.
         CREATE tidut.
      END.
      ELSE DO:
         ASSIGN                     
         SUBSTRING(tidut.UT,1) = tidtext.
         CREATE tidut.
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
  ENABLE BRW_UT BTN_SKRIV BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
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
   DO TRANSACTION: 
      anmtyp = FALSE.
      CREATE tidut.
      IF vstorntemp.STORTYPID = 1 THEN SUBSTRING(tidut.UT,35) = "DRIFTSTÖRNING".
      ELSE SUBSTRING(tidut.UT,35) = "PLANERAT AVBROTT".
      CREATE tidut.
      FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = vstorntemp.DISTRIKTID
      USE-INDEX DISTRIKTID NO-LOCK NO-ERROR.
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = stordistemp.AVDELNINGNR NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Företag:" + SUBSTRING(avdelningtemp.AVDELNINGNAMN,1,30) 
      SUBSTRING(tidut.UT,50) = "Distrikt:" + stordistemp.NAMN.      
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "KUNDUPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Felanmälare:" + SUBSTRING(vstorntemp.VEMFEL,1,30).
      SUBSTRING(tidut.UT,50) = "Fel uppgivet:" + vstorntemp.LOSEN.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Feladress:" + vstorntemp.ADRESSVEMFEL.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Postnr:" + vstorntemp.VEMPOSTNUMMER 
      SUBSTRING(tidut.UT,50) = "Ort:" + vstorntemp.VEMORT.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Telefonnummer:" + vstorntemp.TELVEMFEL.
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "ÖVRIGA UPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.      
      CREATE tidut.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vstorntemp.ANSVARIGPERS
      NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN
      SUBSTRING(tidut.UT,1) = "Överlämnat till:" + anvandartemp.AV-NAMN.
      ELSE SUBSTRING(tidut.UT,1) = "Överlämnat till:" + vstorntemp.ANSVARIGPERS.
      SUBSTRING(tidut.UT,50) = "Återstående arbete:" + STRING(vstorntemp.MERJOBB,"Ja/Nej").
      CREATE tidut.
      IF vstorntemp.MERJOBB = TRUE THEN
      SUBSTRING(tidut.UT,1) = "Färdigdatum:".
      ELSE SUBSTRING(tidut.UT,1) = "Färdigdatum:" + STRING(vstorntemp.FDATUM,"9999/99/99").
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vstorntemp.ANVANDARE
      NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN
      SUBSTRING(tidut.UT,50) = "Upprättad av:" + anvandartemp.AV-NAMN.
      ELSE SUBSTRING(tidut.UT,50) = "Upprättad av:" + vstorntemp.ANVANDARE.
      CREATE tidut.      
      IF LENGTH(vstorntemp.VEMINFO) > 0 THEN DO:
         CREATE tidut.
         ASSIGN         
         SUBSTRING(tidut.UT,1) = "Anmärkning"
         retvar = 1
         ednum = 1
         ednum3 = LENGTH(vstorntemp.VEMINFO)
         retvar = INDEX(vstorntemp.VEMINFO,CHR(10),ednum)
         edtecken = 42
         edtext = vstorntemp.VEMINFO
         tidtext = "".
         {ANMARK2.I}
      END.
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.
      SUBSTRING(tidut.UT,35) = "STÖRNINGS UPPGIFTER".
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = vstorntemp.FRANSPANID
      USE-INDEX SPANID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Frånkopplingsställe" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(spanningsnivtemp.NAMN,1,1)) + LC(SUBSTRING(spanningsnivtemp.NAMN,2)).      
      IF vstorntemp.STORTYPID = 1 THEN DO:
         CREATE tidut.
         CREATE tidut.
         FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = vstorntemp.FELSPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Felställe" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(spanningsnivtemp.NAMN,1,1)) + LC(SUBSTRING(spanningsnivtemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      FIND FIRST brytorgantemp WHERE brytorgantemp.BRYTOID = vstorntemp.BRYTOID 
      USE-INDEX BRYTOID NO-LOCK NO-ERROR.      
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Frånkopplingsorgan" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(brytorgantemp.NAMN,1,1)) + LC(SUBSTRING(brytorgantemp.NAMN,2))
      SUBSTRING(tidut.UT,60) = "Littera :" +  vstorntemp.BRYTORGLIT.
      CREATE tidut.
      CREATE tidut.
      IF vstorntemp.STORTYPID = 1 THEN DO:
         FIND FIRST sektioneringtemp WHERE sektioneringtemp.SEKTIONERID = vstorntemp.SEKTIONERID 
         USE-INDEX SEKTIONERID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Sektionering" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(sektioneringtemp.NAMN,1,1)) + LC(SUBSTRING(sektioneringtemp.NAMN,2))
         SUBSTRING(tidut.UT,60) = "Littera :" + vstorntemp.SEKTIONLIT.
         CREATE tidut.
         CREATE tidut.         
         FIND FIRST utlosningskyddtemp WHERE utlosningskyddtemp.UTLOSID = vstorntemp.UTLOSID
         USE-INDEX UTLOSID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Utlösning" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(utlosningskyddtemp.NAMN,1,1)) + LC(SUBSTRING(utlosningskyddtemp.NAMN,2)).
         CREATE tidut.
         CREATE tidut.                
         FIND FIRST relaindikeringtemp WHERE relaindikeringtemp.RELINID = vstorntemp.RELINID
         USE-INDEX RELINID NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Reläindekering" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(relaindikeringtemp.NAMN,1,1)) + LC(SUBSTRING(relaindikeringtemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = vstorntemp.STDRIFTID 
      USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Belägenhet" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(stordriftomrtemp.NAMN,1,1)) + LC(SUBSTRING(stordriftomrtemp.NAMN,2))
      SUBSTRING(tidut.UT,60) = "Littera :" + vstorntemp.STDRIFTLIT.
      CREATE tidut.
      CREATE tidut.   
      FIND FIRST nattyptemp WHERE nattyptemp.NATTYPID = vstorntemp.NATTYPID 
      USE-INDEX NATTYPID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Nättyp" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(nattyptemp.NAMN,1,1)) + LC(SUBSTRING(nattyptemp.NAMN,2)).
      CREATE tidut.
      CREATE tidut.   
      FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = vstorntemp.ADELID
      USE-INDEX ADELID NO-LOCK NO-ERROR.
      FIND FIRST anlaggningstyptemp WHERE anlaggningstyptemp.ATYPID = 
      anlaggningsdeltemp.ATYPID USE-INDEX ATYPID NO-LOCK NO-ERROR.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Anläggning, -del" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(anlaggningstyptemp.NAMN,1,1)) + LC(SUBSTRING(anlaggningstyptemp.NAMN,2)) + 
      " - " + CAPS(SUBSTRING(anlaggningsdeltemp.NAMN,1,1)) + LC(SUBSTRING(anlaggningsdeltemp.NAMN,2)).
      IF vstorntemp.STORTYPID = 1 THEN DO:
         CREATE tidut.
         CREATE tidut.
         FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = vstorntemp.FELOID
         USE-INDEX FELOID NO-LOCK NO-ERROR.           
         FIND FIRST grundfeltyptemp WHERE grundfeltyptemp.GRUNDFELID = 
         felorsaktemp.GRUNDFELID USE-INDEX GRUNDFELID NO-LOCK NO-ERROR. 
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Felorsak" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = CAPS(SUBSTRING(grundfeltyptemp.NAMN,1,1)) + LC(SUBSTRING(grundfeltyptemp.NAMN,2)) + 
         " - " + CAPS(SUBSTRING(felorsaktemp.NAMN,1,1)) + LC(SUBSTRING(felorsaktemp.NAMN,2)).
      END.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Bortkopplad belastning"
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(vstorntemp.BORTMW) +
      " MW " + STRING(vstorntemp.BORTKW) + " kW".
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Startdatum"
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(vstorntemp.HDATUM,"9999/99/99").
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Klockan" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(vstorntemp.HKLOCKAN,"99.99").
      FIND LAST bortkoppladtemp WHERE bortkoppladtemp.STORNUMMERID = vstorntemp.STORNUMMERID
      USE-INDEX AVBROTT NO-LOCK NO-ERROR.
      IF AVAILABLE bortkoppladtemp THEN DO:         
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Slutdatum"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(bortkoppladtemp.ATERDATUM,"9999/99/99").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Klockan" 
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(bortkoppladtemp.ATERKLOCKAN,"99.99").
      END.
      ELSE DO:
         IF vstorntemp.DATUM70% NE ? THEN DO:
            CREATE tidut.
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Huvuddel tillkopplad"
            SUBSTRING(tidut.UT,23) = ":"
            SUBSTRING(tidut.UT,24) = STRING(vstorntemp.DATUM70%,"9999/99/99").
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Klockan" 
            SUBSTRING(tidut.UT,23) = ":"
            SUBSTRING(tidut.UT,24) = STRING(vstorntemp.KLOCKAN70%,"99.99").
         END.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Slutdatum"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(vstorntemp.DATUM100%,"9999/99/99").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Klockan"
         SUBSTRING(tidut.UT,23) = ":"
         SUBSTRING(tidut.UT,24) = STRING(vstorntemp.KLOCKAN100%,"99.99").
      END.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Spänningslösa kunder" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(vstorntemp.ANTALHSP) +
      " HSP " + STRING(vstorntemp.ANTALLSP) + " LSP".
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Kundavbrottstid" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(klockan60(vstorntemp.AVBROTTSTID),">>>>>9.99").
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Energibortfall kWh" 
      SUBSTRING(tidut.UT,23) = ":"
      SUBSTRING(tidut.UT,24) = STRING(klockan60(vstorntemp.BORTFALL),">>>>>>>>>9.99").
      IF AVAILABLE bortkoppladtemp THEN DO:
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         SUBSTRING(tidut.UT,35) = "SPECIFIKATION".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.         
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Datum"
         SUBSTRING(tidut.UT,13) = "Klockan"
         SUBSTRING(tidut.UT,26) = "HSP"
         SUBSTRING(tidut.UT,36) = "LSP"
         SUBSTRING(tidut.UT,44) = "Avb.tid"
         SUBSTRING(tidut.UT,55) = "Aktivitet".
         OPEN QUERY aq FOR EACH bortkoppladtemp WHERE 
         bortkoppladtemp.STORNUMMERID = vstorntemp.STORNUMMERID USE-INDEX AVBROTT NO-LOCK.
         GET FIRST aq NO-LOCK.
         DO WHILE AVAILABLE(bortkoppladtemp):
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(bortkoppladtemp.ATERDATUM,"9999/99/99") 
            SUBSTRING(tidut.UT,14) = STRING(bortkoppladtemp.ATERKLOCKAN,">99.99")
            SUBSTRING(tidut.UT,22) = STRING(bortkoppladtemp.ANTALHSP,">>>>>>9")
            SUBSTRING(tidut.UT,32) = STRING(bortkoppladtemp.ANTALLSP,">>>>>>9")
            SUBSTRING(tidut.UT,42) = STRING(klockan60(bortkoppladtemp.AVBROTTSTID),">>>>>9.99")
            SUBSTRING(tidut.UT,55) = SUBSTRING(bortkoppladtemp.AKTIVITET,1,25).
            GET NEXT aq NO-LOCK.
         END.
         CLOSE QUERY aq.
      END.
      IF LENGTH(vstorntemp.KOMMENTAR) > 0 THEN DO:
         anmtyp = TRUE.
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         SUBSTRING(tidut.UT,35) = "ANMÄRKNING".
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         ASSIGN
         retvar = 1
         ednum = 1
         ednum3 = LENGTH(vstorntemp.KOMMENTAR)
         retvar = INDEX(vstorntemp.KOMMENTAR,CHR(10),ednum)
         edtecken = 49
         edtext = vstorntemp.KOMMENTAR
         tidtext = "".
         {ANMARK2.I}
      END.
   END.   
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

