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
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE allaspann AS LOGICAL NO-UNDO.
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

DEFINE TEMP-TABLE summ_temp
   FIELD STORTYPID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER.

DEFINE TEMP-TABLE lev_temp   
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE drift_temp
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE plan_temp
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE lev_temp2   
   FIELD ADELID AS INTEGER
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE drift_temp2
   FIELD ADELID AS INTEGER
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE plan_temp2
   FIELD ADELID AS INTEGER
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE lev_temp3   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE drift_temp3   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE plan_temp3
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD SPANID AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE lev_temp4   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE drift_temp4   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE plan_temp4
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD STDRIFTID AS INTEGER
   INDEX STDRIFTID STDRIFTID ASCENDING.

DEFINE TEMP-TABLE lev_temp5   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE drift_temp5   
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE plan_temp5
   FIELD BORTFALL AS DECIMAL
   FIELD EFFEKT AS INTEGER
   FIELD ANTAL AS INTEGER.

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
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_AVS 

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 25.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 27.75 COL 95.5
     BTN_AVS AT ROW 27.75 COL 111
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
         TITLE              = "Översikt över fördelningen av planerade avbrott och driftstörningar"
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
   IF uttyp = 3 THEN RUN SKRIVVAL.W (INPUT TRUE).       
   ELSE RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      IF uttyp = 3 THEN RUN EKLOGL.P.    
      ELSE RUN EKLOGS.P.
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
   IF uttyp = 3 THEN sidlangd = Guru.GlobalaVariabler:globsidl - 3.
   ELSE sidlangd = Guru.GlobalaVariabler:globsids - 3.
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
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.
                 
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
    ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
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
  ENABLE BTN_SKRIV BTN_AVS 
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
            EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
            EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
            EMPTY TEMP-TABLE plan_temp3 NO-ERROR.             
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
               EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
               EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
               EMPTY TEMP-TABLE plan_temp3 NO-ERROR.                
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
            EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
            EMPTY TEMP-TABLE lev_temp3  NO-ERROR. 
            EMPTY TEMP-TABLE plan_temp3 NO-ERROR.             
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
               EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
               EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
               EMPTY TEMP-TABLE plan_temp3 NO-ERROR.                
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
         
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   IF uttyp = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,26) = "D R I F T S T Ö R N I N G A R".      
   END.
   ELSE IF uttyp = 2 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,26) = "P L A N E R A D E  A V B R.". 
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,26) = "D R I F T S T Ö R N I N G A R" 
      SUBSTRING(tidut.UT,66) = "P L A N E R A D E  A V B R."
      SUBSTRING(tidut.UT,97) = "T O T A L T".
   END.
      
   IF uttyp = 1 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "Antal:"
      SUBSTRING(tidut.UT,35) = "Varav"
      SUBSTRING(tidut.UT,44) = "Bortk."
      SUBSTRING(tidut.UT,55) = "Energi".
   END.
   ELSE IF uttyp = 2 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "Antal:"
      SUBSTRING(tidut.UT,35) = "Bortk."
      SUBSTRING(tidut.UT,46) = "Energi".
   END.
   ELSE DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "Antal:"
      SUBSTRING(tidut.UT,35) = "Varav"
      SUBSTRING(tidut.UT,44) = "Bortk."
      SUBSTRING(tidut.UT,55) = "Energi"
      SUBSTRING(tidut.UT,66) = "Antal:"
      SUBSTRING(tidut.UT,75) = "Bortk."
      SUBSTRING(tidut.UT,86) = "Energi"
      SUBSTRING(tidut.UT,97) = "Antal:"
      SUBSTRING(tidut.UT,106) = "Bortk."
      SUBSTRING(tidut.UT,117) = "Energi".
   END.
   IF uttyp = 1 THEN DO:   
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "lev.avbr"
      SUBSTRING(tidut.UT,35) = "dr.avbr"
      SUBSTRING(tidut.UT,44) = "effekt"
      SUBSTRING(tidut.UT,55) = "bortf(kWh)".
   END.
   ELSE IF uttyp = 2 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "lev.avbr"
      SUBSTRING(tidut.UT,35) = "effekt"
      SUBSTRING(tidut.UT,46) = "bortf(kWh)".
   END.
   ELSE DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "lev.avbr"
      SUBSTRING(tidut.UT,35) = "dr.avbr"
      SUBSTRING(tidut.UT,44) = "effekt"
      SUBSTRING(tidut.UT,55) = "bortf(kWh)"
      SUBSTRING(tidut.UT,66) = "lev.avbr"
      SUBSTRING(tidut.UT,75) = "effekt"
      SUBSTRING(tidut.UT,86) = "bortf(kWh)"
      SUBSTRING(tidut.UT,97) = "lev.avbr"
      SUBSTRING(tidut.UT,106) = "effekt"
      SUBSTRING(tidut.UT,117) = "bortf(kWh)".
   END.   
   IF uttyp = 1 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "-------"
      SUBSTRING(tidut.UT,44) = "---------"
      SUBSTRING(tidut.UT,55) = "---------".
   END.
   ELSE IF uttyp = 2 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "---------"
      SUBSTRING(tidut.UT,46) = "---------".
   END.
   ELSE DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "-------"
      SUBSTRING(tidut.UT,44) = "---------"
      SUBSTRING(tidut.UT,55) = "---------"
      SUBSTRING(tidut.UT,66) = "-------"
      SUBSTRING(tidut.UT,75) = "---------"
      SUBSTRING(tidut.UT,86) = "---------"
      SUBSTRING(tidut.UT,97) = "-------"
      SUBSTRING(tidut.UT,106) = "---------"
      SUBSTRING(tidut.UT,117) = "---------".
   END.   
   tant = tant + 5.
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
      SUBSTRING(tidut.UT,1) = "Översikt över fördel. av planerade avbrott och drifts.".      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutallt_UI WINDOW-2 
PROCEDURE slutallt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST drift_temp4 NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp4 THEN DO:
      FOR EACH drift_temp4:      
         ACCUMULATE drift_temp4.ANTAL (TOTAL).
         ACCUMULATE drift_temp4.EFFEKT (TOTAL). 
         ACCUMULATE drift_temp4.BORTFALL (TOTAL).                                     
      END.
      CREATE drift_temp5.
      ASSIGN   
      drift_temp5.ANTAL = (ACCUM TOTAL drift_temp4.ANTAL)
      drift_temp5.EFFEKT = (ACCUM TOTAL drift_temp4.EFFEKT) 
      drift_temp5.BORTFALL = (ACCUM TOTAL drift_temp4.BORTFALL).
   END.

   FIND FIRST lev_temp4 NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp4 THEN DO:
      FOR EACH lev_temp4:      
         ACCUMULATE lev_temp4.ANTAL (TOTAL).
         ACCUMULATE lev_temp4.EFFEKT (TOTAL). 
         ACCUMULATE lev_temp4.BORTFALL (TOTAL).
      END.
      CREATE lev_temp5.
      ASSIGN   
      lev_temp5.ANTAL = (ACCUM TOTAL lev_temp4.ANTAL)
      lev_temp5.EFFEKT = (ACCUM TOTAL lev_temp4.EFFEKT) 
      lev_temp5.BORTFALL = (ACCUM TOTAL lev_temp4.BORTFALL).
   END.

   FIND FIRST plan_temp4 NO-LOCK NO-ERROR.
   IF AVAILABLE plan_temp4 THEN DO:
      FOR EACH plan_temp4:      
         ACCUMULATE plan_temp4.ANTAL (TOTAL).
         ACCUMULATE plan_temp4.EFFEKT (TOTAL). 
         ACCUMULATE plan_temp4.BORTFALL (TOTAL).                                     
      END.
      CREATE plan_temp5.
      ASSIGN       
      plan_temp5.ANTAL = (ACCUM TOTAL plan_temp4.ANTAL)
      plan_temp5.EFFEKT = (ACCUM TOTAL plan_temp4.EFFEKT) 
      plan_temp5.BORTFALL = (ACCUM TOTAL plan_temp4.BORTFALL).
   END.

   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "TOTALSUMMA".   
   FIND FIRST lev_temp5 NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp5 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,26) = STRING(lev_temp5.ANTAL,">>>>>>9")
      SUBSTRING(tidut.UT,44) = STRING(lev_temp5.EFFEKT,">>>>>>>>9")
      SUBSTRING(tidut.UT,55) = STRING(lev_temp5.BORTFALL,">>>>>>>>9").
      FIND FIRST drift_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp5 THEN DO:
         SUBSTRING(tidut.UT,35) = STRING(drift_temp5.ANTAL,">>>>>>9").
      END.
      FIND FIRST plan_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp5 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,66) = STRING(plan_temp5.ANTAL,">>>>>>9")
         SUBSTRING(tidut.UT,75) = STRING(plan_temp5.EFFEKT,">>>>>>>>9")
         SUBSTRING(tidut.UT,86) = STRING(plan_temp5.BORTFALL,">>>>>>>>9")
         SUBSTRING(tidut.UT,97) = STRING(lev_temp5.ANTAL + plan_temp5.ANTAL,">>>>>>9")
         SUBSTRING(tidut.UT,106) = STRING(lev_temp5.EFFEKT + plan_temp5.EFFEKT,">>>>>>>>9")
         SUBSTRING(tidut.UT,117) = STRING(lev_temp5.BORTFALL + plan_temp5.BORTFALL,">>>>>>>>9").
      END.
      ELSE DO:
         IF uttyp = 3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,97) = STRING(lev_temp5.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(lev_temp5.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,117) = STRING(lev_temp5.BORTFALL,">>>>>>>>9").
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST plan_temp5 NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp5 THEN DO:
         IF uttyp = 3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,66) = STRING(plan_temp5.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,75) = STRING(plan_temp5.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,86) = STRING(plan_temp5.BORTFALL,">>>>>>>>9")
            SUBSTRING(tidut.UT,97) = STRING(plan_temp5.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(plan_temp5.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,117) = STRING(plan_temp5.BORTFALL,">>>>>>>>9").
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,26) = STRING(plan_temp5.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,35) = STRING(plan_temp5.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,46) = STRING(plan_temp5.BORTFALL,">>>>>>>>9").
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
   FIND FIRST drift_temp3 NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp3 THEN DO:
      FOR EACH drift_temp3:      
         ACCUMULATE drift_temp3.ANTAL (TOTAL).
         ACCUMULATE drift_temp3.EFFEKT (TOTAL). 
         ACCUMULATE drift_temp3.BORTFALL (TOTAL).                                     
      END.
      CREATE drift_temp4.
      ASSIGN
      drift_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      drift_temp4.ANTAL = (ACCUM TOTAL drift_temp3.ANTAL)
      drift_temp4.EFFEKT = (ACCUM TOTAL drift_temp3.EFFEKT) 
      drift_temp4.BORTFALL = (ACCUM TOTAL drift_temp3.BORTFALL).
   END.

   FIND FIRST lev_temp3 NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp3 THEN DO:
      FOR EACH lev_temp3:      
         ACCUMULATE lev_temp3.ANTAL (TOTAL).
         ACCUMULATE lev_temp3.EFFEKT (TOTAL). 
         ACCUMULATE lev_temp3.BORTFALL (TOTAL).
      END.
      CREATE lev_temp4.
      ASSIGN   
      lev_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      lev_temp4.ANTAL = (ACCUM TOTAL lev_temp3.ANTAL)
      lev_temp4.EFFEKT = (ACCUM TOTAL lev_temp3.EFFEKT) 
      lev_temp4.BORTFALL = (ACCUM TOTAL lev_temp3.BORTFALL).
   END.

   FIND FIRST plan_temp3 NO-LOCK NO-ERROR.
   IF AVAILABLE plan_temp3 THEN DO:
      FOR EACH plan_temp3:      
         ACCUMULATE plan_temp3.ANTAL (TOTAL).
         ACCUMULATE plan_temp3.EFFEKT (TOTAL). 
         ACCUMULATE plan_temp3.BORTFALL (TOTAL).                                     
      END.
      CREATE plan_temp4.
      ASSIGN       
      plan_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      plan_temp4.ANTAL = (ACCUM TOTAL plan_temp3.ANTAL)
      plan_temp4.EFFEKT = (ACCUM TOTAL plan_temp3.EFFEKT) 
      plan_temp4.BORTFALL = (ACCUM TOTAL plan_temp3.BORTFALL).
   END.

   IF endsum = FALSE THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "SA: " + stordriftomrtemp.NAMN.   
   END.
   FIND FIRST lev_temp4 WHERE lev_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
   USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp4 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,26) = STRING(lev_temp4.ANTAL,">>>>>>9")
      SUBSTRING(tidut.UT,44) = STRING(lev_temp4.EFFEKT,">>>>>>>>9")
      SUBSTRING(tidut.UT,55) = STRING(lev_temp4.BORTFALL,">>>>>>>>9").
      FIND FIRST drift_temp4 WHERE drift_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp4 THEN DO:
         SUBSTRING(tidut.UT,35) = STRING(drift_temp4.ANTAL,">>>>>>9").
      END.
      FIND FIRST plan_temp4 WHERE plan_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp4 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,66) = STRING(plan_temp4.ANTAL,">>>>>>9")
         SUBSTRING(tidut.UT,75) = STRING(plan_temp4.EFFEKT,">>>>>>>>9")
         SUBSTRING(tidut.UT,86) = STRING(plan_temp4.BORTFALL,">>>>>>>>9")
         SUBSTRING(tidut.UT,97) = STRING(lev_temp4.ANTAL + plan_temp4.ANTAL,">>>>>>9")
         SUBSTRING(tidut.UT,106) = STRING(lev_temp4.EFFEKT + plan_temp4.EFFEKT,">>>>>>>>9")
         SUBSTRING(tidut.UT,117) = STRING(lev_temp4.BORTFALL + plan_temp4.BORTFALL,">>>>>>>>9").
      END.
      ELSE DO:
         IF uttyp = 3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,97) = STRING(lev_temp4.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(lev_temp4.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,117) = STRING(lev_temp4.BORTFALL,">>>>>>>>9").
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST plan_temp4 WHERE plan_temp4.STDRIFTID = stordriftomrtemp.STDRIFTID
      USE-INDEX STDRIFTID NO-LOCK NO-ERROR.
      IF AVAILABLE plan_temp4 THEN DO:
         IF uttyp = 3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,66) = STRING(plan_temp4.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,75) = STRING(plan_temp4.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,86) = STRING(plan_temp4.BORTFALL,">>>>>>>>9")
            SUBSTRING(tidut.UT,97) = STRING(plan_temp4.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(plan_temp4.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,117) = STRING(plan_temp4.BORTFALL,">>>>>>>>9").
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,26) = STRING(plan_temp4.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,35) = STRING(plan_temp4.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,46) = STRING(plan_temp4.BORTFALL,">>>>>>>>9").
         END.
      END.
   END.   
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
      SUBSTRING(tidut.UT,1) = spann_temp2.NAMN.
      CREATE tidut.
      kant = 2.
      OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(anlaggningsdeltemp):
         FIND FIRST lev_temp2 WHERE lev_temp2.ADELID = anlaggningsdeltemp.ADELID
         USE-INDEX ADELID NO-LOCK NO-ERROR.
         IF AVAILABLE lev_temp2 THEN DO:
            CREATE tidut.
            kant = kant + 1.
            SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20).
            ASSIGN
            SUBSTRING(tidut.UT,26) = STRING(lev_temp2.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,44) = STRING(lev_temp2.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,55) = STRING(lev_temp2.BORTFALL,">>>>>>>>9").
            FIND FIRST drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE drift_temp2 THEN DO:
               SUBSTRING(tidut.UT,35) = STRING(drift_temp2.ANTAL,">>>>>>9").
            END.
            FIND FIRST plan_temp2 WHERE plan_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE plan_temp2 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,66) = STRING(plan_temp2.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,75) = STRING(plan_temp2.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,86) = STRING(plan_temp2.BORTFALL,">>>>>>>>9")
               SUBSTRING(tidut.UT,97) = STRING(lev_temp2.ANTAL + plan_temp2.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,106) = STRING(lev_temp2.EFFEKT + plan_temp2.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,117) = STRING(lev_temp2.BORTFALL + plan_temp2.BORTFALL,">>>>>>>>9").
            END.
            ELSE DO:
               IF uttyp = 3 THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,97) = STRING(lev_temp2.ANTAL,">>>>>>9")
                  SUBSTRING(tidut.UT,106) = STRING(lev_temp2.EFFEKT,">>>>>>>>9")
                  SUBSTRING(tidut.UT,117) = STRING(lev_temp2.BORTFALL,">>>>>>>>9").
               END.
            END.
         END.
         ELSE DO:
            FIND FIRST plan_temp2 WHERE plan_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            IF AVAILABLE plan_temp2 THEN DO:
               CREATE tidut.
               kant = kant + 1.
               SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20).
               IF uttyp = 3 THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,66) = STRING(plan_temp2.ANTAL,">>>>>>9")
                  SUBSTRING(tidut.UT,75) = STRING(plan_temp2.EFFEKT,">>>>>>>>9")
                  SUBSTRING(tidut.UT,86) = STRING(plan_temp2.BORTFALL,">>>>>>>>9")
                  SUBSTRING(tidut.UT,97) = STRING(plan_temp2.ANTAL,">>>>>>9")
                  SUBSTRING(tidut.UT,106) = STRING(plan_temp2.EFFEKT,">>>>>>>>9")
                  SUBSTRING(tidut.UT,117) = STRING(plan_temp2.BORTFALL,">>>>>>>>9").
               END.
               ELSE DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,26) = STRING(plan_temp2.ANTAL,">>>>>>9")
                  SUBSTRING(tidut.UT,35) = STRING(plan_temp2.EFFEKT,">>>>>>>>9")
                  SUBSTRING(tidut.UT,46) = STRING(plan_temp2.BORTFALL,">>>>>>>>9").
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
      SUBSTRING(tidut.UT,1) = "SA: " + spann_temp2.NAMN.   
      FIND FIRST lev_temp3 WHERE lev_temp3.SPANID = spann_temp2.SPANID
      USE-INDEX SPANID NO-LOCK NO-ERROR.
      IF AVAILABLE lev_temp3 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,26) = STRING(lev_temp3.ANTAL,">>>>>>9")
         SUBSTRING(tidut.UT,44) = STRING(lev_temp3.EFFEKT,">>>>>>>>9")
         SUBSTRING(tidut.UT,55) = STRING(lev_temp3.BORTFALL,">>>>>>>>9").
         FIND FIRST drift_temp3 WHERE drift_temp3.SPANID = spann_temp2.SPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp3 THEN DO:
            SUBSTRING(tidut.UT,35) = STRING(drift_temp3.ANTAL,">>>>>>9").
         END.
         FIND FIRST plan_temp3 WHERE plan_temp3.SPANID = spann_temp2.SPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         IF AVAILABLE plan_temp3 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,66) = STRING(plan_temp3.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,75) = STRING(plan_temp3.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,86) = STRING(plan_temp3.BORTFALL,">>>>>>>>9")
            SUBSTRING(tidut.UT,97) = STRING(lev_temp3.ANTAL + plan_temp3.ANTAL,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(lev_temp3.EFFEKT + plan_temp3.EFFEKT,">>>>>>>>9")
            SUBSTRING(tidut.UT,117) = STRING(lev_temp3.BORTFALL + plan_temp3.BORTFALL,">>>>>>>>9").
         END.
         ELSE DO:
            IF uttyp = 3 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,97) = STRING(lev_temp3.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,106) = STRING(lev_temp3.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,117) = STRING(lev_temp3.BORTFALL,">>>>>>>>9").
            END.
         END.
      END.
      ELSE DO:
         FIND FIRST plan_temp3 WHERE plan_temp3.SPANID = spann_temp2.SPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         IF AVAILABLE plan_temp3 THEN DO:            
            IF uttyp = 3 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,66) = STRING(plan_temp3.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,75) = STRING(plan_temp3.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,86) = STRING(plan_temp3.BORTFALL,">>>>>>>>9")
               SUBSTRING(tidut.UT,97) = STRING(plan_temp3.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,106) = STRING(plan_temp3.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,117) = STRING(plan_temp3.BORTFALL,">>>>>>>>9").
            END.
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,26) = STRING(plan_temp3.ANTAL,">>>>>>9")
               SUBSTRING(tidut.UT,35) = STRING(plan_temp3.EFFEKT,">>>>>>>>9")
               SUBSTRING(tidut.UT,46) = STRING(plan_temp3.BORTFALL,">>>>>>>>9").
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
   FOR EACH summ_temp:
      IF summ_temp.STORTYPID = 1 THEN DO:
         IF summ_temp.AVBROTT > 0.05 THEN DO:
            CREATE drift_temp.
            ASSIGN
            drift_temp.ADELID = summ_temp.ADELID
            drift_temp.EFFEKT = summ_temp.EFFEKT
            drift_temp.BORTFALL = summ_temp.BORTFALL
            drift_temp.ANTAL = 1.
         END.
         CREATE lev_temp.
         ASSIGN
         lev_temp.ADELID = summ_temp.ADELID
         lev_temp.EFFEKT = summ_temp.EFFEKT
         lev_temp.BORTFALL = summ_temp.BORTFALL
         lev_temp.ANTAL = 1.
      END.
      ELSE DO:
         CREATE plan_temp.
         ASSIGN
         plan_temp.ADELID = summ_temp.ADELID
         plan_temp.EFFEKT = summ_temp.EFFEKT
         plan_temp.BORTFALL = summ_temp.BORTFALL
         plan_temp.ANTAL = 1.
      END.
   END.
   FOR EACH drift_temp BREAK BY drift_temp.ADELID:      
      ACCUMULATE drift_temp.ANTAL (TOTAL BY drift_temp.ADELID).
      ACCUMULATE drift_temp.EFFEKT (TOTAL BY drift_temp.ADELID). 
      ACCUMULATE drift_temp.BORTFALL (TOTAL BY drift_temp.ADELID).
      IF LAST-OF(drift_temp.ADELID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                 
         drift_temp2.ADELID = drift_temp.ADELID
         drift_temp2.ANTAL = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.ANTAL)
         drift_temp2.EFFEKT = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.EFFEKT) 
         drift_temp2.BORTFALL = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.BORTFALL).                       
      END.     
   END.
   FOR EACH lev_temp BREAK BY lev_temp.ADELID:      
      ACCUMULATE lev_temp.ANTAL (TOTAL BY lev_temp.ADELID).
      ACCUMULATE lev_temp.EFFEKT (TOTAL BY lev_temp.ADELID). 
      ACCUMULATE lev_temp.BORTFALL (TOTAL BY lev_temp.ADELID).
      IF LAST-OF(lev_temp.ADELID) THEN DO TRANSACTION:
         CREATE lev_temp2.
         ASSIGN                                 
         lev_temp2.ADELID = lev_temp.ADELID
         lev_temp2.ANTAL = (ACCUM TOTAL BY lev_temp.ADELID lev_temp.ANTAL)
         lev_temp2.EFFEKT = (ACCUM TOTAL BY lev_temp.ADELID lev_temp.EFFEKT) 
         lev_temp2.BORTFALL = (ACCUM TOTAL BY lev_temp.ADELID lev_temp.BORTFALL).                       
      END.     
   END.
   FOR EACH plan_temp BREAK BY plan_temp.ADELID:      
      ACCUMULATE plan_temp.ANTAL (TOTAL BY plan_temp.ADELID).
      ACCUMULATE plan_temp.EFFEKT (TOTAL BY plan_temp.ADELID). 
      ACCUMULATE plan_temp.BORTFALL (TOTAL BY plan_temp.ADELID).
      IF LAST-OF(plan_temp.ADELID) THEN DO TRANSACTION:
         CREATE plan_temp2.
         ASSIGN                                 
         plan_temp2.ADELID = plan_temp.ADELID
         plan_temp2.ANTAL = (ACCUM TOTAL BY plan_temp.ADELID plan_temp.ANTAL)
         plan_temp2.EFFEKT = (ACCUM TOTAL BY plan_temp.ADELID plan_temp.EFFEKT) 
         plan_temp2.BORTFALL = (ACCUM TOTAL BY plan_temp.ADELID plan_temp.BORTFALL).                       
      END.     
   END.

   FIND FIRST drift_temp2 NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp2 THEN DO:
      FOR EACH drift_temp2:      
         ACCUMULATE drift_temp2.ANTAL (TOTAL).
         ACCUMULATE drift_temp2.EFFEKT (TOTAL). 
         ACCUMULATE drift_temp2.BORTFALL (TOTAL).                                     
      END.
      CREATE drift_temp3.
      ASSIGN        
      drift_temp3.SPANID = spann_temp2.SPANID   
      drift_temp3.ANTAL = (ACCUM TOTAL drift_temp2.ANTAL)
      drift_temp3.EFFEKT = (ACCUM TOTAL drift_temp2.EFFEKT) 
      drift_temp3.BORTFALL = (ACCUM TOTAL drift_temp2.BORTFALL).
   END.

   FIND FIRST lev_temp2 NO-LOCK NO-ERROR.
   IF AVAILABLE lev_temp2 THEN DO:
      FOR EACH lev_temp2:      
         ACCUMULATE lev_temp2.ANTAL (TOTAL).
         ACCUMULATE lev_temp2.EFFEKT (TOTAL). 
         ACCUMULATE lev_temp2.BORTFALL (TOTAL).
      END.
      CREATE lev_temp3.
      ASSIGN   
      lev_temp3.SPANID = spann_temp2.SPANID   
      lev_temp3.ANTAL = (ACCUM TOTAL lev_temp2.ANTAL)
      lev_temp3.EFFEKT = (ACCUM TOTAL lev_temp2.EFFEKT) 
      lev_temp3.BORTFALL = (ACCUM TOTAL lev_temp2.BORTFALL).
   END.

   FIND FIRST plan_temp2 NO-LOCK NO-ERROR.
   IF AVAILABLE plan_temp2 THEN DO:
      FOR EACH plan_temp2:      
         ACCUMULATE plan_temp2.ANTAL (TOTAL).
         ACCUMULATE plan_temp2.EFFEKT (TOTAL). 
         ACCUMULATE plan_temp2.BORTFALL (TOTAL).                                     
      END.
      CREATE plan_temp3.
      ASSIGN    
      plan_temp3.SPANID = spann_temp2.SPANID   
      plan_temp3.ANTAL = (ACCUM TOTAL plan_temp2.ANTAL)
      plan_temp3.EFFEKT = (ACCUM TOTAL plan_temp2.EFFEKT) 
      plan_temp3.BORTFALL = (ACCUM TOTAL plan_temp2.BORTFALL).
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
      RUN STORSUM2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
      INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).      
   END.
   ELSE DO:
      RUN STORSUM2.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
      INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).
   END.    
   /*
   /*slå ihop spänning*/
   IF spannvar = 21 THEN DO:
      spannvar = 20.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN STORSUM2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
         INPUT avdatum, INPUT period, 
         INPUT uttyp, INPUT driftvar, INPUT alla, 
         INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).      
      END.
      ELSE DO:
         RUN STORSUM2.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, 
         INPUT avdatum, INPUT period, 
         INPUT uttyp, INPUT driftvar, INPUT alla, 
         INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).
      END.
   END.
   */
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
   IF uttyp = 1 THEN DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "======="
      SUBSTRING(tidut.UT,35) = "======="
      SUBSTRING(tidut.UT,44) = "========="
      SUBSTRING(tidut.UT,55) = "=========".
   END.
   ELSE IF uttyp = 2 THEN DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "======="
      SUBSTRING(tidut.UT,35) = "========="
      SUBSTRING(tidut.UT,46) = "=========".
   END.
   ELSE DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "======="
      SUBSTRING(tidut.UT,35) = "======="
      SUBSTRING(tidut.UT,44) = "========="
      SUBSTRING(tidut.UT,55) = "========="
      SUBSTRING(tidut.UT,66) = "======="
      SUBSTRING(tidut.UT,75) = "========="
      SUBSTRING(tidut.UT,86) = "========="
      SUBSTRING(tidut.UT,97) = "======="
      SUBSTRING(tidut.UT,106) = "========="
      SUBSTRING(tidut.UT,117) = "=========".
   END.         
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
   IF uttyp = 1 THEN DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "-------"
      SUBSTRING(tidut.UT,44) = "---------"
      SUBSTRING(tidut.UT,55) = "---------".
   END.
   ELSE IF uttyp = 2 THEN DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "---------"
      SUBSTRING(tidut.UT,46) = "---------".
   END.
   ELSE DO:
      CREATE tidut.                        
      ASSIGN
      SUBSTRING(tidut.UT,26) = "-------"
      SUBSTRING(tidut.UT,35) = "-------"
      SUBSTRING(tidut.UT,44) = "---------"
      SUBSTRING(tidut.UT,55) = "---------"
      SUBSTRING(tidut.UT,66) = "-------"
      SUBSTRING(tidut.UT,75) = "---------"
      SUBSTRING(tidut.UT,86) = "---------"
      SUBSTRING(tidut.UT,97) = "-------"
      SUBSTRING(tidut.UT,106) = "---------"
      SUBSTRING(tidut.UT,117) = "---------".
   END.         
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
   SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
   CREATE tidut.
   tant = tant + 2.
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE plan_temp3 NO-ERROR.    
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
      EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
      EMPTY TEMP-TABLE lev_temp3 NO-ERROR. 
      EMPTY TEMP-TABLE plan_temp3 NO-ERROR.       
      CREATE tidut.
      tant = tant + 1.
      SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN.
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

