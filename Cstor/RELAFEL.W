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
{EXECLIN.I}
DEFINE VARIABLE i AS INTEGER NO-UNDO.
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
DEFINE VARIABLE adelvar AS INTEGER NO-UNDO.
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

DEFINE TEMP-TABLE slut_allt
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER.

DEFINE TEMP-TABLE slut_allt2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut_allt3
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut2_allt2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE leddata_temp
   FIELD SPANID AS INTEGER   
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE leddata_temp2
   FIELD SPANID AS INTEGER   
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE summ_temp
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER.

DEFINE TEMP-TABLE drift_temp
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER.

DEFINE TEMP-TABLE drift_temp2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE drift_temp3   
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX SPANID SPANID ASCENDING.

DEFINE TEMP-TABLE drift2_temp2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD UTLOSID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX SPANID SPANID ASCENDING.

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
     BTN_SKRIV AT ROW 27.96 COL 81.5
     FBTN_EXCEL AT ROW 27.96 COL 96.5
     BTN_AVS AT ROW 27.96 COL 111
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
         TITLE              = "Driftstörning fördelade på utlösande relä/felyttring"
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
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-TIDS         = 1000.

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
   
   RUN totexcel_UI.
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
   EMPTY TEMP-TABLE spann_temp2 NO-ERROR.    
   OPEN QUERY sysq FOR EACH inlastabtemp WHERE inlastabtemp.INKOD = "D" NO-LOCK.
   GET FIRST sysq NO-LOCK.
   DO WHILE AVAILABLE(inlastabtemp):
      IF inlastabtemp.INKODPOSCH NE "" THEN DO:
         IF inlastabtemp.INKODTYP = "2" THEN DO:
            FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.INKODID = 
            inlastabtemp.INKODID NO-LOCK NO-ERROR.
            IF AVAILABLE spanningsnivtemp THEN DO:
               CREATE spann_temp2.
               ASSIGN
               spann_temp2.NAMN = spanningsnivtemp.NAMN
               spann_temp2.SPANID = spanningsnivtemp.SPANID.               
            END.
         END.         
      END.
      GET NEXT sysq NO-LOCK.         
   END.   
   CLOSE QUERY sysq.   
str="===============================================================================================================================".
   ASSIGN
   tant = 0
   kant = 0
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
            driftvar = stordriftomrtemp.STDRIFTID.
            CREATE tidut.
            tant = tant + 1.          
            OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
            GET FIRST aq NO-LOCK.
            DO WHILE AVAILABLE(anlaggningsdeltemp):
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = anlaggningsdeltemp.NAMN
               utrec = RECID(tidut).
               CREATE tidut.               
               kant =  2.
               adelvar = anlaggningsdeltemp.ADELID.
               EMPTY TEMP-TABLE leddata_temp NO-ERROR.                
               OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
               AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
               GET FIRST kq NO-LOCK.
               DO WHILE AVAILABLE(stordistemp):
                  ASSIGN
                  distvar = stordistemp.DISTRIKTID.                      
                  IF Guru.Konstanter:appcon THEN DO:                           
                     RUN LEDDATA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
                     INPUT bdatum, INPUT alla, 
                     INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).      
                  END.
                  ELSE DO:
                     RUN LEDDATA.P 
                     (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
                     INPUT bdatum, INPUT alla, 
                     INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).
                  END.
                  GET NEXT kq NO-LOCK.
               END.
               CLOSE QUERY kq.
               EMPTY TEMP-TABLE summ_temp NO-ERROR.                
               FOR EACH spann_temp2:               
                  ASSIGN
                  spannvar = spann_temp2.SPANID               
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
               END.
               RUN storning3_UI.
               RUN bryt_UI.            
               GET NEXT aq NO-LOCK.
            END.         
            CLOSE QUERY aq.
            RUN str_UI.
            RUN slutomr_UI.         
         END.
         ELSE DO:
            FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = 2 NO-LOCK NO-ERROR.         
            driftvar = stordriftomrtemp.STDRIFTID.
            CREATE tidut.               
            tant = tant + 1.          
            OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
            GET FIRST aq NO-LOCK.
            DO WHILE AVAILABLE(anlaggningsdeltemp):
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = anlaggningsdeltemp.NAMN
               utrec = RECID(tidut).
               CREATE tidut.               
               kant = 2.
               adelvar = anlaggningsdeltemp.ADELID.
               EMPTY TEMP-TABLE leddata_temp NO-ERROR.                
               FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
                  ASSIGN
                  distvar = omr_temp.DISTRIKTID.                      
                  IF Guru.Konstanter:appcon THEN DO:                           
                     RUN LEDDATA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
                     INPUT bdatum, INPUT alla, 
                     INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).      
                  END.
                  ELSE DO:
                     RUN LEDDATA.P 
                     (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
                     INPUT bdatum, INPUT alla, 
                     INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).
                  END.
               END.
               EMPTY TEMP-TABLE summ_temp NO-ERROR.                
               FOR EACH spann_temp2:            
                  ASSIGN
                  spannvar = spann_temp2.SPANID              
                  endsum = FALSE.
                  FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
                     ASSIGN
                     distvar = omr_temp.DISTRIKTID.       
                     RUN storning_UI.                           
                  END.
               END.
               RUN storning3_UI. 
               RUN bryt_UI.
               GET NEXT aq NO-LOCK.
            END.         
            CLOSE QUERY aq.
            RUN str_UI.
            RUN slutomr_UI.          
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

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,26) = "Antal"
   SUBSTRING(tidut.UT,35) = "**** Utlösning ****  ************ Utlösande relä / Felyttring ***********   **** Totalt *****".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,26) = "km el."
   SUBSTRING(tidut.UT,35) = "Selektiv"
   SUBSTRING(tidut.UT,45) = "Oselektiv"
   SUBSTRING(tidut.UT,56) = "Över"
   SUBSTRING(tidut.UT,65) = "Jordf."
   SUBSTRING(tidut.UT,74) = "Nollpkt-"
   SUBSTRING(tidut.UT,83) = "Diff-"
   SUBSTRING(tidut.UT,92) = "Vakter,"
   SUBSTRING(tidut.UT,101) = "Övriga"
   SUBSTRING(tidut.UT,111) = "Antal"
   SUBSTRING(tidut.UT,120) = "Relativ".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,26) = "st"
   SUBSTRING(tidut.UT,35) = ""
   SUBSTRING(tidut.UT,45) = ""
   SUBSTRING(tidut.UT,56) = "ström"
   SUBSTRING(tidut.UT,65) = "ström"
   SUBSTRING(tidut.UT,74) = "spänning"
   SUBSTRING(tidut.UT,83) = "skydd"
   SUBSTRING(tidut.UT,92) = "utlösn."
   SUBSTRING(tidut.UT,101) = "faktorer"
   SUBSTRING(tidut.UT,111) = "fel"
   SUBSTRING(tidut.UT,120) = "felfrekv.".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,26) = "-------"
   SUBSTRING(tidut.UT,36) = "-------"
   SUBSTRING(tidut.UT,46) = "-------"
   SUBSTRING(tidut.UT,56) = "-------"
   SUBSTRING(tidut.UT,65) = "-------"
   SUBSTRING(tidut.UT,74) = "-------"
   SUBSTRING(tidut.UT,83) = "-------"
   SUBSTRING(tidut.UT,92) = "-------"
   SUBSTRING(tidut.UT,101) = "-------"
   SUBSTRING(tidut.UT,111) = "-------"
   SUBSTRING(tidut.UT,120) = "-------".   
   tant = tant + 6.
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
      SUBSTRING(tidut.UT,1) = "Driftstörning fördelade på utlösande relä / felyttring".      
      SUBSTRING(tidut.UT,60) = STRING(TODAY).
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "(inom eget nät N1 = 2)".
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Urval:".
      CREATE tidut.      
      SUBSTRING(tidut.UT,1) = "Driftstörningar".      
      CREATE tidut.
      IF period = 1 THEN
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(YEAR(bdatum)).
      ELSE
      SUBSTRING(tidut.UT,1) = "Period:" + STRING(bdatum,"9999/99/99") + " - " + STRING(avdatum,"9999/99/99").
      CREATE tidut.
      tant = tant + 7.
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
      CREATE tidut.
      CREATE tidut.
      tant = tant + 2.
      SUBSTRING(tidut.UT,1) = "Alla spänningsnivåer".
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KUNDSTOR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT TABLE omr_temp, INPUT alla, INPUT bdatum,
         INPUT TABLE spann_temp2, OUTPUT antaletkunder, INPUT 2).      
      END.
      ELSE DO:
         RUN KUNDSTOR.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT TABLE omr_temp, INPUT alla, INPUT bdatum,
         INPUT TABLE spann_temp2, OUTPUT antaletkunder, INPUT 2).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutomr_UI WINDOW-2 
PROCEDURE slutomr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE slut_allt2 NO-ERROR. 
   EMPTY TEMP-TABLE slut_allt3 NO-ERROR. 
   EMPTY TEMP-TABLE slut2_allt2 NO-ERROR.    
   FOR EACH slut_allt BREAK BY slut_allt.FELOID:      
      ACCUMULATE slut_allt.SPANID (COUNT BY slut_allt.FELOID).      
      IF LAST-OF(slut_allt.FELOID) THEN DO TRANSACTION:
         CREATE slut_allt2.
         ASSIGN                                          
         slut_allt2.FELOID = slut_allt.FELOID.
         slut_allt2.ANTAL = (ACCUM COUNT BY slut_allt.FELOID slut_allt.SPANID).         
      END.     
   END.
   FIND FIRST slut_allt NO-LOCK NO-ERROR.
   IF AVAILABLE slut_allt THEN DO:
      FOR EACH slut_allt:      
         ACCUMULATE slut_allt.SPANID (COUNT).            
      END.
      CREATE slut_allt3.   
      slut_allt3.ANTAL = (ACCUM COUNT slut_allt.SPANID).
   END.
   FOR EACH slut_allt BREAK BY slut_allt.UTLOSID:      
      ACCUMULATE slut_allt.SPANID (COUNT BY slut_allt.UTLOSID).      
      IF LAST-OF(slut_allt.UTLOSID) THEN DO TRANSACTION:
         CREATE slut2_allt2.
         ASSIGN                                          
         slut2_allt2.UTLOSID = slut_allt.UTLOSID.
         slut2_allt2.ANTAL = (ACCUM COUNT BY slut_allt.UTLOSID slut_allt.SPANID).         
      END.     
   END.
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      ASSIGN
      kant = kant + 1
      SUBSTRING(tidut.UT,1) = "TOTALSUMMA:"      
      SUBSTRING(tidut.UT,32) = "*"
      SUBSTRING(tidut.UT,126) = "*". 
      FIND FIRST slut_allt2 NO-LOCK NO-ERROR.
      IF AVAILABLE slut_allt2 THEN DO:
         FIND FIRST slut_allt3 NO-LOCK NO-ERROR.         
         FOR EACH slut_allt2 NO-LOCK:
            IF slut_allt2.FELOID = 1 THEN 
            SUBSTRING(tidut.UT,56) = STRING(slut_allt2.ANTAL,">>>>>>9").
            ELSE IF slut_allt2.FELOID = 2 THEN 
            SUBSTRING(tidut.UT,65) = STRING(slut_allt2.ANTAL,">>>>>>9").
            ELSE IF slut_allt2.FELOID = 3 THEN 
            SUBSTRING(tidut.UT,74) = STRING(slut_allt2.ANTAL,">>>>>>9").
            ELSE IF slut_allt2.FELOID = 4 THEN 
            SUBSTRING(tidut.UT,83) = STRING(slut_allt2.ANTAL,">>>>>>9").
            ELSE IF slut_allt2.FELOID = 5 THEN 
            SUBSTRING(tidut.UT,92) = STRING(slut_allt2.ANTAL,">>>>>>9").
            ELSE IF slut_allt2.FELOID = 6 THEN 
            SUBSTRING(tidut.UT,101) = STRING(slut_allt2.ANTAL,">>>>>>9").
         END.
         SUBSTRING(tidut.UT,111) = STRING(slut_allt3.ANTAL,">>>>>>9").
         FOR EACH slut2_allt2 NO-LOCK:
            IF slut2_allt2.UTLOSID = 1 THEN 
            SUBSTRING(tidut.UT,36) = STRING(slut2_allt2.ANTAL,">>>>>>9").
            ELSE IF slut2_allt2.UTLOSID = 2 THEN 
            SUBSTRING(tidut.UT,46) = STRING(slut2_allt2.ANTAL,">>>>>>9").
         END.
         RUN str2_UI.
         FIND FIRST slut2_allt2 WHERE slut2_allt2.UTLOSID = 3 NO-LOCK NO-ERROR.
         IF AVAILABLE slut2_allt2 THEN DO:
            CREATE tidut.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = "Antal driftstörningar där utlösningen är okänd:" +
            STRING(slut2_allt2.ANTAL,">>>>>>9").
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
      FOR EACH spann_temp2 USE-INDEX SPAN:
         FIND FIRST drift_temp2 WHERE drift_temp2.SPANID = spann_temp2.SPANID
         USE-INDEX SPANID NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp2 THEN DO:
            CREATE tidut.
            ASSIGN            
            SUBSTRING(tidut.UT,1) = spann_temp2.NAMN
            kant = kant + 1.                                
            IF anlaggningsdeltemp.ADELID >= 13 AND anlaggningsdeltemp.ADELID <= 16 THEN RUN adel_UI.
            ELSE IF anlaggningsdeltemp.ADELID >= 19 AND anlaggningsdeltemp.ADELID <= 20 THEN RUN adel_UI.
            ELSE IF anlaggningsdeltemp.ADELID >= 22 AND anlaggningsdeltemp.ADELID <= 23 THEN RUN adel_UI.
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,32) = "*"
               SUBSTRING(tidut.UT,126) = "*".
            END.
            FIND FIRST drift_temp3 WHERE drift_temp3.SPANID = spann_temp2.SPANID
            USE-INDEX SPANID NO-LOCK NO-ERROR.            
            FOR EACH drift_temp2 WHERE drift_temp2.SPANID = spann_temp2.SPANID
            USE-INDEX SPANID NO-LOCK:               
               IF drift_temp2.FELOID = 1 THEN 
               SUBSTRING(tidut.UT,56) = STRING(drift_temp2.ANTAL,">>>>>>9").
               ELSE IF drift_temp2.FELOID = 2 THEN 
               SUBSTRING(tidut.UT,65) = STRING(drift_temp2.ANTAL,">>>>>>9").
               ELSE IF drift_temp2.FELOID = 3 THEN 
               SUBSTRING(tidut.UT,74) = STRING(drift_temp2.ANTAL,">>>>>>9").
               ELSE IF drift_temp2.FELOID = 4 THEN 
               SUBSTRING(tidut.UT,83) = STRING(drift_temp2.ANTAL,">>>>>>9").
               ELSE IF drift_temp2.FELOID = 5 THEN 
               SUBSTRING(tidut.UT,92) = STRING(drift_temp2.ANTAL,">>>>>>9").
               ELSE IF drift_temp2.FELOID = 6 THEN 
               SUBSTRING(tidut.UT,101) = STRING(drift_temp2.ANTAL,">>>>>>9").               
            END.
            SUBSTRING(tidut.UT,111) = STRING(drift_temp3.ANTAL,">>>>>>9").
            FOR EACH drift2_temp2 WHERE drift2_temp2.SPANID = spann_temp2.SPANID
            USE-INDEX SPANID NO-LOCK:
               IF drift2_temp2.UTLOSID = 1 THEN 
               SUBSTRING(tidut.UT,36) = STRING(drift2_temp2.ANTAL,">>>>>>9").
               ELSE IF drift2_temp2.UTLOSID = 2 THEN 
               SUBSTRING(tidut.UT,46) = STRING(drift2_temp2.ANTAL,">>>>>>9").
            END.
         END.  
         ELSE DO:
            FIND FIRST leddata_temp2 WHERE leddata_temp2.SPANID = spann_temp2.SPANID
            USE-INDEX SPANID NO-LOCK NO-ERROR.
            IF AVAILABLE leddata_temp2 THEN DO:
               CREATE tidut.
               ASSIGN            
               SUBSTRING(tidut.UT,1) = spann_temp2.NAMN
               kant = kant + 1.
               IF anlaggningsdeltemp.ADELID >= 13 AND anlaggningsdeltemp.ADELID <= 16 THEN RUN adel_UI.
               ELSE IF anlaggningsdeltemp.ADELID >= 19 AND anlaggningsdeltemp.ADELID <= 20 THEN RUN adel_UI.
               ELSE IF anlaggningsdeltemp.ADELID >= 22 AND anlaggningsdeltemp.ADELID <= 23 THEN RUN adel_UI.
               ELSE DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,32) = "*"
                  SUBSTRING(tidut.UT,126) = "*".
               END.
            END.
         END.
      END.      
      RUN str_UI.      
      RUN slutspann_UI.
   END.   
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adel_UI WINDOW-2 
PROCEDURE adel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST leddata_temp2 WHERE leddata_temp2.SPANID = spann_temp2.SPANID
   USE-INDEX SPANID NO-LOCK NO-ERROR.
   IF AVAILABLE leddata_temp2 THEN DO:
      SUBSTRING(tidut.UT,26) = STRING(leddata_temp2.ANTAL,">>>>>>9").
      FIND FIRST drift_temp3 WHERE drift_temp3.SPANID = spann_temp2.SPANID
      USE-INDEX SPANID NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp3 THEN DO:
         SUBSTRING(tidut.UT,120) = STRING(drift_temp3.ANTAL / leddata_temp2.ANTAL,">>9.999").
      END.
      ELSE DO:
         SUBSTRING(tidut.UT,126) = "*".
      END.
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,32) = "*"
      SUBSTRING(tidut.UT,126) = "*".
   END.
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutspann_UI WINDOW-2 
PROCEDURE slutspann_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE drift_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE drift2_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE leddata_temp2 NO-ERROR.    
   FOR EACH drift_temp BREAK BY drift_temp.FELOID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.FELOID).      
      IF LAST-OF(drift_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                          
         drift_temp2.FELOID = drift_temp.FELOID.
         drift_temp2.ANTAL = (ACCUM COUNT BY drift_temp.FELOID drift_temp.SPANID).         
      END.     
   END.
   FIND FIRST drift_temp NO-LOCK NO-ERROR.
   IF AVAILABLE drift_temp THEN DO:
      FOR EACH drift_temp:      
         ACCUMULATE drift_temp.SPANID (COUNT).            
      END.
      CREATE drift_temp3.   
      drift_temp3.ANTAL = (ACCUM COUNT drift_temp.SPANID).
   END.
   FOR EACH drift_temp BREAK BY drift_temp.UTLOSID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.UTLOSID).      
      IF LAST-OF(drift_temp.UTLOSID) THEN DO TRANSACTION:
         CREATE drift2_temp2.
         ASSIGN                                          
         drift2_temp2.UTLOSID = drift_temp.UTLOSID.
         drift2_temp2.ANTAL = (ACCUM COUNT BY drift_temp.UTLOSID drift_temp.SPANID).         
      END.     
   END.
   FIND FIRST leddata_temp NO-LOCK NO-ERROR.
   IF AVAILABLE leddata_temp THEN DO:      
      FOR EACH leddata_temp:      
         ACCUMULATE leddata_temp.ANTAL (TOTAL). 
      END.      
      CREATE leddata_temp2.
      ASSIGN                                
      leddata_temp2.ANTAL = (ACCUM TOTAL leddata_temp.ANTAL).      
   END.
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      kant = kant + 1.
      SUBSTRING(tidut.UT,1) = "SA: " + anlaggningsdeltemp.NAMN.
      FIND FIRST leddata_temp2 NO-LOCK NO-ERROR.
      IF AVAILABLE leddata_temp2 THEN DO:
         SUBSTRING(tidut.UT,26) = STRING(leddata_temp2.ANTAL,">>>>>>9").
         FIND FIRST drift_temp3 NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp3 THEN DO:
            SUBSTRING(tidut.UT,120) = STRING(drift_temp3.ANTAL / leddata_temp2.ANTAL,">>9.999").
         END.
         ELSE DO:
            SUBSTRING(tidut.UT,126) = "*".
         END.
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,32) = "*"
         SUBSTRING(tidut.UT,126) = "*".
      END.
      FIND FIRST drift_temp2 NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp2 THEN DO:
         FIND FIRST drift_temp3 NO-LOCK NO-ERROR.         
         FOR EACH drift_temp2 NO-LOCK:
            IF drift_temp2.FELOID = 1 THEN 
            SUBSTRING(tidut.UT,56) = STRING(drift_temp2.ANTAL,">>>>>>9").
            ELSE IF drift_temp2.FELOID = 2 THEN 
            SUBSTRING(tidut.UT,65) = STRING(drift_temp2.ANTAL,">>>>>>9").
            ELSE IF drift_temp2.FELOID = 3 THEN 
            SUBSTRING(tidut.UT,74) = STRING(drift_temp2.ANTAL,">>>>>>9").
            ELSE IF drift_temp2.FELOID = 4 THEN 
            SUBSTRING(tidut.UT,83) = STRING(drift_temp2.ANTAL,">>>>>>9").
            ELSE IF drift_temp2.FELOID = 5 THEN 
            SUBSTRING(tidut.UT,92) = STRING(drift_temp2.ANTAL,">>>>>>9").
            ELSE IF drift_temp2.FELOID = 6 THEN 
            SUBSTRING(tidut.UT,101) = STRING(drift_temp2.ANTAL,">>>>>>9").
         END.
         SUBSTRING(tidut.UT,111) = STRING(drift_temp3.ANTAL,">>>>>>9").
         FOR EACH drift2_temp2 NO-LOCK:
            IF drift2_temp2.UTLOSID = 1 THEN 
            SUBSTRING(tidut.UT,36) = STRING(drift2_temp2.ANTAL,">>>>>>9").
            ELSE IF drift2_temp2.UTLOSID = 2 THEN 
            SUBSTRING(tidut.UT,46) = STRING(drift2_temp2.ANTAL,">>>>>>9").
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
   EMPTY TEMP-TABLE drift_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR. 
   EMPTY TEMP-TABLE drift2_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE leddata_temp2 NO-ERROR.    
   FOR EACH summ_temp:      
      CREATE drift_temp.
      ASSIGN
      drift_temp.ADELID = summ_temp.ADELID
      drift_temp.FELOID = summ_temp.FELOID
      drift_temp.SPANID = summ_temp.SPANID
      drift_temp.UTLOSID = summ_temp.UTLOSID.         
   END.
   FOR EACH summ_temp:      
      CREATE slut_allt.
      ASSIGN
      slut_allt.ADELID = summ_temp.ADELID
      slut_allt.FELOID = summ_temp.FELOID
      slut_allt.SPANID = summ_temp.SPANID
      slut_allt.UTLOSID = summ_temp.UTLOSID.         
   END.
   FOR EACH leddata_temp BREAK BY leddata_temp.SPANID:      
      ACCUMULATE leddata_temp.ANTAL (TOTAL BY leddata_temp.SPANID).      
      IF LAST-OF(leddata_temp.SPANID) THEN DO TRANSACTION:
         CREATE leddata_temp2.
         ASSIGN                                 
         leddata_temp2.ADELID = leddata_temp.ADELID
         leddata_temp2.SPANID = leddata_temp.SPANID.
         leddata_temp2.ANTAL = (ACCUM TOTAL BY leddata_temp.SPANID leddata_temp.ANTAL).
      END.
   END.
   FOR EACH drift_temp BREAK BY drift_temp.SPANID BY drift_temp.FELOID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.SPANID BY drift_temp.FELOID).      
      IF LAST-OF(drift_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                 
         drift_temp2.ADELID = drift_temp.ADELID
         drift_temp2.FELOID = drift_temp.FELOID
         drift_temp2.SPANID = drift_temp.SPANID.
         drift_temp2.ANTAL = (ACCUM COUNT BY drift_temp.FELOID drift_temp.SPANID).         
      END.     
   END. 
   FOR EACH drift_temp BREAK BY drift_temp.SPANID:      
      ACCUMULATE drift_temp.FELOID (COUNT BY drift_temp.SPANID).      
      IF LAST-OF(drift_temp.SPANID) THEN DO TRANSACTION:
         CREATE drift_temp3.
         ASSIGN                                 
         drift_temp3.ADELID = drift_temp.ADELID
         drift_temp3.SPANID = drift_temp.SPANID.
         drift_temp3.ANTAL = (ACCUM COUNT BY drift_temp.SPANID drift_temp.FELOID).         
      END.     
   END.
   FOR EACH drift_temp BREAK BY drift_temp.SPANID BY drift_temp.UTLOSID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.SPANID BY drift_temp.UTLOSID).      
      IF LAST-OF(drift_temp.UTLOSID) THEN DO TRANSACTION:
         CREATE drift2_temp2.
         ASSIGN                                 
         drift2_temp2.ADELID = drift_temp.ADELID
         drift2_temp2.UTLOSID = drift_temp.UTLOSID
         drift2_temp2.SPANID = drift_temp.SPANID.
         drift2_temp2.ANTAL = (ACCUM COUNT BY drift_temp.UTLOSID drift_temp.SPANID).         
      END.     
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
      RUN STORSUM6.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period, INPUT alla, 
      INPUT spannvar, INPUT adelvar, INPUT-OUTPUT TABLE summ_temp).      
   END.
   ELSE DO:
      RUN STORSUM6.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period, INPUT alla, 
      INPUT spannvar, INPUT adelvar, INPUT-OUTPUT TABLE summ_temp).
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
   SUBSTRING(tidut.UT,26) = "======="
   SUBSTRING(tidut.UT,36) = "======="
   SUBSTRING(tidut.UT,46) = "======="
   SUBSTRING(tidut.UT,56) = "======="
   SUBSTRING(tidut.UT,65) = "======="
   SUBSTRING(tidut.UT,74) = "======="
   SUBSTRING(tidut.UT,83) = "======="
   SUBSTRING(tidut.UT,92) = "======="
   SUBSTRING(tidut.UT,101) = "======="
   SUBSTRING(tidut.UT,111) = "======="
   SUBSTRING(tidut.UT,120) = "=======".
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
   SUBSTRING(tidut.UT,26) = "-------"
   SUBSTRING(tidut.UT,36) = "-------"
   SUBSTRING(tidut.UT,46) = "-------"
   SUBSTRING(tidut.UT,56) = "-------"
   SUBSTRING(tidut.UT,65) = "-------"
   SUBSTRING(tidut.UT,74) = "-------"
   SUBSTRING(tidut.UT,83) = "-------"
   SUBSTRING(tidut.UT,92) = "-------"
   SUBSTRING(tidut.UT,101) = "-------"
   SUBSTRING(tidut.UT,111) = "-------"
   SUBSTRING(tidut.UT,120) = "-------".   
   kant = kant + 1.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totexcel_UI WINDOW-2 
PROCEDURE totexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                                      
   ASSIGN
   slutbredd = 10    
   bredd[1] = 25  
   bredd[2] = 8   
   bredd[3] = 9   
   bredd[4] = 9  
   bredd[5] = 8   
   bredd[6] = 7   
   bredd[7] = 8   
   bredd[8] = 7   
   bredd[9] = 8   
   bredd[10] = 8   
   bredd[11] = 9
   bredd[12] = 10
   i = 13.
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
      
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END. 
      ELSE IF SUBSTRING(tidut.UT,26,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.
      ELSE IF SUBSTRING(tidut.UT,26,5) = "Antal" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
                                                     
      ELSE IF SUBSTRING(tidut.UT,26,6) = "km el." THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF  SUBSTRING(tidut.UT,26,2) = "st" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valelalla_UI WINDOW-2 
PROCEDURE valelalla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = 2 NO-LOCK NO-ERROR.      
   driftvar = stordriftomrtemp.STDRIFTID.
   CREATE tidut.
   tant = tant + 1.
   OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(anlaggningsdeltemp):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = anlaggningsdeltemp.NAMN
      utrec = RECID(tidut).
      CREATE tidut.               
      kant = 2.
      adelvar = anlaggningsdeltemp.ADELID.
      EMPTY TEMP-TABLE leddata_temp NO-ERROR.       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LEDDATA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
         INPUT bdatum, INPUT alla, 
         INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).      
      END.
      ELSE DO:
         RUN LEDDATA.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
         INPUT bdatum, INPUT alla, 
         INPUT adelvar, INPUT-OUTPUT TABLE leddata_temp).
      END.
      EMPTY TEMP-TABLE summ_temp NO-ERROR.       
      FOR EACH spann_temp2 USE-INDEX SPAN:            
         ASSIGN
         spannvar = spann_temp2.SPANID            
         endsum = FALSE.
         RUN storning_UI.               
      END.
      RUN storning3_UI.
      RUN bryt_UI.        
      GET NEXT aq NO-LOCK.
   END.      
   CLOSE QUERY aq.
   RUN str_UI.
   RUN slutomr_UI.
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

