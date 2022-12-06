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
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE endsum AS LOGICAL NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE omrrec AS RECID NO-UNDO.
DEFINE VARIABLE antaletkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}
{EXECLIN.I}
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
   FIELD ADELID AS INTEGER.

DEFINE TEMP-TABLE slut_allt2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut_allt3
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut_omr
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER.

DEFINE TEMP-TABLE slut_omr2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE slut_omr3
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER.

DEFINE TEMP-TABLE summ_temp
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTALHSP AS INTEGER
   FIELD ANTALLSP AS INTEGER.
                            /*
DEFINE TEMP-TABLE drift_temp
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER.
   */

DEFINE TEMP-TABLE drift_temp2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX ADELID ADELID ASCENDING.

DEFINE TEMP-TABLE drift_temp3   
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER
   INDEX ADELID ADELID ASCENDING.

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
      tidut.ut FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28.04 COL 81.88
     FBTN_EXCEL AT ROW 28.04 COL 96.88
     BTN_AVS AT ROW 28.04 COL 111
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
         TITLE              = "Driftstörningars varaktighetsfördelning"
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
     _Options          = "NO-LOCK"
     _FldNameList[1]   > temp-db.tidut.ut
"tidut.ut" ? "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
   
   RUN varexcel_UI.
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
     
str="==============================================================================================================".
   
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
   EMPTY TEMP-TABLE slut_allt NO-ERROR.    
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
            EMPTY TEMP-TABLE slut_omr NO-ERROR.             
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
            FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:                  
               CREATE tidut.         
               ASSIGN
               omrrec = RECID(tidut)
               SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN
               driftvar = stordriftomrtemp.STDRIFTID.
               EMPTY TEMP-TABLE slut_omr NO-ERROR.                
               FOR EACH spann_temp2 USE-INDEX SPAN:
                  EMPTY TEMP-TABLE summ_temp NO-ERROR.                   
                  ASSIGN
                  spannvar = spann_temp2.SPANID            
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
            ASSIGN
            SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN
            driftvar = stordriftomrtemp.STDRIFTID.
            CREATE tidut.
            tant = tant + 2.
            EMPTY TEMP-TABLE slut_omr NO-ERROR.             
            FOR EACH spann_temp2:
               EMPTY TEMP-TABLE summ_temp NO-ERROR.                
               ASSIGN
               spannvar = spann_temp2.SPANID            
               endsum = FALSE.
               FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
                  ASSIGN
                  distvar = omr_temp.DISTRIKTID.       
                  RUN storning_UI.                           
               END.
               RUN storning3_UI.
               RUN bryt_UI.
            END.
            RUN str_UI.
            RUN slutomr_UI.
            CREATE tidut.      
            FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:                  
               CREATE tidut. 
               ASSIGN
               omrrec = RECID(tidut)
               SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN
               driftvar = stordriftomrtemp.STDRIFTID.
               EMPTY TEMP-TABLE slut_omr NO-ERROR.                
               FOR EACH spann_temp2 USE-INDEX SPAN:
                  EMPTY TEMP-TABLE summ_temp NO-ERROR.                   
                  ASSIGN
                  spannvar = spann_temp2.SPANID            
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
         
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   breddantal = 11   /*antal kolumner*/
   bredd[1] = 26
   bredd[2] = 8
   bredd[3] = 8
   bredd[4] = 8
   bredd[5] = 8
   bredd[6] = 8
   bredd[7] = 8
   bredd[8] = 8
   bredd[9] = 8
   bredd[10] = 8
   bredd[11] = 8.   
   
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 2.            
      i = i + 1.
   END.   
   utnr[nrcol[11]] = utnr[nrcol[11]] - 1.
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.   
   CREATE tidut.
   ASSIGN                            
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "<-------------- Minuter ------------->  <------------------- Timmar ------------------->".
   CREATE tidut.
   ASSIGN                                
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "   0- <3"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "  3- <10"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = " 10- <30"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = " 30- <60"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "   1- <2"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "   2- <6"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "  6- <12"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = " 12- <24"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "    >=24"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "   Total".    
   CREATE tidut.
   ASSIGN                                
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "--------".   
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
      SUBSTRING(tidut.UT,1) = "Driftstörningars varaktighetsfördelning".      
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
   EMPTY TEMP-TABLE slut_allt2 NO-ERROR. 
   EMPTY TEMP-TABLE slut_allt3 NO-ERROR.   
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
   CREATE tidut.
   kant = kant + 1.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "TOTAL:".   
   FIND FIRST slut_allt2 NO-LOCK NO-ERROR.
   IF AVAILABLE slut_allt2 THEN DO:
      FIND FIRST slut_allt3 NO-LOCK NO-ERROR.         
      FOR EACH slut_allt2 NO-LOCK:
         IF slut_allt2.FELOID = 1 THEN                                 
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 2 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 3 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 4 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 5 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 6 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 7 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 8 THEN                           
         SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         ELSE IF slut_allt2.FELOID = 9 THEN                             
         SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(slut_allt2.ANTAL,">>>>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(slut_allt3.ANTAL,">>>>>>>9").
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
   EMPTY TEMP-TABLE slut_omr2 NO-ERROR. 
   EMPTY TEMP-TABLE slut_omr3 NO-ERROR.    
   FOR EACH slut_omr BREAK BY slut_omr.FELOID:      
      ACCUMULATE slut_omr.SPANID (COUNT BY slut_omr.FELOID).      
      IF LAST-OF(slut_omr.FELOID) THEN DO TRANSACTION:
         CREATE slut_omr2.
         ASSIGN                                          
         slut_omr2.FELOID = slut_omr.FELOID.
         slut_omr2.ANTAL = (ACCUM COUNT BY slut_omr.FELOID slut_omr.SPANID).         
      END.     
   END.
   FIND FIRST slut_omr NO-LOCK NO-ERROR.
   IF AVAILABLE slut_omr THEN DO:
      FOR EACH slut_omr:      
         ACCUMULATE slut_omr.SPANID (COUNT).            
      END.
      CREATE slut_omr3.   
      slut_omr3.ANTAL = (ACCUM COUNT slut_omr.SPANID).
   END.
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      omrrec = RECID(tidut).
      kant = kant + 1.                      
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SA: " + (SUBSTRING(TRIM(stordriftomrtemp.NAMN),1,19)).
   END.
   FIND FIRST slut_omr2 NO-LOCK NO-ERROR.
   IF AVAILABLE slut_omr2 THEN DO:
      FIND FIRST tidut WHERE RECID(tidut) = omrrec NO-LOCK NO-ERROR.
      FIND FIRST slut_omr3 NO-LOCK NO-ERROR.         
      FOR EACH slut_omr2 NO-LOCK:
         IF slut_omr2.FELOID = 1 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 2 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 3 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 4 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 5 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 6 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 7 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 8 THEN                            
         SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         ELSE IF slut_omr2.FELOID = 9 THEN 
         SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(slut_omr2.ANTAL,">>>>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(slut_omr3.ANTAL,">>>>>>>9").
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
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = spann_temp2.NAMN.
      kant = 1.
      OPEN QUERY aq FOR EACH anlaggningsdeltemp USE-INDEX ADELID NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(anlaggningsdeltemp):
         FIND FIRST drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
         USE-INDEX ADELID NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp2 THEN DO:
            FIND FIRST drift_temp3 WHERE drift_temp3.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            CREATE tidut.
            kant = kant + 1.
            SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,19).
            FOR EACH drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK:
               IF drift_temp2.FELOID = 1 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 2 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 3 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 4 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 5 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 6 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 7 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 8 THEN                            
               SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               ELSE IF drift_temp2.FELOID = 9 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
               SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(drift_temp3.ANTAL,">>>>>>>9").
            END.
         END.
         GET NEXT aq NO-LOCK.
      END.
      CLOSE QUERY aq.
      RUN str_UI.      
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
   EMPTY TEMP-TABLE drift_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR.    
   FOR EACH summ_temp BREAK BY summ_temp.FELOID:      
      ACCUMULATE summ_temp.SPANID (COUNT BY summ_temp.FELOID).      
      IF LAST-OF(summ_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                          
         drift_temp2.FELOID = summ_temp.FELOID.
         drift_temp2.ANTAL = (ACCUM COUNT BY summ_temp.FELOID summ_temp.SPANID).         
      END.     
   END.
   FIND FIRST summ_temp NO-LOCK NO-ERROR.
   IF AVAILABLE summ_temp THEN DO:
      FOR EACH summ_temp:      
         ACCUMULATE summ_temp.SPANID (COUNT).            
      END.
      CREATE drift_temp3.   
      drift_temp3.ANTAL = (ACCUM COUNT summ_temp.SPANID).
   END.
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      kant = kant + 1.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SA: " + (SUBSTRING(TRIM(spann_temp2.NAMN),1,19)).
      FIND FIRST drift_temp2 NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp2 THEN DO:
         FIND FIRST drift_temp3 NO-LOCK NO-ERROR.         
         FOR EACH drift_temp2 NO-LOCK:
            IF drift_temp2.FELOID = 1 THEN                                 
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 2 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 3 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 4 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 5 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 6 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 7 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 8 THEN                            
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            ELSE IF drift_temp2.FELOID = 9 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(drift_temp2.ANTAL,">>>>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(drift_temp3.ANTAL,">>>>>>>9").
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
   /*
   EMPTY TEMP-TABLE drift_temp NO-ERROR. 
   */
   EMPTY TEMP-TABLE drift_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE drift_temp3 NO-ERROR.    
   /*
   FOR EACH summ_temp:      
      CREATE drift_temp.
      ASSIGN
      summ_temp.ADELID = drift_temp.ADELID
      summ_temp.FELOID = drift_temp.FELOID
      summ_temp.SPANID = drift_temp.SPANID.         
   END.
   FOR EACH summ_temp:      
      CREATE slut_omr.
      ASSIGN
      slut_omr.ADELID = summ_temp.ADELID
      slut_omr.FELOID = summ_temp.FELOID
      slut_omr.SPANID = summ_temp.SPANID.         
   END.
   FOR EACH summ_temp:      
      CREATE slut_allt.
      ASSIGN
      slut_allt.ADELID = summ_temp.ADELID
      slut_allt.FELOID = summ_temp.FELOID
      slut_allt.SPANID = summ_temp.SPANID.         
   END.
   */
    FOR EACH summ_temp:  
       /*
      CREATE drift_temp.
      BUFFER-COPY summ_temp TO drift_temp.      
      */
      CREATE slut_omr.
      BUFFER-COPY summ_temp TO slut_omr.      
      CREATE slut_allt.
      BUFFER-COPY summ_temp TO slut_allt.      
   END.
   FOR EACH summ_temp BREAK BY summ_temp.ADELID BY summ_temp.FELOID:      
      ACCUMULATE summ_temp.SPANID (COUNT BY summ_temp.ADELID BY summ_temp.FELOID).      
      IF LAST-OF(summ_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                 
         drift_temp2.ADELID = summ_temp.ADELID
         drift_temp2.FELOID = summ_temp.FELOID.
         drift_temp2.ANTAL = (ACCUM COUNT BY summ_temp.FELOID summ_temp.SPANID).         
      END.     
   END. 
   FOR EACH summ_temp BREAK BY summ_temp.ADELID:      
      ACCUMULATE summ_temp.SPANID (COUNT BY summ_temp.ADELID).      
      IF LAST-OF(summ_temp.ADELID) THEN DO TRANSACTION:
         CREATE drift_temp3.
         ASSIGN                                 
         drift_temp3.ADELID = summ_temp.ADELID
         drift_temp3.FELOID = summ_temp.FELOID.
         drift_temp3.ANTAL = (ACCUM COUNT BY summ_temp.ADELID summ_temp.SPANID).         
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
      RUN STORSUM7.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).      
   END.
   ELSE DO:
      RUN STORSUM7.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp).
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
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "========"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "========".
   kant = kant + 1.
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
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "--------".   
   kant = kant + 1.
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
   ASSIGN
   SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN
   driftvar = stordriftomrtemp.STDRIFTID.
   CREATE tidut.
   tant = tant + 2. 
   EMPTY TEMP-TABLE slut_omr NO-ERROR.    
   FOR EACH spann_temp2 USE-INDEX SPAN:
      EMPTY TEMP-TABLE summ_temp NO-ERROR.       
      ASSIGN
      spannvar = spann_temp2.SPANID         
      endsum = FALSE.
      RUN storning_UI.   
      RUN storning3_UI.
      RUN bryt_UI.
   END.
   RUN str_UI.
   RUN slutomr_UI.
   CREATE tidut.      
   FOR EACH stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID NE 2 USE-INDEX STDRIFTID NO-LOCK:                  
      CREATE tidut.         
      ASSIGN
      omrrec = RECID(tidut)
      SUBSTRING(tidut.UT,1) = stordriftomrtemp.NAMN
      driftvar = stordriftomrtemp.STDRIFTID.
      EMPTY TEMP-TABLE slut_omr NO-ERROR.       
      FOR EACH spann_temp2 USE-INDEX SPAN:
         EMPTY TEMP-TABLE summ_temp NO-ERROR.          
         ASSIGN
         spannvar = spann_temp2.SPANID            
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE varexcel_UI WINDOW-2 
PROCEDURE varexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   ASSIGN
   slutbredd = 10
   bredd[1] = 26  
   bredd[2] = 9 
   bredd[3] = 9 
   bredd[4] = 9 
   bredd[5] = 9  
   bredd[6] = 9 
   bredd[7] = 9  
   bredd[8] = 9  
   bredd[9] = 9  
   bredd[10] = 9  
   bredd[11] = 10       
   i = 12.
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
      MESSAGE SUBSTRING(tidut.UT,utnr[nrcol[2]],8) SKIP
          SUBSTRING(tidut.UT,utnr[nrcol[3]],8) SKIP 
         SUBSTRING(tidut.UT,utnr[nrcol[4]],8) SKIP 
         SUBSTRING(tidut.UT,utnr[nrcol[5]],8)VIEW-AS ALERT-BOX.
            */
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.   
      ELSE IF SUBSTRING(tidut.UT,utnr[nrcol[2]],8) = "========" THEN DO:         
      END.
      ELSE IF SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "<-------------- Minuter ------------->  <------------------- Timmar ------------------->" THEN DO:
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

