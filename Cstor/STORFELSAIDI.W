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
&Scoped-define NEW 
DEFINE SHARED TEMP-TABLE visaupp NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   FIELD VISEX AS INTEGER
   FIELD VISAP AS INTEGER
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
DEFINE SHARED VARIABLE visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
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
DEFINE SHARED VARIABLE treminuter AS LOGICAL NO-UNDO.
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
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL.
   
DEFINE TEMP-TABLE slut_allt2
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD ANTALBO AS INTEGER
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL.

DEFINE TEMP-TABLE slut_allt3
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD ANTAL AS INTEGER
   FIELD ANTALBO AS INTEGER
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL.
   /*
DEFINE TEMP-TABLE summ_allt   
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL.

DEFINE TEMP-TABLE summ_allt2   
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL
   FIELD ANTAL AS INTEGER.
*/




DEFINE TEMP-TABLE summ_temp
   FIELD SPANID AS INTEGER
   FIELD FELOID AS INTEGER
   FIELD ADELID AS INTEGER
   FIELD AVBROTT AS DECIMAL   
   FIELD KUNDER AS INTEGER
   FIELD BORTKW AS DECIMAL.

{DRIFTTEMP.I}

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
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-TIDS                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_DIA BTN_SKRIV FBTN_EXCEL BTN_AVS 

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

DEFINE BUTTON BTN_DIA 
     LABEL "Diagram":L 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 25.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_DIA AT ROW 27.5 COL 56 WIDGET-ID 2
     BTN_SKRIV AT ROW 27.5 COL 81.5
     FBTN_EXCEL AT ROW 27.5 COL 96.5
     BTN_AVS AT ROW 27.5 COL 111
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
         TITLE              = "Driftstörningar fördelade på felorsak"
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


&Scoped-define SELF-NAME BTN_DIA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DIA WINDOW-2
ON CHOOSE OF BTN_DIA IN FRAME FRAME-TIDS /* Diagram */
DO:
   RUN DriftAnlaggOleauto2.p (INPUT TABLE spann_temp2,INPUT TABLE anlaggningsdeltemp,INPUT TABLE felorsaktt,INPUT TABLE drift_tempE2,INPUT TABLE drift_tempE3).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DIA WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_DIA IN FRAME FRAME-TIDS /* Diagram */
DO:
   RUN SIDLANGD.W.
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
   
   RUN felexcel_UI.
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
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista  NO-LOCK NO-ERROR.
   IF AVAILABLE visaupp THEN DO:
      WINDOW-2:TITLE = visaupp.UT.
   END.        
   EMPTY TEMP-TABLE tidut NO-ERROR.    
      
str="===========================================================================================================================".
   
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
   /*
   FOR EACH drift_temp2 WHERE NO-LOCK:
         CREATE drift_tempE2.
         BUFFER-COPY drift_temp2 TO drift_tempE2.
      END.
      FOR EACH drift_temp3 WHERE NO-LOCK:
         CREATE drift_tempE3.
         BUFFER-COPY drift_temp3 TO drift_tempE3.
      END.
      */

   RUN felorsak_UI (INPUT "Åska",1).
   RUN felorsak_UI (INPUT "Övrigt väder",2).
   RUN felorsak_UI (INPUT "Åverkan",3).
   RUN felorsak_UI (INPUT "Mtrl metod",4).
   RUN felorsak_UI (INPUT "Personal",5).
   RUN felorsak_UI (INPUT "Överlast",6).
   RUN felorsak_UI (INPUT "Återv.last",7).
   RUN felorsak_UI (INPUT "Säkr.brott",9).
   RUN felorsak_UI (INPUT "Okänd",8).
   /*
   RUN DriftAnlaggOleauto2.p (INPUT TABLE spann_temp2,INPUT TABLE anlaggningsdeltemp,INPUT TABLE felorsaktt,INPUT TABLE drift_tempE2,INPUT TABLE drift_tempE3).
*/


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
  ENABLE BTN_DIA BTN_SKRIV FBTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felexcel_UI WINDOW-2 
PROCEDURE felexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   ASSIGN
   slutbredd = 9
   bredd[1] = 25  
   bredd[2] = 9  
   bredd[3] = 9  
   bredd[4] = 9  
   bredd[5] = 9  
   bredd[6] = 9  
   bredd[7] = 9  
   bredd[8] = 9  
   bredd[9] = 9  
   bredd[10] = 9  
   bredd[11] = 9       
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
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felorsak_UI WINDOW-2 
PROCEDURE felorsak_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER orsakvar AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER felvar AS INTEGER NO-UNDO.
    
    CREATE felorsaktt.
    felorsaktt.NAMN = orsakvar.
    felorsaktt.FELOID = felvar.  
      
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
   bredd[1] = 25
   bredd[2] = 9
   bredd[3] = 9
   bredd[4] = 9
   bredd[5] = 9
   bredd[6] = 9
   bredd[7] = 9
   bredd[8] = 9
   bredd[9] = 9
   bredd[10] = 9
   bredd[11] = 9.
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.
  
   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "Åska"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Övrigt"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "Åverkan"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Mtrl"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Per-"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "Över-"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "Återv."
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "Säkr.-"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "Okänd"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Summa".

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "väder"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "metod"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "sonal"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "last"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "last"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "brott"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = ""
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "Kolumner".

   CREATE tidut.
   ASSIGN                                
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "---------".   
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
      
      IF AVAILABLE visaupp THEN DO:
         SUBSTRING(tidut.UT,1) = visaupp.UT.
      END.
      ELSE DO:
         IF treminuter = FALSE THEN SUBSTRING(tidut.UT,1) = "Driftstörningar fördelade på felorsak".      
         ELSE  SUBSTRING(tidut.UT,1) = "Driftstörningar fördelade på felorsak > 3 min.".      
      END.
      SUBSTRING(tidut.UT,60) = STRING(TODAY).
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "(inom eget nät N1 = 2, systemspänning vid frånkopplingsställe)".
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
      /*
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
      */
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN SAIFIKUND.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
         INPUT bdatum, INPUT alla, INPUT-OUTPUT antaletkunder).      
      END.
      ELSE DO:
         RUN SAIFIKUND.P 
         (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
         INPUT bdatum, INPUT alla, INPUT-OUTPUT antaletkunder).
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*SUMMERING ALLT*/
   /*
   EMPTY TEMP-TABLE summ_allt2 NO-ERROR. 
   FIND FIRST summ_allt NO-LOCK NO-ERROR.
   IF AVAILABLE summ_allt THEN DO:   
      FOR EACH summ_allt:      
         ACCUMULATE summ_allt.AVBROTT (TOTAL).
         ACCUMULATE summ_allt.KUNDER (TOTAL).
         ACCUMULATE summ_allt.BORTKW (TOTAL).
         ACCUMULATE summ_allt.BORTKW (COUNT).                                   
      END.      
      CREATE summ_allt2.
      ASSIGN 
      summ_allt2.AVBROTT = (ACCUM TOTAL summ_allt.AVBROTT)
      summ_allt2.KUNDER = (ACCUM TOTAL summ_allt.KUNDER)
      summ_allt2.BORTKW = (ACCUM TOTAL summ_allt.BORTKW)
      summ_allt2.ANTAL = (ACCUM COUNT summ_allt.BORTKW).
   END.
   CREATE tidut.
   utrec = RECID(tidut).
   SUBSTRING(tidut.UT,1) = "TOTAL".                 
   FIND FIRST summ_allt2 NO-LOCK NO-ERROR.
   IF AVAILABLE summ_allt2 THEN DO:
      IF totkunder > 0 THEN DO:
         ASSIGN                                                                     
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(summ_allt2.KUNDER / totkunder,">>>>9.9999")            
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((summ_allt2.AVBROTT / totkunder) * 60,">>>>9.9999")          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING((timmar100 - (summ_allt2.AVBROTT / totkunder)) / timmar100,">>>>9.999999").
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,30) = "*"
         SUBSTRING(tidut.UT,41) = "*"
         SUBSTRING(tidut.UT,63) = "*".
      END.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING((summ_allt2.AVBROTT / summ_allt2.KUNDER) * 60,">>>>9.9999")                               
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(summ_allt2.ANTAL,">>>>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(summ_allt2.KUNDER,">>>>>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(summ_allt2.BORTKW,">>>>>>>>>>9").
   END.  
   */ 
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
   FOR EACH slut_allt BREAK BY slut_allt.FELOID:  
      ACCUMULATE slut_allt.SPANID (COUNT BY slut_allt.FELOID).   
     
      ACCUMULATE slut_allt.AVBROTT (TOTAL  BY slut_allt.FELOID).
      ACCUMULATE slut_allt.KUNDER (TOTAL  BY slut_allt.FELOID).
      ACCUMULATE slut_allt.BORTKW (TOTAL  BY slut_allt.FELOID).
      ACCUMULATE slut_allt.BORTKW (COUNT  BY slut_allt.FELOID).
      IF LAST-OF(slut_allt.FELOID) THEN DO TRANSACTION:
         CREATE slut_allt2.
         ASSIGN                                          
         slut_allt2.FELOID = slut_allt.FELOID.
         slut_allt2.ANTAL = (ACCUM COUNT BY slut_allt.FELOID slut_allt.SPANID).  
      
         slut_allt2.AVBROTT = (ACCUM TOTAL BY slut_allt.FELOID slut_allt.AVBROTT).
         slut_allt2.KUNDER = (ACCUM TOTAL BY slut_allt.FELOID slut_allt.KUNDER).
         slut_allt2.BORTKW = (ACCUM TOTAL BY slut_allt.FELOID slut_allt.BORTKW).
         slut_allt2.ANTALBO = (ACCUM COUNT BY slut_allt.FELOID slut_allt.BORTKW).  
      END.     
   END.
   FOR EACH slut_allt2 WHERE NO-LOCK:
      ACCUMULATE slut_allt2.AVBROTT (TOTAL).
   END.
   CREATE slut_allt3.   
   slut_allt3.AVBROTT = (ACCUM TOTAL slut_allt2.AVBROTT).
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      kant = kant + 1.
      SUBSTRING(tidut.UT,1) = "SA: " + stordriftomrtemp.NAMN. 
      FIND FIRST slut_allt2 NO-LOCK NO-ERROR.
      IF AVAILABLE slut_allt2 THEN DO:
         FIND FIRST slut_allt3 NO-LOCK NO-ERROR.         
         FOR EACH slut_allt2 NO-LOCK:
            IF slut_allt2.FELOID = 1 THEN                 
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 2 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 3 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 4 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 5 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 6 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 7 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 9 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF slut_allt2.FELOID = 8 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING((slut_allt2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING((slut_allt3.AVBROTT / antaletkunder) * 60,">>>>9.9999").
         END.
      END.
      CREATE tidut.
      kant = kant + 1.
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
         FIND FIRST drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
         USE-INDEX ADELID NO-LOCK NO-ERROR.
         IF AVAILABLE drift_temp2 THEN DO:
            FIND FIRST drift_temp3 WHERE drift_temp3.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK NO-ERROR.
            CREATE tidut.
            kant = kant + 1.
            SUBSTRING(tidut.UT,4) = SUBSTRING(anlaggningsdeltemp.NAMN,1,20).
            FOR EACH drift_temp2 WHERE drift_temp2.ADELID = anlaggningsdeltemp.ADELID
            USE-INDEX ADELID NO-LOCK:
               IF drift_temp2.FELOID = 1 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 2 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 3 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 4 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 5 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 6 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 7 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 9 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               ELSE IF drift_temp2.FELOID = 8 THEN 
               SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
               SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING((drift_temp3.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            END.
         END.
         GET NEXT aq NO-LOCK.
      END.
      CLOSE QUERY aq.
      RUN str_UI.
      kant = kant + 1.
      
      
      
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
   FOR EACH drift_temp BREAK BY drift_temp.FELOID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.FELOID). 
      ACCUMULATE drift_temp.AVBROTT (TOTAL  BY drift_temp.FELOID).
      ACCUMULATE drift_temp.KUNDER (TOTAL  BY drift_temp.FELOID).
      ACCUMULATE drift_temp.BORTKW (TOTAL  BY drift_temp.FELOID).
      ACCUMULATE drift_temp.BORTKW (COUNT  BY drift_temp.FELOID).     
      IF LAST-OF(drift_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                          
         drift_temp2.FELOID = drift_temp.FELOID.
         drift_temp2.ANTAL = (ACCUM COUNT BY drift_temp.FELOID drift_temp.SPANID).   
         drift_temp2.AVBROTT = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.AVBROTT).
         drift_temp2.KUNDER = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.KUNDER).
         drift_temp2.BORTKW = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.BORTKW).
         drift_temp2.ANTALBO = (ACCUM COUNT BY drift_temp.FELOID drift_temp.BORTKW).       
      END.     
   END.
   FOR EACH drift_temp2 WHERE NO-LOCK:
      ACCUMULATE drift_temp2.AVBROTT (TOTAL).
   END.
   CREATE drift_temp3.   
   drift_temp3.AVBROTT = (ACCUM TOTAL drift_temp2.AVBROTT).
   
   /*
   IF AVAILABLE drift_temp THEN DO:
      FOR EACH drift_temp:      
         ACCUMULATE drift_temp.SPANID (COUNT).            
      END.
      CREATE drift_temp3.   
      drift_temp3.ANTAL = (ACCUM COUNT drift_temp.SPANID).
   END.
   */
   IF endsum = FALSE THEN DO:
      CREATE tidut.
      kant = kant + 1.
      FOR EACH drift_temp2 WHERE NO-LOCK:
         CREATE drift_tempE2.
         BUFFER-COPY drift_temp2 TO drift_tempE2.
         drift_tempE2.SPANID = spann_temp2.SPANID.
      END.
      FOR EACH drift_temp3 WHERE NO-LOCK:
         CREATE drift_tempE3.
         BUFFER-COPY drift_temp3 TO drift_tempE3.
         drift_tempE3.SPANID = spann_temp2.SPANID.
        
      END.


      SUBSTRING(tidut.UT,1) = "SA sss: " + spann_temp2.NAMN. 
      FIND FIRST drift_temp2 NO-LOCK NO-ERROR.
      IF AVAILABLE drift_temp2 THEN DO:
         FIND FIRST drift_temp3 NO-LOCK NO-ERROR.         
         FOR EACH drift_temp2 NO-LOCK:
            IF drift_temp2.FELOID = 1 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 2 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 3 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 4 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 5 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 6 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 7 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 9 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            ELSE IF drift_temp2.FELOID = 8 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING((drift_temp2.AVBROTT / antaletkunder) * 60,">>>>9.9999").
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING((drift_temp3.AVBROTT / antaletkunder) * 60,">>>>9.9999").
         END.
      END.
      
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
   FOR EACH summ_temp:      
      CREATE drift_temp.
      ASSIGN
      drift_temp.ADELID = summ_temp.ADELID
      drift_temp.FELOID = summ_temp.FELOID
      drift_temp.SPANID = summ_temp.SPANID
      drift_temp.AVBROTT = summ_temp.AVBROTT
      drift_temp.KUNDER = summ_temp.KUNDER
      drift_temp.BORTKW = summ_temp.BORTKW.           
   END.
   FOR EACH summ_temp:      
      CREATE slut_allt.
      ASSIGN
      slut_allt.ADELID = summ_temp.ADELID
      slut_allt.FELOID = summ_temp.FELOID
      slut_allt.SPANID = summ_temp.SPANID  
      slut_allt.AVBROTT = summ_temp.AVBROTT
      slut_allt.KUNDER = summ_temp.KUNDER
      slut_allt.BORTKW = summ_temp.BORTKW.        
   END.
  
   FOR EACH drift_temp BREAK BY drift_temp.ADELID BY drift_temp.FELOID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.ADELID BY drift_temp.FELOID). 
      ACCUMULATE drift_temp.AVBROTT (TOTAL BY drift_temp.ADELID BY drift_temp.FELOID).
      ACCUMULATE drift_temp.KUNDER (TOTAL BY drift_temp.ADELID BY drift_temp.FELOID).
      ACCUMULATE drift_temp.BORTKW (TOTAL BY drift_temp.ADELID BY drift_temp.FELOID).
      ACCUMULATE drift_temp.BORTKW (COUNT BY drift_temp.ADELID BY drift_temp.FELOID).     
      IF LAST-OF(drift_temp.FELOID) THEN DO TRANSACTION:
         CREATE drift_temp2.
         ASSIGN                                 
         drift_temp2.ADELID = drift_temp.ADELID
         drift_temp2.FELOID = drift_temp.FELOID.
         drift_temp2.ANTAL = (ACCUM COUNT BY drift_temp.FELOID drift_temp.SPANID).
         
         drift_temp2.AVBROTT = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.AVBROTT).
         drift_temp2.KUNDER = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.KUNDER).
         drift_temp2.BORTKW = (ACCUM TOTAL BY drift_temp.FELOID drift_temp.BORTKW).
         drift_temp2.ANTALBO = (ACCUM COUNT BY drift_temp.FELOID drift_temp.BORTKW).            
      END.     
   END. 
   FOR EACH drift_temp BREAK BY drift_temp.ADELID:      
      ACCUMULATE drift_temp.SPANID (COUNT BY drift_temp.ADELID).
      ACCUMULATE drift_temp.AVBROTT (TOTAL BY drift_temp.ADELID).
      ACCUMULATE drift_temp.KUNDER (TOTAL BY drift_temp.ADELID).
      ACCUMULATE drift_temp.BORTKW (TOTAL BY drift_temp.ADELID).
      ACCUMULATE drift_temp.BORTKW (COUNT BY drift_temp.ADELID).  
            
      IF LAST-OF(drift_temp.ADELID) THEN DO TRANSACTION:
         CREATE drift_temp3.
         ASSIGN                                 
         drift_temp3.ADELID = drift_temp.ADELID
         drift_temp3.FELOID = drift_temp.FELOID.
         drift_temp3.ANTAL = (ACCUM COUNT BY drift_temp.ADELID drift_temp.SPANID).  
         
         drift_temp3.AVBROTT = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.AVBROTT).
         drift_temp3.KUNDER = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.KUNDER).
         drift_temp3.BORTKW = (ACCUM TOTAL BY drift_temp.ADELID drift_temp.BORTKW).
         drift_temp3.ANTALBO = (ACCUM COUNT BY drift_temp.ADELID drift_temp.BORTKW).  
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
      RUN STORSUM5SAIDI.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp, INPUT treminuter).      
   END.
   ELSE DO:
      RUN STORSUM5SAIDI.p 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT driftvar, INPUT alla, 
      INPUT spannvar, INPUT-OUTPUT TABLE summ_temp, INPUT treminuter).
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
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "======="
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "========".
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
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "---------"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "----------".   
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

