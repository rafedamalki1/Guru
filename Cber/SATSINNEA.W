&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.


/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{LISTMTRL.I}
{MTRLTEMP.I}
{SMTRL.I}   
{HOPPSEK2W.I}
{LEVTEMP.I}
DEFINE SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE levvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE satsinneapph AS HANDLE NO-UNDO.
DEFINE VARIABLE truefalse AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE satstemp2 NO-UNDO LIKE satstemp.

/*DEFINE BUFFER listbuff FOR list_mtrl.*/
DEFINE BUFFER specbuff FOR spec_mtrl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_SATS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES satstemp2

/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp2.ENR2 satstemp2.BENAMNING2 ~
satstemp2.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS 
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp2


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_SATS btn_ok FILL-IN-KOD FILL-IN-BEN ~
FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 
&Scoped-Define DISPLAYED-OBJECTS TOG_VAL FILL-IN-KOD FILL-IN-BEN ~
FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_ok 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kod" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Pris" 
      VIEW-AS TEXT 
     SIZE 13 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Satsinnehållet till beredningen" 
     VIEW-AS TOGGLE-BOX
     SIZE 50.13 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_SATS FOR 
      satstemp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS DIALOG-1 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp2.ENR2 FORMAT "X(11)":U
      satstemp2.BENAMNING2 FORMAT "x(256)":U WIDTH 64
      satstemp2.ANTAL FORMAT ">>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 77.5 BY 11.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_VAL AT ROW 7.63 COL 3.88
     BRW_SATS AT ROW 8.75 COL 1.5
     btn_ok AT ROW 21 COL 65
     FILL-IN-KOD AT ROW 1.88 COL 12 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.88 COL 12 COLON-ALIGNED
     FILL-IN-ENR AT ROW 3.88 COL 12 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.88 COL 12 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 5.88 COL 12 COLON-ALIGNED
     SPACE(52.74) SKIP(15.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Satsinnehåll":L
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: satstemp2 T "?" NO-UNDO temp-db satstemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_SATS TOG_VAL DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp2.ENR2
     _FldNameList[2]   > Temp-Tables.satstemp2.BENAMNING2
"satstemp2.BENAMNING2" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "64" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.satstemp2.ANTAL
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Satsinnehåll */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(satsinneapph) THEN DELETE PROCEDURE satsinneapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Satsinnehåll */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS
&Scoped-define SELF-NAME BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS DIALOG-1
ON ROW-LEAVE OF BRW_SATS IN FRAME DIALOG-1
DO:  
   IF AVAILABLE satstemp2 THEN DO:
      DISPLAY satstemp2.ENR2 WITH BROWSE BRW_SATS NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS DIALOG-1
ON VALUE-CHANGED OF BRW_SATS IN FRAME DIALOG-1
DO:
   BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME satstemp2.ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL satstemp2.ENR2 BRW_SATS _BROWSE-COLUMN DIALOG-1
ON ENTRY OF satstemp2.ENR2 IN BROWSE BRW_SATS /* Enr */
DO:
   
   BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL satstemp2.ENR2 BRW_SATS _BROWSE-COLUMN DIALOG-1
ON LEAVE OF satstemp2.ENR2 IN BROWSE BRW_SATS /* Enr */
DO:
   IF AVAILABLE satstemp2 THEN DO:         
      DISPLAY satstemp2.ENR2 WITH BROWSE BRW_SATS NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL satstemp2.ENR2 BRW_SATS _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF satstemp2.ENR2 IN BROWSE BRW_SATS /* Enr */
DO:
   APPLY "ENTRY" TO  satstemp2.ENR2 IN BROWSE BRW_SATS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* OK */
DO:   
   {BORTBRWPROC.I}
   IF TOG_VAL = TRUE THEN DO:
      satsinn = TRUE.      
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = satstemp.KOD AND  spec_mtrl.LEVKOD = satstemp.LEVKOD NO-LOCK NO-ERROR.      
      FOR EACH satstemp2 WHERE satstemp2.SATS = FALSE NO-LOCK:         
         CREATE specbuff.
         ASSIGN           
         specbuff.ENR = satstemp2.ENR2
         specbuff.BENAMNING = satstemp2.BENAMNING2
         specbuff.NPRIS = satstemp2.PRIS2   
         specbuff.BPRIS = satstemp2.PRIS2        
         specbuff.LEVKOD = satstemp2.LEVKOD
         specbuff.BERKVANT = satstemp2.ANTAL * spec_mtrl.BERKVANT              
         specbuff.ENHET = satstemp2.ENHET2
         specbuff.LINKAB = FALSE
         specbuff.SATS = FALSE
         specbuff.MARK = TRUE
         specbuff.MODUL = 0   .
         RUN satshmt3_UI IN satsinneapph (INPUT specbuff.ENR,INPUT specbuff.LEVKOD,OUTPUT truefalse).
         specbuff.SATS = truefalse.
      END.
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = satstemp.KOD AND  spec_mtrl.LEVKOD = satstemp.LEVKOD EXCLUSIVE-LOCK NO-ERROR.
      DELETE spec_mtrl.      
   END.
   IF VALID-HANDLE(satsinneapph) THEN DELETE PROCEDURE satsinneapph.
   musz = FALSE.
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL DIALOG-1
ON VALUE-CHANGED OF TOG_VAL IN FRAME DIALOG-1 /* Satsinnehållet till beredningen */
DO:
   TOG_VAL = INPUT TOG_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   FOR EACH satstemp WHERE satstemp.SATS = FALSE:
      DELETE satstemp.
   END.   
   IF satstrue = TRUE THEN DO:      
      FIND FIRST satstemp WHERE ROWID(satstemp) = mtrl_rowid NO-LOCK NO-ERROR.      
      ASSIGN
      levvar = satstemp.LEVKOD
      kodvar = satstemp.KOD.
      RUN satshmt2_UI IN satsinneapph (INPUT kodvar,INPUT levvar,OUTPUT TABLE satstemp2 APPEND).
   END.
   ELSE IF satsmtrl = TRUE THEN DO:      
      FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-LOCK NO-ERROR.
      ASSIGN
      levvar = mtrltemp.LEVKOD
      kodvar = mtrltemp.ENR.
      RUN satshmt2_UI IN satsinneapph (INPUT kodvar,INPUT levvar,OUTPUT TABLE satstemp2 APPEND).         
      
   END.
   ELSE DO:
      FIND list_mtrl WHERE ROWID(list_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
      ASSIGN
      levvar = list_mtrl.LEVKOD
      kodvar = list_mtrl.ENR.
      RUN satshmt2_UI IN satsinneapph (INPUT kodvar,INPUT levvar,OUTPUT TABLE satstemp2 APPEND).      
   END.
   FIND FIRST satstemp2 NO-LOCK NO-ERROR.
   IF AVAILABLE satstemp2 THEN DO:   
      ASSIGN
      satstemp2.ENR2:LABEL IN BROWSE BRW_SATS = Guru.Konstanter:genk
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-KOD = satstemp2.KOD
      FILL-IN-BEN = satstemp2.BENAMNING
      FILL-IN-ENR = satstemp2.ENR
      FILL-IN-ENHET = satstemp2.ENHET
      FILL-IN-PRIS = satstemp2.PRIS
      levvar = satstemp2.LEVKOD
      kodvar = satstemp2.KOD.      
      RUN satshmt_UI IN satsinneapph (INPUT kodvar,INPUT levvar,OUTPUT TABLE satstemp2 APPEND).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " KOD = '" + kodvar + "' AND LEVKOD = '" + levvar + "' AND SATS = FALSE USE-INDEX KOD").  
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE DO:           
      {musarrow.i}  
      status-mus = SESSION:SET-WAIT-STATE("").
      IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
         RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
         Guru.GlobalaVariabler:retvalkoll = FALSE.
      END.
      MESSAGE "Information om satsen saknas"
      VIEW-AS ALERT-BOX TITLE "Meddelande". 
      LEAVE MAIN-BLOCK.
   END.   
   RUN enable_UI.       
   {FRMSIZED.I}         
   ENABLE TOG_VAL WITH FRAME {&FRAME-NAME}.
   TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.  
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      FILL-IN-PRIS:HIDDEN = TRUE.
   END.   
   status-mus = SESSION:SET-WAIT-STATE("").
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_SATS:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SATSINNEAPP.P PERSISTENT SET satsinneapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SATSINNEAPP.P PERSISTENT SET satsinneapph.
   END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY TOG_VAL FILL-IN-KOD FILL-IN-BEN FILL-IN-ENR FILL-IN-ENHET FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  ENABLE BRW_SATS btn_ok FILL-IN-KOD FILL-IN-BEN FILL-IN-ENR FILL-IN-ENHET 
         FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI DIALOG-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/   
DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.  
  /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
  status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
  IF NOT AVAILABLE satstemp2 THEN RETURN.
  valenr = satstemp2.ENR2.
  FIND FIRST levtemp WHERE levtemp.LEVKOD = satstemp2.LEVKOD
  NO-LOCK NO-ERROR.      
  IF status-ok THEN DO:   
     levnamnvar = levtemp.LEVNAMN.
     {LEVLANK.I}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

