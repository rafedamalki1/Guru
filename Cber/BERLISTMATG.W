&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

{ALLDEF.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LISTMTRL.I} 
DEFINE INPUT PARAMETER TABLE FOR elist_mtrl.


{KONVALTEMP.I}   

DEFINE SHARED VARIABLE btnbervalh AS HANDLE NO-UNDO.
   
DEFINE SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.          
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  

DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE berlistmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlrow AS ROWID NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
   

DEFINE TEMP-TABLE mtrlut   
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE mtrlinfo2  
   FIELD KTYPKOD AS CHARACTER
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD NUM AS INTEGER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS CHARACTER
   FIELD FRI3 AS CHARACTER
   FIELD ANTAL AS DECIMAL
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER
   FIELD UPPLAG AS INTEGER
   INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE mtrl_temp2   
{MTRLTEMP2TT.I}
&Scoped-define SHARED SHARED


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_INFO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrlinfo2

/* Definitions for BROWSE BRW_INFO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_INFO mtrlinfo2.Enr mtrlinfo2.Benamning ~
mtrlinfo2.KTYPKOD mtrlinfo2.FRI2 mtrlinfo2.FRI3 mtrlinfo2.ANTAL ~
mtrlinfo2.LEVKOD mtrlinfo2.PRIS mtrlinfo2.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_INFO 
&Scoped-define QUERY-STRING-BRW_INFO FOR EACH mtrlinfo2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_INFO OPEN QUERY BRW_INFO FOR EACH mtrlinfo2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_INFO mtrlinfo2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_INFO mtrlinfo2


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_INFO}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_INFO BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_INFO FOR 
      mtrlinfo2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_INFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_INFO Dialog-Frame _STRUCTURED
  QUERY BRW_INFO NO-LOCK DISPLAY
      mtrlinfo2.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      mtrlinfo2.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 30
      mtrlinfo2.KTYPKOD COLUMN-LABEL "Konstruktionskod" FORMAT "X(256)":U
            WIDTH 16
      mtrlinfo2.FRI2 COLUMN-LABEL "Id" FORMAT "X(256)":U WIDTH 14
      mtrlinfo2.FRI3 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 14
      mtrlinfo2.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>>>>>9":U
      mtrlinfo2.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(8)":U
      mtrlinfo2.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9.99":U
      mtrlinfo2.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 116.5 BY 25.08.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_INFO AT ROW 2 COL 1.5 WIDGET-ID 200
     BTN_AVS AT ROW 28 COL 103.88 WIDGET-ID 2
     SPACE(0.49) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Visa materiel" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: mtrlinfo2 T "?" NO-UNDO temp-db mtrlinfo2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_INFO 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_INFO
/* Query rebuild information for BROWSE BRW_INFO
     _TblList          = "Temp-Tables.mtrlinfo2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrlinfo2.Enr
"mtrlinfo2.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrlinfo2.Benamning
"mtrlinfo2.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrlinfo2.KTYPKOD
"mtrlinfo2.KTYPKOD" "Konstruktionskod" "X(256)" "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrlinfo2.FRI2
"mtrlinfo2.FRI2" "Id" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrlinfo2.FRI3
"mtrlinfo2.FRI3" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrlinfo2.ANTAL
"mtrlinfo2.ANTAL" "Antal" "->>>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.mtrlinfo2.LEVKOD
"mtrlinfo2.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.mtrlinfo2.PRIS
"mtrlinfo2.PRIS" "Pris" ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.mtrlinfo2.UPPLAG
"mtrlinfo2.UPPLAG" "Upplag" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_INFO */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Visa materiel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON CHOOSE OF BTN_AVS IN FRAME Dialog-Frame /* Avsluta */
DO:
   
   DELETE PROCEDURE Guru.GlobalaVariabler:LastDialogh NO-ERROR.  
   Guru.GlobalaVariabler:LastDialogh = ?.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_INFO
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {muswait.i}    
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE mtrlut NO-ERROR. 
   RUN skapalista_UI IN berlistmapph (INPUT valaonr,INPUT valomrade,INPUT valkonst,
                                      INPUT TABLE kon_val, OUTPUT musz2,OUTPUT TABLE mtrlut,OUTPUT TABLE mtrl_temp2).            
   str = "---------------------------------------------------------------------------------------".
   musz = musz2.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   
   RUN enable_UI.      
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:      
      mtrlinfo2.PRIS:VISIBLE IN BROWSE BRW_INFO = FALSE.                         
   END.
   ELSE DO:
      ASSIGN
      mtrlinfo2.PRIS:VISIBLE IN BROWSE BRW_INFO = TRUE.            
   END.
   RUN visa_UI.             
   Guru.GlobalaVariabler:LastDialogh = THIS-PROCEDURE. 
   {musarrow.i}   
   {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_INFO:HANDLE IN FRAME {&FRAME-NAME} ).      
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERLISTMAPPU.P PERSISTENT SET berlistmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERLISTMAPPU.P PERSISTENT SET berlistmapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andbrwb_UI Dialog-Frame 
PROCEDURE andbrwb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(btnbervalh) THEN DO:
      RUN valpostbrwval_UI IN btnbervalh (INPUT mtrlinfo2.NUM).
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE BRW_INFO BTN_AVS 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movetotop_UI Dialog-Frame 
PROCEDURE Movetotop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {DIA_M_SLUT.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI Dialog-Frame 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE mtrlinfo2 NO-ERROR. 
   {muswait.i}
   FIND FIRST elist_mtrl NO-LOCK NO-ERROR.
   RUN htmkonval2_UI IN berlistmapph (INPUT valaonr,INPUT valomrade,INPUT elist_mtrl.ENR,OUTPUT TABLE mtrlinfo2 APPEND).   
   {musarrow.i}
   FIND FIRST mtrlinfo2 NO-LOCK NO-ERROR.
   IF AVAILABLE mtrlinfo2 THEN DO:
      RUN setcolsortvar_UI  IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

