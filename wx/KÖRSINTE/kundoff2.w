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

  Created: 08/26/96 - 11:48 am

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{ALLDEF.I}
{SOKMTRL.I}
DEFINE NEW SHARED VARIABLE avb AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE offe AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kundoffproch AS HANDLE NO-UNDO. /* KUNDOFFAPP.P */
DEFINE SHARED VARIABLE kalksprec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.  

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

&Scoped-define SHARED SHARED
{HOPPSEK2W.I}

/*{EGENBEN.I}*/



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kund_mtrl

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL kund_mtrl.Enr kund_mtrl.Benamning ~
kund_mtrl.KPRIS kund_mtrl.NPRIS kund_mtrl.Enhet kund_mtrl.BERKVANT ~
kund_mtrl.SUMMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH kund_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH kund_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL kund_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL kund_mtrl


/* Definitions for FRAME FRAME-B                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MTRL FBTN_SKAPA FBTN_VISA FBTN_SKRIV ~
BTN_PER BTN_TOT FBTN_OK BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PER 
     LABEL "Per artikel":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TOT 
     LABEL "Totalt":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK AUTO-END-KEY 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Kund off.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      kund_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-2 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      kund_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      kund_mtrl.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 25
      kund_mtrl.KPRIS COLUMN-LABEL "Brutto" FORMAT ">>>>>9.99":U
      kund_mtrl.NPRIS COLUMN-LABEL "Netto" FORMAT ">>>>>9.99":U
      kund_mtrl.Enhet COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      kund_mtrl.BERKVANT COLUMN-LABEL "Antal" FORMAT "->>>>9.99":U
      kund_mtrl.SUMMA COLUMN-LABEL "Summa" FORMAT ">>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 83.38 BY 14.58
         TITLE "Materielspecifikation".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     BRW_MTRL AT ROW 1.25 COL 1.5
     FBTN_SKAPA AT ROW 6.13 COL 85.88
     FBTN_VISA AT ROW 7.21 COL 85.88
     FBTN_SKRIV AT ROW 8.33 COL 85.88
     BTN_PER AT ROW 16.17 COL 22
     BTN_TOT AT ROW 16.17 COL 37.13
     FBTN_OK AT ROW 16.17 COL 70.88
     BTN_AVB AT ROW 16.17 COL 85.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.38 BY 16.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db kund_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Kund"
         HEIGHT             = 16.54
         WIDTH              = 100.63
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 113.5
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 113.5
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
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
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BRW_MTRL 1 FRAME-B */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-B         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-B = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-B       = TRUE.

ASSIGN 
       BTN_PER:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.kund_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kund_mtrl.Enr
"kund_mtrl.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kund_mtrl.Benamning
"kund_mtrl.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kund_mtrl.KPRIS
"kund_mtrl.KPRIS" "Brutto" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kund_mtrl.NPRIS
"kund_mtrl.NPRIS" "Netto" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kund_mtrl.Enhet
"kund_mtrl.Enhet" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kund_mtrl.BERKVANT
"kund_mtrl.BERKVANT" "Antal" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.kund_mtrl.SUMMA
"kund_mtrl.SUMMA" "Summa" ">>>>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-B /* Materielspecifikation */
DO: 
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF NOT AVAILABLE off_mtrl THEN DO:   
      
      EMPTY TEMP-TABLE ekund_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
      CREATE ekund_mtrl.
      BUFFER-COPY kund_mtrl TO ekund_mtrl.
      RUN OPART2.W (INPUT FALSE, INPUT-OUTPUT TABLE ekund_mtrl,INPUT-OUTPUT TABLE emtrl_mtrl).
      FIND FIRST ekund_mtrl NO-LOCK NO-ERROR.
      BUFFER-COPY ekund_mtrl TO kund_mtrl.
      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kund_mtrl)).
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avbryt */
DO:
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF NOT svar THEN DO:       
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.                    
   ELSE DO:
      musz = musz.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PER WINDOW-2
ON CHOOSE OF BTN_PER IN FRAME FRAME-B /* Per artikel */
DO:
   
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
      
      EMPTY TEMP-TABLE ekund_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
      CREATE ekund_mtrl.
      BUFFER-COPY kund_mtrl TO ekund_mtrl.
      RUN OPART2.W (INPUT FALSE, INPUT-OUTPUT TABLE ekund_mtrl,INPUT-OUTPUT TABLE emtrl_mtrl).
      FIND FIRST ekund_mtrl NO-ERROR.
      IF AVAILABLE ekund_mtrl THEN DO:
         BUFFER-COPY ekund_mtrl TO kund_mtrl.
      END.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kund_mtrl)).
      
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].
   IF antal_valda > 1 THEN status-ok = BRW_MTRL:DESELECT-ROWS() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TOT WINDOW-2
ON CHOOSE OF BTN_TOT IN FRAME FRAME-B /* Totalt */
DO:
   musz = FALSE. 
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      offe = TRUE.
      RUN OPTOT2.W (INPUT ROWID(off_mtrl)).
      
   END.
   ELSE DO:
      offe = FALSE.                        
      RUN OPTOT2.W (INPUT ROWID(kund_mtrl)).
      
   END.   
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      BTN_PER:HIDDEN = TRUE.
   END.
   ELSE DO:
      BTN_PER:HIDDEN = FALSE.
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-2
ON CHOOSE OF FBTN_OK IN FRAME FRAME-B /* Ok */
DO: 
   {muswait.i}
   RUN kundmtrlspar_UI IN kundoffproch (INPUT FALSE,INPUT varforetypval[1],INPUT valkalknr,INPUT TABLE kund_mtrl,INPUT TABLE off_mtrl).   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-2
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-B /* Kund off. */
DO:
   {muswait.i}
   {AVBGOM.I}
   RUN KUNDTRP2.W.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i}                 
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISAKOFF.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO: 
   {muswait.i}
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISAKOFF.W.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(kundoffproch) THEN DELETE PROCEDURE kundoffproch.
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
   {muswait.i}        
   RUN kundmtrlhmt_UI IN kundoffproch (INPUT valkalknr,OUTPUT TABLE kund_mtrl,OUTPUT TABLE off_mtrl).
   kund_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.                                                    
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN enable_UI.      
   
   {FRMSIZE.I}   
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      BTN_PER:HIDDEN = TRUE.
   END.
   ELSE DO:
      BTN_PER:HIDDEN = FALSE.
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN     
      kund_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE
      kund_mtrl.KPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE
      kund_mtrl.SUMMA:VISIBLE IN BROWSE BRW_MTRL = FALSE.      
      BTN_PER:HIDDEN = TRUE.
      BTN_TOT:HIDDEN = TRUE.      
   END.
   ELSE DO:
      ASSIGN
      kund_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = TRUE
      kund_mtrl.KPRIS:VISIBLE IN BROWSE BRW_MTRL = TRUE
      kund_mtrl.SUMMA:VISIBLE IN BROWSE BRW_MTRL = TRUE.
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
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ENR").
   IF Guru.Konstanter:appcon THEN DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch.      
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
  ENABLE BRW_MTRL FBTN_SKAPA FBTN_VISA FBTN_SKRIV BTN_PER BTN_TOT FBTN_OK 
         BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

