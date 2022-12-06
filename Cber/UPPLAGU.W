&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

  Created: 04/17/97 -  3:23 pm

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

{DEPATEMP.I}
&Scoped-define SHARED SHARED   
{BERUPPTEMP.I}
DEFINE SHARED VARIABLE kon_rowid AS ROWID NO-UNDO.  
DEFINE SHARED VARIABLE varupp AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE varadr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE varanm AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE komb AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE ny AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparkod AS INTEGER NO-UNDO.
DEFINE VARIABLE uppaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE uppdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE upplagapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numlines AS INTEGER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_DEPA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES depatemp

/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA depatemp.Dep-Nr depatemp.Benamning ~
depatemp.LevAdress 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH depatemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH depatemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA depatemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA depatemp


/* Definitions for FRAME FRAME-B                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-B ~
    ~{&OPEN-QUERY-BRW_DEPA}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-UPPL BRW_DEPA FILL-IN-ADR ~
FILL-IN-PNR FILL-IN-ORT FILL-IN-XKORDCH FILL-IN-YKORDCH ED_ANM BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-UPPL2 FILL-IN-UPPL FILL-IN-ADR ~
FILL-IN-PNR FILL-IN-ORT FILL-IN-XKORDCH FILL-IN-YKORDCH ED_ANM FILL-IN-ANM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE ED_ANM AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 240 SCROLLBAR-VERTICAL
     SIZE 50 BY 4.75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ADR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 52.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANM AS CHARACTER FORMAT "X(256)":U INITIAL "Anmärkning" 
      VIEW-AS TEXT 
     SIZE 10 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-ORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 52.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PNR AS CHARACTER FORMAT "xxx xx":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPL AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Upplagsnr" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPPL2 AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Upplagsnr" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-XKORDCH AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Koordinater    X" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-YKORDCH AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Y" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEPA FOR 
      depatemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA WINDOW-2 _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      depatemp.Dep-Nr COLUMN-LABEL "Dep-Nr" FORMAT ">>>":U
      depatemp.Benamning COLUMN-LABEL "Depå" FORMAT "x(256)":U
            WIDTH 30
      depatemp.LevAdress COLUMN-LABEL "Leveransadress" FORMAT "x(256)":U
            WIDTH 15.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 56 BY 11.5
         TITLE "Depåadresser(dubbelklicka för val)" ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     FILL-IN-UPPL2 AT ROW 1.75 COL 17 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-UPPL AT ROW 1.75 COL 17 COLON-ALIGNED
     BRW_DEPA AT ROW 3 COL 69.5 WIDGET-ID 100
     FILL-IN-ADR AT ROW 3.13 COL 17 COLON-ALIGNED
     FILL-IN-PNR AT ROW 4.63 COL 17 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-ORT AT ROW 6.13 COL 17 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-XKORDCH AT ROW 7.63 COL 17 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-YKORDCH AT ROW 9.46 COL 17 COLON-ALIGNED WIDGET-ID 20
     ED_ANM AT ROW 13 COL 19 NO-LABEL
     BTN_OK AT ROW 21.25 COL 95.5
     BTN_AVB AT ROW 21.25 COL 110.5
     FILL-IN-ANM AT ROW 13 COL 8.75 NO-LABEL WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 21.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: depatemp T "?" NO-UNDO temp-db depatemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Upplag"
         HEIGHT             = 21.88
         WIDTH              = 125
         MAX-HEIGHT         = 22.46
         MAX-WIDTH          = 135.25
         VIRTUAL-HEIGHT     = 22.46
         VIRTUAL-WIDTH      = 135.25
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_DEPA FILL-IN-UPPL FRAME-B */
/* SETTINGS FOR FILL-IN FILL-IN-ANM IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-ANM:READ-ONLY IN FRAME FRAME-B        = TRUE.

ASSIGN 
       FILL-IN-UPPL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UPPL2 IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-UPPL2:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEPA
/* Query rebuild information for BROWSE BRW_DEPA
     _TblList          = "Temp-Tables.depatemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.depatemp.Dep-Nr
"depatemp.Dep-Nr" "Dep-Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.depatemp.Benamning
"depatemp.Benamning" "Depå" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.depatemp.LevAdress
"depatemp.LevAdress" "Leveransadress" "x(256)" "character" ? ? ? ? ? ? no ? no no "15.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DEPA
&Scoped-define SELF-NAME BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF BRW_DEPA IN FRAME FRAME-B /* Depåadresser(klicka för val) */
DO:
   IF AVAILABLE depatemp THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "DEPAKORD"                   
      inextradatatemp.HUVUDINT = depatemp.DEP-NR.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         FILL-IN-XKORDCH = extradatatemp.SOKCHAR[1]
         FILL-IN-YKORDCH = extradatatemp.SOKCHAR[2].      
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-XKORDCH = ""
         FILL-IN-YKORDCH = "".
      END.   
      ASSIGN
      FILL-IN-ADR = depatemp.LEVADRESS
      FILL-IN-PNR = depatemp.LEVPNR
      FILL-IN-ORT = depatemp.LEVORT.
      DISPLAY  FILL-IN-ADR FILL-IN-PNR  FILL-IN-ORT  FILL-IN-XKORDCH FILL-IN-YKORDCH WITH FRAME FRAME-B IN WINDOW WINDOW-2.
       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avbryt */
DO: 
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-2
ON CHOOSE OF BTN_OK IN FRAME FRAME-B /* Ok */
DO:   
   numlines = ED_ANM:NUM-LINES.
   ASSIGN
   FILL-IN-UPPL = INPUT FILL-IN-UPPL
   FILL-IN-ADR = INPUT FILL-IN-ADR
   ED_ANM = INPUT ED_ANM.
   
   ED_ANM = REPLACE(ED_ANM,"$"," ").
   IF NOT AVAILABLE berupptemp THEN DO:
      CREATE berupptemp.
      ASSIGN
      berupptemp.UPPLAG  =    INPUT FILL-IN-UPPL
      berupptemp.AONR = valaonr
      berupptemp.OMRADE = valomrade.
      berupptemp.BERROW = ROWID(berupptemp).
      /* bugg ändrat 20170330 Lena 
      berupptemp.BERROW = kon_rowid.*/ 
      
   END.
   
   ASSIGN
   berupptemp.ANMARK  = ED_ANM
   berupptemp.ADRESS  = INPUT FILL-IN-ADR
   berupptemp.ORT     =   INPUT FILL-IN-ORT 
   berupptemp.POSTNR  =   INPUT FILL-IN-PNR 
   /*
   berupptemp.XKORD   =   INPUT FILL-IN-XKORD 
   berupptemp.YKORD   =   INPUT FILL-IN-YKORD
   */
   berupptemp.XKORDCH   =   INPUT FILL-IN-XKORDCH 
   berupptemp.YKORDCH   =   INPUT FILL-IN-YKORDCH. 
   EMPTY TEMP-TABLE eberupptemp NO-ERROR.
   CREATE eberupptemp.
   BUFFER-COPY berupptemp TO eberupptemp.
   RUN btnok_UI IN upplagapph (INPUT-OUTPUT TABLE eberupptemp).
   FIND FIRST eberupptemp NO-LOCK NO-ERROR.
   FIND FIRST berupptemp WHERE berupptemp.UPPLAG = eberupptemp.UPPLAG NO-ERROR.
   IF AVAILABLE berupptemp THEN DO:
      berupptemp.BERROW = eberupptemp.BERROW.
   END.   
   /*bugg ändrat 20170330 Lena ROWID måste uppdateras 
   RUN btnok_UI IN upplagapph (INPUT TABLE eberupptemp).*/
     
  /* 
   RUN btnok_UI IN upplagapph (INPUT ny,INPUT-OUTPUT kon_rowid,INPUT valaonr,INPUT valdelnr,
                               INPUT valomrade,INPUT FILL-IN-UPPL,
                               INPUT FILL-IN-ADR,INPUT ED_ANM,INPUT numlines,INPUT-OUTPUT TABLE berupptemp).
                               */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UPPL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UPPL WINDOW-2
ON LEAVE OF FILL-IN-UPPL IN FRAME FRAME-B /* Upplagsnr */
DO:
   IF ny = TRUE THEN DO:
      IF INPUT FILL-IN-UPPL NE "" THEN DO:
         IF INPUT FILL-IN-UPPL NE 0 THEN DO:
            FIND FIRST berupptemp WHERE berupptemp.UPPLAG = INPUT FILL-IN-UPPL AND
            berupptemp.AONR = valaonr AND berupptemp.OMRADE = valomrade NO-LOCK NO-ERROR.
            IF AVAILABLE berupptemp THEN DO:
               RELEASE berupptemp NO-ERROR.
               MESSAGE "Det finns redan ett upplag med detta nummer. Ange ett nytt."
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               RETURN NO-APPLY.
            END.
         END. 
         ELSE DO:
            MESSAGE "Upplagsnummer får ej vara 0." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-UPPL.
            RETURN NO-APPLY.
         END.  
      END.
      ELSE DO:
         MESSAGE "Upplagsnummer kan inte vara blank." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-UPPL.
         RETURN NO-APPLY.         
      END.      
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UPPL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UPPL2 WINDOW-2
ON LEAVE OF FILL-IN-UPPL2 IN FRAME FRAME-B /* Upplagsnr */
DO:
   IF ny = TRUE THEN DO:
      IF INPUT FILL-IN-UPPL NE "" THEN DO:
         IF INPUT FILL-IN-UPPL NE 0 THEN DO:
            FIND FIRST berupptemp WHERE berupptemp.UPPLAG = INPUT FILL-IN-UPPL AND
            berupptemp.AONR = valaonr AND berupptemp.OMRADE = valomrade NO-LOCK NO-ERROR.
            IF AVAILABLE berupptemp THEN DO:
               RELEASE berupptemp NO-ERROR.
               MESSAGE "Det finns redan ett upplag med detta nummer. Ange ett nytt."
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               RETURN NO-APPLY.
            END.
         END. 
         ELSE DO:
            MESSAGE "Upplagsnummer får ej vara 0." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-UPPL.
            RETURN NO-APPLY.
         END.  
      END.
      ELSE DO:
         MESSAGE "Upplagsnummer kan inte vara blank." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-UPPL.
         RETURN NO-APPLY.         
      END.      
   END.  
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
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(upplagapph) THEN DELETE PROCEDURE upplagapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   IF ny = TRUE THEN DO:
      {&WINDOW-NAME}:TITLE = "Skapa nytt upplag".  
   END.
   ELSE DO:     
      {&WINDOW-NAME}:TITLE = "Ändra upplag". 
      FIND FIRST berupptemp WHERE berupptemp.BERROW = kon_rowid NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-UPPL = berupptemp.UPPLAG
      FILL-IN-UPPL2 = berupptemp.UPPLAG
      FILL-IN-ADR = berupptemp.ADRESS
      FILL-IN-ORT = berupptemp.ORT
      FILL-IN-PNR = berupptemp.POSTNR
      FILL-IN-XKORDCH = berupptemp.XKORDCH 
      FILL-IN-YKORDCH = berupptemp.YKORDCH
      ED_ANM = berupptemp.ANMARK
      sparkod = berupptemp.UPPLAG.       
   END.   
  /*RUN openbdynspec_UI IN brwproc[1].*/ 
  RUN enable_UI.   
   {FRMSIZE.I}
     
  IF ny = TRUE THEN DO:
     ASSIGN
     FILL-IN-UPPL2:HIDDEN = TRUE
     FILL-IN-UPPL:HIDDEN = FALSE.
  END.
  ELSE DO: 
     ASSIGN
     FILL-IN-UPPL2:HIDDEN = FALSE
     FILL-IN-UPPL:HIDDEN = TRUE.
  END.
  
  {musarrow.i}
   {WIN_M_SLUT.I}
  {KRYSSBORT.I}
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_DEPA:HANDLE IN FRAME {&FRAME-NAME}).
   tthandle = TEMP-TABLE depatemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEPA", INPUT " ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEPA", INPUT " ").
   END.
   FOR EACH depatemp WHERE depatemp.dep-nr = ? OR depatemp.dep-nr = 0 NO-LOCK:
      DELETE depatemp.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN UPPLAGAPP.P PERSISTENT SET upplagapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN UPPLAGAPP.P PERSISTENT SET upplagapph.
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
  DISPLAY FILL-IN-UPPL2 FILL-IN-UPPL FILL-IN-ADR FILL-IN-PNR FILL-IN-ORT 
          FILL-IN-XKORDCH FILL-IN-YKORDCH ED_ANM FILL-IN-ANM 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE FILL-IN-UPPL BRW_DEPA FILL-IN-ADR FILL-IN-PNR FILL-IN-ORT 
         FILL-IN-XKORDCH FILL-IN-YKORDCH ED_ANM BTN_OK BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

