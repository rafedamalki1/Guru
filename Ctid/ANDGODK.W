&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2






&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/28 -  9:26 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}
{TIDPERS.I}
{SOKDEF.I}
{GODTEMP.I}
{OMRTEMPW.I}
{PHMT.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE VARIABLE brwavdatum AS DATE NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_TID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES godmarkpers

/* Definitions for BROWSE BRW_TID                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TID godmarkpers.PERSONALKOD ~
godmarkpers.FORNAMN godmarkpers.EFTERNAMN godmarkpers.GODKAND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TID 
&Scoped-define QUERY-STRING-BRW_TID FOR EACH godmarkpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TID OPEN QUERY BRW_TID FOR EACH godmarkpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TID godmarkpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TID godmarkpers


/* Definitions for DIALOG-BOX DIALOG-2                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_GODA BTN_GODM BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GODA 
     LABEL "Alla i bilden":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GODM 
     LABEL "Markerade i bilden":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(15)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-T1 AS CHARACTER FORMAT "X(256)":U INITIAL "Det finns inga tidsedlar, som är godkända" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-T2 AS CHARACTER FORMAT "X(256)":U INITIAL "och ej  eko.- och lönesammanst." 
     VIEW-AS FILL-IN 
     SIZE 45.5 BY 1.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_VECKONUMMER AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Vecka" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Godkänner tidsedel", 6,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 54.88 BY 1.17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TID FOR 
      godmarkpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TID DIALOG-2 _STRUCTURED
  QUERY BRW_TID NO-LOCK DISPLAY
      godmarkpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "X(5)":U
      godmarkpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      godmarkpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(25)":U
      godmarkpers.GODKAND COLUMN-LABEL "Godkänd" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 57 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN-REGIS AT ROW 3.46 COL 1.63 COLON-ALIGNED NO-LABEL
     FILL-IN_NAMN AT ROW 3.46 COL 20.5 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_FORNAMN AT ROW 3.46 COL 20.75 COLON-ALIGNED NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 3.46 COL 42.63 COLON-ALIGNED NO-LABEL
     FILL-IN-AR AT ROW 5.46 COL 9.5 COLON-ALIGNED NO-LABEL
     FILL-IN_VECKONUMMER AT ROW 5.46 COL 9.63 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 5.46 COL 18.75 COLON-ALIGNED NO-LABEL
     BRW_TID AT ROW 7.17 COL 1.5
     FILL-IN-T1 AT ROW 10.17 COL 12.5 COLON-ALIGNED NO-LABEL
     FILL-IN-T2 AT ROW 11.92 COL 12.5 COLON-ALIGNED NO-LABEL
     RAD_ALLVAL AT ROW 13.79 COL 2.63 NO-LABEL
     BTN_GODA AT ROW 18 COL 1.63
     BTN_GODM AT ROW 18 COL 18.25
     BTN_AVS AT ROW 18 COL 60
     "Möjliggöra ändring av godkända tidsedlar för:" VIEW-AS TEXT
          SIZE 60.13 BY 1.08 AT ROW 1.71 COL 1.5
          FONT 17
     "Ta bort godkänd markering för:" VIEW-AS TEXT
          SIZE 30 BY 1 AT ROW 16.92 COL 1.5
     SPACE(44.24) SKIP(1.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra godkända tidsedlar":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db godmarkpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_TID FILL-IN-MANAD DIALOG-2 */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_TID IN FRAME DIALOG-2
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TID:HIDDEN  IN FRAME DIALOG-2                = TRUE
       BRW_TID:MAX-DATA-GUESS IN FRAME DIALOG-2         = 300.

/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-T1 IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-T1:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-T2 IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-T2:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_EFTERNAMN:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VECKONUMMER IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VECKONUMMER:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TID
/* Query rebuild information for BROWSE BRW_TID
     _TblList          = "Temp-Tables.godmarkpers"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.godmarkpers.PERSONALKOD
"godmarkpers.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.godmarkpers.FORNAMN
"godmarkpers.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.godmarkpers.EFTERNAMN
"godmarkpers.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.godmarkpers.GODKAND
"godmarkpers.GODKAND" "Godkänd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON WINDOW-CLOSE OF FRAME DIALOG-2 /* Ändra godkända tidsedlar */
DO:
   {BORTBRWPROC.I}
  RETURN.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-2
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-2 /* Avsluta */
DO:
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODA DIALOG-2
ON CHOOSE OF BTN_GODA IN FRAME DIALOG-2 /* Alla i bilden */
DO:
   {muswait.i}
   MESSAGE "Vill du verkligen ta bort godkänningen för alla i bilden ? " 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         musz = musz.          
      END.
      WHEN FALSE THEN DO:                  
         RETURN NO-APPLY.
      END.
   END CASE.      
   IF NOT AVAILABLE godmarkpers THEN FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   ASSIGN
   tidtabrec = RECID(godmarkpers)
   regdatum = TODAY.
   RUN REGVEC.P.   
   EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
   FOR EACH godmarkpers USE-INDEX PERSONALKOD NO-LOCK:
      IF godmarkpers.GODKAND NE "" THEN DO:
         CREATE appmarkpers.
         ASSIGN appmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD
         appmarkpers.VECKONUMMER = godmarkpers.VECKONUMMER.
      END.
   END.                  
   {ANDGODIN.I}   
   FOR EACH godmarkpers USE-INDEX PERSONALKOD EXCLUSIVE-LOCK:
      ASSIGN godmarkpers.GODKAND = "".
   END.                   
   RUN refreshbrw_UI IN brwproc[1].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODM DIALOG-2
ON CHOOSE OF BTN_GODM IN FRAME DIALOG-2 /* Markerade i bilden */
DO:   
   {muswait.i}   
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS. 
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END. 
   ELSE DO:   
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      ASSIGN
      antal_raknare = 1
      regdatum = TODAY.
      RUN REGVEC.P.      
      DO WHILE antal_raknare LE antal_valda :
         status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).
         IF godmarkpers.GODKAND NE "" THEN DO:                        
            CREATE appmarkpers.
            ASSIGN 
            appmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD
            appmarkpers.VECKONUMMER = godmarkpers.VECKONUMMER.           
         END.         
         ASSIGN godmarkpers.GODKAND = "".       
         antal_raknare = antal_raknare + 1.
      END.          
      {ANDGODIN.I}
      tidtabrec = RECID(godmarkpers).   
      
      RUN refreshbrw_UI IN brwproc[1].
      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TID
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   EMPTY TEMP-TABLE godmarkpers NO-ERROR. 
   
   IF Guru.Konstanter:appcon THEN DO:                                                    
      RUN ANDGODAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT regar, INPUT regmnr,INPUT regvnr,INPUT Guru.Konstanter:globanv,
      INPUT TABLE tidpers,OUTPUT TABLE godmarkpers).   
   END.
   ELSE DO:
      RUN ANDGODAP.P  
      (INPUT regar, INPUT regmnr,INPUT regvnr,INPUT Guru.Konstanter:globanv,
       INPUT TABLE tidpers,OUTPUT TABLE godmarkpers).
   END.
   
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN LEAVE MAIN-BLOCK.   
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.   
 
   persrec = tidpers.TIDPERSREC. 
   IF RAD_ALLVAL = 1 THEN DO:
      FILL-IN-REGIS = "Ansvarig:".
      FIND FIRST ansvarigtemp WHERE ansvarigtemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = ansvarigtemp.EFTERNAMN
      FILL-IN_FORNAMN = ansvarigtemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_NAMN:HIDDEN = FALSE
      FILL-IN_NAMN = omrtemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = "".
   END.
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = "Markerade enheter".
   IF RAD_ALLVAL = 6 THEN DO:
      FILL-IN-REGIS = "Godkänner tidsedel:".
      FIND FIRST godkannartemp WHERE godkannartemp.PERSONALKOD = personaltemp.TIDSGODK NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = godkannartemp.EFTERNAMN
      FILL-IN_FORNAMN = godkannartemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   ASSIGN
   FILL-IN-AR = regar 
   FILL-IN-MANAD = regmannamn
   FILL-IN_VECKONUMMER = regvnr.   
   RUN enable_UI.       
   {FRMSIZED.I}  
   FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE godmarkpers THEN DO:       
      ASSIGN
      FILL-IN-T1:HIDDEN = FALSE
      FILL-IN-T2:HIDDEN = FALSE
      BRW_TID:HIDDEN = TRUE.
      DISABLE BTN_GODA BTN_GODM WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-T1 FILL-IN-T2 WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:    
      OPEN QUERY {&BROWSE-NAME} FOR EACH godmarkpers 
      USE-INDEX PERSONALKOD NO-LOCK .    
      ENABLE BRW_TID WITH FRAME {&FRAME-NAME}.
      BRW_TID:HIDDEN = FALSE.
      APPLY "HOME" TO BRW_TID.
      FIND FIRST godmarkpers WHERE godmarkpers.PERSONALKOD = pkod
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE godmarkpers THEN DO:
         FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      END.
      RUN repo_UI (INPUT RECID(godmarkpers)).
      status-ok = BRW_TID:SELECT-FOCUSED-ROW() NO-ERROR.    
   END.   
   DISPLAY FILL-IN-AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.    
   {musarrow.i}  
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TID:HANDLE IN FRAME {&FRAME-NAME}).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-REGIS 
      WITH FRAME DIALOG-2.
  ENABLE BTN_GODA BTN_GODM BTN_AVS 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-2 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

