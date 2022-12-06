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

  Created: 95/09/06 -  1:03 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{PHMT.I}
{OMRTEMPW.I}
{TIDPERS.I}
{DIRDEF.I}
{SOKDEF.I}
DEFINE  SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE  SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE tidvis NO-UNDO   
   FIELD EFTERNAMN AS CHARACTER  FORMAT "X(15)"
   FIELD FORNAMN AS CHARACTER FORMAT "X(15)" 
   FIELD PERSONALKOD AS CHARACTER FORMAT "X(5)"
   FIELD AONR AS CHARACTER FORMAT "X(6)"
   FIELD DELNR AS INTEGER FORMAT ">99"
   FIELD ORT AS CHARACTER FORMAT "X(15)"
   FIELD START AS DECIMAL FORMAT "99.99"
   FIELD SLUT AS DECIMAL FORMAT "99.99"
   FIELD DAG AS CHARACTER FORMAT "X(3)" 
   FIELD DATUM AS DATE     
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_SENASTE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidvis

/* Definitions for BROWSE BRW_SENASTE                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_SENASTE tidvis.PERSONALKOD ~
tidvis.EFTERNAMN tidvis.AONR tidvis.DELNR tidvis.ORT tidvis.START ~
tidvis.SLUT tidvis.DAG tidvis.DATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SENASTE 
&Scoped-define QUERY-STRING-BRW_SENASTE FOR EACH tidvis NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SENASTE OPEN QUERY BRW_SENASTE FOR EACH tidvis NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SENASTE tidvis
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SENASTE tidvis


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_SENASTE}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_SENASTE BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 63.5 BY 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_SENASTE FOR 
      tidvis SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_SENASTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SENASTE DIALOG-1 _STRUCTURED
  QUERY BRW_SENASTE NO-LOCK DISPLAY
      tidvis.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 8
      tidvis.EFTERNAMN COLUMN-LABEL "Namn" FORMAT "X(15)":U
      tidvis.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      tidvis.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      tidvis.ORT COLUMN-LABEL "Ort" FORMAT "X(256)":U WIDTH 15
      tidvis.START COLUMN-LABEL "Start" FORMAT "99.99":U
      tidvis.SLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
      tidvis.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      tidvis.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 75.5 BY 12.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-REGIS AT ROW 3.54 COL 2.88 NO-LABEL
     FILL-IN_NAMN AT ROW 3.54 COL 21.38 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_FORNAMN AT ROW 3.54 COL 21.38 COLON-ALIGNED NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 3.54 COL 42.88 COLON-ALIGNED NO-LABEL
     BRW_SENASTE AT ROW 5.29 COL 1.5
     RAD_ALLVAL AT ROW 15.96 COL 2.5 NO-LABEL
     BTN_AVB AT ROW 18.75 COL 63
     "Senaste tidregistreringen för:" VIEW-AS TEXT
          SIZE 52.5 BY 1.5 AT ROW 1.04 COL 1.5
          FONT 17
     SPACE(24.12) SKIP(17.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidvis
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_SENASTE FILL-IN_EFTERNAMN DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_SENASTE:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300
       BRW_SENASTE:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_SENASTE:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_EFTERNAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SENASTE
/* Query rebuild information for BROWSE BRW_SENASTE
     _TblList          = "Temp-Tables.tidvis"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.tidvis.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidvis.EFTERNAMN
"EFTERNAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidvis.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidvis.DELNR
"DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidvis.ORT
"ORT" "Ort" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidvis.START
"START" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidvis.SLUT
"SLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidvis.DAG
"DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tidvis.DATUM
"DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_SENASTE */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   {BORTBRWPROC.I}
   /*RETURN.   */
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SENASTE
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
   FOR EACH tidvis:
      DELETE tidvis.
   END.   
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF RAD_ALLVAL = 1 THEN DO:
      FILL-IN-REGIS = "Ansvarig:".
      FIND FIRST ansvarigtemp WHERE 
      ansvarigtemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      FILL-IN_EFTERNAMN = ansvarigtemp.EFTERNAMN.
      FILL-IN_FORNAMN = ansvarigtemp.FORNAMN.
      FILL-IN_EFTERNAMN:HIDDEN = FALSE.
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE 
      omrtemp.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      FILL-IN_NAMN:HIDDEN = FALSE.
      FILL-IN_NAMN = omrtemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = "".
   END.
   IF RAD_ALLVAL = 6 THEN DO:
      FILL-IN-REGIS = "Godkänner tid".
      FIND FIRST godkannartemp WHERE 
      godkannartemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      FILL-IN_EFTERNAMN = godkannartemp.EFTERNAMN.
      FILL-IN_FORNAMN = godkannartemp.FORNAMN.
      FILL-IN_EFTERNAMN:HIDDEN = FALSE.
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   tidvis.AONR:LABEL IN BROWSE BRW_SENASTE = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN tidvis.AONR
   &Scoped-define BROWSE-NAME BRW_SENASTE
   {AOFORMAT1.I}

   status-ok = RAD_ALLVAL:DELETE("Ansvarig tidredovisare").
   status-ok = RAD_ALLVAL:DELETE("Område").
   status-ok = RAD_ALLVAL:DELETE("Alla").
   status-ok = RAD_ALLVAL:DELETE("Enhet/Sign").
   status-ok = RAD_ALLVAL:DELETE("Markerade").
   RAD_ALLVAL:ADD-LAST("Ansvarig tidredovisare",1).
   RAD_ALLVAL:ADD-LAST(Guru.Konstanter:gomrk,2).
   RAD_ALLVAL:ADD-LAST("Alla",3).
   RAD_ALLVAL:ADD-LAST("Enhet/Sign",4).
   RAD_ALLVAL:ADD-LAST("Markerade",5).
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = "Markerade enheter".
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK :
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 5
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD.      
      {SOKANROP.I}      
      IF soktemp.SOKCHAR[1] = "XX" THEN NEXT. 
      CREATE tidvis.      
      IF AVAILABLE tidvis THEN DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = soktemp.SOKCHAR[2] AND
         utsokaonr.DELNR = soktemp.SOKINT[1] USE-INDEX AONR NO-LOCK NO-ERROR.
         ASSIGN 
         tidvis.PERSONALKOD = tidpers.PERSONALKOD         
         tidvis.EFTERNAMN = SUBSTRING(tidpers.FORNAMN,1,1) + " " + tidpers.EFTERNAMN        
         tidvis.AONR = soktemp.SOKCHAR[2]
         tidvis.DELNR = soktemp.SOKINT[1]           
         tidvis.START = soktemp.SOKDECI[1]       
         tidvis.SLUT =  soktemp.SOKDECI[2]     
         tidvis.DAG = soktemp.SOKCHAR[3]      
         tidvis.DATUM = soktemp.SOKDATE[1].
          IF AVAILABLE utsokaonr THEN DO:
            tidvis.ORT = utsokaonr.ORT.         
         END.
      END.          
   END.   /*FOR*/  
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
   (INPUT BRW_SENASTE:HANDLE IN FRAME {&FRAME-NAME}).   
   
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
  DISPLAY FILL-IN-REGIS 
      WITH FRAME DIALOG-1.
  ENABLE BRW_SENASTE BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

