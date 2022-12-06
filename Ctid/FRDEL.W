&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 05/13/96 -  3:17 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}
&Scoped-define NEW NEW
{FRDELTEMP.I}
DEFINE NEW SHARED VARIABLE bortrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE placerarec AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_FRDEL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES frdeltemp

/* Definitions for BROWSE BRW_FRDEL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_FRDEL frdeltemp.PERSONALKOD ~
frdeltemp.FORNAMN frdeltemp.EFTERNAMN frdeltemp.AONR frdeltemp.DELNR ~
frdeltemp.PROCENT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRDEL frdeltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FRDEL frdeltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FRDEL frdeltemp
&Scoped-define QUERY-STRING-BRW_FRDEL FOR EACH frdeltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FRDEL OPEN QUERY BRW_FRDEL FOR EACH frdeltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FRDEL frdeltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRDEL frdeltemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_FRDEL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FRDEL BTN_NY BTN_AND BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FRDEL FOR 
      frdeltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FRDEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRDEL DIALOG-1 _STRUCTURED
  QUERY BRW_FRDEL NO-LOCK DISPLAY
      frdeltemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      frdeltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      frdeltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
      frdeltemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      frdeltemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      frdeltemp.PROCENT COLUMN-LABEL "Procent" FORMAT ">99":U
  ENABLE
      frdeltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 73.5 BY 9.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_FRDEL AT ROW 1.5 COL 1.5
     BTN_NY AT ROW 11.25 COL 8.5
     BTN_AND AT ROW 11.25 COL 21.75
     BTN_BORT AT ROW 11.25 COL 34.88
     BTN_AVB AT ROW 11.25 COL 61
     SPACE(2.49) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Deltidsfrånvaro".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: frdeltemp T "?" NO-UNDO temp-db frdeltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_FRDEL 1 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRDEL
/* Query rebuild information for BROWSE BRW_FRDEL
     _TblList          = "Temp-Tables.frdeltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.frdeltemp.PERSONALKOD
"PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.frdeltemp.FORNAMN
"FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.frdeltemp.EFTERNAMN
"EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.frdeltemp.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.frdeltemp.DELNR
"DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.frdeltemp.PROCENT
"PROCENT" "Procent" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_FRDEL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Deltidsfrånvaro */
DO:
   {BORTBRWPROC.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Deltidsfrånvaro */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND DIALOG-1
ON CHOOSE OF BTN_AND IN FRAME DIALOG-1 /* Ändra */
DO:
  RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRDEL
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
   placerarec = ?.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FRALLT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 0,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
   END.
   ELSE DO: 
      RUN FRALLT.P 
      (INPUT 0,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
   END.
   RUN enable_UI.       
   {FRMSIZED.I}                          
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {musarrow.i}  
   APPLY "HOME" TO {&BROWSE-NAME}.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
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
   frdeltemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_FRDEL = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FRDEL:HANDLE IN FRAME {&FRAME-NAME}).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   placerarec = frdeltemp.RECIDVIS.
   {AVBGOMD.I}
   RUN FRDNY.W (INPUT-OUTPUT placerarec). 
   {AVBFRAMD.I}
   IF musz = FALSE THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:
         RUN FRALLT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 2,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      ELSE DO: 
         RUN FRALLT.P 
         (INPUT 2,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST frdeltemp WHERE frdeltemp.RECIDVIS = placerarec NO-LOCK NO-ERROR. 
      IF AVAILABLE frdeltemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(frdeltemp)).
         RUN lastselectdyn_UI IN brwproc[1].                     
      END.
   END.
   musz = FALSE.      
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   bortrec = frdeltemp.RECIDVIS
   placerarec = frdeltemp.RECIDVIS.      
   MESSAGE "Vill du verkligen ta bort denna deltidsfrånvaro?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE frdeltemp.PERSONALKOD
   UPDATE answer AS LOGICAL.
   {muswait.i}
   IF answer THEN DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN placerarec = frdeltemp.RECIDVIS.
      ELSE DO:          
         status-ok = {&BROWSE-NAME}:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN placerarec = frdeltemp.RECIDVIS.          
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN FRALLT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 3,INPUT-OUTPUT bortrec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      ELSE DO: 
         RUN FRALLT.P 
         (INPUT 3,INPUT-OUTPUT bortrec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST frdeltemp WHERE frdeltemp.RECIDVIS = placerarec NO-LOCK NO-ERROR. 
      IF AVAILABLE frdeltemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(frdeltemp)).
         RUN lastselectdyn_UI IN brwproc[1].                          
      END.  
   END.
   {musarrow.i}
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
  ENABLE BRW_FRDEL BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   placerarec = ?.
   {AVBGOMD.I}
   RUN FRDNY.W (INPUT-OUTPUT placerarec).
   {AVBFRAMD.I}
   IF musz = FALSE THEN DO: 
      IF Guru.Konstanter:appcon THEN DO:
         RUN FRALLT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      ELSE DO: 
         RUN FRALLT.P 
         (INPUT 1,INPUT-OUTPUT placerarec,INPUT-OUTPUT TABLE frdeltemp).
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST frdeltemp WHERE frdeltemp.RECIDVIS = placerarec NO-LOCK NO-ERROR. 
      IF AVAILABLE frdeltemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(frdeltemp)).
         RUN lastselectdyn_UI IN brwproc[1].                     
      END.           
   END.
   musz = FALSE.    
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

