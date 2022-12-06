&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{DAGBOKTEMP.I}
/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER avbryt AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR edagboktemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-TIMMAR FILL-IN-STARTDAT BTN_NVE ~
FILL-IN-KM BTN_FVE EDT_DAG BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TIMMAR FILL-IN-STARTDAT FILL-IN-KM ~
EDT_DAG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE EDT_DAG AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 53.5 BY 13.5 NO-UNDO.

DEFINE VARIABLE FILL-IN-KM AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Kilometer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIMMAR AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Antal timmar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-TIMMAR AT ROW 1.21 COL 13.75 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 1.21 COL 36.5 COLON-ALIGNED
     BTN_NVE AT ROW 1.08 COL 49.25 NO-TAB-STOP 
     FILL-IN-KM AT ROW 2.46 COL 13.75 COLON-ALIGNED
     BTN_FVE AT ROW 1.92 COL 49.25 NO-TAB-STOP 
     EDT_DAG AT ROW 3.71 COL 1.5 NO-LABEL
     BTN_REG AT ROW 18.21 COL 26
     BTN_AVS AT ROW 18.21 COL 41
     SPACE(0.37) SKIP(0.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Dagboksanteckningar":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Dagboksanteckningar */
DO:
   avbryt = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Dagboksanteckningar */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   
   avbryt = TRUE.
   APPLY "GO" TO BTN_AVS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON GO OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN
   avbryt = FALSE
   edagboktemp.TIMMAR = FILL-IN-TIMMAR
   edagboktemp.DATUM = FILL-IN-STARTDAT 
   edagboktemp.KILOMETER = FILL-IN-KM
   edagboktemp.DAGBOKAO = INPUT EDT_DAG
   edagboktemp.ANVANDARE = Guru.Konstanter:globanv.   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDT_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDT_DAG DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF EDT_DAG IN FRAME DIALOG-1
DO:
   DEFINE VARIABLE ecopy AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ecopystart AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ecopyindex AS CHARACTER NO-UNDO.
   DEFINE VARIABLE eline AS INTEGER NO-UNDO.
   DEFINE VARIABLE ewidth AS INTEGER NO-UNDO.
   ecopystart = EDT_DAG:SELECTION-TEXT.

   IF EDT_DAG:LENGTH > EDT_DAG:INNER-CHARS THEN ewidth = EDT_DAG:LENGTH. 
   ELSE ewidth = EDT_DAG:INNER-CHARS.
  
   eline = EDT_DAG:CURSOR-LINE.
   
   EDT_DAG:SET-SELECTION(EDT_DAG:CONVERT-TO-OFFSET(eline, 1),EDT_DAG:CONVERT-TO-OFFSET(eline, ewidth)).
   ecopy = EDT_DAG:SELECTION-TEXT.
   IF ecopy BEGINS ecopystart THEN. 
   ELSE DO:
       ecopy = ecopystart + ecopy.
   END.
   IF INDEX(ecopy,CHR(10)) > 0 THEN DO:
       ecopyindex = SUBSTRING(ecopy,1,INDEX(ecopy,CHR(10))).
       ecopy = ecopyindex.
   END.
   EDT_DAG:SEARCH(ecopy,16).  
  
   
   
   ecopy = TRIM(ecopy).
   IF ecopy = "" THEN .
   ELSE  RUN OPENDOC.P (ecopy,"","",NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDT_DAG DIALOG-1
ON RIGHT-MOUSE-CLICK OF EDT_DAG IN FRAME DIALOG-1
DO:
   RUN OPENDOC.P (EDT_DAG:SELECTION-TEXT,"","",NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KM DIALOG-1
ON LEAVE OF FILL-IN-KM IN FRAME DIALOG-1 /* Kilometer */
DO:
   FILL-IN-KM = INPUT FILL-IN-KM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Datum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TIMMAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TIMMAR DIALOG-1
ON LEAVE OF FILL-IN-TIMMAR IN FRAME DIALOG-1 /* Antal timmar */
DO:
   FILL-IN-TIMMAR = INPUT FILL-IN-TIMMAR.
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
   FIND FIRST edagboktemp NO-LOCK NO-ERROR.
   
   ASSIGN 
   FILL-IN-TIMMAR = edagboktemp.TIMMAR
   FILL-IN-STARTDAT = edagboktemp.DATUM
   FILL-IN-KM = edagboktemp.KILOMETER
   EDT_DAG = edagboktemp.DAGBOKAO.
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
  DISPLAY FILL-IN-TIMMAR FILL-IN-STARTDAT FILL-IN-KM EDT_DAG 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-TIMMAR FILL-IN-STARTDAT BTN_NVE FILL-IN-KM BTN_FVE EDT_DAG 
         BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

