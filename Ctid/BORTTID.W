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

  Created: 95/05/03 -  2:05 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER tidalltrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{PHMT.I}
{TIDALLT.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE BUFFER tidbuff FOR tidallt.
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE vadbort AS INTEGER NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD FILL-IN_AONR ~
FILL-IN_DELNR FILL-IN_START FILL-IN_SLUT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(8)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_DAG AS CHARACTER FORMAT "X(8)" 
     LABEL "DAG" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_SLUT AS DECIMAL FORMAT "->>>>9.99" INITIAL 16 
     LABEL "Slut tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_START AS DECIMAL FORMAT "->>>>9.99" INITIAL 7 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_PERSONALKOD AT ROW 3.33 COL 12 COLON-ALIGNED
     FILL-IN_AONR AT ROW 4.83 COL 12 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 4.83 COL 33 COLON-ALIGNED
     FILL-IN_DAG AT ROW 6.33 COL 12 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 6.33 COL 12 COLON-ALIGNED
     FILL-IN_START AT ROW 6.33 COL 33 COLON-ALIGNED
     FILL-IN_SLUT AT ROW 6.33 COL 50.5 COLON-ALIGNED
     BTN_OK AT ROW 8 COL 29.5
     BTN_AVB AT ROW 8 COL 44.5
     "  Vill du verkligen ta bort denna registrering?" VIEW-AS TEXT
          SIZE 53.38 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     SPACE(4.74) SKIP(6.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag av tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DAG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SLUT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_START IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
ON END-ERROR OF FRAME DIALOG-1 /* Borttag av tidregistrering */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag av tidregistrering */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   {muswait.i}    
   vadbort = 1.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
      FIND tidallt WHERE RECID(tidallt) = tidalltrec NO-LOCK NO-ERROR.
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD AND  flexavttemp.FLEXTID = TRUE  USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp AND tidallt.OVERTIDUTTAG = "F" AND tidallt.TIDLOG = TRUE THEN DO:                             
         musz = FALSE.
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = tidallt.PERSONALKOD
         AND tidbuff.DATUM = tidallt.DATUM AND tidbuff.SLUT LE tidallt.START
         AND tidbuff.OVERTIDUTTAG = "F" USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff  THEN musz = TRUE.         
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = tidallt.PERSONALKOD AND 
         tidbuff.DATUM = tidallt.DATUM AND tidbuff.START GE tidallt.SLUT AND 
         tidbuff.OVERTIDUTTAG = "F" USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff THEN musz = TRUE.            
         IF musz = TRUE THEN DO:            
            MESSAGE "Denna person har flexavtal. Vid borttag av tid som kommer från flex rensas alla dagens flexregistreringar och tidregistreringar!" SKIP
            "Vill du ta bort alla dagens flexregistreringar och tillhörande tidregistreringar ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE val AS LOGICAL.   
            IF val = FALSE THEN RETURN NO-APPLY.
            ELSE DO:
               /*ta bort alla flex och tidregistreringar med overtiduttag = "F" denna dag */
               vadbort = 11.
            END. 
         END.
         musz = FALSE.
               
      END.   
   END.     
   EMPTY TEMP-TABLE extratidallt NO-ERROR. 
   CREATE extratidallt.
   BUFFER-COPY tidallt TO extratidallt.
   DELETE tidallt.
   IF Guru.Konstanter:appcon THEN DO:
      RUN TIDBORT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT Guru.Konstanter:globanv,INPUT vadbort,INPUT brwbdatum,INPUT brwavdatum,INPUT TABLE extratidallt,
      OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
   END.
   ELSE DO: 
      RUN TIDBORT.P
      (INPUT Guru.Konstanter:globanv,INPUT vadbort,INPUT brwbdatum,INPUT brwavdatum,INPUT TABLE extratidallt,
      OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
   END. 
   
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
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR. 

   FIND tidallt WHERE RECID(tidallt) = tidalltrec NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_AONR = tidallt.AONR
   FILL-IN_DAG = tidallt.DAG
   FILL-IN_DATUM = tidallt.DATUM 
   FILL-IN_DELNR = tidallt.DELNR 
   FILL-IN_PERSONALKOD = tidallt.PERSONALKOD
   FILL-IN_SLUT = tidallt.SLUT 
   FILL-IN_START = tidallt.START.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
  
   RUN enable_UI.       
   {FRMSIZED.I}
   IF gvisatidpermanad = TRUE THEN DO:
      DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:
      DISPLAY FILL-IN_DAG WITH FRAME {&FRAME-NAME}. 
   END.
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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_AONR FILL-IN_DELNR FILL-IN_START 
          FILL-IN_SLUT 
      WITH FRAME DIALOG-1.
  ENABLE BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

