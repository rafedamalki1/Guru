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
&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}
DEFINE INPUT PARAMETER TABLE FOR extratemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flexdagtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR flextemp.   

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE flrec AS RECID NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE flexavikapph AS HANDLE NO-UNDO.

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
FILL-IN_DELNR FILL-IN_KNAPP FILL-IN_DATUM FILL-IN_TID 

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

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_KNAPP AS CHARACTER FORMAT "X(12)" 
     LABEL "Knapp" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_TID AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_PERSONALKOD AT ROW 3.33 COL 12 COLON-ALIGNED
     FILL-IN_AONR AT ROW 4.83 COL 12 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 4.83 COL 30.75 COLON-ALIGNED
     FILL-IN_KNAPP AT ROW 6.17 COL 45.75 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 6.25 COL 12.13 COLON-ALIGNED
     FILL-IN_TID AT ROW 6.25 COL 30.75 COLON-ALIGNED
     BTN_OK AT ROW 7.54 COL 31.75
     BTN_AVB AT ROW 7.54 COL 46.75
     "Vill du verkligen ta bort denna registrering?" VIEW-AS TEXT
          SIZE 60.75 BY 1.63 AT ROW 1 COL 1.5
          FONT 17
     SPACE(0.00) SKIP(6.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag av flexregistrering":L.


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
/* SETTINGS FOR FILL-IN FILL-IN_DATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_KNAPP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TID IN FRAME DIALOG-1
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
ON END-ERROR OF FRAME DIALOG-1 /* Borttag av flexregistrering */
DO:
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag av flexregistrering */
DO:
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
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
   bdatum = flextemp.DATUM.     
   FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = flextemp.PERSONALKOD AND
   flexdagtemp.DATUM = flextemp.DATUM USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE flexdagtemp THEN DO:
      IF flexdagtemp.KONTROLL = "Kontroll" THEN DO:
         ASSIGN 
         flexdagtemp.KONTROLL = "Ejkontroll" 
         flexdagtemp.FELOK = FALSE.             
      END.   
      ASSIGN 
      flexdagtemp.PLUS = 0 
      flexdagtemp.FLARB = 0 
      flexdagtemp.FELMED = "".
   END.   
   IF flextemp.KOM = TRUE THEN ASSIGN flexdagtemp.START = regstart.
   IF flextemp.GICK = TRUE THEN ASSIGN flexdagtemp.SLUT = regslut.     
   flrec = RECID(flextemp).
   FIND FIRST flextemp WHERE flextemp.DATUM = bdatum AND 
   RECID(flextemp) NE flrec USE-INDEX FLEX NO-LOCK NO-ERROR.
   IF NOT AVAILABLE flextemp THEN DO:
      FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = extratemp.PERSONALKOD AND
      flexdagtemp.DATUM = bdatum USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE flexdagtemp THEN DO:
         ASSIGN flexdagtemp.FELMED = "Ingen registrering gjord".        
      END.      
   END.
   EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
   CREATE eflexdagtemp.
   BUFFER-COPY flexdagtemp TO eflexdagtemp.
   RUN flexdagok_IU IN flexavikapph (INPUT TABLE eflexdagtemp).
   EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.   
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
   
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
   END.
   ELSE DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph.
   END.   
   FIND FIRST extratemp NO-ERROR.
   FIND FIRST flextemp WHERE flextemp.PERSONALKOD = extratemp.PERSONALKOD AND 
   flextemp.DATUM = extratemp.DATUM AND flextemp.TID = extratemp.TID AND 
   flextemp.KNAPP = extratemp.KNAPP  NO-ERROR.    
   ASSIGN
   FILL-IN_AONR = flextemp.AONR
   FILL-IN_DATUM = flextemp.DATUM 
   FILL-IN_DELNR = flextemp.DELNR 
   FILL-IN_PERSONALKOD = flextemp.PERSONALKOD
   FILL-IN_KNAPP = flextemp.KNAPP
   FILL-IN_TID = flextemp.TID. 
   ASSIGN regdatum = flextemp.DATUM.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I} 
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   pkod = extratemp.PERSONALKOD.
   {SLUTARBW.I}
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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_AONR FILL-IN_DELNR FILL-IN_KNAPP 
          FILL-IN_DATUM FILL-IN_TID 
      WITH FRAME DIALOG-1.
  ENABLE BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

