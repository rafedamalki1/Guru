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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE VARIABLE modantapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN TOG_BER btn_ok ~
FILL-IN-ENR FILL-IN-BEN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL TOG_BER FILL-IN-ENR ~
FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_ok 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_BER AS LOGICAL INITIAL no 
     LABEL "Beroende av kopplingstyp" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.5 BY 1.13 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_UP AT ROW 5.75 COL 22.25
     FILL-IN-ANTAL AT ROW 6.25 COL 11.75 COLON-ALIGNED
     BTN_MIN AT ROW 6.92 COL 22.25
     TOG_BER AT ROW 8.08 COL 13.88
     btn_ok AT ROW 9.5 COL 33.38
     FILL-IN-ENR AT ROW 1.75 COL 11.75 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.25 COL 11.75 COLON-ALIGNED
     SPACE(1.49) SKIP(6.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uppdatera antal moduler":L
         DEFAULT-BUTTON btn_ok.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Uppdatera antal moduler */
DO:
   IF VALID-HANDLE(modantapph) THEN DELETE PROCEDURE modantapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Uppdatera antal moduler */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-1
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-1 /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* OK */
DO:   
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   felmedd = "".  
   ASSIGN
   TOG_BER = INPUT TOG_BER.   
   RUN btnok_UI IN modantapph (INPUT mtrl_rowid,INPUT musz,
                            INPUT TOG_BER,INPUT FILL-IN-ANTAL,
                            OUTPUT felmedd).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   ELSE DO:
      ASSIGN
      mtrlbertemp.MODUL = FILL-IN-ANTAL
      mtrlbertemp.TYPBER = TOG_BER.
   END.   
   IF VALID-HANDLE(modantapph) THEN DELETE PROCEDURE modantapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-1
ON CHOOSE OF BTN_UP IN FRAME DIALOG-1 /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
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
      RUN MODANTAPP.P PERSISTENT SET modantapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MODANTAPP.P PERSISTENT SET modantapph.
   END.       
   FIND FIRST mtrlbertemp WHERE mtrlbertemp.MTRLROW = mtrl_rowid NO-ERROR.
   IF AVAILABLE mtrlbertemp THEN DO:
      ASSIGN
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk 
      FILL-IN-BEN = mtrlbertemp.BENAMNING
      FILL-IN-ENR = mtrlbertemp.ENR  
      FILL-IN-ANTAL = mtrlbertemp.MODUL
      TOG_BER = mtrlbertemp.TYPBER.
   END.  
   RUN enable_UI.       
   {FRMSIZED.I} 
   IF musz = TRUE THEN TOG_BER:HIDDEN = TRUE.
   {musarrow.i}   
   {DIA_M_SLUT.I}
    APPLY "ENTRY" TO FILL-IN-ANTAL.
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
  DISPLAY FILL-IN-ANTAL TOG_BER FILL-IN-ENR FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN TOG_BER btn_ok FILL-IN-ENR FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

