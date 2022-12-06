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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED
{SCHAKTADM.I}
{SMTRL.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE rordiaapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN btn_ok ~
FILL-IN-ENR FILL-IN-BEN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN 

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

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Diameter (i mm)" 
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_UP AT ROW 4.92 COL 30.13
     FILL-IN-ANTAL AT ROW 5.38 COL 18.38 COLON-ALIGNED
     BTN_MIN AT ROW 6.04 COL 30.13
     btn_ok AT ROW 7.5 COL 33
     FILL-IN-ENR AT ROW 1.75 COL 11.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.5 COL 11.88 COLON-ALIGNED
     SPACE(0.61) SKIP(4.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uppdatera linor/kablar":L
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
ON END-ERROR OF FRAME DIALOG-1 /* Uppdatera linor/kablar */
DO:
   IF VALID-HANDLE(rordiaapph) THEN DELETE PROCEDURE rordiaapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Uppdatera linor/kablar */
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
   {muswait.i} 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL. 
   IF musz = TRUE THEN DO:    
      IF AVAILABLE ekskyddtemp THEN DO:
         RUN btnok_UI IN rordiaapph (INPUT FILL-IN-ANTAL,
                                     INPUT-OUTPUT TABLE ekskyddtemp).  
      END.
      FIND NEXT ekskyddtemp WHERE ekskyddtemp.KLAR = FALSE USE-INDEX OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekskyddtemp THEN DO:
         APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ASSIGN 
         FILL-IN-BEN = ekskyddtemp.BENAMNING
         FILL-IN-ENR = ekskyddtemp.ENR   
         FILL-IN-ANTAL = ekskyddtemp.DIAMETER.  
         DISPLAY FILL-IN-BEN FILL-IN-ENR FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
         SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-ANTAL.
         RETURN NO-APPLY.
      END.      
   END.
   ELSE DO:     
      IF FILL-IN-ANTAL = 0 THEN DO:  
         espec_mtrl.ROR = FALSE.
      END.
      ELSE DO:
         espec_mtrl.ROR = TRUE.
      END.
      ASSIGN 
      espec_mtrl.DIAMETER = FILL-IN-ANTAL. 
      FIND NEXT espec_mtrl USE-INDEX ENR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE espec_mtrl THEN DO:
         APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ASSIGN 
         FILL-IN-BEN = espec_mtrl.BENAMNING
         FILL-IN-ENR = espec_mtrl.ENR   
         FILL-IN-ANTAL = espec_mtrl.DIAMETER. 
         DISPLAY FILL-IN-BEN FILL-IN-ENR FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
         SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-ANTAL.
         RETURN NO-APPLY.
      END. 
   END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON GO OF btn_ok IN FRAME DIALOG-1 /* OK */
DO:
   IF VALID-HANDLE(rordiaapph) THEN DELETE PROCEDURE rordiaapph.
   musz = FALSE.
   RETURN.
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
   {ALLSTARTDYN.I}
   IF musz = TRUE THEN DO:   
      FIND FIRST ekskyddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE ekskyddtemp THEN DO:
         ASSIGN 
         FILL-IN-BEN = ekskyddtemp.BENAMNING
         FILL-IN-ENR = ekskyddtemp.ENR   
         FILL-IN-ANTAL = ekskyddtemp.DIAMETER.   
      END.
   END.
   ELSE DO:    
      FIND FIRST espec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE espec_mtrl THEN DO:
         ASSIGN 
         FILL-IN-ENR:LABEL = Guru.Konstanter:genk
         FILL-IN-BEN = espec_mtrl.BENAMNING
         FILL-IN-ENR = espec_mtrl.ENR   
         FILL-IN-ANTAL = espec_mtrl.DIAMETER.  
      END.
   END.   
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
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/      
   IF Guru.Konstanter:appcon THEN DO:
      RUN RORDIAAPP.P PERSISTENT SET rordiaapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN RORDIAAPP.P PERSISTENT SET rordiaapph.
   END.     
  
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN btn_ok FILL-IN-ENR FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

