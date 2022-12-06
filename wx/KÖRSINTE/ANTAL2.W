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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{SOKMTRL.I}            
{SOKTEMPNOUN.I}
{SMTRL.I}
{MTRLTEMP.I}

DEFINE INPUT PARAMETER vad AS INTEGER.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR emtrl_mtrl.    /*vad = 1*/
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR esok_mtrl.     /*vad = 2*/
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR espec_mtrl.    /*vad = 3*/
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR emtrltemp.  /*vad = 4*/                   
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
/**/
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED 
{HOPPSEK2W.I}




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
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET FILL-IN-PRIS 

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

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Pris" 
      VIEW-AS TEXT 
     SIZE 13 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_UP AT ROW 7.04 COL 22.25
     FILL-IN-ANTAL AT ROW 7.5 COL 11.75 COLON-ALIGNED
     BTN_MIN AT ROW 8.17 COL 22.25
     btn_ok AT ROW 9.33 COL 32.75
     FILL-IN-ENR AT ROW 1.75 COL 11.75 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.25 COL 11.75 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.75 COL 11.75 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 6 COL 11.75 COLON-ALIGNED
     SPACE(21.24) SKIP(4.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uppdatera Antal":L
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

&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-1
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-1 /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL > 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 1." VIEW-AS ALERT-BOX.
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
   IF vad = 4 THEN DO:
      FIND spec_mtrl WHERE spec_mtrl.ENR = emtrltemp.ENR AND spec_mtrl.LEVKOD = emtrltemp.LEVKOD 
      NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE spec_mtrl THEN DO:
         CREATE spec_mtrl.
         ASSIGN
         spec_mtrl.ENR = emtrltemp.ENR
         spec_mtrl.BENAMNING = emtrltemp.BENAMNING
         spec_mtrl.ENHET = emtrltemp.ENHET        
         spec_mtrl.NPRIS = emtrltemp.NPRIS
         spec_mtrl.BPRIS = emtrltemp.BPRIS
         spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
      END.                        
      ASSIGN 
      emtrltemp.BERKVANT = FILL-IN-ANTAL
      spec_mtrl.BERKVANT = FILL-IN-ANTAL.       
   END. 
   ELSE IF vad = 2 THEN DO:
      FIND spec_mtrl WHERE spec_mtrl.ENR = esok_mtrl.ENR AND spec_mtrl.LEVKOD = esok_mtrl.LEV
      NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE spec_mtrl THEN DO:
         CREATE spec_mtrl.
         ASSIGN
         spec_mtrl.ENR = esok_mtrl.ENR
         spec_mtrl.BENAMNING = esok_mtrl.BENAMNING
         spec_mtrl.ENHET = esok_mtrl.ENHET
         spec_mtrl.NPRIS = esok_mtrl.NPRIS 
         spec_mtrl.BPRIS = esok_mtrl.BPRIS
         spec_mtrl.LEVKOD = esok_mtrl.LEV. 
      END.                          
      ASSIGN
      esok_mtrl.BERKVANT = FILL-IN-ANTAL
      spec_mtrl.BERKVANT = FILL-IN-ANTAL.       
   END. 
   ELSE IF vad = 1 THEN DO:
      ASSIGN      
      emtrl_mtrl.BESTANT = FILL-IN-ANTAL.      
   END.
   ELSE IF vad = 3 THEN DO:
      espec_mtrl.BERKVANT = FILL-IN-ANTAL.
   END.   
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
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   IF vad = 4 THEN DO:
      FIND FIRST emtrltemp NO-ERROR.
      ASSIGN
      FILL-IN-BEN = emtrltemp.BENAMNING
      FILL-IN-ENR = emtrltemp.ENR
      FILL-IN-ENHET = emtrltemp.ENHET
      FILL-IN-PRIS = emtrltemp.NPRIS
      FILL-IN-ANTAL = 1.      
   END. 
   ELSE IF vad = 2 THEN DO:
      FIND FIRST esok_mtrl NO-ERROR.
      ASSIGN
      FILL-IN-BEN = esok_mtrl.BENAMNING
      FILL-IN-ENR = esok_mtrl.ENR
      FILL-IN-ENHET = esok_mtrl.ENHET
      FILL-IN-PRIS = esok_mtrl.NPRIS
      FILL-IN-ANTAL = 1.
   END.            
   ELSE IF vad = 1 THEN DO:
      FIND FIRST emtrl_mtrl NO-ERROR.
      ASSIGN
      FILL-IN-BEN = emtrl_mtrl.BENAMNING
      FILL-IN-ENR = emtrl_mtrl.ENR
      FILL-IN-ENHET = emtrl_mtrl.ENHET
      FILL-IN-PRIS = emtrl_mtrl.NPRIS.      
      FILL-IN-ANTAL = emtrl_mtrl.BESTANT.
   END.           
   ELSE IF vad = 3 THEN DO:
      FIND FIRST espec_mtrl NO-ERROR.
      ASSIGN 
      FILL-IN-BEN = espec_mtrl.BENAMNING
      FILL-IN-ENR = espec_mtrl.ENR
      FILL-IN-ENHET = espec_mtrl.ENHET
      FILL-IN-PRIS = espec_mtrl.NPRIS
      FILL-IN-ANTAL = espec_mtrl.BERKVANT.
   END.   
   RUN enable_UI. 
 
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.

   {FRMSIZED.I}
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN btn_ok FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-ENHET FILL-IN-PRIS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

