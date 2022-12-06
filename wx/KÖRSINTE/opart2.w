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

  Created: 08/20/96 - 10:38 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */  
&Scoped-define NEW 
{ALLDEF.I}

{SOKMTRL.I}
DEFINE INPUT PARAMETER best AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ekund_mtrl.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR emtrl_mtrl.

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE ett AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE tva AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tre AS CHARACTER NO-UNDO. 
DEFINE VARIABLE fyra AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fem AS DECIMAL NO-UNDO. 
DEFINE VARIABLE sex AS DECIMAL NO-UNDO. 


/*{EGENBEN.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-PRIS FILL-IN-SUMMA BTN_OK BTN_AVB ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENH FILL-IN-ANTAL FILL-IN-LPRIS ~
FILL-IN-NPRIS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PRIS FILL-IN-SUMMA FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-ENH FILL-IN-ANTAL FILL-IN-LPRIS FILL-IN-NPRIS 

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

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
      VIEW-AS TEXT 
     SIZE 6 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENH AS CHARACTER FORMAT "X(2)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 3.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-LPRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Listpris/enhet" 
      VIEW-AS TEXT 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Nettopris/enhet" 
      VIEW-AS TEXT 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Offertpris/enhet" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMA AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Summa" 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-PRIS AT ROW 8.88 COL 19.13 COLON-ALIGNED
     FILL-IN-SUMMA AT ROW 10.38 COL 19.13 COLON-ALIGNED
     BTN_OK AT ROW 11.38 COL 18.5
     BTN_AVB AT ROW 11.38 COL 33.5
     FILL-IN-ENR AT ROW 1.38 COL 12.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.88 COL 12.5 COLON-ALIGNED
     FILL-IN-ENH AT ROW 4.38 COL 12.5 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 4.96 COL 33.38 COLON-ALIGNED
     FILL-IN-LPRIS AT ROW 5.88 COL 19.13 COLON-ALIGNED
     FILL-IN-NPRIS AT ROW 7.38 COL 19.13 COLON-ALIGNED
     SPACE(14.73) SKIP(4.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Offertpris/artikel".


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

ASSIGN 
       FILL-IN-NPRIS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO: 
   IF best = TRUE THEN DO:
      ASSIGN
      emtrl_mtrl.ENR = ett
      emtrl_mtrl.BENAMNING = tva
      emtrl_mtrl.ENHET = tre
      emtrl_mtrl.BERKVANT = fyra
      emtrl_mtrl.NPRIS = fem   
      emtrl_mtrl.SUMMA = sex.
   END.
   ELSE DO:   
      ASSIGN
      ekund_mtrl.ENR = ett
      ekund_mtrl.BENAMNING = tva
      ekund_mtrl.ENHET = tre
      ekund_mtrl.BERKVANT = fyra
      ekund_mtrl.KPRIS = fem   
      ekund_mtrl.SUMMA = sex. 
   END.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* OK */
DO: 
   IF best = TRUE THEN DO:   
      ASSIGN
      emtrl_mtrl.ENR = INPUT FILL-IN-ENR
      emtrl_mtrl.BENAMNING = INPUT FILL-IN-BEN
      emtrl_mtrl.ENHET = INPUT FILL-IN-ENH
      emtrl_mtrl.NPRIS = INPUT FILL-IN-PRIS
      emtrl_mtrl.BERKVANT = INPUT FILL-IN-ANTAL. 
   END.         
   ELSE DO:
      ASSIGN
      ekund_mtrl.ENR = INPUT FILL-IN-ENR
      ekund_mtrl.BENAMNING = INPUT FILL-IN-BEN
      ekund_mtrl.ENHET = INPUT FILL-IN-ENH
      ekund_mtrl.KPRIS = INPUT FILL-IN-PRIS
      ekund_mtrl.BERKVANT = INPUT FILL-IN-ANTAL. 
   END.        
   APPLY "GO" TO FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Offertpris/enhet */
DO: 
   IF best = TRUE THEN DO:
      ASSIGN 
      emtrl_mtrl.NPRIS = INPUT FILL-IN-PRIS
      emtrl_mtrl.SUMMA =  emtrl_mtrl.NPRIS * emtrl_mtrl.BERKVANT
      FILL-IN-PRIS = emtrl_mtrl.NPRIS
      FILL-IN-SUMMA = emtrl_mtrl.SUMMA. 
   END.
   ELSE DO:         
      ASSIGN 
      ekund_mtrl.KPRIS = INPUT FILL-IN-PRIS
      ekund_mtrl.SUMMA =  ekund_mtrl.KPRIS * ekund_mtrl.BERKVANT
      FILL-IN-PRIS = ekund_mtrl.KPRIS
      FILL-IN-SUMMA = ekund_mtrl.SUMMA.
   END.      
   DISPLAY FILL-IN-SUMMA WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SUMMA DIALOG-1
ON LEAVE OF FILL-IN-SUMMA IN FRAME DIALOG-1 /* Summa */
DO:
   IF best = TRUE THEN DO:
      ASSIGN 
      emtrl_mtrl.SUMMA = INPUT FILL-IN-SUMMA
      emtrl_mtrl.NPRIS =  emtrl_mtrl.SUMMA / emtrl_mtrl.BERKVANT
      FILL-IN-PRIS = emtrl_mtrl.NPRIS
      FILL-IN-SUMMA = emtrl_mtrl.SUMMA. 
   END.      
   ELSE DO:   
      ASSIGN 
      ekund_mtrl.SUMMA = INPUT FILL-IN-SUMMA
      ekund_mtrl.KPRIS =  ekund_mtrl.SUMMA / ekund_mtrl.BERKVANT
      FILL-IN-PRIS = ekund_mtrl.KPRIS
      FILL-IN-SUMMA = ekund_mtrl.SUMMA. 
   END.     
   DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
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
   IF best = TRUE THEN DO:      
      FIND FIRST emtrl_mtrl NO-ERROR. 
      ASSIGN
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-ENR = emtrl_mtrl.ENR
      FILL-IN-BEN = emtrl_mtrl.BENAMNING
      FILL-IN-ENH = emtrl_mtrl.ENHET
      FILL-IN-ANTAL = emtrl_mtrl.BERKVANT 
      FILL-IN-LPRIS = emtrl_mtrl.BPRIS
      FILL-IN-PRIS = emtrl_mtrl.NPRIS
      FILL-IN-SUMMA = emtrl_mtrl.SUMMA 
      ett = emtrl_mtrl.ENR
      tva = emtrl_mtrl.BENAMNING
      tre = emtrl_mtrl.ENHET
      fyra = emtrl_mtrl.BERKVANT
      fem = emtrl_mtrl.NPRIS
      sex = emtrl_mtrl.SUMMA.
   END.    
   ELSE DO:      
      FIND FIRST ekund_mtrl NO-ERROR. 
      ASSIGN
      FILL-IN-ENR = ekund_mtrl.ENR
      FILL-IN-BEN = ekund_mtrl.BENAMNING
      FILL-IN-ENH = ekund_mtrl.ENHET
      FILL-IN-ANTAL = ekund_mtrl.BERKVANT
      FILL-IN-LPRIS = ekund_mtrl.BPRIS
      FILL-IN-NPRIS = ekund_mtrl.NPRIS
      FILL-IN-PRIS = ekund_mtrl.KPRIS
      FILL-IN-SUMMA = ekund_mtrl.SUMMA
      ett = ekund_mtrl.ENR
      tva = ekund_mtrl.BENAMNING
      tre = ekund_mtrl.ENHET
      fyra = ekund_mtrl.BERKVANT
      fem = ekund_mtrl.KPRIS
      sex = ekund_mtrl.SUMMA.
   END.          
  RUN enable_UI.       
   {FRMSIZED.I}
  IF best = TRUE THEN DO: 
     FILL-IN-NPRIS:HIDDEN = TRUE. 
  END.
  ELSE DO:
     FILL-IN-NPRIS:HIDDEN = FALSE.
  END. 
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
  DISPLAY FILL-IN-PRIS FILL-IN-SUMMA FILL-IN-ENR FILL-IN-BEN FILL-IN-ENH 
          FILL-IN-ANTAL FILL-IN-LPRIS FILL-IN-NPRIS 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-PRIS FILL-IN-SUMMA BTN_OK BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-ENH FILL-IN-ANTAL FILL-IN-LPRIS FILL-IN-NPRIS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

