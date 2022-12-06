&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 08/12/96 -  3:11 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */ 
{SOKMTRL.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR emtrl_mtrl.    /*vad = 1*/
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE valkalknr AS INTEGER  NO-UNDO.
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE varpris2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE kodlev AS CHARACTER NO-UNDO.

    
DEFINE BUFFER bmtrlbuf FOR bmtrl_mtrl.
/*{EGENBEN.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN BTN_OK BTN_AVB ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 

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

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal i lager" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 26.5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTANT AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 5.63 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV AS CHARACTER FORMAT "X(4)":U INITIAL "0" 
     LABEL "Lev" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     BTN_UP AT ROW 7.25 COL 26
     FILL-IN-ANTAL AT ROW 7.75 COL 15.5 COLON-ALIGNED
     BTN_MIN AT ROW 8.42 COL 26
     BTN_OK AT ROW 9.54 COL 10.5
     BTN_AVB AT ROW 9.54 COL 25.5
     FILL-IN-ENR AT ROW 1.75 COL 11.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.25 COL 11.5 COLON-ALIGNED
     FILL-IN-BESTANT AT ROW 4.75 COL 11.5 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.75 COL 25.5 COLON-ALIGNED
     FILL-IN-LEV AT ROW 6.25 COL 11.5 COLON-ALIGNED
     SPACE(20.49) SKIP(3.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Finns i lager".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:  
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-2
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-2 /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0." VIEW-AS ALERT-BOX.
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:        
   ASSIGN 
   kodlev = emtrl_mtrl.LEV
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = emtrl_mtrl.ENR AND bmtrlbuf.LEVKOD = "0" AND 
   bmtrlbuf.BERLEV = kodlev NO-ERROR.
   IF NOT AVAILABLE bmtrlbuf THEN DO: 
      IF emtrl_mtrl.BERKVANT - FILL-IN-ANTAL < 0 THEN DO:
         MESSAGE "Antal i lager kan inte vara större än beställningsantal." VIEW-AS ALERT-BOX.      
      END.   
      ELSE DO:
         ASSIGN
         emtrl_mtrl.BERKVANT = FILL-IN-BESTANT - FILL-IN-ANTAL.                               
         CREATE bmtrl_mtrl. 
         ASSIGN 
         bmtrl_mtrl.KALKNR = valkalknr
         bmtrl_mtrl.ENR = FILL-IN-ENR
         bmtrl_mtrl.BENAMNING = FILL-IN-BEN
         bmtrl_mtrl.ENHET = FILL-IN-ENHET   
         bmtrl_mtrl.BESTANT = FILL-IN-ANTAL      
         bmtrl_mtrl.BERKVANT = FILL-IN-ANTAL
         bmtrl_mtrl.NPRIS = varpris 
         bmtrl_mtrl.BPRIS = varpris2  
         bmtrl_mtrl.LEVKOD = "0"
         bmtrl_mtrl.BERLEV = kodlev.  
         APPLY "GO" TO FRAME {&FRAME-NAME}.
      END. 
   END.
   ELSE DO:
      ASSIGN      
      bmtrlbuf.BESTANT = FILL-IN-ANTAL
      bmtrlbuf.BERKVANT = FILL-IN-ANTAL
      emtrl_mtrl.BERKVANT = FILL-IN-BESTANT - FILL-IN-ANTAL.
      APPLY "GO" TO FRAME {&FRAME-NAME}.
   END. 
   FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.BESTANT = 0 AND bmtrl_mtrl.BERLEV NE " ":
      DELETE bmtrl_mtrl.
   END.    
             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-2
ON CHOOSE OF BTN_UP IN FRAME DIALOG-2 /* + */
DO:
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   FIND FIRST emtrl_mtrl NO-ERROR.
   ASSIGN
   kodlev = emtrl_mtrl.LEVKOD
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk. 
   FIND bmtrlbuf WHERE bmtrlbuf.ENR = emtrl_mtrl.ENR AND bmtrlbuf.LEVKOD = "0" AND 
   bmtrlbuf.BERLEV = kodlev NO-ERROR.
   ASSIGN     
   varpris = emtrl_mtrl.NPRIS 
   varpris2 = emtrl_mtrl.BPRIS
   FILL-IN-BEN = emtrl_mtrl.BENAMNING
   FILL-IN-ENR = emtrl_mtrl.ENR
   FILL-IN-LEV = emtrl_mtrl.LEVKOD
   FILL-IN-ENHET = emtrl_mtrl.ENHET.
   IF NOT AVAILABLE bmtrlbuf THEN DO:      
      ASSIGN
      FILL-IN-ANTAL = 1
      FILL-IN-BESTANT = emtrl_mtrl.BERKVANT.
   END.
   ELSE DO:                   
      ASSIGN        
      FILL-IN-ANTAL = bmtrlbuf.BESTANT.      
      FILL-IN-BESTANT = emtrl_mtrl.BESTANT.
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN FILL-IN-BESTANT FILL-IN-ENHET 
          FILL-IN-LEV 
      WITH FRAME DIALOG-2.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN BTN_OK BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-BESTANT FILL-IN-ENHET FILL-IN-LEV 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

