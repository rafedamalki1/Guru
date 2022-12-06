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

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE varant AS INTEGER NO-UNDO.  
DEFINE VARIABLE varenr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varben AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE varpris AS DECIMAL NO-UNDO. 
DEFINE VARIABLE varenh AS CHARACTER NO-UNDO. 
DEFINE VARIABLE varbestd AS DATE NO-UNDO. 
DEFINE VARIABLE varbestall AS CHARACTER NO-UNDO.  
DEFINE VARIABLE vardep AS INTEGER NO-UNDO. 
DEFINE VARIABLE varbered AS LOGICAL NO-UNDO.
&Scoped-define NEW  
&Scoped-define SHARED SHARED 
{BESTMTRLDEP.I}
{AOBESTMTRLDEP.I}


DEFINE BUFFER bmtrlbuf FOR aobest_mtrl.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN btn_ok BTN_AVB ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-BEST 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-BEST 

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

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Levererat antal" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEST AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Beställt antal" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_UP AT ROW 5.75 COL 27.75
     FILL-IN-ANTAL AT ROW 6.17 COL 17.25 COLON-ALIGNED
     BTN_MIN AT ROW 6.88 COL 27.75
     btn_ok AT ROW 8.13 COL 17.25
     BTN_AVB AT ROW 8.13 COL 32.25
     FILL-IN-ENR AT ROW 1.67 COL 11.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.17 COL 11.25 COLON-ALIGNED
     FILL-IN-BEST AT ROW 4.67 COL 17.25 COLON-ALIGNED
     SPACE(21.99) SKIP(3.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Restnotering av beställt materiel":L
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO: 
   RETURN.
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
      MESSAGE "Levererat antal kan inte vara mindre än 0." VIEW-AS ALERT-BOX.
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO:   
   
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL. 
   IF FILL-IN-ANTAL < varant THEN DO: 
      {muswait.i} 
      IF musz = FALSE THEN DO:  
         FIND aobest_mtrl WHERE ROWID(aobest_mtrl) = mtrl_rowid NO-ERROR.      
         ASSIGN      
         aobest_mtrl.ANTAL = FILL-IN-ANTAL. 
         CREATE bmtrlbuf.
         BUFFER-COPY aobest_mtrl TO bmtrlbuf.
         ASSIGN
         bmtrlbuf.ANTAL = varant - FILL-IN-ANTAL
         bmtrlbuf.REST = TRUE.
         /*
         ASSIGN
         aobest_mtrl.ENR = varenr
         aobest_mtrl.BENAMNING = varben
         aobest_mtrl.LEVKOD = varlev 
         aobest_mtrl.BESTNR = varbestnr
         aobest_mtrl.PRIS = varpris
         aobest_mtrl.ENHET = varenh 
         aobest_mtrl.BESTDATUM = varbestd 
         aobest_mtrl.BESTALLARE = varbestall
         aobest_mtrl.DEPNR = vardep 
         aobest_mtrl.BERED = varbered
         aobest_mtrl.REST = TRUE
         aobest_mtrl.ANTAL = varant - FILL-IN-ANTAL.         
         */
      END.
      ELSE DO: 
         FIND aobest_mtrl WHERE ROWID(aobest_mtrl) = mtrl_rowid NO-ERROR.      
         ASSIGN      
         aobest_mtrl.ANTAL = FILL-IN-ANTAL. 
         FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = aobest_mtrl.ENR 
         AND bmtrlbuf.REST = TRUE NO-ERROR. 
         ASSIGN
         bmtrlbuf.ANTAL = varant - FILL-IN-ANTAL.      
      END.   
      APPLY "GO" TO FRAME {&FRAME-NAME}.  
   END.   
   ELSE DO:
      MESSAGE "Levererat antal kan inte vara lika med eller större än beställningsantalet." VIEW-AS ALERT-BOX.
   END.         
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-1
ON CHOOSE OF BTN_UP IN FRAME DIALOG-1 /* + */
DO:
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.           
   IF FILL-IN-ANTAL < varant - 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   END.   
   ELSE DO:
      MESSAGE "Levererat antal kan inte vara lika med eller större än beställningsantalet." VIEW-AS ALERT-BOX.
   END.            
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
   FIND aobest_mtrl WHERE ROWID(aobest_mtrl) = mtrl_rowid NO-ERROR.
   FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = aobest_mtrl.ENR 
   AND bmtrlbuf.REST = TRUE NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE bmtrlbuf THEN DO:   
      ASSIGN
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-BEN = aobest_mtrl.BENAMNING
      FILL-IN-ENR = aobest_mtrl.ENR  
      FILL-IN-BEST = aobest_mtrl.ANTAL    
      FILL-IN-ANTAL = 0
      varant = aobest_mtrl.ANTAL
      musz = FALSE. 
      /*  
      ASSIGN
      varenr = aobest_mtrl.ENR 
      varben = aobest_mtrl.BENAMNING
      varlev = aobest_mtrl.LEVKOD       
      varbestnr = aobest_mtrl.BESTNR
      varpris = aobest_mtrl.PRIS
      varenh = aobest_mtrl.ENHET 
      varbestd = aobest_mtrl.BESTDATUM 
      varbestall = aobest_mtrl.BESTALLARE 
      vardep = aobest_mtrl.DEPNR
      varbered = aobest_mtrl.BERED. 
      */
   END.
   ELSE DO:   
      ASSIGN
      FILL-IN-BEN = aobest_mtrl.BENAMNING
      FILL-IN-ENR = aobest_mtrl.ENR  
      FILL-IN-BEST = aobest_mtrl.ANTAL + bmtrlbuf.ANTAL   
      FILL-IN-ANTAL = aobest_mtrl.ANTAL
      varant = aobest_mtrl.ANTAL + bmtrlbuf.ANTAL
      musz = TRUE.
   END.     
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN-ANTAL IN FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-BEN FILL-IN-BEST 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-BEST 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

