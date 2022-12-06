&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED 
{BERBILD.I}
DEFINE SHARED VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE varbild AS CHARACTER NO-UNDO.




DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE filedir AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjfilvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE templg AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.    
DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE compdir NO-UNDO
      FIELD BIB AS CHARACTER. 

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-DYNBILD BTN_ANP BTN_PREV BTN_NEXT ~
BTN_SKRIV BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANP 
     LABEL "Anpassa bilden" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NEXT 
     LABEL "Nästa" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PREV 
     LABEL "Föregående" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE IMAGE IMAGE-DYNBILD
     SIZE 124 BY 26.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_ANP AT ROW 28.25 COL 51
     BTN_PREV AT ROW 28.25 COL 66.5
     BTN_NEXT AT ROW 28.25 COL 81.5
     BTN_SKRIV AT ROW 28.25 COL 96.5
     BTN_AVSL AT ROW 28.25 COL 111
     IMAGE-DYNBILD AT ROW 1.5 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Visa konstruktionsbild"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       IMAGE-DYNBILD:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Visa konstruktionsbild */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Visa konstruktionsbild */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANP C-Win
ON CHOOSE OF BTN_ANP IN FRAME DEFAULT-FRAME /* Anpassa bilden */
DO:
   IF BTN_ANP:LABEL = "Anpassa bilden" THEN DO:
      BTN_ANP:LABEL = "Orginal storlek".
      IMAGE-DYNBILD:STRETCH-TO-FIT = TRUE.
   END.
   ELSE DO:
      BTN_ANP:LABEL = "Anpassa bilden".
      IMAGE-DYNBILD:STRETCH-TO-FIT = FALSE.
   END.
   RUN laddabild_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL C-Win
ON CHOOSE OF BTN_AVSL IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NEXT C-Win
ON CHOOSE OF BTN_NEXT IN FRAME DEFAULT-FRAME /* Nästa */
DO:
   BTN_ANP:LABEL = "Anpassa bilden".
   IMAGE-DYNBILD:STRETCH-TO-FIT = FALSE.
   RUN laddabild_UI (INPUT 2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PREV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PREV C-Win
ON CHOOSE OF BTN_PREV IN FRAME DEFAULT-FRAME /* Föregående */
DO:
   BTN_ANP:LABEL = "Anpassa bilden".
   IMAGE-DYNBILD:STRETCH-TO-FIT = FALSE.
   RUN laddabild_UI (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV C-Win
ON CHOOSE OF BTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:    
   {muswait.i}    
   RUN OPENDOC.P (hjfilvar,"","",no).              
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV C-Win
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}        
   filedir = Guru.Konstanter:gurubilder.
   {&WINDOW-NAME}:TITLE = "Bild(er) kopplade till konstruktion:" + varbild.
   RUN enable_UI.    
   {FRMSIZE.I}  
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVSL:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_NEXT:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_PREV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_ANP:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   OPEN QUERY bq FOR EACH berbildtemp WHERE berbildtemp.KTYPKOD = varbild NO-LOCK.
   RUN laddabild_UI (INPUT 1).      
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE IMAGE-DYNBILD BTN_ANP BTN_PREV BTN_NEXT BTN_SKRIV BTN_AVSL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddabild_UI C-Win 
PROCEDURE laddabild_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   IF val = 1 THEN DO:
      GET FIRST bq NO-LOCK.
   END.
   ELSE IF val = 2 THEN DO:
      GET NEXT bq NO-LOCK.
   END.
   ELSE IF val = 3 THEN DO:
      GET PREV bq NO-LOCK.
   END.
   ELSE IF val = 4 THEN DO:
      GET CURRENT bq NO-LOCK.
   END.
  bloblog = TRUE.
   IF bloblog = TRUE THEN DO:
      {FINNSDYNBLOB.I}
      DEFINE VARIABLE resid AS INTEGER NO-UNDO.
      RUN blobfil_UI IN blobproch (INPUT berbildtemp.FILNAMN, OUTPUT resid).
      IF resid = ? THEN DO: 
         DEFINE VARIABLE sokbild AS CHARACTER NO-UNDO.
         sokbild = "bilder\" + berbildtemp.FILNAMN.
         hjfilvar = SEARCH(sokbild).
         IF hjfilvar = ? THEN DO:
            hjfilvar = SEARCH(sokbild + ".gif").              
         END.   
         IF hjfilvar = ? THEN DO:
            hjfilvar = SEARCH(sokbild + ".jpg").
         END.
         IF hjfilvar = ? THEN DO:
            hjfilvar = SEARCH(sokbild + ".pdf").
         END.
      END.
      ELSE DO:
         FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
         RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT hjfilvar).
      END.
      RUN deleteproc_UI IN blobproch.
      IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
      IF hjfilvar NE ? AND INDEX(hjfilvar, ".pdf") NE 0 THEN DO: 
         templg = TRUE.         
         RUN OPENDOC.P (hjfilvar,"","",NO).
      END.
      ELSE DO:
         IF hjfilvar NE ? THEN templg = IMAGE-DYNBILD:LOAD-IMAGE(hjfilvar) IN FRAME {&FRAME-NAME} NO-ERROR.
      END.
      IF templg = FALSE THEN DO: 
         MESSAGE "Filen " + berbildtemp.FILNAMN + " finns inte!".
      END.
   END.
   ELSE DO: 
      hjfilvar = filedir +  berbildtemp.FILNAMN.
      IF INDEX(hjfilvar, ".pdf") NE 0 THEN DO: 
         templg = TRUE.         
         RUN OPENDOC.P (hjfilvar,"","",NO).
      END.
      ELSE DO:      
         templg = IMAGE-DYNBILD:LOAD-IMAGE(hjfilvar) NO-ERROR.
      END.
      IF templg = FALSE THEN DO: 
         MESSAGE "Filen " + berbildtemp.FILNAMN + " finns inte!".
      END.
   END.   
   GET NEXT bq NO-LOCK.
   IF NOT AVAILABLE berbildtemp THEN DISABLE BTN_NEXT WITH FRAME {&FRAME-NAME}.
   ELSE ENABLE BTN_NEXT WITH FRAME {&FRAME-NAME}.
   GET PREV bq NO-LOCK.
   GET PREV bq NO-LOCK.
   IF NOT AVAILABLE berbildtemp THEN DISABLE BTN_PREV WITH FRAME {&FRAME-NAME}.
   ELSE ENABLE BTN_PREV WITH FRAME {&FRAME-NAME}.
   GET NEXT bq NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

