&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

{ALLDEF.I}
{DHMT.I}        
{VHMT.I}
{RHMT.I}
{VECKOVISTEMP.I}
{WHANDLTEMP.I}
   
   
DEFINE INPUT  PARAMETER vad  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR earbtidtemp.
DEFINE INPUT PARAMETER TABLE FOR erullschtemp.
DEFINE INPUT PARAMETER TABLE FOR erullvectemp.  
DEFINE INPUT PARAMETER TABLE FOR arbtidtemp.
DEFINE INPUT PARAMETER TABLE FOR veckoarbtemp.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE bestdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ingar AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 28.13 COL 111
     SPACE(0.99) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Personaluppgifter"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Personaluppgifter */
DO:   
   IF VALID-HANDLE(bestdynh) THEN DO: 
      RUN avs_UI IN bestdynh.
      DELETE PROCEDURE bestdynh NO-ERROR.  
   END.
   
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
   
   
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}   
   {muswait.i}
   {ALLSTARTDYN.I}
   DEBUGGER:SET-BREAK().   
  RUN enable_UI.
  {FRMSIZEDF.I}
  EMPTY TEMP-TABLE veckovistemp NO-ERROR.
  IF vad = 1 THEN DO:
     FIND FIRST earbtidtemp NO-LOCK NO-ERROR.
     IF AVAILABLE earbtidtemp THEN DO: 
        ingar = FALSE.                
        FOR EACH veckoarbtemp  NO-LOCK:
           IF veckoarbtemp.ARBTIDMAN = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDTIS = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDONS = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDTOR = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDFRE = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDLOR = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           ELSE IF veckoarbtemp.ARBTIDSON = earbtidtemp.ARBTIDKOD THEN ingar = TRUE.
           IF ingar = TRUE THEN DO:
              ingar = FALSE.
              RUN visskap_UI. 
           END.   
              
        END.
     END.         
  END.
  ELSE  IF vad = 2 THEN DO:    
     FOR EACH veckoarbtemp  NO-LOCK:        
        RUN visskap_UI.        
     END.
     
  END.
  ELSE  IF vad = 3 THEN DO:     
     FIND FIRST erullschtemp NO-LOCK NO-ERROR.
     FOR EACH erullvectemp BY erullvectemp.ORDNING:
        FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA =  erullvectemp.VECKOSCHEMA NO-ERROR.
        IF AVAILABLE veckoarbtemp THEN DO:
           RUN visskap_UI.  
           ASSIGN
           veckovistemp.RULLID = erullvectemp.RULLID
           veckovistemp.RULLBEN = erullschtemp.BENAMNING.                        
        END.
     END.        
  END.   
  RUN VECKOVISDYN.P PERSISTENT SET bestdynh (INPUT THIS-PROCEDURE, INPUT framesizeh,INPUT TABLE whandltemp).
  RUN skapabrw_UI IN bestdynh (INPUT vad,INPUT 1, TEMP-TABLE veckovistemp:DEFAULT-BUFFER-HANDLE).
  
  

  {DIA_M_SLUT.I}
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 1.
   /*RUN whandle_UI (INPUT ordningnr, {&WINDOW-NAME}:HANDLE).*/
   RUN whandle_UI (INPUT ordningnr,FRAME Dialog-Frame:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME Dialog-Frame). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musa Dialog-Frame 
PROCEDURE musa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {musarrow.i}   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musw Dialog-Frame 
PROCEDURE musw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {muswait.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visskap_UI Dialog-Frame 
PROCEDURE visskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     CREATE veckovistemp.
     BUFFER-COPY veckoarbtemp  TO veckovistemp.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDMAN NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTMAN = arbtidtemp.START
        veckovistemp.SLUTMAN = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDMAN = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDTIS NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTTIS = arbtidtemp.START
        veckovistemp.SLUTTIS = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDTIS = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDONS NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTONS = arbtidtemp.START
        veckovistemp.SLUTONS = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDONS = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDTOR NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTTOR = arbtidtemp.START
        veckovistemp.SLUTTOR = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDTOR = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDFRE NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTFRE = arbtidtemp.START
        veckovistemp.SLUTFRE = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDFRE = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDLOR NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTLOR = arbtidtemp.START
        veckovistemp.SLUTLOR = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDLOR = arbtidtemp.TOTDAGTID.
     END.
     FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckovistemp.ARBTIDSON NO-LOCK NO-ERROR.
     IF AVAILABLE arbtidtemp THEN DO:
        ASSIGN
        veckovistemp.STARTSON = arbtidtemp.START
        veckovistemp.SLUTSON = arbtidtemp.SLUT
        veckovistemp.TOTDAGTIDSON = arbtidtemp.TOTDAGTID.
     END.      
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI Dialog-Frame 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

