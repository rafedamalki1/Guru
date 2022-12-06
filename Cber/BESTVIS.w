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
{BESTVISTT.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{LEVERANT.I}
{WHANDLTEMP.I}
DEFINE INPUT  PARAMETER mdep AS INTEGER NO-UNDO.   
DEFINE INPUT PARAMETER TABLE FOR tempbermtrlz.
DEFINE INPUT PARAMETER TABLE FOR tempbeststat.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE bestdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_MTRLBEST Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS RAD_MTRLBEST 

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

DEFINE VARIABLE RAD_MTRLBEST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Materiel", 1,
"Materiel summerat per projekt", 3,
"Materiel summerat per Enr", 4,
"Beställningsinfo", 2
     SIZE 102 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_MTRLBEST AT ROW 1.5 COL 4.5 NO-LABEL WIDGET-ID 2
     Btn_OK AT ROW 28.25 COL 111
     SPACE(0.99) SKIP(0.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Gjorda Beställningar"
         DEFAULT-BUTTON Btn_OK.

DEFINE FRAME FRAME-MTRLENR
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 124 BY 25 WIDGET-ID 300.

DEFINE FRAME FRAME-MTRLAONR
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 124 BY 25 WIDGET-ID 200.

DEFINE FRAME FRAME-BEST
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 124 BY 25 WIDGET-ID 100.

DEFINE FRAME FRAME-MTRL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 124 BY 25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-BEST:FRAME = FRAME Dialog-Frame:HANDLE
       FRAME FRAME-MTRL:FRAME = FRAME Dialog-Frame:HANDLE
       FRAME FRAME-MTRLAONR:FRAME = FRAME Dialog-Frame:HANDLE
       FRAME FRAME-MTRLENR:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-MTRL:MOVE-AFTER-TAB-ITEM (RAD_MTRLBEST:HANDLE IN FRAME Dialog-Frame)
       XXTABVALXX = FRAME FRAME-MTRLENR:MOVE-BEFORE-TAB-ITEM (Btn_OK:HANDLE IN FRAME Dialog-Frame)
       XXTABVALXX = FRAME FRAME-MTRLAONR:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-MTRLENR:HANDLE)
       XXTABVALXX = FRAME FRAME-BEST:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-MTRLAONR:HANDLE)
       XXTABVALXX = FRAME FRAME-MTRL:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-BEST:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-BEST
                                                                        */
ASSIGN 
       FRAME FRAME-BEST:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-MTRL
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-MTRL:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-MTRLAONR
                                                                        */
ASSIGN 
       FRAME FRAME-MTRLAONR:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-MTRLENR
                                                                        */
ASSIGN 
       FRAME FRAME-MTRLENR:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Gjorda Beställningar */
DO:   
   IF VALID-HANDLE(bestdynh) THEN DO: 
      RUN avs_UI IN bestdynh.
      DELETE PROCEDURE bestdynh NO-ERROR.  
   END.   
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_MTRLBEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_MTRLBEST Dialog-Frame
ON VALUE-CHANGED OF RAD_MTRLBEST IN FRAME Dialog-Frame
DO:
  RAD_MTRLBEST = INPUT RAD_MTRLBEST.
  FRAME FRAME-MTRL:HIDDEN = TRUE.
  FRAME FRAME-MTRLAONR:HIDDEN = TRUE.
  FRAME FRAME-MTRLENR:HIDDEN = TRUE.
  FRAME FRAME-BEST:HIDDEN = TRUE.
  IF RAD_MTRLBEST = 1 THEN FRAME FRAME-MTRL:HIDDEN = FALSE.
  ELSE IF RAD_MTRLBEST = 2 THEN  FRAME FRAME-BEST:HIDDEN = FALSE.     
  ELSE IF RAD_MTRLBEST = 3 THEN FRAME FRAME-MTRLAONR:HIDDEN = FALSE.
  ELSE IF RAD_MTRLBEST = 4 THEN FRAME FRAME-MTRLENR:HIDDEN = FALSE.             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ  ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
   
   
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {DIA_M_START.I}   
  {muswait.i}   
  EMPTY TEMP-TABLE tempbermtrEnr NO-ERROR.
  EMPTY TEMP-TABLE tempbermtrAo NO-ERROR. 
  FOR EACH tempbermtrlz BREAK BY tempbermtrlz.AONR BY tempbermtrlz.DELNR BY tempbermtrlz.LEVKOD BY tempbermtrlz.ENR : 
     ACCUMULATE tempbermtrlz.ANTAL (TOTAL BY tempbermtrlz.ENR).       
     IF LAST-OF(tempbermtrlz.ENR) THEN DO:
        CREATE tempbermtrAo.
        BUFFER-COPY tempbermtrlz TO tempbermtrAo.
        tempbermtrAo.ANTAL = (ACCUM TOTAL BY tempbermtrlz.ENR tempbermtrlz.ANTAL). 
     END.     
  END.    
  FOR EACH tempbermtrlz BREAK BY tempbermtrlz.LEVKOD BY tempbermtrlz.ENR : 
     ACCUMULATE tempbermtrlz.ANTAL (TOTAL BY tempbermtrlz.ENR).       
     IF LAST-OF(tempbermtrlz.ENR) THEN DO:
        CREATE tempbermtrEnr.
        BUFFER-COPY tempbermtrlz TO tempbermtrEnr.
        tempbermtrEnr.ANTAL = (ACCUM TOTAL BY tempbermtrlz.ENR tempbermtrlz.ANTAL). 
     END.     
  END.
  {ALLSTARTDYN.I} 
  status-ok = RAD_MTRLBEST:DELETE("Materiel").
  status-ok = RAD_MTRLBEST:DELETE("Materiel summerat per projekt").
  status-ok = RAD_MTRLBEST:DELETE("Materiel summerat per Enr").
  status-ok = RAD_MTRLBEST:DELETE("Beställningsinfo").  
  status-ok = RAD_MTRLBEST:ADD-LAST("Materiel", 1).  
  status-ok = RAD_MTRLBEST:ADD-LAST("Materiel summerat per " + Guru.Konstanter:genk, 4).
  status-ok = RAD_MTRLBEST:ADD-LAST("Beställningsinfo", 2).
  RAD_MTRLBEST = 1.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO RAD_MTRLBEST.
  {FRMSIZEDF.I}  
  FRAME FRAME-MTRL:HIDDEN = FALSE.
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
   RUN whandle_UI (INPUT ordningnr,FRAME Dialog-Frame:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-MTRL:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEST:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME Dialog-Frame).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-MTRLAONR:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-MTRLENR:HANDLE). 
   RUN BESTVISDYN.P PERSISTENT SET bestdynh (INPUT THIS-PROCEDURE, INPUT FRAME Dialog-FRAME:HANDLE,INPUT TABLE whandltemp).   
   IF mdep = 1 THEN RUN skapabrw_UI IN bestdynh (INPUT 1,INPUT 1, TEMP-TABLE tempbermtrlz:DEFAULT-BUFFER-HANDLE).
   ELSE IF mdep = 2 THEN RUN skapabrw_UI IN bestdynh (INPUT 2, INPUT 1, TEMP-TABLE tempbermtrlz:DEFAULT-BUFFER-HANDLE).
   ELSE IF mdep = 3 THEN RUN skapabrw_UI IN bestdynh (INPUT 3, INPUT 1, TEMP-TABLE tempbermtrlz:DEFAULT-BUFFER-HANDLE).      
   RUN skapabrw_UI IN bestdynh (INPUT 1, INPUT 2, TEMP-TABLE tempbeststat:DEFAULT-BUFFER-HANDLE).
   RUN skapabrw_UI IN bestdynh (INPUT 1,INPUT 3, TEMP-TABLE tempbermtrAo:DEFAULT-BUFFER-HANDLE).
   RUN skapabrw_UI IN bestdynh (INPUT 1,INPUT 4, TEMP-TABLE tempbermtrEnr:DEFAULT-BUFFER-HANDLE).
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
  HIDE FRAME FRAME-BEST.
  HIDE FRAME FRAME-MTRL.
  HIDE FRAME FRAME-MTRLAONR.
  HIDE FRAME FRAME-MTRLENR.
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
  DISPLAY RAD_MTRLBEST 
      WITH FRAME Dialog-Frame.
  ENABLE RAD_MTRLBEST Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  VIEW FRAME FRAME-BEST.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BEST}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRL}
  VIEW FRAME FRAME-MTRLAONR.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRLAONR}
  VIEW FRAME FRAME-MTRLENR.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRLENR}
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

