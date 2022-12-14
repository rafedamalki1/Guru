&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

  Created: 02/13/96 - 10:32 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER liggande AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER rb-print AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE valut AS INTEGER NO-UNDO.
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
DEFINE VARIABLE utvar AS LOGICAL NO-UNDO.
{TIDUTTTSHARED.I}
DEFINE VARIABLE iPrintDlg  AS INTEGER     NO-UNDO.
 
DEFINE VARIABLE lpPrintDlg AS MEMPTR      NO-UNDO.
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     SPACE(15.51) SKIP(4.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
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
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 


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
   RUN enable_UI.       
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.  
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.
   IF vartpro NE "MED" THEN DO: 
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         RUN skrivligg_UI.
         IF utskriv = TRUE THEN DO:
            musz = FALSE. 
            RUN skrivval_UI.
         END.
         ELSE musz = TRUE.          
      END.
      ELSE DO: 
         valut = 1.
         IF valut = 1 THEN DO:
             RUN skrivligg_UI.                  
            
            IF utskriv = TRUE THEN DO:
               musz = FALSE. 
               RUN skrivval_UI.
            END.
            ELSE musz = TRUE. 
         END.         
      END.
   END.
   ELSE DO:
       RUN skrivligg_UI.
      IF utskriv = TRUE THEN musz = FALSE. 
      ELSE musz = TRUE. 
      RUN skrivval_UI.
   END.     
   {musarrow.i}   
   {DIA_M_SLUT.I}
   IF status-ok = status-ok THEN LEAVE MAIN-BLOCK.
   
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
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivligg_UI DIALOG-1 
PROCEDURE skrivligg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                  
      {SKRIVLS.I}
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivval_UI DIALOG-1 
PROCEDURE skrivval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   rb-print = SESSION:PRINTER-NAME.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKRIVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT "",INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
   END.
   ELSE DO:
      RUN SKRIVAPPT.P 
      (INPUT "",INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
   END.     
   Guru.GlobalaVariabler:globsidl = globsidl.
   Guru.GlobalaVariabler:globsids = globsids.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

