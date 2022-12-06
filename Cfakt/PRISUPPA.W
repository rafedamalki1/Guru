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

  Created: 05/05/97 - 11:29 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fprlista AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dprlista AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER overnorm AS LOGICAL NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW 
{FAKTTEMP.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UPP BTN_EJUPP BTN_FRAGEJ 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TXT FILL-IN-TXT-1 FILL-IN-TXT-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_EJUPP AUTO-GO 
     LABEL "Hämta ej ny prislista nu" 
     SIZE 20 BY 1.

DEFINE BUTTON BTN_FRAGEJ AUTO-GO 
     LABEL "Hämta ej och fråga ej mer" 
     SIZE 20 BY 1.

DEFINE BUTTON BTN_UPP AUTO-GO 
     LABEL "Hämta ny prislista" 
     SIZE 20 BY 1 TOOLTIP "Från beställaren/kundens grunduppgifter".

DEFINE VARIABLE FILL-IN-TXT AS CHARACTER FORMAT "X(256)":U INITIAL "Prislistan för" 
     VIEW-AS FILL-IN 
     SIZE 14.5 BY 1.83
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-TXT-1 AS CHARACTER FORMAT "X(256)":U INITIAL "normaltid" 
     VIEW-AS FILL-IN 
     SIZE 11.25 BY 1.83
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-TXT-3 AS CHARACTER FORMAT "X(256)":U INITIAL "är ändrad för denna kund." 
     VIEW-AS FILL-IN 
     SIZE 27.63 BY 1.83
     FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-TXT AT ROW 1.42 COL 1.5 NO-LABEL
     FILL-IN-TXT-1 AT ROW 1.42 COL 16.88 NO-LABEL
     FILL-IN-TXT-3 AT ROW 3.33 COL 1.5 NO-LABEL
     BTN_UPP AT ROW 5.54 COL 4.63
     BTN_EJUPP AT ROW 7.33 COL 4.63
     BTN_FRAGEJ AT ROW 9.13 COL 4.63
     SPACE(5.11) SKIP(0.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Pris / befattning för fakturering".


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

/* SETTINGS FOR FILL-IN FILL-IN-TXT IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-TXT-1 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-TXT-3 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Pris / befattning för fakturering */
DO:
   MESSAGE "Tryck ej här!" VIEW-AS ALERT-BOX.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Pris / befattning för fakturering */
DO:
   MESSAGE "Tryck ej här!" VIEW-AS ALERT-BOX.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EJUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EJUPP DIALOG-1
ON CHOOSE OF BTN_EJUPP IN FRAME DIALOG-1 /* Hämta ej ny prislista nu */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRAGEJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRAGEJ DIALOG-1
ON CHOOSE OF BTN_FRAGEJ IN FRAME DIALOG-1 /* Hämta ej och fråga ej mer */
DO:  
   RUN svarpris_UI IN fakthmth (INPUT 1,INPUT infakplannr, INPUT fprlista, INPUT dprlista).     
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Hämta ny prislista */
DO:
   RUN nylista_UI.
   RETURN.
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
   BTN_UPP:TOOLTIP = "Från " + Guru.Konstanter:gbestk + " grunduppgifter".
   IF overnorm = TRUE THEN FILL-IN-TXT-1 = "övertid".
   ELSE FILL-IN-TXT-1 = "normaltid".   
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
  DISPLAY FILL-IN-TXT FILL-IN-TXT-1 FILL-IN-TXT-3 
      WITH FRAME DIALOG-1.
  ENABLE BTN_UPP BTN_EJUPP BTN_FRAGEJ 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE norm_UI DIALOG-1 
PROCEDURE norm_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST faktpristemp WHERE faktpristemp.FAKTNR = infakplannr AND 
   faktpristemp.OVER = overnorm NO-ERROR.
   faktpristemp.STARTDATUM = TODAY.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nylista_UI DIALOG-1 
PROCEDURE nylista_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF overnorm  = TRUE THEN DO:
      RUN svarpris_UI IN fakthmth (INPUT 2,INPUT infakplannr, INPUT fprlista, INPUT dprlista).     
      RUN over_UI.
   END.
   ELSE DO: 
      RUN svarpris_UI IN fakthmth (INPUT 3,INPUT infakplannr, INPUT fprlista, INPUT dprlista).
      RUN norm_UI.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI DIALOG-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST faktpristemp WHERE faktpristemp.FAKTNR = infakplannr AND 
   faktpristemp.OVER = overnorm NO-ERROR.
   faktpristemp.STARTDATUM = TODAY.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

