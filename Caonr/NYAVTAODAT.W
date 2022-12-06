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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER inrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER aonrvar AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ordvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER avtidvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER utrec AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{REGVAR.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTAONRTEMP.I}
{OMRTEMPW.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyavtaodath AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UPDAT1 FILL-IN-STARTDAT BTN_MINDAT1 ~
FILL-IN-HAND BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-STARTDAT FILL-IN-HAND 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MINDAT1 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_UPDAT1 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-HAND AS CHARACTER FORMAT "X(256)":U 
     LABEL "Händelse" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .96 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_UPDAT1 AT ROW 2.08 COL 22.63
     FILL-IN-STARTDAT AT ROW 2.38 COL 9.63 COLON-ALIGNED
     BTN_MINDAT1 AT ROW 2.96 COL 22.63
     FILL-IN-HAND AT ROW 5.08 COL 9.63 COLON-ALIGNED
     BTN_OK AT ROW 6.58 COL 26.63
     BTN_AVB AT ROW 6.58 COL 41.63
     SPACE(0.61) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR,ENDKEY OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   IF VALID-HANDLE(nyavtaodath) THEN DELETE PROCEDURE nyavtaodath.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MINDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MINDAT1 Dialog-Frame
ON CHOOSE OF BTN_MINDAT1 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-HAND = INPUT FILL-IN-HAND.
   RUN btnok IN nyavtaodath (INPUT aonrvar,INPUT delnrvar,INPUT ordvar,
                             INPUT avtidvar,INPUT FILL-IN-HAND,
                             INPUT FILL-IN-STARTDAT,INPUT-OUTPUT utrec).
   FIND FIRST avtaodattemp WHERE avtaodattemp.AVTAODATREC = utrec
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE avtaodattemp THEN DO:
      CREATE avtaodattemp.
      ASSIGN
      avtaodattemp.AONR = aonrvar
      avtaodattemp.DELNR = delnrvar
      avtaodattemp.ORDNING = ordvar
      avtaodattemp.AVTALID = avtidvar
      avtaodattemp.AVTAODATREC = utrec.
   END.
   ASSIGN
   avtaodattemp.ANMARK = FILL-IN-HAND
   avtaodattemp.DATUM = FILL-IN-STARTDAT.
   IF VALID-HANDLE(nyavtaodath) THEN DELETE PROCEDURE nyavtaodath.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPDAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPDAT1 Dialog-Frame
ON CHOOSE OF BTN_UPDAT1 IN FRAME Dialog-Frame /* + */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Datum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w. 
   FILL-IN-STARTDAT = regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.  
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
   {ALLSTARTDYN.I} 
   FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = aonrvar AND avtalaonrtemp.DELNR = delnrvar AND
   avtalaonrtemp.AVTID = avtidvar AND avtalaonrtemp.ORDN = ordvar NO-LOCK NO-ERROR.
   IF inrec = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny händelse kopplad till tillstånd/avtal: " +
      avtalaonrtemp.BEN
      utrec = ?.
      FILL-IN-STARTDAT = TODAY.
   END.
   ELSE DO:      
      FIND avtaodattemp WHERE avtaodattemp.AVTAODATREC = inrec EXCLUSIVE-LOCK.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring händelse kopplad till tillstånd/avtal: " +
      avtalaonrtemp.BEN
      utrec = avtaodattemp.AVTAODATREC
      FILL-IN-HAND = avtaodattemp.ANMARK      
      FILL-IN-STARTDAT = avtaodattemp.DATUM.
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYAVTAODAT2.P PERSISTENT SET nyavtaodath ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYAVTAODAT2.P PERSISTENT SET nyavtaodath.
   END.
  
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
  DISPLAY FILL-IN-STARTDAT FILL-IN-HAND 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_UPDAT1 FILL-IN-STARTDAT BTN_MINDAT1 FILL-IN-HAND BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

