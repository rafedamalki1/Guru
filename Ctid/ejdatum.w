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
DEFINE INPUT PARAMETER ejvisdatum AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER tdat AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER tdat-2 AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER tdag AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER tdag-2 AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.        
DEFINE VARIABLE datkoll AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_TNVE-4 BTN_TNVE-3 FILL-IN-TDATUM-2 ~
FILL-IN-TDATUM BTN_TFVE-4 BTN_TFVE-3 Btn_OK Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TDATUM-2 FILL-IN-TDAG-2 ~
FILL-IN-TDATUM FILL-IN-TDAG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_TFVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_TFVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_TNVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_TNVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-TDAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDAG-2 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDATUM AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "T.o.m" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TDATUM-2 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "F.o.m" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_TNVE-4 AT ROW 3.17 COL 23.5
     BTN_TNVE-3 AT ROW 3.17 COL 46
     FILL-IN-TDATUM-2 AT ROW 3.5 COL 9.5 COLON-ALIGNED
     FILL-IN-TDAG-2 AT ROW 3.5 COL 12.5 COLON-ALIGNED NO-LABEL
     FILL-IN-TDATUM AT ROW 3.5 COL 32 COLON-ALIGNED
     FILL-IN-TDAG AT ROW 3.5 COL 35 COLON-ALIGNED NO-LABEL
     BTN_TFVE-4 AT ROW 4 COL 23.5
     BTN_TFVE-3 AT ROW 4 COL 46
     Btn_OK AT ROW 5.17 COL 19.5
     Btn_AVB AT ROW 5.17 COL 34.5
     SPACE(0.99) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ange mellan vilka datum du vill visa tidsedeln"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_AVB.


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
       BTN_TFVE-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BTN_TFVE-4:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BTN_TNVE-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BTN_TNVE-4:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TDAG IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TDAG-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Ange mellan vilka datum du vill visa tidsedeln */
DO:
    musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ange mellan vilka datum du vill visa tidsedeln */
DO:
    musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON CHOOSE OF Btn_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
   IF FILL-IN-TDATUM < FILL-IN-TDATUM-2 THEN DO:
      MESSAGE "Startdatum kan inte vara större än slutdatum!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN dagkoll_UI.
   RUN dag_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.   
      RETURN NO-APPLY.
   END.   
   ASSIGN
   tdat = FILL-IN-TDATUM  
   tdat-2 = FILL-IN-TDATUM-2
   tdag =  FILL-IN-TDAG    
   tdag-2 = FILL-IN-TDAG-2.  
   APPLY "GO" TO BTN_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON GO OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TFVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TFVE-3 Dialog-Frame
ON CHOOSE OF BTN_TFVE-3 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-TDATUM = INPUT FILL-IN-TDATUM.   
   RUN btnfve3_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TFVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TFVE-4 Dialog-Frame
ON CHOOSE OF BTN_TFVE-4 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-TDATUM-2 = INPUT FILL-IN-TDATUM-2.   
   RUN btnfve4_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TNVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TNVE-3 Dialog-Frame
ON CHOOSE OF BTN_TNVE-3 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-TDATUM = INPUT FILL-IN-TDATUM.   
   FILL-IN-TDATUM = FILL-IN-TDATUM + 1.
   RUN dagkoll_UI.   
   RUN dag_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.      
   END.
   DISPLAY FILL-IN-TDATUM WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TNVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TNVE-4 Dialog-Frame
ON CHOOSE OF BTN_TNVE-4 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-TDATUM-2 = INPUT FILL-IN-TDATUM-2.   
   FILL-IN-TDATUM-2 = FILL-IN-TDATUM-2 + 1.
   RUN dagkoll_UI.
   
   IF FILL-IN-TDATUM-2 > FILL-IN-TDATUM THEN FILL-IN-TDATUM = FILL-IN-TDATUM-2.
   RUN dag_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.      
   END.
   DISPLAY FILL-IN-TDATUM-2 FILL-IN-TDATUM WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TDATUM Dialog-Frame
ON LEAVE OF FILL-IN-TDATUM IN FRAME Dialog-Frame /* T.o.m */
DO:
   ASSIGN
   FILL-IN-TDATUM = INPUT FILL-IN-TDATUM.         
   RUN dagkoll_UI.
   RUN dag_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TDATUM-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TDATUM-2 Dialog-Frame
ON LEAVE OF FILL-IN-TDATUM-2 IN FRAME Dialog-Frame /* F.o.m */
DO:
   ASSIGN
   FILL-IN-TDATUM-2 = INPUT FILL-IN-TDATUM-2.         
   RUN dagkoll_UI.   
   RUN dag_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
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
   ASSIGN
   FILL-IN-TDATUM = tdat 
   FILL-IN-TDATUM-2 = tdat-2 
   FILL-IN-TDAG = tdag 
   FILL-IN-TDAG-2 = tdag-2. 
   RUN enable_UI.
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfve3_UI Dialog-Frame 
PROCEDURE btnfve3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-TDATUM = FILL-IN-TDATUM - 1.   
   IF FILL-IN-TDATUM = 0 THEN FILL-IN-TDATUM = 1.      
   RUN dagkoll_UI.  
   IF FILL-IN-TDATUM < FILL-IN-TDATUM-2 THEN FILL-IN-TDATUM-2 = FILL-IN-TDATUM.
   RUN dag_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   DISPLAY FILL-IN-TDATUM-2 FILL-IN-TDATUM WITH FRAME {&FRAME-NAME}.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfve4_UI Dialog-Frame 
PROCEDURE btnfve4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-TDATUM-2 = FILL-IN-TDATUM-2 - 1.   
   IF FILL-IN-TDATUM = 0 THEN FILL-IN-TDATUM-2 = 1.
   RUN dagkoll_UI.
   RUN dag_UI.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   DISPLAY FILL-IN-TDATUM-2 WITH FRAME {&FRAME-NAME}.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dagkoll_UI Dialog-Frame 
PROCEDURE dagkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/              
   IF MONTH(ejvisdatum) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(ejvisdatum)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(ejvisdatum) + 1),01,YEAR(ejvisdatum)) - 1.
   END.
   IF FILL-IN-TDATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      FILL-IN-TDATUM = DAY(datkoll).      
      musz = TRUE.      
   END.            
   ELSE IF FILL-IN-TDATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      FILL-IN-TDATUM = 1.      
      musz = TRUE.
      
   END.
   ELSE IF FILL-IN-TDATUM-2 > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      FILL-IN-TDATUM-2 = DAY(datkoll).
      musz = TRUE.
      
   END.            
   ELSE IF FILL-IN-TDATUM-2 <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      FILL-IN-TDATUM-2 = 1.
      musz = TRUE.      
   END.           
   DISPLAY FILL-IN-TDATUM FILL-IN-TDATUM-2 WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dag_UI Dialog-Frame 
PROCEDURE dag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = DATE(MONTH(ejvisdatum),FILL-IN-TDATUM,YEAR(ejvisdatum)).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-TDAG = regdagnamn + "dag".
   DISPLAY FILL-IN-TDAG WITH FRAME {&FRAME-NAME}.
   regdatum = DATE(MONTH(ejvisdatum),FILL-IN-TDATUM-2,YEAR(ejvisdatum)).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-TDAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-TDAG-2 WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-TDATUM-2 FILL-IN-TDAG-2 FILL-IN-TDATUM FILL-IN-TDAG 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_TNVE-4 BTN_TNVE-3 FILL-IN-TDATUM-2 FILL-IN-TDATUM BTN_TFVE-4 
         BTN_TFVE-3 Btn_OK Btn_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

