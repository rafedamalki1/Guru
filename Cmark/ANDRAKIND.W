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

  Created: 04/15/97 -  9:24 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER manadvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED   
{MARKVARD.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE mdivrec AS RECID NO-UNDO.
DEFINE VARIABLE andkindapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_AR FILL-IN_MANAD FILL-IN_VARDE ~
BTN_OK BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AR FILL-IN_MANAD FILL-IN_VARDE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_AR AS INTEGER FORMAT "9999" INITIAL 0 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_MANAD AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Månad" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.

DEFINE VARIABLE FILL-IN_VARDE AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Värde" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_AR AT ROW 1.38 COL 6.63 COLON-ALIGNED
     FILL-IN_MANAD AT ROW 2.38 COL 6.63 COLON-ALIGNED
     FILL-IN_VARDE AT ROW 3.38 COL 6.63 COLON-ALIGNED
     BTN_OK AT ROW 4.75 COL 1.5
     BTN_AVSL AT ROW 4.75 COL 16.5
     SPACE(0.62) SKIP(0.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra INDEX Iv".


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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra INDEX Iv */
DO:
   IF VALID-HANDLE(andkindapph) THEN DELETE PROCEDURE andkindapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra INDEX Iv */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:                                
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   ASSIGN   
   musz = FALSE        
   FILL-IN_AR = INPUT FILL-IN_AR  
   FILL-IN_MANAD = INPUT FILL-IN_MANAD     
   FILL-IN_VARDE = INPUT FILL-IN_VARDE.
   IF FILL-IN_AR = 0 THEN DO:
      MESSAGE "År kan inte vara 0." VIEW-AS ALERT-BOX.   
      /*status-mus2 = SESSION:SET-WAIT-STATE("").*/
      APPLY "ENTRY" TO FILL-IN_AR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.  
   IF FILL-IN_MANAD = 0 THEN DO:
      MESSAGE "Månad kan inte vara 0." VIEW-AS ALERT-BOX.   
/*      status-mus2 = SESSION:SET-WAIT-STATE("").*/
      APPLY "ENTRY" TO FILL-IN_MANAD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.  
   IF FILL-IN_MANAD > 12 THEN DO:
      MESSAGE "Månad kan inte vara större än 12." VIEW-AS ALERT-BOX.   
  /*    status-mus2 = SESSION:SET-WAIT-STATE("").*/
      APPLY "ENTRY" TO FILL-IN_MANAD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END. 
   RUN btnok IN andkindapph (INPUT vart,INPUT FILL-IN_AR,INPUT FILL-IN_MANAD,
                             INPUT FILL-IN_VARDE,OUTPUT TABLE felmeddtemp,
                             INPUT-OUTPUT TABLE kindextemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.
      musz = TRUE.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      ASSIGN
      artalvar = FILL-IN_AR
      manadvar = FILL-IN_MANAD
      musz = FALSE.
   END. 
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
  IF VALID-HANDLE(andkindapph) THEN DELETE PROCEDURE andkindapph.
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
   {muswait.i}
   {ALLSTARTDYN.I} 
   RUN grundtid_UI.
   {musarrow.i}
   RUN enable_UI. 
   IF vart = "AND" THEN DISABLE FILL-IN_AR FILL-IN_MANAD WITH FRAME {&FRAME-NAME}.
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANDKINDAPP.P PERSISTENT SET andkindapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANDKINDAPP.P PERSISTENT SET andkindapph.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN_AR FILL-IN_MANAD FILL-IN_VARDE 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_AR FILL-IN_MANAD FILL-IN_VARDE BTN_OK BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF vart = "NYA" THEN DO:     
      ASSIGN FILL-IN_AR = YEAR(TODAY).                       
   END.   
   ELSE DO:           
      FIND kindextemp WHERE kindextemp.AR = artalvar AND 
      kindextemp.MANAD = manadvar NO-LOCK NO-ERROR.     
      ASSIGN             
      FILL-IN_AR = kindextemp.AR
      FILL-IN_MANAD = kindextemp.MANAD
      FILL-IN_VARDE = kindextemp.VARDE.                          
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI DIALOG-1 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
 /* */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

