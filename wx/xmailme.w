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

  Created: 03/25/96 -  4:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
DEFINE NEW SHARED VARIABLE globniv LIKE ANVANDARE.AV-LEVEL NO-UNDO.
DEFINE NEW SHARED VARIABLE globallpers LIKE ANVANDARE.ALLPERS NO-UNDO. 
DEFINE NEW SHARED VARIABLE globallao LIKE ANVANDARE.ALLAONR NO-UNDO. 
DEFINE NEW SHARED VARIABLE globomr LIKE PERSONALTAB.OMRADE NO-UNDO. 
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE globsidl LIKE ANVANDARE.SIDL NO-UNDO.
DEFINE NEW SHARED VARIABLE globsids LIKE ANVANDARE.SIDS NO-UNDO.           
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE knappval AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED TEMP-TABLE meddpers    
   FIELD NAMN LIKE ANVANDARE.AV-NAMN 
   FIELD ANVANDARE LIKE ANVANDARE.ANVANDARE   
   INDEX ANV IS PRIMARY ANVANDARE ASCENDING.

DEFINE VAR objSession AS COM-HANDLE.
DEFINE VAR objMessage AS COM-HANDLE.
DEFINE VAR objRecip AS COM-HANDLE.
DEFINE VAR one AS LOGICAL INIT YES.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-42 RECT-43 EDITOR_MEDD BTN_OK BTN_SKR ~
BNT_AVB 
&Scoped-Define DISPLAYED-OBJECTS EDITOR_MEDD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BNT_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 2.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Registrera" 
     SIZE 12 BY 2.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut" 
     SIZE 12 BY 2.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 59.5 BY 15.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 50.5 BY 3.73
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     EDITOR_MEDD AT ROW 5.5 COL 5 NO-LABEL
     BTN_OK AT ROW 16 COL 8
     BTN_SKR AT ROW 16 COL 24.5
     BNT_AVB AT ROW 16 COL 41.5
     RECT-42 AT ROW 3.5 COL 1
     "Meddelande:" VIEW-AS TEXT
          SIZE 16.5 BY .82 AT ROW 15 COL 6.25
     "Meddelande" VIEW-AS TEXT
          SIZE 33 BY 2 AT ROW 1.5 COL 21.5
          FONT 17
     RECT-43 AT ROW 14.77 COL 5.5
     SPACE(5.00) SKIP(0.50)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Medelande till Guruanvändare".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BNT_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BNT_AVB DIALOG-1
ON CHOOSE OF BNT_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Registrera */
DO:
     
   EDITOR_MEDD = INPUT EDITOR_MEDD.
   CREATE "MAPI.SESSION" objSession.

   objSession:Logon().
   objMessage = objSession:OutBox:Messages:Add().
   objMessage:Subject = "4GL Automation Test".
   objMessage:Text = EDITOR_MEDD.
   objRecip = objMessage:Recipients:Add().
   /* TODO: PLEASE type in a valid email address inside the quotes */
    
    objRecip:Name = "<put address here>".
    objRecip:Type = 1.
    objRecip:Resolve.
    objMessage:Update(TRUE, TRUE).
    objMessage:Send(TRUE, FALSE).
    objSession:Logoff.
	
RELEASE OBJECT objRecip.
RELEASE OBJECT objMessage.
RELEASE OBJECT objSession.

MESSAGE "Completed" VIEW-AS ALERT-BOX.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W.          
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD DIALOG-1
ON LEAVE OF EDITOR_MEDD IN FRAME DIALOG-1
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
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
   
   RUN enable_UI.
   {musarrow.i}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1 _DEFAULT-ENABLE
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
  DISPLAY EDITOR_MEDD 
      WITH FRAME DIALOG-1.
  ENABLE RECT-42 RECT-43 EDITOR_MEDD BTN_OK BTN_SKR BNT_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
   OUTPUT TO PRINTER PAGE-SIZE VALUE(globsids)
   CONVERT TARGET "iso8859-1".           
   DISPLAY "MEDELANDE FRÅN :" AT 6
   EDITOR_MEDD AT 6  VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


