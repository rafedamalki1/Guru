&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vidnrp AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER juid AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER vjuid AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ktoproch AS HANDLE NO-UNDO. /* Procedure handle till KTOAPP.P */
DEFINE SHARED VARIABLE skarpproch AS HANDLE NO-UNDO. /* SKARPAPP.P */
DEFINE VARIABLE aokaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE aokdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ksbenamn AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Jur pers" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "För juridisk person" 
      VIEW-AS TEXT 
     SIZE 30 BY .96
     FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     CMB_JURP AT ROW 3.13 COL 9.75 COLON-ALIGNED
     BTN_OK AT ROW 5 COL 16.75
     FILL-IN-TEXT AT ROW 1.5 COL 1.5 NO-LABEL
     SPACE(0.49) SKIP(3.86)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Juridisk person för nummerserieprefix":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DIALOG-3
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_JURP:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-3
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Juridisk person för nummerserieprefix */
DO:
   {muswait.i}
   musz = TRUE.   
   {BORTBRWPROC.I}
   IF VALID-HANDLE(skarpproch) THEN DELETE PROCEDURE skarpproch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Juridisk person för nummerserieprefix */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}   
   CMB_JURP = INPUT CMB_JURP.
   FIND FIRST jurtemp WHERE jurtemp.NAMN = CMB_JURP NO-ERROR.
   ASSIGN
   juid = jurtemp.JUDID
   vjuid = jurtemp.VIJUDID.   
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:      
      RUN spnrsjur_UI IN skarpproch (INPUT vidnrp, INPUT juid).   
   END.
   {BORTBRWPROC.I}
   
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   {ALLSTARTDYN.I}         
   FIND FIRST jurtemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurtemp THEN DO:
      RUN jurhmt_UI IN ktoproch
      (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE jurtemp).  
   END.        
   CMB_JURP:DELETE("").
   FOR EACH jurtemp:      
      CMB_JURP:ADD-LAST(jurtemp.NAMN).
   END.
   FIND FIRST jurtemp  NO-ERROR.   
   ASSIGN CMB_JURP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = jurtemp.NAMN.
   DISPLAY FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.
   
   RUN enable_UI.          
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      ENABLE CMB_JURP WITH FRAME {&FRAME-NAME}.
   END.
   
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/         
   /*konton*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KTOAPP.P PERSISTENT SET ktoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN KTOAPP.P PERSISTENT SET ktoproch (INPUT Guru.Konstanter:globanv).   
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKARPAPP.P PERSISTENT SET skarpproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN SKARPAPP.P PERSISTENT SET skarpproch.         
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-TEXT 
      WITH FRAME DIALOG-3.
  ENABLE BTN_OK 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

