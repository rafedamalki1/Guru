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

  Created: 12/18/96 - 10:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER val AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER krow AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER nykonst AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER kongrkod AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER konstvalvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER bbval AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER varkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{UTBYTESLNAMNTEMP.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE sparkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ord AS INTEGER NO-UNDO.
DEFINE VARIABLE brec AS RECID NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(8)" NO-UNDO.   
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE nykonstuapph AS HANDLE NO-UNDO. 
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE benvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ers_uid AS INTEGER NO-UNDO INIT 0.
DEFINE VARIABLE utval AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE utbytlg AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-KOD FILL-IN-BEN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOD FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE cmb_utbyteslkund AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Utbyteslista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",         0
     DROP-DOWN-LIST
     SIZE 29.5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-KOD AT ROW 1.5 COL 13.75 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.75 COL 13.75 COLON-ALIGNED
     cmb_utbyteslkund AT ROW 4 COL 14 COLON-ALIGNED WIDGET-ID 4
     BTN_OK AT ROW 5.75 COL 14.5
     BTN_AVB AT ROW 5.75 COL 29.5
     SPACE(6.12) SKIP(0.41)
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cmb_utbyteslkund IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cmb_utbyteslkund:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1
DO:
  IF VALID-HANDLE(nykonstuapph) THEN DELETE PROCEDURE nykonstuapph.
  musz = TRUE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   {muswait.i}
   FILL-IN-KOD = INPUT FILL-IN-KOD.   
   IF FILL-IN-KOD = " " THEN DO:
      MESSAGE "Kod får inte vara blank."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-KOD.
      RETURN NO-APPLY.
   END.
   FILL-IN-KOD = TRIM(FILL-IN-KOD).
   DISPLAY FILL-IN-KOD WITH FRAME {&FRAME-NAME}.
   RUN AONRNAMNKOLL.P (INPUT 4, INPUT FILL-IN-KOD,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "ENTRY" TO FILL-IN-KOD.
      RETURN NO-APPLY.
   END.      
   ASSIGN
   varkod = INPUT FILL-IN-KOD
   benvar = INPUT FILL-IN-BEN
   ers_uid = INPUT cmb_utbyteslkund
   felmedd = "".
   utval = "".
   
   IF ers_uid = ? THEN
        ers_uid = 0.
     
   IF val = TRUE THEN DO:
      RUN konstvalok_UI IN nykonstuapph (INPUT val,INPUT krow,INPUT nykonst,INPUT sparkod,
                                 INPUT kongrkod,INPUT konstvalvar,
                                 INPUT bbval,INPUT varkod,INPUT benvar,
                                 INPUT TABLE konstvaltemp,OUTPUT felmedd,OUTPUT utval).
   END.
   ELSE DO:
      RUN konstrok_UI IN nykonstuapph (INPUT val,INPUT nykonst, INPUT ers_uid, INPUT sparkod,
                                 INPUT kongrkod,INPUT konstvalvar,
                                 INPUT bbval,INPUT varkod,
                                 INPUT benvar,INPUT-OUTPUT TABLE konsttemp,
                                 OUTPUT felmedd,OUTPUT utval).
   END.
   IF utval NE " " THEN DO:
      IF felmedd NE " " THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         IF utval = "GO" THEN DO:
            APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
            LEAVE.
         END.
         IF utval = "ENTRY" THEN DO:
            APPLY "ENTRY" TO FILL-IN-KOD.
            LEAVE.
         END.
      END.
      ELSE DO:
         APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      IF felmedd NE " " THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         APPLY "ENTRY" TO FILL-IN-KOD.
      END.
      ELSE DO:
         APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
  IF VALID-HANDLE(nykonstuapph) THEN DELETE PROCEDURE nykonstuapph.  
  musz = FALSE.
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
   {ALLSTARTDYN.I}     
   
   utbytlg = TRUE.           
   
   CMB_utbyteslkund:LIST-ITEM-PAIRS = ?. 
   
   IF val = TRUE THEN
   DO:
      utbytlg = FALSE.
   END.
   
   IF utbytlg = TRUE THEN 
   DO:
      ENABLE cmb_utbyteslkund WITH FRAME DIALOG-1.
      cmb_utbyteslkund:LABEL = Guru.Konstanter:gutbytk.
                
      FOR EACH utbyteslnamntemp. 
          status-ok = CMB_utbyteslkund:ADD-LAST(utbyteslnamntemp.UNAMN,utbyteslnamntemp.UID).               
      END.
   END.
   ELSE cmb_utbyteslkund:HIDDEN = TRUE.
   
   IF nykonst = TRUE THEN DO:
      IF val = TRUE THEN DO: 
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nyupplägg av " + bbval.
      END.
      ELSE DO:
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nyupplägg av " + bbval.         
      END.
   END.
   ELSE DO:
      IF val = TRUE THEN DO: 
         FIND FIRST konstvaltemp WHERE konstvaltemp.KVALROW = krow NO-LOCK NO-ERROR. 
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av " + bbval.           
         ASSIGN
         FILL-IN-KOD = konstvaltemp.KVALKOD
         FILL-IN-BEN = konstvaltemp.BENAMNING           
         sparkod = konstvaltemp.KVALKOD.
      END.
      ELSE DO:
           
         FIND FIRST konsttemp WHERE konsttemp.KTYPKOD = konstvalvar NO-LOCK NO-ERROR.
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av " + bbval.   
         ASSIGN
         FILL-IN-KOD = konsttemp.KTYPKOD
         FILL-IN-BEN = konsttemp.BENAMNING
         cmb_utbyteslkund = konsttemp.UID
         sparkod = konsttemp.KTYPKOD.
         IF utbytlg = TRUE THEN
            DISPLAY cmb_utbyteslkund WITH FRAME {&FRAME-NAME}.
      END.      
   END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYKONSTUAPP.P PERSISTENT SET nykonstuapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYKONSTUAPP.P PERSISTENT SET nykonstuapph.
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
  DISPLAY FILL-IN-KOD FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-KOD FILL-IN-BEN BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

