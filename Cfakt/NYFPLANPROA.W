&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
{FAKTSTART.I}
&Scoped-define NEW 
{FAKTPLANTEMP.I}                 
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER offp AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR efaktstarttemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tot% AS DECIMAL NO-UNDO.
DEFINE VARIABLE belopp% AS DECIMAL NO-UNDO.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS efaktstarttemp.PLAN% efaktstarttemp.BELOPP ~
efaktstarttemp.FRITEXT efaktstarttemp.PLANDATUM 
&Scoped-define ENABLED-TABLES efaktstarttemp
&Scoped-define FIRST-ENABLED-TABLE efaktstarttemp
&Scoped-Define ENABLED-OBJECTS BTN_NVE BTN_FVE BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS efaktstarttemp.FAKTNR efaktstarttemp.PLAN% ~
efaktstarttemp.BELOPP efaktstarttemp.FRITEXT efaktstarttemp.PLANDATUM 
&Scoped-define DISPLAYED-TABLES efaktstarttemp
&Scoped-define FIRST-DISPLAYED-TABLE efaktstarttemp
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_OFFERTPRIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_OFFERTPRIS AS DECIMAL FORMAT "->>>>>>>>9.99" INITIAL 0 
     LABEL "Offertpris" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     efaktstarttemp.FAKTNR AT ROW 1.5 COL 15.38 COLON-ALIGNED
          LABEL "Fakturaplannr."
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     FILL-IN_OFFERTPRIS AT ROW 3 COL 15.38 COLON-ALIGNED
     efaktstarttemp.PLAN% AT ROW 7.38 COL 11.25 COLON-ALIGNED
          LABEL "Andel i %"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     efaktstarttemp.BELOPP AT ROW 7.38 COL 24.88 COLON-ALIGNED
          LABEL "Belopp" FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     efaktstarttemp.FRITEXT AT ROW 8.54 COL 11.25 COLON-ALIGNED
          LABEL "Fritext"
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     BTN_NVE AT ROW 9.92 COL 27.63
     efaktstarttemp.PLANDATUM AT ROW 10.17 COL 16.13 COLON-ALIGNED
          LABEL "Plan.datum"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     BTN_FVE AT ROW 10.92 COL 27.63
     BTN_OK AT ROW 12.5 COL 15.25
     BTN_AVBRYT AT ROW 12.5 COL 30.25
     "Ange hur många procent av totalsumman" VIEW-AS TEXT
          SIZE 42.88 BY 1.13 AT ROW 4.92 COL 1.5
          FONT 17
     "som skall faktureras samt när!" VIEW-AS TEXT
          SIZE 42.63 BY 1.25 AT ROW 6 COL 1.5
          FONT 17
     SPACE(0.86) SKIP(6.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Betalningsplan":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: efaktstarttemp T "?" NO-UNDO temp-db efaktstarttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN efaktstarttemp.BELOPP IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN efaktstarttemp.FAKTNR IN FRAME DIALOG-3
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN_OFFERTPRIS IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN efaktstarttemp.FRITEXT IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN efaktstarttemp.PLAN% IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN efaktstarttemp.PLANDATUM IN FRAME DIALOG-3
   EXP-LABEL                                                            */
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
ON END-ERROR OF FRAME DIALOG-3 /* Betalningsplan */
DO:   
  musz = TRUE.
  {BORTBRWPROC.I}
  RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Betalningsplan */
DO:   
  musz = TRUE.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME efaktstarttemp.BELOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL efaktstarttemp.BELOPP DIALOG-3
ON LEAVE OF efaktstarttemp.BELOPP IN FRAME DIALOG-3 /* Belopp */
DO:
   IF INPUT efaktstarttemp.BELOPP > FILL-IN_OFFERTPRIS THEN DO:
      MESSAGE "Mer än " FILL-IN_OFFERTPRIS " går inte!" VIEW-AS ALERT-BOX.
   END.   
   ELSE DO:
      IF efaktstarttemp.BELOPP = INPUT efaktstarttemp.BELOPP THEN musz = musz.
      ELSE DO:
         DO TRANSACTION:
            efaktstarttemp.BELOPP = INPUT efaktstarttemp.BELOPP.
            efaktstarttemp.PLAN% = (efaktstarttemp.BELOPP / FILL-IN_OFFERTPRIS) * 100.
            DISPLAY efaktstarttemp.PLAN% WITH FRAME {&FRAME-NAME}.
         END.         
      END.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:  
   musz = TRUE.
    APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-3
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-3 /* - */
DO: 
   ASSIGN
   efaktstarttemp.PLANDATUM = INPUT efaktstarttemp.PLANDATUM.   
   efaktstarttemp.PLANDATUM = efaktstarttemp.PLANDATUM - 1.      
   DISPLAY efaktstarttemp.PLANDATUM WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-3
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-3 /* + */
DO:   
   ASSIGN
   efaktstarttemp.PLANDATUM = INPUT efaktstarttemp.PLANDATUM.   
   efaktstarttemp.PLANDATUM = efaktstarttemp.PLANDATUM + 1.        
   DISPLAY efaktstarttemp.PLANDATUM WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:   
   musz = FALSE.
   IF INPUT efaktstarttemp.PLAN% > 100 THEN DO:
      MESSAGE "Mer än 100% går inte!".      
      RETURN NO-APPLY.  
   END. 
   IF efaktstarttemp.START = "" THEN DO:
      IF INPUT efaktstarttemp.PLANDATUM = ? THEN DO:
         MESSAGE "Ange datum!".      
         RETURN NO-APPLY.  
      END.
   END.     
   ASSIGN 
   efaktstarttemp.PLAN% = INPUT efaktstarttemp.PLAN%
   efaktstarttemp.PLANDATUM = INPUT efaktstarttemp.PLANDATUM.            
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME efaktstarttemp.FRITEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL efaktstarttemp.FRITEXT DIALOG-3
ON LEAVE OF efaktstarttemp.FRITEXT IN FRAME DIALOG-3 /* Fritext */
DO:
   efaktstarttemp.FRITEXT = INPUT efaktstarttemp.FRITEXT. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME efaktstarttemp.PLAN%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL efaktstarttemp.PLAN% DIALOG-3
ON LEAVE OF efaktstarttemp.PLAN% IN FRAME DIALOG-3 /* Andel i % */
DO:
   IF INPUT efaktstarttemp.PLAN% > 100 THEN DO:
      MESSAGE "Mer än 100% går inte!" VIEW-AS ALERT-BOX.
   END.   
   ELSE DO:
      IF efaktstarttemp.PLAN% = INPUT efaktstarttemp.PLAN% THEN musz = musz.
      ELSE DO:
         ASSIGN
         efaktstarttemp.PLAN% = INPUT efaktstarttemp.PLAN%
         efaktstarttemp.BELOPP = FILL-IN_OFFERTPRIS * efaktstarttemp.PLAN% / 100.
         DISPLAY efaktstarttemp.BELOPP WITH FRAME {&FRAME-NAME}.         
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME efaktstarttemp.PLANDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL efaktstarttemp.PLANDATUM DIALOG-3
ON MOUSE-MENU-CLICK OF efaktstarttemp.PLANDATUM IN FRAME DIALOG-3 /* Plan.datum */
DO:    
   IF INPUT efaktstarttemp.PLANDATUM NE ? THEN
   ASSIGN 
   Guru.GlobalaVariabler:regdatum = INPUT efaktstarttemp.PLANDATUM. 
   ELSE Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   efaktstarttemp.PLANDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY efaktstarttemp.PLANDATUM WITH FRAME {&FRAME-NAME}.
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
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-ERROR.
   FIND FIRST efaktstarttemp NO-ERROR.      
   FILL-IN_OFFERTPRIS = offp. 
   RUN enable_UI.       
   {FRMSIZED.I} 
   IF efaktstarttemp.START NE "" THEN DO:
      ASSIGN
      BTN_FVE:HIDDEN = TRUE 
      BTN_NVE:HIDDEN = TRUE 
      efaktstarttemp.PLANDATUM:HIDDEN = TRUE.
   END.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN_OFFERTPRIS 
      WITH FRAME DIALOG-3.
  IF AVAILABLE efaktstarttemp THEN 
    DISPLAY efaktstarttemp.FAKTNR efaktstarttemp.PLAN% efaktstarttemp.BELOPP 
          efaktstarttemp.FRITEXT efaktstarttemp.PLANDATUM 
      WITH FRAME DIALOG-3.
  ENABLE efaktstarttemp.PLAN% efaktstarttemp.BELOPP efaktstarttemp.FRITEXT 
         BTN_NVE efaktstarttemp.PLANDATUM BTN_FVE BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

