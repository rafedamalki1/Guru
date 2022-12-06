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

  Created: 02/13/96 - 10:32 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE rb-print AS CHARACTER NO-UNDO.
&Scoped-define NEW
{GLOBVAR2DEL1.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRtest FILL-IN_SIDS FILL-IN_SIDL BTN_OK ~
BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_ANVANDARE FILL-IN_AV-NAMN ~
FILL-IN_SIDS FILL-IN_SIDL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRtest 
     LABEL "Skrivar test" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_AV-NAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Användarnamn" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_SIDL AS INTEGER FORMAT ">99" INITIAL 55 
     LABEL "Sidlängd för liggande" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_SIDS AS INTEGER FORMAT ">99" INITIAL 73 
     LABEL "Sidlängd för stående" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_ANVANDARE AT ROW 1.5 COL 11.5 COLON-ALIGNED
     FILL-IN_AV-NAMN AT ROW 1.5 COL 27.5
     BTN_SKRtest AT ROW 1.5 COL 83.5
     FILL-IN_SIDS AT ROW 2.88 COL 22.63 COLON-ALIGNED
     FILL-IN_SIDL AT ROW 4.38 COL 22.63 COLON-ALIGNED
     BTN_OK AT ROW 4.46 COL 68.5
     BTN_AVS AT ROW 4.46 COL 83.5
     SPACE(1.49) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ange antal rader vid utskrift".


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

/* SETTINGS FOR FILL-IN FILL-IN_ANVANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AV-NAMN IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
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

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
/*    DO TRANSACTION:                                                   */
/*       FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv */
/*        NO-LOCK NO-ERROR.                                             */
/*       IF AVAILABLE anvandartemp THEN DO:                             */
/*          ASSIGN                                                      */

/*       END.                                                           */
/*    END.                                                              */
/*    musz = TRUE.                                                      */
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN  
   FILL-IN_SIDS = INPUT FILL-IN_SIDS
   FILL-IN_SIDL = INPUT FILL-IN_SIDL.
   ASSIGN
   globsidl = FILL-IN_SIDL
   globsids = FILL-IN_SIDS.
   RUN SKRIVVALT.W (INPUT FALSE,OUTPUT rb-print).   
   IF musz = FALSE THEN DO:
      IF rb-print = "" THEN DO:
         MESSAGE  VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN SKRIVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv,INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
      END.
      ELSE DO:
         RUN SKRIVAPPT.P 
         (INPUT Guru.Konstanter:globanv,INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
      END.   
      Guru.GlobalaVariabler:globsidl = globsidl.
      Guru.GlobalaVariabler:globsids = globsids.    
   END.
   ELSE RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRtest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRtest DIALOG-1
ON CHOOSE OF BTN_SKRtest IN FRAME DIALOG-1 /* Skrivar test */
DO:   
   ASSIGN  
   FILL-IN_SIDS = INPUT FILL-IN_SIDS
   FILL-IN_SIDL = INPUT FILL-IN_SIDL.
   ASSIGN
   globsidl = FILL-IN_SIDL
   globsids = FILL-IN_SIDS.
   MESSAGE "Vill du testa stående utskrift svara Ja" SKIP
           "Vill du testa liggande utskrift svara Nej" 
            VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Utskrift test?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      RUN utskap_UI.      
      MESSAGE "Glöm ej att välja stående utskrift,  under Egenskaper!" VIEW-AS ALERT-BOX.
      RUN SKRIVVALT.W (INPUT FALSE,OUTPUT rb-print).   
      IF musz = TRUE THEN DO:
         musz = FALSE.         
      END.                       
      ELSE DO:              
         FIND LAST tidut NO-LOCK NO-ERROR.     
         RUN EKLOGS.P.
      END.
      FOR EACH tidut:
         DELETE tidut.
      END.   
   END.
   ELSE IF NOT svar THEN DO:       
      RUN utskap_UI.      
      MESSAGE "Glöm ej att välja liggande utskrift, under Egenskaper! " VIEW-AS ALERT-BOX.
      RUN SKRIVVALT.W (INPUT TRUE,OUTPUT rb-print).   
      IF musz = TRUE THEN DO:
         musz = FALSE.         
      END.                       
      ELSE DO:              
         FIND LAST tidut NO-LOCK NO-ERROR.     
         RUN EKLOGL.P.
      END.
      FOR EACH tidut:
         DELETE tidut.
      END.   
   END.                    
   ELSE DO:
      musz = musz.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRtest DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKRtest IN FRAME DIALOG-1 /* Skrivar test */
DO:
   /*RUN SIDLANGD.W.  */
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
      /*
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
   */
   RUN grundtid_UI.
   RUN enable_UI.       
   FILL-IN_ANVANDARE:HIDDEN = TRUE.
   FILL-IN_AV-NAMN:HIDDEN = TRUE. 
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
  DISPLAY FILL-IN_ANVANDARE FILL-IN_AV-NAMN FILL-IN_SIDS FILL-IN_SIDL 
      WITH FRAME DIALOG-1.
  ENABLE BTN_SKRtest FILL-IN_SIDS FILL-IN_SIDL BTN_OK BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF globsidl = 0 THEN globsidl = 53.
   IF globsids = 0 THEN globsids = 72.

   
      /*
   FILL-IN_ANVANDARE = anvandartemp.ANVANDARE    
   FILL-IN_AV-NAMN = anvandartemp.AV-NAMN
   */
   ASSIGN   
   FILL-IN_SIDL = globsidl    
   FILL-IN_SIDS = globsids.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utskap_UI DIALOG-1 
PROCEDURE utskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   REPEAT i = 1 TO 145:
      CREATE tidut.
      ASSIGN tidut.UT = "Detta är rad nr " + STRING(i).
      IF i = 1 THEN tidut.UT = tidut.UT + " Inställningar liggande: " + STRING(globsidl) + " stående " + STRING(globsids).
      IF i = 2 THEN tidut.UT = tidut.UT + " OBS! Du skall få 3 rader mindre på varje sida än vad du har angivit!".
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

