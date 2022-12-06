&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/12/96 -  3:11 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ALLDEF.I}
{INKTEMP.I}
{GLOBVAR2DEL1.I}

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eink_mtrl.  
DEFINE OUTPUT PARAMETER raddep AS INTEGER NO-UNDO.      

/* Local Variable Definitions ---                                       */

&Scoped-define NEW 


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE inkberh AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_UP FILL-IN-ANTAL BTN_MIN RAD_DEP BTN_OK ~
BTN_AVB FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-LEV FILL-IN-BESTANT 
&Scoped-Define DISPLAYED-OBJECTS EDD_INFO FILL-IN-ANTAL FILL-IN-DEP RAD_DEP ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET FILL-IN-LEV FILL-IN-BESTANT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.6 BY .76.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.6 BY .76.

DEFINE VARIABLE EDD_INFO AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 29.6 BY 5 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Antal från vald leverantör" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTANT AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Lägsta antal" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DEP AS CHARACTER FORMAT "X(256)":U INITIAL "Överskjutande antal ska packas:" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.19
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV AS CHARACTER FORMAT "X(4)":U INITIAL "0" 
     LABEL "Lev-id" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_DEP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Till depå", 1,
"Till konstruktion", 2
     SIZE 12 BY 1.76 TOOLTIP "Hur överskjutande antal ska packas?" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     EDD_INFO AT ROW 6 COL 6 NO-LABEL
     BTN_UP AT ROW 13.19 COL 37.2
     FILL-IN-ANTAL AT ROW 13.67 COL 27 COLON-ALIGNED
     BTN_MIN AT ROW 14.33 COL 37.2
     FILL-IN-DEP AT ROW 16.76 COL 2.8 NO-LABEL WIDGET-ID 6
     RAD_DEP AT ROW 18.14 COL 12.8 NO-LABEL WIDGET-ID 2
     BTN_OK AT ROW 20.52 COL 17.6
     BTN_AVB AT ROW 20.52 COL 32.6
     FILL-IN-ENR AT ROW 1.76 COL 11.6 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.1 COL 11.6 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.43 COL 11.6 COLON-ALIGNED
     FILL-IN-LEV AT ROW 4.43 COL 33.6 COLON-ALIGNED
     FILL-IN-BESTANT AT ROW 11.91 COL 27 COLON-ALIGNED
     SPACE(13.37) SKIP(9.29)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Antal från vald leverantör.".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDD_INFO IN FRAME DIALOG-2
   NO-ENABLE                                                            */
ASSIGN 
       EDD_INFO:READ-ONLY IN FRAME DIALOG-2        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DEP IN FRAME DIALOG-2
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-DEP:READ-ONLY IN FRAME DIALOG-2        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Antal från vald leverantör. */
DO:
   musz = TRUE.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Antal från vald leverantör. */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:  
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-2
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-2 /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0." VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:           
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   RAD_DEP = INPUT RAD_DEP.
   raddep = RAD_DEP. 
   IF string(FILL-IN-ANTAL) = "?" OR FILL-IN-ANTAL = ? THEN DO:
      MESSAGE "Felaktigt ifyllt!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".                   
      RETURN NO-APPLY.
   END. 
   IF FILL-IN-LEV = "0" THEN DO:
      IF FILL-IN-BESTANT < FILL-IN-ANTAL THEN DO:
         MESSAGE "Antal från depå kan inte vara större än antal från beredningen." 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.   
      IF FILL-IN-ANTAL = 0 THEN DO:
         MESSAGE "Antal kan inte vara 0" 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.      
      IF Guru.Konstanter:globforetag = "snat" THEN DO:     
         IF eink_mtrl.DEPASALDO > 0 AND FILL-IN-ANTAL > eink_mtrl.DEPASALDO THEN DO:            
            MESSAGE "Antal från vald depå kan inte vara större än lagersaldo för visat lager "  + STRING(eink_mtrl.DEPASALDO)
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
            RETURN NO-APPLY.
         END.                  
      END.              
   END.
   ELSE IF Guru.Konstanter:globforetag = "bode" OR Guru.Konstanter:globforetag = "KRAF" AND FILL-IN-LEV = "6" THEN DO:
      IF FILL-IN-BESTANT < FILL-IN-ANTAL THEN DO:
         MESSAGE "Antal från depå kan inte vara större än antal från beredningen." 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.   
      IF FILL-IN-ANTAL = 0 THEN DO:
         MESSAGE "Antal kan inte vara 0" 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.              
      IF FILL-IN-ANTAL > eink_mtrl.DEPASALDO THEN DO:
         MESSAGE "Antal från Depå Boden kan inte vara större än lagersaldo " + STRING(eink_mtrl.DEPASALDO)
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.
   END.     
   ELSE IF Guru.Konstanter:globforetag = "GKAL"  AND FILL-IN-LEV = "20" THEN DO:
      IF FILL-IN-BESTANT < FILL-IN-ANTAL THEN DO:
         MESSAGE "Antal från depå kan inte vara större än antal från beredningen." 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.   
      IF FILL-IN-ANTAL = 0 THEN DO:
         MESSAGE "Antal kan inte vara 0" 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.         
      IF FILL-IN-ANTAL > eink_mtrl.DEPASALDO THEN DO:
         MESSAGE "Antal från Katalog Kalmar kan inte vara större än lagersaldo " + STRING(eink_mtrl.DEPASALDO)
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.         
   END.   
   ELSE IF FILL-IN-BESTANT > FILL-IN-ANTAL THEN DO:      
      IF eink_mtrl.TILL = "DEPÅ" OR eink_mtrl.TILL = "KONST" THEN.
      ELSE DO: 
         MESSAGE "Det går inte att beställa mindre än vad beredningen anger." 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
         RETURN NO-APPLY.
      END.          
   END.    
   IF eink_mtrl.TILL = "DEPÅ" OR eink_mtrl.TILL = "KONST" THEN.
   ELSE eink_mtrl.BERANTAL    = eink_mtrl.INKANTAL. 
   ASSIGN          
   eink_mtrl.INKANTAL    = FILL-IN-ANTAL.    
   APPLY "GO" TO FRAME {&FRAME-NAME}.                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-2
ON CHOOSE OF BTN_UP IN FRAME DIALOG-2 /* + */
DO:
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.   
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   FIND FIRST eink_mtrl NO-LOCK NO-ERROR.    
   ASSIGN
   EDD_INFO = "Ska du beställa ett mindre"  + chr(10) + "antal än vad beredningen anger." + chr(10) + 
   "Börja med att byta leverantör" + chr(10) + "till 'Från Depå' och ange!" + chr(10) + "antalet där."
   FILL-IN-BEN     = eink_mtrl.BENAMNING     
   FILL-IN-ENR     = eink_mtrl.ENR
   FILL-IN-LEV     = eink_mtrl.LEVKODINK
   FILL-IN-ENHET   = eink_mtrl.ENHET
   FILL-IN-BESTANT = eink_mtrl.INKANTAL
   FILL-IN-ANTAL   = eink_mtrl.INKANTAL.
   IF FILL-IN-LEV = "0" THEN DO:
      FILL-IN-BESTANT:LABEL = "Högsta antal".
   END.     
   IF eink_mtrl.TILL = "DEPÅ" THEN DO:
      ASSIGN
      FILL-IN-LEV = eink_mtrl.TILL. 
      FILL-IN-LEV:LABEL = "Till".     
   END. 
   RAD_DEP = 1.
   RUN enable_UI.   
   IF eink_mtrl.TILL = "DEPÅ" OR eink_mtrl.TILL = "KONST" THEN DO:
      ASSIGN
      FILL-IN-BESTANT:HIDDEN = TRUE.
   END. 
   ASSIGN
   RAD_DEP = 1
   FILL-IN-DEP:HIDDEN = TRUE
   RAD_DEP:HIDDEN = TRUE.
   IF eink_mtrl.TILL = "KONST" THEN RAD_DEP = 2.
   RAD_DEP:SCREEN-VALUE = STRING(RAD_DEP).  
   IF eink_mtrl.TILL = "DEPÅ" OR eink_mtrl.TILL = "KONST" THEN DO:
      ASSIGN
      FILL-IN-DEP:HIDDEN = TRUE
      RAD_DEP:HIDDEN = TRUE.
   END.   
   ELSE IF FILL-IN-LEV = "0" THEN DO:
      ASSIGN
      FILL-IN-DEP:HIDDEN = TRUE
      RAD_DEP:HIDDEN = TRUE.
   END.         
   ELSE DO:
      ASSIGN
      FILL-IN-DEP:HIDDEN = FALSE
      RAD_DEP:HIDDEN = FALSE.
   END.         
   {FRMSIZED.I}  
   {musarrow.i}
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  DISPLAY EDD_INFO FILL-IN-ANTAL FILL-IN-DEP RAD_DEP FILL-IN-ENR FILL-IN-BEN 
          FILL-IN-ENHET FILL-IN-LEV FILL-IN-BESTANT 
      WITH FRAME DIALOG-2.
  ENABLE BTN_UP FILL-IN-ANTAL BTN_MIN RAD_DEP BTN_OK BTN_AVB FILL-IN-ENR 
         FILL-IN-BEN FILL-IN-ENHET FILL-IN-LEV FILL-IN-BESTANT 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

