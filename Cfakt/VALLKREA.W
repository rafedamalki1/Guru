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

  Created: 08/19/96 -  8:22 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{FAKTBILAG.I}
{FAKTPLANTEMP.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kreditnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR efaktkredtemp.
DEFINE OUTPUT PARAMETER TABLE FOR fakbilag.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE skrivutalla AS LOGICAL NO-UNDO.
DEFINE VARIABLE varbi AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED ~
TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON ~
TOG_FRI TOG_KONT BTN_EXCEL BTN_IE BTN_OK BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS TOG_INGA TOG_TIDMED TOG_TIDTOT ~
TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM ~
TOG_LON TOG_FRI TOG_KONT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL AUTO-GO 
     LABEL "excel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IE AUTO-GO 
     LABEL "visa i IE" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE TOG_AFAK AS LOGICAL INITIAL no 
     LABEL "Skriv ut alla fakturor" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.63 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_FRI AS LOGICAL INITIAL no 
     LABEL "Visa fria kompletteringsposter" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.5 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_INGA AS LOGICAL INITIAL no 
     LABEL "Inga bilagor" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.63 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KONT AS LOGICAL INITIAL no 
     LABEL "Visa kontering" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.5 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KOST AS LOGICAL INITIAL no 
     LABEL "Visa ingående kostnadsregistrering" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.25 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_LON AS LOGICAL INITIAL no 
     LABEL "Visa lönetillägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.25 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_PRIS AS LOGICAL INITIAL no 
     LABEL "Visa kostnadsfälten" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDEJMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ej ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCK AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med klockslag" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCKNAM AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med klockslag och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.38 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOT AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med totaltid" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOTNAMN AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med totaltid och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_AFAK AT ROW 3.08 COL 1.5
     TOG_INGA AT ROW 3.08 COL 47.75
     TOG_TIDMED AT ROW 3.88 COL 1.5
     TOG_TIDTOT AT ROW 3.88 COL 47.75
     TOG_TIDEJMED AT ROW 4.63 COL 1.5
     TOG_TIDKLOCK AT ROW 4.63 COL 47.75
     TOG_PRIS AT ROW 5.42 COL 1.5
     TOG_TIDTOTNAMN AT ROW 5.42 COL 47.75
     TOG_KOST AT ROW 6.17 COL 1.5
     TOG_TIDKLOCKNAM AT ROW 6.17 COL 47.75
     TOG_LON AT ROW 7 COL 1.5
     TOG_FRI AT ROW 7 COL 47.75
     TOG_KONT AT ROW 7.83 COL 47.75
     BTN_EXCEL AT ROW 8.96 COL 34 WIDGET-ID 2
     BTN_IE AT ROW 8.96 COL 48.88
     BTN_OK AT ROW 8.96 COL 63.75
     BTN_AVSL AT ROW 8.96 COL 78.63
     "BILAGOR :" VIEW-AS TEXT
          SIZE 19.75 BY 1 AT ROW 2.04 COL 1.5
          FONT 17
     SPACE(71.99) SKIP(7.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj bilagor".


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

/* SETTINGS FOR TOGGLE-BOX TOG_AFAK IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_AFAK:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:
   
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL DIALOG-1
ON CHOOSE OF BTN_EXCEL IN FRAME DIALOG-1 /* excel */
DO:
   visvalvar = 5.
   musz = FALSE.
   RUN ok_UI.
   /*
   {BORTBRWPROC.I}
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IE DIALOG-1
ON CHOOSE OF BTN_IE IN FRAME DIALOG-1 /* visa i IE */
DO:
   visvalvar = 3.
   musz = FALSE.
   RUN ok_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   visvalvar = 1.
   musz = FALSE.
   RUN ok_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AFAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AFAK DIALOG-1
ON VALUE-CHANGED OF TOG_AFAK IN FRAME DIALOG-1 /* Skriv ut alla fakturor */
DO:                 
   TOG_AFAK = INPUT TOG_AFAK.    
   skrivutalla = TOG_AFAK.  
   IF TOG_AFAK = TRUE THEN DO:       
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
      musz = FALSE.
      APPLY "WINDOW-CLOSE":U TO FRAME DIALOG-1.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FRI DIALOG-1
ON VALUE-CHANGED OF TOG_FRI IN FRAME DIALOG-1 /* Visa fria kompletteringsposter */
DO:
   IF INPUT TOG_FRI = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INGA DIALOG-1
ON VALUE-CHANGED OF TOG_INGA IN FRAME DIALOG-1 /* Inga bilagor */
DO:                 
   TOG_INGA = INPUT TOG_INGA.       
   IF TOG_INGA = TRUE THEN DO: 
      ASSIGN 
      TOG_FRI = FALSE 
      TOG_KOST = FALSE
      TOG_LON = FALSE
      TOG_PRIS = FALSE
      TOG_TIDEJMED = FALSE
      TOG_TIDKLOCK = FALSE 
      TOG_TIDMED = FALSE
      TOG_TIDTOT = FALSE
      TOG_KONT = FALSE
      TOG_TIDKLOCKNAM = FALSE
      TOG_TIDTOTNAMN = FALSE.
      DISPLAY       
      TOG_KONT 
      TOG_FRI 
      TOG_INGA 
      TOG_KOST 
      TOG_LON 
      TOG_PRIS 
      TOG_TIDEJMED 
      TOG_TIDKLOCK 
      TOG_TIDMED 
      TOG_TIDTOT  
      TOG_TIDKLOCKNAM 
      TOG_TIDTOTNAMN
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      TOG_KOST = TRUE
      TOG_FRI = TRUE
      TOG_PRIS = TRUE
      TOG_LON = TRUE
      TOG_TIDMED = TRUE.      
   
      DISPLAY  
      TOG_FRI 
      TOG_INGA 
      TOG_KOST 
      TOG_LON 
      TOG_PRIS 
      TOG_TIDEJMED 
      TOG_TIDKLOCK 
      TOG_TIDMED 
      TOG_TIDTOT  
      WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONT DIALOG-1
ON VALUE-CHANGED OF TOG_KONT IN FRAME DIALOG-1 /* Visa kontering */
DO:
   IF INPUT TOG_KONT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOST DIALOG-1
ON VALUE-CHANGED OF TOG_KOST IN FRAME DIALOG-1 /* Visa ingående kostnadsregistrering */
DO:
   IF INPUT TOG_KOST = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LON DIALOG-1
ON VALUE-CHANGED OF TOG_LON IN FRAME DIALOG-1 /* Visa lönetillägg */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PRIS DIALOG-1
ON VALUE-CHANGED OF TOG_PRIS IN FRAME DIALOG-1 /* Visa kostnadsfälten */
DO:
   IF INPUT TOG_PRIS = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDEJMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDEJMED DIALOG-1
ON VALUE-CHANGED OF TOG_TIDEJMED IN FRAME DIALOG-1 /* Visa tidskrivning som ej ingår i fakturan */
DO:
   IF INPUT TOG_TIDEJMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCK DIALOG-1
ON VALUE-CHANGED OF TOG_TIDKLOCK IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med klockslag */
DO:
   IF INPUT TOG_TIDKLOCK = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCKNAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCKNAM DIALOG-1
ON VALUE-CHANGED OF TOG_TIDKLOCKNAM IN FRAME DIALOG-1 /* Visa det. tid. med klockslag och namn */
DO:
   IF INPUT TOG_TIDKLOCKNAM = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDMED DIALOG-1
ON VALUE-CHANGED OF TOG_TIDMED IN FRAME DIALOG-1 /* Visa tidskrivning som ingår i fakturan */
DO:
   IF INPUT TOG_TIDMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOT DIALOG-1
ON VALUE-CHANGED OF TOG_TIDTOT IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med totaltid */
DO:
   IF INPUT TOG_TIDTOT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOTNAMN DIALOG-1
ON VALUE-CHANGED OF TOG_TIDTOTNAMN IN FRAME DIALOG-1 /* Visa det. tid. med totaltid och namn */
DO:
   IF INPUT TOG_TIDTOTNAMN = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
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
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   ASSIGN
   TOG_KOST = TRUE
   TOG_FRI = TRUE
   TOG_PRIS = TRUE
   TOG_LON = TRUE
   TOG_TIDMED = TRUE.  
   FIND FIRST efaktkredtemp WHERE efaktkredtemp.FAKTNR = infakplannr AND efaktkredtemp.FDELNR = kreditnrvar NO-ERROR.
   IF AVAILABLE efaktkredtemp THEN DO:
      varbi = efaktkredtemp.BILAGOR.                         
      IF varbi = "" THEN DO:
         ASSIGN
         TOG_KOST = TRUE
         TOG_FRI = TRUE
         TOG_PRIS = TRUE
         TOG_LON = TRUE
         TOG_TIDMED = TRUE.
      END.
      ELSE DO:
         ASSIGN
         TOG_KOST = FALSE
         TOG_FRI = FALSE
         TOG_PRIS = FALSE
         TOG_LON = FALSE
         TOG_TIDMED = FALSE.
         IF varbi = ",0," THEN TOG_INGA = TRUE.
         ELSE DO:
            IF INDEX(varbi,",1,") NE 0 THEN TOG_FRI = TRUE.
            IF INDEX(varbi,",2,") NE 0 THEN TOG_KOST = TRUE.
            IF INDEX(varbi,",3,") NE 0 THEN TOG_TIDEJMED = TRUE.
            IF INDEX(varbi,",4,") NE 0 THEN TOG_TIDKLOCK = TRUE.
            IF INDEX(varbi,",5,") NE 0 THEN TOG_TIDMED = TRUE.
            IF INDEX(varbi,",6,") NE 0 THEN TOG_TIDTOT = TRUE.
            IF INDEX(varbi,",7,") NE 0 THEN TOG_LON = TRUE.
            IF INDEX(varbi,",8,") NE 0 THEN TOG_PRIS = TRUE.
            IF INDEX(varbi,",9,") NE 0 THEN TOG_TIDKLOCKNAM = TRUE.
            IF INDEX(varbi,",10,") NE 0 THEN TOG_TIDTOTNAMN = TRUE. 
            IF INDEX(varbi,",11,") NE 0 THEN TOG_KONT = TRUE. 
         END.
      END.   
   END.   
   RUN enable_UI.       
   {FRMSIZED.I}     
   {musarrow.i}
   IF Guru.Konstanter:globforetag = "ELPA" OR  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" THEN .
   ELSE DO:     
      BTN_IE:HIDDEN = TRUE.
      BTN_OK:COLUMN = BTN_IE:COLUMN.
   END.
   /*
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   */
   IF Guru.Konstanter:globforetag = "elpa" THEN.
   ELSE BTN_EXCEL:HIDDEN = TRUE.  
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
  DISPLAY TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS 
          TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI TOG_KONT 
      WITH FRAME DIALOG-1.
  ENABLE TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS 
         TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI TOG_KONT 
         BTN_EXCEL BTN_IE BTN_OK BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI DIALOG-1 
PROCEDURE ok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   TOG_FRI = INPUT FRAME {&FRAME-NAME} TOG_FRI
   TOG_KOST = INPUT TOG_KOST 
   TOG_TIDEJMED = INPUT TOG_TIDEJMED
   TOG_TIDKLOCK = INPUT TOG_TIDKLOCK
   TOG_TIDMED = INPUT TOG_TIDMED
   TOG_TIDTOT = INPUT TOG_TIDTOT
   TOG_LON = INPUT TOG_LON
   TOG_PRIS = INPUT TOG_PRIS
   TOG_KONT = INPUT TOG_KONT
   TOG_TIDKLOCKNAM = INPUT TOG_TIDKLOCKNAM
   TOG_TIDTOTNAMN = INPUT TOG_TIDTOTNAMN.
   
   IF TOG_INGA = FALSE THEN DO:      
      IF TOG_FRI = FALSE AND TOG_KOST = FALSE AND TOG_TIDEJMED = FALSE AND
      TOG_TIDKLOCK = FALSE AND TOG_TIDMED = FALSE AND TOG_TIDTOT = FALSE AND
      TOG_LON = FALSE AND TOG_PRIS = FALSE AND TOG_TIDKLOCKNAM = FALSE AND
      TOG_TIDTOTNAMN = FALSE THEN musz = musz.
      ELSE DO:
         CREATE fakbilag.
         ASSIGN
         fakbilag.FRI = TOG_FRI
         fakbilag.KOST = TOG_KOST 
         fakbilag.TIDEJMED = TOG_TIDEJMED
         fakbilag.TIDKLOCK = TOG_TIDKLOCK
         fakbilag.TIDMED = TOG_TIDMED
         fakbilag.TIDTOT = TOG_TIDTOT
         fakbilag.LON = TOG_LON
         fakbilag.PRIS = TOG_PRIS
         fakbilag.KONTO = TOG_KONT
         fakbilag.TOTNAMN = TOG_TIDKLOCKNAM
         fakbilag.KLOCKNAMN = TOG_TIDTOTNAMN.
      END.   
   END.    
   RUN slutbildatum_UI IN fakthmth (INPUT infakplannr,INPUT kreditnrvar,INPUT FALSE,INPUT varbi).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

