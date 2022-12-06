&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
&Scoped-define NEW
{FAKTPLANTEMP.I}
{HOPPSEK2W.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER gamanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER debkred AS LOGICAL NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER varfakturd AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER varforfalld AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER varbokdatum AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER answeradm  AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER answerprel AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER answerskriv AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER answerFB AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR faktureradtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR faktkredtemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE VARIABLE varbi AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skarpfaktok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED ~
TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON ~
TOG_FRI BTN_NVE-5 FILL-IN-BOK FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-5 ~
BTN_NVE-3 BTN_FVE-3 BTN_NVE-4 BTN_FVE-4 TOG_MEDDADM TOG_SKRIV TOG_MEDDPREL ~
TOG_MEDDFB BTN_OK Btn_AVB TOG_KONT 
&Scoped-Define DISPLAYED-OBJECTS TOG_INGA TOG_TIDMED TOG_TIDTOT ~
TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM ~
TOG_LON TOG_FRI FILL-IN_FDATUM FILL-IN_TDATUM FILL-IN-BOK FILL-IN-STARTDAT ~
FILL-IN-SLUTDAT TOG_MEDDADM TOG_SKRIV TOG_MEDDPREL TOG_MEDDFB TOG_KONT 

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

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_FVE-5 
     LABEL "-" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_NVE-5 
     LABEL "+" 
     SIZE 2.57 BY .77.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BOK AS DATE FORMAT "99/99/99":U 
     LABEL "Bokföringsdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Förfallodatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Fakturadatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN_FDATUM AS DATE FORMAT "99/99/99" INITIAL 02/12/04 
     LABEL "Från och med" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92.

DEFINE VARIABLE FILL-IN_TDATUM AS DATE FORMAT "99/99/99" INITIAL 02/12/04 
     LABEL "Till och med" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92.

DEFINE VARIABLE TOG_AFAK AS LOGICAL INITIAL no 
     LABEL "Skriv ut alla fakturor" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_FRI AS LOGICAL INITIAL no 
     LABEL "Visa fria kompletteringsposter" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_INGA AS LOGICAL INITIAL no 
     LABEL "Inga bilagor" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_KONT AS LOGICAL INITIAL no 
     LABEL "Visa kontering" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_KOST AS LOGICAL INITIAL no 
     LABEL "Visa ingående kostnadsregistrering" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.29 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_LON AS LOGICAL INITIAL no 
     LABEL "Visa lönetillägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.29 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_MEDDADM AS LOGICAL INITIAL no 
     LABEL "Skicka meddelande till fakturaadministratören" 
     VIEW-AS TOGGLE-BOX
     SIZE 50.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_MEDDFB AS LOGICAL INITIAL no 
     LABEL "Skicka meddelande Faktura beställaren" 
     VIEW-AS TOGGLE-BOX
     SIZE 75.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_MEDDPREL AS LOGICAL INITIAL no 
     LABEL "Skicka meddelande till den som har skapat preliminär fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 75.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_PRIS AS LOGICAL INITIAL no 
     LABEL "Visa kostnadsfälten" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.86 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_SKRIV AS LOGICAL INITIAL no 
     LABEL "Skriv ut fakturan efter fakturering" 
     VIEW-AS TOGGLE-BOX
     SIZE 75.57 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDEJMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ej ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCK AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med klockslag" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.86 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCKNAM AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med klockslag och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.86 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.43 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOT AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med totaltid" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .65 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOTNAMN AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med totaltid och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .65 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     TOG_AFAK AT ROW 2.96 COL 1.57
     TOG_INGA AT ROW 2.96 COL 47.14
     TOG_TIDMED AT ROW 3.77 COL 1.57
     TOG_TIDTOT AT ROW 3.77 COL 47.14
     TOG_TIDEJMED AT ROW 4.54 COL 1.57
     TOG_TIDKLOCK AT ROW 4.54 COL 47.14
     TOG_PRIS AT ROW 5.35 COL 1.57
     TOG_TIDTOTNAMN AT ROW 5.35 COL 47.14
     TOG_KOST AT ROW 6.12 COL 1.57
     TOG_TIDKLOCKNAM AT ROW 6.12 COL 47.14
     TOG_LON AT ROW 6.85 COL 1.57
     TOG_FRI AT ROW 6.85 COL 47.14
     FILL-IN_FDATUM AT ROW 9.65 COL 18.57 COLON-ALIGNED
     FILL-IN_TDATUM AT ROW 9.65 COL 45.57 COLON-ALIGNED
     BTN_NVE-5 AT ROW 11.42 COL 42.72
     FILL-IN-BOK AT ROW 11.69 COL 30.57 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 14.31 COL 30.57 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 16.85 COL 30.57 COLON-ALIGNED
     BTN_FVE-5 AT ROW 12.42 COL 42.72
     BTN_NVE-3 AT ROW 13.88 COL 42.72
     BTN_FVE-3 AT ROW 14.88 COL 42.72
     BTN_NVE-4 AT ROW 16.38 COL 42.72
     BTN_FVE-4 AT ROW 17.38 COL 42.72
     TOG_MEDDADM AT ROW 20.27 COL 1.57
     TOG_SKRIV AT ROW 21 COL 1.57
     TOG_MEDDPREL AT ROW 21.77 COL 1.57
     TOG_MEDDFB AT ROW 22.5 COL 1.57
     BTN_OK AT ROW 23.5 COL 63
     Btn_AVB AT ROW 23.5 COL 78
     TOG_KONT AT ROW 7.54 COL 47.14
     "VÄLJ BILAGOR TILL KUND :" VIEW-AS TEXT
          SIZE 39.57 BY 1 AT ROW 1.58 COL 7.57
          FONT 17
     "Godkänd bokföringsperiod:" VIEW-AS TEXT
          SIZE 34 BY 1 AT ROW 8 COL 1.57
          FONT 17
     "Meddelande mm.:" VIEW-AS TEXT
          SIZE 24.57 BY 1 AT ROW 19.27 COL 1.57
          FONT 17
     SPACE(66.35) SKIP(4.35)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bilagor till kund mm.".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FDATUM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TDATUM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOG_AFAK IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_AFAK:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bilagor till kund mm. */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON CHOOSE OF Btn_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 Dialog-Frame
ON CHOOSE OF BTN_FVE-3 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 Dialog-Frame
ON CHOOSE OF BTN_FVE-4 IN FRAME Dialog-Frame /* - */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.      
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-5 Dialog-Frame
ON CHOOSE OF BTN_FVE-5 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-BOK = INPUT FILL-IN-BOK.   
   FILL-IN-BOK = FILL-IN-BOK - 1.        
   DISPLAY FILL-IN-BOK WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 Dialog-Frame
ON CHOOSE OF BTN_NVE-3 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.           
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 Dialog-Frame
ON CHOOSE OF BTN_NVE-4 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT + 1.           
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-5 Dialog-Frame
ON CHOOSE OF BTN_NVE-5 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-BOK = INPUT FILL-IN-BOK.   
   FILL-IN-BOK = FILL-IN-BOK + 1.           
   DISPLAY FILL-IN-BOK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:      
   ASSIGN   
   TOG_FRI = INPUT FRAME {&FRAME-NAME} TOG_FRI
   TOG_MEDDADM  = INPUT TOG_MEDDADM 
   TOG_MEDDPREL = INPUT TOG_MEDDPREL
   TOG_MEDDFB = INPUT TOG_MEDDFB
   TOG_SKRIV    = INPUT TOG_SKRIV  
   TOG_KOST = INPUT TOG_KOST 
   TOG_TIDEJMED = INPUT TOG_TIDEJMED
   TOG_TIDKLOCK = INPUT TOG_TIDKLOCK
   TOG_TIDMED = INPUT TOG_TIDMED
   TOG_TIDTOT = INPUT TOG_TIDTOT
   TOG_LON = INPUT TOG_LON
   TOG_PRIS = INPUT TOG_PRIS
   TOG_TIDKLOCKNAM = INPUT TOG_TIDKLOCKNAM
   TOG_KONT = INPUT TOG_KONT
   TOG_TIDTOTNAMN = INPUT TOG_TIDTOTNAMN.
   varbi = "".
   ASSIGN
   answeradm  = TOG_MEDDADM 
   answerprel =  TOG_MEDDPREL
   answerskriv = TOG_SKRIV
   answerFB = TOG_MEDDFB.
   ASSIGN
   varforfalld = FILL-IN-SLUTDAT 
   varfakturd = FILL-IN-STARTDAT
   varbokdatum = FILL-IN-BOK. 
   IF FILL-IN-STARTDAT >= FILL-IN_FDATUM AND FILL-IN-STARTDAT <= FILL-IN_TDATUM THEN musz = musz.
   ELSE DO:
      MESSAGE "Du måste sätta fakturadatum inom godkänd bokföringsperiod" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF TOG_INGA = TRUE THEN DO:
      varbi = ",0,".
   END.
   ELSE DO:
      varbi = ",".
      IF TOG_FRI = TRUE THEN varbi = varbi + "1,".
      IF TOG_KOST = TRUE THEN varbi = varbi + "2,".
      IF TOG_TIDEJMED = TRUE THEN varbi = varbi + "3,".
      IF TOG_TIDKLOCK = TRUE THEN varbi = varbi + "4,".
      IF TOG_TIDMED = TRUE THEN varbi = varbi + "5,".
      IF TOG_TIDTOT = TRUE THEN varbi = varbi + "6,".
      IF TOG_LON = TRUE THEN varbi = varbi + "7,".   
      IF TOG_PRIS = TRUE THEN varbi = varbi + "8,".
      IF TOG_TIDKLOCKNAM = TRUE THEN varbi = varbi + "9,".
      IF TOG_TIDTOTNAMN = TRUE THEN varbi = varbi + "10,".
      IF TOG_KONT = TRUE THEN varbi = varbi + "11,". 
   END.   
   RUN slutbildatum_UI IN fakthmth (INPUT infakplannr,INPUT fdelnrvar,INPUT debkred,INPUT varbi).   
   IF debkred = TRUE THEN DO:
      FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND faktureradtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
      faktureradtemp.BILAGOR = varbi.                
   END.
   ELSE DO:     
      FIND FIRST faktkredtemp WHERE faktkredtemp.FAKTNR = infakplannr AND faktkredtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
      faktkredtemp.BILAGOR = varbi.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BOK Dialog-Frame
ON LEAVE OF FILL-IN-BOK IN FRAME Dialog-Frame /* Bokföringsdatum */
DO:
   FILL-IN-BOK = INPUT FILL-IN-BOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BOK Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-BOK IN FRAME Dialog-Frame /* Bokföringsdatum */
DO:
   ASSIGN
   FILL-IN-BOK = INPUT FILL-IN-BOK
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-BOK.
   RUN AlmanBtn.w.
   FILL-IN-BOK = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-BOK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT Dialog-Frame
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME Dialog-Frame /* Förfallodatum */
DO:
  FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME Dialog-Frame /* Förfallodatum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.  
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Fakturadatum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Fakturadatum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FDATUM Dialog-Frame
ON LEAVE OF FILL-IN_FDATUM IN FRAME Dialog-Frame /* Från och med */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FDATUM Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_FDATUM IN FRAME Dialog-Frame /* Från och med */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TDATUM Dialog-Frame
ON LEAVE OF FILL-IN_TDATUM IN FRAME Dialog-Frame /* Till och med */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TDATUM Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_TDATUM IN FRAME Dialog-Frame /* Till och med */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AFAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AFAK Dialog-Frame
ON VALUE-CHANGED OF TOG_AFAK IN FRAME Dialog-Frame /* Skriv ut alla fakturor */
DO:                 
   TOG_AFAK = INPUT TOG_AFAK.       
   IF TOG_AFAK = TRUE THEN DO:             
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.     
      APPLY "WINDOW-CLOSE":U TO FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FRI Dialog-Frame
ON VALUE-CHANGED OF TOG_FRI IN FRAME Dialog-Frame /* Visa fria kompletteringsposter */
DO:
   IF INPUT TOG_FRI = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INGA Dialog-Frame
ON VALUE-CHANGED OF TOG_INGA IN FRAME Dialog-Frame /* Inga bilagor */
DO:                 
   TOG_INGA = INPUT TOG_INGA.       
   IF TOG_INGA = TRUE THEN DO: 
      ASSIGN 
      TOG_KONT = FALSE
      TOG_FRI = FALSE 
      TOG_KOST = FALSE
      TOG_LON = FALSE
      TOG_PRIS = FALSE
      TOG_TIDEJMED = FALSE
      TOG_TIDKLOCK = FALSE 
      TOG_TIDMED = FALSE
      TOG_TIDTOT = FALSE
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONT Dialog-Frame
ON VALUE-CHANGED OF TOG_KONT IN FRAME Dialog-Frame /* Visa kontering */
DO:
   IF INPUT TOG_KONT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOST Dialog-Frame
ON VALUE-CHANGED OF TOG_KOST IN FRAME Dialog-Frame /* Visa ingående kostnadsregistrering */
DO:
   IF INPUT TOG_KOST = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LON Dialog-Frame
ON VALUE-CHANGED OF TOG_LON IN FRAME Dialog-Frame /* Visa lönetillägg */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MEDDADM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MEDDADM Dialog-Frame
ON VALUE-CHANGED OF TOG_MEDDADM IN FRAME Dialog-Frame /* Skicka meddelande till fakturaadministratören */
DO:
   IF INPUT TOG_KOST = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MEDDFB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MEDDFB Dialog-Frame
ON VALUE-CHANGED OF TOG_MEDDFB IN FRAME Dialog-Frame /* Skicka meddelande Faktura beställaren */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MEDDPREL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MEDDPREL Dialog-Frame
ON VALUE-CHANGED OF TOG_MEDDPREL IN FRAME Dialog-Frame /* Skicka meddelande till den som har skapat preliminär fakturan */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PRIS Dialog-Frame
ON VALUE-CHANGED OF TOG_PRIS IN FRAME Dialog-Frame /* Visa kostnadsfälten */
DO:
   IF INPUT TOG_PRIS = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SKRIV Dialog-Frame
ON VALUE-CHANGED OF TOG_SKRIV IN FRAME Dialog-Frame /* Skriv ut fakturan efter fakturering */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDEJMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDEJMED Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDEJMED IN FRAME Dialog-Frame /* Visa tidskrivning som ej ingår i fakturan */
DO:
   IF INPUT TOG_TIDEJMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCK Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDKLOCK IN FRAME Dialog-Frame /* Visa detaljerad tidskrivning med klockslag */
DO:
   IF INPUT TOG_TIDKLOCK = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCKNAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCKNAM Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDKLOCKNAM IN FRAME Dialog-Frame /* Visa det. tid. med klockslag och namn */
DO:
   IF INPUT TOG_TIDKLOCKNAM = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDMED Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDMED IN FRAME Dialog-Frame /* Visa tidskrivning som ingår i fakturan */
DO:
   IF INPUT TOG_TIDMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOT Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDTOT IN FRAME Dialog-Frame /* Visa detaljerad tidskrivning med totaltid */
DO:
   IF INPUT TOG_TIDTOT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOTNAMN Dialog-Frame
ON VALUE-CHANGED OF TOG_TIDTOTNAMN IN FRAME Dialog-Frame /* Visa det. tid. med totaltid och namn */
DO:
   IF INPUT TOG_TIDTOTNAMN = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
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
   FILL-IN-SLUTDAT = varforfalld
   FILL-IN-STARTDAT = varfakturd.
   FILL-IN-BOK = varbokdatum.
   
   RUN startbildatum_UI IN fakthmth (OUTPUT FILL-IN_FDATUM, OUTPUT FILL-IN_TDATUM).   
   IF debkred = TRUE THEN DO:
      FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND faktureradtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
      varbi = faktureradtemp.BILAGOR.                
   END.
   ELSE DO:     
      FIND FIRST faktkredtemp WHERE faktkredtemp.FAKTNR = infakplannr AND faktkredtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
      varbi = faktkredtemp.BILAGOR.
      
   END.
   IF varbi = "" THEN DO:
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:
         TOG_FRI = TRUE.
      END.
      ELSE DO:
          ASSIGN
         TOG_KOST = TRUE
         TOG_FRI = TRUE
         TOG_PRIS = TRUE
         TOG_LON = TRUE
         TOG_TIDMED = TRUE.
      END.
   END.
   ELSE DO:
      IF varbi = "0," THEN TOG_INGA = TRUE.
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
   RUN enable_UI.       
   IF gamanv = Guru.Konstanter:globanv THEN DO:
      TOG_MEDDPREL:HIDDEN = TRUE.
   END.
    
   IF skarpfaktok = FALSE THEN DO:
      IF Guru.Konstanter:faktsekvar[5] = FALSE AND Guru.Konstanter:faktsekvar[4] = FALSE THEN DO:
         TOG_MEDDFB = TRUE.
         DISPLAY TOG_MEDDFB WITH FRAME {&FRAME-NAME}. 
      END.
      ELSE DO:
         TOG_MEDDFB = FALSE.
         TOG_MEDDFB:HIDDEN = TRUE.
      END.
   END.
   ELSE DO:
      TOG_MEDDFB = FALSE.
      TOG_MEDDFB:HIDDEN = TRUE.
   END.
   
   IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
      TOG_MEDDPREL = TRUE.
      DISPLAY TOG_MEDDPREL WITH FRAME {&FRAME-NAME}. 
   END.
   IF debkred = FALSE THEN DO:
      ASSIGN
      TOG_MEDDPREL = FALSE
   /*   TOG_MEDDFB = FALSE
      TOG_MEDDFB:HIDDEN = TRUE
   */
      TOG_MEDDPREL:HIDDEN = TRUE. 
      
   END.
   IF Guru.Konstanter:faktsekvar[4] = FALSE AND Guru.Konstanter:faktsekvar[5] = FALSE THEN TOG_MEDDPREL:HIDDEN = TRUE.
   IF Guru.Konstanter:faktsekvar[5] = TRUE THEN TOG_MEDDFB:HIDDEN = TRUE.
   {FRMSIZED.I} 
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS 
          TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI FILL-IN_FDATUM 
          FILL-IN_TDATUM FILL-IN-BOK FILL-IN-STARTDAT FILL-IN-SLUTDAT 
          TOG_MEDDADM TOG_SKRIV TOG_MEDDPREL TOG_MEDDFB TOG_KONT 
      WITH FRAME Dialog-Frame.
  ENABLE TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS 
         TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI BTN_NVE-5 
         FILL-IN-BOK FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-5 BTN_NVE-3 
         BTN_FVE-3 BTN_NVE-4 BTN_FVE-4 TOG_MEDDADM TOG_SKRIV TOG_MEDDPREL 
         TOG_MEDDFB BTN_OK Btn_AVB TOG_KONT 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

