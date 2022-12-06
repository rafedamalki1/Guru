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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{SOKDEF.I}
{HOPALLA.I}
DEFINE VARIABLE guruversionvar AS INTEGER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE Urlsite AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE x-multi AS DECIMAL NO-UNDO.
DEFINE VARIABLE y-multi AS DECIMAL NO-UNDO.
{MANUALERTEMP.I}
DEFINE STREAM dirstrom.   
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MAXFONSTER RAD_AONR RAD_BER RAD_KALK ~
RAD_MARK RAD_PERS RAD_TID RAD_FAKT RAD_PLAN RAD_STOR RAD_ALLTIDMAX RAD_UTF ~
RAD_PRISSORT FILL-IN-DELNR FILL-IN-DELNRMAX CMB_MAN BTN_MAN FILL-IN-NUMFORM ~
FILL-IN-AUTOSPAR FILL-IN-PNRSELGA BTN_OCX BTN_REGED BTN_OCX-2 ~
FILL-IN_HOJDDEF FILL-IN_BREDDDEF BTN_TESTDEFAULT FILL-IN_HOJD FILL-IN_BREDD ~
BTN_TEST BTN_OK FILL-IN_AONR BTN_AVB FILL-IN_BER FILL-IN_KALK FILL-IN_MARK ~
FILL-IN_PERS FILL-IN_TID FILL-IN_FAKT FILL-IN_PLAN FILL-IN_STOR ~
FILL-IN_ALLTIDMAX FILL-IN_UTF FILL-IN_PRISSORT FILL-IN-EXCEL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-MAXFONSTER RAD_AONR RAD_BER ~
RAD_KALK RAD_MARK RAD_PERS RAD_TID RAD_FAKT RAD_PLAN RAD_STOR RAD_ALLTIDMAX ~
RAD_UTF RAD_PRISSORT FILL-IN-DELNR FILL-IN-DELNRMAX CMB_MAN FILL-IN-NUMFORM ~
FILL-IN-AUTOSPAR FILL-IN-PNRSELGA FILL-IN_HOJDDEF FILL-IN_BREDDDEF ~
FILL-IN_HOJD FILL-IN_BREDD FILL-IN_AONR FILL-IN_BER FILL-IN_KALK ~
FILL-IN_MARK FILL-IN_PERS FILL-IN_TID FILL-IN_FAKT FILL-IN_PLAN ~
FILL-IN_STOR FILL-IN_ALLTIDMAX FILL-IN_UTF FILL-IN_PRISSORT FILL-IN_WH ~
FILL-IN_WB FILL-IN-EXCEL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AreControlsRegistered Dialog-Frame 
FUNCTION AreControlsRegistered RETURNS LOGICAL (INPUT ChrObjectList AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MAN 
     LABEL "Läs" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OCX 
     LABEL "Registrering av ActiveX" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OCX-2 
     LABEL "Kontroll av ActiveX" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_REGED 
     LABEL "Möjligör uppdatering" 
     SIZE 14 BY 1 TOOLTIP "Detta gör det möjligt att ta emot nya Guruversioner".

DEFINE BUTTON BTN_TEST 
     LABEL "Test av Maxfönster" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TESTDEFAULT 
     LABEL "Test av Normalfönster" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_MAN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Manualer och Nyheter" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AUTOSPAR AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Tid i minuter mellan autospar (0 = inget autospar) =" 
     VIEW-AS FILL-IN 
     SIZE 3.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Delnr Serie från" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNRMAX AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EXCEL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Jag har äldre version av Excel" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAXFONSTER AS CHARACTER FORMAT "X(256)":U INITIAL "Ange max storlek på fönstren i Guru." 
      VIEW-AS TEXT 
     SIZE 44.5 BY 1.08
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-NUMFORM AS LOGICAL FORMAT "EUROPEAN/AMERICAN":U INITIAL NO 
     LABEL "Numerisktformat komma eller punkt EUROPEAN= ','" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PNRSELGA AS CHARACTER FORMAT "999 99":U INITIAL "0" 
     LABEL "Post nr för Rexel/Selga" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ALLTIDMAX AS CHARACTER FORMAT "X(256)":U INITIAL "Kör alltid maximerat" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BER AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BREDD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Maxbredd i pixels" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BREDDDEF AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Normalbredd i pixels" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FAKT AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_HOJD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Maxhöjd i pixels" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_HOJDDEF AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Normalhöjd i pixels" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KALK AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_MARK AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERS AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PLAN AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PRISSORT AS CHARACTER FORMAT "X(256)":U INITIAL "Sortera alltid på Nettopris i beredning" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_STOR AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_TID AS CHARACTER FORMAT "X(256)":U INITIAL "Vid avslut av Guru spara automatiskt" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_UTF AS CHARACTER FORMAT "X(256)":U INITIAL "Utfärdare förvald" 
      VIEW-AS TEXT 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_WB AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Bredd" 
      VIEW-AS TEXT 
     SIZE 9.5 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN_WH AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Rekomenderade värden är höjd" 
      VIEW-AS TEXT 
     SIZE 9.5 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE RAD_ALLTIDMAX AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_AONR AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_BER AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAKT AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_KALK AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_MARK AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PERS AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PLAN AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PRISSORT AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_STOR AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_TID AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_UTF AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ja", yes,
"Nej", no
     SIZE 12 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-MAXFONSTER AT ROW 26.5 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     RAD_AONR AT ROW 2.21 COL 54 NO-LABEL
     RAD_BER AT ROW 3.21 COL 54 NO-LABEL
     RAD_KALK AT ROW 4.21 COL 54 NO-LABEL
     RAD_MARK AT ROW 5.21 COL 54 NO-LABEL
     RAD_PERS AT ROW 6.21 COL 54 NO-LABEL
     RAD_TID AT ROW 7.21 COL 54 NO-LABEL
     RAD_FAKT AT ROW 8.21 COL 54 NO-LABEL
     RAD_PLAN AT ROW 9.21 COL 54 NO-LABEL
     RAD_STOR AT ROW 10.21 COL 54 NO-LABEL
     RAD_ALLTIDMAX AT ROW 11.21 COL 54 NO-LABEL
     RAD_UTF AT ROW 12.21 COL 54 NO-LABEL
     RAD_PRISSORT AT ROW 13.25 COL 54 NO-LABEL WIDGET-ID 6
     FILL-IN-DELNR AT ROW 14 COL 17 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-DELNRMAX AT ROW 14 COL 26 COLON-ALIGNED WIDGET-ID 24
     CMB_MAN AT ROW 14.75 COL 21 COLON-ALIGNED WIDGET-ID 16
     BTN_MAN AT ROW 15 COL 66.63 WIDGET-ID 18
     FILL-IN-NUMFORM AT ROW 16.17 COL 60 COLON-ALIGNED
     FILL-IN-AUTOSPAR AT ROW 17.5 COL 60 COLON-ALIGNED
     FILL-IN-PNRSELGA AT ROW 18.5 COL 44.5 COLON-ALIGNED WIDGET-ID 20
     BTN_OCX AT ROW 19.17 COL 66.5
     BTN_REGED AT ROW 20.88 COL 66.5
     BTN_OCX-2 AT ROW 22.42 COL 66.5 WIDGET-ID 2
     FILL-IN_HOJDDEF AT ROW 23.38 COL 30.13 COLON-ALIGNED WIDGET-ID 14
     FILL-IN_BREDDDEF AT ROW 23.38 COL 53.38 COLON-ALIGNED WIDGET-ID 12
     BTN_TESTDEFAULT AT ROW 23.79 COL 66.5 WIDGET-ID 10
     FILL-IN_HOJD AT ROW 24.83 COL 30.13 COLON-ALIGNED
     FILL-IN_BREDD AT ROW 24.83 COL 53.38 COLON-ALIGNED
     BTN_TEST AT ROW 25.08 COL 66.5
     BTN_OK AT ROW 28.13 COL 52.25
     FILL-IN_AONR AT ROW 2.21 COL 6.5 COLON-ALIGNED NO-LABEL
     BTN_AVB AT ROW 28.13 COL 66.63
     FILL-IN_BER AT ROW 3.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_KALK AT ROW 4.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_MARK AT ROW 5.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_PERS AT ROW 6.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_TID AT ROW 7.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_FAKT AT ROW 8.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_PLAN AT ROW 9.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_STOR AT ROW 10.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_ALLTIDMAX AT ROW 11.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_UTF AT ROW 12.21 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN_PRISSORT AT ROW 13.25 COL 6.5 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FILL-IN_WH AT ROW 21.75 COL 30.13 COLON-ALIGNED
     FILL-IN_WB AT ROW 21.75 COL 53.38 COLON-ALIGNED
     FILL-IN-EXCEL AT ROW 17.25 COL 60.5 COLON-ALIGNED WIDGET-ID 28
     "Ange din profil:" VIEW-AS TEXT
          SIZE 60 BY 1.21 AT ROW 1 COL 6
          FONT 17
     SPACE(15.37) SKIP(27.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fönsterstorlek mm".


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
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-MAXFONSTER:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_ALLTIDMAX:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_AONR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_BER:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_FAKT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_KALK:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_MARK:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_PERS:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_PLAN:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_PRISSORT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_STOR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_TID:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN_UTF:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_WB IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_WB:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_WH IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_WH:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fönsterstorlek mm */
DO:
   /*
   RUN fonsterkoll_UI.
   */
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
  edataapph = ?. 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:     
   /*
    RUN fonsterkoll_UI.
   */
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.     
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON ENDKEY OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:  
   /*
    RUN fonsterkoll_UI.
   */
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MAN Dialog-Frame
ON CHOOSE OF BTN_MAN IN FRAME Dialog-Frame /* Läs */
DO:  
    FIND FIRST imptemp WHERE imptemp.INVAR = CMB_MAN NO-LOCK NO-ERROR.
    IF AVAILABLE imptemp THEN DO:
       RUN OPENDOC.P (imptemp.INDIR + imptemp.INVAR,"","",NO).        
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OCX Dialog-Frame
ON CHOOSE OF BTN_OCX IN FRAME Dialog-Frame /* Registrering av ActiveX */
DO:  
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO. 

   kommando = "regsvr32 /su pstimer.ocx".
   OS-COMMAND SILENT VALUE(kommando).
   
   kommando = "regsvr32 /su prox.dll".
   OS-COMMAND SILENT VALUE(kommando).
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      
      kommando = SEARCH(".\CSTART\pstimer.ocx").
   END.
   ELSE kommando = SEARCH("system\pstimer.ocx").
   IF kommando = ? THEN kommando = SEARCH("pstimer.ocx").
   
   IF kommando = ? THEN DO:
      MESSAGE "Kontakta Elpool i Umeå AB 090/184540 ange pstimer" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   kommando = "regsvr32 " + '"' + kommando + '"'.   
   OS-COMMAND SILENT VALUE(kommando).

   kommando = SEARCH("prox.dll").
   kommando = "regsvr32 " + '"' + kommando + '"'.
   IF kommando = ? THEN DO:
      MESSAGE "Kontakta Elpool i Umeå AB 090/184540 ange prox" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   OS-COMMAND SILENT VALUE(kommando).
   
   MESSAGE "Klart!" VIEW-AS ALERT-BOX.
/*
     MESSAGE AreControlsRegistered("prox.dll")  AreControlsRegistered("pstimer.ocx")
         VIEW-AS ALERT-BOX.
    RETURN.
  */ 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OCX-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OCX-2 Dialog-Frame
ON CHOOSE OF BTN_OCX-2 IN FRAME Dialog-Frame /* Kontroll av ActiveX */
DO:  
   IF AreControlsRegistered("pstimer.ocx") THEN MESSAGE "Pstimer OK!"   VIEW-AS ALERT-BOX.
   ELSE MESSAGE "Pstimer är inte Ok" skip
   "Kontakta Elpool i Umeå AB 090/184540 ange pstimer"
   VIEW-AS ALERT-BOX.
   IF AreControlsRegistered("prox.dll") THEN MESSAGE "Prox OK!"   VIEW-AS ALERT-BOX.
   ELSE MESSAGE "Prox är inte Ok" skip
   "Kontakta Elpool i Umeå AB 090/184540 ange pstimer"
   VIEW-AS ALERT-BOX.
   
   
   /*
     MESSAGE AreControlsRegistered("prox.dll")  AreControlsRegistered("pstimer.ocx")
         VIEW-AS ALERT-BOX.
    RETURN.
      IF AreControlsRegistered("prox.dll") = TRUE THEN DO:
         MESSAGE AreControlsRegistered("prox.dll")
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ELSE DO:
         MESSAGE AreControlsRegistered("prox.dll")
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
   */   
      /*
      kommando = "regsvr32 /su pstimer.ocx".
      OS-COMMAND SILENT VALUE(kommando).
      kommando = "regsvr32 /su prox.dll".
      OS-COMMAND SILENT VALUE(kommando).
      
      kommando = SEARCH("system\pstimer.ocx").
      IF kommando = ? THEN kommando = SEARCH("pstimer.ocx").
      IF kommando = ? THEN DO:
         MESSAGE "Kontakta Elpool i Umeå AB 090/184540 ange pstimer" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      kommando = "regsvr32 /s " + '"' + kommando + '"'.   
      OS-COMMAND SILENT VALUE(kommando).
   
      kommando = SEARCH("prox.dll").
      kommando = "regsvr32 /s " + '"' + kommando + '"'.
      IF kommando = ? THEN DO:
         MESSAGE "Kontakta Elpool i Umeå AB 090/184540 ange prox" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      OS-COMMAND SILENT VALUE(kommando).   
      */ 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO: 
   
   ASSIGN
   FILL-IN_BREDD = INPUT FILL-IN_BREDD
   FILL-IN_HOJD = INPUT FILL-IN_HOJD
   FILL-IN_BREDDDEF = INPUT FILL-IN_BREDDDEF
   FILL-IN_HOJDDEF = INPUT FILL-IN_HOJDDEF
   RAD_AONR = INPUT RAD_AONR 
   RAD_BER  = INPUT RAD_BER  
   RAD_KALK = INPUT RAD_KALK 
   RAD_MARK = INPUT RAD_MARK 
   RAD_PERS = INPUT RAD_PERS 
   RAD_TID  = INPUT RAD_TID  
   RAD_FAKT = INPUT RAD_FAKT 
   RAD_PLAN = INPUT RAD_PLAN 
   RAD_STOR = INPUT RAD_STOR 
   RAD_ALLTIDMAX = INPUT RAD_ALLTIDMAX
   RAD_UTF = INPUT RAD_UTF
   RAD_PRISSORT = INPUT RAD_PRISSORT
   FILL-IN-NUMFORM = INPUT FILL-IN-NUMFORM
   FILL-IN-EXCEL = INPUT FILL-IN-EXCEL
   FILL-IN-AUTOSPAR = INPUT FILL-IN-AUTOSPAR
   FILL-IN-PNRSELGA = INPUT FILL-IN-PNRSELGA
   FILL-IN-DELNR = INPUT FILL-IN-DELNR
   FILL-IN-DELNRMAX = INPUT FILL-IN-DELNRMAX.
   
   IF FILL-IN_BREDD < FILL-IN_BREDDDEF THEN FILL-IN_BREDD = FILL-IN_BREDDDEF.
   IF FILL-IN_HOJD < FILL-IN_HOJDDEF THEN FILL-IN_HOJD = FILL-IN_HOJDDEF.
   
   RUN fonsterkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   IF FILL-IN_HOJD < FILL-IN_HOJDDEF THEN FILL-IN_HOJD = FILL-IN_HOJDDEF.
   IF FILL-IN_BREDD < FILL-IN_BREDDDEF THEN FILL-IN_BREDD = FILL-IN_BREDDDEF.
   
   ASSIGN 
   Guru.Konstanter:globDefaultstorb = FILL-IN_BREDDDEF
   Guru.Konstanter:globDefaultstorh = FILL-IN_HOJDDEF .
   ASSIGN 
   Guru.Konstanter:globstorb = FILL-IN_BREDD
   Guru.Konstanter:globstorh = FILL-IN_HOJD. 
   IF Guru.Konstanter:appcon THEN DO:  
       RUN ANVAPPT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).                         
   END.
   ELSE DO:
      RUN ANVAPPT.P (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globstorh, INPUT Guru.Konstanter:globstorb).
   END. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?
   inextradatatemp.SOKLOG[1] =  RAD_AONR 
   inextradatatemp.SOKLOG[2] =  RAD_BER  
   inextradatatemp.SOKLOG[3] =  RAD_KALK 
   inextradatatemp.SOKLOG[4] =  RAD_MARK 
   inextradatatemp.SOKLOG[5] =  RAD_PERS 
   inextradatatemp.SOKLOG[6] =  RAD_TID  
   inextradatatemp.SOKLOG[7] =  RAD_FAKT 
   inextradatatemp.SOKLOG[8] =  RAD_PLAN 
   inextradatatemp.SOKLOG[9] =  RAD_STOR 
   inextradatatemp.SOKLOG[10] =  RAD_ALLTIDMAX   
   inextradatatemp.SOKCHAR[1] =  STRING(FILL-IN-NUMFORM,"EUROPEAN/AMERICAN")
   inextradatatemp.SOKCHAR[2] =  TRIM(STRING(FILL-IN-EXCEL,"Ja/Nej"))
   inextradatatemp.SOKINT[1] =  FILL-IN-AUTOSPAR
   inextradatatemp.SOKINT[3] =   0 /* SPRÅK*/
   inextradatatemp.SOKINT[5] = Guru.Konstanter:globDefaultstorb
   inextradatatemp.SOKINT[6] =  Guru.Konstanter:globDefaultstorh
   inextradatatemp.SOKINT[7] = INTEGER(FILL-IN-PNRSELGA)
   inextradatatemp.SOKINT[8] = FILL-IN-DELNR
   inextradatatemp.SOKINT[9] = FILL-IN-DELNRMAX.
   IF FILL-IN-EXCEL = FALSE THEN guruversionvar = 0.
   inextradatatemp.SOKINT[10] = guruversionvar.
   
   IF RAD_UTF = TRUE THEN inextradatatemp.SOKINT[2] = 1.
   ELSE inextradatatemp.SOKINT[2] = 0.
   
   IF RAD_PRISSORT = TRUE THEN inextradatatemp.SOKINT[4] = 1.
   ELSE inextradatatemp.SOKINT[4] = 0.
   
   
   IF FILL-IN-NUMFORM:HIDDEN = FALSE THEN 
   SESSION:NUMERIC-FORMAT = inextradatatemp.SOKCHAR[1].
  
   RUN Modules\Global\NUMBERSEPA.P.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).            
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REGED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REGED Dialog-Frame
ON CHOOSE OF BTN_REGED IN FRAME Dialog-Frame /* Möjligör uppdatering */
DO:  
  
  RUN WEBVERSION.P (INPUT 2,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT vc2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TEST Dialog-Frame
ON CHOOSE OF BTN_TEST IN FRAME Dialog-Frame /* Test av Maxfönster */
DO:  
   ASSIGN
   FILL-IN_BREDD = INPUT FILL-IN_BREDD
   FILL-IN_HOJD = INPUT FILL-IN_HOJD.
   IF FILL-IN_BREDD < 1000 OR FILL-IN_HOJD < 682 THEN DO:  
      RETURN NO-APPLY.
   END.
   
   RUN FRM-SIZE.P (INPUT FRAME {&FRAME-NAME}:HANDLE,INPUT FILL-IN_HOJD,INPUT FILL-IN_BREDD, OUTPUT x-multi, OUTPUT y-multi).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TESTDEFAULT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TESTDEFAULT Dialog-Frame
ON CHOOSE OF BTN_TESTDEFAULT IN FRAME Dialog-Frame /* Test av Normalfönster */
DO:  
   ASSIGN
   FILL-IN_BREDDDEF = INPUT FILL-IN_BREDDDEF
   FILL-IN_HOJDDEF = INPUT FILL-IN_HOJDDEF.
   IF FILL-IN_BREDDDEF < 1000 OR FILL-IN_HOJDDEF < 682 THEN DO:  
      RETURN NO-APPLY.
   END.
   
   RUN FRM-SIZE.P (INPUT FRAME {&FRAME-NAME}:HANDLE,INPUT FILL-IN_HOJDDEF,INPUT FILL-IN_BREDDDEF, OUTPUT x-multi, OUTPUT y-multi).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MAN Dialog-Frame
ON VALUE-CHANGED OF CMB_MAN IN FRAME Dialog-Frame /* Manualer och Nyheter */
DO:
   CMB_MAN = INPUT CMB_MAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AUTOSPAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AUTOSPAR Dialog-Frame
ON MOUSE-SELECT-CLICK OF FILL-IN-AUTOSPAR IN FRAME Dialog-Frame /* Tid i minuter mellan autospar (0 = inget autospar) = */
DO:
  /*IF FILL-IN-NUMFORM = TRUE THEN FILL-IN-NUMFORM = FALSE.
  ELSE IF FILL-IN-NUMFORM = FALSE THEN FILL-IN-NUMFORM = TRUE.
  DISPLAY FILL-IN-NUMFORM WITH FRAME {&FRAME-NAME}.        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-EXCEL Dialog-Frame
ON MOUSE-SELECT-CLICK OF FILL-IN-EXCEL IN FRAME Dialog-Frame /* Jag har äldre version av Excel */
DO:
  IF FILL-IN-EXCEL = TRUE THEN FILL-IN-EXCEL = FALSE.
  ELSE IF FILL-IN-EXCEL = FALSE THEN FILL-IN-EXCEL = TRUE.
  DISPLAY FILL-IN-EXCEL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NUMFORM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NUMFORM Dialog-Frame
ON MOUSE-SELECT-CLICK OF FILL-IN-NUMFORM IN FRAME Dialog-Frame /* Numerisktformat komma eller punkt EUROPEAN= ',' */
DO:
  IF FILL-IN-NUMFORM = TRUE THEN FILL-IN-NUMFORM = FALSE.
  ELSE IF FILL-IN-NUMFORM = FALSE THEN FILL-IN-NUMFORM = TRUE.
  DISPLAY FILL-IN-NUMFORM WITH FRAME {&FRAME-NAME}. 
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
   IF Guru.Konstanter:appcon THEN DO:  
       RUN ANVAPPTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv,OUTPUT FILL-IN_HOJD, OUTPUT FILL-IN_BREDD).                         
   END.
   ELSE DO:
      RUN ANVAPPTO.P (INPUT Guru.Konstanter:globanv,OUTPUT FILL-IN_HOJD, OUTPUT FILL-IN_BREDD).  
   END.   
       
   ASSIGN
   FILL-IN_WB = SESSION:WORK-AREA-WIDTH-PIXELS  - 60
   FILL-IN_WH = SESSION:WORK-AREA-HEIGHT-PIXELS - 80
   /*
   FILL-IN_HOJD = Guru.Konstanter:globstorh
   FILL-IN_BREDD = Guru.Konstanter:globstorb
   FILL-IN_HOJDDEF = Guru.Konstanter:globDefaultstorh
   FILL-IN_BREDDDEF = Guru.Konstanter:globDefaultstorb
   */
   
   FILL-IN_AONR = "Vid avslut av Guru spara valda " + LC(Guru.Konstanter:gaok) + " i favoriter automatiskt"
   FILL-IN_BER = "Vid avslut av Guru spara valda beredningar i favoriter automatiskt"
   FILL-IN_KALK = "Vid avslut av Guru spara valda kalkyler i favoriter automatiskt"
   FILL-IN_MARK = "Vid avslut av Guru spara valda markvärdingar i favoriter automatiskt"
   FILL-IN_PERS = "Vid avslut av Guru spara valda personer i favoriter automatiskt"
   FILL-IN_TID = "Vid avslut av Guru spara valda tidskrivande personal i favoriter automatiskt"
   FILL-IN_FAKT = "Vid avslut av Guru spara valda fakturaplaner i favoriter automatiskt"
   FILL-IN_PLAN = "Vid avslut av Guru spara valda " + LC(Guru.Konstanter:gplk) + " i favoriter automatiskt"
   FILL-IN_STOR = "Vid avslut av Guru spara valda störningar i favoriter automatiskt"
   FILL-IN_ALLTIDMAX = "Kör alltid maximerat"
   FILL-IN_UTF = "Utfärdare förvald" 
   FILL-IN_PRISSORT = "Sortera alltid på Nettopris i beredning". 
   /*
   RUN manin_UI.
   
   CMB_MAN:LIST-ITEMS = "".
   CMB_MAN:ADD-FIRST("GuruNyheter.pdf").
   FOR EACH imptemp WHERE NO-LOCK:
      IF Imptemp.INVAR = "GuruNyheter.pdf" THEN.
      ELSE CMB_MAN:ADD-LAST(Imptemp.INVAR).
   END.
   CMB_MAN = "GuruNyheter.pdf".
   */
     
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   FILL-IN-DELNRMAX:LABEL = "  till ".
   FILL-IN-DELNRMAX:FORMAT = FILL-IN-DELNR:FORMAT. 
   FILL-IN-DELNR:LABEL = "Nästa " + FILL-IN-DELNR:LABEL. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.                  
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      ASSIGN
      FILL-IN-DELNR = extradatatemp.SOKINT[8]  
      FILL-IN-DELNRMAX = extradatatemp.SOKINT[9] 
      FILL-IN-PNRSELGA = STRING(extradatatemp.SOKINT[7])
      FILL-IN_HOJDDEF = extradatatemp.SOKINT[6]
      FILL-IN_BREDDDEF = extradatatemp.SOKINT[5]
      guruversionvar =  extradatatemp.SOKINT[10]  
      RAD_AONR = extradatatemp.SOKLOG[1]
      RAD_BER = extradatatemp.SOKLOG[2]
      RAD_KALK = extradatatemp.SOKLOG[3]
      RAD_MARK = extradatatemp.SOKLOG[4]
      RAD_PERS = extradatatemp.SOKLOG[5]
      RAD_TID = extradatatemp.SOKLOG[6]
      RAD_FAKT = extradatatemp.SOKLOG[7]
      RAD_PLAN = extradatatemp.SOKLOG[8]
      RAD_STOR = extradatatemp.SOKLOG[9]
      FILL-IN-AUTOSPAR = extradatatemp.SOKINT[1]
      RAD_ALLTIDMAX = extradatatemp.SOKLOG[10].
      IF extradatatemp.SOKINT[2] = 1 THEN RAD_UTF = TRUE.
      ELSE RAD_UTF = FALSE. 
      IF extradatatemp.SOKINT[4] = 1 THEN RAD_PRISSORT = TRUE.
      ELSE RAD_PRISSORT = FALSE.
      IF FILL-IN-PNRSELGA = "0" THEN FILL-IN-PNRSELGA =  "00000".
            
      IF extradatatemp.SOKCHAR[1] = "" THEN extradatatemp.SOKCHAR[1] = "AMERICAN".
      IF extradatatemp.SOKCHAR[2] = "" THEN extradatatemp.SOKCHAR[2] = "Nej".
      
      FILL-IN-NUMFORM = LOGICAL(extradatatemp.SOKCHAR[1],"EUROPEAN/AMERICAN").
      FILL-IN-EXCEL = LOGICAL(extradatatemp.SOKCHAR[2],"Ja/Nej").      
   END.
   IF FILL-IN_HOJDDEF < 682 THEN FILL-IN_HOJDDEF = 682.
   IF FILL-IN_BREDDDEF < 1000 THEN FILL-IN_BREDDDEF = 1000.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   RUN enable_UI.
   {FRMSIZED.I} 
   IF Guru.Konstanter:globforetag = "AKEA" OR Guru.Konstanter:globforetag = "elpa" THEN.
   ELSE DO:
      FILL-IN-DELNRMAX:HIDDEN = TRUE. 
      FILL-IN-DELNR:HIDDEN = TRUE.
   END.        
   Guru.GlobalaVariabler:collefth = ?.
   
   IF Guru.Konstanter:hoppsekvar[1] = TRUE THEN DO:       
      ASSIGN
      FILL-IN_AONR:HIDDEN = FALSE
      RAD_AONR:HIDDEN = FALSE.
      Guru.GlobalaVariabler:colrighth = FILL-IN_AONR:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_AONR:ROW = FILL-IN_AONR:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_AONR:HIDDEN = TRUE
      RAD_AONR:HIDDEN = TRUE.      
   END.
   IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
      ASSIGN
      FILL-IN_BER:HIDDEN = FALSE
      RAD_BER:HIDDEN = FALSE.  
      Guru.GlobalaVariabler:colrighth = FILL-IN_BER:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_BER:ROW = FILL-IN_BER:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_BER:HIDDEN = TRUE
      RAD_BER:HIDDEN = TRUE.      
   END.
   
   IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:  
      ASSIGN
      FILL-IN_KALK:HIDDEN = FALSE
      RAD_KALK:HIDDEN = FALSE. 
      Guru.GlobalaVariabler:colrighth = FILL-IN_KALK:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_KALK:ROW = FILL-IN_KALK:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_KALK:HIDDEN = TRUE
      RAD_KALK:HIDDEN = TRUE.      
   END.
   IF Guru.Konstanter:hoppsekvar[12] = TRUE THEN DO:
      ASSIGN
      FILL-IN_MARK:HIDDEN = FALSE
      RAD_MARK:HIDDEN = FALSE. 
      Guru.GlobalaVariabler:colrighth = FILL-IN_MARK:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_MARK:ROW = FILL-IN_MARK:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_MARK:HIDDEN = TRUE
      RAD_MARK:HIDDEN = TRUE.      
   END.   
   IF Guru.Konstanter:hoppsekvar[7] = TRUE THEN DO: 
      ASSIGN
      FILL-IN_PERS:HIDDEN = FALSE
      RAD_PERS:HIDDEN = FALSE. 
      Guru.GlobalaVariabler:colrighth = FILL-IN_PERS:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_PERS:ROW = FILL-IN_PERS:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_PERS:HIDDEN = TRUE
      RAD_PERS:HIDDEN = TRUE.      
   END.
     
   IF Guru.Konstanter:hoppsekvar[4] = TRUE THEN DO:  
      ASSIGN
      FILL-IN_TID:HIDDEN = FALSE
      RAD_TID:HIDDEN = FALSE. 
      Guru.GlobalaVariabler:colrighth = FILL-IN_TID:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_TID:ROW = FILL-IN_TID:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_TID:HIDDEN = TRUE
      RAD_TID:HIDDEN = TRUE.      
   END.
    
   IF Guru.Konstanter:hoppsekvar[10] = TRUE THEN DO: 
      ASSIGN
      FILL-IN_FAKT:HIDDEN = FALSE
      RAD_FAKT:HIDDEN = FALSE.
      Guru.GlobalaVariabler:colrighth = FILL-IN_FAKT:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_FAKT:ROW = FILL-IN_FAKT:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_FAKT:HIDDEN = TRUE
      RAD_FAKT:HIDDEN = TRUE.      
   END.
   IF Guru.Konstanter:hoppsekvar[11] = TRUE THEN DO:  
      ASSIGN
      FILL-IN_PLAN:HIDDEN = FALSE
      RAD_PLAN:HIDDEN = FALSE.
      Guru.GlobalaVariabler:colrighth = FILL-IN_PLAN:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_PLAN:ROW = FILL-IN_PLAN:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_PLAN:HIDDEN = TRUE
      RAD_PLAN:HIDDEN = TRUE.      
   END.
   IF Guru.Konstanter:hoppsekvar[14] = TRUE THEN DO:
      ASSIGN
      FILL-IN_STOR:HIDDEN = FALSE
      RAD_STOR:HIDDEN = FALSE. 
      Guru.GlobalaVariabler:colrighth = FILL-IN_STOR:HANDLE.
      RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
      RAD_STOR:ROW = FILL-IN_STOR:ROW.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_STOR:HIDDEN = TRUE
      RAD_STOR:HIDDEN = TRUE.      
   END.
   ASSIGN
   FILL-IN_ALLTIDMAX:HIDDEN = FALSE
   RAD_ALLTIDMAX:HIDDEN = FALSE.
   Guru.GlobalaVariabler:colrighth = FILL-IN_ALLTIDMAX:HANDLE.
   RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
   RAD_ALLTIDMAX:ROW = FILL-IN_ALLTIDMAX:ROW.
   ASSIGN
   FILL-IN_UTF:HIDDEN = FALSE
   RAD_UTF:HIDDEN = FALSE.
   Guru.GlobalaVariabler:colrighth = FILL-IN_UTF:HANDLE.
   RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
   RAD_UTF:ROW = FILL-IN_UTF:ROW.
  
   ASSIGN
   FILL-IN_PRISSORT:HIDDEN = FALSE
   RAD_PRISSORT:HIDDEN = FALSE.
   Guru.GlobalaVariabler:colrighth = FILL-IN_PRISSORT:HANDLE.
   RUN rowstart_UI (INPUT Guru.GlobalaVariabler:colrighth).
   RAD_PRISSORT:ROW = FILL-IN_PRISSORT:ROW.
   FILL-IN-NUMFORM:HIDDEN = FALSE.
   
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      BTN_OCX:HIDDEN = TRUE.
   END.
   ELSE DO:
       BTN_REGED:HIDDEN = TRUE.
   END.
   
   BTN_OCX:HIDDEN = TRUE.
   BTN_OCX-2:HIDDEN = TRUE.
   CMB_MAN:HIDDEN = TRUE.
   BTN_MAN:HIDDEN = TRUE.
   
   RUN PlaceraKnapp_UI.
   {musarrow.i} 
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
  DISPLAY FILL-IN-MAXFONSTER RAD_AONR RAD_BER RAD_KALK RAD_MARK RAD_PERS RAD_TID 
          RAD_FAKT RAD_PLAN RAD_STOR RAD_ALLTIDMAX RAD_UTF RAD_PRISSORT 
          FILL-IN-DELNR FILL-IN-DELNRMAX CMB_MAN FILL-IN-NUMFORM 
          FILL-IN-AUTOSPAR FILL-IN-PNRSELGA FILL-IN_HOJDDEF FILL-IN_BREDDDEF 
          FILL-IN_HOJD FILL-IN_BREDD FILL-IN_AONR FILL-IN_BER FILL-IN_KALK 
          FILL-IN_MARK FILL-IN_PERS FILL-IN_TID FILL-IN_FAKT FILL-IN_PLAN 
          FILL-IN_STOR FILL-IN_ALLTIDMAX FILL-IN_UTF FILL-IN_PRISSORT FILL-IN_WH 
          FILL-IN_WB FILL-IN-EXCEL 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-MAXFONSTER RAD_AONR RAD_BER RAD_KALK RAD_MARK RAD_PERS RAD_TID 
         RAD_FAKT RAD_PLAN RAD_STOR RAD_ALLTIDMAX RAD_UTF RAD_PRISSORT 
         FILL-IN-DELNR FILL-IN-DELNRMAX CMB_MAN BTN_MAN FILL-IN-NUMFORM 
         FILL-IN-AUTOSPAR FILL-IN-PNRSELGA BTN_OCX BTN_REGED BTN_OCX-2 
         FILL-IN_HOJDDEF FILL-IN_BREDDDEF BTN_TESTDEFAULT FILL-IN_HOJD 
         FILL-IN_BREDD BTN_TEST BTN_OK FILL-IN_AONR BTN_AVB FILL-IN_BER 
         FILL-IN_KALK FILL-IN_MARK FILL-IN_PERS FILL-IN_TID FILL-IN_FAKT 
         FILL-IN_PLAN FILL-IN_STOR FILL-IN_ALLTIDMAX FILL-IN_UTF 
         FILL-IN_PRISSORT FILL-IN-EXCEL 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fonsterkoll_UI Dialog-Frame 
PROCEDURE fonsterkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
   IF FILL-IN_BREDD < 1024 OR FILL-IN_HOJD < 740 THEN DO:  
      IF FILL-IN_BREDD < 1001 OR FILL-IN_HOJD < 683 THEN DO:  
         MESSAGE "Ej giltiga värden!" SKIP           
            "Min höjd är 683."  SKIP
            "Min bredd är 1001."
            VIEW-AS ALERT-BOX.
            musz = TRUE.
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN_BREDDDEF < 1024 OR FILL-IN_HOJDDEF < 740 THEN DO:  
      IF FILL-IN_BREDDDEF < 1001 OR FILL-IN_HOJDDEF < 683 THEN DO:  
         MESSAGE "Ej giltiga värden!" SKIP           
   
            "Normalhöjd minst 683."  SKIP
            "Normalredd minst 1001."
            VIEW-AS ALERT-BOX.
         musz = TRUE.   
         RETURN NO-APPLY.
      END.
   END.
   
   /*
   IF FILL-IN_BREDD > 1024 OR FILL-IN_HOJD > 740 THEN DO:
      IF FILL-IN_BREDD > SESSION:WORK-AREA-WIDTH-PIXELS OR FILL-IN_HOJD > SESSION:WORK-AREA-HEIGHT-PIXELS THEN DO:  
         MESSAGE "Ej giltiga värden!" SKIP
         "Du har en max höj på " SESSION:WORK-AREA-HEIGHT-PIXELS SKIP 
         "och en max bredd på " SESSION:WORK-AREA-WIDTH-PIXELS  SKIP
         VIEW-AS ALERT-BOX.
         musz = TRUE.
         RETURN NO-APPLY.
      END.
   END.
   */
   
   /*
   IF FILL-IN_BREDDDEF > 1024 OR FILL-IN_HOJDDEF > 740 THEN DO:
      IF FILL-IN_BREDDDEF > SESSION:WORK-AREA-WIDTH-PIXELS OR FILL-IN_HOJDDEF > SESSION:WORK-AREA-HEIGHT-PIXELS THEN DO:  
         MESSAGE "Ej giltiga värden!" SKIP
         "Du har en max höj på " SESSION:WORK-AREA-HEIGHT-PIXELS SKIP 
         "och en max bredd på " SESSION:WORK-AREA-WIDTH-PIXELS  SKIP
         VIEW-AS ALERT-BOX.
         musz = TRUE.
         RETURN NO-APPLY.
      END.
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manin_UI Dialog-Frame 
PROCEDURE manin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dirnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(78)" NO-UNDO.
   DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(78)" NO-UNDO.
   DEFINE VARIABLE tmpattrlist AS CHARACTER NO-UNDO.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI Dialog-Frame 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:StartRadForKnappar = RAD_AONR:ROW IN FRAME {&FRAME-NAME}.
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_AONR:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_BER:HANDLE).   
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_KALK:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_MARK:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_PERS:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_TID:HANDLE).   
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_FAKT:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_PLAN:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_STOR:HANDLE).  
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_ALLTIDMAX:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_UTF:HANDLE).   
   Guru.Konstanter:PlaceraKnapparLodratt(RAD_PRISSORT:HANDLE).
   
   ASSIGN                                                   
   FILL-IN_AONR:ROW =                RAD_AONR:ROW           
   FILL-IN_BER:ROW =                 RAD_BER:ROW           
   FILL-IN_KALK:ROW =                RAD_KALK:ROW           
   FILL-IN_MARK:ROW =                RAD_MARK:ROW           
   FILL-IN_PERS:ROW =                RAD_PERS:ROW           
   FILL-IN_TID:ROW =                 RAD_TID:ROW            
   FILL-IN_FAKT:ROW =                RAD_FAKT:ROW           
   FILL-IN_PLAN:ROW =                RAD_PLAN:ROW           
   FILL-IN_STOR:ROW =                RAD_STOR:ROW           
   FILL-IN_ALLTIDMAX:ROW =           RAD_ALLTIDMAX:ROW      
   FILL-IN_UTF:ROW =                 RAD_UTF:ROW            
   FILL-IN_PRISSORT:ROW =            RAD_PRISSORT:ROW       
   
   Guru.GlobalaVariabler:StartRadForKnappar = RAD_PRISSORT:ROW IN FRAME {&FRAME-NAME}  + 2. 
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-DELNR:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-NUMFORM:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-EXCEL:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-AUTOSPAR:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-PNRSELGA:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN-MAXFONSTER:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_WH:HANDLE).
   Guru.GlobalaVariabler:StartRadForKnappar = FILL-IN_WH:ROW. 
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_WB:HANDLE).
   
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_HOJDDEF:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_HOJD:HANDLE).
  
   Guru.GlobalaVariabler:StartRadForKnappar = FILL-IN_HOJDDEF:ROW. 
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_BREDDDEF:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_BREDD:HANDLE).
   BTN_TESTDEFAULT:ROW = FILL-IN_BREDDDEF:ROW.
   BTN_TEST:ROW = FILL-IN_BREDD:ROW.
   Guru.GlobalaVariabler:StartKolumnForKnappar = RAD_PRISSORT:COLUMN - 15.
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-NUMFORM:HANDLE,FALSE).
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-EXCEL:HANDLE,FALSE). 
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-AUTOSPAR:HANDLE,FALSE). 
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-PNRSELGA:HANDLE,FALSE). 
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-DELNR:HANDLE,TRUE). 
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-DELNRMAX:HANDLE,TRUE). 
   FILL-IN-DELNRMAX:ROW =            FILL-IN-DELNR:ROW.
   FILL-IN-DELNRMAX:COLUMN =            FILL-IN-DELNRMAX:COLUMN + 5.   
   Guru.Konstanter:LabelFlytt(FILL-IN-DELNRMAX:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowstart_UI Dialog-Frame 
PROCEDURE rowstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER wh AS HANDLE NO-UNDO.
   IF Guru.GlobalaVariabler:collefth = ? THEN DO:    
      wh:ROW = 3.88.
      Guru.GlobalaVariabler:collefth = wh.
      RETURN.
   END.
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AreControlsRegistered Dialog-Frame 
FUNCTION AreControlsRegistered RETURNS LOGICAL (INPUT ChrObjectList AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE logRC      AS LOGICAL    NO-UNDO INITIAL True.    
   DEFINE VARIABLE intCnt     AS INTEGER    NO-UNDO.    
   DEFINE VARIABLE hdlCheckIt AS COM-HANDLE NO-UNDO.    
   DO intCnt = 1 TO NUM-ENTRIES(chrObjectList):        
      CREATE VALUE(ENTRY(intCnt,chrObjectList)) hdlCheckIt .        
      IF ERROR-STATUS:ERROR = False THEN DO:           
         RELEASE OBJECT hdlCheckIt NO-ERROR.        
      END.
      ELSE  DO:         
         ASSIGN logRC = False.   
      END.
               /*             *             * You can put code here to do more in-depth handling             * of the fact that the object is not registered.             *             * One idea is to attempt to locate and register the             * control using regsvr32.exe (assuming that it exists             * on the users system).             */    
   END.    
   RETURN logRC.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

