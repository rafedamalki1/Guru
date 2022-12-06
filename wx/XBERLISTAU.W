&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/02/96 -  2:27 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */  
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{BERSCHAKTTEMP.I}
{BERKALKTEMP.I}
{KONVALTEMP.I}
DEFINE NEW SHARED VARIABLE stolpval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bryt AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE edmedd AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE aorec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE lista AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE p3var AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE berlistapph AS HANDLE NO-UNDO.
DEFINE VARIABLE anvdator AS CHARACTER NO-UNDO. 
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_UPP RAD_PRIS TOG_MTRL TOG_VAL TOG_ANM ~
TOG_UPP RAD_STOLP TOG_ID TOG_SPANN TOG_TEXT TOG_P3 TOG_EGEN TOG_KON ~
TOG_BYGG TOG_KONVAL TOG_BYGGVAL RAD_SKRIV BTN_VISA BTN_SKRIV BTN_AVB ~
FILL_SKRIV 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP RAD_PRIS TOG_MTRL TOG_VAL TOG_ANM ~
TOG_UPP RAD_STOLP TOG_ID TOG_SPANN TOG_TEXT TOG_P3 TOG_EGEN TOG_KON ~
TOG_BYGG TOG_KONVAL TOG_BYGGVAL ED_MEDD RAD_SKRIV FILL_SKRIV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 12 BY 1.5.

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 51.5 BY 4
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL_SKRIV AS CHARACTER FORMAT "X(256)":U INITIAL "Vill ni vid utskrift" 
      VIEW-AS TEXT 
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PRIS AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pris", 1,
"Ej pris", 2
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SKRIV AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Använda standardskrivare", yes,
"Välja annan skrivare", no
     SIZE 52 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_STOLP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visa allt", 1,
"Bara stolpar", 2,
"Inga stolpar", 3
     SIZE 25 BY 5 NO-UNDO.

DEFINE VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 51.75 BY 7.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANM AS LOGICAL INITIAL no 
     LABEL "Anmärkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_BYGG AS LOGICAL INITIAL no 
     LABEL "Vill Ni vid utskrift få ett byggnummer / sida?" 
     VIEW-AS TOGGLE-BOX
     SIZE 50 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_BYGGVAL AS LOGICAL INITIAL no 
     LABEL "Vill Ni välja byggnummer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.5 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EGEN AS LOGICAL INITIAL no 
     LABEL "Egenkontroll" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ID AS LOGICAL INITIAL no 
     LABEL "Identitet" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_KON AS LOGICAL INITIAL no 
     LABEL "Vill Ni vid utskrift få en konstruktion / sida?" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KONVAL AS LOGICAL INITIAL no 
     LABEL "Vill Ni välja konstruktioner?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SPANN AS LOGICAL INITIAL no 
     LABEL "Spann/kabellängd" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_TEXT AS LOGICAL INITIAL no 
     LABEL "Mtrl text" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.25 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Konstruktionsval" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     SEL_UPP AT ROW 2 COL 4 NO-LABEL
     RAD_PRIS AT ROW 10 COL 4 NO-LABEL
     TOG_MTRL AT ROW 10.25 COL 25
     TOG_VAL AT ROW 10.25 COL 41.38
     TOG_ANM AT ROW 11.67 COL 25
     TOG_UPP AT ROW 11.67 COL 41.38
     RAD_STOLP AT ROW 12.63 COL 4 NO-LABEL
     TOG_ID AT ROW 12.96 COL 25
     TOG_SPANN AT ROW 12.96 COL 41.38
     TOG_TEXT AT ROW 14.29 COL 25
     TOG_P3 AT ROW 14.29 COL 41.38
     TOG_EGEN AT ROW 15.58 COL 25
     TOG_KON AT ROW 16.75 COL 4
     TOG_BYGG AT ROW 16.75 COL 4
     TOG_KONVAL AT ROW 17.88 COL 4
     TOG_BYGGVAL AT ROW 17.88 COL 4
     ED_MEDD AT ROW 19.08 COL 4 NO-LABEL
     RAD_SKRIV AT ROW 24.63 COL 5.75 NO-LABEL
     BTN_VISA AT ROW 26.5 COL 9
     BTN_SKRIV AT ROW 26.5 COL 25.63
     BTN_AVB AT ROW 26.5 COL 41.75
     FILL_SKRIV AT ROW 23.5 COL 18.5 COLON-ALIGNED NO-LABEL
     "Listor:" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 1.17 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61.38 BY 27.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Listor"
         HEIGHT             = 27.71
         WIDTH              = 61.5
         MAX-HEIGHT         = 29.17
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 29.17
         VIRTUAL-WIDTH      = 82
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* SETTINGS FOR EDITOR ED_MEDD IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       ED_MEDD:HIDDEN IN FRAME FRAME-B           = TRUE
       ED_MEDD:RETURN-INSERTED IN FRAME FRAME-B  = TRUE.

ASSIGN 
       FILL_SKRIV:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       RAD_SKRIV:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_STOLP IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       RAD_STOLP:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ANM IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_ANM:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       TOG_BYGG:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       TOG_BYGGVAL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EGEN IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_EGEN:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ID IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_ID:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       TOG_KONVAL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MTRL IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_MTRL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_P3 IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_P3:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SPANN IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_SPANN:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_TEXT IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_TEXT:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_UPP IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_UPP:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_VAL IN FRAME FRAME-B
   SHARED                                                               */
ASSIGN 
       TOG_VAL:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO: 
   IF SEL_UPP = "Schaktning" THEN DO:
      MESSAGE "Denna lista går ej att skriva ut i detta läge. Du kommer nu att hamna i det program som gör det möjligt att visa och skriva ut listan."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      skrivut = FALSE.
      RUN visa_UI.
   END.
   ELSE DO:       
      RUN SKRIVVAL.W (INPUT FALSE).       
      IF musz = TRUE THEN musz = FALSE. 
      ELSE DO:                     
         skrivut = TRUE.
         RUN visa_UI.
      END.        
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-2
ON CHOOSE OF BTN_VISA IN FRAME FRAME-B /* Visa */
DO:   
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PRIS WINDOW-2
ON VALUE-CHANGED OF RAD_PRIS IN FRAME FRAME-B
DO:
   ASSIGN
   RAD_PRIS = INPUT RAD_PRIS
   prisvar = RAD_PRIS.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SKRIV WINDOW-2
ON VALUE-CHANGED OF RAD_SKRIV IN FRAME FRAME-B
DO:
   RAD_SKRIV = INPUT RAD_SKRIV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_STOLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_STOLP WINDOW-2
ON VALUE-CHANGED OF RAD_STOLP IN FRAME FRAME-B
DO:
   RAD_STOLP = INPUT RAD_STOLP.
   stolpval = RAD_STOLP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-B
DO:   
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE. 
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-2
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.   
   IF SEL_UPP = "Materiel/upplag/konstruktion/artikel" THEN DO:
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF p3var = TRUE THEN TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ELSE TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.                 
      IF Guru.Konstanter:appfel = FALSE THEN DO: 
         RUN INLOAPI.P (OUTPUT anvdator).
         datornamn = TRIM(SUBSTRING(anvdator,25,20)).
         IF datornamn = "thnmf" OR datornamn = "thnblade01" THEN DO:
            ASSIGN
            FILL_SKRIV:HIDDEN = TRUE
            RAD_SKRIV:HIDDEN = TRUE      
            RAD_SKRIV = FALSE.
         END.
         ELSE DO:
            FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
      END.
      ELSE DO:
         FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.   
   ELSE IF SEL_UPP = "Materiel/upplag/projekt/objekt" THEN DO:
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
      IF p3var = TRUE THEN TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ELSE TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.           
   END.
   ELSE IF SEL_UPP = "Satsläggning och byggnummer" THEN DO:
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE     
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         
      IF p3var = TRUE THEN TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ELSE TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.           
   END.
   ELSE IF SEL_UPP = "Byggprotokoll i skrivbordsordning" THEN DO:
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ASSIGN
         TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.           
      END.
      ELSE DO:
         ASSIGN
         TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      IF p3var = TRUE THEN TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ELSE TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF Guru.Konstanter:appfel = FALSE THEN DO: 
         RUN INLOAPI.P (OUTPUT anvdator).
         datornamn = TRIM(SUBSTRING(anvdator,25,20)).
         IF datornamn = "thnmf" OR datornamn = "thnblade01" THEN DO:
            ASSIGN
            FILL_SKRIV:HIDDEN = TRUE
            RAD_SKRIV:HIDDEN = TRUE      
            RAD_SKRIV = FALSE.
         END.
         ELSE DO:
            FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
      END.
      ELSE DO:
         FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.  
   ELSE IF SEL_UPP = "Materiel/artikel" THEN DO:
      ASSIGN
      TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE IF SEL_UPP = "Använt materiel" THEN DO:
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ASSIGN
         TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.           
      END.
      ELSE DO:
         ASSIGN
         TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ASSIGN      
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   ELSE IF SEL_UPP = "Materielspecifikation" THEN DO: 
      ASSIGN
      TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE            
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE IF SEL_UPP = "Stolpprotokoll" THEN DO:
      ASSIGN
      TOG_ANM = FALSE
      TOG_ID = TRUE      
      TOG_MTRL = TRUE
      TOG_P3 = FALSE
      TOG_TEXT = FALSE
      TOG_SPANN = FALSE
      TOG_UPP = FALSE
      TOG_VAL = FALSE
      TOG_EGEN = FALSE.   
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ASSIGN
         TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.           
      END.
      ELSE DO:
         ASSIGN
         TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      DISPLAY TOG_ANM TOG_ID TOG_UPP TOG_VAL WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_STOLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ANM WINDOW-2
ON VALUE-CHANGED OF TOG_ANM IN FRAME FRAME-B /* Anmärkning */
DO:
   TOG_ANM  = INPUT TOG_ANM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BYGG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BYGG WINDOW-2
ON VALUE-CHANGED OF TOG_BYGG IN FRAME FRAME-B /* Vill Ni vid utskrift få ett byggnummer / sida? */
DO:
   TOG_BYGG = INPUT TOG_BYGG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BYGGVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BYGGVAL WINDOW-2
ON VALUE-CHANGED OF TOG_BYGGVAL IN FRAME FRAME-B /* Vill Ni välja byggnummer? */
DO:
   TOG_BYGGVAL = INPUT TOG_BYGGVAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EGEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EGEN WINDOW-2
ON VALUE-CHANGED OF TOG_EGEN IN FRAME FRAME-B /* Egenkontroll */
DO:
   TOG_EGEN = INPUT TOG_EGEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ID WINDOW-2
ON VALUE-CHANGED OF TOG_ID IN FRAME FRAME-B /* Identitet */
DO:
   TOG_ID = INPUT TOG_ID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KON WINDOW-2
ON VALUE-CHANGED OF TOG_KON IN FRAME FRAME-B /* Vill Ni vid utskrift få en konstruktion / sida? */
DO:
  TOG_KON = INPUT TOG_KON.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONVAL WINDOW-2
ON VALUE-CHANGED OF TOG_KONVAL IN FRAME FRAME-B /* Vill Ni välja konstruktioner? */
DO:
   TOG_KONVAL = INPUT TOG_KONVAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRL WINDOW-2
ON VALUE-CHANGED OF TOG_MTRL IN FRAME FRAME-B /* Materiel */
DO:
   TOG_MTRL = INPUT TOG_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_P3 WINDOW-2
ON VALUE-CHANGED OF TOG_P3 IN FRAME FRAME-B /* P3-koder */
DO:
   TOG_P3 = INPUT TOG_P3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_SPANN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SPANN WINDOW-2
ON VALUE-CHANGED OF TOG_SPANN IN FRAME FRAME-B /* Spann/kabellängd */
DO:
   TOG_SPANN = INPUT TOG_SPANN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TEXT WINDOW-2
ON VALUE-CHANGED OF TOG_TEXT IN FRAME FRAME-B /* Mtrl text */
DO:
   TOG_TEXT = INPUT TOG_TEXT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP WINDOW-2
ON VALUE-CHANGED OF TOG_UPP IN FRAME FRAME-B /* Upplag */
DO:
   TOG_UPP = INPUT TOG_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL WINDOW-2
ON VALUE-CHANGED OF TOG_VAL IN FRAME FRAME-B /* Konstruktionsval */
DO:
   TOG_VAL = INPUT TOG_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(berlistapph) THEN DELETE PROCEDURE berlistapph.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   RUN laddaberlista_UI IN berlistapph (INPUT valaonr,INPUT valomrade,
                                        OUTPUT TABLE berschakttemp,OUTPUT TABLE berkalktemp).

   {&WINDOW-NAME}:TITLE = "Listor beredning nr: " + STRING(valaonr) + " benämning: " + valort. 
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      ASSIGN
      RAD_PRIS =  1
      prisvar = 1.
   END.
   ELSE DO:
      ASSIGN
      RAD_PRIS = 2
      prisvar = 2.  
   END.
   ASSIGN
   TOG_ANM = TRUE
   TOG_ID = TRUE      
   TOG_MTRL = TRUE
   TOG_P3 = TRUE
   TOG_TEXT = TRUE
   TOG_SPANN = TRUE
   TOG_UPP = TRUE
   TOG_VAL = TRUE
   TOG_EGEN = FALSE
   RAD_STOLP = 1.

   status-ok = SEL_UPP:DELETE(""). 
   EMPTY TEMP-TABLE kon_val NO-ERROR.    
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      ASSIGN
      status-ok = SEL_UPP:ADD-LAST("Materiel/projekt") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/projekt") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/konstruktionsgrupp/projekt") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/projekt/objekt") IN FRAME {&FRAME-NAME}.       
   END.   
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      status-ok = SEL_UPP:ADD-LAST("Materielspecifikation") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/artikel") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/konstruktionsgrupp/artikel") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Satsläggning och byggnummer") IN FRAME {&FRAME-NAME}.       
   END.
   ELSE DO:
      ASSIGN
      status-ok = SEL_UPP:ADD-LAST("Materiel/artikel") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/artikel") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/konstruktionsgrupp/artikel") IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materiel/upplag/konstruktion/artikel") IN FRAME {&FRAME-NAME}.                      
   END. 
   ASSIGN
   status-ok = SEL_UPP:ADD-LAST("Byggprotokoll i skrivbordsordning") IN FRAME {&FRAME-NAME}   
   status-ok = SEL_UPP:ADD-LAST("Kabelskåpskort") IN FRAME {&FRAME-NAME}   
   status-ok = SEL_UPP:ADD-LAST("Stolpprotokoll") IN FRAME {&FRAME-NAME}. 
   status-ok = SEL_UPP:ADD-LAST("Lista artiklar med text") IN FRAME {&FRAME-NAME}.   
   status-ok = SEL_UPP:ADD-LAST("Använt materiel") IN FRAME {&FRAME-NAME}.   
   FIND FIRST berschakttemp WHERE berschakttemp.AONR = valaonr AND
   berschakttemp.OMRADE = valomrade USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE berschakttemp THEN
   status-ok = SEL_UPP:ADD-LAST("Schaktning") IN FRAME {&FRAME-NAME}.

   FIND FIRST berkalktemp WHERE berkalktemp.AONR = valaonr AND
   berkalktemp.OMRADE = valomrade USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE berkalktemp THEN DO:    
      ASSIGN
      p3var = TRUE
      status-ok = SEL_UPP:ADD-LAST("P3-Kalkyl") IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      TOG_P3 = FALSE
      p3var = FALSE.
   END.      
   IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
      status-ok = SEL_UPP:ADD-LAST("Ericsson kablar") IN FRAME {&FRAME-NAME}.
      status-ok = SEL_UPP:ADD-LAST("Transformatorer") IN FRAME {&FRAME-NAME}.
      RAD_STOLP:ADD-LAST("Bara kablar/transformatorer", 4).
      RAD_STOLP:ADD-LAST("Inga kablar/transformatorer", 5).
   END.   
   RUN enable_UI.   
   {FRMSIZE.I}   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ENABLE ED_MEDD WITH FRAME {&FRAME-NAME}.
      ED_MEDD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ED_MEDD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ASSIGN
   TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
   TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_SPANN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_KONVAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_BYGG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_BYGGVAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RAD_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globanv = "ENTREP" THEN DISABLE RAD_PRIS WITH FRAME {&FRAME-NAME}.
   {musarrow.i}  
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERLISTAPP.P PERSISTENT SET berlistapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERLISTAPP.P PERSISTENT SET berlistapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY SEL_UPP RAD_PRIS TOG_MTRL TOG_VAL TOG_ANM TOG_UPP RAD_STOLP TOG_ID 
          TOG_SPANN TOG_TEXT TOG_P3 TOG_EGEN TOG_KON TOG_BYGG TOG_KONVAL 
          TOG_BYGGVAL ED_MEDD RAD_SKRIV FILL_SKRIV 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE SEL_UPP RAD_PRIS TOG_MTRL TOG_VAL TOG_ANM TOG_UPP RAD_STOLP TOG_ID 
         TOG_SPANN TOG_TEXT TOG_P3 TOG_EGEN TOG_KON TOG_BYGG TOG_KONVAL 
         TOG_BYGGVAL RAD_SKRIV BTN_VISA BTN_SKRIV BTN_AVB FILL_SKRIV 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-2 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}   
   {AVBGOM.I}
   DEFINE VARIABLE vem AS CHARACTER NO-UNDO.
   RUN INLOAPI.P (OUTPUT vem).
   valkonst = FALSE.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "UMEA" THEN DO:
      edmedd = INPUT FRAME {&FRAME-NAME} ED_MEDD.
   END.
   IF SEL_UPP = "Använt materiel" THEN DO:
      musz = FALSE.      
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:         
         TOG_BYGGVAL = INPUT TOG_BYGGVAL.                  
         IF TOG_BYGGVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.
      END.
      ELSE DO:      
         TOG_KONVAL = INPUT TOG_KONVAL.
         IF TOG_KONVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.   
      END.
      IF musz = FALSE THEN DO:
         RUN BERLISTM.W.    
      END.      
   END.   
   IF SEL_UPP = "Materiel/artikel" THEN DO:
      musz = FALSE.      
      TOG_KONVAL = INPUT TOG_KONVAL.
      IF TOG_KONVAL THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.      
      IF musz = FALSE THEN DO:
         RUN BERLISTU1.W.    
      END.      
   END.   
   ELSE IF SEL_UPP = "Materiel/projekt" THEN DO: 
      RUN BERLISTU1.W.        
   END.
   ELSE IF SEL_UPP = "Materielspecifikation" THEN DO: 
      musz = FALSE.      
      TOG_BYGGVAL = INPUT TOG_BYGGVAL.
      IF TOG_BYGGVAL THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.      
      IF musz = FALSE THEN DO:
         RUN BERLISTU1.W. 
      END.      
   END.
   ELSE IF SEL_UPP = "Materiel/upplag/artikel" THEN DO: 
      RUN BERLISTU2.W.        
   END.   
   ELSE IF SEL_UPP = "Materiel/upplag/projekt" THEN DO: 
      RUN BERLISTU2.W.          
   END.
   ELSE IF SEL_UPP = "Materiel/upplag/konstruktionsgrupp/artikel" THEN DO:
      RUN BERLISTU3.W.       
   END.   
   ELSE IF SEL_UPP = "Materiel/upplag/konstruktionsgrupp/projekt" THEN DO: 
      RUN BERLISTU3.W.           
   END.
   ELSE IF SEL_UPP = "Materiel/upplag/konstruktion/artikel" THEN DO:      
      ASSIGN
      TOG_KON = INPUT TOG_KON
      TOG_KONVAL = INPUT TOG_KONVAL
      RAD_SKRIV = INPUT RAD_SKRIV.
      IF datornamn = "thnmf" OR datornamn = "thnblade01" THEN DO:
         RAD_SKRIV = FALSE.
      END.
      IF TOG_KON THEN DO:
         bryt = TRUE.
      END.
      ELSE DO:
         bryt = FALSE.
      END.
      musz = FALSE.      
      IF TOG_KONVAL THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.                         
      IF musz = FALSE THEN DO:
         RUN BERLISTU4.W (INPUT SEL_UPP,INPUT RAD_SKRIV, INPUT FALSE).
      END.
   END.   
   ELSE IF SEL_UPP = "Stolpprotokoll" THEN DO:      
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ASSIGN
         TOG_BYGG = INPUT TOG_BYGG
         TOG_BYGGVAL = INPUT TOG_BYGGVAL
         RAD_SKRIV = INPUT RAD_SKRIV.
         IF TOG_BYGG THEN DO:
            bryt = TRUE.
         END.
         ELSE DO:
            bryt = FALSE.
         END.
         musz = FALSE.     
         IF TOG_BYGGVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.
      END.
      ELSE DO:      
         ASSIGN
         TOG_KON = INPUT TOG_KON
         TOG_KONVAL = INPUT TOG_KONVAL
         RAD_SKRIV = INPUT RAD_SKRIV.
         IF datornamn = "thnmf" OR datornamn = "thnblade01" THEN DO:
            RAD_SKRIV = FALSE.
         END.
         IF TOG_KON THEN DO:
            bryt = TRUE.
         END.
         ELSE DO:
            bryt = FALSE.
         END.
         musz = FALSE.      
         IF TOG_KONVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.                         
      END.
      IF musz = FALSE THEN DO:
         RUN BERLISTU4.W (INPUT SEL_UPP,INPUT RAD_SKRIV, INPUT FALSE).
      END.
   END.   
   ELSE IF SEL_UPP = "Satsläggning och byggnummer" THEN DO:      
      ASSIGN
      TOG_BYGG = INPUT TOG_BYGG
      TOG_BYGGVAL = INPUT TOG_BYGGVAL
      RAD_SKRIV = INPUT RAD_SKRIV.
      IF TOG_BYGG THEN DO:
         bryt = TRUE.
      END.
      ELSE DO:
         bryt = FALSE.
      END.
      musz = FALSE.     
      IF TOG_BYGGVAL THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.
      IF musz = FALSE THEN DO:        
         RUN BERLISTU4.W (INPUT SEL_UPP,INPUT RAD_SKRIV, INPUT FALSE).        
      END.
   END.
   ELSE IF SEL_UPP = "Materiel/upplag/projekt/objekt" THEN DO:      
      ASSIGN
      TOG_KON = INPUT TOG_KON
      TOG_KONVAL = INPUT TOG_KONVAL
      RAD_SKRIV = INPUT RAD_SKRIV.
      IF TOG_KON THEN DO:
         bryt = TRUE.
      END.
      ELSE DO:
         bryt = FALSE.
      END. 
      IF TOG_KONVAL THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.                         
      RUN BERLISTU4.W (INPUT SEL_UPP,INPUT RAD_SKRIV, INPUT FALSE).
   END.   
   ELSE IF SEL_UPP = "Byggprotokoll i skrivbordsordning" THEN DO: 
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ASSIGN
         TOG_BYGG = INPUT TOG_BYGG
         TOG_BYGGVAL = INPUT TOG_BYGGVAL
         RAD_SKRIV = INPUT RAD_SKRIV.
         IF TOG_BYGG THEN DO:
            bryt = TRUE.
         END.
         ELSE DO:
            bryt = FALSE.
         END.
         musz = FALSE.     
         IF TOG_BYGGVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.
      END.
      ELSE DO:      
         ASSIGN
         TOG_KON = INPUT TOG_KON
         TOG_KONVAL = INPUT TOG_KONVAL
         RAD_SKRIV = INPUT RAD_SKRIV.
         IF datornamn = "thnmf" OR datornamn = "thnblade01" THEN DO:
            RAD_SKRIV = FALSE.
         END.
         IF TOG_KON THEN DO:
            bryt = TRUE.
         END.
         ELSE DO:
            bryt = FALSE.
         END.
         musz = FALSE.      
         IF TOG_KONVAL THEN DO:
            valkonst = TRUE.
            RUN VALKONSTU.W.
         END.
         ELSE DO:
            valkonst = FALSE.
         END.                       
      END.
      IF musz = FALSE THEN DO:
         RUN BERLISTU4.W (INPUT SEL_UPP,INPUT RAD_SKRIV, INPUT TRUE).
      END.
   END.   
   ELSE IF SEL_UPP = "Schaktning" THEN DO:
      RUN SCHALISTU.W.           
   END.  
   ELSE IF SEL_UPP = "P3-Kalkyl" THEN DO:      
      RUN VBERKALKU.W.                        
   END.
   ELSE IF SEL_UPP = "Kabelskåpskort" THEN DO:      
      RUN VALKORTU.W.                        
   END.
   ELSE IF SEL_UPP = "Ericsson kablar" THEN DO:      
      RUN ERICKABU.W (INPUT 1).                        
   END.
   ELSE IF SEL_UPP = "Transformatorer" THEN DO:      
      RUN ERICKABU.W (INPUT 2).                        
   END.
   ELSE IF SEL_UPP = "Lista artiklar med text" THEN DO:           
      MESSAGE "Vill Ni välja konstruktioner?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar.         
      {AVBGOM.I}
      IF svar THEN DO:
         valkonst = TRUE.
         RUN VALKONSTU.W.
      END.
      ELSE DO:
         valkonst = FALSE.
      END.  
      RUN BERLISTA5U.W.                        
   END.
   {AVBFRAM.I}
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

