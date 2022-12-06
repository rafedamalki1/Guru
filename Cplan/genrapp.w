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

  Created: 95/07/04 - 11:31 am

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
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
   {JURPERST.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{SOKDEF.I}
&SCOPED-DEFINE SHARED SHARED
{REGVAR.I}
{OMRTEMPW.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE listnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE tillar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE muszval AS INTEGER NO-UNDO.     
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE sparar1 AS INTEGER NO-UNDO.
DEFINE VARIABLE sparar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE franarorg AS INTEGER NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     LABEL "DEBITERING" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)" 
     LABEL "ANL.NR." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)" 
     LABEL "ARBETSANSVARIG" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "ARBETSART" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(5)" 
     LABEL "BEREDARE/PROJEKTÖR" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" 
     LABEL "OMRÅDE" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "PRIORITET" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_SLUTVNR AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "SLUT  VNR" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_STARTVNR AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "START VNR" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_FRAN CMB_TILL TOG_EJAV TOG_AVS TOG_KONTO ~
TOG_BEN RAD_FAST FBTN_VLIST FBTN_SKR SEL_OMR SEL_LIST BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_FRAN CMB_TILL TOG_EJAV TOG_AVS ~
TOG_KONTO TOG_BEN RAD_FAST FILL-IN-VALJ SEL_OMR SEL_LIST 

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
     SIZE 13.5 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 13.5 BY 1.

DEFINE BUTTON FBTN_VLIST 
     LABEL "Visa":L 
     SIZE 13.5 BY 1.

DEFINE VARIABLE CMB_FRAN AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Från" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TILL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "till" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.75
     FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillf. Plannr", no,
"Fasta Plannr", yes
     SIZE 37.88 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_LIST AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 36.5 BY 11 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_OMR AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 22 BY 11 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANL AS LOGICAL INITIAL no 
     LABEL "ALLA ANLÄGGNINGAR" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_AVS AS LOGICAL INITIAL no 
     LABEL "Avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_BEN AS LOGICAL INITIAL no 
     LABEL "Visa benämning" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EJAV AS LOGICAL INITIAL no 
     LABEL "Ej avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONTO AS LOGICAL INITIAL no 
     LABEL "Visa konto" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_FRAN AT ROW 3.63 COL 2.25
     CMB_TILL AT ROW 3.63 COL 21
     TOG_EJAV AT ROW 5 COL 1.5
     TOG_AVS AT ROW 5 COL 21.13
     FILL-IN-MELL AT ROW 5 COL 32.63 COLON-ALIGNED NO-LABEL
     FILL-IN-STARTD AT ROW 5 COL 40.75 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 5 COL 51 COLON-ALIGNED NO-LABEL
     FILL-IN-SLUTD AT ROW 5 COL 55.75 COLON-ALIGNED NO-LABEL
     TOG_KONTO AT ROW 5.88 COL 1.5
     TOG_BEN AT ROW 5.88 COL 21.13
     RAD_FAST AT ROW 6.79 COL 1.5 NO-LABEL
     FBTN_VLIST AT ROW 8 COL 69
     FBTN_SKR AT ROW 9.1 COL 69
     FILL-IN-VALJ AT ROW 9.5 COL 1.5 NO-LABEL
     SEL_OMR AT ROW 11.75 COL 1.5 NO-LABEL
     SEL_LIST AT ROW 11.75 COL 30.25 NO-LABEL
     BTN_AVB AT ROW 22.96 COL 69
     TOG_ANL AT ROW 23.5 COL 1.5
     "Gör ett urval och välj lista:" VIEW-AS TEXT
          SIZE 46 BY 1.5 AT ROW 1.25 COL 1.5
          FONT 17
     "Välj lista" VIEW-AS TEXT
          SIZE 23 BY 1.5 AT ROW 9.75 COL 28
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.88 BY 23.54.


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
         TITLE              = "Listor för plannummer"
         HEIGHT             = 23.54
         WIDTH              = 83
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
/* SETTINGS FOR COMBO-BOX CMB_FRAN IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_TILL IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SLUTD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR SELECTION-LIST SEL_LIST IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR SELECTION-LIST SEL_OMR IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_ANL IN FRAME FRAME-B
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_ANL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_AVS IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_BEN IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_EJAV IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_KONTO IN FRAME FRAME-B
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:   
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   franar = franarorg.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FRAN WINDOW-2
ON LEAVE OF CMB_FRAN IN FRAME FRAME-B /* Från */
DO:                            
   ASSIGN
   CMB_FRAN = INPUT CMB_FRAN
   franar = CMB_FRAN.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FRAN WINDOW-2
ON VALUE-CHANGED OF CMB_FRAN IN FRAME FRAME-B /* Från */
DO:                           
   ASSIGN
   CMB_FRAN = INPUT CMB_FRAN
   franar = CMB_FRAN.  
   RUN check_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TILL WINDOW-2
ON LEAVE OF CMB_TILL IN FRAME FRAME-B /* till */
DO:                            
   ASSIGN
   CMB_TILL = INPUT CMB_TILL
   tillar = CMB_TILL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TILL WINDOW-2
ON VALUE-CHANGED OF CMB_TILL IN FRAME FRAME-B /* till */
DO:                           
   ASSIGN
   CMB_TILL = INPUT CMB_TILL
   tillar = CMB_TILL.   
   RUN check_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-2
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-B /* Skriv ut */
DO: 
   skrivut = TRUE.
   RUN visa_UI.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VLIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VLIST WINDOW-2
ON CHOOSE OF FBTN_VLIST IN FRAME FRAME-B /* Visa */
DO:
   skrivut = FALSE.  
   RUN visa_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD WINDOW-2
ON LEAVE OF FILL-IN-SLUTD IN FRAME FRAME-B
DO:
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTD IN FRAME FRAME-B
DO:
   ASSIGN
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTD.
   RUN AlmanBtn.w.
   FILL-IN-SLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD WINDOW-2
ON LEAVE OF FILL-IN-STARTD IN FRAME FRAME-B
DO:
   FILL-IN-STARTD = INPUT FILL-IN-STARTD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-STARTD IN FRAME FRAME-B
DO:
   ASSIGN
   FILL-IN-STARTD = INPUT FILL-IN-STARTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTD.
   RUN AlmanBtn.w.
   FILL-IN-STARTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST WINDOW-2
ON VALUE-CHANGED OF RAD_FAST IN FRAME FRAME-B
DO:
   RAD_FAST = INPUT RAD_FAST.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_LIST WINDOW-2
ON VALUE-CHANGED OF SEL_LIST IN FRAME FRAME-B
DO:
   SEL_LIST = INPUT SEL_LIST.
   IF SEL_LIST = "Alla " + LC(Guru.Konstanter:gplk) + " / " + Guru.Konstanter:gomrk THEN listnr = 1.
   IF SEL_LIST = "Lista debiteringspris / " + Guru.Konstanter:gomrk THEN listnr = 2.
   IF SEL_LIST = "Lista " + LC(Guru.Konstanter:gartk)+ " / " + Guru.Konstanter:gomrk THEN listnr = 3.
   IF SEL_LIST = "Lista " + LC(Guru.Konstanter:gpriok) + " / " + Guru.Konstanter:gomrk THEN listnr = 4.
   IF SEL_LIST = "Lista start - slut VNR / " + Guru.Konstanter:gomrk THEN listnr = 5.
   IF SEL_LIST = "Lista " + Guru.Konstanter:gbestk + " / " + Guru.Konstanter:gomrk THEN listnr = 6.
   IF SEL_LIST = "Lista " + Guru.Konstanter:gberek + " / " + Guru.Konstanter:gomrk THEN listnr = 7.
   IF SEL_LIST = "Lista " + Guru.Konstanter:garbal + " / " + Guru.Konstanter:gomrk THEN listnr = 8.
   IF SEL_LIST = "Lista anläggningsnr / " + Guru.Konstanter:gomrk THEN listnr = 10.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " med " + LC(Guru.Konstanter:gartk) + " / " + Guru.Konstanter:gomrk THEN listnr = 19.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_OMR WINDOW-2
ON VALUE-CHANGED OF SEL_OMR IN FRAME FRAME-B
DO:
   SEL_OMR = INPUT SEL_OMR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ANL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ANL WINDOW-2
ON VALUE-CHANGED OF TOG_ANL IN FRAME FRAME-B /* ALLA ANLÄGGNINGAR */
DO:
   TOG_ANL = INPUT TOG_ANL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVS WINDOW-2
ON VALUE-CHANGED OF TOG_AVS IN FRAME FRAME-B /* Avslutade */
DO:
   TOG_AVS = INPUT TOG_AVS.
   IF TOG_AVS = TRUE THEN DO:
      ENABLE FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-MELL FILL-IN-OCH FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:       
      ASSIGN
      FILL-IN-MELL:HIDDEN = TRUE
      FILL-IN-OCH:HIDDEN = TRUE  
      FILL-IN-SLUTD:HIDDEN = TRUE  
      FILL-IN-STARTD:HIDDEN = TRUE.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BEN WINDOW-2
ON VALUE-CHANGED OF TOG_BEN IN FRAME FRAME-B /* Visa benämning */
DO:
   TOG_BEN = INPUT TOG_BEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJAV WINDOW-2
ON VALUE-CHANGED OF TOG_EJAV IN FRAME FRAME-B /* Ej avslutade */
DO:
   TOG_EJAV = INPUT TOG_EJAV.
   IF TOG_AVS = TRUE THEN DO:
      ENABLE FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-MELL FILL-IN-OCH FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:                      
      ASSIGN
      FILL-IN-MELL:HIDDEN = TRUE
      FILL-IN-OCH:HIDDEN = TRUE  
      FILL-IN-SLUTD:HIDDEN = TRUE  
      FILL-IN-STARTD:HIDDEN = TRUE.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONTO WINDOW-2
ON VALUE-CHANGED OF TOG_KONTO IN FRAME FRAME-B /* Visa konto */
DO:
    TOG_KONTO = INPUT TOG_KONTO.
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
   RUN disable_UI.

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
   status-ok = SEL_OMR:ADD-LAST("ALLA UTAN ORG.ENHET").
   status-ok = SEL_OMR:ADD-LAST("ALLA").
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   
   {OMRHMT.I}
   SEL_OMR:LIST-ITEMS = "". 
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = SEL_OMR:ADD-LAST(omrtemp.NAMN).
   END.
   
   ASSIGN
   TOG_BEN = TRUE
   TOG_KONTO = FALSE.
   status-ok = SEL_LIST:ADD-LAST("Alla " + LC(Guru.Konstanter:gplk) + " / " + LC(Guru.Konstanter:gomrk)).
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " med " + LC(Guru.Konstanter:gartk) + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista debiteringspris / " + LC(Guru.Konstanter:gomrk)).
   status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:gartk)+ " / " + Guru.Konstanter:gomrk).
   /*
   status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:gpriok) + " / " + gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista start - slut VNR / " + LC(Guru.Konstanter:gomrk)).
   */
   status-ok = SEL_LIST:ADD-LAST("Lista " + Guru.Konstanter:gbestk + " / " + LC(Guru.Konstanter:gomrk)).
/*    status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:gberek) + " / " + LC(Guru.Konstanter:gomrk)). */
   status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:garbak) + " / " + LC(Guru.Konstanter:gomrk)).
   /*status-ok = SEL_LIST:ADD-LAST("Lista anläggningsnr / " + LC(Guru.Konstanter:gomrk)).*/
  
   ASSIGN   
   /*
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbal 
   FILL-IN_BEREDARE:LABEL = Guru.Konstanter:gberek    
   */
   TOG_EJAV = TRUE
   TOG_AVS = FALSE
   FILL-IN-MELL = "mellan" 
   FILL-IN-OCH = "och"    
   FILL-IN-SLUTD = TODAY
   FILL-IN-STARTD = DATE(01,01,YEAR(TODAY)).

   
   ASSIGN
   status-ok = CMB_FRAN:DELETE("0")
   status-ok = CMB_TILL:DELETE("0").  
   /*LADDAR ÅR I CMB_FRAN*/  
   ASSIGN
   franarorg = franar
   uppar = franar
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_FRAN:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_FRAN:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   CMB_FRAN:SCREEN-VALUE = STRING(franar,"9999").
   
    /*LADDAR ÅR I CMB_TILL*/  
   uppar = franar.
   slutar = YEAR(TODAY) + 9.
   status-ok = CMB_TILL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_TILL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   IF period = 1 THEN DO:  
      ASSIGN
      CMB_TILL:SCREEN-VALUE = STRING(franar,"9999").
   END.
   ELSE DO:
      ASSIGN
      CMB_TILL:SCREEN-VALUE = STRING(tillar,"9999").
   END.
   ASSIGN
   FILL-IN-VALJ = "Välj " + Guru.Konstanter:gomrk
   status-ok = RAD_FAST:DELETE("Tillf. Plannr")
   status-ok = RAD_FAST:DELETE("Fasta Plannr").

   RAD_FAST:ADD-LAST("Tillf. " + LC(Guru.Konstanter:gplk), no).
   RAD_FAST:ADD-LAST("Fasta " + LC(Guru.Konstanter:gplk), yes).   
   
   ASSIGN  
   CMB_FRAN = INPUT CMB_FRAN
   CMB_TILL = INPUT CMB_TILL
   franar = CMB_FRAN
   tillar = CMB_TILL
   sparar1 = CMB_FRAN
   sparar2 = CMB_TILL.
   
   RUN enable_UI.   
   {FRMSIZE.I}           
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check_UI WINDOW-2 
PROCEDURE check_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF CMB_FRAN > CMB_TILL THEN DO:     
      MESSAGE "Årtalet 'Från' får inte vara större än årtalet 'Till'. Byt årtal." 
      VIEW-AS ALERT-BOX TITLE "Meddelnade".
      ASSIGN
      CMB_FRAN = sparar1
      CMB_TILL = sparar2.
      DISPLAY CMB_FRAN WITH FRAME {&FRAME-NAME}.
      DISPLAY CMB_TILL WITH FRAME {&FRAME-NAME}.
      APPLY "ENTRY" TO CMB_FRAN IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ASSIGN
   sparar1 = CMB_FRAN
   sparar2 = CMB_TILL.   
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
  DISPLAY CMB_FRAN CMB_TILL TOG_EJAV TOG_AVS TOG_KONTO TOG_BEN RAD_FAST 
          FILL-IN-VALJ SEL_OMR SEL_LIST 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_FRAN CMB_TILL TOG_EJAV TOG_AVS TOG_KONTO TOG_BEN RAD_FAST 
         FBTN_VLIST FBTN_SKR SEL_OMR SEL_LIST BTN_AVB 
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
  /* IF SEL_OMR = ? OR SEL_OMR = "" THEN DO:
      MESSAGE "Välj ett " + LC(Guru.Konstanter:gomrk) + "! "VIEW-AS ALERT-BOX.
      RETURN.
   END.*/      
   IF SEL_LIST = ? OR SEL_LIST = "" THEN DO:
      MESSAGE "Välj en lista! "VIEW-AS ALERT-BOX.
      RETURN.
   END. 
   IF TOG_AVS = FALSE AND TOG_EJAV = FALSE THEN DO:
      MESSAGE "Fyll i ""Ej avslutade"" eller ""avslutade"" eller båda!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF listnr NE 5 THEN DO:
      IF TOG_BEN = FALSE AND TOG_KONTO = FALSE THEN DO:
         MESSAGE 
         "Fyll i ""Visa konto"" eller ""Visa benämning"" eller båda!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   IF FILL-IN-STARTD > FILL-IN-SLUTD THEN DO:
      MESSAGE "Startdatum kan inte vara större än slutdatum! " VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-STARTD IN FRAME {&FRAME-NAME}. 
      RETURN.
   END.     
   ASSIGN
   bdatum = FILL-IN-STARTD
   avdatum = FILL-IN-SLUTD.
   {muswait.i}
   
   RUN PURVAL.W.
   IF musz = TRUE THEN DO:
      ASSIGN
      musz = FALSE
      skrivut = FALSE.  
   END. 
   ELSE DO:    
      IF skrivut = TRUE THEN RUN SKRIVVAL.W (INPUT FALSE).
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE
         skrivut = FALSE.  
      END.        
      ELSE DO:
         {AVBGOM.I}
         RUN VGENRAPP.W.         
      END.
   END.  
   {AVBFRAM.I} 
   {musarrow.i}              
   skrivut = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

