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
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonrrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE valanlrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE tillrec AS RECID NO-UNDO.    
DEFINE NEW SHARED VARIABLE omrrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valmanad AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE utomr AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE omr AS LOGICAL NO-UNDO. 
/*DEFINE NEW SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO.*/
DEFINE NEW SHARED VARIABLE kto AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE typ_val AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE akval AS INTEGER NO-UNDO.  

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE aorec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnrlog AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valnamn AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE datvar AS DATE NO-UNDO. 
DEFINE NEW SHARED VARIABLE omrbildvar AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE enrvar AS CHARACTER NO-UNDO.


DEFINE SHARED VARIABLE lista AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.          
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE franar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE VARIABLE rapptyp AS INTEGER NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
{DEPSEKTEMP.I}
{LTRPTEMP.I}
{SPECMTRLTEMP.I}

{OMRTEMPW.I}

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{BESTKUNDALLT.I}
{DIRDEF.I}
{ANSPROJBER.I}
{ARBATE.I}
{AVDTEMP.I}
DEFINE NEW SHARED TEMP-TABLE visa NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
DEFINE NEW SHARED TEMP-TABLE aoval NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD AONRREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_UPP BTN_VISA RAD_VAL CMB_ARTAL BTN_NVE ~
BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP RAD_VAL CMB_ARTAL FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT FILL-IN-ENR 

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
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-nummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 54.63 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Årtal", 1,
"Period", 2
     SIZE 9 BY 3.25 NO-UNDO.

DEFINE VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 48 BY 11 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     SEL_UPP AT ROW 3.25 COL 1.5 NO-LABEL
     BTN_VISA AT ROW 8 COL 52.5
     RAD_PERIOD AT ROW 10.25 COL 2 NO-LABEL
     RAD_VAL AT ROW 14.5 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 14.75 COL 10.75
     BTN_NVE AT ROW 16 COL 27.13
     BTN_NVE-2 AT ROW 16 COL 46.75
     FILL-IN-STARTDAT AT ROW 16.5 COL 15.75 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 16.5 COL 34.25 COLON-ALIGNED
     BTN_FVE AT ROW 17 COL 27.13
     BTN_FVE-2 AT ROW 17 COL 46.75
     FILL-IN-ENR AT ROW 18 COL 15.75 COLON-ALIGNED
     BTN_AVB AT ROW 18 COL 52.5
     "Rapporter:" VIEW-AS TEXT
          SIZE 22 BY 1.21 AT ROW 1.75 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 66.25 BY 18.46.


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
         TITLE              = "Window 2"
         HEIGHT             = 18.83
         WIDTH              = 66.5
         MAX-HEIGHT         = 23.58
         MAX-WIDTH          = 83
         VIRTUAL-HEIGHT     = 23.58
         VIRTUAL-WIDTH      = 83
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
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-ENR IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME FRAME-B
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PERIOD:HIDDEN IN FRAME FRAME-B           = TRUE.

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


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE WINDOW-2
ON CHOOSE OF BTN_FVE IN FRAME FRAME-B /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 WINDOW-2
ON CHOOSE OF BTN_FVE-2 IN FRAME FRAME-B /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE WINDOW-2
ON CHOOSE OF BTN_NVE IN FRAME FRAME-B /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 WINDOW-2
ON CHOOSE OF BTN_NVE-2 IN FRAME FRAME-B /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-2
ON CHOOSE OF BTN_VISA IN FRAME FRAME-B /* Visa */
DO:
   {muswait.i}  
   RUN visa_UI.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL WINDOW-2
ON LEAVE OF CMB_ARTAL IN FRAME FRAME-B /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL WINDOW-2
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME FRAME-B /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-2
ON LEAVE OF FILL-IN-ENR IN FRAME FRAME-B /* E-nummer */
DO:
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
      IF SUBSTRING(FILL-IN-ENR,1,1) NE "E" THEN FILL-IN-ENR = "E" + FILL-IN-ENR.                                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT WINDOW-2
ON LEAVE OF FILL-IN-STARTDAT IN FRAME FRAME-B /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME FRAME-B /* Från */
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


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT WINDOW-2
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME FRAME-B /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME FRAME-B /* Till */
DO:
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w.
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-2
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-B
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN      
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   ELSE DO:
      ASSIGN
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   {muswait.i}  
   RUN visa_UI.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-2
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.
   IF SEL_UPP = "Depå - Uttag/Returer" OR SEL_UPP = "Depå - Returer" OR SEL_UPP = Guru.Konstanter:genk + " - Inköp/Uttag/Returer" OR SEL_UPP = "Omsättning" OR SEL_UPP = "Ökade / Minskade saldon" 
   OR SEL_UPP = Guru.Konstanter:genk + " - Returer" THEN DO:
      RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN      
         CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      END.
      ELSE DO:
         ASSIGN
         CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.      
      IF SEL_UPP = Guru.Konstanter:genk + " - Inköp/Uttag/Returer" OR SEL_UPP = "Omsättning" OR SEL_UPP = Guru.Konstanter:genk + " - Returer" THEN DO:
         FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.      
      ELSE DO:
         FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   ELSE DO:      
      ASSIGN
      RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF SEL_UPP = Guru.Konstanter:genk + "/FIFO" THEN DO:
         FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
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
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.    
   ASSIGN WINDOW-2:TITLE = "Rapporter för depå - " + depatemp.BENAMNING.     
   
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN      
      status-ok = SEL_UPP:ADD-LAST("Lista E-nummer - priser")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Volymvärde")IN FRAME {&FRAME-NAME}      
      status-ok = SEL_UPP:ADD-LAST("Kostnadsminimerad beställningskvant.")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Inventerat lager")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gaok + "/Kund - Uttag/Returer")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Depå - Uttag/Returer")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Depå - Returer")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Saldo - beställningspunkt")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Värde på lager")IN FRAME {&FRAME-NAME}. 
      status-ok = SEL_UPP:ADD-LAST("Värde på lager i EXCEL")IN FRAME {&FRAME-NAME}. 
      status-ok = SEL_UPP:ADD-LAST("Ökade / Minskade saldon")IN FRAME {&FRAME-NAME}.
      status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:genk + " - Inköp/Uttag/Returer")IN FRAME {&FRAME-NAME}.
      status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:genk + " - Returer")IN FRAME {&FRAME-NAME}.
      status-ok = SEL_UPP:ADD-LAST("Byt " + Guru.Konstanter:genk + " X till Y")IN FRAME {&FRAME-NAME}.
      /*kalmar depåändring*/
      IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:
         status-ok = SEL_UPP:ADD-LAST("Ändrade priser")IN FRAME {&FRAME-NAME}.
         status-ok = SEL_UPP:ADD-LAST("Kontroll av depå")IN FRAME {&FRAME-NAME}.
      END.   
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         status-ok = SEL_UPP:ADD-LAST("Omsättning")IN FRAME {&FRAME-NAME}.         
         status-ok = SEL_UPP:ADD-LAST("Ändrade priser")IN FRAME {&FRAME-NAME}.
         status-ok = SEL_UPP:ADD-LAST("Kontroll av depå")IN FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
            status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gaok + " - Inköp")IN FRAME {&FRAME-NAME}.
         END.           
      END.
      IF Guru.Konstanter:varforetypval[54] = 1 THEN DO:
         status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:genk + "/FIFO")IN FRAME {&FRAME-NAME}.
         status-ok = SEL_UPP:ADD-LAST("Alla FIFO-poster i lager")IN FRAME {&FRAME-NAME}.
      END.   
   END.
   ELSE DO:
      ASSIGN
      status-ok = SEL_UPP:ADD-LAST("Lista E-nummer - priser")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Volymvärde")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Materielomsättning/E-nummer")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Inventerat eller teoretiskt lager")IN FRAME {&FRAME-NAME}
      status-ok = SEL_UPP:ADD-LAST("Kostnadsminimerad beställningskvant.")IN FRAME {&FRAME-NAME}. 
   END.
   ASSIGN
   RAD_VAL = 1
   FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY))
   FILL-IN-STOPPDAT = DATE(12,31,YEAR(TODAY)).   
   ASSIGN  
   uppar = YEAR(TODAY) - 5      
   slutar = YEAR(TODAY).   
   /*LADDAR ÅR I CMB_ARTAL*/
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")  
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(slutar,"9999")   
   CMB_ARTAL = INPUT CMB_ARTAL.
   RUN enable_UI.
   ASSIGN
   RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   ENABLE FILL-IN-ENR WITH FRAME {&FRAME-NAME}.
   FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   {FRMSIZE.I}   
   {musarrow.i}  
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY SEL_UPP RAD_VAL CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT 
          FILL-IN-ENR 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE SEL_UPP BTN_VISA RAD_VAL CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 BTN_AVB 
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
   IF SEL_UPP = "Lista E-nummer - priser" THEN DO:       
      RUN ENRRAPPV.W.    
   END.   
   ELSE IF SEL_UPP = "Volymvärde" THEN DO: 
      RUN VOLRAPPV.W.         
   END.   
   ELSE IF SEL_UPP = "Materielomsättning/E-nummer" THEN DO:
      RUN OMSRAPPV.W.      
   END.   
   ELSE IF SEL_UPP = "Inventerat eller teoretiskt lager" THEN DO:
      RUN INVRAPPV.W.     
   END.   
   ELSE IF SEL_UPP = "Kostnadsminimerad beställningskvant." THEN DO:
      RUN KOSTRAPPV.W.      
   END.    
   ELSE IF SEL_UPP = Guru.Konstanter:gaok + "/Kund - Uttag/Returer" THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE avdtemp).
      END.
      ELSE DO:
         RUN AVSKAP.P 
         (OUTPUT TABLE avdtemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ARBARTS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE aarttemp).
      END.
      ELSE DO:
         RUN ARBARTS.P 
         (OUTPUT TABLE aarttemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ANSBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      ELSE DO:
         RUN ANSBER.P 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      {OMRHMT.I}     
      bestvad = 2.
      {BESTHMT.I}
      lista = TRUE.
      akval = 1. 
      FOR EACH uppvaltemp:
         DELETE uppvaltemp.
      END.
      FOR EACH visa:
         DELETE visa.
      END.
      vallista = 40.
      CREATE visa.
      ASSIGN
      visa.ORDNING = 40
      visa.UPPFOLJVAL = 40
      visa.KUURVAL = TRUE
      visa.DELNRKOLL = TRUE
      visa.TYP = "U300"
      visa.UT = SEL_UPP.
      CREATE uppvaltemp.
      ASSIGN         
      uppvaltemp.ENDBEST = ?      
      uppvaltemp.DELNRKOLL = FALSE
      uppvaltemp.VISPERAR = TRUE
      uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
      uppvaltemp.SLUTDATUM  = TODAY
      uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
      uppvaltemp.AVSLUTSLUT = TODAY
      uppvaltemp.TILLFALLFAST = 1
      SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
      uppvaltemp.BEREDARE = "ALLA"
      uppvaltemp.ARBANSVARIG = "ALLA"
      uppvaltemp.BESTNAMN = "ALLA"
      uppvaltemp.OMRNAMN = "ALLA"
      uppvaltemp.AVDNAMN = "ALLA"
      uppvaltemp.AVDNR = "ALLA"
      uppvaltemp.FAKTTYP = "ALLA"
      uppvaltemp.PAAV = 1
      uppvaltemp.MANUPPD = FALSE
      uppvaltemp.VALDLISTA = visa.UT. 
      RUN MDPKUURVALU.W 
      (INPUT-OUTPUT vallista ,
       OUTPUT aonummer ,
       OUTPUT delnummer ,
       INPUT-OUTPUT TABLE uppvaltemp, 
       INPUT-OUTPUT TABLE bestkundallt,
       INPUT-OUTPUT TABLE omrtemp, 
       INPUT-OUTPUT TABLE avdtemp).
      /*
       INPUT-OUTPUT TABLE valdaao,
       INPUT-OUTPUT TABLE utsokaonr).     
       */
        musz = FALSE.
      lista = FALSE.     
   END.  
   ELSE IF SEL_UPP = "Depå - Uttag/Returer" THEN DO:
      enrvar = ?.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN
         avdatum = DATE(01,01,CMB_ARTAL)
         bdatum = DATE(12,31,CMB_ARTAL).      
      END.
      ELSE DO:
         ASSIGN
         avdatum = FILL-IN-STARTDAT
         bdatum = FILL-IN-STOPPDAT.      
      END.
      IF avdatum > bdatum THEN DO:
         MESSAGE "Startdatum måste vara större eller lika med slutdatum."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
      RUN EXCUTREV.P (INPUT 1).
   END.
   ELSE IF SEL_UPP = "Depå - Returer" THEN DO:
      enrvar = ?.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN
         avdatum = DATE(01,01,CMB_ARTAL)
         bdatum = DATE(12,31,CMB_ARTAL).      
      END.
      ELSE DO:
         ASSIGN
         avdatum = FILL-IN-STARTDAT
         bdatum = FILL-IN-STOPPDAT.      
      END.
      IF avdatum > bdatum THEN DO:
         MESSAGE "Startdatum måste vara större eller lika med slutdatum."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
      RUN EXCUTREV.P (INPUT 2).
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:genk + " - Inköp/Uttag/Returer" OR SEL_UPP = Guru.Konstanter:genk + " - Returer" THEN DO:
      ASSIGN 
      enrvar = FILL-IN-ENR.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN
         avdatum = DATE(01,01,CMB_ARTAL)
         bdatum = DATE(12,31,CMB_ARTAL).      
      END.
      ELSE DO:
         ASSIGN
         avdatum = FILL-IN-STARTDAT
         bdatum = FILL-IN-STOPPDAT.      
      END.
      IF avdatum > bdatum THEN DO:
         MESSAGE "Startdatum måste vara större eller lika med slutdatum."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
      IF SEL_UPP = Guru.Konstanter:genk + " - Inköp/Uttag/Returer"  THEN RUN EXCUTRE2V.P (INPUT 1).
      IF SEL_UPP = Guru.Konstanter:genk + " - Returer" THEN RUN EXCUTRE2V.P (INPUT 3).
      
   END. 
   ELSE IF SEL_UPP = "Omsättning" THEN DO:
      enrvar = FILL-IN-ENR.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN
         avdatum = DATE(01,01,CMB_ARTAL)
         bdatum = DATE(12,31,CMB_ARTAL).      
      END.
      ELSE DO:
         ASSIGN
         avdatum = FILL-IN-STARTDAT
         bdatum = FILL-IN-STOPPDAT.      
      END.
      IF avdatum > bdatum THEN DO:
         MESSAGE "Startdatum måste vara större eller lika med slutdatum."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
      RUN EXCUTRE2V.P (INPUT 2).
   END. 
   ELSE IF SEL_UPP = "Saldo - beställningspunkt" THEN DO:
      lista = TRUE.
      RUN SALDRAPPV.W. 
      lista = FALSE.     
   END. 
   ELSE IF SEL_UPP = "Värde på lager" THEN DO:
      lista = TRUE.
      RUN VARDRAPPV.W. 
      lista = FALSE.     
   END.         
   ELSE IF SEL_UPP = Guru.Konstanter:genk + "/FIFO" THEN DO:  
      enrvar = FILL-IN-ENR.
      RUN VISFIFO.P.       
   END.
   ELSE IF SEL_UPP = "Alla FIFO-poster i lager" THEN DO:        
      RUN FIFOVIS.W (INPUT vald_depa).       
   END.     
   ELSE IF SEL_UPP = "Värde på lager i EXCEL" THEN DO:      
      RUN VARDEXCELV.P.       
   END.
   ELSE IF SEL_UPP = "Ökade / Minskade saldon" THEN DO:
      IF RAD_VAL = 1 THEN DO:
         ASSIGN
         avdatum = DATE(01,01,CMB_ARTAL)
         bdatum = DATE(12,31,CMB_ARTAL).      
      END.
      ELSE DO:
         ASSIGN
         avdatum = FILL-IN-STARTDAT
         bdatum = FILL-IN-STOPPDAT.      
      END.
      IF avdatum > bdatum THEN DO:
         MESSAGE "Startdatum måste vara större eller lika med slutdatum."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.      
      RUN SVINRAPPV.W.      
   END.  
   ELSE IF SEL_UPP = "Inventerat lager" THEN DO:      
      RUN INVGKALV.W.      
   END.  
   ELSE IF SEL_UPP = "Byt " + Guru.Konstanter:genk + " X till Y" THEN DO:      
      RUN DEPXTILLY.W.      
   END.  
   ELSE IF SEL_UPP = "Ändrade priser" THEN DO:      
      RUN PRISRAPPV.W.       
   END.
   ELSE IF SEL_UPP = "Kontroll av depå" THEN DO:      
      RUN KATKOLLV2.W.       
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gaok + " - Inköp" THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE avdtemp).
      END.
      ELSE DO:
         RUN AVSKAP.P 
         (OUTPUT TABLE avdtemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ARBARTS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE aarttemp).
      END.
      ELSE DO:
         RUN ARBARTS.P 
         (OUTPUT TABLE aarttemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ANSBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      ELSE DO:
         RUN ANSBER.P 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      {OMRHMT.I}     
      bestvad = 2.
      {BESTHMT.I}
      lista = TRUE.
      akval = 1. 
      FOR EACH uppvaltemp:
         DELETE uppvaltemp.
      END.
      FOR EACH visa:
         DELETE visa.
      END.
      vallista = 40.
      CREATE visa.
      ASSIGN
      visa.ORDNING = 40
      visa.UPPFOLJVAL = 40
      visa.KUURVAL = TRUE
      visa.DELNRKOLL = TRUE
      visa.TYP = "U300"
      visa.UT = SEL_UPP.
      CREATE uppvaltemp.
      ASSIGN         
      uppvaltemp.ENDBEST = ?      
      uppvaltemp.DELNRKOLL = FALSE
      uppvaltemp.VISPERAR = TRUE
      uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
      uppvaltemp.SLUTDATUM  = TODAY
      uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
      uppvaltemp.AVSLUTSLUT = TODAY
      uppvaltemp.TILLFALLFAST = 1
      SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
      uppvaltemp.BEREDARE = "ALLA"
      uppvaltemp.ARBANSVARIG = "ALLA"
      uppvaltemp.BESTNAMN = "ALLA"
      uppvaltemp.OMRNAMN = "ALLA"
      uppvaltemp.AVDNAMN = "ALLA"
      uppvaltemp.AVDNR = "ALLA"
      uppvaltemp.FAKTTYP = "ALLA"
      uppvaltemp.PAAV = 1
      uppvaltemp.MANUPPD = FALSE
      uppvaltemp.VALDLISTA = visa.UT.            
      RUN MDPKUURVALU.W 
      (INPUT-OUTPUT vallista ,
       OUTPUT aonummer ,
       OUTPUT delnummer ,
       INPUT-OUTPUT TABLE uppvaltemp, 
       INPUT-OUTPUT TABLE bestkundallt,
       INPUT-OUTPUT TABLE omrtemp, 
       INPUT-OUTPUT TABLE avdtemp).
      /*
       INPUT-OUTPUT TABLE valdaao,
       INPUT-OUTPUT TABLE utsokaonr).     
       */
        musz = FALSE.
      lista = FALSE.     
   END.  
   {AVBFRAM.I}
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

