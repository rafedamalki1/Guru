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

  Created: 95/09/09 - 10:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW  
{RESDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define NEW NEW 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regdatumspar2 AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MAN FILL-IN-TIS FILL-IN-ONS ~
FILL-IN-TOR FILL-IN-FRE FILL-IN-LOR FILL-IN-SON BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-MAN FILL-IN-KRM FILL-IN-TIS ~
FILL-IN-KRTI FILL-IN-ONS FILL-IN-KRO FILL-IN-TOR FILL-IN-KRTO FILL-IN-FRE ~
FILL-IN-KRFR FILL-IN-LOR FILL-IN-KRL FILL-IN-SON FILL-IN-KRS 

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

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-FRE AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "fre" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRFR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRM AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRTI AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRTO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOR AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "lör" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAN AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "mån" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ONS AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "ons" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SON AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "sön" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIS AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "tis" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOR AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "tor" 
     VIEW-AS FILL-IN 
     SIZE 9.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VNR AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Vecka" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-VNR AT ROW 1.75 COL 20 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 1.88 COL 9.88 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 1.88 COL 28.63 COLON-ALIGNED
     FILL-IN-MAN AT ROW 4.5 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRM AT ROW 4.5 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-TIS AT ROW 5.67 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRTI AT ROW 5.67 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-ONS AT ROW 6.92 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRO AT ROW 6.92 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-TOR AT ROW 8.08 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRTO AT ROW 8.08 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-FRE AT ROW 9.33 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRFR AT ROW 9.33 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-LOR AT ROW 10.42 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRL AT ROW 10.42 COL 18 COLON-ALIGNED NO-LABEL
     FILL-IN-SON AT ROW 11.67 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-KRS AT ROW 11.67 COL 18 COLON-ALIGNED NO-LABEL
     BTN_REG AT ROW 13.17 COL 10.63
     BTN_AVS AT ROW 13.17 COL 25.63
     "text" VIEW-AS TEXT
          SIZE 31.38 BY 1 AT ROW 3 COL 6.5
     SPACE(2.61) SKIP(10.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Havda kostnader":L.


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

/* SETTINGS FOR FILL-IN FILL-IN-KRFR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRO IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRTI IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KRTO IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SLUTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VNR IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Havda kostnader */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Havda kostnader */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:                                 
   ASSIGN
   FILL-IN-FRE = INPUT FILL-IN-FRE 
   FILL-IN-LOR = INPUT FILL-IN-LOR
   FILL-IN-MAN = INPUT FILL-IN-MAN 
   FILL-IN-ONS = INPUT FILL-IN-ONS 
   FILL-IN-SON = INPUT FILL-IN-SON 
   FILL-IN-TIS = INPUT FILL-IN-TIS 
   FILL-IN-TOR = INPUT FILL-IN-TOR.
   regdatum = regdatumspar2.
   REPEAT:
      IF regdatum > avdatum THEN DO:
         musz = TRUE.
         LEAVE.
      END.
      CREATE kosters.             
      IF WEEKDAY(regdatum) = 1 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-SON
         kosters.MDAG = "sön"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
         regdatumspar2 = regdatum + 1.
         IF regdatum = avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.
         ELSE LEAVE.
      END.
      IF WEEKDAY(regdatum) = 2 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-MAN
         kosters.MDAG = "mån"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 3 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-TIS
         kosters.MDAG = "tis"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 4 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-ONS
         kosters.MDAG = "ons"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 5 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-TOR
         kosters.MDAG = "tor"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 6 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-FRE
         kosters.MDAG = "fre"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 7 THEN DO:
         ASSIGN kosters.MPERSONALKOD = pkod
         kosters.KR = FILL-IN-LOR
         kosters.MDAG = "lör"
         kosters.MVECKONUMMER = FILL-IN-VNR
         kosters.MDATUM = regdatum.
      END.
      regdatum = regdatum + 1.
   END.
   IF musz = FALSE THEN DO:      
      regdatum = regdatumspar.
      RUN REGVEC.P.
      ASSIGN
      FILL-IN-VNR = regvnr
      FILL-IN-MAN = 0 
      FILL-IN-TIS = 0 
      FILL-IN-ONS = 0
      FILL-IN-TOR = 0
      FILL-IN-FRE = 0
      FILL-IN-LOR = 0
      FILL-IN-SON = 0
      FILL-IN-KRM = "Kr" 
      FILL-IN-KRTI = "Kr" 
      FILL-IN-KRO = "Kr"
      FILL-IN-KRTO = "Kr"
      FILL-IN-KRFR = "Kr"
      FILL-IN-KRL = "Kr"
      FILL-IN-KRS = "Kr".
      DISPLAY FILL-IN-MAN FILL-IN-TIS FILL-IN-ONS FILL-IN-TOR FILL-IN-FRE FILL-IN-LOR
      FILL-IN-SON WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-MAN:HIDDEN = TRUE 
      FILL-IN-TIS:HIDDEN = TRUE 
      FILL-IN-ONS:HIDDEN = TRUE
      FILL-IN-TOR:HIDDEN = TRUE
      FILL-IN-FRE:HIDDEN = TRUE
      FILL-IN-LOR:HIDDEN = TRUE
      FILL-IN-SON:HIDDEN = TRUE
      FILL-IN-KRM:HIDDEN = TRUE 
      FILL-IN-KRTI:HIDDEN = TRUE 
      FILL-IN-KRO:HIDDEN = TRUE
      FILL-IN-KRTO:HIDDEN = TRUE
      FILL-IN-KRFR:HIDDEN = TRUE
      FILL-IN-KRL:HIDDEN = TRUE
      FILL-IN-KRS:HIDDEN = TRUE.
      REPEAT:
         IF regdatum > avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.      
         IF WEEKDAY(regdatum) = 1 THEN DO:
            ASSIGN
            FILL-IN-SON:HIDDEN = FALSE
            FILL-IN-KRS:HIDDEN = FALSE.
            regdatumspar = regdatum + 1.
            IF regdatum = avdatum THEN  DO:
               musz = TRUE.
               LEAVE.
            END.   
            ELSE LEAVE.
         END.
         IF WEEKDAY(regdatum) = 2 THEN DO:
            ASSIGN
            FILL-IN-MAN:HIDDEN = FALSE
            FILL-IN-KRM:HIDDEN = FALSE.
         END.
         IF WEEKDAY(regdatum) = 3 THEN DO:
            ASSIGN
            FILL-IN-TIS:HIDDEN = FALSE
            FILL-IN-KRTI:HIDDEN = FALSE.
         END.
         IF WEEKDAY(regdatum) = 4 THEN DO:
            ASSIGN
            FILL-IN-ONS:HIDDEN = FALSE
            FILL-IN-KRO:HIDDEN = FALSE.
         END.
         IF WEEKDAY(regdatum) = 5 THEN DO:
            ASSIGN
            FILL-IN-TOR:HIDDEN = FALSE
            FILL-IN-KRTO:HIDDEN = FALSE.
         END.
         IF WEEKDAY(regdatum) = 6 THEN DO:
            ASSIGN
            FILL-IN-FRE:HIDDEN = FALSE
            FILL-IN-KRFR:HIDDEN = FALSE.
         END. 
         IF WEEKDAY(regdatum) = 7 THEN DO:
            ASSIGN
            FILL-IN-LOR:HIDDEN = FALSE
            FILL-IN-KRL:HIDDEN = FALSE.
         END.
         regdatum = regdatum + 1.
      END.
      IF gvisatidpermanad = TRUE THEN DO:                        
         FILL-IN-STARTDAT = FILL-IN-SLUTDAT + 1.
         FILL-IN-SLUTDAT = FILL-IN-STARTDAT + 6.
         IF FILL-IN-STARTDAT > avdatum THEN FILL-IN-STARTDAT = avdatum. 
         IF FILL-IN-SLUTDAT > avdatum THEN FILL-IN-SLUTDAT = avdatum.
         DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         DISPLAY FILL-IN-VNR WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      musz = FALSE.
      APPLY "GO" TO BTN_REG.
   END.
   musz = FALSE.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRE DIALOG-1
ON LEAVE OF FILL-IN-FRE IN FRAME DIALOG-1 /* fre */
DO:
  FILL-IN-FRE = INPUT FILL-IN-FRE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LOR DIALOG-1
ON LEAVE OF FILL-IN-LOR IN FRAME DIALOG-1 /* lör */
DO:
  FILL-IN-LOR = INPUT FILL-IN-LOR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MAN DIALOG-1
ON LEAVE OF FILL-IN-MAN IN FRAME DIALOG-1 /* mån */
DO:
  FILL-IN-MAN = FILL-IN-MAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ONS DIALOG-1
ON LEAVE OF FILL-IN-ONS IN FRAME DIALOG-1 /* ons */
DO:
  FILL-IN-ONS = INPUT FILL-IN-ONS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* till */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* till */
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


&Scoped-define SELF-NAME FILL-IN-SON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SON DIALOG-1
ON LEAVE OF FILL-IN-SON IN FRAME DIALOG-1 /* sön */
DO:
  FILL-IN-SON = INPUT FILL-IN-SON.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Från */
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


&Scoped-define SELF-NAME FILL-IN-TIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TIS DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TIS IN FRAME DIALOG-1 /* tis */
DO:
  FILL-IN-TIS = FILL-IN-TIS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOR DIALOG-1
ON LEAVE OF FILL-IN-TOR IN FRAME DIALOG-1 /* tor */
DO:
  FILL-IN-TOR = INPUT FILL-IN-TOR.
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
   FOR EACH kosters:
      DELETE kosters.
   END.   
   regdatum = bdatum.
   regdatumspar2 = bdatum.
   RUN REGVEC.P.
   ASSIGN
   FILL-IN-VNR = regvnr
   FILL-IN-MAN = 0 
   FILL-IN-TIS = 0
   FILL-IN-ONS = 0
   FILL-IN-TOR = 0
   FILL-IN-FRE = 0
   FILL-IN-LOR = 0
   FILL-IN-SON = 0
   FILL-IN-KRM = "Kr" 
   FILL-IN-KRTI = "Kr"
   FILL-IN-KRO = "Kr"
   FILL-IN-KRTO = "Kr"
   FILL-IN-KRFR = "Kr"
   FILL-IN-KRL = "Kr"
   FILL-IN-KRS = "Kr".
   RUN enable_UI.       
   {FRMSIZED.I}
   ASSIGN 
   FILL-IN-MAN:HIDDEN = TRUE 
   FILL-IN-TIS:HIDDEN = TRUE
   FILL-IN-ONS:HIDDEN = TRUE
   FILL-IN-TOR:HIDDEN = TRUE
   FILL-IN-FRE:HIDDEN = TRUE
   FILL-IN-LOR:HIDDEN = TRUE
   FILL-IN-SON:HIDDEN = TRUE
   FILL-IN-KRM:HIDDEN = TRUE 
   FILL-IN-KRTI:HIDDEN = TRUE
   FILL-IN-KRO:HIDDEN = TRUE
   FILL-IN-KRTO:HIDDEN = TRUE
   FILL-IN-KRFR:HIDDEN = TRUE
   FILL-IN-KRL:HIDDEN = TRUE
   FILL-IN-KRS:HIDDEN = TRUE.
   REPEAT:
      IF regdatum > avdatum THEN LEAVE.
      IF WEEKDAY(regdatum) = 1 THEN DO:
         ASSIGN
         FILL-IN-SON:HIDDEN = FALSE
         FILL-IN-KRS:HIDDEN = FALSE.
         regdatumspar = regdatum + 1.
         LEAVE.
      END.
      IF WEEKDAY(regdatum) = 2 THEN DO:
         ASSIGN
         FILL-IN-MAN:HIDDEN = FALSE 
         FILL-IN-KRM:HIDDEN = FALSE.
      END.
      IF WEEKDAY(regdatum) = 3 THEN DO:
         ASSIGN
         FILL-IN-TIS:HIDDEN = FALSE
         FILL-IN-KRTI:HIDDEN = FALSE.
      END.
      IF WEEKDAY(regdatum) = 4 THEN DO:
         ASSIGN
         FILL-IN-ONS:HIDDEN = FALSE
         FILL-IN-KRO:HIDDEN = FALSE.
      END.
      IF WEEKDAY(regdatum) = 5 THEN DO:
         ASSIGN
         FILL-IN-TOR:HIDDEN = FALSE
         FILL-IN-KRTO:HIDDEN = FALSE.
      END.
      IF WEEKDAY(regdatum) = 6 THEN DO:
         ASSIGN
         FILL-IN-FRE:HIDDEN = FALSE
         FILL-IN-KRFR:HIDDEN = FALSE.
      END.
      IF WEEKDAY(regdatum) = 7 THEN DO:
         ASSIGN
         FILL-IN-LOR:HIDDEN = FALSE
         FILL-IN-KRL:HIDDEN = FALSE.
      END.
      regdatum = regdatum + 1.
   END.
   IF gvisatidpermanad = TRUE THEN DO:      
      IF WEEKDAY(bdatum) = 1 THEN DO:
         FILL-IN-SLUTDAT = bdatum.
      END.
      IF WEEKDAY(bdatum) = 2 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 6.
      END.
      IF WEEKDAY(bdatum) = 3 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 5.
      END.
      IF WEEKDAY(bdatum) = 4 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 4.
      END.
      IF WEEKDAY(bdatum) = 5 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 3.
      END.
      IF WEEKDAY(bdatum) = 6 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 2.
      END.
      IF WEEKDAY(bdatum) = 7 THEN DO:
         FILL-IN-SLUTDAT = bdatum + 1.
      END.
      FILL-IN-STARTDAT = bdatum.
      IF FILL-IN-SLUTDAT > avdatum THEN FILL-IN-SLUTDAT = avdatum. 
      DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      DISPLAY FILL-IN-VNR WITH FRAME {&FRAME-NAME}.
   END.
   regdatumspar = regdatum + 1.       
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
  DISPLAY FILL-IN-MAN FILL-IN-KRM FILL-IN-TIS FILL-IN-KRTI FILL-IN-ONS 
          FILL-IN-KRO FILL-IN-TOR FILL-IN-KRTO FILL-IN-FRE FILL-IN-KRFR 
          FILL-IN-LOR FILL-IN-KRL FILL-IN-SON FILL-IN-KRS 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-MAN FILL-IN-TIS FILL-IN-ONS FILL-IN-TOR FILL-IN-FRE 
         FILL-IN-LOR FILL-IN-SON BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

