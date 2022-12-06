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
/* Local Variable Definitions ---                */
{ALLDEF.I}
&Scoped-define NEW  
{RESDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
{PHMT.I}
&Scoped-define NEW NEW 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regdatumspar2 AS DATE NO-UNDO.
DEFINE VARIABLE allmat AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejand AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM ~
FILL-IN-TISF FILL-IN-TISL FILL-IN-TISM FILL-IN-ONSF FILL-IN-ONSL ~
FILL-IN-ONSM FILL-IN-TORF FILL-IN-TORL FILL-IN-TORM FILL-IN-FREF ~
FILL-IN-FREL FILL-IN-FREM FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM ~
FILL-IN-SONF FILL-IN-SONL FILL-IN-SONM BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-RUBRIK FILL-IN-PKOD ~
FILL-IN_FORNAMN-2 FILL-IN-FR FILL-IN-LU FILL-IN-MI ED_KOSTINFO FILL-IN-MANF ~
FILL-IN-MANL FILL-IN-MANM FILL-IN-TISF FILL-IN-TISL FILL-IN-TISM ~
FILL-IN-ONSF FILL-IN-ONSL FILL-IN-ONSM FILL-IN-TORF FILL-IN-TORL ~
FILL-IN-TORM FILL-IN-FREF FILL-IN-FREL FILL-IN-FREM FILL-IN-LORF ~
FILL-IN-LORL FILL-IN-LORM FILL-IN-SONF FILL-IN-SONL FILL-IN-SONM 

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
     LABEL "Fortsätt":L 
     SIZE 14 BY 1.

DEFINE VARIABLE ED_KOSTINFO AS CHARACTER INITIAL "Frukost förmånsbeskattas aldrig om inte samtliga måltider den dagen varit gratis, därför kan din frukostregistrering komma att ändras utifrån de måltider du bjudits på och tjänsteresans längd" 
     VIEW-AS EDITOR
     SIZE 30 BY 6
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FILL-IN-FR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FREF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "fre" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FREL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FREM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "lör" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORM AS LOGICAL FORMAT "Ja /Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LU AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "mån" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MI AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ONSF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "ons" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ONSL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ONSM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SONF AS LOGICAL FORMAT "Ja /Nej":U INITIAL NO 
     LABEL "sön" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SONL AS LOGICAL FORMAT "Ja /Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SONM AS LOGICAL FORMAT "Ja /Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TISF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "tis" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TISL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TISM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TORF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "tor" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TORL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TORM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VNR AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Vecka" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-RUBRIK AT ROW 1.67 COL 4.13 COLON-ALIGNED NO-LABEL
     FILL-IN-PKOD AT ROW 3.17 COL 12 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 3.17 COL 22.5 NO-LABEL
     FILL-IN-VNR AT ROW 4.46 COL 19.25 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 4.58 COL 9.13 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 4.58 COL 27.75 COLON-ALIGNED
     FILL-IN-FR AT ROW 5.75 COL 5 COLON-ALIGNED NO-LABEL
     FILL-IN-LU AT ROW 5.75 COL 16.63 COLON-ALIGNED NO-LABEL
     FILL-IN-MI AT ROW 5.75 COL 29.88 COLON-ALIGNED NO-LABEL
     ED_KOSTINFO AT ROW 5.88 COL 43 NO-LABEL WIDGET-ID 8
     FILL-IN-MANF AT ROW 7.21 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-MANL AT ROW 7.21 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-MANM AT ROW 7.21 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-TISF AT ROW 8.38 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-TISL AT ROW 8.38 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-TISM AT ROW 8.38 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-ONSF AT ROW 9.63 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-ONSL AT ROW 9.63 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-ONSM AT ROW 9.63 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-TORF AT ROW 10.79 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-TORL AT ROW 10.79 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-TORM AT ROW 10.79 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-FREF AT ROW 12.04 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-FREL AT ROW 12.04 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-FREM AT ROW 12.04 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-LORF AT ROW 13.21 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-LORL AT ROW 13.21 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-LORM AT ROW 13.21 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-SONF AT ROW 14.38 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-SONL AT ROW 14.38 COL 20 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     FILL-IN-SONM AT ROW 14.38 COL 32 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     BTN_REG AT ROW 16.08 COL 41.13
     BTN_AVS AT ROW 16.08 COL 56.13
     SPACE(4.11) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kostförmån":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR ED_KOSTINFO IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LU IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MI IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-RUBRIK IN FRAME DIALOG-1
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

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kostförmån */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kostförmån */
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
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Fortsätt */
DO:                                 
   ASSIGN
   FILL-IN-FREF = INPUT FILL-IN-FREF 
   FILL-IN-FREL = INPUT FILL-IN-FREL 
   FILL-IN-FREM = INPUT FILL-IN-FREM 
   FILL-IN-LORF = INPUT FILL-IN-LORF
   FILL-IN-LORL = INPUT FILL-IN-LORL 
   FILL-IN-LORM = INPUT FILL-IN-LORM 
   FILL-IN-MANF = INPUT FILL-IN-MANF 
   FILL-IN-MANL = INPUT FILL-IN-MANL 
   FILL-IN-MANM = INPUT FILL-IN-MANM 
   FILL-IN-ONSF = INPUT FILL-IN-ONSF 
   FILL-IN-ONSL = INPUT FILL-IN-ONSL 
   FILL-IN-ONSM = INPUT FILL-IN-ONSM 
   FILL-IN-SONF = INPUT FILL-IN-SONF 
   FILL-IN-SONL = INPUT FILL-IN-SONL 
   FILL-IN-SONM = INPUT FILL-IN-SONM 
   FILL-IN-TISF = INPUT FILL-IN-TISF 
   FILL-IN-TISL = INPUT FILL-IN-TISL 
   FILL-IN-TISM = INPUT FILL-IN-TISM 
   FILL-IN-TORF = INPUT FILL-IN-TORF 
   FILL-IN-TORL = INPUT FILL-IN-TORL 
   FILL-IN-TORM = INPUT FILL-IN-TORM.
   regdatum = regdatumspar2.
   REPEAT:
      IF regdatum > avdatum THEN DO:
         musz = TRUE.
         LEAVE.
      END.
      CREATE kostfil.             
      IF WEEKDAY(regdatum) = 1 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-SONF
         kostfil.MLUN = FILL-IN-SONL
         kostfil.MMID = FILL-IN-SONM
         kostfil.MDAG = "sön"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
         regdatumspar2 = regdatum + 1.
         IF regdatum = avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.
         ELSE LEAVE.
      END.
      IF WEEKDAY(regdatum) = 2 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-MANF
         kostfil.MLUN = FILL-IN-MANL
         kostfil.MMID = FILL-IN-MANM
         kostfil.MDAG = "mån"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 3 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-TISF
         kostfil.MLUN = FILL-IN-TISL
         kostfil.MMID = FILL-IN-TISM
         kostfil.MDAG = "tis"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 4 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-ONSF
         kostfil.MLUN = FILL-IN-ONSL
         kostfil.MMID = FILL-IN-ONSM
         kostfil.MDAG = "ons"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 5 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-TORF
         kostfil.MLUN = FILL-IN-TORL
         kostfil.MMID = FILL-IN-TORM
         kostfil.MDAG = "tor"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 6 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-FREF
         kostfil.MLUN = FILL-IN-FREL
         kostfil.MMID = FILL-IN-FREM
         kostfil.MDAG = "fre"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 7 THEN DO:
         ASSIGN kostfil.MPERSONALKOD = pkod
         kostfil.MFRU = FILL-IN-LORF
         kostfil.MLUN = FILL-IN-LORL
         kostfil.MMID = FILL-IN-LORM
         kostfil.MDAG = "lör"
         kostfil.MVECKONUMMER = FILL-IN-VNR
         kostfil.MDATUM = regdatum.
      END.
      regdatum = regdatum + 1.
   END. 
   IF musz = FALSE THEN DO:      
      regdatum = regdatumspar.
      REPEAT:   
         IF regdatum > avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.             
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = regdatum  NO-LOCK NO-ERROR.
         IF maltidfil.MDAG = "mån" THEN DO:
            ASSIGN
            FILL-IN-MANF = FALSE 
            FILL-IN-MANL = maltidfil.MLUN
            FILL-IN-MANM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.           
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-MANF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "tis" THEN DO:
            ASSIGN
            FILL-IN-TISF = FALSE 
            FILL-IN-TISL = maltidfil.MLUN
            FILL-IN-TISM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-TISF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "ons" THEN DO:
            ASSIGN
            FILL-IN-ONSF = FALSE 
            FILL-IN-ONSL = maltidfil.MLUN
            FILL-IN-ONSM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-ONSF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "tor" THEN DO:
            ASSIGN
            FILL-IN-TORF = FALSE 
            FILL-IN-TORL = maltidfil.MLUN
            FILL-IN-TORM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-TORF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "fre" THEN DO:
            ASSIGN
            FILL-IN-FREF = FALSE 
            FILL-IN-FREL = maltidfil.MLUN
            FILL-IN-FREM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-FREF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "lör" THEN DO:
            ASSIGN
            FILL-IN-LORF = FALSE 
            FILL-IN-LORL = maltidfil.MLUN
            FILL-IN-LORM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-LORF = maltidfil.MFRU.
                  END.
               END.
            END.
         END.
         IF maltidfil.MDAG = "sön" THEN DO:
            ASSIGN
            FILL-IN-SONF = FALSE 
            FILL-IN-SONL = maltidfil.MLUN
            FILL-IN-SONM = maltidfil.MMID
            FILL-IN-VNR = maltidfil.MVECKONUMMER.
            IF allmat = TRUE THEN DO: 
               IF regdatum = avdatum THEN regvnr = regvnr.
               ELSE DO:
                  IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                     FILL-IN-SONF = maltidfil.MFRU.
                  END.
               END.
            END.

            regdatumspar = regdatum + 1.
            IF regdatum = avdatum THEN  DO:
               musz = TRUE.
               LEAVE.
            END.   
            ELSE LEAVE.
         END.
         regdatum = regdatum + 1.
      END.         

      DISPLAY FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM FILL-IN-TISF FILL-IN-TISL 
      FILL-IN-TISM FILL-IN-ONSF FILL-IN-ONSL FILL-IN-ONSM FILL-IN-TORF FILL-IN-TORL FILL-IN-TORM 
      FILL-IN-FREF FILL-IN-FREL FILL-IN-FREM FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM FILL-IN-SONF 
      FILL-IN-SONL FILL-IN-SONM WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-MANF:HIDDEN = TRUE 
      FILL-IN-MANL:HIDDEN = TRUE
      FILL-IN-MANM:HIDDEN = TRUE
      FILL-IN-TISF:HIDDEN = TRUE 
      FILL-IN-TISL:HIDDEN = TRUE
      FILL-IN-TISM:HIDDEN = TRUE
      FILL-IN-ONSF:HIDDEN = TRUE
      FILL-IN-ONSL:HIDDEN = TRUE
      FILL-IN-ONSM:HIDDEN = TRUE
      FILL-IN-TORF:HIDDEN = TRUE
      FILL-IN-TORL:HIDDEN = TRUE
      FILL-IN-TORM:HIDDEN = TRUE
      FILL-IN-FREF:HIDDEN = TRUE
      FILL-IN-FREL:HIDDEN = TRUE
      FILL-IN-FREM:HIDDEN = TRUE
      FILL-IN-LORF:HIDDEN = TRUE
      FILL-IN-LORL:HIDDEN = TRUE
      FILL-IN-LORM:HIDDEN = TRUE
      FILL-IN-SONF:HIDDEN = TRUE
      FILL-IN-SONL:HIDDEN = TRUE
      FILL-IN-SONM:HIDDEN = TRUE.   
      regdatum = regdatumspar.
      REPEAT:   
         IF regdatum > avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.    
         FIND FIRST maltidfil WHERE maltidfil.MDATUM = regdatum  NO-LOCK NO-ERROR.
         IF maltidfil.MDAG = "mån" THEN DO:
            ASSIGN       
            FILL-IN-MANF:HIDDEN = FALSE 
            FILL-IN-MANL:HIDDEN = FALSE 
            FILL-IN-MANM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule" THEN DISABLE FILL-IN-MANF WITH FRAME {&FRAME-NAME}.            
         END.
         IF maltidfil.MDAG = "tis" THEN DO:
            ASSIGN
            FILL-IN-TISF:HIDDEN = FALSE 
            FILL-IN-TISL:HIDDEN = FALSE 
            FILL-IN-TISM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-TISF FILL-IN-TISL FILL-IN-TISM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-TISF WITH FRAME {&FRAME-NAME}.            

         END.
         IF maltidfil.MDAG = "ons" THEN DO:
            ASSIGN
            FILL-IN-ONSF:HIDDEN = FALSE 
            FILL-IN-ONSL:HIDDEN = FALSE 
            FILL-IN-ONSM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-ONSF FILL-IN-ONSL FILL-IN-ONSM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-ONSF  WITH FRAME {&FRAME-NAME}.            

         END.
         IF maltidfil.MDAG = "tor" THEN DO:
            ASSIGN
            FILL-IN-TORF:HIDDEN = FALSE 
            FILL-IN-TORL:HIDDEN = FALSE 
            FILL-IN-TORM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-TORF FILL-IN-TORL FILL-IN-TORM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-TORF WITH FRAME {&FRAME-NAME}.            
         END.
         IF maltidfil.MDAG = "fre" THEN DO:
            ASSIGN
            FILL-IN-FREF:HIDDEN = FALSE 
            FILL-IN-FREL:HIDDEN = FALSE 
            FILL-IN-FREM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-FREF FILL-IN-FREL FILL-IN-FREM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-FREF  WITH FRAME {&FRAME-NAME}.            
         END.
         IF maltidfil.MDAG = "lör" THEN DO:
            ASSIGN
            FILL-IN-LORF:HIDDEN = FALSE 
            FILL-IN-LORL:HIDDEN = FALSE 
            FILL-IN-LORM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-LORF  WITH FRAME {&FRAME-NAME}.            
         END.
         IF maltidfil.MDAG = "sön" THEN DO:
            ASSIGN
            FILL-IN-SONF:HIDDEN = FALSE 
            FILL-IN-SONL:HIDDEN = FALSE 
            FILL-IN-SONM:HIDDEN = FALSE.
            IF ejand = TRUE THEN DISABLE FILL-IN-SONF FILL-IN-SONL FILL-IN-SONM WITH FRAME {&FRAME-NAME}.            
            IF Guru.Konstanter:globforetag = "lule"  THEN DISABLE FILL-IN-SONF WITH FRAME {&FRAME-NAME}.            
            regdatumspar = regdatum + 1.
            IF regdatum = avdatum THEN  DO:
               musz = TRUE.
               LEAVE.
            END.   
            ELSE LEAVE.
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
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Fortsätt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FREF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FREF IN FRAME DIALOG-1 /* fre */
DO:
    IF INPUT FILL-IN-FREF = TRUE THEN FILL-IN-FREF = FALSE.
   IF INPUT FILL-IN-FREF = FALSE THEN FILL-IN-FREF = TRUE.
   DISPLAY FILL-IN-FREF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FREL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FREL IN FRAME DIALOG-1
DO:
    IF INPUT FILL-IN-FREL = TRUE THEN FILL-IN-FREL = FALSE.
   IF INPUT FILL-IN-FREL = FALSE THEN FILL-IN-FREL = TRUE.
   DISPLAY FILL-IN-FREL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FREM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FREM IN FRAME DIALOG-1
DO:
    IF INPUT FILL-IN-FREM = TRUE THEN FILL-IN-FREM = FALSE.
   IF INPUT FILL-IN-FREM = FALSE THEN FILL-IN-FREM = TRUE.
   DISPLAY FILL-IN-FREM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LORF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LORF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-LORF IN FRAME DIALOG-1 /* lör */
DO:
    IF INPUT FILL-IN-LORF = TRUE THEN FILL-IN-LORF = FALSE.
   IF INPUT FILL-IN-LORF = FALSE THEN FILL-IN-LORF = TRUE.
   DISPLAY FILL-IN-LORF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LORL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LORL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-LORL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-LORL = TRUE THEN FILL-IN-LORL = FALSE.
   IF INPUT FILL-IN-LORL = FALSE THEN FILL-IN-LORL = TRUE.
   DISPLAY FILL-IN-LORL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LORM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LORM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-LORM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-LORM = TRUE THEN FILL-IN-LORM = FALSE.
   IF INPUT FILL-IN-LORM = FALSE THEN FILL-IN-LORM = TRUE.
   DISPLAY FILL-IN-LORM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MANF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MANF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-MANF IN FRAME DIALOG-1 /* mån */
DO:
   IF INPUT FILL-IN-MANF = TRUE THEN FILL-IN-MANF = FALSE.
   IF INPUT FILL-IN-MANF = FALSE THEN FILL-IN-MANF = TRUE.
   DISPLAY FILL-IN-MANF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MANL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MANL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-MANL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-MANL = TRUE THEN FILL-IN-MANL = FALSE.
   IF INPUT FILL-IN-MANL = FALSE THEN FILL-IN-MANL = TRUE.
   DISPLAY FILL-IN-MANL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MANM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-MANM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-MANM = TRUE THEN FILL-IN-MANM = FALSE.
   IF INPUT FILL-IN-MANM = FALSE THEN FILL-IN-MANM = TRUE.
   DISPLAY FILL-IN-MANM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ONSF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ONSF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ONSF IN FRAME DIALOG-1 /* ons */
DO:
   IF INPUT FILL-IN-ONSF = TRUE THEN FILL-IN-ONSF = FALSE.
   IF INPUT FILL-IN-ONSF = FALSE THEN FILL-IN-ONSF = TRUE.
   DISPLAY FILL-IN-ONSF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ONSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ONSL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ONSL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-ONSL = TRUE THEN FILL-IN-ONSL = FALSE.
   IF INPUT FILL-IN-ONSL = FALSE THEN FILL-IN-ONSL = TRUE.
   DISPLAY FILL-IN-ONSL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ONSM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ONSM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ONSM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-ONSM = TRUE THEN FILL-IN-ONSM = FALSE.
   IF INPUT FILL-IN-ONSM = FALSE THEN FILL-IN-ONSM = TRUE.
   DISPLAY FILL-IN-ONSM WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME FILL-IN-SONF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SONF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-SONF IN FRAME DIALOG-1 /* sön */
DO:
   IF INPUT FILL-IN-SONF = TRUE THEN FILL-IN-SONF = FALSE.
   IF INPUT FILL-IN-SONF = FALSE THEN FILL-IN-SONF = TRUE.
   DISPLAY FILL-IN-SONF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SONL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SONL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-SONL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-SONL = TRUE THEN FILL-IN-SONL = FALSE.
   IF INPUT FILL-IN-SONL = FALSE THEN FILL-IN-SONL = TRUE.
   DISPLAY FILL-IN-SONL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SONM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SONM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-SONM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-SONM = TRUE THEN FILL-IN-SONM = FALSE.
   IF INPUT FILL-IN-SONM = FALSE THEN FILL-IN-SONM = TRUE.
   DISPLAY FILL-IN-SONM WITH FRAME {&FRAME-NAME}.
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
   regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TISF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TISF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TISF IN FRAME DIALOG-1 /* tis */
DO:
   IF INPUT FILL-IN-TISF = TRUE THEN FILL-IN-TISF = FALSE.
   IF INPUT FILL-IN-TISF = FALSE THEN FILL-IN-TISF = TRUE.
   DISPLAY FILL-IN-TISF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TISL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TISL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TISL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-TISL = TRUE THEN FILL-IN-TISL = FALSE.
   IF INPUT FILL-IN-TISL = FALSE THEN FILL-IN-TISL = TRUE.
   DISPLAY FILL-IN-TISL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TISM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TISM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TISM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-TISM = TRUE THEN FILL-IN-TISM = FALSE.
   IF INPUT FILL-IN-TISM = FALSE THEN FILL-IN-TISM = TRUE.
   DISPLAY FILL-IN-TISM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TORF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TORF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TORF IN FRAME DIALOG-1 /* tor */
DO:
   IF INPUT FILL-IN-TORF = TRUE THEN FILL-IN-TORF = FALSE.
   IF INPUT FILL-IN-TORF = FALSE THEN FILL-IN-TORF = TRUE.
   DISPLAY FILL-IN-TORF WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TORL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TORL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TORL IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-TORL = TRUE THEN FILL-IN-TORL = FALSE.
   IF INPUT FILL-IN-TORL = FALSE THEN FILL-IN-TORL = TRUE.
   DISPLAY FILL-IN-TORL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TORM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TORM DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TORM IN FRAME DIALOG-1
DO:
   IF INPUT FILL-IN-TORM = TRUE THEN FILL-IN-TORM = FALSE.
   IF INPUT FILL-IN-TORM = FALSE THEN FILL-IN-TORM = TRUE.
   DISPLAY FILL-IN-TORM WITH FRAME {&FRAME-NAME}.
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
   assign
   ejand = FALSE
   allmat = FALSE.
   /*tidigare Internat gör att det även är kostförmån för frukost. Lena Jönsson MISV och Jarmo Klint SUND har bestämt att detta inte längre gäller 20190221 */
   IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "cMISV" OR Guru.Konstanter:globforetag = "cELPA"  THEN allmat = TRUE.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL"
   OR Guru.Konstanter:globforetag = "elpa" THEN ejand = TRUE.
             
   
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   FILL-IN-RUBRIK = "Kostförmån".
   regdatum = bdatum.
   regdatumspar2 = bdatum.
   RUN REGVEC.P.
   ASSIGN
   FILL-IN-VNR = regvnr.
   FIND FIRST maltidfil NO-LOCK NO-ERROR.
   regdatum = maltidfil.MDATUM.
   REPEAT:   
      IF regdatum > avdatum THEN LEAVE.
      FIND FIRST maltidfil WHERE maltidfil.MDATUM = regdatum  NO-LOCK NO-ERROR.
      IF maltidfil.MDAG = "mån" THEN DO:
         ASSIGN
         FILL-IN-MANF = FALSE 
         FILL-IN-MANL = maltidfil.MLUN
         FILL-IN-MANM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.                  
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-MANF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "tis" THEN DO:
         ASSIGN
         FILL-IN-TISF = FALSE 
         FILL-IN-TISL = maltidfil.MLUN
         FILL-IN-TISM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-TISF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "ons" THEN DO:
         ASSIGN
         FILL-IN-ONSF = FALSE 
         FILL-IN-ONSL = maltidfil.MLUN
         FILL-IN-ONSM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-ONSF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "tor" THEN DO:
         ASSIGN
         FILL-IN-TORF = FALSE 
         FILL-IN-TORL = maltidfil.MLUN
         FILL-IN-TORM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-TORF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "fre" THEN DO:
         ASSIGN
         FILL-IN-FREF = FALSE 
         FILL-IN-FREL = maltidfil.MLUN
         FILL-IN-FREM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-FREF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "lör" THEN DO:
         ASSIGN
         FILL-IN-LORF = FALSE 
         FILL-IN-LORL = maltidfil.MLUN
         FILL-IN-LORM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-LORF = maltidfil.MFRU.
               END.
            END.
         END.
      END.
      IF maltidfil.MDAG = "sön" THEN DO:
         ASSIGN
         FILL-IN-SONF = FALSE 
         FILL-IN-SONL = maltidfil.MLUN
         FILL-IN-SONM = maltidfil.MMID
         FILL-IN-VNR = maltidfil.MVECKONUMMER.         
         IF allmat = TRUE THEN DO: 
            IF regdatum = bdatum OR regdatum = avdatum THEN regvnr = regvnr.
            ELSE DO:
               IF maltidfil.MFRU = TRUE AND maltidfil.MLUN = TRUE AND maltidfil.MMID = TRUE THEN DO:
                  FILL-IN-SONF = maltidfil.MFRU.
               END.
            END.
         END.
         LEAVE.
      END.
      regdatum = regdatum + 1.
   END.   
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN-FR = "Fri Frukost"
   FILL-IN-LU = "Fri Lunch"
   FILL-IN-MI = "Fri Middag".
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      IF bdatum = avdatum THEN DO:
         /*dvs endags- föreslå lunch*/
         ASSIGN
         FILL-IN-FR = ""
         FILL-IN-LU = "Erhållit måltid"
         FILL-IN-MI = "".
      END.
   END.
   RUN enable_UI.
   IF allmat = TRUE THEN ED_KOSTINFO:HIDDEN = FALSE.
   ELSE ED_KOSTINFO:HIDDEN = TRUE.
      
   {FRMSIZED.I}
   ASSIGN 
   FILL-IN-MANF:HIDDEN = TRUE 
   FILL-IN-MANL:HIDDEN = TRUE
   FILL-IN-MANM:HIDDEN = TRUE
   FILL-IN-TISF:HIDDEN = TRUE 
   FILL-IN-TISL:HIDDEN = TRUE
   FILL-IN-TISM:HIDDEN = TRUE
   FILL-IN-ONSF:HIDDEN = TRUE
   FILL-IN-ONSL:HIDDEN = TRUE
   FILL-IN-ONSM:HIDDEN = TRUE
   FILL-IN-TORF:HIDDEN = TRUE
   FILL-IN-TORL:HIDDEN = TRUE
   FILL-IN-TORM:HIDDEN = TRUE
   FILL-IN-FREF:HIDDEN = TRUE
   FILL-IN-FREL:HIDDEN = TRUE
   FILL-IN-FREM:HIDDEN = TRUE
   FILL-IN-LORF:HIDDEN = TRUE
   FILL-IN-LORL:HIDDEN = TRUE
   FILL-IN-LORM:HIDDEN = TRUE
   FILL-IN-SONF:HIDDEN = TRUE
   FILL-IN-SONL:HIDDEN = TRUE
   FILL-IN-SONM:HIDDEN = TRUE.
   
   regdatum = bdatum.
   
   REPEAT:   
      IF regdatum > avdatum THEN LEAVE.
      FIND FIRST maltidfil WHERE maltidfil.MDATUM = regdatum  NO-LOCK NO-ERROR.
      IF maltidfil.MDAG = "mån" THEN DO:
         ASSIGN
         FILL-IN-MANF:HIDDEN = FALSE 
         FILL-IN-MANL:HIDDEN = FALSE 
         FILL-IN-MANM:HIDDEN = FALSE.
         IF ejand = TRUE THEN DISABLE FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule" THEN DISABLE FILL-IN-MANF  WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "tis" THEN DO:
         ASSIGN
         FILL-IN-TISF:HIDDEN = FALSE 
         FILL-IN-TISL:HIDDEN = FALSE 
         FILL-IN-TISM:HIDDEN = FALSE.
         IF ejand = TRUE THEN DISABLE FILL-IN-TISF FILL-IN-TISL FILL-IN-TISM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"  THEN DISABLE FILL-IN-TISF WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "ons" THEN DO:
         ASSIGN
         FILL-IN-ONSF:HIDDEN = FALSE 
         FILL-IN-ONSL:HIDDEN = FALSE 
         FILL-IN-ONSM:HIDDEN = FALSE.
         IF ejand = TRUE THEN DISABLE FILL-IN-ONSF FILL-IN-ONSL FILL-IN-ONSM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-ONSF WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "tor" THEN DO:
         ASSIGN
         FILL-IN-TORF:HIDDEN = FALSE 
         FILL-IN-TORL:HIDDEN = FALSE 
         FILL-IN-TORM:HIDDEN = FALSE.
         IF ejand = TRUE THEN DISABLE FILL-IN-TORF FILL-IN-TORL FILL-IN-TORM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"  THEN DISABLE FILL-IN-TORF  WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "fre" THEN DO:
         ASSIGN
         FILL-IN-FREF:HIDDEN = FALSE 
         FILL-IN-FREL:HIDDEN = FALSE 
         FILL-IN-FREM:HIDDEN = FALSE.
         IF ejand = TRUE THEN DISABLE FILL-IN-FREF FILL-IN-FREL FILL-IN-FREM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"  THEN DISABLE FILL-IN-FREF WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "lör" THEN DO:
         ASSIGN
         FILL-IN-LORF:HIDDEN = FALSE 
         FILL-IN-LORL:HIDDEN = FALSE 
         FILL-IN-LORM:HIDDEN = FALSE.
         IF ejand = TRUE THEN  DISABLE FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"  THEN DISABLE FILL-IN-LORF WITH FRAME {&FRAME-NAME}.         
      END.
      IF maltidfil.MDAG = "sön" THEN DO:
         ASSIGN
         FILL-IN-SONF:HIDDEN = FALSE 
         FILL-IN-SONL:HIDDEN = FALSE 
         FILL-IN-SONM:HIDDEN = FALSE
         regdatumspar = regdatum + 1.
         IF ejand = TRUE THEN  DISABLE FILL-IN-SONF FILL-IN-SONL FILL-IN-SONM WITH FRAME {&FRAME-NAME}.         
         IF Guru.Konstanter:globforetag = "lule"   THEN DISABLE FILL-IN-SONF WITH FRAME {&FRAME-NAME}.         
         LEAVE.
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
      DISPLAY  FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-RUBRIK FILL-IN-PKOD FILL-IN_FORNAMN-2 FILL-IN-FR FILL-IN-LU 
          FILL-IN-MI ED_KOSTINFO FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM 
          FILL-IN-TISF FILL-IN-TISL FILL-IN-TISM FILL-IN-ONSF FILL-IN-ONSL 
          FILL-IN-ONSM FILL-IN-TORF FILL-IN-TORL FILL-IN-TORM FILL-IN-FREF 
          FILL-IN-FREL FILL-IN-FREM FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM 
          FILL-IN-SONF FILL-IN-SONL FILL-IN-SONM 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-MANF FILL-IN-MANL FILL-IN-MANM FILL-IN-TISF FILL-IN-TISL 
         FILL-IN-TISM FILL-IN-ONSF FILL-IN-ONSL FILL-IN-ONSM FILL-IN-TORF 
         FILL-IN-TORL FILL-IN-TORM FILL-IN-FREF FILL-IN-FREL FILL-IN-FREM 
         FILL-IN-LORF FILL-IN-LORL FILL-IN-LORM FILL-IN-SONF FILL-IN-SONL 
         FILL-IN-SONM BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

