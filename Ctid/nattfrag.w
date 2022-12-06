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
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS FILL-IN-MANF FILL-IN-TISF FILL-IN-ONSF ~
FILL-IN-TORF FILL-IN-FREF FILL-IN-LORF FILL-IN-SONF BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NATT FILL-IN-MANF FILL-IN-TISF ~
FILL-IN-ONSF FILL-IN-TORF FILL-IN-FREF FILL-IN-LORF FILL-IN-SONF 

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

DEFINE VARIABLE FILL-IN-FREF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "fre" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "lör" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "mån" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NATT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nattraktamente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ONSF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "ons" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SONF AS LOGICAL FORMAT "Ja /Nej":U INITIAL NO 
     LABEL "sön" 
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

DEFINE VARIABLE FILL-IN-TORF AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "tor" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VNR AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Vecka" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-VNR AT ROW 1.75 COL 20 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 1.88 COL 9.88 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 1.88 COL 28.63 COLON-ALIGNED
     FILL-IN-NATT AT ROW 3.17 COL 18 COLON-ALIGNED
     FILL-IN-MANF AT ROW 4.5 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-TISF AT ROW 5.67 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-ONSF AT ROW 6.92 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-TORF AT ROW 8.08 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-FREF AT ROW 9.33 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-LORF AT ROW 10.5 COL 8 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-SONF AT ROW 11.67 COL 8 COLON-ALIGNED AUTO-RETURN 
     BTN_REG AT ROW 13.21 COL 10.63
     BTN_AVS AT ROW 13.21 COL 25.63
     SPACE(1.74) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nattraktamente":L.


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

/* SETTINGS FOR FILL-IN FILL-IN-NATT IN FRAME DIALOG-1
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
ON END-ERROR OF FRAME DIALOG-1 /* Nattraktamente */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Nattraktamente */
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
   FILL-IN-LORF = INPUT FILL-IN-LORF
   FILL-IN-MANF = INPUT FILL-IN-MANF 
   FILL-IN-ONSF = INPUT FILL-IN-ONSF 
   FILL-IN-SONF = INPUT FILL-IN-SONF 
   FILL-IN-TISF = INPUT FILL-IN-TISF 
   FILL-IN-TORF = INPUT FILL-IN-TORF.
   regdatum = regdatumspar2.
   REPEAT:
      IF regdatum > avdatum THEN DO:
         musz = TRUE.
         LEAVE.
      END.
      CREATE nattfil.             
      IF WEEKDAY(regdatum) = 1 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-SONF      
         nattfil.MDAG = "sön"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
         regdatumspar2 = regdatum + 1.
         IF regdatum = avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.
         ELSE LEAVE.
      END.
      IF WEEKDAY(regdatum) = 2 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-MANF
         nattfil.MDAG = "mån"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 3 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-TISF
         nattfil.MDAG = "tis"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 4 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-ONSF
         nattfil.MDAG = "ons"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 5 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-TORF
         nattfil.MDAG = "tor"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 6 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-FREF
         nattfil.MDAG = "fre"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      IF WEEKDAY(regdatum) = 7 THEN DO:
         ASSIGN nattfil.MPERSONALKOD = pkod
         nattfil.NATTRAKT = FILL-IN-LORF
         nattfil.MDAG = "lör"
         nattfil.MVECKONUMMER = FILL-IN-VNR
         nattfil.MDATUM = regdatum.
      END.
      regdatum = regdatum + 1.
   END.
   IF musz = FALSE THEN DO:      
      regdatum = regdatumspar.
      RUN REGVEC.P.
      ASSIGN
      FILL-IN-VNR = regvnr
      FILL-IN-MANF = TRUE     
      FILL-IN-TISF = TRUE 
      FILL-IN-ONSF = TRUE
      FILL-IN-TORF = TRUE
      FILL-IN-FREF = TRUE
      FILL-IN-LORF = TRUE
      FILL-IN-SONF = TRUE.
      DISPLAY FILL-IN-MANF FILL-IN-TISF FILL-IN-ONSF FILL-IN-TORF 
      FILL-IN-FREF FILL-IN-LORF FILL-IN-SONF WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-MANF:HIDDEN = TRUE 
      FILL-IN-TISF:HIDDEN = TRUE 
      FILL-IN-ONSF:HIDDEN = TRUE
      FILL-IN-TORF:HIDDEN = TRUE
      FILL-IN-FREF:HIDDEN = TRUE
      FILL-IN-LORF:HIDDEN = TRUE
      FILL-IN-SONF:HIDDEN = TRUE.
      REPEAT:
         IF regdatum > avdatum THEN DO:
            musz = TRUE.
            LEAVE.
         END.      
         IF WEEKDAY(regdatum) = 1 THEN DO:
            ASSIGN
            FILL-IN-SONF:HIDDEN = FALSE.             
            regdatumspar = regdatum + 1.
            IF regdatum = avdatum THEN  DO:
               musz = TRUE.
               LEAVE.
            END.   
            ELSE LEAVE.
         END.
         IF WEEKDAY(regdatum) = 2 THEN DO:
            ASSIGN
            FILL-IN-MANF:HIDDEN = FALSE.            
         END.
         IF WEEKDAY(regdatum) = 3 THEN DO:
            ASSIGN
            FILL-IN-TISF:HIDDEN = FALSE.            
         END.
         IF WEEKDAY(regdatum) = 4 THEN DO:
            ASSIGN
            FILL-IN-ONSF:HIDDEN = FALSE.            
         END.
         IF WEEKDAY(regdatum) = 5 THEN DO:
            ASSIGN
            FILL-IN-TORF:HIDDEN = FALSE.
         END.
         IF WEEKDAY(regdatum) = 6 THEN DO:
            ASSIGN
            FILL-IN-FREF:HIDDEN = FALSE.         
         END. 
         IF WEEKDAY(regdatum) = 7 THEN DO:
            ASSIGN
            FILL-IN-LORF:HIDDEN = FALSE.
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
      OPEN QUERY nattq FOR EACH nattfil,
      EACH respers WHERE respers.DATUM = nattfil.MDATUM.
      DO TRANSACTION:
         GET FIRST nattq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(respers):                    
            ASSIGN respers.NATTRAKT = nattfil.NATTRAKT.                 
            GET NEXT nattq EXCLUSIVE-LOCK.
         END.   
      END.             
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
   FOR EACH nattfil:
      DELETE nattfil.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:   
      FILL-IN-NATT:LABEL = "Privat logi".
      FRAME DIALOG-1:TITLE = "Privat logi" .
   END.
   regdatum = bdatum.
   regdatumspar2 = bdatum.
   RUN REGVEC.P.
   ASSIGN
   FILL-IN-VNR = regvnr
   FILL-IN-MANF = TRUE 
   FILL-IN-TISF = TRUE 
   FILL-IN-ONSF = TRUE
   FILL-IN-TORF = TRUE
   FILL-IN-FREF = TRUE
   FILL-IN-LORF = TRUE
   FILL-IN-SONF = TRUE.
   RUN enable_UI.       
   {FRMSIZED.I}
   ASSIGN 
   FILL-IN-MANF:HIDDEN = TRUE    
   FILL-IN-TISF:HIDDEN = TRUE 
   FILL-IN-ONSF:HIDDEN = TRUE
   FILL-IN-TORF:HIDDEN = TRUE
   FILL-IN-FREF:HIDDEN = TRUE
   FILL-IN-LORF:HIDDEN = TRUE
   FILL-IN-SONF:HIDDEN = TRUE.
   REPEAT:
      IF regdatum > avdatum THEN LEAVE.
      IF WEEKDAY(regdatum) = 1 THEN DO:
         ASSIGN
         FILL-IN-SONF:HIDDEN = FALSE          
         regdatumspar = regdatum + 1.
         LEAVE.
      END.
      IF WEEKDAY(regdatum) = 2 THEN DO:
         ASSIGN
         FILL-IN-MANF:HIDDEN = FALSE. 
      END.
      IF WEEKDAY(regdatum) = 3 THEN DO:
         ASSIGN
         FILL-IN-TISF:HIDDEN = FALSE. 
      END.
      IF WEEKDAY(regdatum) = 4 THEN DO:
         ASSIGN
         FILL-IN-ONSF:HIDDEN = FALSE. 
      END.
      IF WEEKDAY(regdatum) = 5 THEN DO:
         ASSIGN
         FILL-IN-TORF:HIDDEN = FALSE. 
      END.
      IF WEEKDAY(regdatum) = 6 THEN DO:
         ASSIGN
         FILL-IN-FREF:HIDDEN = FALSE. 
      END.
      IF WEEKDAY(regdatum) = 7 THEN DO:
         ASSIGN
         FILL-IN-LORF:HIDDEN = FALSE. 
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
  DISPLAY FILL-IN-NATT FILL-IN-MANF FILL-IN-TISF FILL-IN-ONSF FILL-IN-TORF 
          FILL-IN-FREF FILL-IN-LORF FILL-IN-SONF 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-MANF FILL-IN-TISF FILL-IN-ONSF FILL-IN-TORF FILL-IN-FREF 
         FILL-IN-LORF FILL-IN-SONF BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

