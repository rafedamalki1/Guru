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
{GLOBVAR2DEL1.I}
{REGVAR.I}
{RESDEF.I}
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{PHMT.I}
DEFINE  SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE utlandet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-34 RECT-36 RECT-37 CMB_LAND ~
FILL-IN-START FILL-IN-DATU1 FILL-IN-UTSVE FILL-IN-DATU2 FILL-IN-INUTR ~
FILL-IN-DATU3 FILL-IN-UTUTR FILL-IN-DATH1 FILL-IN-INSVE FILL-IN-DATH2 ~
FILL-IN-SLUT FILL-IN-DATH3 BTN_REG BTN_AVB FILL-IN-DEST 
&Scoped-Define DISPLAYED-OBJECTS CMB_LAND FILL-IN-KR FILL-IN-START ~
FILL-IN-DATU1 FILL-IN-UTSVE FILL-IN-DATU2 FILL-IN-INUTR FILL-IN-DATU3 ~
FILL-IN-UTUTR FILL-IN-DATH1 FILL-IN-INSVE FILL-IN-DATH2 FILL-IN-SLUT ~
FILL-IN-DATH3 FILL-IN-DEST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG AUTO-GO 
     LABEL "Fortsätt":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LAND AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 33.63 BY .96 TOOLTIP "Välj Destinationsland. Rätt belopp på traktamente hämtas då." NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATH1 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATH2 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATH3 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATU1 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATU2 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATU3 AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DEST AS CHARACTER FORMAT "X(256)":U INITIAL "DESTINATION:" 
      VIEW-AS TEXT 
     SIZE 11 BY .63
     FGCOLOR 12  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-INSVE AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Anländer Sverige (tex ankomst med utrikesflyg)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-INUTR AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Anländer destinationsland (tex ankomst utrikesflyg)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KR AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Kr" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY 1 TOOLTIP "Traktamentsbelopp för valt destinationsland" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-SLUT AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Slut resa (bostad eller arbete)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-START AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Start utresa (hemifrån eller från arbetet)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-UTSVE AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Lämnar Sverige (tex avgång utrikesflyg)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-UTUTR AS DECIMAL FORMAT ">9.99":U INITIAL ? 
     LABEL "Lämnar destinationslandet (tex avgång utrikesflyg)" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 1.92
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 7.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 7.67
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     CMB_LAND AT ROW 1.54 COL 30.63 COLON-ALIGNED NO-LABEL
     FILL-IN-KR AT ROW 1.54 COL 68.88 COLON-ALIGNED
     FILL-IN-START AT ROW 4.5 COL 52.5 COLON-ALIGNED
     FILL-IN-DATU1 AT ROW 4.5 COL 68.13 COLON-ALIGNED
     FILL-IN-UTSVE AT ROW 6 COL 52.5 COLON-ALIGNED
     FILL-IN-DATU2 AT ROW 6 COL 68.13 COLON-ALIGNED
     FILL-IN-INUTR AT ROW 7.5 COL 52.63 COLON-ALIGNED
     FILL-IN-DATU3 AT ROW 7.5 COL 68.13 COLON-ALIGNED
     FILL-IN-UTUTR AT ROW 11.75 COL 52.5 COLON-ALIGNED
     FILL-IN-DATH1 AT ROW 11.75 COL 68.25 COLON-ALIGNED
     FILL-IN-INSVE AT ROW 13.25 COL 52.5 COLON-ALIGNED
     FILL-IN-DATH2 AT ROW 13.25 COL 68.25 COLON-ALIGNED
     FILL-IN-SLUT AT ROW 14.75 COL 52.5 COLON-ALIGNED
     FILL-IN-DATH3 AT ROW 14.75 COL 68.25 COLON-ALIGNED
     BTN_REG AT ROW 18.17 COL 56
     BTN_AVB AT ROW 18.17 COL 71
     FILL-IN-DEST AT ROW 1.58 COL 18.25 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     "Hemresa:" VIEW-AS TEXT
          SIZE 9.5 BY .75 AT ROW 10.92 COL 1.5
     "Utresa:" VIEW-AS TEXT
          SIZE 9.5 BY .75 AT ROW 3.38 COL 1.5
     RECT-34 AT ROW 1 COL 1
     RECT-36 AT ROW 10.58 COL 1
     RECT-37 AT ROW 2.92 COL 1
     SPACE(0.12) SKIP(8.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Utrikes flerdygnsförrättning":L.


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

/* SETTINGS FOR FILL-IN FILL-IN-DATH1 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-DATH2 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-DATH3 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-DATU1 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-DATU2 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-DATU3 IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-INSVE IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-INUTR IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-KR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SLUT IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-START IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-UTSVE IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-UTUTR IN FRAME DIALOG-1
   SHARED                                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Utrikes flerdygnsförrättning */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Utrikes flerdygnsförrättning */
DO: 
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
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
   CMB_LAND = INPUT CMB_LAND
   utlandet = CMB_LAND
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-UTSVE = INPUT FILL-IN-UTSVE
   FILL-IN-INUTR = INPUT FILL-IN-INUTR
   FILL-IN-UTUTR = INPUT FILL-IN-UTUTR
   FILL-IN-INSVE = INPUT FILL-IN-INSVE
   FILL-IN-SLUT = INPUT FILL-IN-SLUT  
   FILL-IN-DATU1 = INPUT FILL-IN-DATU1
   FILL-IN-DATU2 = INPUT FILL-IN-DATU2
   FILL-IN-DATU3 = INPUT FILL-IN-DATU3
   FILL-IN-DATH1 = INPUT FILL-IN-DATH1
   FILL-IN-DATH2 = INPUT FILL-IN-DATH2
   FILL-IN-DATH3 = INPUT FILL-IN-DATH3.
   IF CMB_LAND = "Afghanistan" THEN DO:
      MESSAGE "Har du verkligen rest till Afghanistan?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.        
      IF val = FALSE THEN DO:
         APPLY "ENTRY" TO CMB_LAND IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF CMB_LAND = "Albanien" THEN DO:
      MESSAGE "Har du verkligen rest till Albanien?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.        
      IF val2 = FALSE THEN DO:
         APPLY "ENTRY" TO CMB_LAND IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF CMB_LAND = "Algeriet" THEN DO:
      MESSAGE "Har du verkligen rest till Algeriet?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.        
      IF val3 = FALSE THEN DO:
         APPLY "ENTRY" TO CMB_LAND IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.   
   IF FILL-IN-DATU1 = FILL-IN-DATU2 AND FILL-IN-UTSVE < FILL-IN-START  THEN DO:
      MESSAGE "Antingen är tiden lämnar Sverige felakigt angiven eller så pågår utresan i flera dagar, ändra i så fall på datum" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-UTSVE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATU2 = FILL-IN-DATU3 AND FILL-IN-INUTR < FILL-IN-UTSVE  THEN DO:
      MESSAGE "Antingen är tiden anländer destinationsland felakigt angiven eller så pågår utresan i flera dagar, ändra i så fall på datum" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-INUTR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATU2 < FILL-IN-DATU1 OR FILL-IN-DATU3 < FILL-IN-DATU2  THEN DO:
      MESSAGE "Datum felaktigt angivet" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-DATU3 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.

   IF FILL-IN-DATH1 = FILL-IN-DATH2 AND FILL-IN-INSVE < FILL-IN-UTUTR  THEN DO:
      MESSAGE "Antingen är tiden anländer Sverige felakigt angiven eller så pågår hemresan i flera dagar, ändra i så fall på datum" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-INSVE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATH2 = FILL-IN-DATH3 AND FILL-IN-SLUT < FILL-IN-INSVE  THEN DO:
      MESSAGE "Antingen är tiden slut resa felakigt angiven eller så pågår hemresan i flera dagar, ändra i så fall på datum" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-SLUT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATH2 < FILL-IN-DATH1 OR FILL-IN-DATU3 < FILL-IN-DATU2  THEN DO:
      MESSAGE "Datum felaktigt angivet" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-DATH3 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATU1 < bdatum  OR FILL-IN-DATU2 < bdatum OR FILL-IN-DATU3 < bdatum THEN DO:
      MESSAGE "Datum kan inte vara mindre än startdatum för utlandsresan" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-DATU1 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-DATH1 > avdatum  OR FILL-IN-DATH2 > avdatum OR FILL-IN-DATH3 > avdatum THEN DO:
      MESSAGE "Datum kan inte vara större än slutdatum för utlandsresan" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-DATH3 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.

   RETURN.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LAND DIALOG-1
ON VALUE-CHANGED OF CMB_LAND IN FRAME DIALOG-1
DO:
  CMB_LAND = INPUT CMB_LAND.
  FIND FIRST landtemp WHERE landtemp.LAND = CMB_LAND NO-LOCK NO-ERROR.
  IF AVAILABLE landtemp THEN ASSIGN FILL-IN-KR = landtemp.BELOPP.
  DISPLAY FILL-IN-KR WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATH1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATH1 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATH1 IN FRAME DIALOG-1 /* Datum */
DO:
  ASSIGN
   FILL-IN-DATH1 = INPUT FILL-IN-DATH1
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATH1.
   RUN AlmanBtn.w.
   FILL-IN-DATH1 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATH1 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATH2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATH2 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATH2 IN FRAME DIALOG-1 /* Datum */
DO:
  ASSIGN
   FILL-IN-DATH2 = INPUT FILL-IN-DATH2
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATH2.
   RUN AlmanBtn.w.
   FILL-IN-DATH2 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATH2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATH3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATH3 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATH3 IN FRAME DIALOG-1 /* Datum */
DO:
  ASSIGN
   FILL-IN-DATH3 = INPUT FILL-IN-DATH3
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATH3.
   RUN AlmanBtn.w.
   FILL-IN-DATH3 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATH3 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATU1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATU1 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATU1 IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   FILL-IN-DATU1 = INPUT FILL-IN-DATU1
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATU1.
   RUN AlmanBtn.w.
   FILL-IN-DATU1 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATU1 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATU2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATU2 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATU2 IN FRAME DIALOG-1 /* Datum */
DO:
  ASSIGN
   FILL-IN-DATU2 = INPUT FILL-IN-DATU2
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATU2.
   RUN AlmanBtn.w.
   FILL-IN-DATU2 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATU2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATU3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATU3 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATU3 IN FRAME DIALOG-1 /* Datum */
DO:
  ASSIGN
   FILL-IN-DATU3 = INPUT FILL-IN-DATU3
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATU3.
   RUN AlmanBtn.w.
   FILL-IN-DATU3 = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN-DATU3 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-INSVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-INSVE DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-INSVE IN FRAME DIALOG-1 /* Anländer Sverige (tex ankomst med utrikesflyg) */
DO:
  klocka = INPUT FILL-IN-INSVE.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-INSVE = klocka.
   DISPLAY FILL-IN-INSVE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-INUTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-INUTR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-INUTR IN FRAME DIALOG-1 /* Anländer destinationsland (tex ankomst utrikesflyg) */
DO:
  klocka = INPUT FILL-IN-INUTR.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-INUTR = klocka.
   DISPLAY FILL-IN-INUTR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUT IN FRAME DIALOG-1 /* Slut resa (bostad eller arbete) */
DO:
  klocka = INPUT FILL-IN-SLUT.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-SLUT = klocka.
   DISPLAY FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-START IN FRAME DIALOG-1 /* Start utresa (hemifrån eller från arbetet) */
DO:
  klocka = INPUT FILL-IN-START.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-START = klocka.
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTSVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTSVE DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-UTSVE IN FRAME DIALOG-1 /* Lämnar Sverige (tex avgång utrikesflyg) */
DO:
  klocka = INPUT FILL-IN-UTSVE.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-UTSVE = klocka.
   DISPLAY FILL-IN-UTSVE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTUTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTUTR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-UTUTR IN FRAME DIALOG-1 /* Lämnar destinationslandet (tex avgång utrikesflyg) */
DO:
  klocka = INPUT FILL-IN-UTUTR.  
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-UTUTR = klocka.
   DISPLAY FILL-IN-UTUTR WITH FRAME {&FRAME-NAME}.
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
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LAND.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE landtemp).
   END.
   ELSE DO:
      RUN LAND.P 
      (OUTPUT TABLE landtemp).
   END.
   FOR EACH landtemp USE-INDEX LAND NO-LOCK:
      status-ok = CMB_LAND:ADD-LAST(landtemp.LAND).
   END.
   FIND FIRST landtemp USE-INDEX LAND NO-LOCK NO-ERROR.
   CMB_LAND:SCREEN-VALUE IN FRAME {&FRAME-NAME} = landtemp.LAND.
   CMB_LAND = INPUT CMB_LAND.
   FILL-IN-KR = landtemp.BELOPP.
   ASSIGN FILL-IN-DATU1 = bdatum
   FILL-IN-DATU2 = bdatum
   FILL-IN-DATU3 = bdatum
   FILL-IN-DATH1 = avdatum
   FILL-IN-DATH2 = avdatum
   FILL-IN-DATH3 = avdatum.
   regdatum = bdatum.
   RUN REGVEC.P.
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-START = regstart
   FILL-IN-UTSVE = regstart
   FILL-IN-INUTR = regstart
   FILL-IN-UTUTR = regslut
   FILL-IN-INSVE = regslut
   FILL-IN-SLUT = regslut.
   RUN enable_UI.       
   {FRMSIZED.I}       
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
  DISPLAY CMB_LAND FILL-IN-KR FILL-IN-START FILL-IN-DATU1 FILL-IN-UTSVE 
          FILL-IN-DATU2 FILL-IN-INUTR FILL-IN-DATU3 FILL-IN-UTUTR FILL-IN-DATH1 
          FILL-IN-INSVE FILL-IN-DATH2 FILL-IN-SLUT FILL-IN-DATH3 FILL-IN-DEST 
      WITH FRAME DIALOG-1.
  ENABLE RECT-34 RECT-36 RECT-37 CMB_LAND FILL-IN-START FILL-IN-DATU1 
         FILL-IN-UTSVE FILL-IN-DATU2 FILL-IN-INUTR FILL-IN-DATU3 FILL-IN-UTUTR 
         FILL-IN-DATH1 FILL-IN-INSVE FILL-IN-DATH2 FILL-IN-SLUT FILL-IN-DATH3 
         BTN_REG BTN_AVB FILL-IN-DEST 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

