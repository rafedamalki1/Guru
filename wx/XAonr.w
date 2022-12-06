&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT8              PROGRESS
*/
&Scoped-define WINDOW-NAME    WAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WAONR 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
&Scoped-define NEW NEW
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE globniv LIKE ANVANDARE.AV-LEVEL NO-UNDO.
DEFINE SHARED VARIABLE globallpers LIKE ANVANDARE.ALLPERS NO-UNDO. 
DEFINE SHARED VARIABLE globallao LIKE ANVANDARE.ALLAONR NO-UNDO. 
DEFINE SHARED VARIABLE globomr LIKE PERSONALTAB.OMRADE NO-UNDO. 
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE BUFFER aonrbuff FOR AONRTAB.
DEFINE BUFFER aonrkontbuff FOR AONRKONTKOD.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE hjdelvar LIKE AONRTAB.DELNR NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE aonrtemp
   FIELD AONR LIKE    AONRTAB.AONR
   FIELD DELNR LIKE      AONRTAB.DELNR
   FIELD OMRADE LIKE      AONRTAB.OMRADE
   FIELD AONRTABREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-A
&Scoped-define BROWSE-NAME BRW_AONR

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR AONRTAB.OMRADE AONRTAB.AONR ~
AONRTAB.DELNR AONRTAB.ORT 
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH AONRTAB NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR AONRTAB
&Scoped-define TABLES-IN-QUERY-BRW_AONR AONRTAB 

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define FIELDS-IN-QUERY-FRAME-A 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-A 

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WAONR AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Arkiv 
       MENU-ITEM m_Stng         LABEL "Stäng"         .

DEFINE SUB-MENU m_Updatera 
       MENU-ITEM m_Ny           LABEL "Ny"            
       MENU-ITEM m_ndra         LABEL "Ändra"         
       MENU-ITEM m_Ta_bort      LABEL "Ta bort"       
       MENU-ITEM m_Visa         LABEL "Visa"          .

DEFINE SUB-MENU m_Funktioner2 
       MENU-ITEM m_Uppdela      LABEL "Underindela"   
       MENU-ITEM m_Kalkylera    LABEL "Kalkylera"     
       MENU-ITEM m_Rapporter    LABEL "Rapporter"     
       MENU-ITEM m_Kostnadsregistrering LABEL "Kostnadsregistrering"
       MENU-ITEM m_Avsluta_aonr LABEL "Avsluta aonr"  
       MENU-ITEM m_Visa_avslutade LABEL "Visa avslutade aonr".

DEFINE SUB-MENU m_G_till 
       MENU-ITEM m_Arbetsorderhantering LABEL "Arbetsorderhantering"
       MENU-ITEM m_Materielhantering LABEL "Materielhantering"
       MENU-ITEM m_Kalkylering  LABEL "Kalkylering"   
       MENU-ITEM m_Tidredovisning LABEL "Tidredovisning"
       MENU-ITEM m_Flextid      LABEL "Flextid"       
       MENU-ITEM m_Uppfljning   LABEL "Uppföljning"   
       MENU-ITEM m_Personalregistrering LABEL "Personalregistrering"
       MENU-ITEM m_Sekretess    LABEL "Sekretess"     
       MENU-ITEM m_Registerhantering LABEL "Registerhantering".

DEFINE MENU MENU-BAR-WAONR MENUBAR
       SUB-MENU  m_Arkiv        LABEL "Arkiv"         
       SUB-MENU  m_Updatera     LABEL "Uppdatera"     
       SUB-MENU  m_Funktioner2  LABEL "Funktioner"    
       SUB-MENU  m_G_till       LABEL "Gå till"       .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSAONR 
     LABEL "Avsluta Aonr":L 
     SIZE 15 BY 2.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 15 BY 2.5.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta Bort":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_KALK 
     LABEL "Kalkylera":L 
     SIZE 15 BY 2.

DEFINE BUTTON BTN_KOST 
     LABEL "Kostnadsreg":L 
     SIZE 15 BY 2.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_RAPP 
     LABEL "Rapporter":L 
     SIZE 15 BY 2.

DEFINE BUTTON BTN_SOKA 
     LABEL "Sök":L 
     SIZE 5 BY 1.

DEFINE BUTTON BTN_SOKB 
     LABEL "Sök":L 
     SIZE 5 BY 1.

DEFINE BUTTON BTN_UNDER 
     LABEL "Underindela":L 
     SIZE 15 BY 2.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_VISAV 
     LABEL "Visa avslutade":L 
     SIZE 15 BY 2.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "AONR" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "BENÄMN." 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga Aonr", no,
"Fasta Aonr", yes
     SIZE 33 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 52 BY 2.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 59.5 BY 21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 18.5 BY 21
     BGCOLOR 8 .

DEFINE {&NEW} SHARED VARIABLE TOG_AONR AS LOGICAL INITIAL ? 
     LABEL "Visa alla aonr" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .91 NO-UNDO.


/* Query definitions                                                    */
DEFINE QUERY BRW_AONR FOR AONRTAB SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR QUERY BRW_AONR NO-LOCK DISPLAY 
      AONRTAB.OMRADE
      AONRTAB.AONR
      AONRTAB.DELNR
      AONRTAB.ORT
    WITH SIZE 57 BY 12.91
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_FAST AT ROW 1.5 COL 3 NO-LABEL
     TOG_AONR AT ROW 1.5 COL 38.5
     BRW_AONR AT ROW 2.73 COL 2.25
     BTN_UNDER AT ROW 3 COL 62
     BTN_KALK AT ROW 5.5 COL 62
     BTN_RAPP AT ROW 8 COL 62
     BTN_KOST AT ROW 10.5 COL 62
     BTN_AVSAONR AT ROW 13 COL 62
     BTN_VISAV AT ROW 15.5 COL 62
     BTN_NY AT ROW 17 COL 7
     BTN_UPP AT ROW 17 COL 19
     BTN_BORT AT ROW 17 COL 31.5
     BTN_VISAO AT ROW 17 COL 44
     BTN_AVSL AT ROW 18.5 COL 61.88
     FILL-IN_AONR AT ROW 19.32 COL 20 COLON-ALIGNED
     BTN_SOKA AT ROW 19.32 COL 44
     FILL-IN_ORT AT ROW 20.64 COL 20 COLON-ALIGNED
     BTN_SOKB AT ROW 20.64 COL 44
     RECT-4 AT ROW 1 COL 1
     RECT-5 AT ROW 1 COL 60.5
     "Funktioner:" VIEW-AS TEXT
          SIZE 14.5 BY 1.5 AT ROW 1.5 COL 62
     "DET FINNS INGA AONR ATT VISA!" VIEW-AS TEXT
          SIZE 43 BY 4.5 AT ROW 8 COL 9.5
          FONT 17
     "Aonr" VIEW-AS TEXT
          SIZE 8 BY .68 AT ROW 15.82 COL 5
     RECT-20 AT ROW 16.5 COL 4.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.13 BY 21.05.

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WAONR ASSIGN
         HIDDEN             = YES
         TITLE              = "Arbetsorderhantering"
         COLUMN             = 11
         ROW                = 1.64
         HEIGHT             = 21.23
         WIDTH              = 78.13
         MAX-HEIGHT         = 27.27
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.27
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-WAONR:HANDLE.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WAONR
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR BROWSE BRW_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME FRAME-A            = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME FRAME-A     = 1000.

/* SETTINGS FOR BUTTON BTN_AVSAONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AVSAONR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_BORT IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORT:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_KALK IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KALK:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_KOST IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KOST:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_RAPP IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_RAPP:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_UNDER IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UNDER:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPP IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_VISAO IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_VISAO:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_VISAV IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_VISAV:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_AONR IN FRAME FRAME-A
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WAONR)
THEN WAONR:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "rt8.AONRTAB"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = rt8.AONRTAB.OMRADE
     _FldNameList[2]   = rt8.AONRTAB.AONR
     _FldNameList[3]   = rt8.AONRTAB.DELNR
     _FldNameList[4]   = rt8.AONRTAB.ORT
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR WAONR
ON MOUSE-SELECT-DBLCLICK OF BRW_AONR IN FRAME FRAME-A
DO:
   IF XSEK.SEK[2] = TRUE THEN RUN andra_UI.
   ELSE MESSAGE "DU ÄR INTE BEHÖRIG ATT GÖRA ÄNDRINGAR PÅ ARBETSORDEREN"
      VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSAONR WAONR
ON CHOOSE OF BTN_AVSAONR IN FRAME FRAME-A /* Avsluta Aonr */
DO:
   RUN avsluta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WAONR
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WAONR
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta Bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KALK WAONR
ON CHOOSE OF BTN_KALK IN FRAME FRAME-A /* Kalkylera */
DO: 
   RUN kalk_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOST WAONR
ON CHOOSE OF BTN_KOST IN FRAME FRAME-A /* Kostnadsreg */
DO:
   RUN kost_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WAONR
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RAPP WAONR
ON CHOOSE OF BTN_RAPP IN FRAME FRAME-A /* Rapporter */
DO:
   RUN rapp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SOKA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SOKA WAONR
ON CHOOSE OF BTN_SOKA IN FRAME FRAME-A /* Sök */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   IF FILL-IN_AONR = '' THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN_AONR + '*'.
   IF TOG_AONR = TRUE THEN DO: 
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
      FIND NEXT AONRTAB WHERE AONRTAB.AONR MATCHES aosok AND
      AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
      FIND NEXT AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.AONR MATCHES aosok AND
      AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.     
   IF NOT AVAILABLE AONRTAB THEN DO:
      IF TOG_AONR = TRUE THEN DO: 
         FIND FIRST AONRTAB WHERE AONRTAB.AONR MATCHES aosok AND
         AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.AONR MATCHES aosok AND
         AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END. 
      IF NOT AVAILABLE AONRTAB THEN DO:
         MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE AONRTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(AONRTAB).      
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SOKB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SOKB WAONR
ON CHOOSE OF BTN_SOKB IN FRAME FRAME-A /* Sök */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   IF FILL-IN_ORT = '' THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ORT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   ortssok = '*' + FILL-IN_ORT + '*'.
   IF TOG_AONR = TRUE THEN DO: 
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
      FIND NEXT AONRTAB WHERE AONRTAB.ORT MATCHES ortssok AND
      AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
      FIND NEXT AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.ORT MATCHES ortssok AND
      AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   
   IF NOT AVAILABLE AONRTAB THEN DO:
      IF TOG_AONR = TRUE THEN DO: 
         FIND FIRST AONRTAB WHERE AONRTAB.ORT MATCHES ortssok AND
         AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.ORT MATCHES ortssok AND
         AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END. 
      IF NOT AVAILABLE AONRTAB THEN DO:
         MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.      
   IF AVAILABLE AONRTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(AONRTAB).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UNDER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UNDER WAONR
ON CHOOSE OF BTN_UNDER IN FRAME FRAME-A /* Underindela */
DO:
   RUN under_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WAONR
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISAO WAONR
ON CHOOSE OF BTN_VISAO IN FRAME FRAME-A /* Visa */
DO:
   RUN visao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISAV WAONR
ON CHOOSE OF BTN_VISAV IN FRAME FRAME-A /* Visa avslutade */
DO:   
   RUN visav_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR WAONR
ON LEAVE OF FILL-IN_AONR IN FRAME FRAME-A /* AONR */
DO:
   FILL-IN_AONR = INPUT FILL-IN_AONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT WAONR
ON LEAVE OF FILL-IN_ORT IN FRAME FRAME-A /* BENÄMN. */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Avsluta_aonr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avsluta_aonr WAONR
ON CHOOSE OF MENU-ITEM m_Avsluta_aonr /* Avsluta aonr */
DO:
   RUN avsluta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Flextid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Flextid WAONR
ON CHOOSE OF MENU-ITEM m_Flextid /* Flextid */
DO:
   vartpro = "FLX".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kalkylera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kalkylera WAONR
ON CHOOSE OF MENU-ITEM m_Kalkylera /* Kalkylera */
DO:
   RUN kalk_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kalkylering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kalkylering WAONR
ON CHOOSE OF MENU-ITEM m_Kalkylering /* Kalkylering */
DO:
   vart = "KAL".
    vartpro = "KAL".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kostnadsregistrering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kostnadsregistrering WAONR
ON CHOOSE OF MENU-ITEM m_Kostnadsregistrering /* Kostnadsregistrering */
DO:
   RUN kost_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Materielhantering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Materielhantering WAONR
ON CHOOSE OF MENU-ITEM m_Materielhantering /* Materielhantering */
DO:
   vartpro = "DEP".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ndra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ndra WAONR
ON CHOOSE OF MENU-ITEM m_ndra /* Ändra */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ny WAONR
ON CHOOSE OF MENU-ITEM m_Ny /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Personalregistrering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Personalregistrering WAONR
ON CHOOSE OF MENU-ITEM m_Personalregistrering /* Personalregistrering */
DO:
   vartpro = "PER".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Rapporter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Rapporter WAONR
ON CHOOSE OF MENU-ITEM m_Rapporter /* Rapporter */
DO:
   RUN rapp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Registerhantering
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Registerhantering WAONR
ON CHOOSE OF MENU-ITEM m_Registerhantering /* Registerhantering */
DO:
   vartpro = "REG".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sekretess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sekretess WAONR
ON CHOOSE OF MENU-ITEM m_Sekretess /* Sekretess */
DO:
   vartpro = "SEK".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Stng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Stng WAONR
ON CHOOSE OF MENU-ITEM m_Stng /* Stäng */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ta_bort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ta_bort WAONR
ON CHOOSE OF MENU-ITEM m_Ta_bort /* Ta bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Tidredovisning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Tidredovisning WAONR
ON CHOOSE OF MENU-ITEM m_Tidredovisning /* Tidredovisning */
DO:
   vartpro = "TID".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Uppdela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Uppdela WAONR
ON CHOOSE OF MENU-ITEM m_Uppdela /* Underindela */
DO:
   RUN under_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Uppfljning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Uppfljning WAONR
ON CHOOSE OF MENU-ITEM m_Uppfljning /* Uppföljning */
DO:
   vartpro = "UPP".
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa WAONR
ON CHOOSE OF MENU-ITEM m_Visa /* Visa */
DO:
    RUN visao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_avslutade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_avslutade WAONR
ON CHOOSE OF MENU-ITEM m_Visa_avslutade /* Visa avslutade aonr */
DO:
   RUN visav_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST WAONR
ON VALUE-CHANGED OF RAD_FAST IN FRAME FRAME-A
DO:              
   RAD_FAST = INPUT RAD_FAST.   
   RUN fastaao_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AONR WAONR
ON VALUE-CHANGED OF TOG_AONR IN FRAME FRAME-A /* Visa alla aonr */
DO:              
   TOG_AONR = INPUT TOG_AONR.
   RUN allaao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WAONR 


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
   DEFINE VARIABLE status-mus2 AS LOGICAL NO-UNDO. 
status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("WAIT").

   RAD_FAST = FALSE.
   TOG_AONR = FALSE.
   RUN enable_UI.
    
   /*HOPPSEK.I*/
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.
xhop = "GURU".
PROCEDURE nextguru_UI:
   FIND FIRST XSEK WHERE XSEK.MENYVART = xhop AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.  
END PROCEDURE.

   RUN nextguru_UI.
   IF XSEK.SEK[1] = FALSE THEN DO:  
      MENU-ITEM m_Arbetsorderhantering:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[2] = FALSE THEN DO:
      MENU-ITEM m_Materielhantering:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[3] = FALSE THEN DO:
      MENU-ITEM m_Kalkylering:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[4] = FALSE THEN DO:
      MENU-ITEM m_Tidredovisning:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[5] = FALSE THEN DO:       
      IF (globforetag = "NORD" OR 
          Guru.Konstanter:globforetag = "GRAN" OR 
          Guru.Konstanter:globforetag = "GADM" OR 
          Guru.Konstanter:globforetag = "ROSL" OR 
          Guru.Konstanter:globforetag = "MALA" OR 
          Guru.Konstanter:globforetag = "GSYD" OR 
          Guru.Konstanter:globforetag = "SOLE")  
      THEN MENU-ITEM m_Flextid:SENSITIVE IN MENU m_G_till = FALSE. 
      ELSE Guru.Konstanter:globforetag = globforetag.
   END.       
   IF XSEK.SEK[6] = FALSE THEN DO:
      MENU-ITEM m_Uppfljning:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[7] = FALSE THEN DO:
      MENU-ITEM m_Personalregistrering:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[8] = FALSE THEN DO:
      MENU-ITEM m_Sekretess:SENSITIVE IN MENU m_G_till = FALSE.
   END.      
   IF XSEK.SEK[9] = FALSE THEN DO:
      MENU-ITEM m_Registerhantering:SENSITIVE IN MENU m_G_till = FALSE.
   END.          
run vismedD.w.
   xhop = "AONR".     

   RUN nextguru_UI.
   IF XSEK.SEK[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.
   ELSE MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = FALSE.
   IF XSEK.SEK[2] = TRUE THEN ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}. 
   ELSE MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = FALSE.
   IF XSEK.SEK[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
   ELSE MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = FALSE.    
   IF XSEK.SEK[4] = TRUE THEN ENABLE BTN_VISAO WITH FRAME {&FRAME-NAME}.     
   ELSE MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = FALSE.     
   IF XSEK.SEK[5] = TRUE THEN ENABLE BTN_UNDER WITH FRAME {&FRAME-NAME}.
   ELSE MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
   IF XSEK.SEK[6] = TRUE THEN ENABLE BTN_KALK WITH FRAME {&FRAME-NAME}. 
   ELSE MENU-ITEM m_Kalkylera:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
   IF XSEK.SEK[7] = TRUE THEN ENABLE BTN_RAPP WITH FRAME {&FRAME-NAME}.              
   ELSE MENU-ITEM m_Rapporter:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
   IF XSEK.SEK[8] = TRUE THEN ENABLE BTN_KOST WITH FRAME {&FRAME-NAME}.              
   ELSE MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
   IF XSEK.SEK[9] = TRUE THEN ENABLE BTN_AVSAONR WITH FRAME {&FRAME-NAME}.           
   ELSE MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
   IF XSEK.SEK[10] = TRUE THEN ENABLE BTN_VISAV WITH FRAME {&FRAME-NAME}.             
   ELSE MENU-ITEM m_Visa_avslutade:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
   IF Guru.Konstanter:globomr = "" THEN DO:
      TOG_AONR:HIDDEN = TRUE.
      TOG_AONR = TRUE.
   END.
   ELSE DO:
      TOG_AONR = globallao.
      DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
   END.   
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   IF TOG_AONR = TRUE THEN DO: 
      OPEN QUERY BRW_AONR
      FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      OPEN QUERY BRW_AONR 
      FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      BRW_AONR:HIDDEN = FALSE.
      BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
      /*APPLY "HOME" TO {&BROWSE-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      */
   END.
   ELSE DO:                                                
      ASSIGN
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO. 
      BRW_AONR:HIDDEN = TRUE.
      DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.
   END.      
 
   my1hand = BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}. 
   status-ok = RAD_FAST:MOVE-AFTER-TAB-ITEM(my1hand).                     
  status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("arrow").

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI WAONR 
PROCEDURE allaao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF TOG_AONR = TRUE THEN DO: 
      OPEN QUERY BRW_AONR
      FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      OPEN QUERY BRW_AONR 
      FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      /*APPLY "HOME" TO {&BROWSE-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW(). */
      {AONRSEK.I} 
      ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
      /*ASSIGN
      BRW_AONR:HIDDEN = FALSE
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES. 
      ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER
      BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.*/
   END.
   ELSE DO:                                              
      ASSIGN
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO.      
      BRW_AONR:HIDDEN = TRUE.
      DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WAONR 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   {muswait.i}       
   
   RUN F:\PRO8\GURU\WX\XANDAONR.W.
   IF musz = FALSE THEN RUN ANMARK.W.
   IF musz = FALSE THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "NORD" THEN RUN AOFAKUP.W.
   END.
   IF musz = FALSE THEN RUN AONRKONT.W.
   IF musz = FALSE THEN RUN AONR%F.W.    
   musz = FALSE.
   FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
   IF AONRTAB.OMRADE = globomr THEN DO:   
      IF TOG_AONR = TRUE 
      THEN OPEN QUERY BRW_AONR FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      ELSE OPEN QUERY BRW_AONR FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND 
      AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
   END.
   ELSE DO:
      TOG_AONR = TRUE.
      IF globomr NE "" THEN DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      OPEN QUERY BRW_AONR FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION. 
   END.
   REPOSITION {&BROWSE-NAME} TO RECID aonrrec.
   

   {musarrow.i}
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI WAONR 
PROCEDURE avsluta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF TOG_AONR = TRUE THEN DO: 
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      aonrrec = RECID(AONRTAB).
      aonrrec2 = aonrrec.   
      RUN AONRAVS.W.
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF Guru.Konstanter:globomr = "" THEN DO:
         TOG_AONR:HIDDEN = TRUE.      
      END. 
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.
      /*
      ELSE IF Guru.Konstanter:globallao = TRUE THEN DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.  
      END.
      */ 
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      IF TOG_AONR = TRUE THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.     
      IF AVAILABLE AONRTAB THEN DO:
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         /*APPLY "HOME" TO {&BROWSE-NAME}.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/
         {AONRSEK.I} 
         ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
         /*
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES          
         ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB 
         BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}. */  
         
      END.
      ELSE DO:
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO 
         BRW_AONR:HIDDEN = TRUE.
         DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.
      END.
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.     
      IF AONRTAB.FASTAAONR = RAD_FAST AND AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:      
         IF TOG_AONR = TRUE THEN DO:
            REPOSITION {&BROWSE-NAME} TO RECID aonrrec.  
            status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
         END.
         ELSE DO: 
            IF AONRTAB.OMRADE = globomr THEN DO:
               REPOSITION {&BROWSE-NAME} TO RECID aonrrec.  
               status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
            END.
            ELSE DO:
               APPLY "HOME" TO BRW_AONR.
            END.   
         END.
      END. 
      ELSE DO:
         APPLY "HOME" TO BRW_AONR.
      END.
   END.
   ELSE DO:
      RUN AONRAVS.W. 
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF Guru.Konstanter:globomr = "" THEN DO:
         TOG_AONR:HIDDEN = TRUE.      
      END.
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.  
      /*
      ELSE IF Guru.Konstanter:globallao = TRUE THEN DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.  
      END. 
      */  
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      IF TOG_AONR = TRUE THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.  
      IF AVAILABLE AONRTAB THEN DO:
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         /*APPLY "HOME" TO {&BROWSE-NAME}.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().  */
         {AONRSEK.I}
        /*
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP
         WITH FRAME {&FRAME-NAME}.*/      
         ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:                                                 
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
         BRW_AONR:HIDDEN = TRUE.
         DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP
         WITH FRAME {&FRAME-NAME}.
      END.
   END.   
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WAONR 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   aonrrec2 = aonrrec.       
   RUN BORTAONR.W.
   {muswait.i}
   IF musz = FALSE THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN aonrrec2 = RECID(AONRTAB).
      ELSE DO:          
         status-ok = {&BROWSE-NAME}:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN aonrrec2 = RECID(AONRTAB).          
         ELSE aonrrec2 = aonrrec.
      END.
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.     
      DEFINE QUERY aonrkq FOR AONRKONT.
      OPEN QUERY aonrkq FOR EACH AONRKONT WHERE AONRKONT.AONR = AONRTAB.AONR AND
      AONRKONT.DELNR = AONRTAB.DELNR USE-INDEX AONRKONT NO-LOCK.
      DO TRANSACTION:       
         GET FIRST aonrkq EXCLUSIVE-LOCK.
         IF AVAILABLE AONRKONT THEN DELETE AONRKONT.    
      END.
      REPEAT:  
         DO TRANSACTION:
            GET NEXT aonrkq EXCLUSIVE-LOCK.
            IF AVAILABLE AONRKONT THEN DELETE AONRKONT.    
            ELSE LEAVE.      
         END.         
      END.   
      DEFINE QUERY kostq FOR KOSTREG.
      OPEN QUERY kostq FOR EACH KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND
      KOSTREG.DELNR = AONRTAB.DELNR USE-INDEX KOST NO-LOCK.
      DO TRANSACTION:       
         GET FIRST kostq EXCLUSIVE-LOCK.
         IF AVAILABLE KOSTREG THEN DELETE KOSTREG.    
      END.
      REPEAT:  
         DO TRANSACTION:
            GET NEXT kostq EXCLUSIVE-LOCK.
            IF AVAILABLE KOSTREG THEN DELETE KOSTREG.    
            ELSE LEAVE.      
         END.         
      END. 
      DEFINE QUERY kalspecq FOR KALKSPEC.
      OPEN QUERY kalspecq FOR EACH KALKSPEC WHERE KALKSPEC.AONR = AONRTAB.AONR AND
      KALKSPEC.DELNR = AONRTAB.DELNR USE-INDEX AONR NO-LOCK.
      DO TRANSACTION:       
         GET FIRST kalspecq EXCLUSIVE-LOCK.
         IF AVAILABLE KALKSPEC THEN DO:
            ASSIGN KALKSPEC.AONR = "?"
            KALKSPEC.DELNR = ?.
         END.    
      END.
      REPEAT:  
         DO TRANSACTION:
            GET NEXT kalspecq EXCLUSIVE-LOCK.
            IF AVAILABLE KALKSPEC THEN DO:
               ASSIGN KALKSPEC.AONR = "?"
               KALKSPEC.DELNR = ?.
            END.    
            ELSE LEAVE.      
         END.         
      END. 
                                                                      
      DO TRANSACTION:
         FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec EXCLUSIVE-LOCK NO-ERROR.
         DELETE AONRTAB.  
      END.
      IF TOG_AONR = TRUE THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.  
      IF AVAILABLE AONRTAB THEN DO:
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         IF aonrrec2 NE aonrrec THEN      
         REPOSITION {&BROWSE-NAME} TO RECID aonrrec2.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      END.
      ELSE DO:                                                     
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO 
         BRW_AONR:HIDDEN = TRUE.
         DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.         
      END.     
   END.
   musz = FALSE. 
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WAONR _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WAONR.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WAONR _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY RAD_FAST TOG_AONR FILL-IN_AONR FILL-IN_ORT 
      WITH FRAME FRAME-A IN WINDOW WAONR.
  ENABLE RECT-4 RECT-5 RAD_FAST TOG_AONR RECT-20 BTN_AVSL FILL-IN_AONR BTN_SOKA 
         FILL-IN_ORT BTN_SOKB 
      WITH FRAME FRAME-A IN WINDOW WAONR.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WAONR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastaao_UI WAONR 
PROCEDURE fastaao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/        
   IF Guru.Konstanter:globomr = "" THEN DO:
      TOG_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         
   END.
   ELSE DO:         
      DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.  
   END. 
      /*
      ELSE IF Guru.Konstanter:globallao = TRUE THEN DO:         
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:         
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.  
      END. 
      */  
   IF TOG_AONR = TRUE THEN DO: 
      OPEN QUERY BRW_AONR
      FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      OPEN QUERY BRW_AONR 
      FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.     
   IF AVAILABLE AONRTAB THEN DO:
      ASSIGN
      BRW_AONR:HIDDEN = FALSE.
      BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
      {AONRSEK.I} 
      ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
      /*
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES. 
      ENABLE BTN_VISAO BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO 
      WITH FRAME {&FRAME-NAME}.        
      */
      /*APPLY "HOME" TO {&BROWSE-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/
   END.
   ELSE DO:                                              
      ASSIGN
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO     
      BRW_AONR:HIDDEN = TRUE.
      DISABLE BTN_VISAO BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalk_UI WAONR 
PROCEDURE kalk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF TOG_AONR = TRUE THEN DO: 
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      aonrrec = RECID(AONRTAB).
      aonrrec2 = aonrrec.
   END.       
   RUN KALKREG.W.
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   IF Guru.Konstanter:globomr = "" THEN DO:
      TOG_AONR:HIDDEN = TRUE. 
      DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:
      DISPLAY TOG_AONR RAD_FAST WITH FRAME {&FRAME-NAME}.
   END.                      
   IF TOG_AONR = TRUE THEN DO: 
      OPEN QUERY BRW_AONR
      FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      OPEN QUERY BRW_AONR 
      FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.                          
      BRW_AONR:HIDDEN = FALSE.
      BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
      /*APPLY "HOME" TO {&BROWSE-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW(). */
      {AONRSEK.I}
      /*
      ASSIGN
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES 
      BRW_AONR:HIDDEN = FALSE.
      BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
      ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.
      */
      ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec2 NO-LOCK NO-ERROR.
      IF AONRTAB.FASTAAONR = RAD_FAST AND AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:      
         IF TOG_AONR = TRUE THEN DO:
            REPOSITION {&BROWSE-NAME} TO RECID aonrrec2.  
            status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
         END.
         ELSE DO: 
            IF AONRTAB.OMRADE = globomr THEN DO:
               REPOSITION {&BROWSE-NAME} TO RECID aonrrec2.  
               status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
            END.
            ELSE DO:
               musz = musz.
               /*APPLY "HOME" TO BRW_AONR.*/
            END.   
         END.
      END.
      ELSE DO:
         musz = musz.
         /*APPLY "HOME" TO BRW_AONR.*/
      END.
   END.
   ELSE DO:                                              
      ASSIGN
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO 
      BRW_AONR:HIDDEN = TRUE.
      DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.
   END.
  
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kost_UI WAONR 
PROCEDURE kost_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   {muswait.i}
   RUN KOSTAND.W.         
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WAONR 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
   AONRTAB.AONRAVDATUM = 01/01/1991 
   USE-INDEX OMRADE NO-LOCK NO-ERROR.
   IF AVAILABLE AONRTAB THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      aonrrec = RECID(AONRTAB).
      aonrrec2 = aonrrec.
   END.    
   {muswait.i}
   RUN NYTTAONR.W.
   IF musz = FALSE THEN RUN ANMARK.W.
   IF musz = FALSE THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "NORD" THEN RUN AOFAKUP.W.
   END.
   IF musz = FALSE THEN RUN AONRKONT.W.
   IF musz = FALSE THEN RUN AONR%F.W.    
   {musarrow.i} 
   IF musz = FALSE THEN DO:
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.    
      IF AONRTAB.OMRADE = globomr THEN DO:     
         IF TOG_AONR = TRUE THEN 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         ELSE 
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.      
      END.  
      ELSE DO:          
         TOG_AONR = TRUE.
         IF globomr NE "" THEN DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      END.
      ASSIGN      
      BRW_AONR:HIDDEN = FALSE    
      {AONRSEK.I}
      /*
      MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
      MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
      MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
      MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES. 
      ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER
      BTN_UPP BTN_VISAO
      WITH FRAME {&FRAME-NAME}.*/
      ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
      VIEW WAONR.    
      REPOSITION {&BROWSE-NAME} TO RECID aonrrec.
   END.
   musz = FALSE.
   APPLY "ENTRY" TO BRW_AONR IN FRAME {&FRAME-NAME}. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp_UI WAONR 
PROCEDURE rapp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   RUN AORAPP.W.
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE under_UI WAONR 
PROCEDURE under_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).
   FIND LAST aonrbuff WHERE  aonrbuff.OMRADE = AONRTAB.OMRADE AND 
   aonrbuff.AONR = AONRTAB.AONR USE-INDEX OMRADE NO-LOCK NO-ERROR.
   hjdelvar = aonrbuff.DELNR.
   FIND LAST AVAONR WHERE  AVAONR.OMRADE = AONRTAB.OMRADE AND 
   AVAONR.AONR = AONRTAB.AONR USE-INDEX OMRADE NO-LOCK NO-ERROR.
   IF AVAILABLE AVAONR THEN DO:
      IF hjdelvar < AVAONR.DELNR THEN hjdelvar = AVAONR.DELNR.
   END.   
   FIND aonrbuff WHERE RECID(aonrbuff) = aonrrec NO-LOCK NO-ERROR.  
   IF AVAILABLE aonrbuff THEN DO TRANSACTION:
      CREATE AONRTAB.
      ASSIGN 
      AONRTAB.AONR = aonrbuff.AONR
      AONRTAB.DELNR = hjdelvar + 1
      AONRTAB.OMRADE = aonrbuff.OMRADE
      AONRTAB.ANLNR = aonrbuff.ANLNR
      AONRTAB.PRISTYP = aonrbuff.PRISTYP
      AONRTAB.AUTOREG = TRUE
      AONRTAB.ORT = aonrbuff.ORT
      AONRTAB.ARBUPPG[1] = aonrbuff.ARBUPPG[1]
      AONRTAB.ARBUPPG[2] = aonrbuff.ARBUPPG[2]
      AONRTAB.TRAKTAMENTE = aonrbuff.TRAKTAMENTE
      AONRTAB.UTRYCKNING = aonrbuff.UTRYCKNING
      AONRTAB.ARBARTKOD = aonrbuff.ARBARTKOD
      AONRTAB.PKOD = aonrbuff.PKOD
      AONRTAB.STARTVNR = aonrbuff.STARTVNR   
      AONRTAB.STARTDAG = aonrbuff.STARTDAG
      AONRTAB.SLUTVNR = aonrbuff.SLUTVNR
      AONRTAB.SLUTDAG = aonrbuff.SLUTDAG
      AONRTAB.BESTID = aonrbuff.BESTID
      AONRTAB.BEREDARE = aonrbuff.BEREDARE
      AONRTAB.ARBANSVARIG = aonrbuff.ARBANSVARIG
      AONRTAB.UTFARDAT = aonrbuff.UTFARDAT
      AONRTAB.ARBBESKED = aonrbuff.ARBBESKED
      AONRTAB.FASTAAONR = aonrbuff.FASTAAONR
      AONRTAB.ANM[1] = aonrbuff.ANM[1]   
      AONRTAB.ANM[2] = aonrbuff.ANM[2]  
      AONRTAB.ANM[3] = aonrbuff.ANM[3]  
      AONRTAB.ANM[4] = aonrbuff.ANM[4] 
      AONRTAB.ANM[5] = aonrbuff.ANM[5]  
      AONRTAB.ANM[6] = aonrbuff.ANM[6].
      aonrrec = RECID(AONRTAB).
      VALIDATE AONRTAB. 
      FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = aonrbuff.AONR AND 
      AONRKONTKOD.DELNR = aonrbuff.DELNR USE-INDEX AONRKONT:
         CREATE aonrkontbuff.
         ASSIGN 
         aonrkontbuff.AONR = AONRTAB.AONR
         aonrkontbuff.DELNR = AONRTAB.DELNR
         aonrkontbuff.K1 = AONRKONTKOD.K1
         aonrkontbuff.K2 = AONRKONTKOD.K2
         aonrkontbuff.K3 = AONRKONTKOD.K3
         aonrkontbuff.K4 = AONRKONTKOD.K4
         aonrkontbuff.K5 = AONRKONTKOD.K5
         aonrkontbuff.SATS% = AONRKONTKOD.SATS%.
      END.   
      MESSAGE "Underindelat aonr lagrat. Gör ändringar via -UPPDATERA-"
      VIEW-AS ALERT-BOX.             
      IF TOG_AONR = TRUE THEN DO:
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND
         AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
      END.
   END.
   REPOSITION {&BROWSE-NAME} TO RECID aonrrec.
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visao_UI WAONR 
PROCEDURE visao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(AONRTAB).  
   CREATE aonrtemp.
   ASSIGN aonrtemp.AONR = AONRTAB.AONR
   aonrtemp.DELNR = AONRTAB.DELNR
   aonrtemp.OMRADE = AONRTAB.OMRADE
   aonrtemp.AONRTABREC = RECID(AONRTAB).              
   RUN VISAAO.W.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visav_UI WAONR 
PROCEDURE visav_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   IF TOG_AONR = TRUE THEN DO: 
      FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
      AONRTAB.AONRAVDATUM = 01/01/1991 
      USE-INDEX OMRADE NO-LOCK NO-ERROR.
   END.  
   IF AVAILABLE AONRTAB THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      aonrrec = RECID(AONRTAB).
      aonrrec2 = aonrrec.   
      RUN AONRTAVS.W.
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF Guru.Konstanter:globomr = "" THEN DO:
         TOG_AONR:HIDDEN = TRUE.      
      END.
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END.       
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      IF TOG_AONR = TRUE THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.     
      IF AVAILABLE AONRTAB THEN DO:
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         /*APPLY "HOME" TO {&BROWSE-NAME}.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().  */
         {AONRSEK.I}       
         ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
         /*
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES 
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.                         */
      END.
      ELSE DO:                                              
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO  
         BRW_AONR:HIDDEN = TRUE.
         DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.
      END.
      FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.     
      IF AONRTAB.FASTAAONR = RAD_FAST AND AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:      
         IF TOG_AONR = TRUE THEN DO:
            REPOSITION {&BROWSE-NAME} TO RECID aonrrec.  
            status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
         END.
         ELSE DO: 
            IF AONRTAB.OMRADE = globomr THEN DO:
               REPOSITION {&BROWSE-NAME} TO RECID aonrrec.  
               status-ok = BRW_AONR:SELECT-FOCUSED-ROW(). 
            END.
            ELSE DO:
               APPLY "HOME" TO BRW_AONR.
            END.   
         END.
      END.   
      ELSE DO:
         APPLY "HOME" TO BRW_AONR.
      END.
   END.
   ELSE DO:
      RUN AONRTAVS.W. 
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF vartpro NE "" THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
      END.
      IF Guru.Konstanter:globomr = "" THEN DO:
         TOG_AONR:HIDDEN = TRUE.      
      END.
      ELSE DO:
         DISPLAY TOG_AONR WITH FRAME {&FRAME-NAME}.
      END. 
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      IF TOG_AONR = TRUE THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST AONRTAB WHERE AONRTAB.OMRADE = globomr AND AONRTAB.FASTAAONR = RAD_FAST AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.     
      IF AVAILABLE AONRTAB THEN DO:
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         /*APPLY "HOME" TO {&BROWSE-NAME}.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/    
         {AONRSEK.I}       
         ENABLE BTN_SOKA BTN_SOKB WITH FRAME {&FRAME-NAME}.
         /*
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = YES 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = YES
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = YES 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = YES 
         BRW_AONR:HIDDEN = FALSE.
         BRW_AONR:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 1000.
         ENABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.
         */
      END.
      ELSE DO:                                              
         ASSIGN
         MENU-ITEM m_Avsluta_Aonr:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Kostnadsregistrering:SENSITIVE IN MENU m_Funktioner2 = NO 
         MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = NO
         MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = NO 
         MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = NO 
         BRW_AONR:HIDDEN = TRUE.
         DISABLE BTN_AVSAONR BTN_KOST BTN_BORT BTN_SOKA BTN_SOKB BTN_UNDER BTN_UPP BTN_VISAO
         WITH FRAME {&FRAME-NAME}.
      END.
   END.   
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
