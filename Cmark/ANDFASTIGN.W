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

  Created: 04/15/97 -  9:24 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */


/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{SOKDEF.I}
&Scoped-define SHARED SHARED  
{FASTIGHET.I}
{LANKOM.I}

FIND FIRST komuntemp NO-LOCK NO-ERROR.
IF NOT AVAILABLE komuntemp THEN DO:
   RUN LANKOM.P.
END.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE orade AS CHARACTER NO-UNDO.
DEFINE VARIABLE mnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
DEFINE VARIABLE andfastigapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

DEFINE INPUT PARAMETER valdlan AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valdkommun AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE efastighettemp NO-UNDO LIKE fastighettemp.
DEFINE TEMP-TABLE kfastighettemp NO-UNDO LIKE fastighettemp.

DEFINE INPUT-OUTPUT PARAMETER fastighbet AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR kfastighettemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_BETECKNING SEL-LAN FILL-IN_LAN ~
FILL-IN_KOMMUN FILL-IN_SOCKEN CMB_PAKER CMB_VSKOG CMB_GOMRTALL CMB_GOMRGRAN ~
FILL-IN_LOPNR FILL-IN-ANMARK FILL-IN_ARRENDATOR SEL-KOMMUN FILL-IN_ANAMN ~
FILL-IN_APERNR FILL-IN_AADRESS FILL-IN_APONR FILL-IN_APADRESS ~
FILL-IN_ABOXEN FILL-IN_ATELEFON FILL-IN_ATELEFON2 FILL-IN_ATELEFONM ~
FILL-IN_AMAIL BTN_OK BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_BETECKNING SEL-LAN FILL-IN_LAN ~
FILL-IN_KOMMUN FILL-IN_SOCKEN CMB_PAKER CMB_VSKOG CMB_GOMRTALL CMB_GOMRGRAN ~
FILL-IN_LOPNR FILL-IN-ANMARK FILL-IN_ARRENDATOR SEL-KOMMUN FILL-IN_ANAMN ~
FILL-IN_APERNR FILL-IN_AADRESS FILL-IN_APONR FILL-IN_APADRESS ~
FILL-IN_ABOXEN FILL-IN_ATELEFON FILL-IN_ATELEFON2 FILL-IN_ATELEFONM ~
FILL-IN_AMAIL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_GOMRGRAN AS CHARACTER FORMAT "X(16)":U 
     LABEL "Geografiskt område Gran/Björk" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Norra" 
     DROP-DOWN-LIST
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_GOMRTALL AS CHARACTER FORMAT "X(16)":U 
     LABEL "Geografiskt område tall" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Norra","Mellan" 
     DROP-DOWN-LIST
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PAKER AS CHARACTER FORMAT "X(30)":U 
     LABEL "Produktionsområde åker" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 32.13 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VSKOG AS CHARACTER FORMAT "X(8)":U 
     LABEL "Värderingsområde skog" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANMARK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Anmärkning" 
     VIEW-AS FILL-IN 
     SIZE 41.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KONTAKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 41.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AADRESS AS CHARACTER FORMAT "X(256)" 
     LABEL "Adress / telefon" 
     VIEW-AS FILL-IN 
     SIZE 41.38 BY 1.

DEFINE VARIABLE FILL-IN_ABOXEN AS CHARACTER FORMAT "x(8)" 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_AMAIL AS CHARACTER FORMAT "x(30)" 
     LABEL "Mail" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.

DEFINE VARIABLE FILL-IN_ANAMN AS CHARACTER FORMAT "X(256)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 29.38 BY 1.

DEFINE VARIABLE FILL-IN_APADRESS AS CHARACTER FORMAT "X(20)" 
     LABEL "Postadress" 
     VIEW-AS FILL-IN 
     SIZE 25.5 BY 1.

DEFINE VARIABLE FILL-IN_APERNR AS CHARACTER FORMAT "999999-9999" INITIAL "0000000000" 
     LABEL "Personnummer" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_APONR AS CHARACTER FORMAT "X(8)" 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_ARRENDATOR AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Arrendator" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ATELEFON AS CHARACTER FORMAT "x(11)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_ATELEFON2 AS CHARACTER FORMAT "x(30)" 
     LABEL "Telefon arb" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.

DEFINE VARIABLE FILL-IN_ATELEFONM AS CHARACTER FORMAT "x(30)" 
     LABEL "Telefon mobil" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.

DEFINE VARIABLE FILL-IN_BETECKNING AS CHARACTER FORMAT "X(54)" 
     LABEL "Fastighetsbeteckning" 
     VIEW-AS FILL-IN 
     SIZE 40.63 BY 1.

DEFINE VARIABLE FILL-IN_KOMMUN AS CHARACTER FORMAT "X(20)" 
     LABEL "Kommun" 
     VIEW-AS FILL-IN 
     SIZE 22.25 BY 1.

DEFINE VARIABLE FILL-IN_LAN AS CHARACTER FORMAT "X(20)" 
     LABEL "Län" 
     VIEW-AS FILL-IN 
     SIZE 22.25 BY 1.

DEFINE VARIABLE FILL-IN_LOPNR AS CHARACTER FORMAT "X(256)" 
     LABEL "Löpnr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_SOCKEN AS CHARACTER FORMAT "X(20)" 
     LABEL "Socken" 
     VIEW-AS FILL-IN 
     SIZE 22.25 BY 1.

DEFINE VARIABLE SEL-KOMMUN AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 10.75 NO-UNDO.

DEFINE VARIABLE SEL-LAN AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "   " 
     SIZE 24 BY 10.21 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_BETECKNING AT ROW 1.38 COL 31 COLON-ALIGNED
     SEL-LAN AT ROW 2.29 COL 76.5 NO-LABEL
     FILL-IN_LAN AT ROW 2.42 COL 31 COLON-ALIGNED
     FILL-IN_KOMMUN AT ROW 3.46 COL 31 COLON-ALIGNED
     FILL-IN_SOCKEN AT ROW 4.5 COL 31 COLON-ALIGNED
     CMB_PAKER AT ROW 5.54 COL 31 COLON-ALIGNED
     CMB_VSKOG AT ROW 6.54 COL 31 COLON-ALIGNED
     CMB_GOMRTALL AT ROW 7.54 COL 31 COLON-ALIGNED
     CMB_GOMRGRAN AT ROW 8.54 COL 31 COLON-ALIGNED
     FILL-IN_LOPNR AT ROW 9.54 COL 31 COLON-ALIGNED
     FILL-IN-ANMARK AT ROW 10.58 COL 31 COLON-ALIGNED
     FILL-IN-KONTAKT AT ROW 11.63 COL 31 COLON-ALIGNED
     FILL-IN_ARRENDATOR AT ROW 12.67 COL 31 COLON-ALIGNED
     SEL-KOMMUN AT ROW 13.54 COL 76.5 NO-LABEL
     FILL-IN_ANAMN AT ROW 13.71 COL 31 COLON-ALIGNED
     FILL-IN_APERNR AT ROW 14.75 COL 31 COLON-ALIGNED
     FILL-IN_AADRESS AT ROW 15.79 COL 31 COLON-ALIGNED
     FILL-IN_APONR AT ROW 16.83 COL 31 COLON-ALIGNED
     FILL-IN_APADRESS AT ROW 17.88 COL 31 COLON-ALIGNED
     FILL-IN_ABOXEN AT ROW 18.92 COL 31 COLON-ALIGNED
     FILL-IN_ATELEFON AT ROW 19.96 COL 31 COLON-ALIGNED HELP
          "HEMTELE"
     FILL-IN_ATELEFON2 AT ROW 21 COL 31 COLON-ALIGNED HELP
          "ARB.TELE"
     FILL-IN_ATELEFONM AT ROW 22.04 COL 31 COLON-ALIGNED HELP
          "ARB.TELE"
     FILL-IN_AMAIL AT ROW 23.08 COL 31 COLON-ALIGNED HELP
          "ARB.TELE"
     BTN_OK AT ROW 24.75 COL 71.5
     BTN_AVSL AT ROW 24.75 COL 86.5
     "Kommun" VIEW-AS TEXT
          SIZE 10.5 BY .63 AT ROW 12.79 COL 76.5
     "Län" VIEW-AS TEXT
          SIZE 14 BY .63 AT ROW 1.54 COL 76.5
     SPACE(10.87) SKIP(23.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera/ändra fastighet".


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

/* SETTINGS FOR FILL-IN FILL-IN-KONTAKT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-KONTAKT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Registrera/ändra fastighet */
DO:  
   IF VALID-HANDLE(andfastigapph) THEN DELETE PROCEDURE andfastigapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Registrera/ändra fastighet */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:                                
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   RUN klar_UI.
   {musarrow.i}    
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(andfastigapph) THEN DELETE PROCEDURE andfastigapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_GOMRGRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_GOMRGRAN DIALOG-1
ON LEAVE OF CMB_GOMRGRAN IN FRAME DIALOG-1 /* Geografiskt område Gran/Björk */
DO:
   CMB_GOMRTALL = INPUT CMB_GOMRTALL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_GOMRGRAN DIALOG-1
ON VALUE-CHANGED OF CMB_GOMRGRAN IN FRAME DIALOG-1 /* Geografiskt område Gran/Björk */
DO:
   CMB_GOMRGRAN = INPUT CMB_GOMRGRAN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_GOMRTALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_GOMRTALL DIALOG-1
ON LEAVE OF CMB_GOMRTALL IN FRAME DIALOG-1 /* Geografiskt område tall */
DO:
   CMB_GOMRTALL = INPUT CMB_GOMRTALL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_GOMRTALL DIALOG-1
ON VALUE-CHANGED OF CMB_GOMRTALL IN FRAME DIALOG-1 /* Geografiskt område tall */
DO:
   CMB_GOMRTALL = INPUT CMB_GOMRTALL. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PAKER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PAKER DIALOG-1
ON LEAVE OF CMB_PAKER IN FRAME DIALOG-1 /* Produktionsområde åker */
DO:
   CMB_PAKER = INPUT CMB_PAKER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PAKER DIALOG-1
ON VALUE-CHANGED OF CMB_PAKER IN FRAME DIALOG-1 /* Produktionsområde åker */
DO:
   CMB_PAKER = INPUT CMB_PAKER. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VSKOG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VSKOG DIALOG-1
ON LEAVE OF CMB_VSKOG IN FRAME DIALOG-1 /* Värderingsområde skog */
DO:
   CMB_VSKOG = INPUT CMB_VSKOG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VSKOG DIALOG-1
ON VALUE-CHANGED OF CMB_VSKOG IN FRAME DIALOG-1 /* Värderingsområde skog */
DO:
   CMB_VSKOG = INPUT CMB_VSKOG. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ARRENDATOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARRENDATOR DIALOG-1
ON LEAVE OF FILL-IN_ARRENDATOR IN FRAME DIALOG-1 /* Arrendator */
DO:
   FILL-IN_ARRENDATOR = INPUT FILL-IN_ARRENDATOR.
   IF FILL-IN_ARRENDATOR = FALSE THEN DO:
      ASSIGN  FILL-IN_ANAMN:HIDDEN = TRUE
      FILL-IN_AADRESS:HIDDEN = TRUE
      FILL-IN_APERNR:HIDDEN = TRUE
      FILL-IN_APONR:HIDDEN = TRUE
      FILL-IN_APADRESS:HIDDEN = TRUE
      FILL-IN_ABOXEN:HIDDEN = TRUE 
      FILL-IN_ATELEFON:HIDDEN = TRUE 
      FILL-IN_ATELEFON2:HIDDEN = TRUE 
      FILL-IN_ATELEFONM:HIDDEN = TRUE
      FILL-IN_AMAIL:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN_ANAMN:HIDDEN = FALSE
      FILL-IN_AADRESS:HIDDEN = FALSE
      FILL-IN_APERNR:HIDDEN = FALSE
      FILL-IN_APONR:HIDDEN = FALSE
      FILL-IN_APADRESS:HIDDEN = FALSE
      FILL-IN_ABOXEN:HIDDEN = FALSE 
      FILL-IN_ATELEFON:HIDDEN = FALSE 
      FILL-IN_ATELEFON2:HIDDEN = FALSE 
      FILL-IN_ATELEFONM:HIDDEN = FALSE
      FILL-IN_AMAIL:HIDDEN = FALSE.
      DISPLAY FILL-IN_ARRENDATOR FILL-IN_ANAMN FILL-IN_AADRESS FILL-IN_APERNR FILL-IN_APONR FILL-IN_APADRESS
      FILL-IN_ABOXEN FILL-IN_ATELEFON FILL-IN_ATELEFON2 FILL-IN_ATELEFONM FILL-IN_AMAIL WITH FRAME {&FRAME-NAME}.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARRENDATOR DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_ARRENDATOR IN FRAME DIALOG-1 /* Arrendator */
DO:
  IF INPUT FILL-IN_ARRENDATOR = TRUE THEN FILL-IN_ARRENDATOR = FALSE.
   IF INPUT FILL-IN_ARRENDATOR = FALSE THEN FILL-IN_ARRENDATOR = TRUE.
   IF FILL-IN_ARRENDATOR = TRUE THEN DO:      
      ASSIGN  FILL-IN_ANAMN:HIDDEN = FALSE
      FILL-IN_AADRESS:HIDDEN = FALSE
      FILL-IN_APERNR:HIDDEN = FALSE
      FILL-IN_APONR:HIDDEN = FALSE
      FILL-IN_APADRESS:HIDDEN = FALSE
      FILL-IN_ABOXEN:HIDDEN = FALSE 
      FILL-IN_ATELEFON:HIDDEN = FALSE 
      FILL-IN_ATELEFON2:HIDDEN = FALSE 
      FILL-IN_ATELEFONM:HIDDEN = FALSE
      FILL-IN_AMAIL:HIDDEN = FALSE.
   END.   
   ELSE DO:          
      ASSIGN  FILL-IN_ANAMN:HIDDEN = TRUE
      FILL-IN_AADRESS:HIDDEN = TRUE
      FILL-IN_APERNR:HIDDEN = TRUE
      FILL-IN_APONR:HIDDEN = TRUE
      FILL-IN_APADRESS:HIDDEN = TRUE
      FILL-IN_ABOXEN:HIDDEN = TRUE 
      FILL-IN_ATELEFON:HIDDEN = TRUE 
      FILL-IN_ATELEFON2:HIDDEN = TRUE 
      FILL-IN_ATELEFONM:HIDDEN = TRUE
      FILL-IN_AMAIL:HIDDEN = TRUE.
   END.     
   DISPLAY FILL-IN_ARRENDATOR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BETECKNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BETECKNING DIALOG-1
ON LEAVE OF FILL-IN_BETECKNING IN FRAME DIALOG-1 /* Fastighetsbeteckning */
DO:
  ASSIGN
  FILL-IN_BETECKNING = INPUT FILL-IN_BETECKNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-KOMMUN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-KOMMUN DIALOG-1
ON VALUE-CHANGED OF SEL-KOMMUN IN FRAME DIALOG-1
DO:
  SEL-KOMMUN = INPUT SEL-KOMMUN.
   FILL-IN_KOMMUN = SEL-KOMMUN.   
   DISPLAY FILL-IN_KOMMUN WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-LAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-LAN DIALOG-1
ON VALUE-CHANGED OF SEL-LAN IN FRAME DIALOG-1
DO:
   SEL-LAN = INPUT SEL-LAN.
   FILL-IN_LAN = SEL-LAN.   
   DISPLAY FILL-IN_LAN WITH FRAME {&FRAME-NAME}.

   SEL-KOMMUN:list-items = "".   
   FIND FIRST lantemp WHERE lantemp.BENAMNING = FILL-IN_LAN USE-INDEX BENAMNING NO-LOCK NO-ERROR .   
   FOR EACH komuntemp WHERE komuntemp.LANID = lantemp.LANID USE-INDEX BENAMNING:
      SEL-KOMMUN:add-last(komuntemp.BENAMNING).
   END.

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
   {muswait.i} 
   {ALLSTARTDYN.I}
   orade = "".
   
   RUN ladda IN andfastigapph (OUTPUT TABLE pakerregtemp,
                               OUTPUT TABLE vskogregtemp,
                               OUTPUT TABLE volskogtemp,
                               OUTPUT TABLE akervardtemp).
   ASSIGN
   SEL-LAN:list-items = ""
   SEL-KOMMUN:list-items = "".
   
   OPEN QUERY oq FOR EACH pakerregtemp USE-INDEX PRODOMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(pakerregtemp):       
      IF pakerregtemp.PRODOMR = orade THEN musz = musz.
      ELSE DO:
         status-ok = CMB_PAKER:ADD-LAST(pakerregtemp.BENAMNING).
      END.  
      orade = pakerregtemp.PRODOMR.  
      GET NEXT oq NO-LOCK.      
   END.  
   IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" THEN DO:
      status-ok = CMB_GOMRGRAN:ADD-LAST("Södra"). 
      status-ok = CMB_GOMRTALL:ADD-LAST("Södra").
   END.   
   orade = "".
   OPEN QUERY vs FOR EACH vskogregtemp USE-INDEX VARDOMR NO-LOCK.
   GET FIRST vs NO-LOCK.
   DO WHILE AVAILABLE(vskogregtemp):       
      IF vskogregtemp.VARDOMR = orade THEN musz = musz.
      ELSE DO:
         status-ok = CMB_VSKOG:ADD-LAST(vskogregtemp.BENAMNING).
      END.  
      orade = vskogregtemp.VARDOMR.  
      GET NEXT vs NO-LOCK.      
   END.  
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING  = fastighbet NO-LOCK NO-ERROR.
   FIND FIRST pakerregtemp WHERE pakerregtemp.PRODOMR = fastighettemp.PAKER USE-INDEX PRODOMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE pakerregtemp THEN DO:
      FIND FIRST pakerregtemp USE-INDEX PRODOMR NO-LOCK NO-ERROR.
   END.
   ASSIGN  CMB_PAKER:SCREEN-VALUE = pakerregtemp.BENAMNING. 
  
   FIND FIRST vskogregtemp WHERE vskogregtemp.VARDOMR = fastighettemp.VSKOG USE-INDEX VARDOMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE vskogregtemp THEN DO:
      FIND FIRST vskogregtemp USE-INDEX VARDOMR NO-LOCK NO-ERROR.
   END.
   ASSIGN  CMB_VSKOG:SCREEN-VALUE = vskogregtemp.BENAMNING.
   IF vart = "NYA" THEN DO:    
      ASSIGN  CMB_GOMRTALL:SCREEN-VALUE = "NORRA". 
      ASSIGN  CMB_GOMRGRAN:SCREEN-VALUE = "NORRA".           
      IF valdlan NE "Alla" THEN DO:         
         FILL-IN_LAN = valdlan.       
      END.
      IF valdkommun NE "Alla" THEN DO:         
         FILL-IN_KOMMUN = valdkommun.        
      END.
      IF FILL-IN_KOMMUN = "" THEN DO:      
         FIND FIRST kfastighettemp NO-LOCK NO-ERROR.
         IF AVAILABLE kfastighettemp THEN DO:
            FIND FIRST vskogregtemp WHERE vskogregtemp.VARDOMR = kfastighettemp.VSKOG USE-INDEX VARDOMR NO-LOCK NO-ERROR.
            IF AVAILABLE vskogregtemp THEN DO:
               CMB_VSKOG = vskogregtemp.BENAMNING.
            END.
            FIND FIRST pakerregtemp WHERE pakerregtemp.PRODOMR = kfastighettemp.PAKER USE-INDEX PRODOMR NO-LOCK NO-ERROR.
            IF AVAILABLE pakerregtemp THEN DO:
               CMB_PAKER = pakerregtemp.BENAMNING.   
            END.
            ASSIGN
            FILL-IN_KOMMUN = kfastighettemp.KOMMUN
            FILL-IN_SOCKEN = kfastighettemp.SOCKEN                  
            FILL-IN_LAN = kfastighettemp.VAKER        
            CMB_GOMRTALL = kfastighettemp.GOMRTALL 
            CMB_GOMRGRAN = kfastighettemp.GOMRGRAN.       
         END.
      END.
      IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "SKEL" OR Guru.Konstanter:globforetag = "LULE"  THEN DO:
         ASSIGN  
         CMB_PAKER:SCREEN-VALUE = "Norrland övre"
         CMB_VSKOG:SCREEN-VALUE = "Norrland".
      END.
      IF Guru.Konstanter:globforetag = "KEWA" OR Guru.Konstanter:globforetag = "GULL" OR Guru.Konstanter:globforetag = "SVKK"  THEN DO:
         ASSIGN  
         CMB_PAKER:SCREEN-VALUE = "Mellersta Sveriges skogsbygder".         
      END.
      
      
          
      IF Guru.Konstanter:globforetag = "KRIN"  THEN DO:
         ASSIGN  
         CMB_PAKER:SCREEN-VALUE = "Götalands södra slättbygder"
         CMB_VSKOG:SCREEN-VALUE = "Götaland".
         CMB_GOMRGRAN:SCREEN-VALUE = "Södra".
         CMB_GOMRTALL:SCREEN-VALUE = "Södra".
      END.
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:                     
         ASSIGN  
         CMB_PAKER:SCREEN-VALUE = "Norrland nedre"
         CMB_VSKOG:SCREEN-VALUE = "Norrland".
      END.   
      IF Guru.Konstanter:globforetag = "reji" THEN DO:
         IF Guru.Konstanter:globanv = "LIS" OR Guru.Konstanter:globanv = "USJ" OR  Guru.Konstanter:globanv = "jhx" or Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) 
         THEN DO:            
            ASSIGN  
            CMB_PAKER:SCREEN-VALUE = "Norrland nedre"
            CMB_VSKOG:SCREEN-VALUE = "Norrland".
         END.
         ELSE DO:
            ASSIGN
            CMB_PAKER:SCREEN-VALUE = "Götalands skogsbygder"
            CMB_VSKOG:SCREEN-VALUE = "Norrland".
         END.       
      END.   
   END.    
   ELSE DO:         
      ASSIGN
      FILL-IN_BETECKNING = fastighettemp.BETECKNING
      FILL-IN_KOMMUN = fastighettemp.KOMMUN
      FILL-IN_SOCKEN = fastighettemp.SOCKEN
      FILL-IN_ARRENDATOR = fastighettemp.ARRENDATOR     
      FILL-IN_ANAMN = SUBSTRING(fastighettemp.ANAMN,1,30) 
      FILL-IN-ANMARK = SUBSTRING(fastighettemp.ANAMN,31,80) 
      FILL-IN_APERNR = fastighettemp.APERNR
      FILL-IN_AADRESS = fastighettemp.AADRESS
      FILL-IN_APONR = fastighettemp.APONR
      FILL-IN_APADRESS = fastighettemp.APADRESS
      CMB_PAKER = pakerregtemp.BENAMNING
      FILL-IN_LAN = fastighettemp.VAKER
      CMB_VSKOG = vskogregtemp.BENAMNING
      CMB_GOMRTALL = fastighettemp.GOMRTALL 
      CMB_GOMRGRAN = fastighettemp.GOMRGRAN.       
   END.   
   FILL-IN-KONTAKT = "".
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FASTLOPNR"                   
   inextradatatemp.HUVUDCH = FILL-IN_BETECKNING.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      FILL-IN_LOPNR = extradatatemp.SOKCHAR[1]
      FILL-IN_ABOXEN = extradatatemp.SOKCHAR[2]
      FILL-IN_ATELEFON = extradatatemp.SOKCHAR[3]
      FILL-IN_ATELEFON2 = extradatatemp.SOKCHAR[4]
      FILL-IN_ATELEFONM = extradatatemp.SOKCHAR[5]
      FILL-IN_AMAIL = extradatatemp.SOKCHAR[6].
      IF  extradatatemp.SOKINT[1] > 0 THEN DO:      
         RUN kontakt IN andfastigapph (INPUT extradatatemp.SOKINT[1],OUTPUT mnamn).
         FILL-IN-KONTAKT = STRING(extradatatemp.SOKINT[1]) + " " + mnamn.
      END.
   END. 
   FOR EACH lantemp USE-INDEX BENAMNING:
      SEL-LAN:ADD-LAST(lantemp.BENAMNING).
   END.      
   IF FILL-IN_LAN NE "" THEN DO:   
      FIND FIRST lantemp WHERE lantemp.BENAMNING = FILL-IN_LAN USE-INDEX BENAMNING NO-LOCK NO-ERROR .
   END.
   IF NOT AVAILABLE lantemp THEN DO:   
      FIND FIRST lantemp USE-INDEX BENAMNING NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE lantemp THEN DO:      
      SEL-LAN = lantemp.BENAMNING.      
      ASSIGN SEL-LAN:SCREEN-VALUE = lantemp.BENAMNING.   
   END.
   FOR EACH komuntemp WHERE komuntemp.LANID = lantemp.LANID USE-INDEX BENAMNING:
      SEL-KOMMUN:ADD-LAST(komuntemp.BENAMNING).
   END.
   IF FILL-IN_KOMMUN NE "" THEN DO:   
      FIND FIRST komuntemp WHERE komuntemp.BENAMNING = FILL-IN_KOMMUN USE-INDEX BENAMNING NO-LOCK NO-ERROR .
   END.   
   IF AVAILABLE komuntemp THEN DO:      
      SEL-KOMMUN = komuntemp.BENAMNING.      
      ASSIGN SEL-KOMMUN:SCREEN-VALUE = komuntemp.BENAMNING.   
   END.

   {musarrow.i}
   RUN enable_UI.
   ASSIGN 
   CMB_VSKOG:HIDDEN = TRUE. 
   /*CMB_GOMRTALL:HIDDEN = TRUE
   CMB_GOMRGRAN:HIDDEN = TRUE.*/
   
   
   {FRMSIZED.I} 
   IF FILL-IN_ARRENDATOR = TRUE THEN DO:
      ASSIGN
      FILL-IN_ANAMN:HIDDEN = FALSE 
      FILL-IN_APERNR:HIDDEN = FALSE
      FILL-IN_AADRESS:HIDDEN = FALSE
      FILL-IN_APONR:HIDDEN = FALSE
      FILL-IN_APADRESS:HIDDEN = FALSE
      FILL-IN_ABOXEN:HIDDEN = FALSE 
      FILL-IN_ATELEFON:HIDDEN = FALSE 
      FILL-IN_ATELEFON2:HIDDEN = FALSE 
      FILL-IN_ATELEFONM:HIDDEN = FALSE
      FILL-IN_AMAIL:HIDDEN = FALSE.
   END.    
   ELSE DO:
      ASSIGN
      FILL-IN_ANAMN:HIDDEN = TRUE 
      FILL-IN_APERNR:HIDDEN = TRUE
      FILL-IN_AADRESS:HIDDEN = TRUE
      FILL-IN_APONR:HIDDEN = TRUE
      FILL-IN_APADRESS:HIDDEN = TRUE
      FILL-IN_ABOXEN:HIDDEN = TRUE 
      FILL-IN_ATELEFON:HIDDEN = TRUE 
      FILL-IN_ATELEFON2:HIDDEN = TRUE 
      FILL-IN_ATELEFONM:HIDDEN = TRUE
      FILL-IN_AMAIL:HIDDEN = TRUE.
   END.             
   IF FILL-IN-KONTAKT NE "" THEN DISPLAY FILL-IN-KONTAKT WITH FRAME {&FRAME-NAME}.
   IF vart = "AND" THEN DISABLE FILL-IN_BETECKNING WITH FRAME {&FRAME-NAME}.
   {DIA_M_SLUT.I}   
      
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANDFASTIGUAPP.P PERSISTENT SET andfastigapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANDFASTIGUAPP.P PERSISTENT SET andfastigapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN_BETECKNING SEL-LAN FILL-IN_LAN FILL-IN_KOMMUN FILL-IN_SOCKEN 
          CMB_PAKER CMB_VSKOG CMB_GOMRTALL CMB_GOMRGRAN FILL-IN_LOPNR 
          FILL-IN-ANMARK FILL-IN_ARRENDATOR SEL-KOMMUN FILL-IN_ANAMN 
          FILL-IN_APERNR FILL-IN_AADRESS FILL-IN_APONR FILL-IN_APADRESS 
          FILL-IN_ABOXEN FILL-IN_ATELEFON FILL-IN_ATELEFON2 FILL-IN_ATELEFONM 
          FILL-IN_AMAIL 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_BETECKNING SEL-LAN FILL-IN_LAN FILL-IN_KOMMUN FILL-IN_SOCKEN 
         CMB_PAKER CMB_VSKOG CMB_GOMRTALL CMB_GOMRGRAN FILL-IN_LOPNR 
         FILL-IN-ANMARK FILL-IN_ARRENDATOR SEL-KOMMUN FILL-IN_ANAMN 
         FILL-IN_APERNR FILL-IN_AADRESS FILL-IN_APONR FILL-IN_APADRESS 
         FILL-IN_ABOXEN FILL-IN_ATELEFON FILL-IN_ATELEFON2 FILL-IN_ATELEFONM 
         FILL-IN_AMAIL BTN_OK BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI DIALOG-1 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   {muswait.i}
   EMPTY TEMP-TABLE efastighettemp NO-ERROR. 
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   ASSIGN
   FILL-IN_BETECKNING = INPUT FRAME {&FRAME-NAME} FILL-IN_BETECKNING
   FILL-IN_KOMMUN = INPUT FILL-IN_KOMMUN
   FILL-IN_SOCKEN = INPUT FILL-IN_SOCKEN
   FILL-IN_LAN = INPUT FILL-IN_LAN
   FILL-IN_ARRENDATOR = INPUT FILL-IN_ARRENDATOR  
   FILL-IN_ANAMN = INPUT FILL-IN_ANAMN
   FILL-IN_APERNR = INPUT FILL-IN_APERNR
   FILL-IN_AADRESS = INPUT FILL-IN_AADRESS
   FILL-IN_APONR = INPUT FILL-IN_APONR
   FILL-IN_APADRESS = INPUT FILL-IN_APADRESS
   FILL-IN_LOPNR = INPUT FILL-IN_LOPNR
   FILL-IN_ABOXEN = INPUT FILL-IN_ABOXEN
   FILL-IN_ATELEFON = INPUT FILL-IN_ATELEFON
   FILL-IN_ATELEFON2 = INPUT FILL-IN_ATELEFON2
   FILL-IN_ATELEFONM = INPUT FILL-IN_ATELEFONM
   FILL-IN_AMAIL = INPUT FILL-IN_AMAIL   
   CMB_PAKER = INPUT CMB_PAKER
   /* CMB_VAKER = INPUT CMB_VAKER*/
   CMB_VSKOG = INPUT CMB_VSKOG
   CMB_GOMRTALL = INPUT CMB_GOMRTALL
   CMB_GOMRGRAN = INPUT CMB_GOMRGRAN
   FILL-IN-ANMARK = INPUT FILL-IN-ANMARK.
   IF FILL-IN_BETECKNING = "" THEN DO:
      MESSAGE "Fastighetsbeteckning kan inte vara blank" VIEW-AS ALERT-BOX.            
      musz = TRUE. 
      RETURN NO-APPLY.
   END.  
   IF vart = "NYA" THEN DO:      
      RUN fastkoll_UI IN andfastigapph (INPUT FILL-IN_BETECKNING,OUTPUT finns).
      IF finns = TRUE THEN DO:
         MESSAGE "Det finns redan en fastighet med denna beteckning. Kontrollera uppgifterna på den redan upplagda fastigheten." VIEW-AS ALERT-BOX.   
         musz = TRUE.
         fastighbet = FILL-IN_BETECKNING.
         RETURN NO-APPLY.
      END.   
            
   END.
   IF vart = "AND" THEN DO:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.     
      FIND FIRST volskogtemp WHERE volskogtemp.BETECKNING = fastighettemp.BETECKNING USE-INDEX VARDNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp THEN DO:
         IF CMB_GOMRTALL NE fastighettemp.GOMRTALL THEN DO:
            MESSAGE "Volymberäkning är utförd på denna fastighet för värdering" volskogtemp.VARDNR
            ". Det går ej att ändra värderingsområde tall." VIEW-AS ALERT-BOX.   
            RETURN NO-APPLY.
         END.
         IF CMB_GOMRGRAN NE fastighettemp.GOMRGRAN THEN DO:
            MESSAGE "Volymberäkning är utförd på denna fastighet för värdering" volskogtemp.VARDNR
            ". Det går ej att ändra värderingsområde gran/björk." VIEW-AS ALERT-BOX.   
            RETURN NO-APPLY.

         END.
      END.  
      FIND FIRST akervardtemp WHERE akervardtemp.BETECKNING = fastighettemp.BETECKNING USE-INDEX VARDNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         FIND FIRST pakerregtemp WHERE pakerregtemp.BENAMNING = CMB_PAKER USE-INDEX PRODOMR NO-LOCK NO-ERROR.
         IF AVAILABLE pakerregtemp THEN DO:         
            IF pakerregtemp.PRODOMR NE fastighettemp.PAKER THEN DO:
               MESSAGE "Åkernorm är utförd på denna fastighet för värdering" akervardtemp.VARDNR
               ". Det går ej att ändra produktionsområde åker" pakerregtemp.PRODOMR fastighettemp.PAKER VIEW-AS ALERT-BOX.   
               RETURN NO-APPLY.

            END.
         END.   
   
      END.           
   END. 
   CREATE valsoktemp.
   ASSIGN                  
   valsoktemp.SOKCHAR[1] = FILL-IN_BETECKNING
   valsoktemp.SOKCHAR[2] = FILL-IN_KOMMUN    
   valsoktemp.SOKCHAR[3] = FILL-IN_SOCKEN    
   valsoktemp.SOKCHAR[4] = FILL-IN_LAN
   valsoktemp.SOKCHAR[5] = SUBSTRING(FILL-IN_ANAMN,1,30)
   valsoktemp.SOKCHAR[6] = FILL-IN_APERNR
   valsoktemp.SOKCHAR[7] = FILL-IN_AADRESS
   valsoktemp.SOKCHAR[8] = FILL-IN_APONR
   valsoktemp.SOKCHAR[9] = FILL-IN_APADRESS
   valsoktemp.SOKCHAR[10] = FILL-IN-ANMARK   
   valsoktemp.SOKLOG[1] = FILL-IN_ARRENDATOR.   
   RUN klar IN andfastigapph (INPUT-OUTPUT fastighbet,INPUT TABLE valsoktemp,
                              INPUT CMB_GOMRTALL,INPUT CMB_GOMRGRAN,
                              INPUT CMB_PAKER,INPUT CMB_VSKOG,
                              OUTPUT TABLE efastighettemp).

   FIND FIRST efastighettemp NO-LOCK NO-ERROR.
   IF AVAILABLE efastighettemp THEN DO:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = efastighettemp.BETECKNING NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE fastighettemp THEN DO:
         CREATE fastighettemp.
      END.
      BUFFER-COPY efastighettemp TO fastighettemp.      

   END.
   FIND FIRST efastighettemp NO-LOCK NO-ERROR.
   IF AVAILABLE efastighettemp THEN DO:
      EMPTY TEMP-TABLE kfastighettemp  NO-ERROR. 
      CREATE kfastighettemp.
      BUFFER-COPY efastighettemp TO kfastighettemp.    
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FASTLOPNR"                   
   inextradatatemp.HUVUDCH = fastighettemp.BETECKNING
   inextradatatemp.SOKCHAR[1] = FILL-IN_LOPNR
   inextradatatemp.SOKCHAR[2] = FILL-IN_ABOXEN
   inextradatatemp.SOKCHAR[3] = FILL-IN_ATELEFON
   inextradatatemp.SOKCHAR[4] = FILL-IN_ATELEFON2 
   inextradatatemp.SOKCHAR[5] = FILL-IN_ATELEFONM 
   inextradatatemp.SOKCHAR[6] = FILL-IN_AMAIL.

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

