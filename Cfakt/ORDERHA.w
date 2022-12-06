&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/14/97 -  1:08 pm

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
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER gamanv AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER slut AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER varacont AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{FAKTPLANTEMP.I}                 
{FAKTTEMP.I}
{FAKTTYPDEF.I}
{OMRTEMPW.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE prelgodvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE feltexvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kontber AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_NVE FILL-IN-TOMDAT BTN_FVE RAD_UPP ~
SEL_FAKTTYP FILL-IN_BESTNAMN FILL-IN_VARREF EDITOR_MEDD FILL-IN_BESTALLARE ~
FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_FAKPNR ~
FILL-IN_FAKORT FILL-IN_LAND FILL-IN_VAT BTN_AVS BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TOMDAT FILL-IN-VK FILL-IN-DAG ~
FILL-IN_VECKOKORD FILL-IN_MEDTID FILL-IN_FDELNR FILL-IN_AVDELNINGNR ~
FILL-IN_AVDELNINGNAMN RAD_UPP SEL_FAKTTYP FILL-IN-TIDIGARE FILL-IN_BESTNAMN ~
FILL-IN_VARREF EDITOR_MEDD FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN-TIDUM ~
FILL-IN_TEL FILL-IN-TIDMM FILL-IN_FAKADRESS FILL-IN-FORS FILL-IN_CO ~
FILL-IN-FORSKOTTM FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_LAND FILL-IN_VAT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "AvBRYT" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(256)":U INITIAL "Dag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORS AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Tidigare försäljning" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORSKOTTM AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Moms" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIDIGARE AS CHARACTER FORMAT "X(256)":U INITIAL "Tidigare fakturerat:" 
     VIEW-AS FILL-IN 
     SIZE 22.13 BY 1.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIDMM AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Förskott med moms" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIDUM AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Förskott utan moms" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VK AS CHARACTER FORMAT "X(256)":U INITIAL "Senaste ekonomi- och lönesammanställningen:" 
     VIEW-AS FILL-IN 
     SIZE 50.88 BY 1.38
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_AVDELNINGNAMN AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY .83.

DEFINE VARIABLE FILL-IN_AVDELNINGNR AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Bolag" 
     VIEW-AS FILL-IN 
     SIZE 4.88 BY .83.

DEFINE VARIABLE FILL-IN_BESTALLARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Beställare" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_BESTNAMN AS CHARACTER FORMAT "x(50)" 
     LABEL "Beställare/Kund" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FILL-IN_CO AS CHARACTER FORMAT "X(25)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FAKADRESS AS CHARACTER FORMAT "x(25)" 
     LABEL "Fakturaadress" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_FAKORT AS CHARACTER FORMAT "x(25)" 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_FAKPNR AS CHARACTER FORMAT "999 99" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_FDELNR AS INTEGER FORMAT "999999" INITIAL 0 
     LABEL "Preliminär faktura" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83.

DEFINE VARIABLE FILL-IN_KONTAKT AS CHARACTER FORMAT "x(256)" 
     LABEL "Er ref" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_LAND AS CHARACTER FORMAT "x(25)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_MEDTID AS CHARACTER FORMAT "x(4)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.33
     FONT 17.

DEFINE VARIABLE FILL-IN_TEL AS CHARACTER FORMAT "X(11)" 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_VARREF AS CHARACTER FORMAT "x(256)" 
     LABEL "Vår ref" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_VAT AS CHARACTER FORMAT "x(25)" 
     LABEL "VAT" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_VECKOKORD AS CHARACTER FORMAT "x(11)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.33
     FONT 17.

DEFINE VARIABLE RAD_UPP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Kunduppgifter", 1,
"Fakturainformation", 2,
"Fakturatext", 3
     SIZE 74.88 BY 1 NO-UNDO.

DEFINE VARIABLE SEL_FAKTTYP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 45 BY 9 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_NVE AT ROW 3.33 COL 75.5
     FILL-IN-TOMDAT AT ROW 3.67 COL 62.5 COLON-ALIGNED NO-LABEL
     BTN_FVE AT ROW 4.33 COL 75.5
     FILL-IN-VK AT ROW 5.25 COL 1.5 NO-LABEL
     FILL-IN-DAG AT ROW 5.25 COL 53.5 COLON-ALIGNED NO-LABEL
     FILL-IN_VECKOKORD AT ROW 5.25 COL 63 COLON-ALIGNED NO-LABEL
     FILL-IN_MEDTID AT ROW 6.92 COL 63 COLON-ALIGNED NO-LABEL
     FILL-IN_FDELNR AT ROW 9.42 COL 22 COLON-ALIGNED
     FILL-IN_AVDELNINGNR AT ROW 9.42 COL 46.5 COLON-ALIGNED
     FILL-IN_AVDELNINGNAMN AT ROW 9.42 COL 53.5 COLON-ALIGNED NO-LABEL
     RAD_UPP AT ROW 10.33 COL 1.5 NO-LABEL
     SEL_FAKTTYP AT ROW 11.25 COL 1.5 NO-LABEL
     FILL-IN-TIDIGARE AT ROW 11.58 COL 58.5 COLON-ALIGNED NO-LABEL
     FILL-IN_BESTNAMN AT ROW 11.67 COL 18.38 COLON-ALIGNED
     FILL-IN_VARREF AT ROW 11.67 COL 60.63 COLON-ALIGNED
     EDITOR_MEDD AT ROW 12.92 COL 1.5 NO-LABEL
     FILL-IN_BESTALLARE AT ROW 12.92 COL 18.38 COLON-ALIGNED
     FILL-IN_KONTAKT AT ROW 12.92 COL 60.63 COLON-ALIGNED
     FILL-IN-TIDUM AT ROW 12.92 COL 70 COLON-ALIGNED
     FILL-IN_TEL AT ROW 14.13 COL 18.38 COLON-ALIGNED
     FILL-IN-TIDMM AT ROW 14.13 COL 70 COLON-ALIGNED
     FILL-IN_FAKADRESS AT ROW 15.42 COL 18.38 COLON-ALIGNED
     FILL-IN-FORS AT ROW 15.42 COL 70 COLON-ALIGNED
     FILL-IN_CO AT ROW 16.54 COL 18.38 COLON-ALIGNED
     FILL-IN-FORSKOTTM AT ROW 16.54 COL 70 COLON-ALIGNED
     FILL-IN_FAKPNR AT ROW 17.75 COL 18.38 COLON-ALIGNED
     FILL-IN_FAKORT AT ROW 17.75 COL 37 COLON-ALIGNED
     FILL-IN_LAND AT ROW 18 COL 70 COLON-ALIGNED WIDGET-ID 4
     FILL-IN_VAT AT ROW 19.25 COL 70 COLON-ALIGNED WIDGET-ID 6
     BTN_AVS AT ROW 22.42 COL 74.63
     BTN_OK AT ROW 22.46 COL 59.63
     "Senaste godkända tidsedel avsåg:" VIEW-AS TEXT
          SIZE 51.75 BY 1.33 AT ROW 6.92 COL 1.5
          FONT 17
     "Faktureringen skall gälla alla ekonomi- och lönesammanställda" VIEW-AS TEXT
          SIZE 73.63 BY 1.25 AT ROW 2 COL 1.5
          FONT 17
     "registreringar till och med datum :" VIEW-AS TEXT
          SIZE 53.13 BY 1.5 AT ROW 3.5 COL 1.5
          FONT 17
     "Vecka" VIEW-AS TEXT
          SIZE 7.88 BY 1.33 AT ROW 6.92 COL 56
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.5 BY 22.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Orderhuvud"
         HEIGHT             = 22.71
         WIDTH              = 89
         MAX-HEIGHT         = 24.83
         MAX-WIDTH          = 95.5
         VIRTUAL-HEIGHT     = 24.83
         VIRTUAL-WIDTH      = 95.5
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       EDITOR_MEDD:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FORS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FORSKOTTM IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TIDIGARE IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TIDMM IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TIDUM IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TOMDAT IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-VK IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_AVDELNINGNAMN IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AVDELNINGNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_BESTALLARE:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_BESTNAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_CO:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_FAKADRESS:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_FAKORT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_FAKPNR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FDELNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_KONTAKT:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MEDTID IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_TEL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_VARREF:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VECKOKORD IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WINDOW-1 WINDOW-1
ON WINDOW-CLOSE OF WINDOW-1 /* Orderhuvud */
DO:   
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-1
ON CHOOSE OF BTN_AVS IN FRAME FRAME-A /* AvBRYT */
DO:
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE WINDOW-1
ON CHOOSE OF BTN_FVE IN FRAME FRAME-A /* - */
DO: 
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT - 1.      
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE WINDOW-1
ON CHOOSE OF BTN_NVE IN FRAME FRAME-A /* + */
DO:   
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT + 1.        
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   musz = FALSE.  
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT
   EDITOR_MEDD = INPUT EDITOR_MEDD
   FILL-IN_BESTNAMN = INPUT FILL-IN_BESTNAMN
   FILL-IN_FAKADRESS = INPUT FILL-IN_FAKADRESS
   FILL-IN_FAKORT = INPUT FILL-IN_FAKORT
   FILL-IN_LAND = INPUT FILL-IN_LAND
   FILL-IN_VAT = INPUT FILL-IN_VAT
   FILL-IN_FAKPNR = INPUT FILL-IN_FAKPNR
   FILL-IN_KONTAKT = INPUT FILL-IN_KONTAKT
   FILL-IN_TEL = INPUT FILL-IN_TEL
   FILL-IN_VARREF = INPUT FILL-IN_VARREF
   FILL-IN_CO = INPUT FILL-IN_CO.
   FILL-IN_BESTALLARE = INPUT FILL-IN_BESTALLARE.
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.
   IF vfaktplantemp.FAKTTYP = "Löpande utan" THEN DO:
      IF FILL-IN-TOMDAT >= TODAY THEN DO:
         MESSAGE "Datum kan ej vara senare än i dag" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   IF SEL_FAKTTYP = "" OR SEL_FAKTTYP = ? THEN DO:
      MESSAGE "Du måste välja någon faktureringstyp." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   RUN slutordeh_UI IN fakthmth 
   (INPUT infakplannr,
    INPUT fdelnrvar,                   
    INPUT EDITOR_MEDD, 
    INPUT EDITOR_MEDD:NUM-LINES,
    INPUT SEL_FAKTTYP,
    INPUT FILL-IN-TOMDAT,
    OUTPUT slut,
    OUTPUT varacont).   
   
   
   FIND FIRST faktnamntemp WHERE faktnamntemp.FAKTURNR = infakplannr AND faktnamntemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
   faktnamntemp.FAKADRESS = "".
   faktnamntemp.FAKORT = "".
   ASSIGN  
   
   SUBSTRING(faktnamntemp.FAKADRESS,1,25) = STRING(FILL-IN_FAKADRESS,"X(25)")
   SUBSTRING(faktnamntemp.FAKADRESS,26,25) = FILL-IN_CO
   SUBSTRING(faktnamntemp.FAKORT,1,25) = FILL-IN_FAKORT
   SUBSTRING(faktnamntemp.FAKORT,26,25) = FILL-IN_LAND
   SUBSTRING(faktnamntemp.FAKORT,52,25) = FILL-IN_VAT
   faktnamntemp.BESTNAMN = FILL-IN_BESTNAMN 
   faktnamntemp.FAKPNR = FILL-IN_FAKPNR 
   faktnamntemp.KONTAKT = FILL-IN_KONTAKT
   faktnamntemp.VARREF = FILL-IN_VARREF
   faktnamntemp.TEL = FILL-IN_TEL
   faktnamntemp.BESTALLARE = FILL-IN_BESTALLARE.            
   
   RUN faktnamn_UI IN fakthmth 
   (INPUT 2,INPUT infakplannr,INPUT fdelnrvar,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE faktnamntemp).     
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD WINDOW-1
ON LEAVE OF EDITOR_MEDD IN FRAME FRAME-A
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TOMDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT WINDOW-1
ON LEAVE OF FILL-IN-TOMDAT IN FRAME FRAME-A
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT WINDOW-1
ON MOUSE-MENU-CLICK OF FILL-IN-TOMDAT IN FRAME FRAME-A
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-TOMDAT.
   RUN AlmanBtn.w.
   FILL-IN-TOMDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAND WINDOW-1
ON LEAVE OF FILL-IN_LAND IN FRAME FRAME-A /* Land */
DO:    
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VAT WINDOW-1
ON LEAVE OF FILL-IN_VAT IN FRAME FRAME-A /* VAT */
DO:    
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_UPP WINDOW-1
ON VALUE-CHANGED OF RAD_UPP IN FRAME FRAME-A
DO:
   RAD_UPP = INPUT RAD_UPP.   
   ASSIGN
   EDITOR_MEDD:HIDDEN = TRUE 
   FILL-IN_BESTALLARE:HIDDEN = TRUE 
   FILL-IN_BESTNAMN:HIDDEN = TRUE 
   FILL-IN_CO:HIDDEN = TRUE 
   FILL-IN_FAKADRESS:HIDDEN = TRUE
   FILL-IN_VAT:HIDDEN = TRUE
   FILL-IN_LAND:HIDDEN = TRUE 
   FILL-IN_FAKORT:HIDDEN = TRUE 
   FILL-IN_FAKPNR:HIDDEN = TRUE 
   FILL-IN_KONTAKT:HIDDEN = TRUE 
   FILL-IN_TEL:HIDDEN = TRUE 
   FILL-IN_VARREF:HIDDEN = TRUE
   FILL-IN-FORS:HIDDEN = TRUE 
   FILL-IN-FORSKOTTM:HIDDEN = TRUE 
   FILL-IN-TIDIGARE:HIDDEN = TRUE 
   FILL-IN-TIDMM:HIDDEN = TRUE
   FILL-IN-TIDUM:HIDDEN = TRUE
   SEL_FAKTTYP:HIDDEN = TRUE.
   IF RAD_UPP = 1 THEN DO:
      ASSIGN      
      FILL-IN_BESTALLARE:HIDDEN = FALSE 
      FILL-IN_BESTNAMN:HIDDEN = FALSE 
      FILL-IN_CO:HIDDEN = FALSE 
      FILL-IN_FAKADRESS:HIDDEN = FALSE
      FILL-IN_LAND:HIDDEN = FALSE
      FILL-IN_VAT:HIDDEN = FALSE 
      FILL-IN_FAKORT:HIDDEN = FALSE 
      FILL-IN_FAKPNR:HIDDEN = FALSE       
      FILL-IN_KONTAKT:HIDDEN = FALSE 
      FILL-IN_TEL:HIDDEN = FALSE 
      FILL-IN_VARREF:HIDDEN = FALSE.
   END.
   ELSE IF RAD_UPP = 2 THEN DO:
      ASSIGN
      FILL-IN-FORS:HIDDEN = FALSE 
      FILL-IN-FORSKOTTM:HIDDEN = FALSE 
      FILL-IN-TIDIGARE:HIDDEN = FALSE 
      FILL-IN-TIDMM:HIDDEN = FALSE
      FILL-IN-TIDUM:HIDDEN = FALSE
      SEL_FAKTTYP:HIDDEN = FALSE.
      ASSIGN SEL_FAKTTYP:SCREEN-VALUE = SEL_FAKTTYP.
      DISPLAY FILL-IN-FORS FILL-IN-FORSKOTTM FILL-IN-TIDIGARE FILL-IN-TIDMM WITH FRAME {&FRAME-NAME}.
      IF kontber = TRUE THEN DO: 
         DISABLE SEL_FAKTTYP WITH FRAME {&FRAME-NAME}.            
         DISABLE BTN_NVE FILL-IN-TOMDAT BTN_FVE WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE IF RAD_UPP = 3 THEN DO:
      ASSIGN
      EDITOR_MEDD:HIDDEN = FALSE.       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_FAKTTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_FAKTTYP WINDOW-1
ON LEAVE OF SEL_FAKTTYP IN FRAME FRAME-A
DO:
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_FAKTTYP WINDOW-1
ON VALUE-CHANGED OF SEL_FAKTTYP IN FRAME FRAME-A
DO:
/*   MMMMMMMMMM*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


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
  musz = TRUE.
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
       
   ASSIGN
  /* FILL-IN_BESTALLARE:LABEL = Guru.Konstanter:gbestk*/
   FILL-IN_BESTNAMN:LABEL = Guru.Konstanter:gbestk
   FILL-IN_FDELNR = fdelnrvar.      
   ASSIGN
   FILL-IN-TOMDAT = TODAY.
   
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-ERROR.
   IF vfaktplantemp.FAKTTYP = "Löpande utan" THEN DO:
      FILL-IN-TOMDAT = TODAY - 1.
   END.
   
   RUN startordeh_UI IN fakthmth (INPUT  infakplannr, 
                                  INPUT  fdelnrvar,
                                  INPUT  Guru.Konstanter:globanv,
                                  INPUT-OUTPUT  FILL-IN-TOMDAT,
                                  OUTPUT FILL-IN_VECKOKORD,
                                  OUTPUT FILL-IN_MEDTID,
                                  OUTPUT feltexvar,
                                  OUTPUT EDITOR_MEDD,
                                  OUTPUT FILL-IN_AVDELNINGNR,
                                  OUTPUT FILL-IN_AVDELNINGNAMN,
                                  OUTPUT prelgodvar,
                                  OUTPUT FILL-IN-FORSKOTTM,     
                                  OUTPUT FILL-IN-FORS,          
                                  OUTPUT FILL-IN-TIDUM,         
                                  OUTPUT FILL-IN-TIDMM,         
                                  OUTPUT kontber,
                                  OUTPUT TABLE valsoktemp).   
   IF feltexvar NE "" THEN DO:
      MESSAGE feltexvar VIEW-AS ALERT-BOX.
      musz = TRUE.
      LEAVE MAIN-BLOCK.
   END.
   RUN faktnamn_UI.
   RAD_UPP = 2.
   
   RUN momstidigare_UI.
   FILL-IN_AVDELNINGNR:LABEL= Guru.Konstanter:gavdk.
   RUN enable_UI.   
   {FRMSIZE.I}        
   APPLY "VALUE-CHANGED" TO RAD_UPP.
   IF prelgodvar = TRUE THEN DO:
      DISABLE BTN_NVE FILL-IN-TOMDAT BTN_FVE WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}
   {WIN_M_SLUT.I}
   RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW). 
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-TOMDAT FILL-IN-VK FILL-IN-DAG FILL-IN_VECKOKORD FILL-IN_MEDTID 
          FILL-IN_FDELNR FILL-IN_AVDELNINGNR FILL-IN_AVDELNINGNAMN RAD_UPP 
          SEL_FAKTTYP FILL-IN-TIDIGARE FILL-IN_BESTNAMN FILL-IN_VARREF 
          EDITOR_MEDD FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN-TIDUM 
          FILL-IN_TEL FILL-IN-TIDMM FILL-IN_FAKADRESS FILL-IN-FORS FILL-IN_CO 
          FILL-IN-FORSKOTTM FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_LAND 
          FILL-IN_VAT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BTN_NVE FILL-IN-TOMDAT BTN_FVE RAD_UPP SEL_FAKTTYP FILL-IN_BESTNAMN 
         FILL-IN_VARREF EDITOR_MEDD FILL-IN_BESTALLARE FILL-IN_KONTAKT 
         FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_FAKPNR FILL-IN_FAKORT 
         FILL-IN_LAND FILL-IN_VAT BTN_AVS BTN_OK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktnamn_UI WINDOW-1 
PROCEDURE faktnamn_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN faktnamn_UI IN fakthmth 
   (INPUT 1,INPUT infakplannr,INPUT fdelnrvar,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE faktnamntemp).     
   FIND FIRST faktnamntemp WHERE faktnamntemp.FAKTURNR = infakplannr AND faktnamntemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_BESTNAMN = faktnamntemp.BESTNAMN
   FILL-IN_FAKADRESS = SUBSTRING(faktnamntemp.FAKADRESS,1,25)
   FILL-IN_FAKORT = SUBSTRING(faktnamntemp.FAKORT,1,25) 
   FILL-IN_LAND = SUBSTRING(faktnamntemp.FAKORT,26,25) 
   FILL-IN_VAT = SUBSTRING(faktnamntemp.FAKORT,52,25) 
   FILL-IN_FAKPNR = faktnamntemp.FAKPNR
   FILL-IN_KONTAKT = faktnamntemp.KONTAKT
   FILL-IN_TEL = faktnamntemp.TEL   
   FILL-IN_VARREF = faktnamntemp.VARREF 
   FILL-IN_BESTALLARE = faktnamntemp.BESTALLARE
   FILL-IN_CO = SUBSTRING(faktnamntemp.FAKADRESS,26,25).    
   IF FILL-IN_BESTALLARE = "" THEN FILL-IN_BESTALLARE = FILL-IN_KONTAKT.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE momstidigare_UI WINDOW-1 
PROCEDURE momstidigare_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH valsoktemp:
      status-ok = SEL_FAKTTYP:ADD-LAST(valsoktemp.SOKCHAR[1]) IN FRAME {&FRAME-NAME}. 
      IF valsoktemp.SOKVAL = 1 THEN SEL_FAKTTYP = valsoktemp.SOKCHAR[1].
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND valsoktemp.SOKCHAR[1] = "Förskott med moms" THEN DO:
         SEL_FAKTTYP = valsoktemp.SOKCHAR[1].
      END.   
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

