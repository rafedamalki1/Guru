&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



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
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{FAKTTEMP.I}
   
{FAKTPLANTEMP.I}
&Scoped-define SHARED SHARED
{FAKKOTEMP.I}
{SOKDEF.I}
{HOPPSEK2W.I}
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE VARIABLE fakkoproch AS HANDLE NO-UNDO. 

DEFINE VARIABLE typfinns AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidfaktvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE tidmomsvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE frifor AS DECIMAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_KRED

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktdebkred

/* Definitions for BROWSE BRW_KRED                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KRED faktdebkred.VKREDIT ~
faktdebkred.FDELNR faktdebkred.VFAKTNR faktdebkred.NAMN faktdebkred.TOTPRIS ~
faktdebkred.DATUM faktdebkred.KTOTPRIS faktdebkred.KDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KRED 
&Scoped-define QUERY-STRING-BRW_KRED FOR EACH faktdebkred NO-LOCK ~
    BY faktdebkred.FDELNR ~
       BY faktdebkred.VFAKTNR
&Scoped-define OPEN-QUERY-BRW_KRED OPEN QUERY BRW_KRED FOR EACH faktdebkred NO-LOCK ~
    BY faktdebkred.FDELNR ~
       BY faktdebkred.VFAKTNR.
&Scoped-define TABLES-IN-QUERY-BRW_KRED faktdebkred
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KRED faktdebkred


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_KRED FBTN_OK FBTN_GVFAK BTN_NY BTN_BORT ~
RAD_UPP SEL_FAKTTYP EDITOR_MEDD FILL-IN_BESTNAMN FILL-IN_VARREF ~
FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO ~
FILL-IN_LAND FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_VAT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AVDELNINGNR FILL-IN_AVDELNINGNAMN ~
RAD_UPP SEL_FAKTTYP EDITOR_MEDD FILL-IN_BESTNAMN FILL-IN_VARREF ~
FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO ~
FILL-IN_LAND FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_VAT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort kreditfaktura":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny kreditfaktura":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_GVFAK 
     LABEL "Visa faktura" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Fakturera" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9 NO-UNDO.

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

DEFINE VARIABLE FILL-IN_KONTAKT AS CHARACTER FORMAT "x(256)" 
     LABEL "Er ref" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_LAND AS CHARACTER FORMAT "x(25)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KRED FOR 
      faktdebkred SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KRED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KRED WINDOW-1 _STRUCTURED
  QUERY BRW_KRED NO-LOCK DISPLAY
      faktdebkred.VKREDIT COLUMN-LABEL "Kredit!faktura" FORMAT "->>>>>>>>9":U
      faktdebkred.FDELNR COLUMN-LABEL "Preliminär!kreditfaktura" FORMAT "9999999":U
      faktdebkred.VFAKTNR COLUMN-LABEL "Debet!Faktura" FORMAT ">>>>>>>>9":U
      faktdebkred.NAMN COLUMN-LABEL "Benämning" FORMAT "X(40)":U
            WIDTH 26
      faktdebkred.TOTPRIS COLUMN-LABEL "Debet" FORMAT "->>>>>>>9.99":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      faktdebkred.DATUM COLUMN-LABEL "Faktura!datum" FORMAT "99/99/99":U
      faktdebkred.KTOTPRIS COLUMN-LABEL "Kredit" FORMAT "->>>>>>>9.99":U
            COLUMN-FGCOLOR 12 LABEL-FGCOLOR 12
      faktdebkred.KDATUM COLUMN-LABEL "Kredit!datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 101 BY 7.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_KRED AT ROW 3.25 COL 1.5
     FBTN_OK AT ROW 8 COL 103.5
     FBTN_GVFAK AT ROW 9.08 COL 103.5
     BTN_NY AT ROW 11.38 COL 28
     BTN_BORT AT ROW 11.38 COL 54.63
     FILL-IN_AVDELNINGNR AT ROW 13.25 COL 25.25 COLON-ALIGNED
     FILL-IN_AVDELNINGNAMN AT ROW 13.25 COL 33.25 COLON-ALIGNED NO-LABEL
     RAD_UPP AT ROW 14.33 COL 1.5 NO-LABEL
     SEL_FAKTTYP AT ROW 15.54 COL 1.5 NO-LABEL
     EDITOR_MEDD AT ROW 15.58 COL 1.5 NO-LABEL
     FILL-IN_BESTNAMN AT ROW 15.67 COL 16.5 COLON-ALIGNED
     FILL-IN_VARREF AT ROW 15.67 COL 58.75 COLON-ALIGNED
     FILL-IN_BESTALLARE AT ROW 16.92 COL 16.5 COLON-ALIGNED
     FILL-IN_KONTAKT AT ROW 16.92 COL 58.75 COLON-ALIGNED
     FILL-IN_TEL AT ROW 18.13 COL 16.5 COLON-ALIGNED
     FILL-IN_FAKADRESS AT ROW 19.42 COL 16.5 COLON-ALIGNED
     FILL-IN_CO AT ROW 20.54 COL 16.5 COLON-ALIGNED
     FILL-IN_LAND AT ROW 21.5 COL 58.75 COLON-ALIGNED WIDGET-ID 4
     FILL-IN_FAKPNR AT ROW 21.71 COL 16.5 COLON-ALIGNED
     FILL-IN_FAKORT AT ROW 21.71 COL 29.88 COLON-ALIGNED
     FILL-IN_VAT AT ROW 22.71 COL 58.75 COLON-ALIGNED WIDGET-ID 6
     BTN_AVB AT ROW 23.58 COL 103.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.13 BY 24.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: faktdebkred T "?" NO-UNDO temp-db faktdebkred
      TABLE: ? T "?" NO-UNDO TEMP-DB faktkredtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Orderhuvud"
         COLUMN             = 24.38
         ROW                = 10.33
         HEIGHT             = 24.25
         WIDTH              = 117.13
         MAX-HEIGHT         = 25.58
         MAX-WIDTH          = 126
         VIRTUAL-HEIGHT     = 25.58
         VIRTUAL-WIDTH      = 126
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
                                                                        */
/* BROWSE-TAB BRW_KRED 1 FRAME-A */
ASSIGN 
       BRW_KRED:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_KRED:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       EDITOR_MEDD:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_GVFAK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_OK:HIDDEN IN FRAME FRAME-A           = TRUE.

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

ASSIGN 
       FILL-IN_KONTAKT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_TEL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN_VARREF:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       RAD_UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       SEL_FAKTTYP:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KRED
/* Query rebuild information for BROWSE BRW_KRED
     _TblList          = "Temp-Tables.faktdebkred"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.faktdebkred.FDELNR|yes,Temp-Tables.faktdebkred.VFAKTNR|yes"
     _FldNameList[1]   > Temp-Tables.faktdebkred.VKREDIT
"faktdebkred.VKREDIT" "Kredit!faktura" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktdebkred.FDELNR
"faktdebkred.FDELNR" "Preliminär!kreditfaktura" "9999999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktdebkred.VFAKTNR
"faktdebkred.VFAKTNR" "Debet!Faktura" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.faktdebkred.NAMN
"faktdebkred.NAMN" "Benämning" "X(40)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.faktdebkred.TOTPRIS
"faktdebkred.TOTPRIS" "Debet" "->>>>>>>9.99" "decimal" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.faktdebkred.DATUM
"faktdebkred.DATUM" "Faktura!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.faktdebkred.KTOTPRIS
"faktdebkred.KTOTPRIS" "Kredit" "->>>>>>>9.99" "decimal" ? 12 ? ? 12 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.faktdebkred.KDATUM
"faktdebkred.KDATUM" "Kredit!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KRED */
&ANALYZE-RESUME

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
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KRED
&Scoped-define SELF-NAME BRW_KRED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KRED WINDOW-1
ON VALUE-CHANGED OF BRW_KRED IN FRAME FRAME-A
DO:   
   status-ok = RAD_UPP:DELETE("Fakturainformation") NO-ERROR.   
   RUN hamtafaktkred_UI (INPUT 1).
   RUN hamta_UI.
   ASSIGN   
   RAD_UPP:HIDDEN = TRUE
   FBTN_OK:HIDDEN = TRUE
   BTN_BORT:HIDDEN = TRUE 
   FBTN_GVFAK:HIDDEN = TRUE
   EDITOR_MEDD:HIDDEN = TRUE 
   FILL-IN_BESTALLARE:HIDDEN = TRUE 
   FILL-IN_BESTNAMN:HIDDEN = TRUE 
   FILL-IN_CO:HIDDEN = TRUE 
   FILL-IN_FAKADRESS:HIDDEN = TRUE
   FILL-IN_LAND:HIDDEN = TRUE
   FILL-IN_VAT:HIDDEN = TRUE 
   FILL-IN_FAKORT:HIDDEN = TRUE 
   FILL-IN_FAKPNR:HIDDEN = TRUE 
   FILL-IN_KONTAKT:HIDDEN = TRUE 
   FILL-IN_TEL:HIDDEN = TRUE 
   FILL-IN_VARREF:HIDDEN = TRUE
   SEL_FAKTTYP:HIDDEN = TRUE.   
   IF faktkredtemp.DEBKRED = FALSE THEN DO:
      ASSIGN   
      FBTN_GVFAK:HIDDEN = FALSE.    
      IF faktkredtemp.VKREDIT = 0 THEN DO:
         ASSIGN         
         FBTN_OK:HIDDEN = FALSE
         BTN_BORT:HIDDEN = FALSE.         
      END.
      IF faktkredtemp.VFAKTNR = 0 THEN DO:
      /*   RAD_UPP = 2.*/
         status-ok = RAD_UPP:ADD-LAST("Fakturainformation", 2).
         DISPLAY RAD_UPP WITH FRAME {&FRAME-NAME}.  
      END.   
      FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = faktkredtemp.FAKTNR AND
      faktureradtemp.VFAKTNR = faktkredtemp.VFAKTNR NO-LOCK NO-ERROR.      
      FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = faktureradtemp.FAKTTYPID NO-LOCK NO-ERROR.
      IF AVAILABLE faktureringstyptemp THEN DO:
         ASSIGN SEL_FAKTTYP = faktureringstyptemp.FAKTTYPTEXT.
      END.      
      ASSIGN
      EDITOR_MEDD = faktkredtemp.FAKTXT
      RAD_UPP:HIDDEN = FALSE.
      APPLY "VALUE-CHANGED" TO RAD_UPP.            
   END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. 
   {BORTBRWPROC.I}
    musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort kreditfaktura */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny kreditfaktura */
DO:
   RUN ny_UI.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD WINDOW-1
ON ENTRY OF EDITOR_MEDD IN FRAME FRAME-A
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD WINDOW-1
ON LEAVE OF EDITOR_MEDD IN FRAME FRAME-A
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_GVFAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_GVFAK WINDOW-1
ON CHOOSE OF FBTN_GVFAK IN FRAME FRAME-A /* Visa faktura */
DO:
   status-ok = BRW_KRED:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN spara_UI.
   skrivut = FALSE.
   {AVBGOM.I}
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR. 
   CREATE efaktkredtemp.
   BUFFER-COPY faktkredtemp TO efaktkredtemp.
   
   {AMERICANEUROPEAN.I}
   RUN VLOPFAKKA.W (INPUT infakplannr,INPUT faktkredtemp.FDELNR,INPUT FALSE,INPUT TABLE efaktkredtemp, INPUT TABLE sumtidtemp).
   {EUROPEANAMERICAN.I}
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Fakturera */
DO:   
   musz = FALSE.                      
   {muswait.i}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 97
   soktemp.SOKINT[1] = faktkredtemp.FAKTNR.
   {SOKANROP.I}      
   IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
      IF soktemp.SOKLOG[1] = FALSE THEN DO:
         MESSAGE "Du kan bara fakturera godkända preliminär fakturor!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.    
      END.
   END.
   IF Guru.Konstanter:faktsekvar[4] = FALSE AND Guru.Konstanter:faktsekvar[5] = FALSE THEN DO:
      IF soktemp.SOKLOG[1] = TRUE THEN DO:
         MESSAGE "Fakturan är preliminär godkända och kan inte ändras!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.    
      END.
   END.   
   RUN sparatemp_UI.
   IF SEL_FAKTTYP = "" OR SEL_FAKTTYP = ? THEN DO:
      MESSAGE "Du måste välja någon faktureringstyp under Fakturainformation" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN spara_UI.
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR. 
   CREATE efaktkredtemp.
   BUFFER-COPY faktkredtemp TO efaktkredtemp.
   {AVBGOM.I}
   RUN LOPFAKKA.W (INPUT-OUTPUT TABLE efaktkredtemp).
   FIND FIRST efaktkredtemp NO-LOCK NO-ERROR.
   FIND FIRST faktkredtemp WHERE faktkredtemp.FAKTNR = efaktkredtemp.FAKTNR AND 
   faktkredtemp.FDELNR = efaktkredtemp.FDELNR NO-LOCK NO-ERROR.
   BUFFER-COPY efaktkredtemp TO faktkredtemp.
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR. 
   {AVBFRAM.I}
   musz = FALSE.
   {musarrow.i}
   RUN hamtafaktkred_UI (INPUT 2).
   BUFFER-COPY faktkredtemp TO faktdebkred.
   IF faktkredtemp.DEBKRED = TRUE THEN DO:
      ASSIGN
      faktdebkred.KTOTPRIS = 0
      faktdebkred.KDATUM = ?.
   END.
   ELSE DO:
      ASSIGN
      faktdebkred.TOTPRIS = 0
      faktdebkred.DATUM = ?
      faktdebkred.KTOTPRIS = faktkredtemp.TOTPRIS
      faktdebkred.KDATUM =   faktkredtemp.DATUM.
      IF faktkredtemp.VFAKTNR NE 0 THEN DO:
         FIND FIRST faktkredtempbuff WHERE faktkredtempbuff.FAKTNR = faktkredtemp.FAKTNR AND
         faktkredtempbuff.VFAKTNR = faktkredtemp.VFAKTNR
         NO-LOCK NO-ERROR.
         ASSIGN
         faktdebkred.TOTPRIS = faktkredtempbuff.TOTPRIS
         faktdebkred.DATUM =   faktkredtempbuff.DATUM. 
      END.
   END.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(faktdebkred)).                  
   RUN refreshbrw_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].            
   APPLY "VALUE-CHANGED" TO BRW_KRED.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTALLARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTALLARE WINDOW-1
ON ENTRY OF FILL-IN_BESTALLARE IN FRAME FRAME-A /* Beställare */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTALLARE WINDOW-1
ON LEAVE OF FILL-IN_BESTALLARE IN FRAME FRAME-A /* Beställare */
DO:   
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTNAMN WINDOW-1
ON ENTRY OF FILL-IN_BESTNAMN IN FRAME FRAME-A /* Beställare/Kund */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTNAMN WINDOW-1
ON LEAVE OF FILL-IN_BESTNAMN IN FRAME FRAME-A /* Beställare/Kund */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_CO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CO WINDOW-1
ON ENTRY OF FILL-IN_CO IN FRAME FRAME-A
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CO WINDOW-1
ON LEAVE OF FILL-IN_CO IN FRAME FRAME-A
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKADRESS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKADRESS WINDOW-1
ON ENTRY OF FILL-IN_FAKADRESS IN FRAME FRAME-A /* Fakturaadress */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKADRESS WINDOW-1
ON LEAVE OF FILL-IN_FAKADRESS IN FRAME FRAME-A /* Fakturaadress */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKORT WINDOW-1
ON ENTRY OF FILL-IN_FAKORT IN FRAME FRAME-A /* Ort */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKORT WINDOW-1
ON LEAVE OF FILL-IN_FAKORT IN FRAME FRAME-A /* Ort */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKPNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKPNR WINDOW-1
ON ENTRY OF FILL-IN_FAKPNR IN FRAME FRAME-A /* Postnr */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKPNR WINDOW-1
ON LEAVE OF FILL-IN_FAKPNR IN FRAME FRAME-A /* Postnr */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KONTAKT WINDOW-1
ON ENTRY OF FILL-IN_KONTAKT IN FRAME FRAME-A /* Er ref */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KONTAKT WINDOW-1
ON LEAVE OF FILL-IN_KONTAKT IN FRAME FRAME-A /* Er ref */
DO:
   RUN sparatemp_UI.
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


&Scoped-define SELF-NAME FILL-IN_TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEL WINDOW-1
ON ENTRY OF FILL-IN_TEL IN FRAME FRAME-A /* Tel */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEL WINDOW-1
ON LEAVE OF FILL-IN_TEL IN FRAME FRAME-A /* Tel */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VARREF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARREF WINDOW-1
ON ENTRY OF FILL-IN_VARREF IN FRAME FRAME-A /* Vår ref */
DO:
   /*RUN hitta_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARREF WINDOW-1
ON LEAVE OF FILL-IN_VARREF IN FRAME FRAME-A /* Vår ref */
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
   FILL-IN_LAND:HIDDEN = TRUE
   FILL-IN_VAT:HIDDEN = TRUE  
   FILL-IN_FAKORT:HIDDEN = TRUE 
   FILL-IN_FAKPNR:HIDDEN = TRUE 
   FILL-IN_KONTAKT:HIDDEN = TRUE 
   FILL-IN_TEL:HIDDEN = TRUE 
   FILL-IN_VARREF:HIDDEN = TRUE
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
      DISPLAY FILL-IN_BESTALLARE FILL-IN_BESTNAMN FILL-IN_CO FILL-IN_FAKADRESS FILL-IN_LAND FILL-IN_VAT 
      FILL-IN_FAKORT FILL-IN_FAKPNR FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_VARREF
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_UPP = 2 THEN DO:
      ASSIGN
      SEL_FAKTTYP:HIDDEN = FALSE.
      ASSIGN SEL_FAKTTYP:SCREEN-VALUE = SEL_FAKTTYP.      
   END.
   ELSE IF RAD_UPP = 3 THEN DO:
      ASSIGN
      EDITOR_MEDD:HIDDEN = FALSE.       
      DISPLAY EDITOR_MEDD WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_FAKTTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_FAKTTYP WINDOW-1
ON LEAVE OF SEL_FAKTTYP IN FRAME FRAME-A
DO:
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_FAKTTYP WINDOW-1
ON VALUE-CHANGED OF SEL_FAKTTYP IN FRAME FRAME-A
DO:
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.
   FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPTEXT = SEL_FAKTTYP NO-LOCK NO-ERROR.        
   faktkredtemp.FAKTTYPID = faktureringstyptemp.FAKTTYPID. 
     
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
DO:
   {BORTBRWPROC.I}
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
   {ALLSTARTDYN.I}
   {muswait.i}
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-ERROR.
   FILL-IN_BESTNAMN:LABEL = Guru.Konstanter:gbestk.

   FOR EACH faktureringstyptemp:
      IF faktureringstyptemp.FAKTTYPID = 11 THEN musz = musz.
      ELSE status-ok = SEL_FAKTTYP:ADD-LAST(faktureringstyptemp.FAKTTYPTEXT) IN FRAME {&FRAME-NAME}.             
   END.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 85
   soktemp.SOKCHAR[1] = vfaktplantemp.OMRADE.
   {SOKANROP.I}      
   ASSIGN                                       
   FILL-IN_AVDELNINGNR   = soktemp.SOKINT[1] 
   FILL-IN_AVDELNINGNAMN = soktemp.SOKCHAR[1].
   RUN faktkredhmt_UI IN fakthmth (INPUT infakplannr,OUTPUT TABLE faktkredtemp,OUTPUT TABLE faktureradtemp,OUTPUT TABLE faktdebkred).   
   FILL-IN_AVDELNINGNR:LABEL= Guru.Konstanter:gavdk.   
   RUN enable_UI.   
   OPEN QUERY BRW_KRED FOR EACH faktdebkred NO-LOCK BY faktdebkred.FDELNR BY faktdebkred.DATUM BY faktdebkred.VFAKTNR.   
   {FRMSIZE.I}        
   ASSIGN
   FBTN_OK:HIDDEN = TRUE
   BTN_BORT:HIDDEN = TRUE 
   FBTN_GVFAK:HIDDEN = TRUE      
   EDITOR_MEDD:HIDDEN = TRUE 
   FILL-IN_BESTALLARE:HIDDEN = TRUE 
   FILL-IN_BESTNAMN:HIDDEN = TRUE 
   FILL-IN_CO:HIDDEN = TRUE 
   FILL-IN_FAKADRESS:HIDDEN = TRUE
   FILL-IN_LAND:HIDDEN = TRUE 
   FILL-IN_VAT:HIDDEN = TRUE 
   FILL-IN_FAKORT:HIDDEN = TRUE 
   FILL-IN_FAKPNR:HIDDEN = TRUE 
   FILL-IN_KONTAKT:HIDDEN = TRUE 
   FILL-IN_TEL:HIDDEN = TRUE 
   FILL-IN_VARREF:HIDDEN = TRUE
   SEL_FAKTTYP:HIDDEN = TRUE
   RAD_UPP:HIDDEN = TRUE.
   GET LAST BRW_KRED NO-LOCK.  
   IF NOT AVAILABLE faktdebkred THEN DO:
      ASSIGN
      BRW_KRED:HIDDEN = TRUE.      
   END.
   ELSE DO:
      RUN hamtafaktkred_UI (INPUT 1).
      FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = faktkredtemp.FAKTTYPID NO-LOCK NO-ERROR.
      IF AVAILABLE faktureringstyptemp THEN DO:
         ASSIGN SEL_FAKTTYP = faktureringstyptemp.FAKTTYPTEXT.
      END.
      IF faktkredtemp.VFAKTNR = 0 THEN RAD_UPP = 2.
      ASSIGN SEL_FAKTTYP:SCREEN-VALUE = SEL_FAKTTYP.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(faktdebkred)).                  
      RUN lastselectdyn_UI IN brwproc[1].            
      APPLY "VALUE-CHANGED" TO BRW_KRED.         
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_KRED:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch.         
   END.
   RUN laddakfmtemp_UI IN fakkoproch 
   (OUTPUT TABLE styrkfmtemp,OUTPUT TABLE faktureringstyptemp, OUTPUT TABLE kundtyptemp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_KRED:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN hamtafaktkred_UI (INPUT 1).
   IF NOT AVAILABLE faktkredtemp THEN RETURN.
   ASSIGN
   soktemp.SOKVAL = 97
   soktemp.SOKINT[1] = faktkredtemp.FAKTNR.
   {SOKANROP.I}      
   IF Guru.Konstanter:faktsekvar[4] = FALSE AND Guru.Konstanter:faktsekvar[5] = FALSE THEN DO:
      IF soktemp.SOKLOG[1] = TRUE THEN DO:
         MESSAGE "Fakturan är preliminär godkända och kan tas bort!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.    
      END.
   END.
   MESSAGE "Vill du ta bort den kreditfaktura " faktkredtemp.NAMN  " ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fakturera"
   UPDATE answer1 AS LOGICAL.
   
   IF answer1 THEN DO:
      IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO: 
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 99
         soktemp.SOKINT[1] = faktkredtemp.FAKTNR.
         {SOKANROP.I}      
         MESSAGE "Nu är faktura öppen för ändringar." VIEW-AS ALERT-BOX.
         RETURN.
      END.
      {muswait.i}
      RUN faktkredbort_UI IN fakthmth
      (INPUT faktkredtemp.FAKTNR,INPUT faktkredtemp.FDELNR).   
      RUN hamtafaktkred_UI (INPUT 2).
      DELETE faktdebkred.
      DELETE faktkredtemp.                                  
      RUN selnextprevrow_UI IN brwproc[1].
      OPEN QUERY BRW_KRED FOR EACH faktdebkred NO-LOCK BY faktdebkred.FDELNR BY faktdebkred.DATUM BY faktdebkred.VFAKTNR.   
      RUN lastselectdyn_UI IN brwproc[1].        
      IF AVAILABLE faktdebkred THEN DO:
         APPLY "VALUE-CHANGED" TO BRW_KRED.
      END.
      ELSE DO:
         ASSIGN
         FBTN_OK:HIDDEN = TRUE
         RAD_UPP:HIDDEN = TRUE
         BRW_KRED:HIDDEN = TRUE
         BTN_BORT:HIDDEN = TRUE 
         FBTN_GVFAK:HIDDEN = TRUE
         EDITOR_MEDD:HIDDEN = TRUE 
         FILL-IN_BESTALLARE:HIDDEN = TRUE 
         FILL-IN_BESTNAMN:HIDDEN = TRUE 
         FILL-IN_CO:HIDDEN = TRUE 
         FILL-IN_FAKADRESS:HIDDEN = TRUE
         FILL-IN_LAND:HIDDEN = TRUE
         FILL-IN_VAT:HIDDEN = TRUE  
         FILL-IN_FAKORT:HIDDEN = TRUE 
         FILL-IN_FAKPNR:HIDDEN = TRUE 
         FILL-IN_KONTAKT:HIDDEN = TRUE 
         FILL-IN_TEL:HIDDEN = TRUE 
         FILL-IN_VARREF:HIDDEN = TRUE
         SEL_FAKTTYP:HIDDEN = TRUE
         RAD_UPP:HIDDEN = TRUE.   
      END.
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN_AVDELNINGNR FILL-IN_AVDELNINGNAMN RAD_UPP SEL_FAKTTYP 
          EDITOR_MEDD FILL-IN_BESTNAMN FILL-IN_VARREF FILL-IN_BESTALLARE 
          FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_LAND 
          FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_VAT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_KRED FBTN_OK FBTN_GVFAK BTN_NY BTN_BORT RAD_UPP SEL_FAKTTYP 
         EDITOR_MEDD FILL-IN_BESTNAMN FILL-IN_VARREF FILL-IN_BESTALLARE 
         FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_LAND 
         FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_VAT BTN_AVB 
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
   RUN hamtafaktkred_UI (INPUT 1).
   RUN faktkrednamn_UI IN fakthmth
   (INPUT faktkredtemp.FAKTNR,INPUT faktkredtemp.FDELNR,INPUT faktkredtemp.VFAKTNR,INPUT Guru.Konstanter:globanv,OUTPUT TABLE faktnamntemp).           
   FIND FIRST faktnamntemp WHERE faktnamntemp.FAKTURNR = faktkredtemp.FAKTNR AND 
   faktnamntemp.FDELNR = faktkredtemp.FDELNR   NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_BESTNAMN = faktnamntemp.BESTNAMN
   FILL-IN_FAKPNR = faktnamntemp.FAKPNR
   FILL-IN_KONTAKT = faktnamntemp.KONTAKT
   FILL-IN_TEL = faktnamntemp.TEL   
   FILL-IN_VARREF = faktnamntemp.VARREF 
   FILL-IN_BESTALLARE = faktnamntemp.BESTALLARE
   FILL-IN_FAKADRESS = SUBSTRING(faktnamntemp.FAKADRESS,1,25)
   FILL-IN_CO = SUBSTRING(faktnamntemp.FAKADRESS,26,25) 
   FILL-IN_FAKORT = SUBSTRING(faktnamntemp.FAKORT,1,25) 
   FILL-IN_LAND = SUBSTRING(faktnamntemp.FAKORT,26,25).
   FILL-IN_VAT = SUBSTRING(faktnamntemp.FAKORT,52,25).   
   IF FILL-IN_BESTALLARE = "" THEN FILL-IN_BESTALLARE = FILL-IN_KONTAKT.   
   RAD_UPP = 1.
   RAD_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   APPLY "VALUE-CHANGED" TO RAD_UPP.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtafaktkred_UI WINDOW-1 
PROCEDURE hamtafaktkred_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FIND FIRST faktkredtemp WHERE faktkredtemp.FAKTNR = faktdebkred.FAKTNR AND 
      faktkredtemp.FDELNR = faktdebkred.FDELNR AND faktkredtemp.VFAKTNR = faktdebkred.VFAKTNR
      NO-LOCK NO-ERROR.   
   END.
   IF vad = 2 THEN DO:
      FIND FIRST faktdebkred WHERE faktdebkred.FAKTNR = faktkredtemp.FAKTNR AND 
      faktdebkred.FDELNR = faktkredtemp.FDELNR AND faktdebkred.VFAKTNR = faktkredtemp.VFAKTNR
      NO-LOCK NO-ERROR.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-1 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN faktnamn_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR. 
   status-ok = BRW_KRED:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN hamtafaktkred_UI (INPUT 1).
   IF NOT AVAILABLE faktkredtemp THEN DO:
      CREATE efaktkredtemp.      
      ASSIGN
      efaktkredtemp.FAKTNR = vfaktplantemp.FAKTNR
      efaktkredtemp.FDELNR = INTEGER(STRING(TODAY,"999999") + "1")
      efaktkredtemp.VFAKTNR = 0 
      efaktkredtemp.VKREDIT = 0 
      efaktkredtemp.NAMN = vfaktplantemp.NAMN
      efaktkredtemp.DATUM = TODAY     
      efaktkredtemp.FAKTTYPID = 0 
      efaktkredtemp.FAKTXT = ""
      efaktkredtemp.DEBKRED = FALSE.           
   END.
   ELSE DO:    
      CREATE efaktkredtemp.
      ASSIGN
      efaktkredtemp.FAKTNR = faktkredtemp.FAKTNR
      efaktkredtemp.FDELNR = INTEGER(STRING(TODAY,"999999") + "1")
      efaktkredtemp.VFAKTNR = 0 
      efaktkredtemp.VKREDIT = 0 
      efaktkredtemp.NAMN = faktkredtemp.NAMN
      efaktkredtemp.DATUM = TODAY     
      efaktkredtemp.FAKTTYPID = faktkredtemp.FAKTTYPID 
      efaktkredtemp.FAKTXT = faktkredtemp.FAKTXT
      efaktkredtemp.DEBKRED = FALSE.
      IF faktkredtemp.DEBKRED = TRUE THEN DO:
         MESSAGE "Vill du skapa en kreditfaktura av debetfakturan " faktkredtemp.VFAKTNR  " ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fakturera"
         UPDATE answer1 AS LOGICAL.
         IF answer1 THEN DO:
            efaktkredtemp.VFAKTNR = faktkredtemp.VFAKTNR.        
         END.
      END.            
      FIND LAST faktkredtemp WHERE faktkredtemp.DEBKRED = FALSE  
      USE-INDEX FDELNR NO-ERROR.
      IF AVAILABLE faktkredtemp THEN efaktkredtemp.FDELNR = faktkredtemp.FDELNR + 1.      
   END.  
   RUN faktkredny_UI IN fakthmth (INPUT-OUTPUT TABLE efaktkredtemp).             
   ASSIGN
   BRW_KRED:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   RAD_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   FIND FIRST efaktkredtemp NO-LOCK NO-ERROR.
   CREATE faktkredtemp.
   BUFFER-COPY efaktkredtemp TO faktkredtemp.
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR.
   CREATE faktdebkred.
   BUFFER-COPY faktkredtemp TO faktdebkred.
   IF faktkredtemp.DEBKRED = TRUE THEN DO:
      ASSIGN
      faktdebkred.KTOTPRIS = 0
      faktdebkred.KDATUM = ?.
   END.
   ELSE DO:
      ASSIGN
      faktdebkred.TOTPRIS = 0
      faktdebkred.DATUM = ?
      faktdebkred.KTOTPRIS = faktkredtemp.TOTPRIS
      faktdebkred.KDATUM =   faktkredtemp.DATUM.
      IF faktkredtemp.VFAKTNR NE 0 THEN DO:
         FIND FIRST faktkredtempbuff WHERE faktkredtempbuff.FAKTNR = faktkredtemp.FAKTNR AND
         faktkredtempbuff.VFAKTNR = faktkredtemp.VFAKTNR
         NO-LOCK NO-ERROR.
         ASSIGN
         faktdebkred.TOTPRIS = faktkredtempbuff.TOTPRIS
         faktdebkred.DATUM =   faktkredtempbuff.DATUM. 
      END.
   END.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(faktdebkred)).              
   OPEN QUERY BRW_KRED FOR EACH faktdebkred NO-LOCK BY faktdebkred.FDELNR BY faktdebkred.DATUM BY faktdebkred.VFAKTNR.   
   RUN faktnamn_UI.
   IF faktkredtemp.VFAKTNR = 0 THEN DO:
      RAD_UPP = 2.
   END.   
   RUN lastselectdyn_UI IN brwproc[1].            
   APPLY "VALUE-CHANGED" TO BRW_KRED.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparatemp_UI WINDOW-1 
PROCEDURE sparatemp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN   
   EDITOR_MEDD = INPUT FRAME {&FRAME-NAME} EDITOR_MEDD
   FILL-IN_BESTNAMN = INPUT FILL-IN_BESTNAMN
   FILL-IN_FAKADRESS = INPUT FILL-IN_FAKADRESS
   FILL-IN_LAND = INPUT FILL-IN_LAND
   FILL-IN_VAT = INPUT FILL-IN_VAT
   FILL-IN_FAKORT = INPUT FILL-IN_FAKORT
   FILL-IN_FAKPNR = INPUT FILL-IN_FAKPNR
   FILL-IN_KONTAKT = INPUT FILL-IN_KONTAKT
   FILL-IN_TEL = INPUT FILL-IN_TEL
   FILL-IN_VARREF = INPUT FILL-IN_VARREF
   FILL-IN_CO = INPUT FILL-IN_CO
   FILL-IN_BESTALLARE = INPUT FILL-IN_BESTALLARE
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI WINDOW-1 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   
   EDITOR_MEDD = INPUT FRAME {&FRAME-NAME} EDITOR_MEDD.
   RUN hamtafaktkred_UI (INPUT 1).
   IF faktkredtemp.VFAKTNR = 0 THEN DO:   
      FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPTEXT = SEL_FAKTTYP NO-LOCK NO-ERROR.        
      IF NOT AVAILABLE faktureringstyptemp THEN DO:
         MESSAGE "Du måste välja någon faktureringstyp." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         ASSIGN
         faktkredtemp.FAKTTYPID = faktureringstyptemp.FAKTTYPID.
      END.
   END.
   faktkredtemp.FAKTXT = EDITOR_MEDD.
   EMPTY TEMP-TABLE efaktkredtemp NO-ERROR. 
   CREATE efaktkredtemp.
   BUFFER-COPY faktkredtemp TO efaktkredtemp.
   EMPTY TEMP-TABLE efaktnamntemp NO-ERROR. 
   CREATE efaktnamntemp.
   BUFFER-COPY faktnamntemp TO efaktnamntemp.
   efaktnamntemp.FAKADRESS = "".
   efaktnamntemp.FAKORT = "".       
   ASSIGN  
   SUBSTRING(efaktnamntemp.FAKADRESS,1,25) = STRING(FILL-IN_FAKADRESS,"X(25)")
   SUBSTRING(efaktnamntemp.FAKADRESS,26,25) = FILL-IN_CO
   SUBSTRING(efaktnamntemp.FAKORT,1,25) = FILL-IN_FAKORT
   SUBSTRING(efaktnamntemp.FAKORT,26,25) = FILL-IN_LAND
   SUBSTRING(efaktnamntemp.FAKORT,52,25) = FILL-IN_VAT
   efaktnamntemp.BESTNAMN = FILL-IN_BESTNAMN 
   
   efaktnamntemp.FAKPNR = FILL-IN_FAKPNR 
   efaktnamntemp.KONTAKT = FILL-IN_KONTAKT
   efaktnamntemp.VARREF = FILL-IN_VARREF
   efaktnamntemp.TEL = FILL-IN_TEL
   efaktnamntemp.BESTALLARE = FILL-IN_BESTALLARE.           
   RUN faktkredspara_UI IN fakthmth (INPUT TABLE efaktkredtemp,INPUT TABLE efaktnamntemp).                
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

