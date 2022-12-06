&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/19/96 -  8:22 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{FAKTBILAG.I} 
{FAKTPLANTEMP.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER direkt AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER vfktnrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR fakbilag.
DEFINE INPUT-OUTPUT PARAMETER atervar AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR skrivutfakttemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE VARIABLE varbi AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_URVAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktureradtemp

/* Definitions for BROWSE BRW_URVAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URVAL faktureradtemp.VFAKTNR ~
faktureradtemp.FDELNR faktureradtemp.DATUM faktureradtemp.SENASTTID ~
faktureradtemp.NAMN faktureradtemp.TOTPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URVAL 
&Scoped-define QUERY-STRING-BRW_URVAL FOR EACH faktureradtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URVAL OPEN QUERY BRW_URVAL FOR EACH faktureradtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URVAL faktureradtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URVAL faktureradtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-46 RECT-47 FILL-IN_BESTNAMN ~
FILL-IN_VARREF FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN_TEL ~
FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_LAND ~
FILL-IN_VAT TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK ~
TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI TOG_KONT ~
RAD_UPP TOG_VISA EDITOR_MEDD BTN_EXCEL BTN_OK BTN_IE BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_BESTNAMN FILL-IN_VARREF ~
FILL-IN_BESTALLARE FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO ~
FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_LAND FILL-IN_VAT TOG_INGA TOG_TIDMED ~
TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST ~
TOG_TIDKLOCKNAM TOG_LON TOG_FRI TOG_KONT RAD_UPP TOG_VISA EDITOR_MEDD 

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

DEFINE BUTTON BTN_EXCEL AUTO-GO 
     LABEL "excel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IE AUTO-GO 
     LABEL "visa i IE" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9 NO-UNDO.

DEFINE VARIABLE FILL-IN_BESTALLARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Beställare" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_BESTNAMN AS CHARACTER FORMAT "x(50)" 
     LABEL "Beställare/Kund" 
     VIEW-AS FILL-IN 
     SIZE 33.38 BY 1.

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

DEFINE VARIABLE FILL-IN_VAT AS CHARACTER FORMAT "X(30)" 
     LABEL "VAT" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RAD_UPP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Välj fakturor", 1,
"Fakturatext", 2
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.75 BY 8.67
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.75 BY 6.5
     BGCOLOR 8 .

DEFINE VARIABLE TOG_FRI AS LOGICAL INITIAL no 
     LABEL "Visa fria kompletteringsposter" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.5 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_INGA AS LOGICAL INITIAL no 
     LABEL "Inga bilagor" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.63 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KONT AS LOGICAL INITIAL no 
     LABEL "Visa kontering" 
     VIEW-AS TOGGLE-BOX
     SIZE 38.5 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KOST AS LOGICAL INITIAL no 
     LABEL "Visa ingående kostnadsregistrering" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.25 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_LON AS LOGICAL INITIAL no 
     LABEL "Visa lönetillägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.25 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_PRIS AS LOGICAL INITIAL no 
     LABEL "Visa kostnadsfälten" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDEJMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ej ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCK AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med klockslag" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDKLOCKNAM AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med klockslag och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.88 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDMED AS LOGICAL INITIAL no 
     LABEL "Visa tidskrivning som ingår i fakturan" 
     VIEW-AS TOGGLE-BOX
     SIZE 42.38 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOT AS LOGICAL INITIAL no 
     LABEL "Visa detaljerad tidskrivning med totaltid" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_TIDTOTNAMN AS LOGICAL INITIAL no 
     LABEL "Visa det. tid. med totaltid och namn" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_VISA AS LOGICAL INITIAL no 
     LABEL "Åter denna bild efter visning av faktura" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_URVAL FOR 
      faktureradtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_URVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URVAL DIALOG-1 _STRUCTURED
  QUERY BRW_URVAL NO-LOCK DISPLAY
      faktureradtemp.VFAKTNR COLUMN-LABEL "Faktura!nummer" FORMAT ">>>>>>>>>9":U
      faktureradtemp.FDELNR COLUMN-LABEL "Delnr" FORMAT "99999999":U
      faktureradtemp.DATUM COLUMN-LABEL "Fakturerad" FORMAT "99/99/99":U
      faktureradtemp.SENASTTID COLUMN-LABEL "Tid t.o.m." FORMAT "99/99/99":U
            WIDTH 9
      faktureradtemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U
            WIDTH 20
      faktureradtemp.TOTPRIS COLUMN-LABEL "Total" FORMAT "->>>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 72.5 BY 9
         TITLE "Fakturor".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_BESTNAMN AT ROW 2.04 COL 18.13 COLON-ALIGNED
     FILL-IN_VARREF AT ROW 2.04 COL 60.75 COLON-ALIGNED
     FILL-IN_BESTALLARE AT ROW 3.17 COL 18.13 COLON-ALIGNED
     FILL-IN_KONTAKT AT ROW 3.17 COL 60.75 COLON-ALIGNED
     FILL-IN_TEL AT ROW 4.29 COL 18.13 COLON-ALIGNED
     FILL-IN_FAKADRESS AT ROW 5.42 COL 18.13 COLON-ALIGNED
     FILL-IN_CO AT ROW 6.54 COL 18.13 COLON-ALIGNED
     FILL-IN_FAKPNR AT ROW 7.67 COL 18.13 COLON-ALIGNED
     FILL-IN_FAKORT AT ROW 7.67 COL 33.75 COLON-ALIGNED
     FILL-IN_LAND AT ROW 7.67 COL 60.75 COLON-ALIGNED WIDGET-ID 4
     FILL-IN_VAT AT ROW 8.79 COL 60.75 COLON-ALIGNED WIDGET-ID 6
     TOG_INGA AT ROW 10.96 COL 48
     TOG_TIDMED AT ROW 11.75 COL 3.13
     TOG_TIDTOT AT ROW 11.75 COL 48
     TOG_TIDEJMED AT ROW 12.54 COL 3.13
     TOG_TIDKLOCK AT ROW 12.54 COL 48
     TOG_PRIS AT ROW 13.33 COL 3.13
     TOG_TIDTOTNAMN AT ROW 13.33 COL 48
     TOG_KOST AT ROW 14.13 COL 3.13
     TOG_TIDKLOCKNAM AT ROW 14.13 COL 48
     TOG_LON AT ROW 14.92 COL 3.13
     TOG_FRI AT ROW 14.92 COL 48
     TOG_KONT AT ROW 15.63 COL 48
     RAD_UPP AT ROW 17.04 COL 1.5 NO-LABEL
     TOG_VISA AT ROW 17.04 COL 62.5
     BRW_URVAL AT ROW 18.13 COL 1.5
     EDITOR_MEDD AT ROW 18.13 COL 19.88 NO-LABEL
     BTN_EXCEL AT ROW 27.25 COL 34 WIDGET-ID 2
     BTN_OK AT ROW 27.25 COL 49.25
     BTN_IE AT ROW 27.25 COL 64.25
     BTN_AVSL AT ROW 27.25 COL 79
     "BILAGOR :" VIEW-AS TEXT
          SIZE 19.75 BY 1 AT ROW 10.38 COL 2.63
          FONT 17
     RECT-46 AT ROW 1.46 COL 1.5
     RECT-47 AT ROW 10.25 COL 1.5
     SPACE(0.74) SKIP(11.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj faktura".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: faktureradtemp T "?" NO-UNDO temp-db faktureradtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_URVAL TOG_VISA DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_URVAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_URVAL:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_URVAL:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

ASSIGN 
       EDITOR_MEDD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URVAL
/* Query rebuild information for BROWSE BRW_URVAL
     _TblList          = "Temp-Tables.faktureradtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktureradtemp.VFAKTNR
"faktureradtemp.VFAKTNR" "Faktura!nummer" ">>>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktureradtemp.FDELNR
"faktureradtemp.FDELNR" "Delnr" "99999999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktureradtemp.DATUM
"faktureradtemp.DATUM" "Fakturerad" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.faktureradtemp.SENASTTID
"faktureradtemp.SENASTTID" "Tid t.o.m." ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.faktureradtemp.NAMN
"faktureradtemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.faktureradtemp.TOTPRIS
"faktureradtemp.TOTPRIS" "Total" "->>>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URVAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Välj faktura */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Välj faktura */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_URVAL
&Scoped-define SELF-NAME BRW_URVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_URVAL DIALOG-1
ON VALUE-CHANGED OF BRW_URVAL IN FRAME DIALOG-1 /* Fakturor */
DO:
   status-ok = BRW_URVAL:SELECT-FOCUSED-ROW() NO-ERROR.
   RUN faktnamn_UI.
   EDITOR_MEDD = faktureradtemp.FAKTXT.

   /*
   RUN hamta_UI.
   */
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


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL DIALOG-1
ON CHOOSE OF BTN_EXCEL IN FRAME DIALOG-1 /* excel */
DO:
   visvalvar = 5.
   musz = FALSE.
   RUN ok_UI.
   {BORTBRWPROC.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IE DIALOG-1
ON CHOOSE OF BTN_IE IN FRAME DIALOG-1 /* visa i IE */
DO:
   visvalvar = 3.
   musz = FALSE.
   RUN ok_UI.
   {BORTBRWPROC.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* visa */
DO:
   visvalvar = 1.
   musz = FALSE.
   RUN ok_UI.
   {BORTBRWPROC.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_MEDD DIALOG-1
ON LEAVE OF EDITOR_MEDD IN FRAME DIALOG-1
DO:
   EDITOR_MEDD = INPUT EDITOR_MEDD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTALLARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTALLARE DIALOG-1
ON ENTRY OF FILL-IN_BESTALLARE IN FRAME DIALOG-1 /* Beställare */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTALLARE DIALOG-1
ON LEAVE OF FILL-IN_BESTALLARE IN FRAME DIALOG-1 /* Beställare */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTNAMN DIALOG-1
ON ENTRY OF FILL-IN_BESTNAMN IN FRAME DIALOG-1 /* Beställare/Kund */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTNAMN DIALOG-1
ON LEAVE OF FILL-IN_BESTNAMN IN FRAME DIALOG-1 /* Beställare/Kund */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_CO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CO DIALOG-1
ON ENTRY OF FILL-IN_CO IN FRAME DIALOG-1
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CO DIALOG-1
ON LEAVE OF FILL-IN_CO IN FRAME DIALOG-1
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKADRESS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKADRESS DIALOG-1
ON ENTRY OF FILL-IN_FAKADRESS IN FRAME DIALOG-1 /* Fakturaadress */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKADRESS DIALOG-1
ON LEAVE OF FILL-IN_FAKADRESS IN FRAME DIALOG-1 /* Fakturaadress */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKORT DIALOG-1
ON ENTRY OF FILL-IN_FAKORT IN FRAME DIALOG-1 /* Ort */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKORT DIALOG-1
ON LEAVE OF FILL-IN_FAKORT IN FRAME DIALOG-1 /* Ort */
DO:    
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKPNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKPNR DIALOG-1
ON ENTRY OF FILL-IN_FAKPNR IN FRAME DIALOG-1 /* Postnr */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKPNR DIALOG-1
ON LEAVE OF FILL-IN_FAKPNR IN FRAME DIALOG-1 /* Postnr */
DO:
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KONTAKT DIALOG-1
ON ENTRY OF FILL-IN_KONTAKT IN FRAME DIALOG-1 /* Er ref */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KONTAKT DIALOG-1
ON LEAVE OF FILL-IN_KONTAKT IN FRAME DIALOG-1 /* Er ref */
DO:
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAND DIALOG-1
ON ENTRY OF FILL-IN_LAND IN FRAME DIALOG-1 /* Land */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAND DIALOG-1
ON LEAVE OF FILL-IN_LAND IN FRAME DIALOG-1 /* Land */
DO:    
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEL DIALOG-1
ON ENTRY OF FILL-IN_TEL IN FRAME DIALOG-1 /* Tel */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEL DIALOG-1
ON LEAVE OF FILL-IN_TEL IN FRAME DIALOG-1 /* Tel */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VARREF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARREF DIALOG-1
ON ENTRY OF FILL-IN_VARREF IN FRAME DIALOG-1 /* Vår ref */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARREF DIALOG-1
ON LEAVE OF FILL-IN_VARREF IN FRAME DIALOG-1 /* Vår ref */
DO:
   RUN sparatemp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VAT DIALOG-1
ON ENTRY OF FILL-IN_VAT IN FRAME DIALOG-1 /* VAT */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VAT DIALOG-1
ON LEAVE OF FILL-IN_VAT IN FRAME DIALOG-1 /* VAT */
DO:    
   RUN sparatemp_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_UPP DIALOG-1
ON VALUE-CHANGED OF RAD_UPP IN FRAME DIALOG-1
DO:
   RAD_UPP = INPUT RAD_UPP.   
   ASSIGN
   EDITOR_MEDD:HIDDEN = TRUE 
   BRW_URVAL:HIDDEN = TRUE.
   IF RAD_UPP = 1 THEN DO:
      ASSIGN      
      BRW_URVAL:HIDDEN = FALSE.      
   END.
   ELSE IF RAD_UPP = 2 THEN DO:
      ASSIGN
      EDITOR_MEDD:HIDDEN = FALSE. 
      DISPLAY EDITOR_MEDD  WITH FRAME {&FRAME-NAME}.      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FRI DIALOG-1
ON ENTRY OF TOG_FRI IN FRAME DIALOG-1 /* Visa fria kompletteringsposter */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FRI DIALOG-1
ON VALUE-CHANGED OF TOG_FRI IN FRAME DIALOG-1 /* Visa fria kompletteringsposter */
DO:
   IF INPUT TOG_FRI = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INGA DIALOG-1
ON ENTRY OF TOG_INGA IN FRAME DIALOG-1 /* Inga bilagor */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INGA DIALOG-1
ON VALUE-CHANGED OF TOG_INGA IN FRAME DIALOG-1 /* Inga bilagor */
DO:                 
   TOG_INGA = INPUT TOG_INGA.       
   IF TOG_INGA = TRUE THEN DO: 
      ASSIGN 
      TOG_FRI = FALSE 
      TOG_KOST = FALSE
      TOG_LON = FALSE
      TOG_PRIS = FALSE
      TOG_TIDEJMED = FALSE
      TOG_TIDKLOCK = FALSE 
      TOG_TIDMED = FALSE
      TOG_TIDTOT = FALSE
      TOG_TIDKLOCKNAM = FALSE
      TOG_KONT = FALSE
      TOG_TIDTOTNAMN = FALSE.
      DISPLAY  
      TOG_KONT
      TOG_FRI 
      TOG_INGA 
      TOG_KOST 
      TOG_LON 
      TOG_PRIS 
      TOG_TIDEJMED 
      TOG_TIDKLOCK 
      TOG_TIDMED 
      TOG_TIDTOT  
      TOG_TIDKLOCKNAM 
      TOG_TIDTOTNAMN
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      TOG_KOST = TRUE
      TOG_FRI = TRUE
      TOG_PRIS = TRUE
      TOG_LON = TRUE
      TOG_TIDMED = TRUE.         
      DISPLAY  
      TOG_FRI 
      TOG_INGA 
      TOG_KOST 
      TOG_LON 
      TOG_PRIS 
      TOG_TIDEJMED 
      TOG_TIDKLOCK 
      TOG_TIDMED 
      TOG_TIDTOT  
      WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONT DIALOG-1
ON VALUE-CHANGED OF TOG_KONT IN FRAME DIALOG-1 /* Visa kontering */
DO:
   IF INPUT TOG_KONT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOST DIALOG-1
ON ENTRY OF TOG_KOST IN FRAME DIALOG-1 /* Visa ingående kostnadsregistrering */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOST DIALOG-1
ON VALUE-CHANGED OF TOG_KOST IN FRAME DIALOG-1 /* Visa ingående kostnadsregistrering */
DO:
   IF INPUT TOG_KOST = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LON DIALOG-1
ON ENTRY OF TOG_LON IN FRAME DIALOG-1 /* Visa lönetillägg */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LON DIALOG-1
ON VALUE-CHANGED OF TOG_LON IN FRAME DIALOG-1 /* Visa lönetillägg */
DO:
   IF INPUT TOG_LON = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PRIS DIALOG-1
ON ENTRY OF TOG_PRIS IN FRAME DIALOG-1 /* Visa kostnadsfälten */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PRIS DIALOG-1
ON VALUE-CHANGED OF TOG_PRIS IN FRAME DIALOG-1 /* Visa kostnadsfälten */
DO:
   IF INPUT TOG_PRIS = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDEJMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDEJMED DIALOG-1
ON ENTRY OF TOG_TIDEJMED IN FRAME DIALOG-1 /* Visa tidskrivning som ej ingår i fakturan */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDEJMED DIALOG-1
ON VALUE-CHANGED OF TOG_TIDEJMED IN FRAME DIALOG-1 /* Visa tidskrivning som ej ingår i fakturan */
DO:
   IF INPUT TOG_TIDEJMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCK DIALOG-1
ON ENTRY OF TOG_TIDKLOCK IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med klockslag */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCK DIALOG-1
ON VALUE-CHANGED OF TOG_TIDKLOCK IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med klockslag */
DO:
   IF INPUT TOG_TIDKLOCK = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDKLOCKNAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCKNAM DIALOG-1
ON ENTRY OF TOG_TIDKLOCKNAM IN FRAME DIALOG-1 /* Visa det. tid. med klockslag och namn */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDKLOCKNAM DIALOG-1
ON VALUE-CHANGED OF TOG_TIDKLOCKNAM IN FRAME DIALOG-1 /* Visa det. tid. med klockslag och namn */
DO:
   IF INPUT TOG_TIDKLOCKNAM = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDMED DIALOG-1
ON ENTRY OF TOG_TIDMED IN FRAME DIALOG-1 /* Visa tidskrivning som ingår i fakturan */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDMED DIALOG-1
ON VALUE-CHANGED OF TOG_TIDMED IN FRAME DIALOG-1 /* Visa tidskrivning som ingår i fakturan */
DO:
   IF INPUT TOG_TIDMED = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOT DIALOG-1
ON ENTRY OF TOG_TIDTOT IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med totaltid */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOT DIALOG-1
ON VALUE-CHANGED OF TOG_TIDTOT IN FRAME DIALOG-1 /* Visa detaljerad tidskrivning med totaltid */
DO:
   IF INPUT TOG_TIDTOT = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDTOTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOTNAMN DIALOG-1
ON ENTRY OF TOG_TIDTOTNAMN IN FRAME DIALOG-1 /* Visa det. tid. med totaltid och namn */
DO:
   RUN hitta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDTOTNAMN DIALOG-1
ON VALUE-CHANGED OF TOG_TIDTOTNAMN IN FRAME DIALOG-1 /* Visa det. tid. med totaltid och namn */
DO:
   IF INPUT TOG_TIDTOTNAMN = TRUE THEN DO:
      TOG_INGA = FALSE.
      DISPLAY TOG_INGA WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VISA DIALOG-1
ON VALUE-CHANGED OF TOG_VISA IN FRAME DIALOG-1 /* Åter denna bild efter visning av faktura */
DO:
   TOG_VISA = INPUT TOG_VISA.
   atervar = TOG_VISA.
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
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   FILL-IN_BESTNAMN:LABEL = Guru.Konstanter:gbestk.
   ASSIGN
   TOG_VISA = atervar
   TOG_KOST = TRUE
   TOG_FRI = TRUE
   TOG_PRIS = TRUE
   TOG_LON = TRUE
   TOG_TIDMED = TRUE.           
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-LOCK NO-ERROR.
   ASSIGN
   FRAME {&FRAME-NAME}:TITLE = "Välj faktura " + STRING(vfaktplantemp.FAKTNR) + " " + vfaktplantemp.NAMN.   
   RUN hamtaenfakturd_UI IN fakthmth (INPUT infakplannr,OUTPUT TABLE faktureradtemp).         
   RUN enable_UI.   
   /*Robin Sjöberg Elpool i Umeå AB  2 jan 2014 10:47:41 
   Fakturor till excel 
   */
   
   {FRMSIZED.I}        
   {DIA_M_SLUT.I}
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVSL:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   Guru.GlobalaVariabler:colrighth = BTN_IE:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_OK:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF Guru.Konstanter:globforetag = "elpa" THEN.
   ELSE BTN_EXCEL:HIDDEN = TRUE.  
   
   {musarrow.i}
      /*
   IF Guru.Konstanter:globforetag = "ELPA" OR  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" THEN .
   ELSE DO:     
      BTN_IE:HIDDEN = TRUE.
      BTN_OK:COLUMN = BTN_IE:COLUMN.
   END.
   */   
   
   IF direkt = TRUE THEN DO:  
      IF vfaktplantemp.FDELNR NE 0 THEN DO:
         FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = vfaktplantemp.FAKTNR AND 
         faktureradtemp.FDELNR = vfaktplantemp.FDELNR NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = vfaktplantemp.FAKTNR AND 
         faktureradtemp.VFAKTNR = vfktnrvar NO-LOCK NO-ERROR.
      END.         
      EDITOR_MEDD = faktureradtemp.FAKTXT.
      RAD_UPP = 2.
      DISPLAY RAD_UPP EDITOR_MEDD WITH FRAME {&FRAME-NAME}. 
      APPLY "VALUE-CHANGED" TO RAD_UPP.
      DISABLE RAD_UPP WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:      
      ENABLE BRW_URVAL WITH FRAME {&FRAME-NAME}.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = vfaktplantemp.FAKTNR AND 
      faktureradtemp.VFAKTNR = vfktnrvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE faktureradtemp THEN DO:
         GET LAST BRW_URVAL NO-LOCK.
      END.
      IF NOT AVAILABLE faktureradtemp THEN DO:         
         MESSAGE "Det finns inga fakturor att visa!" VIEW-AS ALERT-BOX.
         musz = TRUE.
         LEAVE MAIN-BLOCK.
      END.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(faktureradtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].      
      EDITOR_MEDD = faktureradtemp.FAKTXT.
      RAD_UPP = 1.
      DISPLAY RAD_UPP EDITOR_MEDD WITH FRAME {&FRAME-NAME}. 
      APPLY "VALUE-CHANGED" TO RAD_UPP.
      
   END.      
   RUN faktnamn_UI.   
   IF direkt = TRUE THEN TOG_VISA:HIDDEN = TRUE.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_URVAL:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "FDELNR").
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
  DISPLAY FILL-IN_BESTNAMN FILL-IN_VARREF FILL-IN_BESTALLARE FILL-IN_KONTAKT 
          FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO FILL-IN_FAKPNR FILL-IN_FAKORT 
          FILL-IN_LAND FILL-IN_VAT TOG_INGA TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED 
          TOG_TIDKLOCK TOG_PRIS TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON 
          TOG_FRI TOG_KONT RAD_UPP TOG_VISA EDITOR_MEDD 
      WITH FRAME DIALOG-1.
  ENABLE RECT-46 RECT-47 FILL-IN_BESTNAMN FILL-IN_VARREF FILL-IN_BESTALLARE 
         FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_FAKADRESS FILL-IN_CO 
         FILL-IN_FAKPNR FILL-IN_FAKORT FILL-IN_LAND FILL-IN_VAT TOG_INGA 
         TOG_TIDMED TOG_TIDTOT TOG_TIDEJMED TOG_TIDKLOCK TOG_PRIS 
         TOG_TIDTOTNAMN TOG_KOST TOG_TIDKLOCKNAM TOG_LON TOG_FRI TOG_KONT 
         RAD_UPP TOG_VISA EDITOR_MEDD BTN_EXCEL BTN_OK BTN_IE BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fakexcel_UI DIALOG-1 
PROCEDURE fakexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktnamn_UI DIALOG-1 
PROCEDURE faktnamn_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   varbi = faktureradtemp.BILAGOR.                         
   IF varbi = "" THEN DO:
      ASSIGN
      TOG_KOST = TRUE
      TOG_FRI = TRUE
      TOG_PRIS = TRUE
      TOG_LON = TRUE
      TOG_TIDMED = TRUE.
   END.
   ELSE DO:
      ASSIGN
      TOG_KOST = FALSE
      TOG_FRI = FALSE
      TOG_PRIS = FALSE
      TOG_LON = FALSE
      TOG_TIDMED = FALSE.
      IF varbi = ",0," THEN TOG_INGA = TRUE.
      ELSE DO:
         TOG_INGA = FALSE.
         IF INDEX(varbi,",1,") NE 0 THEN TOG_FRI = TRUE.
         IF INDEX(varbi,",2,") NE 0 THEN TOG_KOST = TRUE.
         IF INDEX(varbi,",3,") NE 0 THEN TOG_TIDEJMED = TRUE.
         IF INDEX(varbi,",4,") NE 0 THEN TOG_TIDKLOCK = TRUE.
         IF INDEX(varbi,",5,") NE 0 THEN TOG_TIDMED = TRUE.
         IF INDEX(varbi,",6,") NE 0 THEN TOG_TIDTOT = TRUE.
         IF INDEX(varbi,",7,") NE 0 THEN TOG_LON = TRUE.
         IF INDEX(varbi,",8,") NE 0 THEN TOG_PRIS = TRUE.
         IF INDEX(varbi,",9,") NE 0 THEN TOG_TIDKLOCKNAM = TRUE.
         IF INDEX(varbi,",10,") NE 0 THEN TOG_TIDTOTNAMN = TRUE. 
         IF INDEX(varbi,",11,") NE 0 THEN TOG_KONT = TRUE. 
      END.
   END.
   RUN faktnamn_UI IN fakthmth 
   (INPUT 1,INPUT infakplannr,INPUT faktureradtemp.FDELNR,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE faktnamntemp).     
   RUN hamta_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI DIALOG-1 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST faktnamntemp WHERE faktnamntemp.FAKTURNR = infakplannr AND faktnamntemp.FDELNR = faktureradtemp.FDELNR NO-LOCK NO-ERROR.
   IF AVAILABLE faktnamntemp THEN DO:
      ASSIGN
      FILL-IN_BESTNAMN = faktnamntemp.BESTNAMN
      FILL-IN_FAKADRESS = SUBSTRING(faktnamntemp.FAKADRESS,1,25)
      FILL-IN_CO = SUBSTRING(faktnamntemp.FAKADRESS,26,25)
      FILL-IN_FAKORT = SUBSTRING(faktnamntemp.FAKORT,1,25) 
      FILL-IN_LAND = SUBSTRING(faktnamntemp.FAKORT,26,25)
      FILL-IN_VAT = SUBSTRING(faktnamntemp.FAKORT,52,25)
      FILL-IN_FAKPNR = faktnamntemp.FAKPNR
      FILL-IN_KONTAKT = faktnamntemp.KONTAKT
      FILL-IN_TEL = faktnamntemp.TEL   
      FILL-IN_VARREF = faktnamntemp.VARREF 
      FILL-IN_BESTALLARE = faktnamntemp.BESTALLARE.
          
   END.
   
   IF FILL-IN_BESTALLARE = "" THEN FILL-IN_BESTALLARE = FILL-IN_KONTAKT.
   DISPLAY FILL-IN_BESTNAMN FILL-IN_FAKADRESS FILL-IN_FAKORT FILL-IN_LAND FILL-IN_VAT
   FILL-IN_FAKPNR FILL-IN_KONTAKT FILL-IN_TEL FILL-IN_VARREF  
   FILL-IN_BESTALLARE FILL-IN_CO FILL-IN_BESTALLARE 
   TOG_FRI TOG_KOST TOG_TIDEJMED TOG_TIDKLOCK TOG_TIDMED TOG_TIDTOT TOG_LON 
   TOG_PRIS TOG_TIDKLOCKNAM TOG_TIDTOTNAMN TOG_INGA 
   WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hitta_UI DIALOG-1 
PROCEDURE hitta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*FIND faktnamnbuff WHERE RECID(faktnamnbuff) = RECID(FAKTNAMN) NO-LOCK NO-ERROR.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI DIALOG-1 
PROCEDURE ok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
   ASSIGN
   TOG_FRI = INPUT FRAME {&FRAME-NAME} TOG_FRI
   TOG_KOST = INPUT TOG_KOST 
   TOG_TIDEJMED = INPUT TOG_TIDEJMED
   TOG_TIDKLOCK = INPUT TOG_TIDKLOCK
   TOG_TIDMED = INPUT TOG_TIDMED
   TOG_TIDTOT = INPUT TOG_TIDTOT
   TOG_LON = INPUT TOG_LON
   TOG_PRIS = INPUT TOG_PRIS
   TOG_KONT = INPUT TOG_KONT
   TOG_TIDKLOCKNAM = INPUT TOG_TIDKLOCKNAM
   TOG_TIDTOTNAMN = INPUT TOG_TIDTOTNAMN.
 /*  EDITOR_MEDD = INPUT EDITOR_MEDD.*/
   ASSIGN
   faktureradtemp.ANTALRADER = EDITOR_MEDD:NUM-LINES
   faktureradtemp.FAKTXT = EDITOR_MEDD. 
   IF TOG_INGA = FALSE THEN DO:      
      IF TOG_FRI = FALSE AND TOG_KOST = FALSE AND TOG_TIDEJMED = FALSE AND
      TOG_TIDKLOCK = FALSE AND TOG_TIDMED = FALSE AND TOG_TIDTOT = FALSE AND
      TOG_LON = FALSE AND TOG_PRIS = FALSE AND TOG_TIDKLOCKNAM = FALSE AND
      TOG_TIDTOTNAMN = FALSE THEN musz = musz.
      ELSE DO:
         CREATE fakbilag.
         ASSIGN
         fakbilag.FRI = TOG_FRI
         fakbilag.KOST = TOG_KOST 
         fakbilag.TIDEJMED = TOG_TIDEJMED
         fakbilag.TIDKLOCK = TOG_TIDKLOCK
         fakbilag.TIDMED = TOG_TIDMED
         fakbilag.TIDTOT = TOG_TIDTOT
         fakbilag.LON = TOG_LON
         fakbilag.PRIS = TOG_PRIS
         fakbilag.TOTNAMN = TOG_TIDKLOCKNAM
         fakbilag.KONTO = TOG_KONT
         fakbilag.KLOCKNAMN = TOG_TIDTOTNAMN.      
      END.   
   END.      
   IF TOG_INGA = TRUE THEN DO:
      varbi = ",0,".
   END.
   ELSE DO:
      varbi = ",".
      IF TOG_FRI = TRUE THEN varbi = varbi + "1,".
      IF TOG_KOST = TRUE THEN varbi = varbi + "2,".
      IF TOG_TIDEJMED = TRUE THEN varbi = varbi + "3,".
      IF TOG_TIDKLOCK = TRUE THEN varbi = varbi + "4,".
      IF TOG_TIDMED = TRUE THEN varbi = varbi + "5,".
      IF TOG_TIDTOT = TRUE THEN varbi = varbi + "6,".
      IF TOG_LON = TRUE THEN varbi = varbi + "7,".   
      IF TOG_PRIS = TRUE THEN varbi = varbi + "8,".
      IF TOG_TIDKLOCKNAM = TRUE THEN varbi = varbi + "9,".
      IF TOG_TIDTOTNAMN = TRUE THEN varbi = varbi + "10,".
      IF TOG_KONT = TRUE THEN varbi = varbi + "11,". 
   END.  
   IF direkt = TRUE THEN DO:
      EMPTY TEMP-TABLE skrivutfakttemp NO-ERROR.
      IF  vfktnrvar = 0 THEN
      RUN slutbildatum_UI IN fakthmth (INPUT infakplannr,INPUT faktureradtemp.FDELNR,INPUT TRUE,INPUT varbi).   
      RUN faktnamn_UI IN fakthmth 
      (INPUT 2,INPUT infakplannr,INPUT faktureradtemp.FDELNR,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE faktnamntemp).     
      EMPTY TEMP-TABLE allafaktureradtemp NO-ERROR. 
      CREATE allafaktureradtemp.
      BUFFER-COPY faktureradtemp TO allafaktureradtemp.
      CREATE skrivutfakttemp.
      ASSIGN
      skrivutfakttemp.VFAKTNR = faktureradtemp.VFAKTNR.      
      vfktnrvar = faktureradtemp.VFAKTNR.
      RUN sparfakturerad_UI IN fakthmth (INPUT TABLE allafaktureradtemp).

   END.
   ELSE DO:

      antal_valda = BRW_URVAL:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      IF antal_valda = 0 THEN DO:
         MESSAGE "Ni måste välja någon faktura!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         EMPTY TEMP-TABLE skrivutfakttemp NO-ERROR.
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_URVAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE faktureradtemp THEN DO:
               CREATE skrivutfakttemp.
               ASSIGN
               skrivutfakttemp.VFAKTNR = faktureradtemp.VFAKTNR.      
               vfktnrvar = faktureradtemp.VFAKTNR.
               IF  vfktnrvar = 0 THEN RUN slutbildatum_UI IN fakthmth (INPUT infakplannr,INPUT faktureradtemp.FDELNR,INPUT TRUE,INPUT varbi).   
               RUN faktnamn_UI IN fakthmth (INPUT 2,INPUT infakplannr,INPUT faktureradtemp.FDELNR,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE faktnamntemp).     
               EMPTY TEMP-TABLE allafaktureradtemp NO-ERROR. 
               CREATE allafaktureradtemp.
               BUFFER-COPY faktureradtemp TO allafaktureradtemp.
               RUN sparfakturerad_UI IN fakthmth (INPUT TABLE allafaktureradtemp).
            END.
            antal_raknare = antal_raknare + 1.   
         END.
      END.    
   END.
   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparatemp_UI DIALOG-1 
PROCEDURE sparatemp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN
   TOG_FRI = INPUT FRAME {&FRAME-NAME} TOG_FRI
   TOG_KOST = INPUT TOG_KOST 
   TOG_TIDEJMED = INPUT TOG_TIDEJMED
   TOG_TIDKLOCK = INPUT TOG_TIDKLOCK
   TOG_TIDMED = INPUT TOG_TIDMED
   TOG_TIDTOT = INPUT TOG_TIDTOT
   TOG_LON = INPUT TOG_LON
   TOG_PRIS = INPUT TOG_PRIS
   TOG_TIDKLOCKNAM = INPUT TOG_TIDKLOCKNAM
   TOG_TIDTOTNAMN = INPUT TOG_TIDTOTNAMN 
   FILL-IN_BESTALLARE = INPUT FILL-IN_BESTALLARE
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
   RUN spara_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI DIALOG-1 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST faktnamntemp WHERE faktnamntemp.FAKTURNR = infakplannr AND faktnamntemp.FDELNR = faktureradtemp.FDELNR NO-LOCK NO-ERROR.
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
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

