&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 95/05/10 -  1:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{ALLDEF.I}
{ANVPERS.I}
{SOKDEF.I}
{FAKTPLANTEMP.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE VARIABLE meddh AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE fuppapph AS HANDLE NO-UNDO.
DEFINE VARIABLE vart AS INTEGER NO-UNDO.
DEFINE VARIABLE fnr AS INTEGER NO-UNDO.
DEFINE VARIABLE innr AS INTEGER NO-UNDO.
DEFINE VARIABLE lasanv AS CHARACTER NO-UNDO.
DEFINE BUFFER fbuff FOR faktureradtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp faktureradtemp faktkredtemp

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_FAKT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FAKT faktureradtemp.SENASTTID ~
faktureradtemp.FAKTNR faktureradtemp.FDELNR faktureradtemp.VFAKTNR ~
faktureradtemp.DATUM faktureradtemp.NAMN faktureradtemp.TOTPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FAKT 
&Scoped-define QUERY-STRING-BRW_FAKT FOR EACH faktureradtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FAKT OPEN QUERY BRW_FAKT FOR EACH faktureradtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FAKT faktureradtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FAKT faktureradtemp


/* Definitions for BROWSE BRW_KRED                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KRED faktkredtemp.FAKTNR ~
faktkredtemp.FDELNR faktkredtemp.VKREDIT faktkredtemp.DATUM ~
faktkredtemp.VFAKTNR faktkredtemp.NAMN faktkredtemp.TOTPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KRED 
&Scoped-define QUERY-STRING-BRW_KRED FOR EACH faktkredtemp NO-LOCK ~
    BY faktkredtemp.FDELNR ~
       BY faktkredtemp.VFAKTNR
&Scoped-define OPEN-QUERY-BRW_KRED OPEN QUERY BRW_KRED FOR EACH faktkredtemp NO-LOCK ~
    BY faktkredtemp.FDELNR ~
       BY faktkredtemp.VFAKTNR.
&Scoped-define TABLES-IN-QUERY-BRW_KRED faktkredtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KRED faktkredtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ANV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAD BRW_FAKT BRW_KRED BRW_ANV ~
EDITOR_MEDD BTN_TAB FBTN_PRELB FILL-IN_ANVANDARE BTN_UPPARB BTN_PREV ~
BTN_NASTA BTN_SKR BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAD FILL-IN_SANDARE FILL-IN_SDATUM ~
EDITOR_MEDD FILL-IN-ADMTEXT FILL-IN_ANVANDARE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_NASTA 
     LABEL "Nästa med." 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PREV 
     LABEL "Föregåend med." 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TAB 
     LABEL "Ta bort faktura" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPPARB 
     LABEL "Upparbetad kostnad" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_PRELB 
     LABEL "Ta bort prel.fakt.":L 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-ADMTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Ange huvudansvarig för fakturering" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_SANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Sändare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_SDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Sänt datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE RAD_VAD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Administration", 1,
"Meddelande", 2,
"Ta bort debet fakturor", 3,
"Ta bort kredit fakturor", 4
     SIZE 88 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_FAKT FOR 
      faktureradtemp SCROLLING.

DEFINE QUERY BRW_KRED FOR 
      faktkredtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV DIALOG-1 _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(40)":U
  ENABLE
      anvandartemp.ANVANDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 55.5 BY 9.92
         TITLE "Guruanvändare".

DEFINE BROWSE BRW_FAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FAKT DIALOG-1 _STRUCTURED
  QUERY BRW_FAKT NO-LOCK DISPLAY
      faktureradtemp.SENASTTID COLUMN-LABEL "Tid t.o.m." FORMAT "99/99/99":U
            WIDTH 9
      faktureradtemp.FAKTNR COLUMN-LABEL "Faktplan!nr" FORMAT ">>>>>>9":U
      faktureradtemp.FDELNR COLUMN-LABEL "Delnr" FORMAT "999999":U
      faktureradtemp.VFAKTNR COLUMN-LABEL "Faktura!nummer" FORMAT ">>>>>>>>>9":U
      faktureradtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      faktureradtemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(20)":U
      faktureradtemp.TOTPRIS COLUMN-LABEL "Total" FORMAT "->>>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 79.5 BY 9.92
         TITLE "Debet fakturor".

DEFINE BROWSE BRW_KRED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KRED DIALOG-1 _STRUCTURED
  QUERY BRW_KRED DISPLAY
      faktkredtemp.FAKTNR COLUMN-LABEL "Faktplan!nr" FORMAT ">>>>>>9":U
      faktkredtemp.FDELNR COLUMN-LABEL "Preliminär!kreditfaktura" FORMAT "9999999":U
      faktkredtemp.VKREDIT COLUMN-LABEL "Kredit!faktura" FORMAT "->>>>>>>>9":U
      faktkredtemp.DATUM COLUMN-LABEL "Faktura!datum" FORMAT "99/99/99":U
      faktkredtemp.VFAKTNR COLUMN-LABEL "Debet!Faktura" FORMAT ">>>>>>>>9":U
      faktkredtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(35)":U
      faktkredtemp.TOTPRIS COLUMN-LABEL "Totaltpris" FORMAT "->>>>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 101.13 BY 9.92
         TITLE "Kredit fakturor".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     RAD_VAD AT ROW 1.08 COL 1.5 NO-LABEL
     FILL-IN_SANDARE AT ROW 2.96 COL 37.25 COLON-ALIGNED
     FILL-IN_SDATUM AT ROW 2.96 COL 64.75 COLON-ALIGNED
     BRW_FAKT AT ROW 3 COL 1.5
     BRW_KRED AT ROW 3 COL 1.5
     BRW_ANV AT ROW 3 COL 50
     EDITOR_MEDD AT ROW 4 COL 33.75 NO-LABEL
     FILL-IN-ADMTEXT AT ROW 5 COL 1.5 NO-LABEL
     BTN_TAB AT ROW 5.75 COL 106.63
     FBTN_PRELB AT ROW 7 COL 106.63
     FILL-IN_ANVANDARE AT ROW 7.63 COL 19.63 COLON-ALIGNED
     BTN_UPPARB AT ROW 13.96 COL 21.75
     BTN_PREV AT ROW 13.96 COL 35
     BTN_NASTA AT ROW 13.96 COL 55
     BTN_SKR AT ROW 13.96 COL 75
     BTN_OK AT ROW 14 COL 106.63
     SPACE(0.24) SKIP(0.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Faktureringsadministration":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: faktkredtemp T "?" NO-UNDO temp-db faktkredtemp
      TABLE: faktureradtemp T "?" NO-UNDO temp-db faktureradtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_FAKT FILL-IN_SDATUM DIALOG-1 */
/* BROWSE-TAB BRW_KRED BRW_FAKT DIALOG-1 */
/* BROWSE-TAB BRW_ANV BRW_KRED DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_ANV:HIDDEN  IN FRAME DIALOG-1                = TRUE.

ASSIGN 
       BRW_FAKT:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_FAKT:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

ASSIGN 
       BRW_KRED:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

ASSIGN 
       FBTN_PRELB:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ADMTEXT IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_SANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SDATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Användarnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FAKT
/* Query rebuild information for BROWSE BRW_FAKT
     _TblList          = "Temp-Tables.faktureradtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktureradtemp.SENASTTID
"faktureradtemp.SENASTTID" "Tid t.o.m." ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktureradtemp.FAKTNR
"faktureradtemp.FAKTNR" "Faktplan!nr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktureradtemp.FDELNR
"faktureradtemp.FDELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktureradtemp.VFAKTNR
"faktureradtemp.VFAKTNR" "Faktura!nummer" ">>>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktureradtemp.DATUM
"faktureradtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktureradtemp.NAMN
"faktureradtemp.NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktureradtemp.TOTPRIS
"faktureradtemp.TOTPRIS" "Total" "->>>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KRED
/* Query rebuild information for BROWSE BRW_KRED
     _TblList          = "Temp-Tables.faktkredtemp"
     _OrdList          = "Temp-Tables.faktkredtemp.FDELNR|yes,Temp-Tables.faktkredtemp.VFAKTNR|yes"
     _FldNameList[1]   > Temp-Tables.faktkredtemp.FAKTNR
"faktkredtemp.FAKTNR" "Faktplan!nr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktkredtemp.FDELNR
"faktkredtemp.FDELNR" "Preliminär!kreditfaktura" "9999999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktkredtemp.VKREDIT
"faktkredtemp.VKREDIT" "Kredit!faktura" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktkredtemp.DATUM
"faktkredtemp.DATUM" "Faktura!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktkredtemp.VFAKTNR
"faktkredtemp.VFAKTNR" "Debet!Faktura" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktkredtemp.NAMN
"faktkredtemp.NAMN" "Benämning" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktkredtemp.TOTPRIS
"faktkredtemp.TOTPRIS" "Totaltpris" "->>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KRED */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Faktureringsadministration */
DO: 
   RUN avslut_UI.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&Scoped-define SELF-NAME BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANV DIALOG-1
ON VALUE-CHANGED OF BRW_ANV IN FRAME DIALOG-1 /* Guruanvändare */
DO:
   status-ok = BRW_ANV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN FILL-IN_ANVANDARE = anvandartemp.ANVANDARE.
   DISPLAY FILL-IN_ANVANDARE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NASTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NASTA DIALOG-1
ON CHOOSE OF BTN_NASTA IN FRAME DIALOG-1 /* Nästa med. */
DO:   
   vart = 2.
   RUN medd_UI IN meddh (INPUT vart,OUTPUT EDITOR_MEDD,OUTPUT FILL-IN_SANDARE,OUTPUT FILL-IN_SDATUM).
   IF FILL-IN_SANDARE = "$FEL" THEN FILL-IN_SANDARE = "".
   DISPLAY EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Avsluta */
DO: 
   RUN avslut_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PREV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PREV DIALOG-1
ON CHOOSE OF BTN_PREV IN FRAME DIALOG-1 /* Föregåend med. */
DO:   
   vart = 1.
   RUN medd_UI IN meddh (INPUT vart,OUTPUT EDITOR_MEDD,OUTPUT FILL-IN_SANDARE,OUTPUT FILL-IN_SDATUM).
   IF FILL-IN_SANDARE = "$FEL" THEN FILL-IN_SANDARE = "".
   DISPLAY EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TAB DIALOG-1
ON CHOOSE OF BTN_TAB IN FRAME DIALOG-1 /* Ta bort faktura */
DO:
      
   {muswait.i} 
   IF RAD_VAD = 3 THEN DO:
      status-ok = BRW_FAKT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      IF NOT AVAILABLE faktureradtemp THEN RETURN NO-APPLY.
      innr = faktureradtemp.FAKTNR.
      RUN bort_UI.
      RUN efterupp_UI (INPUT innr).      
   END.
   IF RAD_VAD = 4 THEN RUN bortkred_UI.
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPARB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPARB DIALOG-1
ON CHOOSE OF BTN_UPPARB IN FRAME DIALOG-1 /* Upparbetad kostnad */
DO:
   {muswait.i}
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKUPPARPA.P PERSISTENT SET fuppapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FAKUPPARPA.P PERSISTENT SET fuppapph.
   END.   
   RUN start2_UI IN fuppapph.
   RUN getfirst_UI IN fuppapph (OUTPUT fnr).
   IF fnr NE 0 THEN DO:
      REPEAT:
         FRAME {&FRAME-NAME}:TITLE = "Nu beräknas " + STRING(fnr).
         RUN kost_UI IN fuppapph (INPUT fnr).
         RUN getnext_UI IN fuppapph (OUTPUT fnr).
         IF fnr = 0 THEN LEAVE.
      END.
   END.
   RUN start4_UI IN fuppapph.
   RUN getfirst_UI IN fuppapph (OUTPUT fnr).
   IF fnr NE 0 THEN DO:
      REPEAT:
         FRAME {&FRAME-NAME}:TITLE = "Nu beräknas upparbetadkostnad för " + STRING(fnr).
         RUN kost_UI IN fuppapph (INPUT fnr).
         RUN getnext_UI IN fuppapph (OUTPUT fnr).
         IF fnr = 0 THEN LEAVE.
      END.
   END.
   FRAME {&FRAME-NAME}:TITLE = "Beräkningarna klar".
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_PRELB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_PRELB DIALOG-1
ON CHOOSE OF FBTN_PRELB IN FRAME DIALOG-1 /* Ta bort prel.fakt. */
DO:
   IF NOT AVAILABLE vfaktplantemp THEN DO:
      RETURN NO-APPLY.
   END.
   RUN faktstop_UI (INPUT TRUE,INPUT vfaktplantemp.FAKTNR).
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      {muswait.i}
      RUN prelbo_UI.
      {musarrow.i} 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ANVANDARE DIALOG-1
ON LEAVE OF FILL-IN_ANVANDARE IN FRAME DIALOG-1 /* Användare */
DO:                                             
   /*CCC*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAD DIALOG-1
ON VALUE-CHANGED OF RAD_VAD IN FRAME DIALOG-1
DO:
   RAD_VAD = INPUT RAD_VAD.
   RUN goma_UI.
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
   {ALLSTARTDYN.I}
   {musarrow.i}    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 5,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 5,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
      RUN hamtaffakturd_UI IN fakthmth (INPUT TABLE vfaktplantemp,OUTPUT TABLE faktureradtemp,OUTPUT TABLE faktkredtemp).     
   END.
   RAD_VAD = 1.
   IF Guru.Konstanter:globanv NE CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
      RAD_VAD:DELETE("Ta bort debet fakturor"). 
      RAD_VAD:DELETE("Ta bort kredit fakturor"). 
   END.
   BTN_UPPARB:LABEL = "Beräkna~nupparbetad~nkostnad".
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 66
   soktemp.SOKCHAR[1] = Guru.Konstanter:globanv.
   {SOKANROP.I}
   FILL-IN_ANVANDARE = soktemp.SOKCHAR[2].
   RUN enable_UI.       
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = FILL-IN_ANVANDARE NO-ERROR.
   IF AVAILABLE anvandartemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(anvandartemp)).              
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN goma_UI.
   IF Guru.Konstanter:appcon THEN DO:
      RUN GAMMED.P PERSISTENT SET meddh ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT FILL-IN_ANVANDARE).
   END.
   ELSE DO:
      RUN GAMMED.P PERSISTENT SET meddh
      (INPUT FILL-IN_ANVANDARE).
   END.
   {FRMSIZED.I}      
   {DIA_M_SLUT.I}
   /*
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_UPPARB:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_UPPARB:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   */   
   RUN lastselectdyn_UI IN brwproc[1].                        
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
   ASSIGN
   anvandartemp.ANVANDARE:READ-ONLY IN BROWSE BRW_ANV = TRUE.    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_FAKT:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_KRED:HANDLE IN FRAME {&FRAME-NAME}).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avslut_UI DIALOG-1 
PROCEDURE avslut_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:         
-------------------------------------------------------------*/ 
   FILL-IN_ANVANDARE = INPUT FRAME {&FRAME-NAME} FILL-IN_ANVANDARE.
   FIND FIRST anvandartemp WHERE anvandartemp.AV-LEVEL NE 0 AND anvandartemp.ANVANDARE = FILL-IN_ANVANDARE
   NO-ERROR. 
   IF NOT AVAILABLE anvandartemp THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = FILL-IN_ANVANDARE
         NO-ERROR. 
      END.
      
   END.
   IF NOT AVAILABLE anvandartemp THEN DO:
      RAD_VAD = 1.
      DISPLAY RAD_VAD WITH FRAME {&FRAME-NAME}.
      MESSAGE "Användare finns ej." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ANVANDARE.
      musz = TRUE.
      RETURN.
   END.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 82
   soktemp.SOKCHAR[1] = FILL-IN_ANVANDARE.
   {SOKANROP.I}
   {BORTBRWPROC.I}
   IF VALID-HANDLE(fuppapph) THEN DELETE PROCEDURE fuppapph.   
   IF VALID-HANDLE(meddh) THEN DELETE PROCEDURE meddh.   
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bortkred_UI DIALOG-1 
PROCEDURE bortkred_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   status-ok = BRW_KRED:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   MESSAGE "Vill du verkligen ta bort denna kreditfaktura "
   faktkredtemp.FAKTNR INTEGER(STRING(faktkredtemp.VKREDIT,"999999")) " " faktkredtemp.NAMN " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av faktura"
   UPDATE answer AS LOGICAL.
   {muswait.i}
   IF answer THEN DO:
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
         RUN selnextprevrow_UI IN brwproc[3].
         RUN faktkredbort_UI IN fakthmth (INPUT faktkredtemp.FAKTNR,INPUT faktkredtemp.FDELNR).   
         IF AVAILABLE faktkredtemp THEN DELETE faktkredtemp.
         OPEN QUERY BRW_KRED FOR EACH faktkredtemp WHERE faktkredtemp.VKREDIT NE 0 NO-LOCK BY faktkredtemp.FAKTNR BY faktkredtemp.VKREDIT.
         RUN lastselectdyn_UI IN brwproc[3].              
      END.
   END. 
   {musarrow.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   status-ok = BRW_FAKT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = faktureradtemp.FAKTNR NO-LOCK NO-ERROR.
   IF vfaktplantemp.FDELNR NE 0 THEN DO:
      MESSAGE "Denna faktura går ej att ta bort då det finns en preliminär faktura uttagen."
      VIEW-AS ALERT-BOX TITLE "Bortag av faktura".
      RETURN.
   END.
   FIND FIRST fbuff WHERE fbuff.FAKTNR = faktureradtemp.FAKTNR AND 
   fbuff.VFAKTNR > faktureradtemp.VFAKTNR AND fbuff.DATUM > faktureradtemp.DATUM
   NO-LOCK NO-ERROR.
   IF AVAILABLE fbuff THEN DO:
      MESSAGE "Det går bara att ta bort den senaste faktureringen."
      VIEW-AS ALERT-BOX TITLE "Bortag av faktura".
      RETURN.
   END.
   FIND FIRST fbuff WHERE fbuff.FAKTNR = faktureradtemp.FAKTNR AND fbuff.VFAKTNR = 0 NO-LOCK NO-ERROR.
   IF AVAILABLE fbuff THEN DO:
      MESSAGE "Du har redan tagit bort en faktura på denna plan."
      VIEW-AS ALERT-BOX TITLE "Bortag av faktura".
      RETURN.
   END.
   MESSAGE "Vill du verkligen ta bort denna faktura "
   faktureradtemp.FAKTNR INTEGER(STRING(faktureradtemp.VFAKTNR,"999999")) " " faktureradtemp.NAMN " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av faktura"
   UPDATE answer AS LOGICAL.
   {muswait.i}
   IF answer THEN DO:
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
         RUN selnextprevrow_UI IN brwproc[2].
         RUN faktbort_UI IN fakthmth (INPUT faktureradtemp.FAKTNR,INPUT faktureradtemp.VFAKTNR).                             
         IF AVAILABLE faktureradtemp THEN faktureradtemp.VFAKTNR = 0.
         OPEN QUERY BRW_FAKT FOR EACH faktureradtemp WHERE faktureradtemp.VFAKTNR NE 0 NO-LOCK BY faktureradtemp.FAKTNR BY faktureradtemp.VFAKTNR.
         RUN lastselectdyn_UI IN brwproc[2].              
      END.
   END. 
   {musarrow.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE efterupp_UI DIALOG-1 
PROCEDURE efterupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER varfnr AS INTEGER NO-UNDO.
   
   EMPTY TEMP-TABLE extravfaktplantemp NO-ERROR.
   RUN hamtafplan_UI IN fakthmth (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv,INPUT varfnr,OUTPUT TABLE extravfaktplantemp).     
   FIND FIRST extravfaktplantemp NO-ERROR.   
   IF NOT AVAILABLE extravfaktplantemp THEN DO:
      MESSAGE " finns inte!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FOR EACH extravfaktplantemp:
      FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = extravfaktplantemp.FAKTNR     
      NO-LOCK NO-ERROR.             
      IF NOT AVAILABLE vfaktplantemp THEN CREATE vfaktplantemp.
      BUFFER-COPY extravfaktplantemp TO vfaktplantemp.         
      DELETE extravfaktplantemp.         
   END.
   
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
  DISPLAY RAD_VAD FILL-IN_SANDARE FILL-IN_SDATUM EDITOR_MEDD FILL-IN-ADMTEXT 
          FILL-IN_ANVANDARE 
      WITH FRAME DIALOG-1.
  ENABLE RAD_VAD BRW_FAKT BRW_KRED BRW_ANV EDITOR_MEDD BTN_TAB FBTN_PRELB 
         FILL-IN_ANVANDARE BTN_UPPARB BTN_PREV BTN_NASTA BTN_SKR BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktstop_UI DIALOG-1 
PROCEDURE faktstop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER stopvar AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER inrvar AS INTEGER NO-UNDO.
   IF stopvar = TRUE THEN DO:
      musz = FALSE.
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 4,INPUT "FAKTURA PLAN " + STRING(inrvar),INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 4,INPUT "FAKTURA PLAN " + STRING(inrvar),INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.  
         musz = TRUE.
         RETURN.
      END.
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT "FAKTURA PLAN " + STRING(inrvar),INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT "FAKTURA PLAN " + STRING(inrvar),INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.   
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-1 
PROCEDURE goma_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:         
-------------------------------------------------------------*/ 
   ASSIGN
   FBTN_PRELB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_TAB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_KRED:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_ANV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NASTA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_PREV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   EDITOR_MEDD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_ANVANDARE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_SANDARE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_SDATUM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-ADMTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF RAD_VAD = 1 THEN DO:
      ASSIGN
      BTN_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_ANV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_ANVANDARE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-ADMTEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GRAN" OR 
      Guru.Konstanter:globforetag = "GKAL" THEN status-ok = status-ok.
      ELSE BTN_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF RAD_VAD = 2 THEN DO:
      ASSIGN
      BTN_PREV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NASTA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      /*BTN_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE */
      EDITOR_MEDD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      FILL-IN_SANDARE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_SDATUM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      IF vart = 0 THEN DO:
         vart = 3.
         RUN medd_UI IN meddh (INPUT vart,OUTPUT EDITOR_MEDD,OUTPUT FILL-IN_SANDARE,OUTPUT FILL-IN_SDATUM).
         IF FILL-IN_SANDARE = "$FEL" THEN FILL-IN_SANDARE = "".
      END.
      DISPLAY EDITOR_MEDD FILL-IN_SANDARE FILL-IN_SDATUM WITH FRAME {&FRAME-NAME}.      
   END.
   IF RAD_VAD = 3 THEN DO:
      OPEN QUERY BRW_FAKT FOR EACH faktureradtemp WHERE faktureradtemp.VFAKTNR NE 0 NO-LOCK BY faktureradtemp.FAKTNR BY faktureradtemp.VFAKTNR.
      ASSIGN
      FBTN_PRELB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_TAB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.           
   END.
   IF RAD_VAD = 4 THEN DO:
      OPEN QUERY BRW_KRED FOR EACH faktkredtemp WHERE faktkredtemp.VKREDIT NE 0 NO-LOCK BY faktkredtemp.FAKTNR BY faktkredtemp.VKREDIT.
      ASSIGN
      BRW_KRED:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_TAB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.           
   END.

   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prelbo_UI DIALOG-1 
PROCEDURE prelbo_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   /*BÖR APPAS*/
   /*FAKTKOLL ?????*/
   IF NOT AVAILABLE vfaktplantemp THEN DO:
      RETURN.
   END.
   IF vfaktplantemp.FDELNR = 0 THEN DO:
      MESSAGE "Preliminärfaktura saknas."      
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   MESSAGE "Vill du verkligen ta bort denna preliminärfaktura "
   vfaktplantemp.FAKTNR vfaktplantemp.FDELNR " " vfaktplantemp.NAMN " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av preliminärfaktura"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:  
      {muswait.i}
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 68
      soktemp.SOKINT[1] = vfaktplantemp.FAKTNR.
      {SOKANROP.I}     
      EMPTY TEMP-TABLE extravfaktplantemp NO-ERROR.
      RUN efterupp_UI (INPUT soktemp.SOKINT[1]).      
      FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = vfaktplantemp.FAKTNR AND faktureradtemp.VFAKTNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE faktureradtemp THEN DO:
         DELETE faktureradtemp.
      END.
      {musarrow.i}
      
   END.
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {PRINTSTAENDE.I}              
   DISPLAY TODAY AT  6
   "MEDELANDE FRÅN :" AT 6
   FILL-IN_SANDARE AT 23 NO-LABEL  
           "TILL           :" AT 6 
   Guru.Konstanter:globanv AT 23  NO-LABEL        
           "SÄNT DEN       :" AT 6
   FILL-IN_SDATUM AT 23  NO-LABEL
   EDITOR_MEDD AT 6 VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

