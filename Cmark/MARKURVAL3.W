&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER aodelnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{IMPFAST.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
{MARKVAL.I} 
{FASTEXTRA.I}

DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vardrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE skogrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skogrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE volrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE volrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE akerrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE akerrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE kabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE kabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastvrec AS RECID NO-UNDO. 

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.




DEFINE NEW SHARED VARIABLE blobproch AS HANDLE NO-UNDO.    


DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE ordnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE frannr AS INTEGER NO-UNDO. 
DEFINE VARIABLE till_recid AS RECID NO-UNDO. 
DEFINE VARIABLE ponr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ktakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE mall AS INTEGER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE markurvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE marknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE fragvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE returnr AS INTEGER NO-UNDO.
DEFINE VARIABLE bant AS INTEGER NO-UNDO.
DEFINE VARIABLE hant AS INTEGER NO-UNDO.

DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.

DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.


DEFINE VARIABLE flerafiler AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempi AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

DEFINE VARIABLE fildir2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE spmall AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivutmall AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokhitta AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokenamn AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE compdir NO-UNDO
      FIELD BIB AS CHARACTER.
{MAGA.I}
DEFINE BUFFER magabuff FOR maga2.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MARK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES maga2

/* Definitions for BROWSE BRW_MARK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MARK maga2.MARKNR maga2.PERSONNUMMER ~
maga2.MARKAGARE maga2.KONTAKT maga2.ANDEL maga2.PROCENT maga2.BETECKNING ~
maga2.GATUADRESS maga2.POSTNUMMER maga2.POSTADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MARK maga2.KONTAKT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MARK maga2
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MARK maga2
&Scoped-define QUERY-STRING-BRW_MARK FOR EACH maga2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MARK OPEN QUERY BRW_MARK FOR EACH maga2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MARK maga2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MARK maga2


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_SKRIVMALL TOG_ALLAMA CMB_VARDVAL ~
FBTN_SPMALL BRW_MARK BTN_ORDUPP BTN_ANDM BTN_ORDNER BTN_NYBLOB BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VARDNR FILL-IN_BENAMNING ~
FILL-IN_AONR FILL-IN_DELNR TOG_ALLAMA CMB_VARDVAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDM 
     LABEL "Ändra Markägare":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NYBLOB 
     LABEL "Nytt infobrev":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4.63 BY 1.83.

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4.63 BY 1.83.

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_FASTFOR 
     LABEL "mall":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_FEXTRA 
     LABEL "Extra uppgifter":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OVERSIKT 
     LABEL "Översikt":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIVMALL 
     LABEL "Skriv ut mall/mallar":L 
     SIZE 14 BY 1 TOOLTIP "Visa word/excel-mall".

DEFINE BUTTON FBTN_SPMALL 
     LABEL "Spara mall/mallar":L 
     SIZE 14 BY 1 TOOLTIP "Visa word/excel-mall".

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_VARDVAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj word/excel-mall" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 25.63 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE TOG_ALLAMA AS LOGICAL INITIAL no 
     LABEL "Alla markägare":L 
     VIEW-AS TOGGLE-BOX
     SIZE 22.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MARK FOR 
      maga2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MARK WINDOW-1 _STRUCTURED
  QUERY BRW_MARK NO-LOCK DISPLAY
      maga2.MARKNR COLUMN-LABEL "Marknr" FORMAT ">>>>9":U
      maga2.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      maga2.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "X(256)":U
            WIDTH 40
      maga2.KONTAKT COLUMN-LABEL "Kont" FORMAT "ja/nej":U
      maga2.ANDEL COLUMN-LABEL "Andel" FORMAT "X(6)":U
      maga2.PROCENT COLUMN-LABEL "%" FORMAT ">>9":U
      maga2.BETECKNING COLUMN-LABEL "Beteckning" FORMAT "X(256)":U
            WIDTH 25
      maga2.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(256)":U
            WIDTH 25
      maga2.POSTNUMMER COLUMN-LABEL "Pnr" FORMAT "XXX XX":U WIDTH 7
      maga2.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(256)":U
            WIDTH 20
  ENABLE
      maga2.KONTAKT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 104.5 BY 15.5
         TITLE "Markägare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN_VARDNR AT ROW 1.63 COL 16 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 1.63 COL 28.13 COLON-ALIGNED NO-LABEL
     FILL-IN_AONR AT ROW 3 COL 16 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 3.04 COL 31 COLON-ALIGNED
     FBTN_SKRIVMALL AT ROW 3.83 COL 111.25 WIDGET-ID 6
     TOG_ALLAMA AT ROW 4.5 COL 1.38
     CMB_VARDVAL AT ROW 4.75 COL 78.13 COLON-ALIGNED
     FBTN_SPMALL AT ROW 4.92 COL 111.25 WIDGET-ID 4
     BRW_MARK AT ROW 6 COL 1.38
     FBTN_FASTFOR AT ROW 6 COL 111.25
     FBTN_VISA AT ROW 7.08 COL 111.25
     FBTN_SKRIV AT ROW 8.21 COL 111.25
     FBTN_EX AT ROW 9.29 COL 111.25
     BTN_ORDUPP AT ROW 9.75 COL 106.38
     FBTN_FEXTRA AT ROW 10.38 COL 111.25
     BTN_ANDM AT ROW 11.46 COL 111.25
     FBTN_OVERSIKT AT ROW 12.54 COL 111.25 WIDGET-ID 8
     BTN_ORDNER AT ROW 12.67 COL 106.38
     BTN_NYBLOB AT ROW 15.13 COL 111.25 WIDGET-ID 2
     BTN_AVB AT ROW 23.67 COL 111.75
     "Grönmarkerade fastigheter har ifylld Ersatt/Intrångsers i Extra uppgifter" VIEW-AS TEXT
          SIZE 60.75 BY .63 AT ROW 23.25 COL 2.25 WIDGET-ID 14
          BGCOLOR 10 
     "Gulmarkerade fastigheter har en anmärkning i Extra uppgifter" VIEW-AS TEXT
          SIZE 42.63 BY 1 AT ROW 22 COL 1.88 WIDGET-ID 10
          BGCOLOR 14 
     "Högerklicka på markägare för att söka telefonnummer via Internet" VIEW-AS TEXT
          SIZE 64 BY 1 AT ROW 22 COL 46 WIDGET-ID 12
          BGCOLOR 11 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 23.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: maga2 T "?" NO-UNDO temp-db maga2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Markvärdering"
         HEIGHT             = 23.79
         WIDTH              = 125
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
/* BROWSE-TAB BRW_MARK FBTN_SPMALL FRAME-A */
ASSIGN 
       BRW_MARK:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_EX IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_EX:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_FASTFOR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_FASTFOR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_FEXTRA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_FEXTRA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_OVERSIKT IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_OVERSIKT:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_SKRIV IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_SKRIV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_SKRIVMALL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_SPMALL:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BENAMNING IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MARK
/* Query rebuild information for BROWSE BRW_MARK
     _TblList          = "Temp-Tables.maga2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.maga2.MARKNR
"maga2.MARKNR" "Marknr" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.maga2.PERSONNUMMER
"maga2.PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.maga2.MARKAGARE
"maga2.MARKAGARE" "Markägare" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.maga2.KONTAKT
"maga2.KONTAKT" "Kont" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.maga2.ANDEL
"maga2.ANDEL" "Andel" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.maga2.PROCENT
"maga2.PROCENT" "%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.maga2.BETECKNING
"maga2.BETECKNING" "Beteckning" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.maga2.GATUADRESS
"maga2.GATUADRESS" "Gatuadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.maga2.POSTNUMMER
"maga2.POSTNUMMER" "Pnr" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.maga2.POSTADRESS
"maga2.POSTADRESS" "Postadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MARK
&Scoped-define SELF-NAME BRW_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MARK WINDOW-1
ON ROW-LEAVE OF BRW_MARK IN FRAME FRAME-A /* Markägare */
DO:
  
   IF AVAILABLE maga2 THEN DO:
      IF maga2.KONTAKT = INPUT BROWSE BRW_MARK maga2.KONTAKT THEN.
      ELSE DO:
         DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
         maga2.KONTAKT = INPUT BROWSE BRW_MARK maga2.KONTAKT.
         DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
      END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MARK WINDOW-1
ON VALUE-CHANGED OF BRW_MARK IN FRAME FRAME-A /* Markägare */
DO:   
   status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maga2.KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maga2.KONTAKT BRW_MARK _BROWSE-COLUMN WINDOW-1
ON ENTRY OF maga2.KONTAKT IN BROWSE BRW_MARK /* Kont */
DO:
   status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
   DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maga2.KONTAKT BRW_MARK _BROWSE-COLUMN WINDOW-1
ON LEAVE OF maga2.KONTAKT IN BROWSE BRW_MARK /* Kont */
DO: 
   
   IF AVAILABLE maga2 THEN DO:      
      ASSIGN ktakt = INPUT BROWSE BRW_MARK maga2.KONTAKT.      
      IF ktakt = TRUE THEN DO:      
         FIND FIRST magabuff WHERE magabuff.BETECKNING = maga2.BETECKNING AND 
         magabuff.MARKNR NE maga2.MARKNR AND magabuff.KONTAKT = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE magabuff THEN DO:
            MESSAGE "Det kan bara finnas 1 kontaktperson per fastighet. " VIEW-AS ALERT-BOX.
            DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
            RETURN NO-APPLY.

         END.
      END.
      maga2.KONTAKT = ktakt.
      DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK NO-ERROR.       
      IF maga2.KONTAKT = TRUE THEN DO:      
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "FASTLOPNR"                   
         inextradatatemp.HUVUDCH = maga2.BETECKNING
         inextradatatemp.SOKINT[1] = maga2.MARKNR.
         RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
      END.
      ELSE DO:
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "FASTLOPNR"                   
         inextradatatemp.HUVUDCH = maga2.BETECKNING
         inextradatatemp.SOKINT[1] = 0.
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
         END.
      END.      
   END. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maga2.KONTAKT BRW_MARK _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF maga2.KONTAKT IN BROWSE BRW_MARK /* Kont */
DO:
   
   DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
   IF INPUT BROWSE BRW_MARK maga2.KONTAKT = TRUE THEN ktakt = FALSE.
   IF INPUT BROWSE BRW_MARK maga2.KONTAKT = FALSE THEN ktakt = TRUE.
   IF ktakt = TRUE THEN DO:      
      FIND FIRST magabuff WHERE magabuff.BETECKNING = maga2.BETECKNING AND 
      magabuff.MARKNR NE maga2.MARKNR AND magabuff.KONTAKT = TRUE NO-LOCK NO-ERROR.
      IF AVAILABLE magabuff THEN DO:
         MESSAGE "Det kan bara finnas 1 kontaktperson per fastighet. " VIEW-AS ALERT-BOX.
         DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
         RETURN NO-APPLY.
      END.
   END.
   maga2.KONTAKT = ktakt.
   DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK NO-ERROR.       
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDM WINDOW-1
ON CHOOSE OF BTN_ANDM IN FRAME FRAME-A /* Ändra Markägare */
DO:
   
   status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME  {&FRAME-NAME} NO-ERROR.
   till_recid = RECID(maga2).
   marknrvar =  maga2.MARKNR.
   {muswait.i}
   vart = "AND".
   tthandle = TEMP-TABLE markagaretemp:HANDLE. 
   fragvar = " WHERE MARKNR = " + STRING(marknrvar).
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markagaretemp THEN DO:
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MARKAGARE", INPUT fragvar).
   END.
   RUN ANDMARKV.W (INPUT-OUTPUT marknrvar,OUTPUT returnr,OUTPUT fragvar).      
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar NO-LOCK NO-ERROR.
   IF AVAILABLE markagaretemp THEN DO:
      FIND FIRST maga2 WHERE maga2.MARKNR = marknrvar NO-ERROR.
      IF AVAILABLE maga2 THEN DO:         
         ASSIGN 
         maga2.MARKAGARE = markagaretemp.MARKAGARE  
         maga2.PERSONNUMMER = markagaretemp.PERSONNUMMER          
         maga2.GATUADRESS = markagaretemp.GATUADRESS
         maga2.POSTNUMMER = markagaretemp.POSTNUMMER
         maga2.POSTADRESS = markagaretemp.POSTADRESS.         
         IF maga2.PNR2 BEGINS "0000" THEN .
         ELSE DO:            
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "MARKAG"                   
            inextradatatemp.HUVUDINT = maga2.MARKNR.                    
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF AVAILABLE extradatatemp THEN DO:     
               IF INDEX(maga2.MARKAGARE,extradatatemp.SOKCHAR[3]) = 0 THEN DO:                 
                  maga2.MARKAGARE = maga2.MARKAGARE + " " + extradatatemp.SOKCHAR[3].
               END.   
            END. 
         END.
      END.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   FIND maga2 WHERE RECID(maga2) = till_recid NO-LOCK NO-ERROR.
   IF AVAILABLE maga2 THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(maga2)).           
      RUN lastselectdynnv_UI IN brwproc[1].                                
   END.
   
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   musz = TRUE.
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYBLOB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYBLOB WINDOW-1
ON CHOOSE OF BTN_NYBLOB IN FRAME FRAME-A /* Nytt infobrev */
DO:
   RUN FILEMULTISEL.P ( INPUT "All Files (*.*)|*.*",
                        INPUT ?,
                        INPUT "Välj de filer som du vill lägga in i databasen.",
                        OUTPUT flerafiler,
                        OUTPUT logresult).
   {muswait.i}
      
   IF logresult = TRUE THEN DO:                 
      REPEAT tempi = 1 TO NUM-ENTRIES(flerafiler):
         RUN skapablobbar_UI (INPUT ENTRY(tempi,flerafiler)).
      END.
   END.
   {musarrow.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDNER WINDOW-1
ON CHOOSE OF BTN_ORDNER IN FRAME FRAME-A /* Ner */
DO:   
   RUN btnordner_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDUPP WINDOW-1
ON CHOOSE OF BTN_ORDUPP IN FRAME FRAME-A
DO:
   RUN btnordupp_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX WINDOW-1
ON CHOOSE OF FBTN_EX IN FRAME FRAME-A /* Visa I EXCEL */
DO:   
   visvalvar = 2.
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE. 
   RUN VISAMARKV.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FASTFOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FASTFOR WINDOW-1
ON CHOOSE OF FBTN_FASTFOR IN FRAME FRAME-A /* mall */
DO:    
   
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   RUN mallar_UI.
   IF mall = 1 OR mall = 2  OR mall = 5 OR mall = 6 OR mall = 70 OR mall = 71 OR mall = 72 OR mall = 8 OR mall = 9 OR mall = 11 THEN DO:         
      RUN valda_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.   
      musz = FALSE.       
      IF Guru.Konstanter:varforetypchar[7] = "1" THEN DO:       
         RUN PROJNATAG.W (OUTPUT ponr).      
      END.      
      ELSE DO:   
         RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "agar",INPUT mall,OUTPUT ponr).
      END.      
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO: 
         RUN mall12_UI.
      END.
   END.   
   
   IF   mall = 53 OR mall = 54 OR mall = 55 OR mall = 56 OR mall = 57 OR mall = 58 OR mall = 60 OR mall = 61 OR mall = 62  THEN DO:         
      RUN valda_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.   
      /* alla får mellansida  annars får de inte med ponr Lena 20140516*/      
      musz = FALSE. 
      RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "info",INPUT mall,OUTPUT ponr).                     
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO: 
          skrivut = FALSE.                
         RUN GFYTTRANDE.P (INPUT ponr,INPUT STRING(mall), INPUT spmall, INPUT fildir2, INPUT skrivutmall).       
      END.
   END.   
   
   
   IF mall = 3  THEN DO:            
      RUN valda_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.   
      musz = FALSE. 
      RUN mall3_UI.            
   END.
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF mall = 4  THEN DO:            
      DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
      EMPTY TEMP-TABLE marktmp NO-ERROR.     
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      mappvar = fildir.
      SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj den excelfil som Ni vill läsa in"
      FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
      INITIAL-DIR    mappvar
      UPDATE OKvald.      
      IF OKvald = TRUE THEN DO:                      
         RUN mall4_UI.
      END.            
   END.   
   IF mall = 7  THEN DO:            
      DEFINE VARIABLE OKvald2 AS LOGICAL NO-UNDO.
      EMPTY TEMP-TABLE marktmp NO-ERROR. 
      
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      mappvar = fildir.
      SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj det wordbrev som Ni vill skicka etikett till"
      FILTERS        "All Files (*.doc;*.docx)"  "*.doc;*.docx"
      INITIAL-DIR    mappvar
      UPDATE OKvald2.      
      IF OKvald2 = TRUE THEN DO:                      
         RUN mall7_UI.
      END.            
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FEXTRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FEXTRA WINDOW-1
ON CHOOSE OF FBTN_FEXTRA IN FRAME FRAME-A /* Extra uppgifter */
DO:      
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.     
   {AVBGOM.I}
      
   IF Guru.Konstanter:globforetag = "UMEA" OR  Guru.Konstanter:globforetag = "UMBR" THEN RUN FASTEXTRAAG.W .         
   ELSE RUN FASTEXTRA.W .
    
   RUN fastextrahmt_UI.
   RUN openbdynspec_UI IN brwproc[1].
   {AVBFRAM.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OVERSIKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OVERSIKT WINDOW-1
ON CHOOSE OF FBTN_OVERSIKT IN FRAME FRAME-A /* Översikt */
DO:      
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   {AVBGOM.I}
   RUN OVERSIKTVIS.W /*(INPUT 1, INPUT TABLE eperstemp)*/.
   {AVBFRAM.I}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   visvalvar = 1.
   RUN valda_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.             
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).  
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
      RETURN NO-APPLY.
   END.                 
   RUN VISAMARKV.W.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIVMALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIVMALL WINDOW-1
ON CHOOSE OF FBTN_SKRIVMALL IN FRAME FRAME-A /* Skriv ut mall/mallar */
DO:
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   RUN mallar_UI.
   
   IF    mall = 53 OR mall = 54 OR mall = 55 OR mall = 56 OR mall = 57 OR mall = 58 OR mall = 60 OR mall = 61 OR mall = 62  THEN.   
   ELSE DO:
      MESSAGE "Vald mall går ej att skriva ut" VIEW-AS ALERT-BOX.    
      RETURN NO-APPLY.
   END.     
   skrivutmall = TRUE.
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN DO:
      musz = FALSE. 
      skrivutmall = FALSE.
      RETURN NO-APPLY.
   END.
  
   APPLY "CHOOSE" TO FBTN_FASTFOR.
   
   ASSIGN fildir = ""      
   skrivutmall = FALSE.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SPMALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SPMALL WINDOW-1
ON CHOOSE OF FBTN_SPMALL IN FRAME FRAME-A /* Spara mall/mallar */
DO:
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   RUN mallar_UI.
   
   IF mall = 1 OR mall = 2 OR mall = 5 OR  mall = 53 OR mall = 54 OR mall = 55 OR mall = 56 OR mall = 57 OR mall = 58 OR mall = 60 OR mall = 61 OR mall = 62 OR mall = 8 THEN.   
   ELSE DO:
      MESSAGE "Vald mall går ej att spara" VIEW-AS ALERT-BOX.    
      RETURN NO-APPLY.
   END. 
   spmall = TRUE. 
   SYSTEM-DIALOG GET-DIR fildir2
         TITLE          "Välj mapp som dokumenten skall sparas i"                  
         INITIAL-DIR    SESSION:TEMP-DIRECTORY.                
   
   
   APPLY "CHOOSE" TO FBTN_FASTFOR.
   
   ASSIGN fildir2 = ""      
   spmall = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:   
   visvalvar = 1.
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE.
   {AVBGOM.I} 
   RUN VISAMARKV.W.
   {AVBFRAM.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLAMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLAMA WINDOW-1
ON VALUE-CHANGED OF TOG_ALLAMA IN FRAME FRAME-A /* Alla markägare */
DO:
   TOG_ALLAMA = INPUT TOG_ALLAMA.
   maga2.KONTAKT:READ-ONLY IN BROWSE BRW_MARK = TRUE.
   IF TOG_ALLAMA THEN DO:
      APPLY "CTRL-A" TO BRW_MARK.   
   END.
   ELSE DO:
       status-ok = BRW_MARK:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.        
   END.
   maga2.KONTAKT:READ-ONLY IN BROWSE BRW_MARK = FALSE.
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
   IF VALID-HANDLE(framesizeh) THEN DELETE PROCEDURE framesizeh.
   IF VALID-HANDLE(markurvalapph) THEN DELETE PROCEDURE markurvalapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.   
   
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
   {muswait.i}
   {ALLSTARTDYN.I}
   &Scoped-define FORMATNAMN FILL-IN_AONR
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}    
   EMPTY TEMP-TABLE maga2 NO-ERROR.    
   ASSIGN fildir2 = ""
   spmall = FALSE
   skrivutmall = FALSE.
   RUN ladda2 IN markurvalapph (INPUT valvardnr,INPUT aodelnr,
                               OUTPUT TABLE varderingtemp,
                               OUTPUT TABLE maga2,
                               OUTPUT TABLE aovardtemp).                               
   DEBUGGER:SET-BREAK().
   RUN fastextrahmt_UI.
      
   FOR EACH maga2:
      IF maga2.PROCENT = 100 THEN maga2.KONTAKT = TRUE.
   END.
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN vardrec = RECID(varderingtemp).
   FIND varderingtemp WHERE RECID(varderingtemp) = vardrec NO-LOCK NO-ERROR.
   ASSIGN FILL-IN_VARDNR = varderingtemp.VARDNR
   FILL-IN_BENAMNING = varderingtemp.BENAMNING.   
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE aovardtemp THEN DO:
      ASSIGN FILL-IN_AONR = aovardtemp.AONR
      FILL-IN_DELNR = aovardtemp.DELNR.
      IF aodelnr = 1 THEN FILL-IN_DELNR = 0.
   END.
   ELSE DO:
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.         
   ASSIGN TOG_ALLAMA = FALSE.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall").         
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").           
         
   END.
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "ETSA"
   OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS"  OR Guru.Konstanter:globforetag = "SKEL"  OR Guru.Konstanter:globforetag = "wsp"  THEN DO:         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall").
      IF Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK" THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall äldre anpassad").
      END.   
      IF Guru.Konstanter:globforetag = "TECT" THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Tectel").                          
      END.
      IF Guru.Konstanter:globforetag = "TECT" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Mall Utbetalning Tectel").                 
      END.         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning EON").   
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
      
      IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Etikett från excel").           
      END.
      IF Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "TECT"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "skel" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         
         IF Guru.Konstanter:globforetag = "REJI" THEN DO:
            /*status-ok = CMB_VARDVAL:ADD-LAST("Informationsbrev Vattenfall").*/
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev 1 förstärkning").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev 2 förstärkning").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev 3 förstärkning").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev Vilhelmina").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev Byviken").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev stakningstillstånd").            
         END.
         IF Guru.Konstanter:globforetag = "CTECT" OR Guru.Konstanter:globforetag = "SWEO"  OR Guru.Konstanter:globforetag = "SKEL"  THEN DO:
            status-ok = CMB_VARDVAL:ADD-LAST("Mall Utbetalning 3%").
            status-ok = CMB_VARDVAL:ADD-LAST("Mall Utbetalning 10%").
         END.   
         IF Guru.Konstanter:globforetag = "CPICA" OR Guru.Konstanter:globforetag = "CSWEO" THEN DO:
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev 1").
            status-ok = CMB_VARDVAL:ADD-LAST("Infobrev 2").            
         END.
                                
      END.
      IF Guru.Konstanter:globforetag = "cTECT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Etikett till brevmall word").           
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "NYLB" OR Guru.Konstanter:globforetag = "GETB" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall").         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning EON ").
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").                 
   END.
   ELSE IF Guru.Konstanter:globforetag = "snat" THEN DO:
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
   END.
   ELSE IF Guru.Konstanter:globforetag = "lule" THEN DO:
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Luleå").
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
   END.
   
   ELSE IF Guru.Konstanter:globforetag = "POWE" THEN DO:
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Ellevio").       
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
   END.
   ELSE IF Guru.Konstanter:globforetag = "PFBK" OR Guru.Konstanter:globforetag = "KEWA" OR Guru.Konstanter:globforetag = "GULL" OR Guru.Konstanter:globforetag = "SVKK" THEN DO:      
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Ellevio").
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall").
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" OR  Guru.Konstanter:globforetag = "UMBR" THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
   END.   
   ELSE IF Guru.Konstanter:globforetag = "VALL" THEN DO:
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Allmän").
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning Vattenfall").
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").
   END.   
   ELSE DO:        
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").           
   END.
   
   RUN enable_UI.   
   IF Guru.Konstanter:globforetag = "UMEA" OR  Guru.Konstanter:globforetag = "UMBR" THEN DO: 
      FBTN_SPMALL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FBTN_SKRIVMALL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   FBTN_FASTFOR:LABEL = "Visa word/excel-mall".     
   
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "GRAN" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Vattenfall".         
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "SKOK" 
   OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "SKEL" OR Guru.Konstanter:globforetag = "WSP"   THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Vattenfall".         
   ELSE IF Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "NYLB" OR Guru.Konstanter:globforetag = "getb"  THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Vattenfall".
   ELSE IF Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "VALL" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Allmän".
   ELSE IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Allmän".
   ELSE IF Guru.Konstanter:globforetag = "lule" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Luleå".
   ELSE IF Guru.Konstanter:globforetag = "KEWA" OR Guru.Konstanter:globforetag = "GULL" OR Guru.Konstanter:globforetag = "SVKK"  THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning Ellevio".
   
            
   ELSE CMB_VARDVAL:SCREEN-VALUE = "Etikett".            
   FBTN_FASTFOR:HIDDEN = FALSE.
   ENABLE FBTN_FASTFOR WITH FRAME {&FRAME-NAME}.
   
   IF Guru.Konstanter:varforetypval[22] = 1  THEN DO:
      maga2.PROCENT:VISIBLE IN BROWSE BRW_MARK = FALSE.
   END.
   ELSE DO:
      maga2.ANDEL:VISIBLE IN BROWSE BRW_MARK = FALSE.
   END.
   {FRMSIZE.I}  
   /*ENABLE BRW_MARK  WITH FRAME {&FRAME-NAME}. */
   ASSIGN
   BRW_MARK:HIDDEN = FALSE.
   TOG_ALLAMA = FALSE.  
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aovardtemp THEN DO:    
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.         
   RUN setcolindex_UI IN brwproc[1] (INPUT "maga2.ORDNING").
   RUN openbdynspec_UI IN brwproc[1].
   RUN getfirst_UI IN brwproc[1].
   RELEASE maga2 NO-ERROR.

   ENABLE FBTN_SKRIV FBTN_VISA FBTN_EX WITH FRAME {&FRAME-NAME}.       
   
   ENABLE FBTN_FEXTRA WITH FRAME {&FRAME-NAME}.       
   
   BTN_NYBLOB:HIDDEN = TRUE.

   ENABLE FBTN_OVERSIKT WITH FRAME {&FRAME-NAME}.  

   musz = FALSE.
   {musarrow.i}
   {WIN_M_SLUT.I}
   
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
   /*ASSIGN
   maga2.MARKNR:READ-ONLY IN BROWSE BRW_MARK = TRUE.*/
   DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MARK:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_MARK:HANDLE,INPUT "Sök telefonnummer via Internet",INPUT "infotel_UI").
   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispkom_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MARKURVALAPP.P PERSISTENT SET markurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MARKURVALAPP.P PERSISTENT SET markurvalapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT valford).
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT valford).
   END.
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   IF logresult = TRUE THEN DO:
      {FINNSDYNBLOB.I}
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordner_UI WINDOW-1 
PROCEDURE btnordner_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   FIND FIRST maga2 NO-LOCK NO-ERROR.
   IF AVAILABLE maga2 THEN DO:
      ASSIGN
      status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}
      frannr = maga2.ORDNING
      till_recid = RECID(maga2)      
      status-ok = BRW_MARK:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = maga2.ORDNING
         maga2.ORDNING = frannr.   
         FIND maga2 WHERE RECID(maga2) = till_recid.
         maga2.ORDNING = tillnr.
         RUN openbdynspec_UI IN brwproc[1].
         FIND maga2 WHERE RECID(maga2) = till_recid NO-LOCK NO-ERROR.
         IF AVAILABLE maga2 THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(maga2)).           
            RUN lastselectdynnv_UI IN brwproc[1].                                
         END.
      END. 
        
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordupp_UI WINDOW-1 
PROCEDURE btnordupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   FIND FIRST maga2 NO-LOCK NO-ERROR.
   IF AVAILABLE maga2 THEN DO:
      ASSIGN
      status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}
      frannr = maga2.ORDNING
      till_recid = RECID(maga2)
      status-ok = BRW_MARK:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.
      
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = maga2.ORDNING
         maga2.ORDNING = frannr.   
         FIND maga2 WHERE RECID(maga2) = till_recid.
         maga2.ORDNING = tillnr.
         RUN openbdynspec_UI IN brwproc[1].
        
         FIND maga2 WHERE RECID(maga2) = till_recid NO-LOCK NO-ERROR.
         IF AVAILABLE maga2 THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(maga2)).   
            RUN lastselectdynnv_UI IN brwproc[1].                     
         END.
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
  DISPLAY FILL-IN_VARDNR FILL-IN_BENAMNING FILL-IN_AONR FILL-IN_DELNR TOG_ALLAMA 
          CMB_VARDVAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE FBTN_SKRIVMALL TOG_ALLAMA CMB_VARDVAL FBTN_SPMALL BRW_MARK BTN_ORDUPP 
         BTN_ANDM BTN_ORDNER BTN_NYBLOB BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastextrahmt_UI WINDOW-1 
PROCEDURE fastextrahmt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF Guru.Konstanter:globforetag = "umea" OR Guru.Konstanter:globforetag = "umbr" THEN DO:
   /*UMEÅ har valt att spara uppgifterna per fastighet och marknr. Övriga spara per fastighet  Lena 20220207*/
   EMPTY TEMP-TABLE fastextra NO-ERROR.      
      FOR EACH maga2 NO-LOCK:
         FIND FIRST fastextra WHERE fastextra.BETECKNING = maga2.BETECKNING AND fastextra.MARKNR = maga2.MARKNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE fastextra THEN DO:
            CREATE fastextra.
            ASSIGN
            fastextra.VARDNR = maga2.VARDNR
            fastextra.BETECKNING = maga2.BETECKNING
            fastextra.MARKNR = maga2.MARKNR.         
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            EMPTY TEMP-TABLE extradatatemp NO-ERROR.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "VARDFASTAG"     
            inextradatatemp.HUVUDINT = maga2.VARDNR
            inextradatatemp.HUVUDCH = maga2.BETECKNING.         
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp WHERE extradatatemp.SOKINT[1] = maga2.MARKNR  NO-LOCK NO-ERROR.
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN
               fastextra.CH1 = extradatatemp.SOKCH[1]
               fastextra.CH2 = extradatatemp.SOKCH[2]
               fastextra.CH3 = extradatatemp.SOKCH[3]
               fastextra.CH4 = extradatatemp.SOKCH[4]
               fastextra.CH5 = extradatatemp.SOKCH[5]
               fastextra.CH6 = extradatatemp.SOKCH[6]
               fastextra.CH7 = extradatatemp.SOKCH[7]
               fastextra.CH8 = extradatatemp.SOKCH[8]
               fastextra.CH9 = extradatatemp.SOKCH[9]
               fastextra.CH10 = extradatatemp.SOKCH[10].            
            END.                           
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            EMPTY TEMP-TABLE extradatatemp NO-ERROR.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "VARDFASTAG2"     
            inextradatatemp.HUVUDINT = maga2.VARDNR
            inextradatatemp.HUVUDCH = maga2.BETECKNING.
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp WHERE extradatatemp.SOKINT[4] = maga2.MARKNR  NO-LOCK NO-ERROR.
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN
               fastextra.CH11 = extradatatemp.SOKCH[1]         
               fastextra.LG1 = extradatatemp.SOKLOG[1]        
               fastextra.LG2 = extradatatemp.SOKLOG[2]
               fastextra.LG3 = extradatatemp.SOKLOG[3]
               fastextra.INT1 = extradatatemp.SOKINT[1]        
               fastextra.INT2 = extradatatemp.SOKINT[2]
               fastextra.INT3 = extradatatemp.SOKINT[3].
            END.
         END.      
      END.      
   
END.
ELSE DO:    
    EMPTY TEMP-TABLE fastextra NO-ERROR.      
      FOR EACH maga2 NO-LOCK:
         FIND FIRST fastextra WHERE fastextra.BETECKNING = maga2.BETECKNING NO-LOCK NO-ERROR.
         IF NOT AVAILABLE fastextra THEN DO:
            CREATE fastextra.
            ASSIGN
            fastextra.VARDNR = maga2.VARDNR
            fastextra.BETECKNING = maga2.BETECKNING.
         END.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         EMPTY TEMP-TABLE extradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "VARDFAST"     
         inextradatatemp.HUVUDINT = maga2.VARDNR
         inextradatatemp.HUVUDCH = maga2.BETECKNING.         
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN
            fastextra.CH1 = extradatatemp.SOKCH[1]
            fastextra.CH2 = extradatatemp.SOKCH[2]
            fastextra.CH3 = extradatatemp.SOKCH[3]
            fastextra.CH4 = extradatatemp.SOKCH[4]
            fastextra.CH5 = extradatatemp.SOKCH[5]
            fastextra.CH6 = extradatatemp.SOKCH[6]
            fastextra.CH7 = extradatatemp.SOKCH[7]
            fastextra.CH8 = extradatatemp.SOKCH[8]
            fastextra.CH9 = extradatatemp.SOKCH[9]
            fastextra.CH10 = extradatatemp.SOKCH[10].            
         END.                           
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         EMPTY TEMP-TABLE extradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "VARDFAST2"     
         inextradatatemp.HUVUDINT = maga2.VARDNR
         inextradatatemp.HUVUDCH = maga2.BETECKNING.
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN
            fastextra.CH11 = extradatatemp.SOKCH[1]   
            fastextra.CH14 = extradatatemp.SOKCH[2]                     /*KEWA VÄRDERING KLAR*/
            fastextra.CH15 =  extradatatemp.SOKCH[3]                     /*KEWA ROTPOST*/
            fastextra.CH16 = extradatatemp.SOKCH[4]                    /*KEWA TILL NÄTÄGAREN*/      
            fastextra.LG1 = extradatatemp.SOKLOG[1]        
            fastextra.LG2 = extradatatemp.SOKLOG[2]
            fastextra.LG3 = extradatatemp.SOKLOG[3]
            fastextra.INT1 = extradatatemp.SOKINT[1]        
            fastextra.INT2 = extradatatemp.SOKINT[2]
            fastextra.INT3 = extradatatemp.SOKINT[3].
         END.   
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infotel_UI WINDOW-1 
PROCEDURE infotel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok THEN DO:   
      sokenamn = INDEX(maga2.MARKAGARE,",").
      IF sokenamn > 0 THEN sokhitta =  SUBSTRING(maga2.MARKAGARE,1,(sokenamn - 1)) + " " +   maga2.GATUADRESS + " " + maga2.POSTADRESS.
      ELSE sokhitta =  maga2.MARKAGARE + " " +   maga2.GATUADRESS + " " + maga2.POSTADRESS.       
      {HITTALANK.I} 
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mall12_UI WINDOW-1 
PROCEDURE mall12_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}         
   
   IF mall = 1 THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 1, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   IF mall = 8 THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 8, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   IF mall = 2 THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 2, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).         
   ELSE IF mall = 5  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 5, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 6  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 6, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 9  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 9, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 11  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 11, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 70  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 70, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 71  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 71, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   ELSE IF mall = 72  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 72, INPUT aodelnr, INPUT spmall, INPUT fildir2, INPUT skrivutmall).
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mall3_UI WINDOW-1 
PROCEDURE mall3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}          
   IF Guru.Konstanter:globforetag = "Ctect" OR Guru.Konstanter:globforetag = "Celpa" THEN ASSIGN bant = 3 hant = 8.
   ELSE ASSIGN bant = 3 hant = 7.

   RUN EXETIKE2.P (INPUT bant , INPUT hant).     
   
   {musarrow.i}            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mall4_UI WINDOW-1 
PROCEDURE mall4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   RUN ETIKEXELIN.P (INPUT fildir,OUTPUT TABLE marktmp ).   
   IF Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" 
    OR Guru.Konstanter:globforetag = "SKEL" OR Guru.Konstanter:globforetag = "elpa" THEN RUN EXETIKE3.P (INPUT TABLE marktmp ).   
   ELSE RUN EXETIKETT2.P (INPUT TABLE marktmp ).   
   {musarrow.i}     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mall7_UI WINDOW-1 
PROCEDURE mall7_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   
   {musarrow.i}     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mallar_UI WINDOW-1 
PROCEDURE mallar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF CMB_VARDVAL = "Fastighetsförteckning Vattenfall äldre anpassad"  THEN mall = 1.
   IF CMB_VARDVAL = "Fastighetsförteckning Vattenfall"  THEN mall = 8.
   IF CMB_VARDVAL = "Fastighetsförteckning Tectel"  THEN mall = 6.
   IF CMB_VARDVAL = "Fastighetsförteckning Allmän"  THEN mall = 2.
   IF CMB_VARDVAL = "Fastighetsförteckning Ellevio"  THEN mall = 9.
   IF CMB_VARDVAL = "Fastighetsförteckning Luleå"  THEN mall = 11.   
   IF CMB_VARDVAL = "Etikett" THEN mall = 3.
   IF CMB_VARDVAL = "Etikett från excel" THEN mall = 4.
   IF CMB_VARDVAL = "Fastighetsförteckning EON"  THEN mall = 5.
   
   IF CMB_VARDVAL = "Etikett till brevmall word"  THEN mall = 7.
   
   /*IF CMB_VARDVAL = "Informationsbrev Vattenfall"  THEN mall = 52.
   IF CMB_VARDVAL = "Informationsbrev från Nätägare"  THEN mall = 53.*/
   IF CMB_VARDVAL = "Infobrev 1 förstärkning"  THEN mall = 54.
   IF CMB_VARDVAL = "Infobrev 2 förstärkning"  THEN mall = 55.
   IF CMB_VARDVAL = "Infobrev 3 förstärkning"  THEN mall = 62.
   IF CMB_VARDVAL = "Infobrev Vilhelmina"  THEN mall = 56.
   IF CMB_VARDVAL = "Infobrev Byviken"  THEN mall = 57.
   IF CMB_VARDVAL = "Infobrev stakningstillstånd"  THEN mall = 58.
   IF CMB_VARDVAL = "Infobrev 1"  THEN mall = 60.
   IF CMB_VARDVAL = "Infobrev 2"  THEN mall = 61.
   IF CMB_VARDVAL = "Mall Utbetalning 3%"  THEN mall = 70.
   IF CMB_VARDVAL = "Mall Utbetalning 10%"  THEN mall = 71.
   IF CMB_VARDVAL = "Mall Utbetalning Tectel"  THEN mall = 72.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispkom_UI WINDOW-1 
PROCEDURE rowdispkom_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TABLE FOR coltemp.
DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   IF AVAILABLE maga2 THEN DO:
      IF Guru.Konstanter:globforetag = "UMEA" OR  Guru.Konstanter:globforetag = "UMBR" THEN DO:
         FIND FIRST fastextra WHERE fastextra.BETECKNING = maga2.BETECKNING AND fastextra.MARKNR =  maga2.MARKNR NO-LOCK NO-ERROR.
         IF AVAILABLE fastextra THEN DO: 
            IF fastextra.CH9 NE "" THEN DO:                       
               /*färga fastigheter som har kommentar gula*/
               maga2.BETECKNING:BGCOLOR IN BROWSE BRW_MARK = 14.
            END.
            IF fastextra.CH5 NE "" THEN DO:                       
               /*färga fastigheter som har kommentar gula*/
               maga2.MARKAGARE:BGCOLOR IN BROWSE BRW_MARK = 10.
            END.                        
         END.
      END.
      ELSE DO:   
         FIND FIRST fastextra WHERE fastextra.BETECKNING = maga2.BETECKNING /*AND fastextra.CH9 NE ""*/ NO-LOCK NO-ERROR.
         IF AVAILABLE fastextra THEN DO: 
            IF fastextra.CH9 NE "" THEN DO:                       
               /*färga fastigheter som har kommentar gula*/
               maga2.BETECKNING:BGCOLOR IN BROWSE BRW_MARK = 14.
            END.
            IF fastextra.CH5 NE "" THEN DO:                       
               /*färga fastigheter som har kommentar gula*/
               maga2.MARKAGARE:BGCOLOR IN BROWSE BRW_MARK = 10.
            END.                        
         END.
      END.   
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapablobbar_UI WINDOW-1 
PROCEDURE skapablobbar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER fildir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE exeinfovar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vartvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vartvarwc AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dummy AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE searchdir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(25)" LABEL "File" NO-UNDO.
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
   ASSIGN
   searchdir = Guru.Konstanter:guruvar.  /*Sökväg till de filer som ska kompileras.*/
   tempfil = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1).
   orgdir = REPLACE(fildir,tempfil,"").
   RUN blobcheck_UI IN blobproch (INPUT fildir, OUTPUT blobid).
   ASSIGN
   vartvar    = ""
   vartvarwc  = ""
   exeinfovar = "".
   EMPTY TEMP-TABLE compdir NO-ERROR.  
   IF entry(2,tempfil,".") = "R" THEN DO: 
      vartvarwc = SEARCH((ENTRY(1,tempfil,".") + ".w")).
      IF vartvarwc = ? OR vartvarwc BEGINS "." THEN vartvarwc = SEARCH((ENTRY(1,tempfil,".") + ".p")).
      IF vartvarwc = ? OR vartvarwc BEGINS "." THEN DO:
         INPUT FROM OS-DIR(searchdir) NO-ECHO.
         REPEAT:
            SET filnamn ^ attrlist NO-ERROR.
            /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
            IF filnamn NE ? THEN DO:
               IF (filnamn BEGINS "c" OR filnamn BEGINS "W") AND attrlist = "D" THEN DO: 
                  CREATE compdir.
                  compdir.BIB = searchdir + filnamn + "\".      
               END.
            END.
         END.
         TRAFF:
         FOR EACH compdir NO-LOCK:
            vartvarwc = SEARCH((ENTRY(1,compdir.BIB + tempfil,".") + ".w")).
            IF vartvarwc = ? THEN vartvarwc = SEARCH((ENTRY(1,compdir.BIB + tempfil,".") + ".p")).
            IF vartvarwc NE ? THEN LEAVE TRAFF.
            vartvarwc = "".
         END.
         
      END.
      IF vartvarwc = ? OR vartvarwc = "" THEN vartvarwc = vartvarwc.
      ELSE DO:
         dummy = ENTRY(2, vartvarwc, ".").
         vartvarwc = "\" + ENTRY(NUM-ENTRIES(vartvarwc,"\") - 1, vartvarwc, "\").
      END.
      vartvar = "\WTID".
      vartvarwc = REPLACE(vartvarwc,"\W","\C").
   END.
   ELSE DO:
      ASSIGN
      vartvar = REPLACE(fildir,Guru.Konstanter:guruvar,"\").
      vartvar = REPLACE(vartvar,"\" + tempfil,"").
      vartvar = REPLACE(vartvar,"\C","\W").
      IF SUBSTRING(vartvar,1,6) = "\komp10" THEN DO:
         SUBSTRING(vartvar,1,6) = " \WTID".      
         vartvar = TRIM(vartvar).
      END.
      IF SUBSTRING(vartvar,1,5) = "\WTID" THEN vartvar = vartvar.
      ELSE DO:
         vartvar = "\WTID".
         IF orgdir = Guru.Konstanter:guruvar THEN vartvar = "".
      END.
      INPUT FROM OS-DIR(searchdir) NO-ECHO.
      REPEAT:
         SET filnamn ^ attrlist NO-ERROR.
         /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
         IF filnamn NE ? THEN DO:
            IF (filnamn BEGINS "c" OR filnamn BEGINS "W") AND attrlist = "D" THEN DO: 
               CREATE compdir.
               compdir.BIB = searchdir + filnamn + "\".                  
            END.
         END.
      END.
      
      TRAFFWC:
      FOR EACH compdir NO-LOCK:
         vartvarwc = SEARCH(compdir.BIB + tempfil).
         IF vartvarwc NE ? THEN LEAVE TRAFFWC.
         vartvarwc = "".
      END.
      IF vartvarwc = "" THEN DO:
         vartvarwc = vartvar.
      END.
      ELSE vartvarwc = "\" + ENTRY(NUM-ENTRIES(vartvarwc,"\") - 1, vartvarwc, "\").
      vartvarwc = REPLACE(vartvarwc,"\W","\C").        
   END.
   
   RUN blobskapa_UI IN blobproch (INPUT fildir, INPUT-OUTPUT blobid, INPUT "PROG",
                                  INPUT exeinfovar,INPUT vartvar,INPUT vartvarwc, OUTPUT felmedd).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI WINDOW-1 
PROCEDURE valda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE markval NO-ERROR. 
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen markägare är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   antal_raknare = 1.
   GET FIRST BRW_MARK.
   DO WHILE AVAILABLE(maga2):
      maga2.ORDNING = antal_raknare.
      antal_raknare = antal_raknare + 1.
      GET NEXT BRW_MARK.
   END.
   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).      
      CREATE markval.
      BUFFER-COPY maga2 TO markval.
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

