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

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
/*{EGENBEN.I}*/
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}                                                         
{MARKTEMP.I}
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
DEFINE SHARED VARIABLE valvard AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE ponr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ktakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE vardurvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mall AS INTEGER NO-UNDO.
DEFINE VARIABLE kontakth AS HANDLE NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antfast AS INTEGER NO-UNDO.
DEFINE VARIABLE fbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollfast AS LOGICAL NO-UNDO.
DEFINE VARIABLE mallharno AS LOGICAL NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE spmall AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivutmall AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokhitta AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokenamn AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE maga                       
   FIELD VARDNR AS INTEGER                   
   FIELD MARKNR AS INTEGER                   
   FIELD PERSONNUMMER AS CHARACTER           
   FIELD PNR2 AS CHARACTER                   
   FIELD MARKAGARE AS CHARACTER              
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER             
   FIELD BETECKNING AS CHARACTER             
   FIELD PROCENT AS INTEGER                  
   FIELD ANAMN AS CHARACTER                  
   FIELD AADRESS AS CHARACTER                
   FIELD APONR AS CHARACTER                  
   FIELD APADRESS AS CHARACTER               
   FIELD APERNR AS CHARACTER 
   FIELD ORDNING AS INTEGER
   FIELD MARKREC AS RECID                    
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING
   INDEX ORDN ORDNING ASCENDING.
DEFINE TEMP-TABLE maga2                       
   FIELD VARDNR AS INTEGER                   
   FIELD MARKNR AS INTEGER                   
   FIELD PERSONNUMMER AS CHARACTER           
   FIELD PNR2 AS CHARACTER                   
   FIELD MARKAGARE AS CHARACTER              
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER             
   FIELD BETECKNING AS CHARACTER             
   FIELD PROCENT AS INTEGER                  
   FIELD ANAMN AS CHARACTER                  
   FIELD AADRESS AS CHARACTER                
   FIELD APONR AS CHARACTER                  
   FIELD APADRESS AS CHARACTER               
   FIELD APERNR AS CHARACTER 
   FIELD ORDNING AS INTEGER
   FIELD MARKREC AS RECID                    
   FIELD KONTAKT AS LOGICAL
   FIELD ANDEL AS CHARACTER   
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING
   INDEX ORDN ORDNING ASCENDING.
DEFINE TEMP-TABLE fastval                       
   FIELD BETECKNING AS CHARACTER                                
   INDEX BETECKNING BETECKNING.
&Scoped-define SHARED SHARED  
{MARKVAL.I}                         
DEFINE BUFFER markvalbuff FOR markval.
DEFINE BUFFER markvalbuff2 FOR markval.
DEFINE BUFFER magabuff FOR maga2.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

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
&Scoped-Define ENABLED-OBJECTS FBTN_SKRIVMALL TOG_FORSTA TOG_VAGHALLARE ~
TOG_TILLFSK TOG_STAMP CMB_VARDVAL FBTN_SPMALL BRW_MARK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VARDNR FILL-IN_BENAMNING ~
FILL-IN_AONR FILL-IN_DELNR TOG_FORSTA TOG_VAGHALLARE TOG_TILLFSK TOG_STAMP ~
CMB_VARDVAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I eXCEL":L 
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

DEFINE BUTTON FBTN_Vmark 
     LABEL "Markuppl. luft":L 
     SIZE 14 BY 1 TOOLTIP "Visa word/excel-mall".

DEFINE VARIABLE CMB_VARDVAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj word/excel-mall" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

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

DEFINE {&NEW} SHARED VARIABLE TOG_FORSTA AS LOGICAL INITIAL no 
     LABEL "Enbart sammanställning":L 
     VIEW-AS TOGGLE-BOX
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_STAMP AS LOGICAL INITIAL no 
     LABEL "Enbart stämplingslängd":L 
     VIEW-AS TOGGLE-BOX
     SIZE 57.88 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_TILLFSK AS LOGICAL INITIAL no 
     LABEL "Visa enbart tillfällig skada" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.5 BY .79 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_VAGHALLARE AS LOGICAL INITIAL no 
     LABEL "Väghållare" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MARK FOR 
      maga2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MARK WINDOW-1 _STRUCTURED
  QUERY BRW_MARK NO-LOCK DISPLAY
      maga2.MARKNR COLUMN-LABEL "Marknr" FORMAT ">>>9":U
      maga2.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      maga2.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "X(256)":U
            WIDTH 40
      maga2.KONTAKT COLUMN-LABEL "Kont" FORMAT "Ja/Nej":U
      maga2.ANDEL COLUMN-LABEL "Andel" FORMAT "X(8)":U WIDTH 6
      maga2.PROCENT COLUMN-LABEL "%" FORMAT ">>9":U
      maga2.BETECKNING COLUMN-LABEL "Beteckning" FORMAT "X(256)":U
            WIDTH 25
      maga2.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(256)":U
            WIDTH 25
      maga2.POSTNUMMER COLUMN-LABEL "Postnummer" FORMAT "XXX XX":U
      maga2.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(256)":U
            WIDTH 20
  ENABLE
      maga2.KONTAKT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 102.5 BY 15.5
         TITLE "Markägare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN_VARDNR AT ROW 1.5 COL 15.75 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 1.5 COL 27.88 COLON-ALIGNED NO-LABEL
     FILL-IN_AONR AT ROW 2.88 COL 15.75 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 2.92 COL 30.75 COLON-ALIGNED
     FBTN_SKRIVMALL AT ROW 4.25 COL 105.5 WIDGET-ID 4
     TOG_FORSTA AT ROW 4.38 COL 2.63
     TOG_VAGHALLARE AT ROW 4.38 COL 31.63
     TOG_TILLFSK AT ROW 4.38 COL 47
     TOG_STAMP AT ROW 5.5 COL 2.63
     CMB_VARDVAL AT ROW 5.5 COL 72 COLON-ALIGNED
     FBTN_SPMALL AT ROW 5.5 COL 105.5 WIDGET-ID 2
     FBTN_Vmark AT ROW 6.79 COL 105.5
     BRW_MARK AT ROW 6.83 COL 1.5
     FBTN_VISA AT ROW 7.92 COL 105.5
     FBTN_SKRIV AT ROW 9 COL 105.5
     FBTN_EX AT ROW 10.08 COL 105.5
     BTN_AVB AT ROW 22.5 COL 105.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.13 BY 22.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: maga2 T "?" NO-UNDO temp-db maga2
      TABLE: ? T "?" NO-UNDO temp-db maga
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Markvärdering"
         HEIGHT             = 22.96
         WIDTH              = 120.25
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 30.04
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
/* BROWSE-TAB BRW_MARK FBTN_Vmark FRAME-A */
/* SETTINGS FOR BUTTON FBTN_EX IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_EX:HIDDEN IN FRAME FRAME-A           = TRUE.

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

/* SETTINGS FOR BUTTON FBTN_Vmark IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_Vmark:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BENAMNING IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOG_FORSTA IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_STAMP IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_TILLFSK IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_VAGHALLARE IN FRAME FRAME-A
   SHARED                                                               */
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
"maga2.MARKNR" "Marknr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.maga2.PERSONNUMMER
"maga2.PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.maga2.MARKAGARE
"maga2.MARKAGARE" "Markägare" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.maga2.KONTAKT
"maga2.KONTAKT" "Kont" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.maga2.ANDEL
"maga2.ANDEL" "Andel" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.maga2.PROCENT
"maga2.PROCENT" "%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.maga2.BETECKNING
"maga2.BETECKNING" "Beteckning" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.maga2.GATUADRESS
"maga2.GATUADRESS" "Gatuadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.maga2.POSTNUMMER
"maga2.POSTNUMMER" "Postnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
      /*
      status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
      */
      DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
      maga2.KONTAKT = INPUT BROWSE BRW_MARK maga2.KONTAKT.
      DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
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
   /*
   status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   */
   IF AVAILABLE maga2 THEN DO:
      DISPLAY maga2.KONTAKT WITH BROWSE BRW_MARK.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maga2.KONTAKT BRW_MARK _BROWSE-COLUMN WINDOW-1
ON LEAVE OF maga2.KONTAKT IN BROWSE BRW_MARK /* Kont */
DO:
   /*
  status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
  */
   IF AVAILABLE maga2 THEN DO:      
      /*
      ASSIGN ktakt = INPUT BROWSE BRW_MARK maga2.KONTAKT.      
      ktakt = LOGICAL(kontakth:SCREEN-VALUE,"Ja/nej").     
      */
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
   /*APPLY "ENTRY" TO maga2.KONTAKT IN BROWSE BRW_MARK.   */
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


&Scoped-define SELF-NAME CMB_VARDVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VARDVAL WINDOW-1
ON VALUE-CHANGED OF CMB_VARDVAL IN FRAME FRAME-A /* Välj word/excel-mall */
DO:
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2015" OR  CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2014" 
   OR CMB_VARDVAL = "Värderingsprotokoll EON"  OR CMB_VARDVAL = "Värderingsprotokoll BIVA" 
   OR CMB_VARDVAL = "Värderingsprotokoll Boden 2013" OR CMB_VARDVAL = "Värderingsprotokoll Boden lågsp 2013" 
   OR CMB_VARDVAL = "Värderingsprotokoll Luleå 2013" OR CMB_VARDVAL = "Värderingsprotokoll Luleå lågsp 2013" OR CMB_VARDVAL = "Värderingsprotokoll Kraftringen"  THEN DO:  
          
      ASSIGN TOG_STAMP = TRUE.   
      DISPLAY TOG_STAMP WITH  FRAME {&FRAME-NAME}.         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX WINDOW-1
ON CHOOSE OF FBTN_EX IN FRAME FRAME-A /* Visa I eXCEL */
DO:   
   visvalvar = 2.
   RUN btnok_UI.   
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
   IF Guru.Konstanter:globforetag = "GRAN" THEN musz = musz.
   ELSE DO:      
      IF TOG_STAMP = TRUE AND antal_valda > 1 THEN DO:
         MESSAGE "Markera en markägare till fastigheten.Övriga syns i värderingsprotokollet" VIEW-AS ALERT-BOX.    
         RETURN NO-APPLY.

      END.
      IF AVAILABLE maga2 THEN DO:
         IF TOG_STAMP = TRUE THEN DO:         
            FIND FIRST magabuff WHERE magabuff.BETECKNING = maga2.BETECKNING AND magabuff.KONTAKT = TRUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE magabuff  THEN DO:
               MESSAGE "Markera kontaktpersonen för fastigheten med Ja i kolumnen Kont" VIEW-AS ALERT-BOX.    
               RETURN NO-APPLY.
            END.
         END.
         
      END.
   END.
   EMPTY TEMP-TABLE maga NO-ERROR. 
   FOR EACH maga2 NO-LOCK:
      CREATE maga.
      BUFFER-COPY maga2 TO maga.
   END.
   {AVBGOM.I}
   RUN VARDUTF3.W (INPUT valvard,INPUT ?,INPUT RECID(valvardtemp),INPUT TABLE maga).
   {AVBFRAM.I}
   
   /*ELSE DO:   
      {AVBGOM.I}
      RUN VARDUTF2.W (INPUT valvard,INPUT ?,INPUT RECID(valvardtemp)).
      {AVBFRAM.I}
   END.*/
   
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
   
   IF mall = 6 OR  mall = 17 OR mall = 18 OR  mall = 47 OR  mall = 51 OR mall = 52 OR mall = 53 OR mall = 54 OR mall = 55 OR mall = 56 OR mall = 57 OR mall = 58 OR mall = 59  OR mall = 60 OR mall = 61 OR mall = 62 OR mall = 63 
   OR mall = 64 OR mall = 65 OR  mall = 70 OR mall = 71 OR mall = 72 OR mall = 73 OR mall = 72 OR mall = 74  OR mall = 75 OR mall = 76  OR mall = 77  THEN .
   ELSE DO:
      MESSAGE "Endast MUA och VP går att skriva ut" VIEW-AS ALERT-BOX.    
      RETURN NO-APPLY.
   END.
   skrivutmall = TRUE.     
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN DO:
      musz = FALSE. 
      skrivutmall = FALSE.
      RETURN NO-APPLY.
   END.
    
    
   APPLY "CHOOSE" TO FBTN_Vmark.
   
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
   IF mall = 6 OR mall = 17 OR mall = 18 OR mall = 47  OR  mall = 51 OR mall = 52 OR mall = 53 OR mall = 54 OR mall = 55 OR mall = 56 OR mall = 57 OR mall = 58 OR mall = 59 OR mall = 60 OR mall = 61 OR mall = 62 OR
   mall = 63 OR mall = 64 OR mall = 65  OR mall = 70 OR mall = 71 OR mall = 72 OR mall = 73 OR mall = 74 OR mall = 75 OR mall = 76  OR mall = 77 OR mall = 78  OR mallharno = true   THEN .
   ELSE DO:
      MESSAGE "Vald mall går ej att spara" VIEW-AS ALERT-BOX.    
      RETURN NO-APPLY.
   END.     
   spmall = TRUE. 
   SYSTEM-DIALOG GET-DIR fildir
         TITLE          "Välj mapp som dokumenten skall sparas i"                  
         INITIAL-DIR    SESSION:TEMP-DIRECTORY.                
   
   
   APPLY "CHOOSE" TO FBTN_Vmark.
   
   ASSIGN fildir = ""      
   spmall = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:   
   visvalvar = 1.
   RUN btnok_UI.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_Vmark
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_Vmark WINDOW-1
ON CHOOSE OF FBTN_Vmark IN FRAME FRAME-A /* Markuppl. luft */
DO:   
   {AMERICANEUROPEAN.I}   
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   RUN mallar_UI.     
   
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   ASSIGN
   antfast = 0
   fbet = "".
   FOR EACH markvalbuff BY markvalbuff.BETECKNING :
      IF fbet NE markvalbuff.BETECKNING THEN antfast = antfast + 1.
      fbet = markvalbuff.BETECKNING.
   END.

   FIND FIRST markval NO-LOCK NO-ERROR.
   IF AVAILABLE markval THEN DO:
      IF mall = 6 OR mall = 47 OR mall = 48 THEN.      
      ELSE DO:      
         kollfast = TRUE.
         IF  mall = 10 OR mall = 11 OR mall = 17 OR mall = 18 OR mall = 55 OR mall = 56 OR mall = 60 OR mall = 61 OR mall = 62 OR mall = 63 OR mall = 73 OR mall = 75 OR mall = 76 OR mall = 78 OR mallharno = TRUE THEN DO:
            IF antfast > 2 THEN DO:                            
               kollfast = TRUE.
            END.
            ELSE kollfast = FALSE.
         END.
         /*TEST Lena flera vp samtidigt för flera fastigheter*/
         IF mall = 6 OR mall = 17 OR mall = 18 OR mall = 47 or  mall = 55 OR mall = 56 OR mall = 60 OR mall = 61 OR mall = 62 OR mall = 63 OR mallharno = true   THEN kollfast = FALSE.
         IF mall = 51 OR mall = 52 OR mall = 53 OR mall = 54 OR mall = 57 OR mall = 58 OR mall = 59 OR mall = 64 OR mall = 65 OR mall = 70 OR mall = 71 OR mall = 72 OR mall = 73 OR mall = 74  OR mall = 75 OR mall = 76 OR mall = 78
          OR mall = 77  THEN kollfast = FALSE.
                 
         IF kollfast = TRUE THEN DO:         
            FOR EACH markvalbuff:
                IF markvalbuff.BETECKNING NE markval.BETECKNING THEN DO:
                   MESSAGE "Du kan inte välja mer än 1 fastighet åt gången." VIEW-AS ALERT-BOX.    
                   RETURN NO-APPLY.
                END.
            END.   
         END.
      END.
     
      FIND FIRST magabuff  WHERE magabuff.BETECKNING = markval.BETECKNING AND magabuff.KONTAKT = TRUE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE magabuff THEN DO:
         MESSAGE "Ingen kontaktperson är markerad med Ja i kolumnen Kont för fastigheten.Vill du markera kontaktperson ?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Avbryt och markera kontaktperson?" UPDATE svar.         
         IF svar THEN DO:
            RETURN NO-APPLY.
         END.

         /*MESSAGE "Markera kontaktpersonen för fastigheten med Ja i kolumnen Kont.Vill du markera kontaktperson ?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Uppdatera kontaktperson?" UPDATE svar.         
         IF svar THEN DO:
            RETURN NO-APPLY.
         END.*/
         /*MESSAGE "Markera kontaktpersonen för fastigheten med Ja i kolumnen Kont" VIEW-AS ALERT-BOX.          
         RETURN NO-APPLY.*/
      END.

   END.
   IF  mall = 3 OR mall = 4  OR mall = 6 OR mall = 10 OR mall = 11 OR mall = 12 OR mall = 13 OR mall = 17 OR mall = 18 OR mall = 47 OR mall = 55 OR mall = 56 OR mall = 60 OR mall = 61 OR
   mall = 62 OR mall = 63 OR mall = 73  OR mall = 75 OR mall = 76 OR mall = 78  or mallharno = TRUE THEN DO:   
      
      FIND FIRST markval NO-LOCK NO-ERROR.
      FOR EACH maga2 WHERE maga2.BETECKNING = markval.BETECKNING NO-LOCK:
         FIND FIRST markvalbuff WHERE markvalbuff.MARKNR = maga2.MARKNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE markvalbuff THEN DO:
            /* markupplåtelseavtalet ska tas ut för alla markägare på en fastighet*/
            CREATE markvalbuff.
            BUFFER-COPY maga2 TO markvalbuff.            
         END.
      END.      
      IF mall = 6 OR mall = 10 OR mall = 11 OR mall = 17 OR mall = 18 OR mall = 47 OR mall = 55 OR mall = 56 OR mall = 60 OR mall = 61 OR mall = 62 OR mall = 63 OR mall = 73  OR mall = 75
      OR mall = 76 OR mall = 78 OR mallharno = TRUE THEN DO:
         FOR EACH fastval NO-LOCK:
            FIND FIRST markval WHERE markval.BETECKNING = fastval.BETECKNING NO-LOCK NO-ERROR.
            FOR EACH maga2 WHERE maga2.BETECKNING = markval.BETECKNING NO-LOCK:
               FIND FIRST markvalbuff WHERE markvalbuff.MARKNR = maga2.MARKNR AND markvalbuff.BETECKNING = maga2.BETECKNING NO-LOCK NO-ERROR.
               IF NOT AVAILABLE markvalbuff THEN DO:
                  /* markupplåtelseavtalet ska tas ut för alla markägare på en fastighet*/
                  CREATE markvalbuff.
                  BUFFER-COPY maga2 TO markvalbuff.            
               END.
            END.
         END.               
      END.
      
      /* det går att ta ut flera avtal samtidigt lena 20140604
      FIND FIRST markval NO-LOCK NO-ERROR.
      IF AVAILABLE markval THEN DO:
         IF mall = 6 OR mall = 47 OR mall = 48 THEN.
         ELSE DO:      
            kollfast = TRUE.
            IF  mall = 60 OR mall = 61 THEN.
            ELSE IF  mall = 10 OR mall = 11 OR mall = 17 OR mall = 18 OR mall = 55 OR mall = 56 OR mall = 60 OR mall = 61 OR mallharno = TRUE THEN DO:
               IF antfast > 1 AND antfast < 3 THEN DO:             
                  FOR EACH markvalbuff:
                     FIND FIRST markvalbuff2 WHERE markvalbuff2.BETECKNING NE markvalbuff.BETECKNING
                     AND markvalbuff2.MARKAGARE = markvalbuff.MARKAGARE    NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE markvalbuff2 THEN DO:               
                        MESSAGE "Det går bara att visa 2 fastigheter som har samma ägare på samma markupplåtelseavtal." VIEW-AS ALERT-BOX.    
                        RETURN NO-APPLY.
                     END.                  
                  END.   
                  kollfast = FALSE.
               END.
            END.
             /*TEST Lena flera vp samtidigt för flera fastigheter*/
            IF mall = 51 OR mall = 52 THEN kollfast = FALSE.
            IF mall = 60 OR mall = 61 THEN kollfast = FALSE.           
            IF kollfast = TRUE THEN DO:         
               FOR EACH markvalbuff:
                   IF markvalbuff.BETECKNING NE markval.BETECKNING THEN DO:
                      MESSAGE "Du kan inte välja mer än 1 fastighet åt gången." VIEW-AS ALERT-BOX.    
                      RETURN NO-APPLY.
                   END.
               END.   
            END.
         END.
      END.*/
      skrivut = FALSE.
      musz = FALSE.
      ponr = ?.
      FIND FIRST markval NO-LOCK NO-ERROR.
      IF AVAILABLE markval THEN DO:
         IF mall = 12  OR mall = 13 THEN.
         ELSE DO:                                 
            IF varforetypchar[7] = "1" THEN DO:     
               RUN PROJEJV.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).
               
            END.
            ELSE DO:            
               RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).
            END.
         END.
         IF musz = TRUE THEN musz = FALSE.
         ELSE DO:                                                          
            IF mall = 3 THEN RUN EXEMKAB.P (INPUT ponr).            
            ELSE IF mall = 4 THEN RUN EXEMLUFT.P (INPUT ponr).            
            ELSE IF mall = 6 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "eonupplåtelse", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 10 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluft", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 11 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemark", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 60 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftLast", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 61 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarkLast", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 73 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarkLastjämt", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 75 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftLastsnat", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 76 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarkLastsnat", INPUT spmall, INPUT fildir, INPUT skrivutmall).                       
            ELSE IF mall = 12 THEN RUN EXEMKAB.P (INPUT "umea").            
            ELSE IF mall = 13 THEN RUN EXEMLUFT.P (INPUT "umea").            
            ELSE IF mall = 17 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftbod", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 18 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarkbod", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 55 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftlul", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 56 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarklul", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 62 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftjuk", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 63 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarkjuk", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            /*ELSE IF mall = 75 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseluftsun", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 76 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelsemarksun", INPUT spmall, INPUT fildir, INPUT skrivutmall).*/
            ELSE IF mall = 78 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "upplåtelseskell", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 27 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandL06", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 28 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandM06", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 29 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFL06", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 30 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFM06", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 31 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandL07", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 32 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandM07", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 33 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFL07", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 34 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFM07", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 35 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandL08", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 36 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandM08", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 37 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFL08", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 38 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFM08", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 39 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandL10", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 40 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandM10", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 41 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFL10", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 42 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFM10", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 43 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandL12", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 44 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandM12", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
            ELSE IF mall = 45 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFL12", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 46 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosandFM12", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 47 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "eonupplåtelseR", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            ELSE IF mall = 48 THEN RUN EXEMKABLU2.P (INPUT ponr, INPUT "harnosanduts", INPUT spmall, INPUT fildir, INPUT skrivutmall).            
                        
         END.        
      END.
      ELSE DO:
         MESSAGE "Det finns inga markägare upplagda för denna värdering." VIEW-AS ALERT-BOX.     
      END.   
   END.
   IF   mall = 22  OR mall = 51 OR mall = 52 OR mall = 53 OR mall = 54 OR mall = 57 OR mall = 58 OR mall = 59 OR mall = 64 OR mall = 65 OR mall = 70 OR mall = 71 OR mall = 72 OR mall = 74 OR mall = 77 THEN DO:   
      
      /*ASSIGN TOG_STAMP = TRUE.   
      DISPLAY TOG_STAMP WITH  FRAME {&FRAME-NAME}.         */
      skrivut = FALSE.
      ponr = ?.
      musz = FALSE.   
      EMPTY TEMP-TABLE maga NO-ERROR. 
      FOR EACH maga2 NO-LOCK:
         CREATE maga.
         BUFFER-COPY maga2 TO maga.
      END.
      FIND FIRST markval NO-LOCK NO-ERROR.
      IF AVAILABLE markval THEN DO:
         IF musz = TRUE THEN musz = FALSE.
         ELSE DO:               
            IF TOG_STAMP = TRUE THEN DO:               
               IF varforetypchar[7] = "1" THEN DO: 
                  RUN PROJEJV.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).                 
               END.                
               ELSE DO:    
                  RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).
               END.
               
               IF musz = TRUE THEN musz = FALSE.
               ELSE RUN EXPROTOVSABN.P (INPUT ponr,INPUT mall,INPUT TABLE maga,INPUT "alla", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            END.
            ELSE DO:                            
               IF varforetypchar[7] = "1" THEN DO: 
                  RUN PROJEJV.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).                 
               END.                
               ELSE DO:    
                  RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).
               END.
               IF musz = TRUE THEN musz = FALSE.               
               ELSE RUN EXPROTOVSABN.P (INPUT ponr,INPUT mall,INPUT TABLE maga,INPUT "vald", INPUT spmall, INPUT fildir, INPUT skrivutmall).
            END.
            
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inga markägare upplagda för denna värdering." VIEW-AS ALERT-BOX.     
      END.      
   END.
   IF mall = 7  THEN DO:   
      skrivut = FALSE.
      FIND FIRST markval NO-LOCK NO-ERROR.
      IF AVAILABLE markval THEN DO:

         RUN EXEBYGG.P.
      END.
      ELSE DO:
         MESSAGE "Det finns inga markägare upplagda för denna värdering." VIEW-AS ALERT-BOX.
      END.   
   END.
   IF mall = 14  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT ponr,INPUT "14", INPUT spmall, INPUT fildir, INPUT skrivutmall).
   END.
   IF mall = 15  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT ponr,INPUT "15", INPUT spmall, INPUT fildir, INPUT skrivutmall).
   END.
   IF mall = 16  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT ponr,INPUT "16", INPUT spmall, INPUT fildir, INPUT skrivutmall).
   END.
   IF mall = 21  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT ponr,INPUT "21", INPUT spmall, INPUT fildir, INPUT skrivutmall).
   END.
   IF mall = 50  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT ponr,INPUT "50", INPUT spmall, INPUT fildir, INPUT skrivutmall).
   END.
   /*IF mall = 27  THEN DO:   
      skrivut = FALSE.      
      RUN GFYTTRANDE.P (INPUT "27").
   END.*/
   IF mall = 8  THEN DO:   
      ASSIGN TOG_STAMP = TRUE.   
      DISPLAY TOG_STAMP WITH  FRAME {&FRAME-NAME}.         
      skrivut = FALSE.
      ponr = ?.
      musz = FALSE.   
      EMPTY TEMP-TABLE maga NO-ERROR. 
      FOR EACH maga2 NO-LOCK:
         CREATE maga.
         BUFFER-COPY maga2 TO maga.
      END.
      FIND FIRST markval NO-LOCK NO-ERROR.
      IF AVAILABLE markval THEN DO:
         /*IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "ETSA"
         OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:*/ 
          IF varforetypchar[7] = "2" THEN DO:      
            RUN PROJTECT2.W (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT "vard",INPUT mall,OUTPUT ponr).
         END.
         IF musz = TRUE THEN musz = FALSE.
         ELSE DO:                   
            RUN EXPROTOEON.P (INPUT ponr,INPUT TABLE maga).
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inga markägare upplagda för denna värdering." VIEW-AS ALERT-BOX.     
      END.      
   END.
   {EUROPEANAMERICAN.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FORSTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FORSTA WINDOW-1
ON VALUE-CHANGED OF TOG_FORSTA IN FRAME FRAME-A /* Enbart sammanställning */
DO:
   TOG_FORSTA = INPUT TOG_FORSTA.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_STAMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_STAMP WINDOW-1
ON VALUE-CHANGED OF TOG_STAMP IN FRAME FRAME-A /* Enbart stämplingslängd */
DO:
   TOG_STAMP = INPUT TOG_STAMP.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TILLFSK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TILLFSK WINDOW-1
ON VALUE-CHANGED OF TOG_TILLFSK IN FRAME FRAME-A /* Visa enbart tillfällig skada */
DO:
  TOG_TILLFSK = INPUT TOG_TILLFSK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAGHALLARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAGHALLARE WINDOW-1
ON VALUE-CHANGED OF TOG_VAGHALLARE IN FRAME FRAME-A /* Väghållare */
DO:
  TOG_VAGHALLARE = INPUT TOG_VAGHALLARE.
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
   IF VALID-HANDLE(vardurvalapph) THEN DELETE PROCEDURE vardurvalapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
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
   ASSIGN fildir = ""
   spmall = FALSE.
   skrivutmall = FALSE.
   kontakth = maga2.KONTAKT:HANDLE IN BROWSE BRW_MARK.
   &Scoped-define FORMATNAMN FILL-IN_AONR
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   RUN ladda2 IN vardurvalapph (INPUT valvardnr,OUTPUT TABLE maga2,
                               OUTPUT TABLE aovardtemp,
                               OUTPUT TABLE varderingtemp).
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN DO: 
      vardrec = RECID(varderingtemp).
      FIND FIRST varderingtemp WHERE RECID(varderingtemp) = vardrec NO-LOCK NO-ERROR.
      ASSIGN 
      FILL-IN_VARDNR = varderingtemp.VARDNR
      FILL-IN_BENAMNING = varderingtemp.BENAMNING.   
      FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
      IF AVAILABLE aovardtemp THEN DO:
         ASSIGN 
         FILL-IN_AONR = aovardtemp.AONR
         FILL-IN_DELNR = aovardtemp.DELNR.
      END.
      ELSE DO:
         FILL-IN_AONR:HIDDEN = TRUE.
         FILL-IN_DELNR:HIDDEN = TRUE.
      END.         
   END.
   ASSIGN 
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok   
   TOG_FORSTA = FALSE
   TOG_STAMP = FALSE
   TOG_VAGHALLARE = FALSE.     
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").
      /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2014").   
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2014").*/
      /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal mark 2013").   
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luftled 2013").*/        
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015").
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2014").     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2014").*/        
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel ej Vattenfall").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft ej Vattenfall").     
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall bilagor").   borttaget 20101128 -inget 25% påslag på åker och skog..  */
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2008"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2009").   */  
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2010").     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2010").     */           
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON LN").     
      /*status-ok = CMB_VARDVAL:ADD-LAST("Bygglov").     */
   END.
   ELSE IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:   
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").   
      /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2014").   
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2014").*/            
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015").                    
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2014").     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2014").*/  
                                     
      IF Guru.Konstanter:globforetag = "VAST"  THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel ej Vattenfall").     
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft ej Vattenfall").                   
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON LN").
      END.              
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Vatt").
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").     
      
   END.
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "SKOK"
   OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "SKEL" THEN DO:           
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").
      /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2014"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2014").*/                       
      IF Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK"  OR Guru.Konstanter:globforetag = "SKEL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Boden mark"). 
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Boden luftled").
         IF Guru.Konstanter:globforetag = "SKOK" THEN DO:
            status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Luleå mark"). 
            status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Luleå luftled").
            status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Jukkasjärvi mark"). 
            status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Jukkasjärvi luftled").
         END.                                     
      END.      
      IF Guru.Konstanter:globforetag = "JSBF" THEN DO:            
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Jukkasjärvi mark"). 
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Jukkasjärvi luftled").
      END.
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015").
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2014").     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2014").*/                 
                       
      IF Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK"  OR Guru.Konstanter:globforetag = "SKEL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Boden 2013").           
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Boden lågsp 2013").                             
      END.
      IF  Guru.Konstanter:globforetag = "SWEO" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Jämtkraft").
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Jämtkraft").                    
      END.
      IF Guru.Konstanter:globforetag = "SKOK"  THEN DO:          
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Luleå 2013").           
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Luleå lågsp 2013").
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Jukkasjärvi 2013").           
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Jukkasjärvi lågsp 2013").
      END.   
      IF Guru.Konstanter:globforetag = "JSBF"  THEN DO:                   
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Jukkasjärvi 2013").           
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Jukkasjärvi lågsp 2013").
      END.   
      IF Guru.Konstanter:globforetag = "cTECT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB").              
      END.
      IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
         status-ok = CMB_VARDVAL:ADD-LAST("Markägaravtal FIN").              
      END.
      IF Guru.Konstanter:globforetag = "SWEO"  OR Guru.Konstanter:globforetag = "SKEL" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Schablon Storskogsbruket").     
      END.        
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON LN").
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON RN").
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel allmän").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft allmän").     
      /*status-ok = CMB_VARDVAL:ADD-LAST("Bygglov").    */ 
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Vatt").
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").
      IF Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  THEN DO:
         status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal JBF").
      END.        
      IF Guru.Konstanter:globforetag = "CTECT" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll EON").     
      END.
      IF Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel UmeåE").     
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft UmeåE").     
      END.
      
      IF Guru.Konstanter:globforetag = "ELPA" OR  Guru.Konstanter:globforetag = "GREL" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll BIVA").     
      END.
      /*IF Guru.Konstanter:globforetag = "sweo" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Schablon Storskogsbruket").     
      END.*/
      IF Guru.Konstanter:globforetag = "sweo"  OR Guru.Konstanter:globforetag = "SKEL" THEN DO:         
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Skellefteå Kraft").     
      END.
            
   END.
   ELSE IF Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:                                           
      IF varderingtemp.NATAGARE BEGINS "HÄRNÖSAND" THEN DO:              
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB L 06").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB M 06").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev L 06").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev M 06").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB L 07").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB M 07").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev L 07").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev M 07").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB L 08").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB M 08").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev L 08").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev M 08").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB L 10").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB M 10").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev L 10").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev M 10").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB L 12").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB M 12").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev L 12").
         status-ok = CMB_VARDVAL:ADD-LAST("HEMAB Följebrev M 12").
         IF Guru.Konstanter:globforetag = "CREJI" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
            status-ok = CMB_VARDVAL:ADD-LAST("HEMAB utskick").
         END.                
      END.                          
      ELSE DO:
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").
         /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2014"). 
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2014").*/  
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015").        
         /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2014").     
         status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2014").*/                                           
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON LN").
         status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON RN").
      END.               
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel allmän").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft allmän").           
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Vatt").           
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").
   END.
   ELSE IF Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "NYLB" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").               
      /*status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2014").   
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2014").*/     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015"). 
      /*status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2014").     
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall lågsp 2014").*/           
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel allmän").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft allmän").                       
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal EON LN").           
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Vatt").  
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").   
   END.
   ELSE IF Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:                    
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Kraftringen").
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel allmän").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft allmän").  
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").                  
      /*  ej vattenfalls status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal").*/  
   END.
   ELSE IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:                    
      status-ok = CMB_VARDVAL:ADD-LAST("MUA fiber Sundsvall Elnät").     
      status-ok = CMB_VARDVAL:ADD-LAST("MUA starkström Sundsvall Elnät").      
      /*status-ok = CMB_VARDVAL:ADD-LAST("MUA fiber/mark Sundsvall Elnät").     
      status-ok = CMB_VARDVAL:ADD-LAST("MUA luft Sundsvall Elnät").*/
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Sundsvall Elnät 2015").
      /*status-ok = CMB_VARDVAL:ADD-LAST("Uträkningsmall Sundsvall Elnät").
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel allmän").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft allmän"). */ 
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt").       
      
   /*   status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall mark 2015"). 
     status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal Vattenfall luftled 2015").      
      status-ok = CMB_VARDVAL:ADD-LAST("Värderingsprotokoll Vattenfall 2015").*/           
      /*  ej vattenfalls status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal").*/  
   END.
   ELSE DO:        
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal kabel").     
      status-ok = CMB_VARDVAL:ADD-LAST("Markupplåtelseavtal luft").     
      /*status-ok = CMB_VARDVAL:ADD-LAST("Bygglov").     */
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Vatt").    
      status-ok = CMB_VARDVAL:ADD-LAST("REV-avtal Allmänt"). 
   END.
   
   RUN enable_UI.   
   FBTN_Vmark:LABEL = "Visa word/excel-mall".     
   
   FBTN_SPMALL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "ELPA" THEN CMB_VARDVAL:SCREEN-VALUE = "Markupplåtelseavtal Vattenfall mark 2015".            
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "SKOK"
   OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "cELPA" THEN CMB_VARDVAL:SCREEN-VALUE = "Markupplåtelseavtal Vattenfall mark 2015".            
   ELSE IF Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "NYLB" OR Guru.Konstanter:globforetag = "cELPA" THEN CMB_VARDVAL:SCREEN-VALUE = "Markupplåtelseavtal Vattenfall mark 2015".
   ELSE IF Guru.Konstanter:globforetag = "KRIN"  THEN CMB_VARDVAL:SCREEN-VALUE = "Värderingsprotokoll Kraftringen".
   ELSE IF Guru.Konstanter:globforetag = "snat"  THEN CMB_VARDVAL:SCREEN-VALUE = "MUA fiber Sundsvall Elnät".         
   ELSE IF Guru.Konstanter:globforetag = "SKEL"  THEN CMB_VARDVAL:SCREEN-VALUE = "Markupplåtelseavtal Skellefteå Kraft".               
   ELSE CMB_VARDVAL:SCREEN-VALUE = "Markupplåtelseavtal kabel".
   IF Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" THEN DO:                                           
      IF varderingtemp.NATAGARE BEGINS "HÄRNÖSAND" THEN DO:              
         CMB_VARDVAL:SCREEN-VALUE = "HEMAB L 06".
      END.
   END.                  
   IF varforetypval[22] = 1  THEN DO:
      maga2.PROCENT:VISIBLE IN BROWSE BRW_MARK = FALSE.
   END.
   ELSE DO:
      maga2.ANDEL:VISIBLE IN BROWSE BRW_MARK = FALSE.
   END.
   {FRMSIZE.I}    
   
   ENABLE BRW_MARK  WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_MARK:HIDDEN = FALSE.   
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      TOG_STAMP:LABEL = "Enbart stämplingslängd".
   END.
   ELSE DO:  
      TOG_STAMP:LABEL = "Ett markvärderingsprotokoll för samtliga delägare av fastigheten.".
      TOG_STAMP:TOOLTIP IN FRAME {&FRAME-NAME} = "Markera kontaktperson , övriga delägare skrivs in i protokollet".
   END.   
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aovardtemp THEN DO:    
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.         
   IF Guru.Konstanter:globforetag = "UMEA" THEN TOG_TILLFSK:HIDDEN = FALSE.
   ELSE TOG_TILLFSK:HIDDEN = TRUE.
   /*lena 20110519*/
   TOG_VAGHALLARE:HIDDEN = TRUE.   
      
   RUN setcolindex_UI IN brwproc[1] (INPUT "maga2.BETECKNING BY maga2.MARKNR").
   RUN openbdynspec_UI IN brwproc[1].
   RUN getfirst_UI IN brwproc[1].
   RELEASE maga2 NO-ERROR.
      
   ENABLE FBTN_SKRIV FBTN_VISA FBTN_EX FBTN_VMARK  WITH FRAME {&FRAME-NAME}.     
        
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT  BRW_MARK:HANDLE IN FRAME {&FRAME-NAME}).  
   
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_MARK:HANDLE,INPUT "Sök telefonnummer via Internet",INPUT "infotel_UI").
   IF Guru.Konstanter:appcon THEN DO:
      RUN VARDURVALAPP.P PERSISTENT SET vardurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VARDURVALAPP.P PERSISTENT SET vardurvalapph.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI WINDOW-1 
PROCEDURE btnok_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" THEN musz = musz.
   ELSE DO:      
      IF TOG_STAMP = TRUE AND antal_valda > 1 THEN DO:
         MESSAGE "Markera en markägare till fastigheten.Övriga syns i värderingsprotokollet" VIEW-AS ALERT-BOX.    
         RETURN NO-APPLY.
      END.
      IF AVAILABLE maga2 THEN DO:
         IF TOG_STAMP = TRUE THEN DO:         
            FIND FIRST magabuff WHERE magabuff.BETECKNING = maga2.BETECKNING AND magabuff.KONTAKT = TRUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE magabuff  THEN DO:
               MESSAGE "Markera kontaktpersonen för fastigheten med Ja i kolumnen Kont" VIEW-AS ALERT-BOX.    
               RETURN NO-APPLY.
            END.
         END.
         
      END.

   END.
   skrivut = FALSE.    
   {AVBGOM.I}
   EMPTY TEMP-TABLE maga NO-ERROR. 
   FOR EACH maga2 NO-LOCK:
      CREATE maga.
      BUFFER-COPY maga2 TO maga.
   END.   
   RUN VARDUTF3.W (INPUT valvard,INPUT ?,INPUT RECID(valvardtemp),INPUT TABLE maga).
   {AVBFRAM.I}   
   /*ELSE DO:   
      {AVBGOM.I}
      RUN VARDUTF2.W (INPUT valvard,INPUT ?,INPUT RECID(valvardtemp)).
      {AVBFRAM.I}
   END.*/
   
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
  DISPLAY FILL-IN_VARDNR FILL-IN_BENAMNING FILL-IN_AONR FILL-IN_DELNR TOG_FORSTA 
          TOG_VAGHALLARE TOG_TILLFSK TOG_STAMP CMB_VARDVAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE FBTN_SKRIVMALL TOG_FORSTA TOG_VAGHALLARE TOG_TILLFSK TOG_STAMP 
         CMB_VARDVAL FBTN_SPMALL BRW_MARK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mallar_UI WINDOW-1 
PROCEDURE mallar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   IF CMB_VARDVAL = "Markupplåtelseavtal kabel ej Vattenfall" OR CMB_VARDVAL = "Markupplåtelseavtal kabel allmän" 
   OR CMB_VARDVAL = "Markupplåtelseavtal kabel"   THEN mall = 3.
   IF CMB_VARDVAL = "Markupplåtelseavtal luft ej Vattenfall" OR CMB_VARDVAL = "Markupplåtelseavtal luft allmän" 
   OR CMB_VARDVAL = "Markupplåtelseavtal luft"   THEN mall = 4.
   /*IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall bilagor" THEN mall = 5.*/
   IF CMB_VARDVAL = "Markupplåtelseavtal EON LN" THEN mall = 6.
   IF CMB_VARDVAL = "Markupplåtelseavtal EON RN" THEN mall = 47.
  /* IF CMB_VARDVAL = "Bygglov" THEN mall = 7.*/
   IF CMB_VARDVAL = "Värderingsprotokoll EON" THEN mall = 8.
  /* IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2008" THEN mall = 9.*/
   /*IF CMB_VARDVAL = "Markupplåtelseavtal luftled 2013" OR CMB_VARDVAL = "Markupplåtelseavtal Vattenfall luftled 2013" THEN mall = 10.
   IF CMB_VARDVAL = "Markupplåtelseavtal mark 2013" OR CMB_VARDVAL = "Markupplåtelseavtal Vattenfall mark 2013" THEN mall = 11.*/
   IF CMB_VARDVAL = "Markupplåtelseavtal kabel UmeåE"  THEN mall = 12.
   IF CMB_VARDVAL = "Markupplåtelseavtal luft UmeåE" THEN mall = 13.
   IF CMB_VARDVAL = "REV-avtal Vatt" THEN mall = 14.
   IF CMB_VARDVAL = "REV-avtal JBF" THEN mall = 15.
   IF CMB_VARDVAL = "REV-avtal Allmänt" THEN mall = 16.  
   
   /*IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2009" THEN mall = 16.*/
   IF CMB_VARDVAL = "Markupplåtelseavtal Boden luftled" THEN mall = 17.
   IF CMB_VARDVAL = "Markupplåtelseavtal Boden mark" THEN mall = 18.
   /*IF CMB_VARDVAL = "Värderingsprotokoll Boden" THEN mall = 19.
   IF CMB_VARDVAL = "Värderingsprotokoll Boden 2009" THEN mall = 20.*/
   IF CMB_VARDVAL = "HEMAB" THEN mall = 21.
   IF CMB_VARDVAL = "Värderingsprotokoll BIVA" THEN mall = 22.
   /*IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2010" THEN mall = 23.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall lågsp 2010" THEN mall = 24.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2011" THEN mall = 25.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall lågsp 2011" THEN mall = 26.*/
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2014" THEN mall = 51.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall lågsp 2014" THEN mall = 52.   
   
   
   /*IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2013" THEN mall = 51.
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall lågsp 2013" THEN mall = 52.*/
   IF CMB_VARDVAL = "Värderingsprotokoll Vattenfall 2015" THEN mall = 70.
   IF CMB_VARDVAL = "Värderingsprotokoll Boden 2013" THEN mall = 53.
   IF CMB_VARDVAL = "Värderingsprotokoll Boden lågsp 2013" THEN mall = 54.
   IF CMB_VARDVAL = "Värderingsprotokoll Jämtkraft" THEN mall = 72. 
   IF CMB_VARDVAL = "Markupplåtelseavtal Jämtkraft" THEN mall = 73.
   IF CMB_VARDVAL = "Värderingsprotokoll Sundsvall Elnät 2015" THEN mall = 74.
     
   IF CMB_VARDVAL = "HEMAB L 06" THEN mall = 27.
   IF CMB_VARDVAL = "HEMAB M 06" THEN mall = 28.
   IF CMB_VARDVAL = "HEMAB Följebrev L 06" THEN mall = 29.
   IF CMB_VARDVAL = "HEMAB Följebrev M 06" THEN mall = 30.
   
   IF CMB_VARDVAL = "HEMAB L 07" THEN mall = 31.
   IF CMB_VARDVAL = "HEMAB M 07" THEN mall = 32.
   IF CMB_VARDVAL = "HEMAB Följebrev L 07" THEN mall = 33.
   IF CMB_VARDVAL = "HEMAB Följebrev M 07" THEN mall = 34.
   IF CMB_VARDVAL = "HEMAB L 08" THEN mall = 35.
   IF CMB_VARDVAL = "HEMAB M 08" THEN mall = 36.
   IF CMB_VARDVAL = "HEMAB Följebrev L 08" THEN mall = 37.
   IF CMB_VARDVAL = "HEMAB Följebrev M 08" THEN mall = 38.
   IF CMB_VARDVAL = "HEMAB L 10" THEN mall = 39.
   IF CMB_VARDVAL = "HEMAB M 10" THEN mall = 40.
   IF CMB_VARDVAL = "HEMAB Följebrev L 10" THEN mall = 41.
   IF CMB_VARDVAL = "HEMAB Följebrev M 10" THEN mall = 42.
   IF CMB_VARDVAL = "HEMAB L 12" THEN mall = 43.
   IF CMB_VARDVAL = "HEMAB M 12" THEN mall = 44.
   IF CMB_VARDVAL = "HEMAB Följebrev L 12" THEN mall = 45.
   IF CMB_VARDVAL = "HEMAB Följebrev M 12" THEN mall = 46.
   IF CMB_VARDVAL = "HEMAB utskick" THEN mall = 48.
   IF CMB_VARDVAL = "Markägaravtal FIN" THEN mall = 50.
   IF CMB_VARDVAL = "Markupplåtelseavtal Luleå luftled" THEN mall = 55.
   IF CMB_VARDVAL = "Markupplåtelseavtal Luleå mark" THEN mall = 56.
   IF CMB_VARDVAL = "Värderingsprotokoll Luleå 2013" THEN mall = 57.   
   IF CMB_VARDVAL = "Värderingsprotokoll Luleå lågsp 2013" THEN mall = 58.
   IF CMB_VARDVAL = "Värderingsprotokoll Kraftringen" THEN mall = 59.
   
   IF CMB_VARDVAL = "Markupplåtelseavtal Jukkasjärvi luftled" THEN mall = 62.
   IF CMB_VARDVAL = "Markupplåtelseavtal Jukkasjärvi mark" THEN mall = 63.
   IF CMB_VARDVAL = "Värderingsprotokoll Jukkasjärvi 2013" THEN mall = 64.   
   IF CMB_VARDVAL = "Värderingsprotokoll Jukkasjärvi lågsp 2013" THEN mall = 65.
   IF CMB_VARDVAL = "Markupplåtelseavtal Vattenfall luftled 2015" THEN mall = 60.
   IF CMB_VARDVAL = "Markupplåtelseavtal Vattenfall mark 2015" THEN mall = 61.
   /*IF CMB_VARDVAL = "Markupplåtelseavtal Vattenfall luftled 2014" THEN mall = 60.
   IF CMB_VARDVAL = "Markupplåtelseavtal Vattenfall mark 2014" THEN mall = 61.*/   
   IF CMB_VARDVAL = "Schablon Storskogsbruket" THEN mall = 71.
   
   IF CMB_VARDVAL = "MUA starkström Sundsvall Elnät" THEN mall = 75.
   IF CMB_VARDVAL = "MUA fiber Sundsvall Elnät" THEN mall = 76.
   IF CMB_VARDVAL = "Uträkningsmall Sundsvall Elnät" THEN mall = 77.
   IF CMB_VARDVAL = "Markupplåtelseavtal Skellefteå Kraft" THEN mall = 78.
   
   
   mallharno = FALSE.
   IF mall GE 27 AND mall LE 46 THEN mallharno = TRUE.
   IF mall = 48 THEN mallharno = TRUE.
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
   EMPTY TEMP-TABLE fastval NO-ERROR. 
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).      
      CREATE markval.
      BUFFER-COPY maga2 TO markval.
      FIND FIRST fastval WHERE fastval.BETECKNING = markval.BETECKNING NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastval THEN DO:
         CREATE fastval.
         ASSIGN fastval.BETECKNING = markval.BETECKNING.
      END.
      antal_raknare = antal_raknare + 1.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

