&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
DEFINE SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.


/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/
DEFINE SHARED VARIABLE globniv LIKE ANVANDARE.AV-LEVEL NO-UNDO.
DEFINE SHARED VARIABLE globallpers LIKE ANVANDARE.ALLPERS NO-UNDO. 
DEFINE SHARED VARIABLE globallao LIKE ANVANDARE.ALLAONR NO-UNDO. 
DEFINE SHARED VARIABLE globomr LIKE PERSONALTAB.OMRADE NO-UNDO. 
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE plusdval LIKE PROGVAL.JANEJ NO-UNDO.
DEFINE SHARED VARIABLE plusaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE plusdnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.        
DEFINE SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.

DEFINE VARIABLE stansrec AS RECID NO-UNDO.
DEFINE VARIABLE kollraknare AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE kontrollstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE regbtn AS LOGICAL NO-UNDO.
DEFINE VARIABLE lop1 AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE halvkvart AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tidapptemp
   FIELD FORETAG LIKE FORETAG.FORETAG
   FIELD ANVANDARE LIKE ANVANDARE.ANVANDARE
   FIELD RECPERS AS RECID
   FIELD RECTID AS RECID
   FIELD DATUM AS DATE.

DEFINE SHARED TEMP-TABLE tidpers    
   FIELD EFTERNAMN LIKE PERSONALTAB.EFTERNAMN 
   FIELD FORNAMN LIKE PERSONALTAB.FORNAMN 
   FIELD PERSONALKOD LIKE PERSONALTAB.PERSONALKOD 
   FIELD ANSVARIGTIDR LIKE PERSONALTAB.ANSVARIGTIDR 
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD VECKONUMMER LIKE TIDREGITAB.VECKONUMMER 
   FIELD TIDPERSREC AS RECID
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.  


DEFINE TEMP-TABLE nytidtemp
   FIELD AONR LIKE TIDREGITAB.AONR 
   FIELD DATUM AS INTEGER FORMAT ">9" 
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD NODF LIKE TIDREGITAB.NODF 
   FIELD OVERTIDUTTAG AS CHARACTER FORMAT "X(4)" 
   FIELD PRIS LIKE TIDREGITAB.PRIS 
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD SLUT LIKE TIDREGITAB.SLUT 
   FIELD START LIKE TIDREGITAB.START 
   FIELD TRAKTAMENTE LIKE TIDREGITAB.TRAKTAMENTE 
   FIELD UTRYCK LIKE TIDREGITAB.UTRYCKNING
   FIELD TIDREC AS RECID
   FIELD ANDRA AS LOGICAL
   INDEX DATUM IS PRIMARY DATUM START SLUT
   INDEX ANDRA ANDRA DATUM START SLUT.
DEFINE TEMP-TABLE restemp   
   FIELD DATUM AS INTEGER FORMAT ">9" 
   FIELD SLUT LIKE TIDREGITAB.SLUT 
   FIELD START LIKE TIDREGITAB.START 
   INDEX DATUM IS PRIMARY DATUM START SLUT.


DEFINE TEMP-TABLE omrtemp
   FIELD OMRADE LIKE OMRADETAB.OMRADE
   FIELD NAMN LIKE OMRADETAB.NAMN
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.
    
DEFINE TEMP-TABLE stanstid
   FIELD ANVANDARE      LIKE TIDREGITAB.ANVANDARE 
   FIELD AONR      LIKE TIDREGITAB.AONR 
   FIELD BERANTAL          LIKE TIDREGITAB.BERANTAL 
   FIELD BERBEORD          LIKE TIDREGITAB.BERBEORD 
   FIELD BEREDSKAP         LIKE TIDREGITAB.BEREDSKAP 
   FIELD BEREDSKAPSLUT     LIKE TIDREGITAB.BEREDSKAPSLUT 
   FIELD BEREDSKAPSTART            LIKE TIDREGITAB.BEREDSKAPSTART 
   FIELD BILFORARE         LIKE TIDREGITAB.BILFORARE 
   FIELD DAG       LIKE TIDREGITAB.DAG 
   FIELD DATUM     LIKE TIDREGITAB.DATUM 
   FIELD DELNR     LIKE TIDREGITAB.DELNR 
   FIELD ENFLERDAGS        LIKE TIDREGITAB.ENFLERDAGS 
   FIELD GODKAND           LIKE TIDREGITAB.GODKAND 
   FIELD LAGANTAL          LIKE TIDREGITAB.LAGANTAL 
   FIELD LAGBAS            LIKE TIDREGITAB.LAGBAS 
   FIELD LONAUTO           LIKE TIDREGITAB.LONAUTO 
   FIELD LONTILLAGG        LIKE TIDREGITAB.LONTILLAGG 
   FIELD LONTILLANTAL      LIKE TIDREGITAB.LONTILLANTAL 
   FIELD NODF      LIKE TIDREGITAB.NODF 
   FIELD OANT1     LIKE TIDREGITAB.OANT1 
   FIELD OANT2     LIKE TIDREGITAB.OANT2 
   FIELD OANT3     LIKE TIDREGITAB.OANT3 
   FIELD OKOD1     LIKE TIDREGITAB.OKOD1 
   FIELD OKOD2     LIKE TIDREGITAB.OKOD2 
   FIELD OKOD3     LIKE TIDREGITAB.OKOD3 
   FIELD OSL1      LIKE TIDREGITAB.OSL1 
   FIELD OSL2      LIKE TIDREGITAB.OSL2 
   FIELD OSL3      LIKE TIDREGITAB.OSL3 
   FIELD OST1      LIKE TIDREGITAB.OST1 
   FIELD OST2      LIKE TIDREGITAB.OST2 
   FIELD OST3      LIKE TIDREGITAB.OST3 
   FIELD OVERANTAL         LIKE TIDREGITAB.OVERANTAL 
   FIELD OVERAUTO          LIKE TIDREGITAB.OVERAUTO 
   FIELD OVERTIDTILL       LIKE TIDREGITAB.OVERTIDTILL 
   FIELD OVERTIDUTTAG      LIKE TIDREGITAB.OVERTIDUTTAG 
   FIELD PERSONALKOD       LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS      LIKE TIDREGITAB.PRIS 
   FIELD PRISTYP           LIKE TIDREGITAB.PRISTYP 
   FIELD PROGRAM           LIKE TIDREGITAB.PROGRAM 
   FIELD RECTIDVIS         LIKE TIDREGITAB.RECTIDVIS 
   FIELD RESMAL            LIKE TIDREGITAB.RESMAL 
   FIELD SLUT              LIKE TIDREGITAB.SLUT  
   FIELD START             LIKE TIDREGITAB.START
   FIELD SLUTM              LIKE TIDREGITAB.SLUT  
   FIELD STARTM             LIKE TIDREGITAB.START  
   FIELD TIDLOG            LIKE TIDREGITAB.TIDLOG 
   FIELD TOTALT            LIKE TIDREGITAB.TOTALT 
   FIELD TRAKTAMENTE       LIKE TIDREGITAB.TRAKTAMENTE 
   FIELD TRAKTANTAL        LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAKTAUTO         LIKE TIDREGITAB.TRAKTAUTO 
   FIELD TRAKTKOD          LIKE TIDREGITAB.TRAKTKOD 
   FIELD TRAKTTOT          LIKE TIDREGITAB.TRAKTTOT 
   FIELD UTRYCKNING        LIKE TIDREGITAB.UTRYCKNING 
   FIELD VECKOKORD         LIKE TIDREGITAB.VECKOKORD 
   FIELD VECKONUMMER       LIKE TIDREGITAB.VECKONUMMER
   FIELD FELTXT            AS CHARACTER
   FIELD FELNR             AS INTEGER
   FIELD ORDNING           AS INTEGER
   INDEX TID IS PRIMARY ORDNING PERSONALKOD DATUM START SLUT AONR DELNR.
DEFINE BUFFER nytidtempbuff FOR nytidtemp.
DEFINE BUFFER oradebuff FOR OMRADETAB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_STANS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES stanstid

/* Definitions for BROWSE BRW_STANS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_STANS stanstid.PERSONALKOD ~
stanstid.DATUM stanstid.AONR stanstid.DELNR stanstid.TRAKTAMENTE ~
stanstid.START stanstid.SLUT stanstid.OVERTIDUTTAG stanstid.LONTILLAGG ~
stanstid.LONTILLANTAL stanstid.FELTXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STANS stanstid.PERSONALKOD ~
stanstid.DATUM stanstid.AONR stanstid.DELNR stanstid.TRAKTAMENTE ~
stanstid.START stanstid.SLUT stanstid.OVERTIDUTTAG stanstid.LONTILLAGG ~
stanstid.LONTILLANTAL 
&Scoped-define FIELD-PAIRS-IN-QUERY-BRW_STANS~
 ~{&FP1}PERSONALKOD ~{&FP2}PERSONALKOD ~{&FP3}~
 ~{&FP1}DATUM ~{&FP2}DATUM ~{&FP3}~
 ~{&FP1}AONR ~{&FP2}AONR ~{&FP3}~
 ~{&FP1}DELNR ~{&FP2}DELNR ~{&FP3}~
 ~{&FP1}TRAKTAMENTE ~{&FP2}TRAKTAMENTE ~{&FP3}~
 ~{&FP1}START ~{&FP2}START ~{&FP3}~
 ~{&FP1}SLUT ~{&FP2}SLUT ~{&FP3}~
 ~{&FP1}OVERTIDUTTAG ~{&FP2}OVERTIDUTTAG ~{&FP3}~
 ~{&FP1}LONTILLAGG ~{&FP2}LONTILLAGG ~{&FP3}~
 ~{&FP1}LONTILLANTAL ~{&FP2}LONTILLANTAL ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STANS stanstid
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STANS stanstid
&Scoped-define OPEN-QUERY-BRW_STANS OPEN QUERY BRW_STANS FOR EACH stanstid NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STANS stanstid
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STANS stanstid


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_STANS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-36 RECT-32 BRW_STANS BTN_REG BTN_AVB ~
BTN_KONTROLL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD FILL-IN_FORNAMN ~
FILL-IN_EFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY  NO-CONVERT-3D-COLORS
     LABEL "Avbryt":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_KONTROLL 
     LABEL "Kontroll" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_REG 
     LABEL "Registrera":L 
     SIZE 12 BY 1.5.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 95.63 BY 22.82
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.75 BY 2.27
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_STANS FOR 
      stanstid SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_STANS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STANS C-Win _STRUCTURED
  QUERY BRW_STANS NO-LOCK DISPLAY
      stanstid.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(6)"
      stanstid.DATUM COLUMN-LABEL "Datum"
      stanstid.AONR COLUMN-LABEL "Aonr" FORMAT "X(8)"
      stanstid.DELNR COLUMN-LABEL "Delnr" FORMAT ">99" LABEL-FONT 11
      stanstid.TRAKTAMENTE COLUMN-LABEL "Trakt.!zon" FORMAT "9"
            LABEL-FONT 11
      stanstid.START COLUMN-LABEL "Start!tid" FORMAT ">9999" LABEL-FONT 11
      stanstid.SLUT COLUMN-LABEL "Slut!tid" FORMAT ">9999" LABEL-FONT 11
      stanstid.OVERTIDUTTAG COLUMN-LABEL "Öv.tid!uttag" LABEL-FONT 11
      stanstid.LONTILLAGG COLUMN-LABEL "Lart" FORMAT "x(5)"
      stanstid.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>>>>9.<<"
      stanstid.FELTXT COLUMN-LABEL "Feltext" FORMAT "X(68)" COLUMN-FONT 11
            LABEL-FONT 11
  ENABLE
      stanstid.PERSONALKOD
      stanstid.DATUM
      stanstid.AONR
      stanstid.DELNR
      stanstid.TRAKTAMENTE
      stanstid.START
      stanstid.SLUT
      stanstid.OVERTIDUTTAG
      stanstid.LONTILLAGG
      stanstid.LONTILLANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 94 BY 12.68
         FONT 11.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN_PERSONALKOD AT ROW 2.77 COL 16.13 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 2.77 COL 33.38 COLON-ALIGNED
     FILL-IN_EFTERNAMN AT ROW 2.77 COL 65.38 COLON-ALIGNED
     BRW_STANS AT ROW 4 COL 2.38
     BTN_REG AT ROW 17.32 COL 26.25
     BTN_AVB AT ROW 17.32 COL 40.25
     BTN_KONTROLL AT ROW 17.32 COL 54.25
     "Person som tidskrivs:" VIEW-AS TEXT
          SIZE 45.75 BY 1.09 AT ROW 1.64 COL 16.88
          FONT 17
     "Hur skall det finnas direktkopplingar till andra funktioner?" VIEW-AS TEXT
          SIZE 71.63 BY .91 AT ROW 22.14 COL 2
     "om man är bilförare och resmål,lönetillägg med moms mm. Kom med förslag." VIEW-AS TEXT
          SIZE 91 BY 1 AT ROW 20.86 COL 2
     "Ev. kan man ha fält för start beredskap, endagsförättning start-slut," VIEW-AS TEXT
          SIZE 91 BY 1 TOOLTIP "Hur skall det finnas direktkopplingar till andra funktioner?" AT ROW 19.59 COL 2
     RECT-36 AT ROW 16.95 COL 24.25
     RECT-32 AT ROW 1.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.63 BY 22.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db stanstid
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Stansning av tiduppgifter"
         HEIGHT             = 23.09
         WIDTH              = 96.38
         MAX-HEIGHT         = 24.59
         MAX-WIDTH          = 97.25
         VIRTUAL-HEIGHT     = 24.59
         VIRTUAL-WIDTH      = 97.25
         RESIZE             = yes
         SCROLL-BARS        = no
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_STANS FILL-IN_EFTERNAMN DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STANS
/* Query rebuild information for BROWSE BRW_STANS
     _TblList          = "Temp-Tables.stanstid"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.stanstid.PERSONALKOD
"stanstid.PERSONALKOD" "Enhet/!Sign" "x(6)" "character" ? ? ? ? ? ? yes ?
     _FldNameList[2]   > Temp-Tables.stanstid.DATUM
"stanstid.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ?
     _FldNameList[3]   > Temp-Tables.stanstid.AONR
"stanstid.AONR" "Aonr" "X(8)" "character" ? ? ? ? ? ? yes ?
     _FldNameList[4]   > Temp-Tables.stanstid.DELNR
"stanstid.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? 11 yes ?
     _FldNameList[5]   > Temp-Tables.stanstid.TRAKTAMENTE
"stanstid.TRAKTAMENTE" "Trakt.!zon" "9" "integer" ? ? ? ? ? 11 yes ?
     _FldNameList[6]   > Temp-Tables.stanstid.START
"stanstid.START" "Start!tid" ">9999" "decimal" ? ? ? ? ? 11 yes ?
     _FldNameList[7]   > Temp-Tables.stanstid.SLUT
"stanstid.SLUT" "Slut!tid" ">9999" "decimal" ? ? ? ? ? 11 yes ?
     _FldNameList[8]   > Temp-Tables.stanstid.OVERTIDUTTAG
"stanstid.OVERTIDUTTAG" "Öv.tid!uttag" ? "character" ? ? ? ? ? 11 yes ?
     _FldNameList[9]   > Temp-Tables.stanstid.LONTILLAGG
"stanstid.LONTILLAGG" "Lart" "x(5)" "character" ? ? ? ? ? ? yes ?
     _FldNameList[10]   > Temp-Tables.stanstid.LONTILLANTAL
"stanstid.LONTILLANTAL" "Antal" ">>>>>>9.<<" "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[11]   > Temp-Tables.stanstid.FELTXT
"stanstid.FELTXT" "Feltext" "X(68)" "character" ? ? 11 ? ? 11 no ?
     _Query            is OPENED
*/  /* BROWSE BRW_STANS */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Stansning av tiduppgifter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Stansning av tiduppgifter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STANS
&Scoped-define SELF-NAME BRW_STANS
&Scoped-define SELF-NAME stanstid.PERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.PERSONALKOD BRW_STANS _BROWSE-COLUMN C-Win
ON ANY-KEY OF stanstid.PERSONALKOD IN BROWSE BRW_STANS /* Enhet/!Sign */
DO:
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      APPLY "ENTRY" TO stanstid.DATUM IN BROWSE BRW_STANS.      
      APPLY "ENDKEY" TO stanstid.DATUM IN BROWSE BRW_STANS.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.PERSONALKOD BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.PERSONALKOD IN BROWSE BRW_STANS /* Enhet/!Sign */
DO:
   RUN visa_UI.  
   IF stanstid.FELNR NE 1 THEN DO:      
      IF stanstid.FELNR NE 0 THEN DO:
         RUN felnr_UI.
         APPLY "LEAVE" TO stanstid.PERSONALKOD IN BROWSE BRW_STANS.
      END.
   END.      
   RUN perskoll_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.PERSONALKOD BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.PERSONALKOD IN BROWSE BRW_STANS /* Enhet/!Sign */
DO:
   stanstid.PERSONALKOD = INPUT BROWSE BRW_STANS stanstid.PERSONALKOD.
   DISPLAY stanstid.PERSONALKOD WITH BROWSE BRW_STANS.
   RUN perskoll_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.DATUM BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.DATUM IN BROWSE BRW_STANS /* Datum */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.DATUM BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.DATUM IN BROWSE BRW_STANS /* Datum */
DO:
   stanstid.DATUM = INPUT BROWSE BRW_STANS stanstid.DATUM.
   DISPLAY stanstid.DATUM WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.AONR BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.AONR IN BROWSE BRW_STANS /* Aonr */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.AONR BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.AONR IN BROWSE BRW_STANS /* Aonr */
DO:
   stanstid.AONR = INPUT BROWSE BRW_STANS stanstid.AONR.
   DISPLAY stanstid.AONR WITH BROWSE BRW_STANS.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.DELNR BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.DELNR IN BROWSE BRW_STANS /* Delnr */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.DELNR BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.DELNR IN BROWSE BRW_STANS /* Delnr */
DO:
   stanstid.DELNR = INPUT BROWSE BRW_STANS stanstid.DELNR.
   DISPLAY stanstid.DELNR WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.TRAKTAMENTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.TRAKTAMENTE BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.TRAKTAMENTE IN BROWSE BRW_STANS /* Trakt.!zon */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.TRAKTAMENTE BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.TRAKTAMENTE IN BROWSE BRW_STANS /* Trakt.!zon */
DO:
   stanstid.TRAKTAMENTE = INPUT BROWSE BRW_STANS stanstid.TRAKTAMENTE.
   DISPLAY stanstid.TRAKTAMENTE WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.START BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.START IN BROWSE BRW_STANS /* Start!tid */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.START BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.START IN BROWSE BRW_STANS /* Start!tid */
DO:
   stanstid.START = INPUT BROWSE BRW_STANS stanstid.START.
   DISPLAY stanstid.START WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.SLUT BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.SLUT IN BROWSE BRW_STANS /* Slut!tid */
DO:
   RUN visa_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.SLUT BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.SLUT IN BROWSE BRW_STANS /* Slut!tid */
DO:
   stanstid.SLUT = INPUT BROWSE BRW_STANS stanstid.SLUT.
   DISPLAY stanstid.SLUT WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.OVERTIDUTTAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.OVERTIDUTTAG BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.OVERTIDUTTAG IN BROWSE BRW_STANS /* Öv.tid!uttag */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.OVERTIDUTTAG BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.OVERTIDUTTAG IN BROWSE BRW_STANS /* Öv.tid!uttag */
DO:
   stanstid.OVERTIDUTTAG = INPUT BROWSE BRW_STANS stanstid.OVERTIDUTTAG.
   DISPLAY stanstid.OVERTIDUTTAG WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.LONTILLAGG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.LONTILLAGG BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.LONTILLAGG IN BROWSE BRW_STANS /* Lart */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.LONTILLAGG BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.LONTILLAGG IN BROWSE BRW_STANS /* Lart */
DO:
   stanstid.LONTILLAGG = INPUT BROWSE BRW_STANS stanstid.LONTILLAGG.
   DISPLAY stanstid.LONTILLAGG WITH BROWSE BRW_STANS. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stanstid.LONTILLANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.LONTILLANTAL BRW_STANS _BROWSE-COLUMN C-Win
ON ENDKEY OF stanstid.LONTILLANTAL IN BROWSE BRW_STANS /* Antal */
DO:
   APPLY "ENTRY" TO stanstid.PERSONALKOD IN BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.LONTILLANTAL BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF stanstid.LONTILLANTAL IN BROWSE BRW_STANS /* Antal */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stanstid.LONTILLANTAL BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF stanstid.LONTILLANTAL IN BROWSE BRW_STANS /* Antal */
DO:
   stanstid.LONTILLANTAL = INPUT BROWSE BRW_STANS stanstid.LONTILLANTAL.
   DISPLAY stanstid.LONTILLANTAL WITH BROWSE BRW_STANS.
   
   IF stanstid.FELTXT = "" THEN DO:
      IF stanstid.PERSONALKOD = "" AND kollraknare = FALSE THEN DO:
         DELETE stanstid.
         FIND FIRST stanstid NO-ERROR.
         IF NOT AVAILABLE stanstid THEN DO:
            kollraknare = TRUE.
            RUN ngnkey_UI.
            APPLY "ENDKEY" TO stanstid.LONTILLANTAL IN BROWSE BRW_STANS.
         END.
         ELSE status-ok = BRW_STANS:REFRESH() IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF stanstid.PERSONALKOD NE "" THEN DO: 
         kollraknare = FALSE.
         RUN ngnkey_UI.   
         APPLY "ENDKEY" TO stanstid.LONTILLANTAL IN BROWSE BRW_STANS.
      END.  
      ELSE DO:
         APPLY "ENDKEY" TO stanstid.LONTILLANTAL IN BROWSE BRW_STANS.
      END.
   END.  
   ELSE IF stanstid.PERSONALKOD = "" THEN DO:
      DELETE stanstid.
      FIND FIRST stanstid NO-ERROR.
      IF NOT AVAILABLE stanstid THEN DO:
         kollraknare = TRUE.
         RUN ngnkey_UI.
         APPLY "ENDKEY" TO stanstid.LONTILLANTAL IN BROWSE BRW_STANS.
      END.
      ELSE status-ok = BRW_STANS:REFRESH() IN FRAME {&FRAME-NAME}.
   END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTROLL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTROLL C-Win
ON CHOOSE OF BTN_KONTROLL IN FRAME DEFAULT-FRAME /* Kontroll */
DO:
   {muswait.i}   
   IF AVAILABLE stanstid THEN DO:
      IF stanstid.DATUM NE ? THEN regdatum = stanstid.DATUM. 
   END.  
   bdatum = DATE(MONTH(regdatum),01,YEAR(regdatum)).
   regdatum = bdatum.
   IF MONTH(regdatum) = 12 THEN avdatum = DATE(12,31,YEAR(regdatum)).
   avdatum = DATE(MONTH(regdatum) + 1,01,YEAR(regdatum)) - 1.
   FOR EACH tidpers:
      DELETE tidpers.
   END.
   pkod = "".
   FOR EACH stanstid:
      IF pkod NE stanstid.PERSONALKOD THEN DO:         
         IF stanstid.PERSONALKOD NE "" THEN DO:
            pkod = stanstid.PERSONALKOD.
            FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = stanstid.PERSONALKOD AND PERSONALTAB.AKTIV = TRUE
            NO-LOCK NO-ERROR.
            IF AVAILABLE PERSONALTAB THEN RUN skapapers_UI.
         END.
      END.
   END.     
   FIND FIRST tidpers NO-LOCK NO-ERROR.
   {AVBGOM.I}
   IF AVAILABLE tidpers THEN RUN KTRLTID.W.
   {AVBFRAM.I}
   FOR EACH tidpers:
      DELETE tidpers.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG C-Win
ON CHOOSE OF BTN_REG IN FRAME DEFAULT-FRAME /* Registrera */
DO:
   {muswait.i} 
   RUN regstart_UI.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG C-Win
ON GO OF BTN_REG IN FRAME DEFAULT-FRAME /* Registrera */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG C-Win
ON LEAVE OF BTN_REG IN FRAME DEFAULT-FRAME /* Registrera */
DO:
   musz = musz.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   CREATE stanstid.
   ASSIGN
   kollraknare = TRUE
   stanstid.ORDNING = 99
   stanstid.UTRYCKNING = FALSE
  /* stanstid.DATUM = ?*/
   stanstid.OVERTIDUTTAG = "Ö"
   stanstid.START = 0000
   stanstid.SLUT = 0000.
   RUN enable_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI C-Win 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST UTRYCKNING WHERE  UTRYCKNING.KOD = ANSTFORMTAB.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_FORNAMN FILL-IN_EFTERNAMN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-36 RECT-32 BRW_STANS BTN_REG BTN_AVB BTN_KONTROLL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felnr_UI C-Win 
PROCEDURE felnr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF stanstid.FELNR = 1 THEN DO:
      APPLY "ENTRY" TO stanstid.PERSONALKOD IN BROWSE BRW_STANS.
      RETURN.
   END.   
   ELSE IF stanstid.FELNR = 2 THEN DO:
      APPLY "ENTRY" TO stanstid.DATUM IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 3 THEN DO:
      APPLY "ENTRY" TO stanstid.OVERTIDUTTAG IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 4 THEN DO:
      APPLY "ENTRY" TO stanstid.TRAKTAMENTE IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 5 THEN DO:
      APPLY "ENTRY" TO stanstid.AONR IN BROWSE BRW_STANS.      
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 6 THEN DO:
      APPLY "ENTRY" TO stanstid.LONTILLAGG IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 7 THEN DO:
      APPLY "ENTRY" TO stanstid.LONTILLANTAL IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 8 THEN DO:
      APPLY "ENTRY" TO stanstid.START IN BROWSE BRW_STANS.
      RETURN.
   END.
   ELSE IF stanstid.FELNR = 9 THEN DO:
      APPLY "ENTRY" TO stanstid.SLUT IN BROWSE BRW_STANS.
      RETURN.
   END.     
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ngnkey_UI C-Win 
PROCEDURE ngnkey_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {TRYCKS.I}   
   IF KEYFUNCTION(LASTKEY) = ("TAB") OR regbtn = TRUE THEN DO:      
      regdatum = stanstid.DATUM.
      CREATE stanstid.   
      ASSIGN
      stanstid.ORDNING = 99
      stanstid.DATUM = regdatum
      stanstid.UTRYCKNING = FALSE
      stanstid.OVERTIDUTTAG = "Ö"
      stanstid.START = 0000
      stanstid.SLUT = 0000.     
      stansrec = RECID(stanstid).
      OPEN QUERY BRW_STANS FOR EACH stanstid USE-INDEX TID NO-LOCK.            
      RUN repoao_UI (INPUT stansrec).      
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytolk_UI C-Win 
PROCEDURE nytolk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   tidtabrecspar = tidtabrec.   
   FOR EACH tidapptemp:
      DELETE tidapptemp.
   END.   
   CREATE tidapptemp.
   ASSIGN
   tidapptemp.FORETAG = globforetag
   tidapptemp.ANVANDARE = Guru.Konstanter:globanv
   tidapptemp.RECPERS = persrec
   tidapptemp.RECTID = tidtabrec
   tidapptemp.DATUM = regdatum.            
   {TIDUPPIN.I}   
   RUN FELTEXT.P.
   musz = FALSE.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE perskoll_UI C-Win 
PROCEDURE perskoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF stanstid.PERSONALKOD = FILL-IN_PERSONALKOD THEN RETURN.   
   ASSIGN
   FILL-IN_EFTERNAMN = ""
   FILL-IN_FORNAMN = ""
   FILL-IN_PERSONALKOD = "".
   IF stanstid.PERSONALKOD = "" THEN DO:
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN FILL-IN_PERSONALKOD WITH FRAME {&FRAME-NAME}.
      RETURN.
   END.   
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = stanstid.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      ASSIGN
      FILL-IN_EFTERNAMN = PERSONALTAB.EFTERNAMN
      FILL-IN_FORNAMN = PERSONALTAB.FORNAMN
      FILL-IN_PERSONALKOD = PERSONALTAB.PERSONALKOD.
   END.
   DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN FILL-IN_PERSONALKOD WITH FRAME {&FRAME-NAME}.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE regstart_UI C-Win 
PROCEDURE regstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   OPEN QUERY BRW_STANS FOR EACH stanstid WHERE stanstid.PERSONALKOD = "" USE-INDEX TID NO-LOCK.     
   GET FIRST BRW_STANS.    
   DO WHILE AVAILABLE(stanstid):                 
      DELETE stanstid.                 
      GET NEXT BRW_STANS.
   END.
   OPEN QUERY BRW_STANS FOR EACH stanstid USE-INDEX TID NO-LOCK.     
   GET FIRST BRW_STANS.    
   DO WHILE AVAILABLE(stanstid):            
      ASSIGN     
      stanstid.STARTM = stanstid.START / 100
      stanstid.SLUTM = stanstid.SLUT / 100
      stanstid.FELNR = 0
      stanstid.FELTXT = "".
      /*RUN repoao_UI (INPUT RECID(stanstid)). */    
      IF stanstid.PERSONALKOD = "" THEN DO:        
         DELETE stanstid.     
         status-ok = BRW_STANS:REFRESH() IN FRAME {&FRAME-NAME}.     
      END.
      ELSE DO:
         RUN reg_UI.
         IF stanstid.FELTXT = "" THEN DELETE stanstid.
      END.
              
      GET NEXT BRW_STANS.
   END. 
   status-ok = BRW_STANS:REFRESH() IN FRAME {&FRAME-NAME}.
   GET FIRST BRW_STANS.
   IF NOT AVAILABLE stanstid THEN DO:      
      CREATE stanstid.
      ASSIGN
      stanstid.ORDNING = 99
      stanstid.UTRYCKNING = FALSE
      stanstid.OVERTIDUTTAG = "Ö"
      stanstid.START = 0000
      stanstid.SLUT = 0000.
      OPEN QUERY BRW_STANS FOR EACH stanstid USE-INDEX TID NO-LOCK.  
      GET FIRST BRW_STANS. 
   END.   
   RUN repoao_UI (INPUT RECID(stanstid)).
   IF AVAILABLE stanstid THEN DO:
      IF stanstid.FELTXT NE "" THEN DO:
         RUN felnr_UI.
      END.
   END.
                      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reg_UI C-Win 
PROCEDURE reg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   regdatum = stanstid.DATUM
   regmnr = MONTH(stanstid.DATUM)
   regar = YEAR(stanstid.DATUM)   
   regstart = stanstid.STARTM
   regslut = stanstid.SLUTM.
        
   RUN REGVEC.P.
   RUN REGDAG.P.
   IF pkod NE stanstid.PERSONALKOD THEN DO:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = stanstid.PERSONALKOD AND 
      PERSONALTAB.AKTIV = TRUE
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PERSONALTAB THEN DO:
         pkod = "".
         ASSIGN
         stanstid.FELNR = 1 
         stanstid.FELTXT = "Enhet/Sign finns ej.".
         RETURN.
      END.
      {TSEK.I}
      IF musz =  TRUE THEN DO:
         musz = FALSE.
         ASSIGN
         stanstid.FELNR = 1
         stanstid.FELTXT = "Du är ej behörig att ändra på denna Enhet/Sign.".
         pkod = "".
         RETURN.
      END.
      persrec = RECID(PERSONALTAB).       
      RUN anst_UI.        
   END.      
   
   IF MONTH(stanstid.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(stanstid.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(stanstid.DATUM) + 1),01,YEAR(stanstid.DATUM)) - 1.
   END.
   IF DAY(stanstid.DATUM) > DAY(datkoll) THEN DO:
      ASSIGN
      stanstid.FELNR = 2
      stanstid.FELTXT = "Felaktigt angivet datum. Denna månad har bara " + STRING(DAY(datkoll)) + "dagar.".
      RETURN.
   END.
   IF DAY(stanstid.DATUM) <= 0 THEN DO:
      stanstid.FELTXT = "Felaktigt angivet datum. Datum kann ej vara mindre än 1.".             
      RETURN.
   END.               
   FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   GODKOLL.DATAR = regar AND GODKOLL.DATMAN = regmnr       
   USE-INDEX PKODAR NO-LOCK NO-ERROR.
   IF AVAILABLE GODKOLL THEN DO:
      IF GODKOLL.DATUM >= stanstid.DATUM THEN DO:
         ASSIGN
         stanstid.FELNR = 2
         stanstid.FELTXT = 
         "Felaktigt angivet datum. Tidsedeln är godkänd till och med " + STRING(GODKOLL.DATUM).
         RETURN.
      END.      
   END.
   IF stanstid.OVERTIDUTTAG = "" THEN stanstid.OVERTIDUTTAG = "Ö".                            
   IF stanstid.OVERTIDUTTAG = "Ö" THEN musz = FALSE.
   ELSE IF stanstid.OVERTIDUTTAG = "Ö" THEN musz = FALSE.
   ELSE IF stanstid.OVERTIDUTTAG = "K" THEN musz = FALSE.
   ELSE IF stanstid.OVERTIDUTTAG = "F" THEN musz = FALSE.
   ELSE IF stanstid.OVERTIDUTTAG = "I" THEN musz = FALSE.
   ELSE IF stanstid.OVERTIDUTTAG = "L" THEN musz = FALSE.
   ELSE DO:
      ASSIGN
      stanstid.FELNR = 3
      stanstid.FELTXT = 
      "Övertidsutag har felaktigt värde. Tillåtna värden är K,Ö,F,I,L.".
      RETURN.
   END.
   IF stanstid.TRAKTAMENTE = 0 THEN musz = FALSE.
   ELSE IF stanstid.TRAKTAMENTE = 1 THEN musz = FALSE.
   ELSE DO:
      ASSIGN
      stanstid.FELNR = 4
      stanstid.FELTXT = 
      "Traktamente har felaktigt värde.Tillåtna värden är 0,1".
      RETURN.
   END.
   IF stanstid.AONR NE "" THEN DO: 
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = stanstid.AONR AND 
      AONRTAB.DELNR = stanstid.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE AONRTAB THEN DO:
         ASSIGN
         stanstid.FELNR = 5
         stanstid.FELTXT = 
         "Aonr " + stanstid.AONR + " " + STRING(stanstid.DELNR,">99") + " finns inte.".                  
         RETURN.
      END.
      ELSE DO:                          
         IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR
         Guru.Konstanter:globforetag = "ETA" THEN DO:
            IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "1" THEN DO:
               IF ANSTFORMTAB.KOD BEGINS "T" THEN DO:
                  ASSIGN
                  stanstid.FELNR = 5
                  stanstid.FELTXT = 
                  "Endast kollektivanställda får skriva på detta aonr".
                  RETURN.           
               END.   
            END.
            ELSE IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "2"  THEN DO:
               IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:
                  ASSIGN
                  stanstid.FELNR = 5
                  stanstid.FELTXT = "Endast tjänstemän får skriva på detta aonr".
                  RETURN.
               END.   
            END. 
            ELSE IF SUBSTRING(STRING(AONRTAB.ELVOMRKOD,"999999999"),1,1) = "3"  THEN DO:
               ASSIGN
               stanstid.FELNR = 5
               stanstid.FELTXT = "Ingen tidskrivning på detta aonr".
               RETURN.
            END.       
         END.         
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR
         Guru.Konstanter:globforetag = "ETA" THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST oradebuff WHERE oradebuff.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE oradebuff THEN DO:
               IF OMRADETAB.AVDELNINGNR NE oradebuff.AVDELNINGNR THEN DO:
                  ASSIGN
                  stanstid.FELNR = 5
                  stanstid.FELTXT = "Du får inte använda ett annat bolags aonr".
                  RETURN.
               END.   
            END.   
         END.   

         IF AONRTAB.AONRAVDATUM = 01/01/1991 OR AONRTAB.AONRAVDATUM >= regdatum THEN musz = musz.
         ELSE DO:
            ASSIGN
            stanstid.FELNR = 5
            stanstid.FELTXT = 
            "Aonr " + stanstid.AONR + " " + STRING(stanstid.DELNR,">99") + " är redan avslutat.".                      
            RETURN.
         END.
      END.         
   END.
   ELSE DO:
      IF stanstid.LONTILLAG = "" THEN DO:
         ASSIGN
         stanstid.FELNR = 5
         stanstid.FELTXT = "Aonr kan inte vara blankt".
         RETURN.      
      END.      
   END.
   IF stanstid.LONTILLAG NE "" THEN DO:
      FIND FIRST LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND 
      LONTILL.VILART = stanstid.LONTILLAG USE-INDEX VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LONTILL THEN DO:
         ASSIGN
         stanstid.FELNR = 6
         stanstid.FELTXT = "Detta lönetillägg finns ej.".                      
         RETURN.
      END.
      IF LONTILL.VALBAR = FALSE THEN DO:
         stanstid.FELTXT = "Lönetillägg " + stanstid.LONTILLAG + " går ej att använda.".         
         RETURN.
      END.
      IF LONTILL.AONROBL = TRUE THEN DO:
         IF stanstid.AONR = "" THEN DO:    
            ASSIGN
            stanstid.FELNR = 5
            stanstid.FELTXT = 
            "Lönetillägg " + stanstid.LONTILLAG + " kräver arbetsordernummer.".               
            RETURN.
         END.
      END.    
      IF LONTILL.AONROBL = ? THEN DO:
         IF stanstid.AONR = "" AND stanstid.DELNR = 0 THEN musz = musz.
         ELSE DO:    
            ASSIGN
            stanstid.FELNR = 5 
            stanstid.FELTXT = 
            "Vid lönetillägg " + stanstid.LONTILLAG + " krävs att arbetsordernummer är blankt.". 
            RETURN.
         END.
      END.   
      IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" 
      OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         IF AVAILABLE AONRTAB THEN DO: 
            IF AONRTAB.PRISTYP = "FRÅNVARO." THEN DO:
               ASSIGN
               stanstid.FELNR = 5
               stanstid.FELTXT = "Lönetillägg kan inte registreras på frånvaro aonr.".             
               RETURN.
            END.
         END.
      END.         
      IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ETA" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA"
      OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         IF stanstid.LONTILLANTAL > 999 AND          
            (LONTILL.ENHET = "ST" OR LONTILL.ENHET = "MI" OR
            LONTILL.ENHET = "KM" OR LONTILL.ENHET = "TI" ) THEN DO:
            ASSIGN
            stanstid.FELNR = 7
            stanstid.FELTXT = "Antal kan inte vara större än 999.".
            RETURN.
         END.
      END.             
   END.    
   IF stanstid.STARTM = stanstid.SLUTM AND stanstid.LONTILLAG NE "" THEN musz = musz.
   ELSE DO:
      IF stanstid.STARTM = stanstid.SLUTM THEN DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Start och slut kan inte vara lika.".
         RETURN.
      END.
      IF stanstid.STARTM > 24.00 OR stanstid.STARTM < 0 THEN DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Orimligt klockslag. För starttiden.".             
         RETURN.
      END.      
      IF SUBSTRING(STRING(stanstid.STARTM),3 ,2) > "59" THEN DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Orimligt klockslag. För starttiden.".             
         RETURN.
      END.
      IF stanstid.SLUTM > 24.00 THEN DO:
         ASSIGN
         stanstid.FELNR = 9
         stanstid.FELTXT = "Orimligt klockslag. För sluttiden.".
         RETURN.
      END. 
      IF SUBSTRING(STRING(stanstid.SLUTM),3 ,2) > "59" THEN DO:
         ASSIGN
         stanstid.FELNR = 9
         stanstid.FELTXT = "Orimligt klockslag. För sluttiden.".
         RETURN.
      END.    
      IF stanstid.STARTM > stanstid.SLUTM THEN DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Start kan inte vara större än slut.".         
         RETURN.
      END.
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START LE regstart AND
      TIDREGITAB.SLUT > regstart AND TIDREGITAB.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Det finns redan en registrering med start " + 
         STRING(TIDREGITAB.START) + " och slut " + STRING(TIDREGITAB.SLUT) + ".".  
         RETURN.
      END.
      FIND FIRST TIDREGITAB WHERE 
      TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START < regslut AND
      TIDREGITAB.SLUT >= regslut AND TIDREGITAB.TIDLOG = TRUE 
      USE-INDEX PSTART NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE TIDREGITAB THEN DO: 
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START > regstart AND
         TIDREGITAB.SLUT < regslut AND TIDREGITAB.TIDLOG = TRUE 
         USE-INDEX PSTART NO-LOCK NO-ERROR.        
         IF NOT AVAILABLE TIDREGITAB THEN DO:    
            musz = musz.
         END.  
         ELSE DO:
            ASSIGN
            stanstid.FELNR = 8
            stanstid.FELTXT = "Det finns redan en registrering med start " + 
            STRING(TIDREGITAB.START) + " och slut " + STRING(TIDREGITAB.SLUT) + ".".  
            RETURN.
         END.                  
      END.  
      ELSE DO:
         ASSIGN
         stanstid.FELNR = 8
         stanstid.FELTXT = "Det finns redan en registrering med start " + 
         STRING(TIDREGITAB.START) + " och slut " + STRING(TIDREGITAB.SLUT) + ".".  
         RETURN.
      END.             
      IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "GADM"
        OR Guru.Konstanter:globforetag = "ELPA"
       THEN DO:
         IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:
            OPEN QUERY btidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONAL = PERSONALTAB.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum NO-LOCK.
            GET FIRST btidq NO-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB):
               IF stanstid.STARTM GE TIDREGITAB.BEREDSKAPSTART AND 
               stanstid.STARTM < TIDREGITAB.BEREDSKAPSLUT THEN DO: 
                  ASSIGN stanstid.UTRYCKNING = TRUE.            
               END.    
               GET NEXT btidq NO-LOCK.
            END.
         END.   
      END.   
      IF AVAILABLE AONRTAB THEN DO:          
         IF AVAILABLE UTRYCKNING THEN DO:       
            IF UTRYCKNING.HALV = TRUE THEN DO: 
               FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX
               PERSONALKOD NO-LOCK NO-ERROR.
               IF AVAILABLE FLEXAVT AND FLEXAVT.FLEXTID = TRUE AND stanstid.OVERTIDUTTAG = "F" THEN musz = musz.                         
               ELSE DO:                           
                  RUN SLUTARB.P.          
                  IF AONRTAB.PRISTYP = "FRÅNVARO." THEN DO:
                     IF stanstid.STARTM GE regslut OR stanstid.SLUTM LE regstart OR 
                     stanstid.SLUTM > regslut OR stanstid.STARTM < regstart THEN DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Övertid kan inte registreras på frånvaro.".             
                        RETURN.
                     END.
                  END. 
                  IF stanstid.STARTM < regstart AND stanstid.SLUTM > regslut THEN DO:
                     ASSIGN nytid = stanstid.STARTM.
                     RUN TIMSEK.P.
                     ASSIGN overant = sekunder
                     nytid = regstart.
                     RUN TIMSEK.P.
                     ASSIGN overant = sekunder - overant
                     nytid = stanstid.SLUTM.
                     RUN TIMSEK.P.
                     ASSIGN seku = sekunder
                     nytid = regslut.
                     RUN TIMSEK.P.
                     overant = overant + seku - sekunder.            
                  END.
                  ELSE DO:
                     IF stanstid.SLUTM > regslut AND  stanstid.STARTM < regslut THEN nytid = regslut.
                     ELSE nytid = stanstid.STARTM.
                     RUN TIMSEK.P.
                     overant = sekunder. 
                     IF stanstid.STARTM < regstart AND  stanstid.SLUTM > regstart THEN nytid = regstart.
                     ELSE nytid = stanstid.SLUTM.
                     RUN TIMSEK.P.         
                     ASSIGN 
                     overant = sekunder - overant.
                  END.
                  halvkvart = 1800.
                  IF Guru.Konstanter:globforetag = "VATT" THEN halvkvart = 900.
                     ASSIGN   
                     otim = TRUNCATE(overant / halvkvart ,0)
                     otim2 = overant - (otim * halvkvart).
                     IF stanstid.UTRYCKNING = TRUE AND overant > UTRYCKNING.UTRYCKNBER AND
                     otim2 > 0 AND stanstid.SLUTM > regslut THEN DO:
                     IF Guru.Konstanter:globforetag = "VATT" THEN DO: 
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna kvartar får registreras.".
                        RETURN.
                     END.                     
                     ELSE DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna halvtimmar får registreras.".
                        RETURN.
                     END.                        
                  END.   
                  IF stanstid.UTRYCKNING = TRUE AND overant > UTRYCKNING.UTRYCKNBER AND
                  otim2 > 0 AND stanstid.STARTM < regstart THEN DO:
                     IF Guru.Konstanter:globforetag = "VATT" THEN DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna kvartar får registreras.".
                        RETURN.
                     END.                     
                     ELSE DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna halvtimmar får registreras.".
                        RETURN.
                     END.
                  END.
                  IF stanstid.UTRYCKNING = FALSE AND otim2 > 0 AND stanstid.SLUTM > regslut THEN DO:
                     IF Guru.Konstanter:globforetag = "VATT" THEN DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna kvartar får registreras.".
                        RETURN.
                     END.                     
                     ELSE DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna halvtimmar får registreras.".
                        RETURN.
                     END.
                  END.
                  IF stanstid.UTRYCKNING = FALSE AND otim2 > 0 AND stanstid.STARTM < regstart THEN DO:
                     IF Guru.Konstanter:globforetag = "VATT" THEN DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna kvartar får registreras.".
                        RETURN.
                     END.                     
                     ELSE DO:
                        ASSIGN
                        stanstid.FELNR = 8
                        stanstid.FELTXT = "Endast jämna halvtimmar får registreras.".
                        RETURN.
                     END.
                  END.             
               END.   
            END.
         END. 
         FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = stanstid.PERSONALKOD AND
         TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP 
         USE-INDEX PRISPERS NO-LOCK NO-ERROR.          
         ASSIGN
         stanstid.PRISTYP = TIMKOSTNADSTAB.PRISTYP.
         stanstid.PRIS = TIMKOSTNADSTAB.PRISA.
      END.
   END.          
   IF stanstid.LONTILLAG NE "" THEN DO TRANSACTION:
      CREATE TIDREGITAB.
      ASSIGN
      TIDREGITAB.TIDLOG = FALSE
      TIDREGITAB.DATUM = stanstid.DATUM
      TIDREGITAB.AONR = stanstid.AONR 
      TIDREGITAB.DELNR = stanstid.DELNR
      TIDREGITAB.PROGRAM = "STANSTID" + STRING(TODAY) + globanv
      TIDREGITAB.PERSONALKOD = stanstid.PERSONALKOD  
      TIDREGITAB.DAG = regdagnamn
      TIDREGITAB.VECKONUMMER = regvnr
      TIDREGITAB.SLUT = 7.00
      TIDREGITAB.START = 7.00 
      TIDREGITAB.LONTILLAGG = LONTILL.LONTILLAGG
      TIDREGITAB.LONTILLANTAL = stanstid.LONTILLANTAL  
      TIDREGITAB.LONAUTO = FALSE.                       
      
      /*TIDREGITAB.RESMAL = FILL-IN-RESMAL            
 *       IF FILL-IN-MOMS > 0 THEN DO:
 *          FIND FIRST LONTILL WHERE LONTILL.LONKODTEXT = 'MOMS' NO-LOCK NO-ERROR.
 *          IF AVAILABLE LONTILL THEN DO:
 *             CREATE tidbuff.
 *             ASSIGN
 *             tidbuff.DATUM = TIDREGITAB.DATUM 
 *             tidbuff.AONR = TIDREGITAB.AONR  
 *             tidbuff.DELNR = TIDREGITAB.DELNR 
 *             tidbuff.PROGRAM = TIDREGITAB.PROGRAM 
 *             tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD   
 *             tidbuff.DAG = TIDREGITAB.DAG 
 *             tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
 *             tidbuff.TIDLOG = FALSE
 *             tidbuff.SLUT = 7.00
 *             tidbuff.START = 7.00 
 *             tidbuff.LONTILLAGG = LONTILL.LONTILLAGG
 *             tidbuff.LONTILLANTAL = FILL-IN-MOMS  
 *             tidbuff.RESMAL = FILL-IN-RESMAL
 *             tidbuff.LONAUTO = FALSE.                
 *          END.  
 *       END.*/   
   END.

   OPEN QUERY tq FOR EACH TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONAUTO = TRUE 
   USE-INDEX PSTART NO-LOCK.
   DO TRANSACTION:
      GET FIRST tq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):     
         FIND FIRST LONKORT WHERE LONKORT.LONTILLAGG = LONTILL.LONTILLAGG AND
         LONKORT.KORTLON = TIDREGITAB.LONTILLAGG USE-INDEX LONKORT NO-LOCK NO-ERROR.
         IF NOT AVAILABLE LONKORT THEN persrec = persrec.
         ELSE DO:
            ASSIGN TIDREGITAB.LONTILLAGG = " " TIDREGITAB.LONTILLANTAL = 0
            TIDREGITAB.LONAUTO = FALSE.
         END.
         GET NEXT tq EXCLUSIVE-LOCK.
      END.        
   END.   
   IF stanstid.STARTM NE stanstid.SLUTM THEN DO:
      DO TRANSACTION:
         ASSIGN
         bustart3 = stanstid.STARTM.
         CREATE TIDREGITAB.               
         tidtabrec = RECID(TIDREGITAB).
         ASSIGN 
         TIDREGITAB.PROGRAM = "STANSTID" + STRING(TODAY) + globanv
         TIDREGITAB.PERSONALKOD = stanstid.PERSONALKOD      
         TIDREGITAB.DAG = regdagnamn
         TIDREGITAB.VECKONUMMER = regvnr
         TIDREGITAB.SLUT = stanstid.SLUTM
         TIDREGITAB.START = stanstid.STARTM 
         TIDREGITAB.TRAKTAMENTE = stanstid.TRAKTAMENTE
         TIDREGITAB.OVERTIDUTTAG = stanstid.OVERTIDUTTAG 
         TIDREGITAB.UTRYCKNING = stanstid.UTRYCKNING
         /*
         TIDREGITAB.NODF = FILL-IN_NODF
         */
         TIDREGITAB.PRISTYP = stanstid.PRISTYP
         TIDREGITAB.PRIS = stanstid.PRIS
         TIDREGITAB.AONR = stanstid.AONR
         TIDREGITAB.DELNR = stanstid.DELNR               
         TIDREGITAB.DATUM = stanstid.DATUM.
         ASSIGN      
         nytid = TIDREGITAB.START.
         RUN TIMSEK.P.
         regstartsek = sekunder.
         nytid = TIDREGITAB.SLUT.
         RUN TIMSEK.P.
         regslutsek = sekunder.
         regdatum = TIDREGITAB.DATUM.
         RUN TOTTID.P.                        
         ASSIGN TIDREGITAB.TOTALT = nytid.        
      END.                   
      RELEASE TIDREGITAB NO-ERROR.            
      RUN nytolk_UI.          
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repoao_UI C-Win 
PROCEDURE repoao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   BRW_STANS:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.                        
   REPOSITION BRW_STANS TO RECID browrec.
   status-ok = BRW_STANS:SELECT-FOCUSED-ROW().                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapapers_UI C-Win 
PROCEDURE skapapers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
  musz = FALSE.
  DO TRANSACTION: 
      {TSEK.I}
      IF musz =  TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.
      CREATE tidpers.
      ASSIGN 
      tidpers.ANSVARIGTIDR = PERSONALTAB.ANSVARIGTIDR
      tidpers.OMRADE = PERSONALTAB.OMRADE
      tidpers.TIDPERSREC = RECID(PERSONALTAB)
      tidpers.PERSONALKOD = PERSONALTAB.PERSONALKOD
      tidpers.EFTERNAMN = PERSONALTAB.EFTERNAMN   
      tidpers.FORNAMN = PERSONALTAB.FORNAMN.
      RELEASE tidpers. 
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISPLAY
   stanstid.AONR stanstid.DATUM stanstid.DELNR stanstid.LONTILLAGG stanstid.LONTILLANTAL 
   stanstid.OVERTIDUTTAG stanstid.PERSONALKOD stanstid.SLUT stanstid.START stanstid.TRAKTAMENTE 
   WITH WITH BROWSE BRW_STANS.
   IF stanstid.PERSONALKOD NE "" THEN stanstid.ORDNING = 0.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


