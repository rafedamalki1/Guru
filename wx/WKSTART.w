&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WWSTART



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WWSTART 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/06 - 10:23 am

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
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
&Scoped-define NEW NEW
{FORETEMP.I}
   CREATE foretemp.
   
   
   DEFINE NEW SHARED VARIABLE alltidmax AS LOGICAL NO-UNDO.
                      ASSIGN
   globstorh = 682
   globstorb = 1000.
DEFINE {&NEW} SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globanvavdnr AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globanvpkod AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallm AS LOGICAL NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globniv AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallpers AS LOGICAL NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globallao AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globavd AS INTEGER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globomr AS CHARACTER NO-UNDO. 
DEFINE {&NEW} SHARED VARIABLE globlos AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globpersnamn AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globsidl AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE globsids AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE Guru.GlobalaVariabler:plusaonr AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE Guru.GlobalaVariabler:plusdnr AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusdval AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE succelval AS LOGICAL NO-UNDO. 
DEFINE VARIABLE Guru.GlobalaVariabler:collefth AS HANDLE NO-UNDO.
DEFINE VARIABLE Guru.GlobalaVariabler:colrighth AS HANDLE NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     
  
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".

DEFINE VARIABLE tillochmeddatum AS DATE NO-UNDO.
DEFINE TEMP-TABLE felinut
   FIELD ANVANDARE AS CHARACTER
   FIELD DATUM    AS DATE
   FIELD EMOTAGET AS LOGICAL
   FIELD FELKOD   AS INTEGER
   FIELD FELTEXT  AS CHARACTER
   FIELD PROGRAM  AS CHARACTER
   INDEX FELTEXT IS PRIMARY ANVANDARE FELKOD.
DEFINE {&NEW} SHARED TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
/*
  INDEX VAL IS PRIMARY VAL.
  */
DEFINE {&NEW} SHARED TEMP-TABLE xsektemp
  FIELD AV-LEVEL AS INTEGER
  FIELD MENYVART AS CHARACTER
  FIELD SEK AS LOGICAL EXTENT 20
  INDEX XSEK IS PRIMARY MENYVART AV-LEVEL.

{FRAMSIZETEMP.I}
/*EGENBVAR.I*/
DEFINE {&NEW} SHARED VARIABLE gavdl AS CHARACTER NO-UNDO. /*AVDELNING LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gavdk AS CHARACTER NO-UNDO.     /*AVDELNING KORT*/
DEFINE {&NEW} SHARED VARIABLE gomrl AS CHARACTER NO-UNDO.  /*OMRÅDE LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gomrk AS CHARACTER NO-UNDO.      /*OMRÅDE KORT*/
DEFINE {&NEW} SHARED VARIABLEgaol AS CHARACTER NO-UNDO.   /*AONR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgaok AS CHARACTER NO-UNDO.       /*AONR KORT*/   
DEFINE {&NEW} SHARED VARIABLEgpll AS CHARACTER NO-UNDO.  /*PLANNR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgplk AS CHARACTER NO-UNDO.     /*PLANNR KORT*/
DEFINE {&NEW} SHARED VARIABLEgenl AS CHARACTER NO-UNDO. /*E-NUMMER LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgenk AS CHARACTER NO-UNDO.  /*E-NUMMER KORT*/
DEFINE {&NEW} SHARED VARIABLE gjul AS CHARACTER NO-UNDO. /*JURIDISK LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgjuk AS CHARACTER NO-UNDO.  /*JURIDISK KORT*/
DEFINE {&NEW} SHARED VARIABLEgfastl AS CHARACTER NO-UNDO. /*FASTA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gfastk AS CHARACTER NO-UNDO.     /*FASTA KORT*/
DEFINE {&NEW} SHARED VARIABLE gtilll AS CHARACTER NO-UNDO. /*TILLFÄLIGA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtillk AS CHARACTER NO-UNDO.     /*TILLFÄLIGA KORT*/
DEFINE {&NEW} SHARED VARIABLEgberel AS CHARACTER NO-UNDO. /*BEREDARE LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgberek AS CHARACTER NO-UNDO.     /*BEREDARE KORT*/
DEFINE {&NEW} SHARED VARIABLE gprojl AS CHARACTER NO-UNDO. /*PROJEKTÖR LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gprojk AS CHARACTER NO-UNDO.     /*PROJEKTÖR KORT*/
DEFINE {&NEW} SHARED VARIABLE garbal AS CHARACTER NO-UNDO. /*ARBETSANSVARIG LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE garbak AS CHARACTER NO-UNDO.     /*ARBETSANSVARIG KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfk AS CHARACTER NO-UNDO.     /*Utförande KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfl AS CHARACTER NO-UNDO. /*Utförande LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gbestk AS CHARACTER NO-UNDO.     /*Beställare/Kund KORT*/
DEFINE {&NEW} SHARED VARIABLE gbestl AS CHARACTER NO-UNDO. /*Beställare/Kund LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gdebk AS CHARACTER NO-UNDO.     /*Debiterigstyp KORT*/
DEFINE {&NEW} SHARED VARIABLE gdebl AS CHARACTER NO-UNDO.  /*Debiterigstyp LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtidlk AS CHARACTER NO-UNDO.     /*tidläge KORT*/
DEFINE {&NEW} SHARED VARIABLE gtidll AS CHARACTER NO-UNDO.  /*tidläge LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gutfardk AS CHARACTER NO-UNDO.     /*utfärdat av KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfardl AS CHARACTER NO-UNDO.  /*utfärdat av LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE grefbefk AS CHARACTER NO-UNDO.     /*Ref.nr beställare KORT*/
DEFINE {&NEW} SHARED VARIABLE grefbefl AS CHARACTER NO-UNDO.  /*Ref.nr beställare  LÅNG*/ 



   
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
/*
     Filename: KALKTEMP3.I
      Created: 2004.03.09 09:51ELPAO     
     Modified: 
*/

DEFINE {&NEW} {&SHARED} TEMP-TABLE kalk_temp NO-UNDO
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD F1 AS DECIMAL 
   FIELD F2 AS DECIMAL 
   FIELD F3 AS DECIMAL 
   FIELD F4 AS DECIMAL 
   FIELD F5 AS DECIMAL 
   FIELD F6 AS DECIMAL 
   FIELD F7 AS DECIMAL 
   FIELD F8 AS DECIMAL 
   FIELD F9 AS DECIMAL 
   FIELD F10 AS DECIMAL
   FIELD EA AS DECIMAL
   FIELD ARBETE AS DECIMAL
   FIELD MATERIEL AS DECIMAL
   FIELD MASKINKOST AS DECIMAL
   FIELD OVRIGT AS DECIMAL
   FIELD ENTRP AS DECIMAL
   FIELD ANTAL AS DECIMAL
   FIELD UTRUST AS DECIMAL
   FIELD UTRUSTKOST AS DECIMAL
   FIELD MASKINTIMMAR AS DECIMAL
   INDEX KOD ARBKOD LOPNR ASCENDING.

DEFINE TEMP-TABLE efastkalktemp NO-UNDO  LIKE kalk_temp
   FIELD SUMMA AS DECIMAL.


DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.


                              


DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.



   DEFINE VARIABLE hjwebbvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-KALK
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES efastkalktemp tidut

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK efastkalktemp.ARBKOD ~
efastkalktemp.LOPNR efastkalktemp.BENAMNING efastkalktemp.ANTAL ~
efastkalktemp.ENHET efastkalktemp.F1 efastkalktemp.F2 ~
efastkalktemp.MASKINTIMMAR efastkalktemp.UTRUST efastkalktemp.EA ~
efastkalktemp.ARBETE efastkalktemp.MATERIEL efastkalktemp.MASKINKOST ~
efastkalktemp.UTRUSTKOST efastkalktemp.OVRIGT efastkalktemp.SUMMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK efastkalktemp.ARBKOD ~
efastkalktemp.LOPNR efastkalktemp.BENAMNING efastkalktemp.ANTAL ~
efastkalktemp.F1 efastkalktemp.F2 efastkalktemp.MASKINTIMMAR ~
efastkalktemp.UTRUST efastkalktemp.EA efastkalktemp.ARBETE ~
efastkalktemp.MATERIEL efastkalktemp.MASKINKOST efastkalktemp.UTRUSTKOST ~
efastkalktemp.OVRIGT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH efastkalktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH efastkalktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK efastkalktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK efastkalktemp


/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-KALK                                     */

/* Definitions for FRAME FRAME-TIDUT                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDUT ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF ~
FILL-IN-ARBF FILL-IN-OVRF BRW_KALK BTN_NY BTN_BORT FILL-IN-TOTSUM 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF ~
FILL-IN-ARBF FILL-IN-OVRF FILL-IN-TOTSUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WWSTART AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BORT 
     LABEL "tA BORT" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_NY 
     LABEL "NY" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FILL-IN-ARBF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Arbetskostnad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASKF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Maskin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MTRLF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Materiel" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVRF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Övrigt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTSUM AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 0 
     LABEL "Total summa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRF AS DECIMAL FORMAT "->>>>>>>9.99":U INITIAL 1 
     LABEL "Faktor Utrustning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i excel":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_HTML 
     LABEL "Visa i IE":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_PDF 
     LABEL "Visa i PDF":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.

DEFINE VARIABLE RAD_KALK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Kalkyl", 1,
"Ändra kalkyl", 2
     SIZE 27.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 119.75 BY .5
     BGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      efastkalktemp SCROLLING.

DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK WWSTART _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      efastkalktemp.ARBKOD FORMAT "X(5)":U
      efastkalktemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      efastkalktemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(20)":U
            WIDTH 10
      efastkalktemp.ANTAL FORMAT "->>>>9.99":U
      efastkalktemp.ENHET FORMAT "X(3)":U
      efastkalktemp.F1 COLUMN-LABEL "F1!TEST" FORMAT ">>>9.99":U
      efastkalktemp.F2 COLUMN-LABEL "F2!TEST" FORMAT ">>>9.99":U
      efastkalktemp.MASKINTIMMAR COLUMN-LABEL "MASKIN!TIMMAR" FORMAT "->>>>9.99":U
      efastkalktemp.UTRUST COLUMN-LABEL "Utrustning!TEST" FORMAT ">>>9.99":U
      efastkalktemp.EA FORMAT ">>>9.99":U
      efastkalktemp.ARBETE COLUMN-LABEL "Arbete!TEST" FORMAT "->>>>9.99":U
      efastkalktemp.MATERIEL COLUMN-LABEL "Materiel!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.MASKINKOST COLUMN-LABEL "Maskinkost!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.UTRUSTKOST COLUMN-LABEL "Utrustnings!kostnad" FORMAT "->>>>>>9.99":U
      efastkalktemp.OVRIGT COLUMN-LABEL "Övrigt!TEST" FORMAT "->>>>>>9.99":U
      efastkalktemp.SUMMA FORMAT "->>>>>>>>9.99":U
  ENABLE
      efastkalktemp.ARBKOD
      efastkalktemp.LOPNR
      efastkalktemp.BENAMNING
      efastkalktemp.ANTAL
      efastkalktemp.F1
      efastkalktemp.F2
      efastkalktemp.MASKINTIMMAR
      efastkalktemp.UTRUST
      efastkalktemp.EA
      efastkalktemp.ARBETE
      efastkalktemp.MATERIEL
      efastkalktemp.MASKINKOST
      efastkalktemp.UTRUSTKOST
      efastkalktemp.OVRIGT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 119.5 BY 16.75 EXPANDABLE.

DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WWSTART _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 119.5 BY 26.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     RAD_KALK AT ROW 1 COL 76 NO-LABEL
     BTN_SKRIV AT ROW 1.08 COL 16
     BTN_EXCEL AT ROW 1.08 COL 28
     BTN_HTML AT ROW 1.08 COL 40
     BTN_PDF AT ROW 1.08 COL 52
     BTN_AVB AT ROW 1.08 COL 108.75
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.08 COL 1
          FGCOLOR 1 FONT 17
     RECT-57 AT ROW 2.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.

DEFINE FRAME FRAME-KALK
     FILL-IN-MASKF AT ROW 1.5 COL 23.25 COLON-ALIGNED
     FILL-IN-MTRLF AT ROW 1.5 COL 44.75
     FILL-IN-UTRF AT ROW 1.5 COL 81.25
     FILL-IN-ARBF AT ROW 3.5 COL 3.25
     FILL-IN-OVRF AT ROW 3.5 COL 59.75 COLON-ALIGNED
     BRW_KALK AT ROW 5.25 COL 1
     BTN_NY AT ROW 22.5 COL 44
     BTN_BORT AT ROW 22.5 COL 63.5
     FILL-IN-TOTSUM AT ROW 24.5 COL 57.5 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.58
         SIZE 124.5 BY 26.67.

DEFINE FRAME FRAME-TIDUT
     BRW_UT AT ROW 1.04 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.58
         SIZE 124.5 BY 26.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: efastkalktemp T "?" NO-UNDO temp-db efastkalktemp
      TABLE: fastktemp T "?" NO-UNDO temp-db fastktemp
      TABLE: tidut T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WWSTART ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU"
         COLUMN             = 4.5
         ROW                = 2.04
         HEIGHT             = 28.88
         WIDTH              = 118.63
         MAX-HEIGHT         = 28.88
         MAX-WIDTH          = 119.75
         VIRTUAL-HEIGHT     = 28.88
         VIRTUAL-WIDTH      = 119.75
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 16
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WWSTART
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KALK:FRAME = FRAME FRAME-VINST:HANDLE
       FRAME FRAME-TIDUT:FRAME = FRAME FRAME-VINST:HANDLE.

/* SETTINGS FOR FRAME FRAME-KALK
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KALK FILL-IN-OVRF FRAME-KALK */
ASSIGN 
       FRAME FRAME-KALK:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ARBF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-MTRLF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-TOTSUM:READ-ONLY IN FRAME FRAME-KALK        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UTRF IN FRAME FRAME-KALK
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-TIDUT
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDUT */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDUT                = TRUE.

/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
ASSIGN 
       RAD_KALK:HIDDEN IN FRAME FRAME-VINST           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
THEN WWSTART:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.efastkalktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.efastkalktemp.ARBKOD
"ARBKOD" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.efastkalktemp.LOPNR
"LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.efastkalktemp.BENAMNING
"BENAMNING" "Benämning" "X(20)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.efastkalktemp.ANTAL
"ANTAL" ? "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.efastkalktemp.ENHET
     _FldNameList[6]   > Temp-Tables.efastkalktemp.F1
"F1" "F1!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.efastkalktemp.F2
"F2" "F2!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.efastkalktemp.MASKINTIMMAR
"MASKINTIMMAR" "MASKIN!TIMMAR" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.efastkalktemp.UTRUST
"UTRUST" "Utrustning!TEST" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.efastkalktemp.EA
"EA" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.efastkalktemp.ARBETE
"ARBETE" "Arbete!TEST" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.efastkalktemp.MATERIEL
"MATERIEL" "Materiel!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > Temp-Tables.efastkalktemp.MASKINKOST
"MASKINKOST" "Maskinkost!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-Tables.efastkalktemp.UTRUSTKOST
"UTRUSTKOST" "Utrustnings!kostnad" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Temp-Tables.efastkalktemp.OVRIGT
"OVRIGT" "Övrigt!TEST" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > Temp-Tables.efastkalktemp.SUMMA
"SUMMA" ? "->>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WWSTART WWSTART
ON WINDOW-CLOSE OF WWSTART /* GURU */
DO:   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
&Scoped-define SELF-NAME BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KALK WWSTART
ON ROW-LEAVE OF BRW_KALK IN FRAME FRAME-KALK
DO:
   RUN visa_UI.   
   ASSIGN
   efastkalktemp.ANTAL         = INPUT BROWSE BRW_KALK efastkalktemp.ANTAL       
   efastkalktemp.ARBETE        = INPUT BROWSE BRW_KALK efastkalktemp.ARBETE      
   efastkalktemp.EA            = INPUT BROWSE BRW_KALK efastkalktemp.EA          
   efastkalktemp.F1            = INPUT BROWSE BRW_KALK efastkalktemp.F1          
   efastkalktemp.F2            = INPUT BROWSE BRW_KALK efastkalktemp.F2          
   efastkalktemp.MASKINKOST    = INPUT BROWSE BRW_KALK efastkalktemp.MASKINKOST  
   efastkalktemp.MASKINTIMMAR  = INPUT BROWSE BRW_KALK efastkalktemp.MASKINTIMMAR
   efastkalktemp.MATERIEL      = INPUT BROWSE BRW_KALK efastkalktemp.MATERIEL    
   efastkalktemp.OVRIGT        = INPUT BROWSE BRW_KALK efastkalktemp.OVRIGT      
   efastkalktemp.UTRUST        = INPUT BROWSE BRW_KALK efastkalktemp.UTRUST      
   efastkalktemp.UTRUSTKOST    = INPUT BROWSE BRW_KALK efastkalktemp.UTRUSTKOST.  
   RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WWSTART
ON CHOOSE OF BTN_AVB IN FRAME FRAME-VINST /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KALK
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WWSTART
ON CHOOSE OF BTN_BORT IN FRAME FRAME-KALK /* tA BORT */
DO:
   MESSAGE "Vill du verkligen ta bort denna post?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Kalkyl" 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:
      RUN selnextprevrow_UI IN brwproc[1].
      DELETE efastkalktemp.
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME BTN_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HTML WWSTART
ON CHOOSE OF BTN_HTML IN FRAME FRAME-VINST /* Visa i IE */
DO:
   {muswait.i}
   RUN UTFKALHTM.P (INPUT TABLE tidut). 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KALK
&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WWSTART
ON CHOOSE OF BTN_NY IN FRAME FRAME-KALK /* NY */
DO:
   DEBUGGER:SET-BREAK().
   DEFINE VARIABLE erow AS ROWID NO-UNDO.
   CREATE efastkalktemp.
   erow = ROWID(efastkalktemp).
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(efastkalktemp)).
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   APPLY "ENTRY" TO efastkalktemp.ARBKOD IN BROWSE BRW_KALk.    
    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME BTN_PDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PDF WWSTART
ON CHOOSE OF BTN_PDF IN FRAME FRAME-VINST /* Visa i PDF */
DO:
   {muswait.i}
   RUN UTFKALPDF.P (INPUT TABLE tidut). 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WWSTART
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
     {muswait.i}             
     RUN ut_UI.               
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WWSTART
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KALK
&Scoped-define SELF-NAME FILL-IN-ARBF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARBF WWSTART
ON LEAVE OF FILL-IN-ARBF IN FRAME FRAME-KALK /* Faktor Arbetskostnad */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MASKF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MASKF WWSTART
ON LEAVE OF FILL-IN-MASKF IN FRAME FRAME-KALK /* Faktor Maskin */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MTRLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MTRLF WWSTART
ON LEAVE OF FILL-IN-MTRLF IN FRAME FRAME-KALK /* Faktor Materiel */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OVRF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVRF WWSTART
ON LEAVE OF FILL-IN-OVRF IN FRAME FRAME-KALK /* Faktor Övrigt */
DO:
   RUN faktorer_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTRF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRF WWSTART
ON LEAVE OF FILL-IN-UTRF IN FRAME FRAME-KALK /* Faktor Utrustning */
DO:
   RUN faktorer_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define SELF-NAME RAD_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KALK WWSTART
ON VALUE-CHANGED OF RAD_KALK IN FRAME FRAME-VINST
DO:
   RAD_KALK = INPUT RAD_KALK.
   IF RAD_KALK = 1 THEN DO:
      ASSIGN
      FRAME FRAME-KALK:HIDDEN = TRUE
      FRAME FRAME-TIDUT:HIDDEN = FALSE.
   END.
   IF RAD_KALK = 2 THEN DO:
      ASSIGN
      FRAME FRAME-KALK:HIDDEN = FALSE
      FRAME FRAME-TIDUT:HIDDEN = TRUE.
      RUN openbdynspec_UI IN brwproc[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KALK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WWSTART 


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
   RUN avb_UI.     
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.  
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
   RUN avb_UI.     
   
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
   CREATE efastkalktemp.
   efastkalktemp.ARBKOD = "EGEN".
   efastkalktemp.lopnr = 33.
   CREATE efastkalktemp.
   efastkalktemp.ARBKOD = "EGEN".
   efastkalktemp.lopnr = 33.
   CREATE efastkalktemp.
   efastkalktemp.ARBKOD = "EGEN".
   efastkalktemp.lopnr = 33.

    varforetypval[20] = 6. 
    SESSION:DATA-ENTRY-RETURN = FALSE.

{FRMFONTDEF.I}
   

   RUN DYNBRW.P PERSISTENT SET brwproc[1]
   (INPUT BRW_KALK:HANDLE IN FRAME FRAME-KALK).            
   RUN enable_UI.    
   {FRMSIZEF.I} 
   FRAME FRAME-KALK:HIDDEN = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {musarrow.i}      
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WWSTART  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
  THEN DELETE WIDGET WWSTART.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WWSTART  _DEFAULT-ENABLE
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
  DISPLAY RAD_KALK 
      WITH FRAME FRAME-VINST IN WINDOW WWSTART.
  ENABLE RAD_KALK BTN_SKRIV BTN_EXCEL BTN_HTML RECT-57 BTN_PDF BTN_AVB 
      WITH FRAME FRAME-VINST IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  DISPLAY FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF FILL-IN-ARBF FILL-IN-OVRF 
          FILL-IN-TOTSUM 
      WITH FRAME FRAME-KALK IN WINDOW WWSTART.
  ENABLE FILL-IN-MASKF FILL-IN-MTRLF FILL-IN-UTRF FILL-IN-ARBF FILL-IN-OVRF 
         BRW_KALK BTN_NY BTN_BORT FILL-IN-TOTSUM 
      WITH FRAME FRAME-KALK IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  ENABLE BRW_UT 
      WITH FRAME FRAME-TIDUT IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDUT}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktorer_UI WWSTART 
PROCEDURE faktorer_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE gfaktorA AS DECIMAL NO-UNDO.
   DEFINE VARIABLE gfaktorM AS DECIMAL NO-UNDO.
   DEFINE VARIABLE gfaktorMT AS DECIMAL NO-UNDO.
   DEFINE VARIABLE gfaktorO AS DECIMAL NO-UNDO.
   DEFINE VARIABLE gfaktorU AS DECIMAL NO-UNDO.
   ASSIGN       
   gfaktorA  = FILL-IN-ARBF 
   gfaktorM  = FILL-IN-MASKF
   gfaktorMT = FILL-IN-MTRLF
   gfaktorO  = FILL-IN-OVRF 
   gfaktorU  = FILL-IN-UTRF. 



   ASSIGN
   FILL-IN-ARBF   = INPUT FRAME FRAME-KALK       FILL-IN-ARBF          
   FILL-IN-MASKF  = INPUT       FILL-IN-MASKF         
   FILL-IN-MTRLF  = INPUT       FILL-IN-MTRLF         
   FILL-IN-OVRF   = INPUT       FILL-IN-OVRF          
   FILL-IN-UTRF   = INPUT       FILL-IN-UTRF.          
   FILL-IN-TOTSUM = 0.
   FOR EACH efastkalktemp:
      ASSIGN
      efastkalktemp.ARBETE        =     FILL-IN-ARBF / gfaktorA   * efastkalktemp.ARBETE
      efastkalktemp.MASKINKOST    =     FILL-IN-MASKF / gfaktorM   * efastkalktemp.MASKINKOST  
      efastkalktemp.MATERIEL      =     FILL-IN-MTRLF / gfaktorMT  * efastkalktemp.MATERIEL    
      efastkalktemp.OVRIGT        =     FILL-IN-OVRF  / gfaktorO   * efastkalktemp.OVRIGT         
      efastkalktemp.UTRUSTKOST    =     FILL-IN-UTRF  / gfaktorU   *  efastkalktemp.UTRUSTKOST .
      efastkalktemp.SUMMA = efastkalktemp.ARBETE + efastkalktemp.MATERIEL + efastkalktemp.MASKINKOST + 
      efastkalktemp.OVRIGT + efastkalktemp.ENTRP + efastkalktemp.UTRUSTKOST.   
      FILL-IN-TOTSUM = FILL-IN-TOTSUM + efastkalktemp.SUMMA.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   RUN visa_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summakalk_UI WWSTART 
PROCEDURE summakalk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-TOTSUM = FILL-IN-TOTSUM - efastkalktemp.SUMMA.
   efastkalktemp.SUMMA = efastkalktemp.ARBETE + efastkalktemp.MATERIEL + efastkalktemp.MASKINKOST + 
   efastkalktemp.OVRIGT + efastkalktemp.ENTRP + efastkalktemp.UTRUSTKOST.   
   DISPLAY efastkalktemp.SUMMA WITH BROWSE BRW_KALK.
   FILL-IN-TOTSUM = FILL-IN-TOTSUM + efastkalktemp.SUMMA.
   RUN visa_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WWSTART 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE efastkalktemp THEN DO:
      DISPLAY
      efastkalktemp.ARBKOD efastkalktemp.LOPNR
      efastkalktemp.ANTAL efastkalktemp.ARBETE efastkalktemp.ARBKOD efastkalktemp.BENAMNING 
      efastkalktemp.EA efastkalktemp.ENHET efastkalktemp.F1 efastkalktemp.F2 efastkalktemp.LOPNR 
      efastkalktemp.MASKINKOST efastkalktemp.MASKINTIMMAR efastkalktemp.MATERIEL 
      efastkalktemp.OVRIGT efastkalktemp.SUMMA efastkalktemp.UTRUST efastkalktemp.UTRUSTKOST
      WITH BROWSE BRW_KALK.
   END.
   DISPLAY FILL-IN-TOTSUM WITH FRAME FRAME-KALK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

