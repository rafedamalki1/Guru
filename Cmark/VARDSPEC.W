&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valdfast AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardtyp AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
{MARKVARD.I}
DEFINE BUFFER skogvbuff FOR skogvardtemp.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE  VARIABLE valvard AS CHARACTER NO-UNDO. 
DEFINE  VARIABLE volrec2 AS RECID NO-UNDO.
DEFINE  VARIABLE volrec AS RECID NO-UNDO. 
DEFINE  VARIABLE akerrec2 AS RECID NO-UNDO.
DEFINE  VARIABLE akerrec AS RECID NO-UNDO. 
DEFINE  VARIABLE skogrec2 AS RECID NO-UNDO.
DEFINE  VARIABLE skogrec AS RECID NO-UNDO. 
DEFINE  VARIABLE fastvrec AS RECID NO-UNDO. 
DEFINE  VARIABLE vardrec AS RECID NO-UNDO.
DEFINE  VARIABLE vardrec2 AS RECID NO-UNDO.
DEFINE  VARIABLE kabrec AS RECID NO-UNDO. 
DEFINE  VARIABLE kabrec2 AS RECID NO-UNDO. 
DEFINE VARIABLE markarec AS RECID NO-UNDO.
DEFINE VARIABLE fbeteckn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fastighbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE bestandnr AS INTEGER NO-UNDO.
DEFINE VARIABLE stpnr AS INTEGER NO-UNDO.
DEFINE VARIABLE volskogtabrec AS RECID NO-UNDO.
DEFINE VARIABLE akerkabrec2 AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrevar AS INTEGER NO-UNDO.
DEFINE VARIABLE imon AS INTEGER NO-UNDO.
DEFINE VARIABLE iyr AS INTEGER NO-UNDO.
DEFINE VARIABLE bmon AS INTEGER NO-UNDO.
DEFINE VARIABLE byr AS INTEGER NO-UNDO.
DEFINE VARIABLE totkr AS INTEGER NO-UNDO.
DEFINE VARIABLE revrak AS INTEGER NO-UNDO.
DEFINE VARIABLE kommandoq AS CHARACTER NO-UNDO.
DEFINE VARIABLE vardspecapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
&Scoped-define SHARED SHARED  
{MARKVAL.I}
{MAGA.I}
/*DEFINE TEMP-TABLE maga                       
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
   INDEX ORDN ORDNING ASCENDING.*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AKER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES akervardtemp akerkabtemp revtemp ~
skogvardtemp volskogtemp

/* Definitions for BROWSE BRW_AKER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AKER akervardtemp.STOLPNR ~
akervardtemp.SORT akervardtemp.L1 akervardtemp.L2 akervardtemp.L3 ~
akervardtemp.PROCENT akervardtemp.KRONOR akervardtemp.FASTPRIS ~
akervardtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AKER akervardtemp.STOLPNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AKER akervardtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AKER akervardtemp
&Scoped-define QUERY-STRING-BRW_AKER FOR EACH akervardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AKER OPEN QUERY BRW_AKER FOR EACH akervardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AKER akervardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AKER akervardtemp


/* Definitions for BROWSE BRW_KAB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KAB akerkabtemp.L1 akerkabtemp.FLKAB ~
akerkabtemp.L2 akerkabtemp.KRONOR akerkabtemp.FASTPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KAB akerkabtemp.L1 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KAB akerkabtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KAB akerkabtemp
&Scoped-define QUERY-STRING-BRW_KAB FOR EACH akerkabtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KAB OPEN QUERY BRW_KAB FOR EACH akerkabtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KAB akerkabtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KAB akerkabtemp


/* Definitions for BROWSE BRW_REV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_REV revtemp.zon revtemp.krpm ~
revtemp.meter revtemp.kronor 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_REV 
&Scoped-define QUERY-STRING-BRW_REV FOR EACH revtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_REV OPEN QUERY BRW_REV FOR EACH revtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_REV revtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_REV revtemp


/* Definitions for BROWSE BRW_SKOG                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SKOG skogvardtemp.BESTAND ~
skogvardtemp.LANGD skogvardtemp.BREDD skogvardtemp.HA skogvardtemp.MARKTYP ~
skogvardtemp.BESTTYP skogvardtemp.BARRLOV skogvardtemp.SLENH ~
skogvardtemp.PROCENT skogvardtemp.KR-HA skogvardtemp.KRONOR ~
skogvardtemp.FASTPRIS skogvardtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SKOG skogvardtemp.BESTAND 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SKOG skogvardtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SKOG skogvardtemp
&Scoped-define QUERY-STRING-BRW_SKOG FOR EACH skogvardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SKOG OPEN QUERY BRW_SKOG FOR EACH skogvardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SKOG skogvardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SKOG skogvardtemp


/* Definitions for BROWSE BRW_TILLFS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_TILLFS akervardtemp.KRONOR ~
akervardtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILLFS 
&Scoped-define QUERY-STRING-BRW_TILLFS FOR EACH akervardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TILLFS OPEN QUERY BRW_TILLFS FOR EACH akervardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TILLFS akervardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILLFS akervardtemp


/* Definitions for BROWSE BRW_TRAFO                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_TRAFO akervardtemp.STOLPNR ~
akervardtemp.L1 akervardtemp.L2 akervardtemp.L4 akervardtemp.KRONOR ~
akervardtemp.FASTPRIS akervardtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TRAFO akervardtemp.STOLPNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TRAFO akervardtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TRAFO akervardtemp
&Scoped-define QUERY-STRING-BRW_TRAFO FOR EACH akervardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TRAFO OPEN QUERY BRW_TRAFO FOR EACH akervardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TRAFO akervardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TRAFO akervardtemp


/* Definitions for BROWSE BRW_VOLYM                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VOLYM volskogtemp.DIAM volskogtemp.TANT ~
volskogtemp.THKL volskogtemp.TM3 volskogtemp.GANT volskogtemp.GHKL ~
volskogtemp.GM3 volskogtemp.BANT volskogtemp.BHKL volskogtemp.BM3 ~
volskogtemp.OANT volskogtemp.OHKL volskogtemp.OM3 volskogtemp.OKORR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOLYM volskogtemp.DIAM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VOLYM volskogtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VOLYM volskogtemp
&Scoped-define QUERY-STRING-BRW_VOLYM FOR EACH volskogtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VOLYM OPEN QUERY BRW_VOLYM FOR EACH volskogtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VOLYM volskogtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOLYM volskogtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VARDSO CMB_FAST FILL-IN_FORDYRAD ~
FILL-IN-M3 BRW_SKOG BRW_VOLYM BRW_REV BRW_AKER BRW_TRAFO BRW_TILLFS BRW_KAB ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VARDNR FILL-IN_BENAMNING ~
FILL-IN_AONR FILL-IN_DELNR RAD_VARDSO FILL-IN-PRODOMR CMB_FAST ~
FILL-IN_VARDETID FILL-IN_FORDYRAD FILL-IN-M3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  
   ( INPUT valbrw AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISKAL 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_FAST AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Fastigheter" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-M3 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "m3" 
     VIEW-AS FILL-IN 
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRODOMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prod.omr." 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 25.63 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FORDYRAD AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Fördyrad avverkn. kr/m3" 
     VIEW-AS FILL-IN 
     SIZE 8.5 BY 1.

DEFINE VARIABLE FILL-IN_VARDETID AS CHARACTER FORMAT "X(4)" 
     LABEL "Värdetidpkt ÅÅMM" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE RAD_VARDSO AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Skog", 1,
"Volymberäkning", 2,
"Åkernorm", 3,
"Kabel i mark", 4,
"Station / Ksk", 5
     SIZE 91.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AKER FOR 
      akervardtemp SCROLLING.

DEFINE QUERY BRW_KAB FOR 
      akerkabtemp SCROLLING.

DEFINE QUERY BRW_REV FOR 
      revtemp SCROLLING.

DEFINE QUERY BRW_SKOG FOR 
      skogvardtemp SCROLLING.

DEFINE QUERY BRW_TILLFS FOR 
      akervardtemp SCROLLING.

DEFINE QUERY BRW_TRAFO FOR 
      akervardtemp SCROLLING.

DEFINE QUERY BRW_VOLYM FOR 
      volskogtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AKER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AKER C-Win _STRUCTURED
  QUERY BRW_AKER NO-LOCK DISPLAY
      akervardtemp.STOLPNR COLUMN-LABEL "Stolp!nr" FORMAT ">>>9":U
      akervardtemp.SORT COLUMN-LABEL "Sort" FORMAT "X(10)":U
      akervardtemp.L1 COLUMN-LABEL "Längd!L1" FORMAT ">>9":U
      akervardtemp.L2 COLUMN-LABEL "Bredd!L2" FORMAT ">>9":U
      akervardtemp.L3 FORMAT ">>9":U
      akervardtemp.PROCENT COLUMN-LABEL "Procent" FORMAT ">>9":U
      akervardtemp.KRONOR COLUMN-LABEL "Kronor" FORMAT "->>>>>>9":U
      akervardtemp.FASTPRIS COLUMN-LABEL "Fastpris" FORMAT "Ja/Nej":U
            WIDTH 4
      akervardtemp.BENAMNING COLUMN-LABEL "Kommentar" FORMAT "X(256)":U
            WIDTH 33
  ENABLE
      akervardtemp.STOLPNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 80 BY 14.17
         TITLE "Åkernorm".

DEFINE BROWSE BRW_KAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KAB C-Win _STRUCTURED
  QUERY BRW_KAB NO-LOCK DISPLAY
      akerkabtemp.L1 COLUMN-LABEL "Längd" FORMAT ">>>9":U
      akerkabtemp.FLKAB FORMAT "Ja/Nej":U
      akerkabtemp.L2 COLUMN-LABEL "Bredd" FORMAT ">9.99":U
      akerkabtemp.KRONOR COLUMN-LABEL "Kronor" FORMAT "->>>>>>9":U
      akerkabtemp.FASTPRIS COLUMN-LABEL "Fastpris" FORMAT "Ja/Nej":U
            WIDTH 9
  ENABLE
      akerkabtemp.L1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 81.75 BY 14.17
         TITLE "Kabel i mark".

DEFINE BROWSE BRW_REV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_REV C-Win _STRUCTURED
  QUERY BRW_REV NO-LOCK DISPLAY
      revtemp.zon COLUMN-LABEL "Zon" FORMAT ">>9":U
      revtemp.krpm COLUMN-LABEL "Kr/m" FORMAT "->>>>9.99":U
      revtemp.meter COLUMN-LABEL "Meter" FORMAT "->>>>9.99":U
      revtemp.kronor COLUMN-LABEL "Kronor" FORMAT "->>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 51.5 BY 14.17
         TITLE "Rev-avtal".

DEFINE BROWSE BRW_SKOG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SKOG C-Win _STRUCTURED
  QUERY BRW_SKOG NO-LOCK DISPLAY
      skogvardtemp.BESTAND COLUMN-LABEL "Best!ånd" FORMAT ">>9":U
      skogvardtemp.LANGD COLUMN-LABEL "Längd" FORMAT "->>>>>>9":U
            WIDTH 6
      skogvardtemp.BREDD COLUMN-LABEL "Bredd" FORMAT ">>9":U
      skogvardtemp.HA COLUMN-LABEL "Ha" FORMAT ">>9.999":U
      skogvardtemp.MARKTYP COLUMN-LABEL "Mark!typ" FORMAT "X(256)":U
            WIDTH 15
      skogvardtemp.BESTTYP COLUMN-LABEL "Best!typ" FORMAT "X(7)":U
      skogvardtemp.BARRLOV COLUMN-LABEL "Barr!löv" FORMAT "X(4)":U
      skogvardtemp.SLENH COLUMN-LABEL "Slut!enhet" FORMAT ">9.99":U
      skogvardtemp.PROCENT COLUMN-LABEL "%" FORMAT ">>9":U
      skogvardtemp.KR-HA COLUMN-LABEL "Kr-Ha" FORMAT ">>>>9":U
      skogvardtemp.KRONOR COLUMN-LABEL "Kronor" FORMAT "->>>>>9":U
      skogvardtemp.FASTPRIS COLUMN-LABEL "Fast!pris" FORMAT "Ja/Nej":U
      skogvardtemp.BENAMNING COLUMN-LABEL "Kommentar" FORMAT "X(256)":U
            WIDTH 20
  ENABLE
      skogvardtemp.BESTAND
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 93 BY 14.17
         TITLE "Skog".

DEFINE BROWSE BRW_TILLFS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILLFS C-Win _STRUCTURED
  QUERY BRW_TILLFS NO-LOCK DISPLAY
      akervardtemp.KRONOR COLUMN-LABEL "Kronor" FORMAT "->>>>>>9":U
      akervardtemp.BENAMNING COLUMN-LABEL "Kommentar" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 78.13 BY 14.17
         TITLE "Tillfällig skada".

DEFINE BROWSE BRW_TRAFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TRAFO C-Win _STRUCTURED
  QUERY BRW_TRAFO NO-LOCK DISPLAY
      akervardtemp.STOLPNR COLUMN-LABEL "Nr" FORMAT ">>>9":U
      akervardtemp.L1 COLUMN-LABEL "Längd" FORMAT ">>>9":U
      akervardtemp.L2 COLUMN-LABEL "Bredd" FORMAT ">>>>9":U
      akervardtemp.L4 COLUMN-LABEL "Enl. ritning nr" FORMAT ">>>>>>>9":U
      akervardtemp.KRONOR COLUMN-LABEL "Kronor" FORMAT "->>>>>>9":U
      akervardtemp.FASTPRIS COLUMN-LABEL "Fastpris" FORMAT "Ja/Nej":U
            WIDTH 4
      akervardtemp.BENAMNING COLUMN-LABEL "Kommentar" FORMAT "X(256)":U
            WIDTH 30
  ENABLE
      akervardtemp.STOLPNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 78.13 BY 14.17
         TITLE "Nätstation / Kabelskåp".

DEFINE BROWSE BRW_VOLYM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOLYM C-Win _STRUCTURED
  QUERY BRW_VOLYM NO-LOCK DISPLAY
      volskogtemp.DIAM COLUMN-LABEL "Diam" FORMAT ">9":U
      volskogtemp.TANT COLUMN-LABEL "Tall!ant" FORMAT ">>9":U
      volskogtemp.THKL COLUMN-LABEL "Tall!hkl" FORMAT "X(3)":U
      volskogtemp.TM3 COLUMN-LABEL "Tall!m3" FORMAT ">9.99":U
      volskogtemp.GANT COLUMN-LABEL "Gran!ant" FORMAT ">>9":U
      volskogtemp.GHKL COLUMN-LABEL "Gran!hkl" FORMAT "X(3)":U
      volskogtemp.GM3 COLUMN-LABEL "Gran!m3" FORMAT ">9.99":U
      volskogtemp.BANT COLUMN-LABEL "Björk!ant" FORMAT ">>9":U
      volskogtemp.BHKL COLUMN-LABEL "Björk!hkl" FORMAT "X(3)":U
      volskogtemp.BM3 COLUMN-LABEL "Björk!m3" FORMAT ">9.99":U
      volskogtemp.OANT COLUMN-LABEL "Övriga!ant" FORMAT ">>9":U
      volskogtemp.OHKL COLUMN-LABEL "Övriga!hkl" FORMAT "X(3)":U
      volskogtemp.OM3 COLUMN-LABEL "Övriga!m3" FORMAT ">>9.99":U
      volskogtemp.OKORR COLUMN-LABEL "Korr %" FORMAT ">>9":U
  ENABLE
      volskogtemp.DIAM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 79 BY 14.17
         TITLE "Volymberäkning".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN_VARDNR AT ROW 1.58 COL 16 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 1.58 COL 28.13 COLON-ALIGNED NO-LABEL
     FILL-IN_AONR AT ROW 1.58 COL 62.5 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 1.58 COL 77.25 COLON-ALIGNED
     RAD_VARDSO AT ROW 3 COL 1.5 NO-LABEL
     FILL-IN-PRODOMR AT ROW 4.5 COL 98.38 COLON-ALIGNED WIDGET-ID 2
     CMB_FAST AT ROW 4.54 COL 14 COLON-ALIGNED
     FILL-IN_VARDETID AT ROW 4.54 COL 61.25 COLON-ALIGNED
     FILL-IN_FORDYRAD AT ROW 4.54 COL 65.5 COLON-ALIGNED
     FILL-IN-M3 AT ROW 4.54 COL 79.5 COLON-ALIGNED
     BRW_SKOG AT ROW 5.83 COL 1.5
     BRW_VOLYM AT ROW 5.83 COL 1.5
     BRW_REV AT ROW 5.83 COL 1.5
     BRW_AKER AT ROW 5.83 COL 1.5
     BRW_TRAFO AT ROW 5.83 COL 1.88
     BRW_TILLFS AT ROW 5.83 COL 1.88
     BRW_KAB AT ROW 5.83 COL 1.88
     FBTN_VISKAL AT ROW 8 COL 96.75
     FBTN_SKR AT ROW 9.08 COL 96.75
     BTN_NY AT ROW 20.38 COL 22.75
     BTN_UPP AT ROW 20.38 COL 36.5
     BTN_BORT AT ROW 20.38 COL 50.13
     BTN_AVB AT ROW 20.38 COL 96.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111 BY 20.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: akerkabtemp T "?" NO-UNDO temp-db akerkabtemp
      TABLE: akervardtemp T "?" NO-UNDO temp-db akervardtemp
      TABLE: revtemp T "?" NO-UNDO temp-db revtemp
      TABLE: skogvardtemp T "?" NO-UNDO temp-db skogvardtemp
      TABLE: volskogtemp T "?" NO-UNDO temp-db volskogtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Markvärdering"
         HEIGHT             = 20.83
         WIDTH              = 111.13
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 114.25
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 114.25
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_SKOG FILL-IN-M3 DEFAULT-FRAME */
/* BROWSE-TAB BRW_VOLYM BRW_SKOG DEFAULT-FRAME */
/* BROWSE-TAB BRW_REV BRW_VOLYM DEFAULT-FRAME */
/* BROWSE-TAB BRW_AKER BRW_REV DEFAULT-FRAME */
/* BROWSE-TAB BRW_TRAFO BRW_AKER DEFAULT-FRAME */
/* BROWSE-TAB BRW_TILLFS BRW_TRAFO DEFAULT-FRAME */
/* BROWSE-TAB BRW_KAB BRW_TILLFS DEFAULT-FRAME */
/* SETTINGS FOR BUTTON BTN_BORT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_SKR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_SKR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISKAL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISKAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PRODOMR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BENAMNING IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDETID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AKER
/* Query rebuild information for BROWSE BRW_AKER
     _TblList          = "Temp-Tables.akervardtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.akervardtemp.STOLPNR
"akervardtemp.STOLPNR" "Stolp!nr" ">>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.akervardtemp.SORT
"akervardtemp.SORT" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.akervardtemp.L1
"akervardtemp.L1" "Längd!L1" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.akervardtemp.L2
"akervardtemp.L2" "Bredd!L2" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.akervardtemp.L3
     _FldNameList[6]   > Temp-Tables.akervardtemp.PROCENT
"akervardtemp.PROCENT" "Procent" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.akervardtemp.KRONOR
"akervardtemp.KRONOR" "Kronor" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.akervardtemp.FASTPRIS
"akervardtemp.FASTPRIS" "Fastpris" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.akervardtemp.BENAMNING
"akervardtemp.BENAMNING" "Kommentar" "X(256)" "character" ? ? ? ? ? ? no ? no no "33" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AKER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KAB
/* Query rebuild information for BROWSE BRW_KAB
     _TblList          = "Temp-Tables.akerkabtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.akerkabtemp.L1
"akerkabtemp.L1" "Längd" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.akerkabtemp.FLKAB
"akerkabtemp.FLKAB" ? "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.akerkabtemp.L2
"akerkabtemp.L2" "Bredd" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.akerkabtemp.KRONOR
"akerkabtemp.KRONOR" "Kronor" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.akerkabtemp.FASTPRIS
"akerkabtemp.FASTPRIS" "Fastpris" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_REV
/* Query rebuild information for BROWSE BRW_REV
     _TblList          = "Temp-Tables.revtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.revtemp.zon
"revtemp.zon" "Zon" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.revtemp.krpm
"revtemp.krpm" "Kr/m" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.revtemp.meter
"revtemp.meter" "Meter" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.revtemp.kronor
"revtemp.kronor" "Kronor" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_REV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKOG
/* Query rebuild information for BROWSE BRW_SKOG
     _TblList          = "Temp-Tables.skogvardtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.skogvardtemp.BESTAND
"skogvardtemp.BESTAND" "Best!ånd" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.skogvardtemp.LANGD
"skogvardtemp.LANGD" "Längd" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.skogvardtemp.BREDD
"skogvardtemp.BREDD" "Bredd" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.skogvardtemp.HA
"skogvardtemp.HA" "Ha" ">>9.999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.skogvardtemp.MARKTYP
"skogvardtemp.MARKTYP" "Mark!typ" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.skogvardtemp.BESTTYP
"skogvardtemp.BESTTYP" "Best!typ" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.skogvardtemp.BARRLOV
"skogvardtemp.BARRLOV" "Barr!löv" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.skogvardtemp.SLENH
"skogvardtemp.SLENH" "Slut!enhet" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.skogvardtemp.PROCENT
"skogvardtemp.PROCENT" "%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.skogvardtemp.KR-HA
"skogvardtemp.KR-HA" "Kr-Ha" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.skogvardtemp.KRONOR
"skogvardtemp.KRONOR" "Kronor" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.skogvardtemp.FASTPRIS
"skogvardtemp.FASTPRIS" "Fast!pris" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.skogvardtemp.BENAMNING
"skogvardtemp.BENAMNING" "Kommentar" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKOG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILLFS
/* Query rebuild information for BROWSE BRW_TILLFS
     _TblList          = "Temp-Tables.akervardtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.akervardtemp.KRONOR
"akervardtemp.KRONOR" "Kronor" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.akervardtemp.BENAMNING
"akervardtemp.BENAMNING" "Kommentar" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TILLFS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TRAFO
/* Query rebuild information for BROWSE BRW_TRAFO
     _TblList          = "Temp-Tables.akervardtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.akervardtemp.STOLPNR
"akervardtemp.STOLPNR" "Nr" ">>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.akervardtemp.L1
"akervardtemp.L1" "Längd" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.akervardtemp.L2
"akervardtemp.L2" "Bredd" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.akervardtemp.L4
"akervardtemp.L4" "Enl. ritning nr" ">>>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.akervardtemp.KRONOR
"akervardtemp.KRONOR" "Kronor" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.akervardtemp.FASTPRIS
"akervardtemp.FASTPRIS" "Fastpris" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.akervardtemp.BENAMNING
"akervardtemp.BENAMNING" "Kommentar" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TRAFO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOLYM
/* Query rebuild information for BROWSE BRW_VOLYM
     _TblList          = "Temp-Tables.volskogtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.volskogtemp.DIAM
"volskogtemp.DIAM" "Diam" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.volskogtemp.TANT
"volskogtemp.TANT" "Tall!ant" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.volskogtemp.THKL
"volskogtemp.THKL" "Tall!hkl" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.volskogtemp.TM3
"volskogtemp.TM3" "Tall!m3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.volskogtemp.GANT
"volskogtemp.GANT" "Gran!ant" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.volskogtemp.GHKL
"volskogtemp.GHKL" "Gran!hkl" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.volskogtemp.GM3
"volskogtemp.GM3" "Gran!m3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.volskogtemp.BANT
"volskogtemp.BANT" "Björk!ant" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.volskogtemp.BHKL
"volskogtemp.BHKL" "Björk!hkl" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.volskogtemp.BM3
"volskogtemp.BM3" "Björk!m3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.volskogtemp.OANT
"volskogtemp.OANT" "Övriga!ant" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.volskogtemp.OHKL
"volskogtemp.OHKL" "Övriga!hkl" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.volskogtemp.OM3
"volskogtemp.OM3" "Övriga!m3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.volskogtemp.OKORR
"volskogtemp.OKORR" "Korr %" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VOLYM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Markvärdering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Markvärdering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON GO OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   IF brwval(RAD_VARDSO) = TRUE THEN RETURN NO-APPLY.
   ASSIGN CMB_FAST = INPUT CMB_FAST.
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
   fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE fastvardtemp THEN DO:
      fastvrec = RECID(fastvardtemp).
   END.
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   CMB_FAST = INPUT CMB_FAST.      
   IF RAD_VARDSO = 1 THEN DO:
      /* ska gå att kominera 20150210 Lena
      FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND 
      revtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR  NO-LOCK NO-ERROR.
      IF AVAILABLE revtemp THEN DO:
         MESSAGE "Rev-avtal redan registrerad för fastighet. Kan ej kombineras med skog för fastighet" VIEW-AS ALERT-BOX.                     
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.*/      
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).
      bestandnr = ?.
      vart = "NYA".  
      RUN andspec_UI (INPUT 1).
           
      RUN nyreg_UI.     
   END.   
   ELSE IF RAD_VARDSO = 2 THEN DO:
      /* ska gå att kominera 20150210 Lena*/      
      /*FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND 
      revtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR  NO-LOCK NO-ERROR.
      IF AVAILABLE revtemp THEN DO:
         MESSAGE "Rev-avtal redan registrerad för fastighet. Kan ej kombineras med volymberäkning för fastighet" VIEW-AS ALERT-BOX.                     
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.*/      
      FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
      volskogtemp.BETECKNING = CMB_FAST AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp  THEN DO:
         MESSAGE "Volym för fastigheten är manuellt redovisad. För att istället räkna ut volym - ange noll" VIEW-AS ALERT-BOX.            
         APPLY "ENTRY" TO FILL-IN-M3 IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FSVOL"                   
      inextradatatemp.HUVUDINT = varderingtemp.VARDNR                    
      inextradatatemp.HUVUDCH = CMB_FAST.                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp WHERE extradatatemp.SOKDEC[2] > 0 NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         MESSAGE "Volym för fastigheten är beräknad via Skog." VIEW-AS ALERT-BOX.                     
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.    
      END.
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).     
      vart = "NYA".        
      RUN andspec_UI (INPUT 2).
     
      RUN nyreg_UI.          
   END.     
   ELSE IF RAD_VARDSO = 3 THEN DO:      
           
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).     
      vart = "NYA".  
      IF INPUT FILL-IN_VARDETID = "" THEN DO:
         MESSAGE "Värdetidpunkt kan inte vara blank!" VIEW-AS ALERT-BOX.            
         APPLY "ENTRY" TO FILL-IN_VARDETID IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.   
      imon = INTEGER(SUBSTR(FILL-IN_VARDETID,3,2)).      
      iyr = INTEGER(SUBSTRING(FILL-IN_VARDETID,1,2)). 
      IF iyr > 50 THEN ASSIGN  byr = iyr + 1900.
      ELSE ASSIGN  byr = iyr + 2000.  
      FIND FIRST kindextemp WHERE kindextemp.AR = byr AND kindextemp.MANAD = imon USE-INDEX AR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kindextemp THEN DO:
         FIND LAST kindextemp WHERE kindextemp.AR = byr AND kindextemp.VARDE > 0 USE-INDEX AR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kindextemp THEN DO:
            iyr = iyr - 1.
            byr = byr - 1.
            FIND LAST kindextemp WHERE kindextemp.AR = byr AND kindextemp.VARDE > 0 USE-INDEX AR NO-LOCK NO-ERROR.
            IF AVAILABLE kindextemp THEN DO:
               bmon = kindextemp.MANAD.
               ASSIGN FILL-IN_VARDETID = STRING(iyr) + STRING(bmon). 
               MESSAGE "Senaste uppdaterade värdetidpunkt är" FILL-IN_VARDETID  VIEW-AS ALERT-BOX. 
            END.
         END.
         ELSE DO:          
            bmon = kindextemp.MANAD.
            ASSIGN FILL-IN_VARDETID = STRING(iyr,"99") + STRING(bmon,"99").            
            MESSAGE "Senaste uppdaterade värdetidpunkt är" FILL-IN_VARDETID  VIEW-AS ALERT-BOX. 
         END.   
      END.
      stpnr = ?. 
      RUN andspec_UI (INPUT 3).     
            
      RUN nyreg_UI.          
   END.   
   ELSE IF RAD_VARDSO = 4 THEN DO:            
            
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).  
      vart = "NYA".
      akerkabrec2 = ?.     
      RUN andspec_UI (INPUT 4).
            
      RUN nyreg_UI.          
   END.   
   ELSE IF RAD_VARDSO = 5 THEN DO:      
      
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).   
      stpnr = ?.
      vart = "NYA".    
      RUN andspec_UI (INPUT 5).          
            
      RUN nyreg_UI.        
   END.
   ELSE IF RAD_VARDSO = 6 THEN DO:      
      /*BORTAGEN KONTROL 20160808  MICHAEL ALAMAA säger att det skall gå att kombinera*/
      /*FIND FIRST akerkabtemp WHERE akerkabtemp.VARDNR = varderingtemp.VARDNR AND
      akerkabtemp.BETECKNING =  CMB_FAST  USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      IF AVAILABLE akerkabtemp THEN DO:  
         MESSAGE "Kabel i mark finns redan registrerad för fastighet. Kan ej kombineras med rev-avtal för fastighet" VIEW-AS ALERT-BOX.                     
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.*/
      revrak = 0.
      FOR EACH revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND 
      revtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR  NO-LOCK:
         revrak = revrak + 1.
      END.
      IF revrak = 2 THEN DO:
         MESSAGE "Det kan inte finnas mer än en registrering per zon" VIEW-AS ALERT-BOX.                     
         APPLY "ENDKEY" TO BTN_NY IN FRAME {&FRAME-NAME}.
      END.
      
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).   
      stpnr = ?.
      vart = "NYA".     
      RUN andspec_UI (INPUT 6).         
            
      RUN nyreg_UI.        
   END.
   ELSE IF RAD_VARDSO = 7 THEN DO:      
            
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
      fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
      fastvrec = RECID(fastvardtemp).   
      stpnr = ?.
      vart = "NYA".
      RUN andspec_UI (INPUT 7).              
      RUN nyreg_UI.        
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP C-Win
ON CHOOSE OF BTN_UPP IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   IF brwval(RAD_VARDSO) = TRUE THEN RETURN NO-APPLY.
   ASSIGN CMB_FAST = INPUT CMB_FAST.
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
   fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
   fastvrec = RECID(fastvardtemp).
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FAST C-Win
ON VALUE-CHANGED OF CMB_FAST IN FRAME DEFAULT-FRAME /* Fastigheter */
DO:

   ASSIGN CMB_FAST = INPUT CMB_FAST.
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
   fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
   fastvrec = RECID(fastvardtemp).
   FILL-IN_FORDYRAD = fastvardtemp.FORDYRAD.
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING  = CMB_FAST NO-LOCK NO-ERROR.
   IF AVAILABLE fastighettemp THEN DO:   
      FILL-IN-PRODOMR = fastighettemp.PAKER.
   END. 
   FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
   volskogtemp.BETECKNING = fastvardtemp.BETECKNING AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE volskogtemp THEN FILL-IN-M3 = volskogtemp.OM3.
   ELSE FILL-IN-M3 = 0.
   FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR =  varderingtemp.VARDNR AND 
   skogvardtemp.BETECKNING =  fastvardtemp.BETECKNING AND 
   skogvardtemp.BESTAND = bestandnr USE-INDEX VARDNR NO-LOCK NO-ERROR. 
   
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR C-Win
ON CHOOSE OF FBTN_SKR IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   IF brwval(RAD_VARDSO) = TRUE THEN RETURN NO-APPLY.
   EMPTY TEMP-TABLE markval NO-ERROR.    
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   FIND FIRST markfasttemp WHERE markfasttemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = markfasttemp.MARKNR USE-INDEX MARKNR NO-LOCK NO-ERROR.
   IF AVAILABLE markagaretemp THEN DO:              
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK 
      NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         CREATE markval.
         ASSIGN  
         markval.MARKREC = RECID(markagaretemp)
         markval.VARDNR = fastvardtemp.VARDNR
         markval.MARKNR = markagaretemp.MARKNR
         markval.PERSONNUMMER = markagaretemp.PERSONNUMMER 
         markval.PNR2 = markagaretemp.PNR2
         markval.MARKAGARE = markagaretemp.MARKAGARE       
         markval.GATUADRESS = markagaretemp.GATUADRESS    
         markval.POSTNUMMER = markagaretemp.POSTNUMMER   
         markval.POSTADRESS = markagaretemp.POSTADRESS 
         markval.BETECKNING = markfasttemp.BETECKNING
         markval.PROCENT =  markfasttemp.PROCENT
         markval.ANAMN = fastighettemp.ANAMN
         markval.AADRESS = fastighettemp.AADRESS
         markval.APONR = fastighettemp.APONR
         markval.APADRESS = fastighettemp.APADRESS
         markval.APERNR = fastighettemp.APERNR.
         ASSIGN 
         fbeteckn = markfasttemp.BETECKNING
         markarec = RECID(markagaretemp).
         /*musz = TRUE.    */
      END.   
   END.   
   IF RAD_VARDSO = 1 THEN valvard = "skog".
   ELSE IF RAD_VARDSO = 2 THEN valvard = "volym".
   ELSE IF RAD_VARDSO = 3 THEN valvard = "aker".
   ELSE IF RAD_VARDSO = 4 THEN valvard = "kabel".
   ELSE IF RAD_VARDSO = 5 THEN valvard = "trafo".
   ELSE IF RAD_VARDSO = 6 THEN valvard = "rev".
   ELSE IF RAD_VARDSO = 7 THEN valvard = "trafo".
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT TRUE).   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.   
   IF skrivut = FALSE THEN valvard = "".
   ELSE DO:       
       {AVBGOM.I}
       RUN VARDUTF3.W (INPUT valvard,INPUT fbeteckn,INPUT markarec,INPUT TABLE maga).
       /*RUN VARDUTF2.W (INPUT valvard,INPUT fbeteckn,INPUT markarec).                    */
       {AVBFRAM.I}
       valvard = "".
       APPLY "GO" TO FBTN_SKR IN FRAME {&FRAME-NAME}.  
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISKAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISKAL C-Win
ON CHOOSE OF FBTN_VISKAL IN FRAME DEFAULT-FRAME /* Visa */
DO:    
   
   IF brwval(RAD_VARDSO) = TRUE THEN RETURN NO-APPLY.
   EMPTY TEMP-TABLE markval NO-ERROR.    
   ASSIGN CMB_FAST = INPUT CMB_FAST.
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr AND 
   fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
   FIND FIRST markfasttemp WHERE markfasttemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = markfasttemp.MARKNR USE-INDEX MARKNR NO-LOCK NO-ERROR.
   IF AVAILABLE markagaretemp THEN DO:              
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK 
      NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         CREATE markval.
         ASSIGN    
         markval.MARKREC = RECID(markagaretemp)
         markval.VARDNR = fastvardtemp.VARDNR
         markval.MARKNR = markagaretemp.MARKNR
         markval.PERSONNUMMER = markagaretemp.PERSONNUMMER 
         markval.PNR2 = markagaretemp.PNR2
         markval.MARKAGARE = markagaretemp.MARKAGARE       
         markval.GATUADRESS = markagaretemp.GATUADRESS    
         markval.POSTNUMMER = markagaretemp.POSTNUMMER   
         markval.POSTADRESS = markagaretemp.POSTADRESS 
         markval.BETECKNING = markfasttemp.BETECKNING
         markval.PROCENT =  markfasttemp.PROCENT
         markval.ANAMN = fastighettemp.ANAMN
         markval.AADRESS = fastighettemp.AADRESS
         markval.APONR = fastighettemp.APONR
         markval.APADRESS = fastighettemp.APADRESS
         markval.APERNR = fastighettemp.APERNR.
         ASSIGN 
         fbeteckn = markfasttemp.BETECKNING
         markarec = RECID(markagaretemp).
         /*musz = TRUE.    */
      END.   
   END.   
   IF RAD_VARDSO = 1 THEN valvard = "skog".
   ELSE IF RAD_VARDSO = 2 THEN valvard = "volym".
   ELSE IF RAD_VARDSO = 3 THEN valvard = "aker".
   ELSE IF RAD_VARDSO = 4 THEN valvard = "kabel".
   ELSE IF RAD_VARDSO = 5 THEN valvard = "trafo".
   ELSE IF RAD_VARDSO = 6 THEN valvard = "rev".
   ELSE IF RAD_VARDSO = 7 THEN valvard = "trafo".
   {AVBGOM.I}      
   RUN VARDUTF3.W (INPUT valvard,INPUT fbeteckn,INPUT markarec,INPUT TABLE maga).
   /*RUN VARDUTF2.W (INPUT valvard,INPUT fbeteckn,INPUT markarec).*/
   {AVBFRAM.I}
   valvard = "".   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE.   
   APPLY "GO" TO FBTN_VISKAL IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-M3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-M3 C-Win
ON LEAVE OF FILL-IN-M3 IN FRAME DEFAULT-FRAME /* m3 */
DO:   
   FILL-IN-M3 = INPUT FILL-IN-M3.
   mtrevar = FILL-IN-M3.
   RUN mtre IN vardspecapph (INPUT varderingtemp.VARDNR,
                               INPUT fastvardtemp.BETECKNING,
                               INPUT mtrevar,OUTPUT volrec,
                               INPUT-OUTPUT TABLE volskogtemp).  
   IF mtrevar = 0 THEN DO:
      
   END.
   ELSE DO:
      FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volrec NO-LOCK NO-ERROR. 
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FORDYRAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FORDYRAD C-Win
ON LEAVE OF FILL-IN_FORDYRAD IN FRAME DEFAULT-FRAME /* Fördyrad avverkn. kr/m3 */
DO:
   FILL-IN_FORDYRAD = INPUT FILL-IN_FORDYRAD.
   FIND fastvardtemp WHERE RECID(fastvardtemp) = fastvrec NO-LOCK NO-ERROR.
   IF AVAILABLE fastvardtemp THEN DO:  
      RUN sparfordyrad IN vardspecapph (INPUT valvardnr,
                                        INPUT fastvardtemp.BETECKNING,
                                        INPUT FILL-IN_FORDYRAD).
      ASSIGN fastvardtemp.FORDYRAD = FILL-IN_FORDYRAD.
      DISPLAY FILL-IN_FORDYRAD WITH FRAME {&FRAME-NAME}.
   END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VARDETID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARDETID C-Win
ON LEAVE OF FILL-IN_VARDETID IN FRAME DEFAULT-FRAME /* Värdetidpkt ÅÅMM */
DO:
  FILL-IN_VARDETID = INPUT FILL-IN_VARDETID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VARDSO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VARDSO C-Win
ON VALUE-CHANGED OF RAD_VARDSO IN FRAME DEFAULT-FRAME
DO:
   RAD_VARDSO  = INPUT RAD_VARDSO.
   RUN hidden_UI.
   RUN visa_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AKER
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
   IF VALID-HANDLE(vardspecapph) THEN DELETE PROCEDURE vardspecapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   RUN disable_UI.
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
   
   CMB_FAST:DELIMITER = "$".
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:   
      ASSIGN
      akerkabtemp.FLKAB:LABEL IN BROWSE BRW_KAB = "> 1m mellan yttersta kablarna".      
   END.  
   FOR EACH fastighettemp,
   EACH akervardtemp WHERE akervardtemp.BETECKNING = fastighettemp.BETECKNING AND akervardtemp.L5 = 0 AND akervardtemp.STOLPNR NE 0 NO-LOCK:   
      FIND FIRST akerproctemp WHERE akerproctemp.PRODOMR = fastighettemp.PAKER AND
      akerproctemp.L3MIN LE akervardtemp.L3 AND akerproctemp.L3MAX GE akervardtemp.L3  USE-INDEX PRODOMR NO-LOCK NO-ERROR.      
      IF AVAILABLE akerproctemp THEN DO: 
         akervardtemp.PROCENT = akerproctemp.PROCENT.
         IF akervardtemp.SORT = "ÅKER" THEN.
         ELSE IF akervardtemp.SORT = "BETESMARK" THEN akervardtemp.PROCENT = 15.
         ELSE IF akervardtemp.SORT = "IMPEDIMENT" THEN akervardtemp.PROCENT = 10.
      END.
   END.
   status-ok = RAD_VARDSO:ADD-LAST("Rev-avtal",6).          
   
   IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" THEN DO:
      status-ok = RAD_VARDSO:ADD-LAST("Tillf skada",7).  
   END.
   FILL-IN-M3:TOOLTIP = "Skall vara det totala M3 för hela fastigheten".
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN DO:
      ASSIGN FILL-IN_VARDNR = varderingtemp.VARDNR
      FILL-IN_BENAMNING = varderingtemp.BENAMNING.   
   END.
   OPEN QUERY fq FOR EACH fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr USE-INDEX VARDNR NO-LOCK.
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(fastvardtemp):
      status-ok = CMB_FAST:ADD-LAST(fastvardtemp.BETECKNING).
      GET NEXT fq NO-LOCK.
   END.       
   IF valdfast NE ? THEN DO:   
       FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr AND fastvardtemp.BETECKNING = valdfast USE-INDEX VARDNR NO-LOCK NO-ERROR.
       IF AVAILABLE fastvardtemp THEN DO:
          ASSIGN  CMB_FAST:SCREEN-VALUE = fastvardtemp.BETECKNING.
          fastvrec = RECID(fastvardtemp).
          FILL-IN_FORDYRAD = fastvardtemp.FORDYRAD.
       END.
   END.
   ELSE DO:   
       FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr USE-INDEX VARDNR NO-LOCK NO-ERROR.
       IF AVAILABLE fastvardtemp THEN DO:
          ASSIGN  CMB_FAST:SCREEN-VALUE = fastvardtemp.BETECKNING.
          fastvrec = RECID(fastvardtemp).
          FILL-IN_FORDYRAD = fastvardtemp.FORDYRAD.
       END.
   END.
   FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
   volskogtemp.BETECKNING = fastvardtemp.BETECKNING AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE volskogtemp THEN DO:
      FILL-IN-M3 = volskogtemp.OM3.
   END.
   FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
   volskogtemp.BETECKNING = fastvardtemp.BETECKNING AND volskogtemp.DIAM > 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE volskogtemp  THEN DO:
      /*DISABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.    */
      FILL-IN-M3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE aovardtemp THEN DO:
      ASSIGN FILL-IN_AONR = aovardtemp.AONR
      FILL-IN_DELNR = aovardtemp.DELNR.
   END.
   ASSIGN RAD_VARDSO = 1.      
   IF vardtyp NE ? THEN DO:
       IF vardtyp = 1 THEN RAD_VARDSO = 1.
       IF vardtyp = 2 THEN RAD_VARDSO = 2.
       IF vardtyp = 3 THEN RAD_VARDSO = 3.
       IF vardtyp = 4 THEN RAD_VARDSO = 4.
       IF vardtyp = 5 THEN RAD_VARDSO = 5.
       IF vardtyp = 6 THEN RAD_VARDSO = 6.
       
   END.
   ASSIGN
   vart = "".  
   RUN enable_UI.   
   {FRMSIZE.I}   
   
   IF valdfast NE ? THEN DO:
      /*RUN fastighbethmt IN vardspecapph (INPUT valdfast, OUTPUT fastighbet). */
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING  = valdfast NO-LOCK NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         ASSIGN  CMB_FAST:SCREEN-VALUE = fastighettemp.BETECKNING.
         FILL-IN-PRODOMR = fastighettemp.PAKER.
      END.
      ELSE DO:

      END.
      FIND FIRST fastvardtemp WHERE fastvardtemp.BETECKNING = fastighettemp.BETECKNING USE-INDEX VARDNR NO-LOCK NO-ERROR.      
      IF AVAILABLE fastvardtemp THEN DO:
         fastvrec = RECID(fastvardtemp).
         FILL-IN_FORDYRAD = fastvardtemp.FORDYRAD.
      END.
      
      FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
      volskogtemp.BETECKNING = fastighettemp.BETECKNING AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp THEN DO:
         FILL-IN-M3 = volskogtemp.OM3.
      END.
   END.
   ENABLE BRW_SKOG BRW_VOLYM BRW_AKER WITH FRAME {&FRAME-NAME}.  
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aovardtemp THEN DO:
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.         
   ENABLE 
   BTN_AVB BTN_BORT BTN_NY FBTN_SKR 
   BTN_UPP  FBTN_VISKAL WITH FRAME {&FRAME-NAME}.      
   RUN hidden_UI.
   RUN visa_UI.         
   {musarrow.i} 
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
  WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   akervardtemp.STOLPNR:READ-ONLY IN BROWSE BRW_AKER = TRUE
   akerkabtemp.L1:READ-ONLY IN BROWSE BRW_KAB = TRUE
   skogvardtemp.BESTAND:READ-ONLY IN BROWSE BRW_SKOG = TRUE
   akervardtemp.STOLPNR:READ-ONLY IN BROWSE BRW_TRAFO  = TRUE
   volskogtemp.DIAM:READ-ONLY IN BROWSE BRW_VOLYM = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT  BRW_AKER:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT  BRW_KAB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT  BRW_SKOG:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT  BRW_TRAFO:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT  BRW_VOLYM:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[6]
      (INPUT  BRW_REV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[7]
      (INPUT  BRW_TILLFS:HANDLE IN FRAME {&FRAME-NAME}).    
      
    RUN setcolindex_UI IN brwproc[5] (INPUT "THKL BY GHKL by DIAM").   
      
   IF Guru.Konstanter:appcon THEN DO:
      RUN VARDSPECUAPP.P PERSISTENT SET vardspecapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN VARDSPECUAPP.P PERSISTENT SET vardspecapph.
   END.
   RUN laddany2_UI IN vardspecapph (INPUT valvardnr, OUTPUT TABLE volskogtemp,
                                 OUTPUT TABLE akerkabtemp,
                                 OUTPUT TABLE markfasttemp, OUTPUT TABLE markagaretemp ). 
   
   
   tthandle = TEMP-TABLE skogvardtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "SKOGVARD", INPUT " WHERE SKOGVARD.VARDNR = " + STRING(valvardnr) + " ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "SKOGVARD", INPUT " WHERE SKOGVARD.VARDNR = " + STRING(valvardnr) + " ").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   EMPTY TEMP-TABLE revtemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "REVAVTAL"                   
   inextradatatemp.HUVUDINT = valvardnr                    
   inextradatatemp.HUVUDCH = ?.                          
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FOR EACH extradatatemp :
      CREATE revtemp.
      ASSIGN
      revtemp.VARDNR = valvardnr
      revtemp.BETECKNING = extradatatemp.HUVUDCH
      revtemp.ZON = extradatatemp.SOKINT[1]
      revtemp.KRPM = extradatatemp.SOKDEC[1]
      revtemp.METER = extradatatemp.SOKDEC[2]
      revtemp.KRONOR = extradatatemp.SOKDEC[3]
      revtemp.BENAMNING = extradatatemp.SOKCHAR[1].
   END.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   tthandle = TEMP-TABLE akervardtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AKERVARD", INPUT " WHERE AKERVARD.VARDNR = " + STRING(valvardnr) + " ").
   tthandle = TEMP-TABLE aovardtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AOVARD", INPUT " WHERE AOVARD.VARDNR = " + STRING(valvardnr) + " ").
   EMPTY TEMP-TABLE kindextemp  NO-ERROR. 
   EMPTY TEMP-TABLE akerproctemp NO-ERROR.
   EMPTY TEMP-TABLE prodakertemp NO-ERROR.
   EMPTY TEMP-TABLE markdivtemp NO-ERROR.
   tthandle = TEMP-TABLE kindextemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KINDEX", INPUT "").
   tthandle = TEMP-TABLE akerproctemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AKERPROC", INPUT "").
   tthandle = TEMP-TABLE prodakertemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PRODAKER", INPUT "").
   tthandle = TEMP-TABLE markdivtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MARKDIV", INPUT "").
   
   
   
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI C-Win 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   IF RAD_VARDSO = 1 THEN DO:
      status-ok = BRW_SKOG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_SKOG.
      ASSIGN
      skogrec = RECID(skogvardtemp)
      bestandnr = skogvardtemp.BESTAND.
      vart = "AND".

      RUN andspec_UI (INPUT 1).
      kommandoq = "skogvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " 
      AND skogvardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ". 
      RUN setcolsortvar_UI IN brwproc[3] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[3]. 
      FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR =  varderingtemp.VARDNR AND 
      skogvardtemp.BETECKNING = fastvardtemp.BETECKNING AND 
      skogvardtemp.BESTAND = bestandnr USE-INDEX VARDNR NO-LOCK NO-ERROR. 
      IF AVAILABLE skogvardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(skogvardtemp)).
         RUN lastselectdyn_UI IN brwproc[3].     
      END.
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 2 THEN DO:
      status-ok = BRW_VOLYM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_VOLYM.
      DEBUGGER:SET-BREAK().
      vart = "AND".
      volskogtabrec = volskogtemp.VOLSKOGREC.

      RUN andspec_UI (INPUT 2).
      kommandoq = "volskogtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND volskogtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND volskogtemp.DIAM > 0 ".
      RUN setcolsortvar_UI IN brwproc[5] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[5]. 
      FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(volskogtemp)).
         RUN lastselectdyn_UI IN brwproc[5].                    
      END.
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 3 THEN DO:
      status-ok = BRW_AKER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_AKER.
      ASSIGN
      stpnr = akervardtemp.STOLPNR
      vart = "AND".
      RUN andspec_UI (INPUT 3).
      
       kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 0" + " AND akervardtemp.STOLPNR NE 0".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[1]. 
      /*L5 = 0 Åker L5 = 1 trafo*/
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING = fastvardtemp.BETECKNING AND akervardtemp.L5 = 0 AND 
      akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[1].           
      END.
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 4 THEN DO:
      status-ok = BRW_KAB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_KAB.
      ASSIGN
      vart = "AND"
      akerkabrec2 = akerkabtemp.AKERKABREC.
      RUN andspec_UI (INPUT 4).
     
      kommandoq = "akerkabtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akerkabtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
      RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[2]. 
      FIND FIRST akerkabtemp WHERE akerkabtemp.AKERKABREC = akerkabrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE akerkabtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(akerkabtemp)).
         RUN lastselectdyn_UI IN brwproc[2].  
      END.
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 5 THEN DO:      
      status-ok = BRW_TRAFO:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_TRAFO.
      ASSIGN
      stpnr = akervardtemp.STOLPNR
      vart = "AND".      
      RUN andspec_UI (INPUT 5).
       kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR > 0 ".
      RUN setcolsortvar_UI IN brwproc[4] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[4].                
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
      AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[4].  
      END.        
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 6 THEN DO:      
      status-ok = BRW_REV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_REV.
      ASSIGN
      stpnr = revtemp.ZON
      vart = "AND".
      RUN andspec_UI (INPUT 6).
      
       kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' " .
      RUN setcolsortvar_UI IN brwproc[6] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[6].                
      FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND
      revtemp.BETECKNING =  fastvardtemp.BETECKNING AND
      revtemp.ZON = stpnr NO-LOCK NO-ERROR.
      IF AVAILABLE revtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(revtemp)).
         RUN lastselectdyn_UI IN brwproc[6].  
      END.        
      musz = FALSE.
   END.   
   ELSE IF RAD_VARDSO = 7 THEN DO:
      status-ok = BRW_TILLFS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_TILLFS.
      ASSIGN
      stpnr = akervardtemp.STOLPNR
      vart = "AND".      
      RUN andspec_UI (INPUT 7).
      
       kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR = 0 ".
      RUN setcolsortvar_UI IN brwproc[7] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[7].                
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
      AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[7].  
      END.        
      musz = FALSE.
   END. 


   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andspec_UI C-Win 
PROCEDURE andspec_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER radvarso AS INTEGER NO-UNDO.
  
  RUN ANDVARDSPECN3.W (INPUT radvarso,INPUT valvardnr,INPUT fastvardtemp.BETECKNING,INPUT FILL-IN_VARDETID,
                         INPUT-OUTPUT akerkabrec2,INPUT-OUTPUT volskogtabrec,
                         INPUT-OUTPUT stpnr,INPUT-OUTPUT bestandnr).
     
                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI C-Win 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_VARDSO = 1 THEN DO:
      status-ok = BRW_SKOG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_SKOG.
      skogrec = RECID(skogvardtemp).
      skogrec2 = skogrec.
      MESSAGE "Vill du verkligen ta bort bestånd "
      skogvardtemp.BESTAND " för fastighet " skogvardtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av bestånd"
      UPDATE answer AS LOGICAL.
         /*{muswait.i}*/
         IF answer THEN DO:      
         status-ok = BRW_SKOG:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN skogrec2 = RECID(skogvardtemp).
         ELSE DO:          
            status-ok = BRW_SKOG:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN skogrec2 = RECID(skogvardtemp).
            ELSE skogrec2 = skogrec.
         END.
         FIND skogvardtemp WHERE RECID(skogvardtemp) = skogrec NO-LOCK NO-ERROR.     
         IF AVAILABLE skogvardtemp  THEN DO:
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "FSVOL"                   
            inextradatatemp.HUVUDINT = skogvardtemp.VARDNR
            inextradatatemp.HUVUDCH = skogvardtemp.BETECKNING
            inextradatatemp.SOKINT[1] = skogvardtemp.BESTAND.                           
            RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).  
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.            
         END.         
         FIND skogvardtemp WHERE RECID(skogvardtemp) = skogrec NO-LOCK NO-ERROR.     
         IF AVAILABLE skogvardtemp THEN DO:
            RUN bortskog IN vardspecapph (INPUT skogvardtemp.VARDNR,
                                          INPUT skogvardtemp.BETECKNING,
                                          INPUT skogvardtemp.BESTAND).            
            FIND skogvardtemp WHERE RECID(skogvardtemp) = skogrec NO-LOCK NO-ERROR.     
            FOR EACH skogvbuff WHERE skogvbuff.VARDNR = skogvardtemp.VARDNR AND skogvbuff.BETECKNING = skogvardtemp.BETECKNING AND
            skogvbuff.BESTAND = skogvardtemp.BESTAND   :            
               DELETE skogvbuff.  
            END.
         END.
      END.          
   END.   
   ELSE IF RAD_VARDSO = 2 THEN DO:
      IF AVAILABLE volskogtemp THEN DO:
         DEBUGGER:SET-BREAK().
         status-ok = BRW_VOLYM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO BRW_VOLYM.
         volrec = RECID(volskogtemp).
         volrec2 = volrec.
         MESSAGE "Vill du ta bort denna rad i volymberäkning-svara Ja," SKIP 
         "eller vill du ta bort hela stämplingsländen för "
         volskogtemp.THKL " och" volskogtemp.GHKL " för fastighet "volskogtemp.BETECKNING " -svara Nej  ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Bortag av volymberäkning" UPDATE answer2 AS LOGICAL.
         CASE answer2:
            WHEN TRUE THEN DO:            
               status-ok = BRW_VOLYM:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.
               IF status-ok = TRUE THEN volrec2 = RECID(volskogtemp).
               ELSE DO:
                  status-ok = BRW_VOLYM:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.
                  IF status-ok = TRUE THEN volrec2 = RECID(volskogtemp).
                  ELSE volrec2 = volrec.
               END.
               FIND volskogtemp WHERE RECID(volskogtemp) = volrec NO-LOCK NO-ERROR.
               IF AVAILABLE volskogtemp THEN DO:
                  RUN bortvol IN vardspecapph (INPUT volskogtemp.VOLSKOGREC).
                  DELETE volskogtemp.
               END.
            END.
            WHEN FALSE  THEN DO:               
               FIND volskogtemp WHERE RECID(volskogtemp) = volrec NO-LOCK NO-ERROR.
               RUN bortvolstamp IN vardspecapph (INPUT volskogtemp.VOLSKOGREC).
               FOR EACH  volskogtempbuf WHERE volskogtempbuf.VARDNR =  volskogtemp.VARDNR AND volskogtempbuf.BETECKNING =  volskogtemp.BETECKNING AND
               volskogtempbuf.GHKL =  volskogtemp.GHKL AND volskogtempbuf.THKL =  volskogtemp.THKL AND volskogtempbuf.BHKL =  volskogtemp.BHKL AND volskogtempbuf.OHKL =  volskogtemp.OHKL   EXCLUSIVE-LOCK:                                 
                  DELETE volskogtempbuf.
               END.
            
            END.
         END CASE.      
         
      END.
      /*IF AVAILABLE volskogtemp THEN DO:
         status-ok = BRW_VOLYM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO BRW_VOLYM.
         volrec = RECID(volskogtemp).
         volrec2 = volrec.
         MESSAGE "Vill du verkligen ta bort denna rad i volymberäkning "
         volskogtemp.DIAM " för fastighet "volskogtemp.BETECKNING " ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av volymberäkning"
         UPDATE answer2 AS LOGICAL.
            /*{muswait.i}*/
            IF answer2 THEN DO:
            status-ok = BRW_VOLYM:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.
            IF status-ok = TRUE THEN volrec2 = RECID(volskogtemp).
            ELSE DO:
               status-ok = BRW_VOLYM:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.
               IF status-ok = TRUE THEN volrec2 = RECID(volskogtemp).
               ELSE volrec2 = volrec.
            END.
            FIND volskogtemp WHERE RECID(volskogtemp) = volrec NO-LOCK NO-ERROR.
            IF AVAILABLE volskogtemp THEN DO:
               RUN bortvol IN vardspecapph (INPUT volskogtemp.VOLSKOGREC).
               DELETE volskogtemp.
            END.
         END.
      END.*/
   END.   
   ELSE IF RAD_VARDSO = 3 THEN DO:
      status-ok = BRW_AKER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_AKER.
      akerrec = RECID(akervardtemp).
      akerrec2 = akerrec.
      MESSAGE "Vill du verkligen ta bort denna stolpe "
      akervardtemp.STOLPNR " för fastighet "akervardtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av åkernorm"
      UPDATE answer3 AS LOGICAL.
         /*{muswait.i}*/
         IF answer3 THEN DO:      
         status-ok = BRW_AKER:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).
         ELSE DO:          
            status-ok = BRW_AKER:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).                          
            ELSE akerrec2 = akerrec.
         END.
         FIND akervardtemp WHERE RECID(akervardtemp) = akerrec NO-LOCK NO-ERROR.     
         IF AVAILABLE akervardtemp THEN DO: 
            RUN bortakervard IN vardspecapph (INPUT akervardtemp.VARDNR,
                                              INPUT akervardtemp.BETECKNING,
                                              INPUT akervardtemp.STOLPNR,
                                              INPUT 0).
            DELETE akervardtemp.  
         END.
      END.   
   END.   
   ELSE IF RAD_VARDSO = 4 THEN DO:
      status-ok = BRW_KAB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_KAB.
      ASSIGN
      akerkabrec2 = akerkabtemp.AKERKABREC
      kabrec = RECID(akerkabtemp)
      kabrec2 = kabrec.
      MESSAGE "Vill du verkligen ta bort denna kabel "
      akerkabtemp.L1 " för fastighet "akerkabtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av kabel"
      UPDATE answer4 AS LOGICAL.
         /*{muswait.i}*/
      IF answer4 THEN DO:      
         status-ok = BRW_KAB:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN kabrec2 = RECID(akerkabtemp).
         ELSE DO:          
            status-ok = BRW_KAB:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN kabrec2 = RECID(akerkabtemp).          
            ELSE kabrec2 = kabrec.
         END.
         FIND akerkabtemp WHERE akerkabtemp.AKERKABREC = akerkabrec2 NO-LOCK NO-ERROR.     
         IF AVAILABLE akerkabtemp THEN DO:
            RUN bortakerkab IN vardspecapph (INPUT akerkabtemp.AKERKABREC).
            IF akerkabtemp.FASTPRIS = FALSE THEN DO: 
               FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = akerkabtemp.VARDNR 
               AND akervardtemp.BETECKNING = akerkabtemp.BETECKNING AND akervardtemp.L5 = akerkabtemp.L1
               AND akervardtemp.STOLPNR = 0  NO-LOCK NO-ERROR. 
            END.
            ELSE DO:
               FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = akerkabtemp.VARDNR 
               AND akervardtemp.BETECKNING = akerkabtemp.BETECKNING AND akervardtemp.L5 = akerkabtemp.KRONOR
               AND akervardtemp.STOLPNR = 0  NO-LOCK NO-ERROR. 
            END.
            IF AVAILABLE akervardtemp THEN DO:               
               IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" THEN DELETE akervardtemp.
               ELSE IF akervardtemp.L5 > 1 THEN DELETE akervardtemp.
            END.
            DELETE akerkabtemp.  
         END.
      END.   
   END.   
   ELSE IF RAD_VARDSO = 5 THEN DO:
      status-ok = BRW_TRAFO:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_TRAFO.
      akerrec = RECID(akervardtemp).
      akerrec2 = akerrec.
      MESSAGE "Vill du verkligen ta bort denna station "
      akervardtemp.STOLPNR " för fastighet "akervardtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av station"
      UPDATE answer5 AS LOGICAL.
         /*{muswait.i}*/
         IF answer5 THEN DO:     
         status-ok = BRW_TRAFO:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).
         ELSE DO:          
            status-ok = BRW_TRAFO:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).          
            ELSE akerrec2 = akerrec.
         END.
         FIND FIRST akervardtemp WHERE RECID(akervardtemp) = akerrec NO-LOCK NO-ERROR.     
         IF AVAILABLE akervardtemp THEN DO:
            RUN bortakervard IN vardspecapph (INPUT akervardtemp.VARDNR,
                                              INPUT akervardtemp.BETECKNING,
                                              INPUT akervardtemp.STOLPNR,
                                              INPUT 1).
            DELETE akervardtemp.  
         END.
      END.   
   END.   
   ELSE IF RAD_VARDSO = 6 THEN DO:
      status-ok = BRW_REV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_REV.
      akerrec = RECID(revtemp).
      akerrec2 = akerrec.
      MESSAGE "Vill du verkligen ta bort denna registrering  Rev Zon "
      revtemp.ZON " för fastighet "revtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av Rev-avtal"
      UPDATE answer6 AS LOGICAL.         
         IF answer6 THEN DO:     
         status-ok = BRW_REV:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN akerrec2 = RECID(revtemp).
         ELSE DO:          
            status-ok = BRW_REV:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN akerrec2 = RECID(revtemp).          
            ELSE akerrec2 = akerrec.
         END.
         FIND FIRST revtemp WHERE RECID(revtemp) = akerrec NO-LOCK NO-ERROR.     
         IF AVAILABLE revtemp THEN DO:
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "REVAVTAL"                   
            inextradatatemp.HUVUDINT = revtemp.VARDNR
            inextradatatemp.HUVUDCH = revtemp.BETECKNING
            inextradatatemp.SOKINT[1] = revtemp.ZON.                           
            RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).  
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.                      
            FIND FIRST revtemp WHERE RECID(revtemp) = akerrec NO-ERROR.     
            IF AVAILABLE revtemp THEN DELETE revtemp.  
         END.
      END.   
   END.
   ELSE IF RAD_VARDSO = 7 THEN DO:
      status-ok = BRW_TRAFO:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_TRAFO.
      akerrec = RECID(akervardtemp).
      akerrec2 = akerrec.
      MESSAGE "Vill du verkligen ta bort denna tillfälliga skada "
      akervardtemp.STOLPNR " för fastighet "akervardtemp.BETECKNING " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av tillfällig skada"
      UPDATE answer7 AS LOGICAL.
         /*{muswait.i}*/
         IF answer7 THEN DO:     
         status-ok = BRW_TILLFS:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).
         ELSE DO:          
            status-ok = BRW_TILLFS:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN akerrec2 = RECID(akervardtemp).          
            ELSE akerrec2 = akerrec.
         END.
         FIND FIRST akervardtemp WHERE RECID(akervardtemp) = akerrec NO-LOCK NO-ERROR.     
         IF AVAILABLE akervardtemp THEN DO:
            RUN bortakervard IN vardspecapph (INPUT akervardtemp.VARDNR,
                                              INPUT akervardtemp.BETECKNING,
                                              INPUT akervardtemp.STOLPNR,
                                              INPUT 1).
            DELETE akervardtemp.  
         END.
      END.   
   END.   
   RUN oppnabr_UI.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_VARDNR FILL-IN_BENAMNING FILL-IN_AONR FILL-IN_DELNR RAD_VARDSO 
          FILL-IN-PRODOMR CMB_FAST FILL-IN_VARDETID FILL-IN_FORDYRAD FILL-IN-M3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_VARDSO CMB_FAST FILL-IN_FORDYRAD FILL-IN-M3 BRW_SKOG BRW_VOLYM 
         BRW_REV BRW_AKER BRW_TRAFO BRW_TILLFS BRW_KAB BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hidden_UI C-Win 
PROCEDURE hidden_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF RAD_VARDSO = 1 THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VOLYM:HIDDEN = TRUE      
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE 
      FILL-IN_FORDYRAD:HIDDEN = TRUE      
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = TRUE 
      BRW_TILLFS:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.      
   END.  
   ELSE IF RAD_VARDSO = 2  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = FALSE
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE  
      FILL-IN_FORDYRAD:HIDDEN = FALSE
      FILL-IN-M3:HIDDEN = FALSE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = TRUE
      BRW_TILLFS:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.      
   END.  
   ELSE IF RAD_VARDSO = 3  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = TRUE
      BRW_AKER:HIDDEN = FALSE
      FILL-IN_VARDETID:HIDDEN = FALSE
      FILL-IN_FORDYRAD:HIDDEN = TRUE  
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = FALSE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = TRUE
      BRW_TILLFS:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.
   END.  
   ELSE IF RAD_VARDSO = 4  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = TRUE
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE 
      FILL-IN_FORDYRAD:HIDDEN = TRUE 
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = FALSE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.
   END.  
   ELSE IF RAD_VARDSO = 5  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = TRUE
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE 
      FILL-IN_FORDYRAD:HIDDEN = TRUE 
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = FALSE
      BRW_REV:HIDDEN = TRUE
      BRW_TILLFS:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.
   END.  
   ELSE IF RAD_VARDSO = 6  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = TRUE
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE 
      FILL-IN_FORDYRAD:HIDDEN = TRUE 
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = FALSE
      BRW_TILLFS:HIDDEN = TRUE
      FBTN_VISKAL:HIDDEN = TRUE
      FBTN_SKR:HIDDEN = TRUE.
   END.
   ELSE IF RAD_VARDSO = 7  THEN DO:
      ASSIGN BRW_SKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOLYM:HIDDEN = TRUE
      BRW_AKER:HIDDEN = TRUE
      FILL-IN_VARDETID:HIDDEN = TRUE 
      FILL-IN_FORDYRAD:HIDDEN = TRUE 
      FILL-IN-M3:HIDDEN = TRUE
      FILL-IN-PRODOMR:HIDDEN = TRUE
      BRW_KAB:HIDDEN = TRUE
      BRW_TRAFO:HIDDEN = TRUE
      BRW_REV:HIDDEN = TRUE
      BRW_TILLFS:HIDDEN = FALSE
      FBTN_VISKAL:HIDDEN = FALSE
      FBTN_SKR:HIDDEN = FALSE.
   END.  
   /*DISPLAY FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyreg_UI C-Win 
PROCEDURE nyreg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_VARDSO = 1 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "skogvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " 
         AND skogvardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ". 
         RUN setcolsortvar_UI IN brwproc[3] (INPUT kommandoq).                        
         RUN openbdynspec_UI IN brwproc[3].                                           
         FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR =  varderingtemp.VARDNR AND 
         skogvardtemp.BETECKNING =  fastvardtemp.BETECKNING AND 
         skogvardtemp.BESTAND = bestandnr USE-INDEX VARDNR NO-LOCK NO-ERROR. 
         IF AVAILABLE skogvardtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(skogvardtemp)).
            RUN lastselectdyn_UI IN brwproc[3].     
         END.
      END.
      IF musz = TRUE THEN DO:
         FIND FIRST skogvardtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE skogvardtemp THEN DO:               
         END.
         ELSE DO:         
            kommandoq = "skogvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " 
            AND skogvardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ". 
            RUN setcolsortvar_UI IN brwproc[3] (INPUT kommandoq).                        
            RUN openbdynspec_UI IN brwproc[3].                                           
            FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR =  varderingtemp.VARDNR AND 
            skogvardtemp.BETECKNING =  fastvardtemp.BETECKNING AND 
            skogvardtemp.BESTAND = bestandnr USE-INDEX VARDNR NO-LOCK NO-ERROR. 
            IF AVAILABLE skogvardtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(skogvardtemp)).
               RUN lastselectdyn_UI IN brwproc[3].     
            END.    
         END.
      END.
   END.   
   ELSE IF RAD_VARDSO = 2 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "volskogtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND volskogtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND volskogtemp.DIAM > 0 ".
         RUN setcolsortvar_UI IN brwproc[5] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[5]. 
         FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec NO-LOCK NO-ERROR.
         IF AVAILABLE volskogtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(volskogtemp)).
            RUN lastselectdyn_UI IN brwproc[5].   
            DISABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.          
         END.
      END.
      IF musz = TRUE THEN DO TRANSACTION:
         FIND FIRST volskogtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE volskogtemp THEN DO:   
            
         END.
         ELSE DO:         
            kommandoq = "volskogtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
            " AND volskogtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND volskogtemp.DIAM > 0 ".
            RUN setcolsortvar_UI IN brwproc[5] (INPUT kommandoq).        
            RUN openbdynspec_UI IN brwproc[5]. 
            FIND FIRST volskogtemp NO-LOCK NO-ERROR.
            IF AVAILABLE volskogtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(volskogtemp)).
               RUN lastselectdyn_UI IN brwproc[5].            
            END. 
         END.
      END.
   END.   
   ELSE IF RAD_VARDSO = 3 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 0 " + " AND akervardtemp.STOLPNR NE 0".         
         RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[1]. 
         /*L5 = 0 åker L5 = 1 trafo*/
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
         akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.L5 = 0 AND 
         akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
         IF AVAILABLE akervardtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(akervardtemp)).
            RUN lastselectdyn_UI IN brwproc[1].  
            DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
         END.
      END.
      IF musz = TRUE THEN DO TRANSACTION:
         FIND FIRST akervardtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE akervardtemp THEN DO:   
            
         END.
         ELSE DO:         
            kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
            " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 0 " + " AND akervardtemp.STOLPNR NE 0".
            RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoq).        
            RUN openbdynspec_UI IN brwproc[1].   
            /*L5 = 0 åker L5 = 1 trafo*/
            FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
            akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.L5 = 0 AND 
            akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
            IF AVAILABLE akervardtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(akervardtemp)).
               RUN lastselectdyn_UI IN brwproc[1].  
               DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
            END.
        END.
      END.
   END.   
   ELSE IF RAD_VARDSO = 4 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "akerkabtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND akerkabtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
         RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[2]. 
         FIND FIRST akerkabtemp WHERE akerkabtemp.AKERKABREC = akerkabrec2 NO-LOCK NO-ERROR.
         IF AVAILABLE akerkabtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(akerkabtemp)).
            RUN lastselectdyn_UI IN brwproc[2].  
         END.
      END.
      IF musz = TRUE THEN DO TRANSACTION:
         FIND FIRST akerkabtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE akerkabtemp THEN DO:   
            
         END.
         ELSE DO:         
            kommandoq = "akerkabtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
            " AND akerkabtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
            RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).        
            RUN openbdynspec_UI IN brwproc[2].                                            
            FIND FIRST akerkabtemp WHERE akerkabtemp.AKERKABREC = akerkabrec2 NO-LOCK NO-ERROR.
            IF AVAILABLE akerkabtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(akerkabtemp)).
               RUN lastselectdyn_UI IN brwproc[2].  
            END.               
        END.
      END.
   END.   
   ELSE IF RAD_VARDSO = 5 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
         + " AND akervardtemp.STOLPNR > 0 ".
         RUN setcolsortvar_UI IN brwproc[4] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[4]. 
         /*L5 = 0 åker L5 = 1 trafo*/     
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
         akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
         AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = stpnr NO-LOCK NO-ERROR.
         IF AVAILABLE akervardtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(akervardtemp)).
            RUN lastselectdyn_UI IN brwproc[4].  
         END.        
      END.
      IF musz = TRUE THEN DO TRANSACTION:         
         FIND FIRST akervardtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE akervardtemp THEN DO:            
            
         END.
         ELSE DO:         
            akerrec = akerrec2.
             kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
            " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
            + " AND akervardtemp.STOLPNR > 0 ".
            RUN setcolsortvar_UI IN brwproc[4] (INPUT kommandoq).
            RUN openbdynspec_UI IN brwproc[4].
            /*L5 = 0 åker L5 = 1 trafo*/       
           FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
            akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
            AND akervardtemp.L5 = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE akervardtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(akervardtemp)).
               RUN lastselectdyn_UI IN brwproc[4].  
            END.  
        END.
      END.
   END.
   ELSE IF RAD_VARDSO = 6 THEN DO:
      IF musz = FALSE THEN DO:  
         kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
         
         RUN setcolsortvar_UI IN brwproc[6] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[6]. 
         
         FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND
         revtemp.BETECKNING =  fastvardtemp.BETECKNING AND revtemp.ZON = stpnr NO-LOCK NO-ERROR.
         IF AVAILABLE revtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(revtemp)).
            RUN lastselectdyn_UI IN brwproc[6].  
         END.        
      END.
      IF musz = TRUE THEN DO TRANSACTION:         
         FIND FIRST revtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE revtemp THEN DO:            
            
         END.
         ELSE DO:         
            akerrec = akerrec2.
             kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
         
            RUN setcolsortvar_UI IN brwproc[6] (INPUT kommandoq).
            RUN openbdynspec_UI IN brwproc[6].
           
           FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND
           revtemp.BETECKNING =  fastvardtemp.BETECKNING NO-LOCK NO-ERROR.
            IF AVAILABLE revtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(revtemp)).
               RUN lastselectdyn_UI IN brwproc[6].  
            END.  
        END.
      END.
   END.
   ELSE IF RAD_VARDSO = 7 THEN DO:       
      IF musz = FALSE THEN DO:  
         kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
         " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
         + " AND akervardtemp.STOLPNR = 0 ".
         RUN setcolsortvar_UI IN brwproc[7] (INPUT kommandoq).        
         RUN openbdynspec_UI IN brwproc[7]. 
         /* L5 = 1 trafo  stolpnr = 0  tillfälligskada*/     
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
         akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
         AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = 0 NO-LOCK NO-ERROR.
         IF AVAILABLE akervardtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(akervardtemp)).
            RUN lastselectdyn_UI IN brwproc[7].  
         END.        
      END.
      IF musz = TRUE THEN DO TRANSACTION:         
         FIND FIRST akervardtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE akervardtemp THEN DO:            
            
         END.
         ELSE DO:         
            akerrec = akerrec2.
             kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
            " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
            + " AND akervardtemp.STOLPNR = 0 ".
            RUN setcolsortvar_UI IN brwproc[7] (INPUT kommandoq).
            RUN openbdynspec_UI IN brwproc[7].
            /* L5 = 1 trafo  stolpnr = 0  tillfälligskada*/       
           FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
            akervardtemp.BETECKNING =  fastvardtemp.BETECKNING
            AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = 0 NO-LOCK NO-ERROR.
            IF AVAILABLE akervardtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(akervardtemp)).
               RUN lastselectdyn_UI IN brwproc[7].  
            END.  
        END.
      END.
   END.

   musz = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppnabr_UI C-Win 
PROCEDURE oppnabr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_VARDSO = 1 THEN DO:
      kommandoq = "skogvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " 
      AND skogvardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ". 
      RUN setcolsortvar_UI IN brwproc[3] (INPUT kommandoq).                        
      RUN openbdynspec_UI IN brwproc[3]. 
      musz = TRUE.
      FIND FIRST skogvardtemp WHERE RECID(skogvardtemp) = skogrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE skogvardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(skogvardtemp)).
         RUN lastselectdyn_UI IN brwproc[3]. 
         musz = FALSE.   
      END.     
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE.         
      END. 
      ELSE DO:
         ENABLE BRW_SKOG WITH FRAME {&FRAME-NAME}.         
      END. 
    END.
    ELSE IF RAD_VARDSO = 2 THEN DO:
       kommandoq = "volskogtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND volskogtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND volskogtemp.DIAM > 0 ".
      RUN setcolsortvar_UI IN brwproc[5] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[5]. 
      musz = TRUE.      
      FIND FIRST volskogtemp  WHERE RECID(volskogtemp) = volrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(volskogtemp)).
         RUN lastselectdyn_UI IN brwproc[5]. 
         musz = FALSE.      
      END.     
      IF musz = TRUE THEN DO:         
         ASSIGN
         musz = FALSE.         
         ENABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.  
         FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
         volskogtemp.BETECKNING = fastvardtemp.BETECKNING AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
         IF AVAILABLE volskogtemp THEN DO:
            
         END.
       /*  ENABLE FILL-IN_FORDYRAD.         */
      END. 
      ELSE DO:
         ENABLE BRW_VOLYM WITH FRAME {&FRAME-NAME}.
         
      END. 
    END.
    ELSE IF RAD_VARDSO = 3 THEN DO:
      kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 0 " + " AND akervardtemp.STOLPNR NE 0".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[1]. 
      musz = TRUE. 
      FIND FIRST akervardtemp  WHERE RECID(akervardtemp) = akerrec2 NO-LOCK NO-ERROR.  
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[1].  
         musz = FALSE.      
      END. 
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE
         FILL-IN_VARDETID = "".         
         ENABLE FILL-IN_VARDETID.
         DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
      END. 
      ELSE DO:
         ENABLE BRW_AKER WITH FRAME {&FRAME-NAME}.
         DISABLE FILL-IN_VARDETID.
         ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(akervardtemp.VARDEPKT),1,2) + SUBSTRING(STRING(akervardtemp.VARDEPKT),4,2).
         DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}. 
         
      END. 
   END.
   ELSE IF RAD_VARDSO = 4 THEN DO:
      kommandoq = "akerkabtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akerkabtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
      RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[2].  
      musz = TRUE.
      FIND FIRST akerkabtemp WHERE RECID(akerkabtemp) = kabrec2 NO-LOCK NO-ERROR. 
      IF AVAILABLE akerkabtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(akerkabtemp)).
         RUN lastselectdyn_UI IN brwproc[2]. 
         musz = FALSE.      
      END.     
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE.         
      END. 
      ELSE DO:
         ENABLE BRW_KAB WITH FRAME {&FRAME-NAME}.         
      END. 
   END.
   ELSE IF RAD_VARDSO = 5 THEN DO:
       kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR > 0 ".
      RUN setcolsortvar_UI IN brwproc[4] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[4].
      musz = TRUE.  
      FIND FIRST akervardtemp WHERE RECID(akervardtemp) = akerrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[4]. 
         musz = FALSE.      
      END.     
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE.         
      END. 
      ELSE DO:
         ENABLE BRW_TRAFO WITH FRAME {&FRAME-NAME}.         
      END. 
    END.
    ELSE IF RAD_VARDSO = 6 THEN DO:
       kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
      RUN setcolsortvar_UI IN brwproc[6] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[6].
      musz = TRUE.  
      FIND FIRST revtemp WHERE RECID(revtemp) = akerrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE revtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(revtemp)).
         RUN lastselectdyn_UI IN brwproc[6]. 
         musz = FALSE.      
      END.     
      IF musz = TRUE THEN ASSIGN musz = FALSE.               
      ELSE ENABLE BRW_REV WITH FRAME {&FRAME-NAME}.               
    END.
    ELSE IF RAD_VARDSO = 7 THEN DO:
       kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR = 0 ".
      RUN setcolsortvar_UI IN brwproc[7] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[7].
      musz = TRUE.  
      FIND FIRST akervardtemp WHERE RECID(akervardtemp) = akerrec2 NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(akervardtemp)).
         RUN lastselectdyn_UI IN brwproc[7]. 
         musz = FALSE.      
      END.     
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE.         
      END. 
      ELSE DO:
         ENABLE BRW_TILLFS WITH FRAME {&FRAME-NAME}.         
      END. 
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
   
   IF RAD_VARDSO = 1 THEN DO:
      kommandoq = "skogvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " 
      AND skogvardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ". 
      RUN setcolsortvar_UI IN brwproc[3] (INPUT kommandoq).                        
      RUN openbdynspec_UI IN brwproc[3].                                           
      
      FIND FIRST volskogtemp WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
      volskogtemp.BETECKNING =  fastvardtemp.BETECKNING AND volskogtemp.DIAM GE 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      /*IF AVAILABLE volskogtemp THEN DO:
         FILL-IN_FORDYRAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         
      END.
      ELSE DO:    
         FILL-IN_FORDYRAD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
         DISPLAY FILL-IN_FORDYRAD   WITH FRAME {&FRAME-NAME}.        
      END.      */
   END.
   ELSE IF RAD_VARDSO = 2 THEN DO:                    
      kommandoq = "volskogtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND volskogtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND volskogtemp.DIAM > 0 ".
      RUN setcolsortvar_UI IN brwproc[5] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[5]. 
      FIND FIRST volskogtemp WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
      volskogtemp.BETECKNING =  fastvardtemp.BETECKNING AND volskogtemp.DIAM > 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      IF AVAILABLE volskogtemp THEN DO:
         DISPLAY FILL-IN_FORDYRAD   WITH FRAME {&FRAME-NAME}.        
         DISABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.             
         FILL-IN-M3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ELSE DO:    
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "FSVOL"                   
         inextradatatemp.HUVUDINT = varderingtemp.VARDNR                    
         inextradatatemp.HUVUDCH = fastvardtemp.BETECKNING.                          
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp WHERE extradatatemp.SOKDEC[2] > 0 NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            DISPLAY FILL-IN_FORDYRAD   WITH FRAME {&FRAME-NAME}.        
            DISABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.             
            FILL-IN-M3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
         ELSE DO:         
            ENABLE FILL-IN-M3 WITH FRAME {&FRAME-NAME}.    
            DISPLAY FILL-IN_FORDYRAD FILL-IN-M3 WITH FRAME {&FRAME-NAME}.
         END.
         FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = varderingtemp.VARDNR AND
         volskogtemp.BETECKNING = fastvardtemp.BETECKNING AND volskogtemp.DIAM = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
         IF AVAILABLE volskogtemp THEN DO:            
         END.
      END.      
   END.
   ELSE IF RAD_VARDSO = 3 THEN DO:                    
      kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 0 " + " AND akervardtemp.STOLPNR NE 0".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[1]. 
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.L5 = 0 AND akervardtemp.STOLPNR NE 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      IF AVAILABLE akervardtemp THEN DO:
         ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(akervardtemp.VARDEPKT),1,2) + SUBSTRING(STRING(akervardtemp.VARDEPKT),4,2).
         DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}. 
         DISABLE FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.         
      END.
      ELSE DO:    
         FIND LAST kindextemp USE-INDEX AR NO-LOCK NO-ERROR.
         IF AVAILABLE kindextemp THEN DO:
            ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(kindextemp.AR),3,2) + STRING(kindextemp.MANAD,"99").
         END.            
         ENABLE FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
         DISPLAY FILL-IN_VARDETID  WITH FRAME {&FRAME-NAME}.
      END.      
      DISPLAY FILL-IN-PRODOMR  WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VARDSO = 4 THEN DO:  
       kommandoq = "akerkabtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + 
      " AND akerkabtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".
      RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).        
      RUN openbdynspec_UI IN brwproc[2].  
      FIND FIRST akerkabtemp WHERE akerkabtemp.VARDNR = varderingtemp.VARDNR AND
      akerkabtemp.BETECKNING =  fastvardtemp.BETECKNING USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      IF AVAILABLE akerkabtemp THEN DO:         
      END.
      ELSE DO:             
      END.      
      
   END.
   ELSE IF RAD_VARDSO = 5 THEN DO: 
      kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR > 0 ".
      RUN setcolsortvar_UI IN brwproc[4] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[4].
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR > 0   USE-INDEX VARDNR NO-LOCK NO-ERROR.
      
       ASSIGN akervardtemp.L1:VISIBLE IN BROWSE BRW_TRAFO = FALSE.
       ASSIGN akervardtemp.L2:VISIBLE IN BROWSE BRW_TRAFO = FALSE.
       ASSIGN akervardtemp.L4:VISIBLE IN BROWSE BRW_TRAFO = FALSE.
       ASSIGN akervardtemp.FASTPRIS:VISIBLE IN BROWSE BRW_TRAFO = FALSE.
          
      
   END.
   ELSE IF RAD_VARDSO = 6 THEN DO: 
      kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) + " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "'". 
      
      /*kommandoq = "revtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND revtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' ".*/
      RUN setcolsortvar_UI IN brwproc[6] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[6].
      FIND FIRST revtemp WHERE revtemp.VARDNR = varderingtemp.VARDNR AND
      revtemp.BETECKNING =  fastvardtemp.BETECKNING USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      
   END.
   ELSE IF RAD_VARDSO = 7 THEN DO: 
      kommandoq = "akervardtemp.VARDNR = " + STRING(varderingtemp.VARDNR) +
      " AND akervardtemp.BETECKNING = '" + STRING(fastvardtemp.BETECKNING) + "' AND akervardtemp.L5 = 1 " + "AND akervardtemp.KRONOR GE 0 "
      + " AND akervardtemp.STOLPNR = 0 ".
      RUN setcolsortvar_UI IN brwproc[7] (INPUT kommandoq).
      RUN openbdynspec_UI IN brwproc[7].
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
      akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.L5 = 1 AND akervardtemp.STOLPNR = 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.    
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  
   ( INPUT valbrw AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   IF valbrw = 1 THEN DO:
      IF  BRW_SKOG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.  
   IF valbrw = 2 THEN DO:
      IF  BRW_VOLYM:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.  
   IF valbrw = 3 THEN DO:
      IF  BRW_AKER:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.  
   IF valbrw = 4 THEN DO:
      IF  BRW_KAB:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.  
   IF valbrw = 5 THEN DO:
      IF  BRW_TRAFO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

