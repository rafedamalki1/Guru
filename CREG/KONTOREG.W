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
/*          This .W file was created with the Progress AppBuilder.      */
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
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{HOPPSEK2W.I}
{JURPERST.I}
DEFINE TEMP-TABLE gurutextemp  NO-UNDO
   FIELD EGENTEXT      AS CHARACTER 
   FIELD EGENTEXTFULL  AS CHARACTER
   FIELD GURUTEXT      AS CHARACTER
   FIELD PROGRAM       AS CHARACTER
   FIELD ORDNING       AS INTEGER
   INDEX ORDNING IS PRIMARY ORDNING.

DEFINE NEW SHARED VARIABLE knamnapph AS HANDLE NO-UNDO.   /*KNAMNAPP.P*/
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ktoproch AS HANDLE NO-UNDO. /* Procedure handle till KTOAPP.P */
 

DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkonto AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkontonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempjur AS CHARACTER NO-UNDO.
DEFINE VARIABLE montbervar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektledvar AS INTEGER NO-UNDO.
DEFINE VARIABLE projektorvar AS INTEGER NO-UNDO.
DEFINE VARIABLE dokuvar AS INTEGER NO-UNDO.

{TIDUTTTNEW.I}  
DEFINE NEW SHARED TEMP-TABLE ktotemp               
   FIELD KONTO AS CHARACTER                        
   FIELD KONTONR AS CHARACTER                      
   FIELD BENAMNING AS CHARACTER                    
   FIELD JUDID AS CHARACTER                        
   FIELD VIJUDID AS CHARACTER                      
   FIELD OMRADE AS CHARACTER                       
   FIELD AVDELNINGNR AS INTEGER                    
   FIELD AKTIV AS LOGICAL                          
   FIELD RECTIDVIS AS RECID                        
   INDEX KONTO KONTO KONTONR.                      
DEFINE NEW SHARED TEMP-TABLE kbtemp
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER.
DEFINE NEW SHARED TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.   

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.

DEFINE NEW SHARED VARIABLE org AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE orgnamn AS CHARACTER NO-UNDO.


DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.

DEFINE VARIABLE persproch AS HANDLE NO-UNDO.
DEFINE VARIABLE vadgora AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE borec2 AS RECID NO-UNDO.
DEFINE VARIABLE ktorec AS RECID NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE radnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE brec AS RECID NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.

{OMRTEMPW.I}
DEFINE TEMP-TABLE kstrang
   FIELD BENAMNING AS CHARACTER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD RADNR AS INTEGER
   FIELD RECTIDVIS AS RECID
   INDEX KONTO K1 K2 K3 K4 K5
   INDEX KONTO2 RADNR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define BROWSE-NAME BRW_GTEXT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES gurutextemp ktotemp kstrang

/* Definitions for BROWSE BRW_GTEXT                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GTEXT gurutextemp.GURUTEXT ~
gurutextemp.EGENTEXT gurutextemp.EGENTEXTFULL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GTEXT gurutextemp.EGENTEXT ~
gurutextemp.EGENTEXTFULL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_GTEXT gurutextemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_GTEXT gurutextemp
&Scoped-define QUERY-STRING-BRW_GTEXT FOR EACH gurutextemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_GTEXT OPEN QUERY BRW_GTEXT FOR EACH gurutextemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_GTEXT gurutextemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GTEXT gurutextemp


/* Definitions for BROWSE BRW_K-1                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-1 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-1 
&Scoped-define QUERY-STRING-BRW_K-1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-1 OPEN QUERY BRW_K-1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-1 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-1 ktotemp


/* Definitions for BROWSE BRW_K-2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-2 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-2 
&Scoped-define QUERY-STRING-BRW_K-2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-2 OPEN QUERY BRW_K-2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-2 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-2 ktotemp


/* Definitions for BROWSE BRW_K-3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-3 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-3 
&Scoped-define QUERY-STRING-BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-3 OPEN QUERY BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-3 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-3 ktotemp


/* Definitions for BROWSE BRW_K-4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-4 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-4 
&Scoped-define QUERY-STRING-BRW_K-4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-4 OPEN QUERY BRW_K-4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-4 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-4 ktotemp


/* Definitions for BROWSE BRW_K-5                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-5 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-5 
&Scoped-define QUERY-STRING-BRW_K-5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_K-5 OPEN QUERY BRW_K-5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_K-5 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-5 ktotemp


/* Definitions for BROWSE BRW_K1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K1 ktotemp.VIJUDID ktotemp.KONTONR ~
ktotemp.BENAMNING ktotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K1 ktotemp.VIJUDID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K1 ktotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K1 ktotemp
&Scoped-define QUERY-STRING-BRW_K1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K1 OPEN QUERY BRW_K1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K1 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K1 ktotemp


/* Definitions for BROWSE BRW_K2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K2 ktotemp.VIJUDID ktotemp.KONTONR ~
ktotemp.BENAMNING ktotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K2 ktotemp.VIJUDID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K2 ktotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K2 ktotemp
&Scoped-define QUERY-STRING-BRW_K2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K2 OPEN QUERY BRW_K2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K2 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K2 ktotemp


/* Definitions for BROWSE BRW_K3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K3 ktotemp.VIJUDID ktotemp.KONTONR ~
ktotemp.BENAMNING ktotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K3 ktotemp.VIJUDID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K3 ktotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K3 ktotemp
&Scoped-define QUERY-STRING-BRW_K3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K3 OPEN QUERY BRW_K3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K3 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K3 ktotemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 ktotemp.VIJUDID ktotemp.KONTONR ~
ktotemp.BENAMNING ktotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 ktotemp.VIJUDID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K4 ktotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K4 ktotemp
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 ktotemp


/* Definitions for BROWSE BRW_K5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K5 ktotemp.VIJUDID ktotemp.KONTONR ~
ktotemp.BENAMNING ktotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K5 ktotemp.VIJUDID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K5 ktotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K5 ktotemp
&Scoped-define QUERY-STRING-BRW_K5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K5 OPEN QUERY BRW_K5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K5 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K5 ktotemp


/* Definitions for BROWSE BRW_STR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_STR kstrang.BENAMNING kstrang.K1 ~
kstrang.K2 kstrang.K3 kstrang.K4 kstrang.K5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STR kstrang.BENAMNING 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STR kstrang
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STR kstrang
&Scoped-define QUERY-STRING-BRW_STR FOR EACH kstrang NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STR OPEN QUERY BRW_STR FOR EACH kstrang NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STR kstrang
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STR kstrang


/* Definitions for FRAME FRAME-KONTO                                    */

/* Definitions for FRAME FRAME-NAMN                                     */

/* Definitions for FRAME FRAME-STRANG                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_KTONAMN MBTN_KTO MBTN_KTOSTRANG RAD_VAL ~
RECT-25 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON MBTN_KTO 
     LABEL "Konto" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_KTONAMN 
     LABEL "Kontonamn" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_KTOSTRANG 
     LABEL "Kontosträng" 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "konto", 1
     SIZE 43.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109.5 BY .08.

DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB-KONTO AUTO-GO 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_KONTO AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 15.5 BY 9.5 NO-UNDO.

DEFINE BUTTON BTN_AVB-NAMN AUTO-GO 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Spara":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-DOKU AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dokumentation" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORDELB AS CHARACTER FORMAT "X(256)":U INITIAL "Fördelningsnyckel beredare i %:" 
      VIEW-AS TEXT 
     SIZE 30.88 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-FORDELM AS CHARACTER FORMAT "X(256)":U INITIAL "Fördelningsnyckel montör i %:" 
      VIEW-AS TEXT 
     SIZE 30 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MONTBER AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Montör" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKTLED AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Projektledare" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKTOR AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Projektör" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ATRHOME AS CHARACTER FORMAT "X(256)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1.

DEFINE VARIABLE FILL-IN_K1 AS CHARACTER FORMAT "x(6)" 
     LABEL "Fält 1" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_K2 AS CHARACTER FORMAT "x(6)" 
     LABEL "Fält 2" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_K3 AS CHARACTER FORMAT "x(6)" 
     LABEL "Fält 3" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_K4 AS CHARACTER FORMAT "x(6)" 
     LABEL "Fält 4" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_K5 AS CHARACTER FORMAT "x(6)" 
     LABEL "Fält 5" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE BUTTON BTN_AVB-STRANG AUTO-GO 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT-2 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KONTO 
     IMAGE-UP FILE "BILDER\export-d":U
     LABEL "Btn 9":L 
     SIZE 4 BY 1.5.

DEFINE BUTTON FBTN_SKR-2 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER-2 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18.75 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K1 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K2 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K3 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K4 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K5 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE VARIABLE SEL_OMR AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 19 BY 7.5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_GTEXT FOR 
      gurutextemp SCROLLING.

DEFINE QUERY BRW_K-1 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-2 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-3 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-4 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-5 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K1 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K5 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_STR FOR 
      kstrang SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_GTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GTEXT C-Win _STRUCTURED
  QUERY BRW_GTEXT NO-LOCK DISPLAY
      gurutextemp.GURUTEXT COLUMN-LABEL "Det som i Guru kallas" FORMAT "X(21)":U
      gurutextemp.EGENTEXT COLUMN-LABEL "Egen text" FORMAT "X(21)":U
      gurutextemp.EGENTEXTFULL COLUMN-LABEL "Hela texten" FORMAT "X(256)":U
            WIDTH 51.38
  ENABLE
      gurutextemp.EGENTEXT HELP "Texten utan för kortningar"
      gurutextemp.EGENTEXTFULL HELP "Texten utan för kortningar"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 90.5 BY 14.83.

DEFINE BROWSE BRW_K-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-1 C-Win _STRUCTURED
  QUERY BRW_K-1 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9.

DEFINE BROWSE BRW_K-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-2 C-Win _STRUCTURED
  QUERY BRW_K-2 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9.

DEFINE BROWSE BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-3 C-Win _STRUCTURED
  QUERY BRW_K-3 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9.

DEFINE BROWSE BRW_K-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-4 C-Win _STRUCTURED
  QUERY BRW_K-4 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-5 C-Win _STRUCTURED
  QUERY BRW_K-5 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "" FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 11.75 BY 9 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 C-Win _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      ktotemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(256)":U WIDTH 9
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      ktotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      ktotemp.VIJUDID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50 BY 22.25.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 C-Win _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      ktotemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(256)":U WIDTH 9
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      ktotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      ktotemp.VIJUDID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50 BY 22.25.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 C-Win _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      ktotemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(256)":U WIDTH 9
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      ktotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      ktotemp.VIJUDID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50 BY 22.25.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 C-Win _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      ktotemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(256)":U WIDTH 9
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      ktotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      ktotemp.VIJUDID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50 BY 22.25.

DEFINE BROWSE BRW_K5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K5 C-Win _STRUCTURED
  QUERY BRW_K5 NO-LOCK DISPLAY
      ktotemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(256)":U WIDTH 9
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      ktotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      ktotemp.VIJUDID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50 BY 22.25.

DEFINE BROWSE BRW_STR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STR C-Win _STRUCTURED
  QUERY BRW_STR NO-LOCK DISPLAY
      kstrang.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 40
      kstrang.K1 FORMAT "X(256)":U WIDTH 9
      kstrang.K2 FORMAT "X(256)":U WIDTH 9
      kstrang.K3 FORMAT "X(256)":U WIDTH 9
      kstrang.K4 FORMAT "X(256)":U WIDTH 9
      kstrang.K5 FORMAT "X(256)":U WIDTH 9
  ENABLE
      kstrang.BENAMNING
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 86.25 BY 11.25
         TITLE "Färdiga kontosträngar för område".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-GRUND
     MBTN_KTONAMN AT ROW 1.08 COL 1
     MBTN_KTO AT ROW 1.08 COL 15
     MBTN_KTOSTRANG AT ROW 1.08 COL 29
     RAD_VAL AT ROW 1.08 COL 66.13 NO-LABEL
     RECT-25 AT ROW 2.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.63 BY 26.04.

DEFINE FRAME FRAME-NAMN
     FILL-IN-MONTBER AT ROW 1.13 COL 102.75 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-PROJEKTLED AT ROW 2.21 COL 102.75 COLON-ALIGNED WIDGET-ID 4
     FILL-IN_ATRHOME AT ROW 2.75 COL 1.5 NO-LABEL
     FILL-IN-PROJEKTOR AT ROW 5.25 COL 102.75 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-DOKU AT ROW 6.33 COL 102.75 COLON-ALIGNED WIDGET-ID 8
     FILL-IN_K1 AT ROW 7.08 COL 8 COLON-ALIGNED
     FILL-IN_K2 AT ROW 7.08 COL 24.5 COLON-ALIGNED
     FILL-IN_K3 AT ROW 7.08 COL 41 COLON-ALIGNED
     FILL-IN_K4 AT ROW 7.08 COL 57.5 COLON-ALIGNED
     FILL-IN_K5 AT ROW 7.08 COL 74 COLON-ALIGNED
     BRW_GTEXT AT ROW 10.17 COL 1.5
     BTN_OK AT ROW 12 COL 95.38
     BTN_AVB-NAMN AT ROW 24 COL 95.38
     FILL-IN-FORDELM AT ROW 1.33 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FILL-IN-FORDELB AT ROW 5.5 COL 59.63 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     "Uppdatera benämningarna till kontofälten" VIEW-AS TEXT
          SIZE 45.5 BY 1 AT ROW 5.21 COL 1.5
          FONT 17
     "Rubrik i Guru huvudmeny" VIEW-AS TEXT
          SIZE 47 BY 1.13 AT ROW 1.13 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 109 BY 24.5.

DEFINE FRAME FRAME-KONTO
     RAD_KONTO AT ROW 1.5 COL 1.5 NO-LABEL
     BRW_K2 AT ROW 1.5 COL 24
     BRW_K3 AT ROW 1.5 COL 24
     BRW_K4 AT ROW 1.5 COL 24
     BRW_K5 AT ROW 1.5 COL 24
     BRW_K1 AT ROW 1.5 COL 24
     FBTN_VPER AT ROW 8 COL 95.38
     FBTN_SKR AT ROW 9.08 COL 95.38
     BTN_NY AT ROW 24 COL 28
     BTN_AND AT ROW 24 COL 43
     BTN_BORT AT ROW 24 COL 58
     BTN_AVB-KONTO AT ROW 24 COL 95.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 109 BY 24.25.

DEFINE FRAME FRAME-STRANG
     FILL-IN-VALJ AT ROW 1.54 COL 1.5 NO-LABEL
     BRW_STR AT ROW 1.54 COL 23.25
     SEL_OMR AT ROW 2.63 COL 1.5 NO-LABEL
     BTN_KONTO AT ROW 10.58 COL 14.5
     BTN_BORT-2 AT ROW 12.92 COL 41.38
     FBTN_VPER-2 AT ROW 12.92 COL 55.5
     FBTN_SKR-2 AT ROW 12.92 COL 71.5
     BRW_K-1 AT ROW 14.83 COL 23.25
     BRW_K-2 AT ROW 14.83 COL 35.38
     BRW_K-3 AT ROW 14.83 COL 47.5
     BRW_K-4 AT ROW 14.83 COL 59.63
     BRW_K-5 AT ROW 14.83 COL 71.75
     SEL_K1 AT ROW 16.46 COL 23.13 NO-LABEL
     SEL_K2 AT ROW 16.46 COL 35.63 NO-LABEL
     SEL_K3 AT ROW 16.46 COL 47.63 NO-LABEL
     SEL_K4 AT ROW 16.46 COL 59.63 NO-LABEL
     SEL_K5 AT ROW 16.46 COL 71.63 NO-LABEL
     BTN_AVB-STRANG AT ROW 24 COL 95.38
     FILL-IN_K1 AT ROW 15.46 COL 21.63 COLON-ALIGNED NO-LABEL FORMAT "x(6)"
           VIEW-AS TEXT 
          SIZE 11.5 BY 1
     FILL-IN_K2 AT ROW 15.46 COL 33.63 COLON-ALIGNED NO-LABEL FORMAT "x(6)"
           VIEW-AS TEXT 
          SIZE 11.5 BY 1
     FILL-IN_K3 AT ROW 15.46 COL 45.63 COLON-ALIGNED NO-LABEL FORMAT "x(6)"
           VIEW-AS TEXT 
          SIZE 11.5 BY 1
     FILL-IN_K4 AT ROW 15.46 COL 57.63 COLON-ALIGNED NO-LABEL FORMAT "x(6)"
           VIEW-AS TEXT 
          SIZE 11.5 BY 1
     FILL-IN_K5 AT ROW 15.46 COL 69.63 COLON-ALIGNED NO-LABEL FORMAT "x(6)"
           VIEW-AS TEXT 
          SIZE 11.5 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.5
         SIZE 109 BY 24.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: gurutextemp T "?" NO-UNDO temp-db gurutextemp
      TABLE: kstrang T "?" NO-UNDO temp-db kstrang
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 26.13
         WIDTH              = 109.88
         MAX-HEIGHT         = 29.83
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 29.83
         VIRTUAL-WIDTH      = 114
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KONTO:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-NAMN:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-STRANG:FRAME = FRAME FRAME-GRUND:HANDLE.

/* SETTINGS FOR FRAME FRAME-GRUND
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME FRAME-GRUND:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME FRAME-GRUND
   NO-DISPLAY                                                           */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME FRAME-GRUND           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONTO
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_K2 RAD_KONTO FRAME-KONTO */
/* BROWSE-TAB BRW_K3 BRW_K2 FRAME-KONTO */
/* BROWSE-TAB BRW_K4 BRW_K3 FRAME-KONTO */
/* BROWSE-TAB BRW_K5 BRW_K4 FRAME-KONTO */
/* BROWSE-TAB BRW_K1 BRW_K5 FRAME-KONTO */
ASSIGN 
       FRAME FRAME-KONTO:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_KONTO IN FRAME FRAME-KONTO
   SHARED                                                               */
/* SETTINGS FOR FRAME FRAME-NAMN
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_GTEXT FILL-IN_K5 FRAME-NAMN */
ASSIGN 
       FRAME FRAME-NAMN:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DOKU IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-DOKU:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FORDELB IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-FORDELB:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FORDELM IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-FORDELM:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MONTBER IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-MONTBER:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PROJEKTLED IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-PROJEKTLED:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PROJEKTOR IN FRAME FRAME-NAMN
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-PROJEKTOR:HIDDEN IN FRAME FRAME-NAMN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ATRHOME IN FRAME FRAME-NAMN
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-STRANG
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_STR FILL-IN-VALJ FRAME-STRANG */
/* BROWSE-TAB BRW_K-1 FBTN_SKR-2 FRAME-STRANG */
/* BROWSE-TAB BRW_K-2 BRW_K-1 FRAME-STRANG */
/* BROWSE-TAB BRW_K-3 BRW_K-2 FRAME-STRANG */
/* BROWSE-TAB BRW_K-4 BRW_K-3 FRAME-STRANG */
/* BROWSE-TAB BRW_K-5 BRW_K-4 FRAME-STRANG */
ASSIGN 
       FRAME FRAME-STRANG:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME FRAME-STRANG
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_K1 IN FRAME FRAME-STRANG
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_K1:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_K2 IN FRAME FRAME-STRANG
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_K2:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_K3 IN FRAME FRAME-STRANG
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_K3:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_K4 IN FRAME FRAME-STRANG
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_K4:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_K5 IN FRAME FRAME-STRANG
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_K5:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K1 IN FRAME FRAME-STRANG
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K1:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K2 IN FRAME FRAME-STRANG
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K2:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K3 IN FRAME FRAME-STRANG
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K3:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K4 IN FRAME FRAME-STRANG
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K4:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K5 IN FRAME FRAME-STRANG
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K5:HIDDEN IN FRAME FRAME-STRANG           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GTEXT
/* Query rebuild information for BROWSE BRW_GTEXT
     _TblList          = "Temp-Tables.gurutextemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.gurutextemp.GURUTEXT
"gurutextemp.GURUTEXT" "Det som i Guru kallas" "X(21)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.gurutextemp.EGENTEXT
"gurutextemp.EGENTEXT" "Egen text" "X(21)" "character" ? ? ? ? ? ? yes "Texten utan för kortningar" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.gurutextemp.EGENTEXTFULL
"gurutextemp.EGENTEXTFULL" "Hela texten" "X(256)" "character" ? ? ? ? ? ? yes "Texten utan för kortningar" no no "51.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GTEXT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-1
/* Query rebuild information for BROWSE BRW_K-1
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k1"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-2
/* Query rebuild information for BROWSE BRW_K-2
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k2"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-3
/* Query rebuild information for BROWSE BRW_K-3
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k3"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-4
/* Query rebuild information for BROWSE BRW_K-4
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k4"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-5
/* Query rebuild information for BROWSE BRW_K-5
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k5"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k1"""
     _FldNameList[1]   > Temp-Tables.ktotemp.VIJUDID
"ktotemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ktotemp.AKTIV
"ktotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k2"""
     _FldNameList[1]   > Temp-Tables.ktotemp.VIJUDID
"ktotemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ktotemp.AKTIV
"ktotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k3"""
     _FldNameList[1]   > Temp-Tables.ktotemp.VIJUDID
"ktotemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ktotemp.AKTIV
"ktotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k4"""
     _FldNameList[1]   > Temp-Tables.ktotemp.VIJUDID
"ktotemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ktotemp.AKTIV
"ktotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K5
/* Query rebuild information for BROWSE BRW_K5
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k5"""
     _FldNameList[1]   > Temp-Tables.ktotemp.VIJUDID
"ktotemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ktotemp.AKTIV
"ktotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STR
/* Query rebuild information for BROWSE BRW_STR
     _TblList          = "Temp-Tables.kstrang"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kstrang.BENAMNING
"kstrang.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kstrang.K1
"kstrang.K1" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kstrang.K2
"kstrang.K2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kstrang.K3
"kstrang.K3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kstrang.K4
"kstrang.K4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kstrang.K5
"kstrang.K5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KONTO
/* Query rebuild information for FRAME FRAME-KONTO
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KONTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-NAMN
/* Query rebuild information for FRAME FRAME-NAMN
     _Query            is NOT OPENED
*/  /* FRAME FRAME-NAMN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-STRANG
/* Query rebuild information for FRAME FRAME-STRANG
     _Query            is NOT OPENED
*/  /* FRAME FRAME-STRANG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_GTEXT
&Scoped-define FRAME-NAME FRAME-NAMN
&Scoped-define SELF-NAME BRW_GTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_GTEXT C-Win
ON ROW-DISPLAY OF BRW_GTEXT IN FRAME FRAME-NAMN
DO:
   RUN format_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gurutextemp.EGENTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gurutextemp.EGENTEXT BRW_GTEXT _BROWSE-COLUMN C-Win
ON ENTRY OF gurutextemp.EGENTEXT IN BROWSE BRW_GTEXT /* Egen text */
DO:
   RUN format_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-1
&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME BRW_K-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-1 C-Win
ON VALUE-CHANGED OF BRW_K-1 IN FRAME FRAME-STRANG
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   SEL_K1 = ktotemp.kontonr.   
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-2
&Scoped-define SELF-NAME BRW_K-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-2 C-Win
ON VALUE-CHANGED OF BRW_K-2 IN FRAME FRAME-STRANG
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   SEL_K2 = ktotemp.kontonr.
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-3
&Scoped-define SELF-NAME BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-3 C-Win
ON VALUE-CHANGED OF BRW_K-3 IN FRAME FRAME-STRANG
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   SEL_K3 = ktotemp.kontonr.
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-4
&Scoped-define SELF-NAME BRW_K-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-4 C-Win
ON VALUE-CHANGED OF BRW_K-4 IN FRAME FRAME-STRANG
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   SEL_K4 = ktotemp.kontonr.
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-5
&Scoped-define SELF-NAME BRW_K-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-5 C-Win
ON VALUE-CHANGED OF BRW_K-5 IN FRAME FRAME-STRANG
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.     
   SEL_K5 = ktotemp.kontonr.
   {&BROWSE-NAME}:TOOLTIP = ktotemp.BENAMNING.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_K1 IN FRAME FRAME-KONTO
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_K2 IN FRAME FRAME-KONTO
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_K4 IN FRAME FRAME-KONTO
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K5
&Scoped-define SELF-NAME BRW_K5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K5 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_K5 IN FRAME FRAME-KONTO
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STR
&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME BRW_STR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STR C-Win
ON ROW-LEAVE OF BRW_STR IN FRAME FRAME-STRANG /* Färdiga kontosträngar för område */
DO:
   DEBUGGER:SET-BREAK().
   status-ok = BRW_STR:SELECT-FOCUSED-ROW() IN FRAME FRAME-STRANG NO-ERROR.
   IF AVAILABLE kstrang THEN DO:
      DISPLAY  
      kstrang.BENAMNING   WITH BROWSE BRW_STR.
      kstrang.BENAMNING    = INPUT BROWSE BRW_STR kstrang.BENAMNING   .
      DISPLAY   kstrang.BENAMNING WITH BROWSE BRW_STR.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STR C-Win
ON VALUE-CHANGED OF BRW_STR IN FRAME FRAME-STRANG /* Färdiga kontosträngar för område */
DO:
    
   status-ok = BRW_STR:SELECT-FOCUSED-ROW() IN FRAME FRAME-STRANG NO-ERROR.
   IF AVAILABLE kstrang THEN  brec = RECID(kstrang).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kstrang.BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kstrang.BENAMNING BRW_STR _BROWSE-COLUMN C-Win
ON ENTRY OF kstrang.BENAMNING IN BROWSE BRW_STR /* Benämning */
DO:
  IF AVAILABLE kstrang THEN DO:
      DISPLAY kstrang.BENAMNING WITH BROWSE BRW_STR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kstrang.BENAMNING BRW_STR _BROWSE-COLUMN C-Win
ON LEAVE OF kstrang.BENAMNING IN BROWSE BRW_STR /* Benämning */
DO:

   IF AVAILABLE kstrang THEN DO:
      IF kstrang.BENAMNING NE INPUT BROWSE BRW_STR kstrang.BENAMNING THEN DO:           
         kstrang.BENAMNING = INPUT BROWSE BRW_STR kstrang.BENAMNING.           
         DISPLAY kstrang.BENAMNING WITH BROWSE BRW_STR .    
         ktorec = kstrang.RECTIDVIS.         
        
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN KSTUPPD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 2,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).                        
         END.
         ELSE DO:
            RUN KSTUPPD.P 
            (INPUT 2,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).      
         END.   
         RUN openbdynspec_UI IN brwproc[16].
         FIND FIRST kstrang WHERE kstrang.RECTIDVIS = ktorec NO-LOCK NO-ERROR.
             
      END.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME FRAME-KONTO /* Ändra */
DO:
   RUN andra_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB-KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB-KONTO C-Win
ON CHOOSE OF BTN_AVB-KONTO IN FRAME FRAME-KONTO /* Avsluta */
DO:
   RUN avslut_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-NAMN
&Scoped-define SELF-NAME BTN_AVB-NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB-NAMN C-Win
ON CHOOSE OF BTN_AVB-NAMN IN FRAME FRAME-NAMN /* Avsluta */
DO:
   RUN avslut_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME BTN_AVB-STRANG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB-STRANG C-Win
ON CHOOSE OF BTN_AVB-STRANG IN FRAME FRAME-STRANG /* Avsluta */
DO:
   RUN avslut_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME FRAME-KONTO /* Ta bort */
DO:
   RUN bortkonto_UI.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME BTN_BORT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT-2 C-Win
ON CHOOSE OF BTN_BORT-2 IN FRAME FRAME-STRANG /* Ta bort */
DO:
   RUN bortstrang_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTO C-Win
ON CHOOSE OF BTN_KONTO IN FRAME FRAME-STRANG /* Btn 9 */
DO:   
   status-ok = BRW_K-1 :SELECT-FOCUSED-ROW() IN FRAME FRAME-STRANG NO-ERROR.
   SEL_K1 = ktotemp.kontonr.
   status-ok = BRW_K-2:SELECT-FOCUSED-ROW() NO-ERROR.
   SEL_K2 = ktotemp.kontonr.
   status-ok = BRW_K-3:SELECT-FOCUSED-ROW() NO-ERROR.
   SEL_K3 = ktotemp.kontonr.
   status-ok = BRW_K-4:SELECT-FOCUSED-ROW() NO-ERROR.
   SEL_K4 = ktotemp.kontonr.
   status-ok = BRW_K-5:SELECT-FOCUSED-ROW() NO-ERROR.
   SEL_K5 = ktotemp.kontonr.
   IF SEL_K1 = " Blank" THEN ASSIGN SEL_K1 = "" .
   ELSE IF SEL_K1 = ? THEN ASSIGN SEL_K1 = "" .
   IF SEL_K2 = " Blank" THEN ASSIGN SEL_K2 = "" .
   ELSE IF SEL_K2 = ? THEN ASSIGN SEL_K2 = "" .
   IF SEL_K3 = " Blank" THEN ASSIGN SEL_K3 = "" .
   ELSE IF SEL_K3 = ? THEN ASSIGN SEL_K3 = "" .
   IF SEL_K4 = " Blank" THEN ASSIGN SEL_K4 = "" .
   ELSE IF SEL_K4 = ? THEN ASSIGN SEL_K4 = "" .   
   IF SEL_K5 = " Blank" THEN ASSIGN SEL_K5 = "" .
   ELSE IF SEL_K5 = ? THEN ASSIGN SEL_K5 = "" .
   FIND LAST kstrang USE-INDEX KONTO2 NO-LOCK NO-ERROR.
   IF AVAILABLE kstrang THEN radnrvar = kstrang.RADNR + 1.
   ELSE radnrvar = 1.
   FIND FIRST kstrang WHERE kstrang.OMRADE = org AND
   kstrang.K1 = SEL_K1 AND kstrang.K2 = SEL_K2 AND 
   kstrang.K3 = SEL_K3 AND kstrang.K4 = SEL_K4 AND 
   kstrang.K5 = SEL_K5 USE-INDEX KONTO NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kstrang THEN DO :
      {muswait.i}
      RUN KSTRNY.W (INPUT radnrvar,OUTPUT ktorec, INPUT-OUTPUT TABLE kstrang).
      {musarrow.i}
      FIND FIRST kstrang WHERE kstrang.RECTIDVIS = ktorec NO-ERROR.
      IF AVAILABLE kstrang THEN DO:
         RUN setlastrowid_UI IN brwproc[16] (INPUT ROWID(kstrang)).              
         RUN openbdynspec_UI IN brwproc[16].
         RUN lastselectdyn_UI IN brwproc[16].     
      END. 
   END.   
   ELSE DO:
      MESSAGE "Denna kontosträng finns redan upplagd." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME FRAME-KONTO /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-NAMN
&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME FRAME-NAMN /* Spara */
DO:
   {muswait.i}
   ASSIGN
   FILL-IN_ATRHOME = INPUT FILL-IN_ATRHOME
   FILL-IN_K1 = INPUT FILL-IN_K1
   FILL-IN_K2 = INPUT FILL-IN_K2
   FILL-IN_K3 = INPUT FILL-IN_K3
   FILL-IN_K4 = INPUT FILL-IN_K4
   FILL-IN_K5 =  INPUT FILL-IN_K5.
   FILL-IN-MONTBER = INPUT  FILL-IN-MONTBER.
   FILL-IN-PROJEKTLED = INPUT  FILL-IN-PROJEKTLED.
   FILL-IN-PROJEKTOR = INPUT FILL-IN-PROJEKTOR.
   FILL-IN-DOKU = INPUT  FILL-IN-DOKU.
   IF Guru.Konstanter:varforetypval[45] = 1  THEN DO:  
      /*HÄR SELNÄT LENA*/       
      IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) NE 100 THEN DO:
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) > 100 THEN DO:
            MESSAGE "Total fördelning kan inte vara mer än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) < 100 THEN DO:
            MESSAGE "Total fördelning kan inte vara mindre än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.               
         musz = TRUE.
         RETURN NO-APPLY.            
      END.   
   END.

   
   RUN konto_UI (INPUT 1).
   RUN knamnspara_UI IN knamnapph (INPUT FILL-IN_ATRHOME,INPUT FILL-IN_K1,INPUT FILL-IN_K2,
                                 INPUT FILL-IN_K3,INPUT FILL-IN_K4,INPUT FILL-IN_K5,INPUT TABLE gurutextemp).               
   RUN kontorad_UI.
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:    
      /*HÄR SELNÄT LENA*/         
      RUN fordelhuvspar_UI IN knamnapph (INPUT FILL-IN-MONTBER,INPUT FILL-IN-PROJEKTLED, INPUT FILL-IN-PROJEKTOR, INPUT FILL-IN-DOKU ) .
   END.                                
   {musarrow.i}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR C-Win
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-KONTO /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN utkonto_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-KONTO /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME FBTN_SKR-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR-2 C-Win
ON CHOOSE OF FBTN_SKR-2 IN FRAME FRAME-STRANG /* Skriv ut */
DO: 
   
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT TRUE).
   IF musz = TRUE THEN DO:
      ASSIGN
      musz = FALSE
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN utstrang_UI.
      RUN EKLOGL.P.                     
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR-2 C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKR-2 IN FRAME FRAME-STRANG /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER C-Win
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-KONTO /* Visa */
DO:
   skrivut = FALSE.  
   RUN utkonto_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME FBTN_VPER-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER-2 C-Win
ON CHOOSE OF FBTN_VPER-2 IN FRAME FRAME-STRANG /* Visa */
DO:
   skrivut = FALSE.
   DEBUGGER:SET-BREAK().  
   RUN utstrang_UI.  
   RUN UTRAPP2.W.  
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME MBTN_KTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KTO C-Win
ON CHOOSE OF MBTN_KTO IN FRAME FRAME-GRUND /* Konto */
DO:
   IF Guru.Konstanter:regsekvar[6] = TRUE THEN DO:
      RAD_VAL = 2.
      APPLY "VALUE-CHANGED" TO RAD_VAL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KTONAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KTONAMN C-Win
ON CHOOSE OF MBTN_KTONAMN IN FRAME FRAME-GRUND /* Kontonamn */
DO:
   IF Guru.Konstanter:regsekvar[5] = TRUE THEN DO:
      RAD_VAL = 1.
      APPLY "VALUE-CHANGED" TO RAD_VAL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KTOSTRANG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KTOSTRANG C-Win
ON CHOOSE OF MBTN_KTOSTRANG IN FRAME FRAME-GRUND /* Kontosträng */
DO:
   IF Guru.Konstanter:regsekvar[7] = TRUE THEN DO:
      RAD_VAL = 3.
      APPLY "VALUE-CHANGED" TO RAD_VAL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONTO
&Scoped-define SELF-NAME RAD_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO C-Win
ON VALUE-CHANGED OF RAD_KONTO IN FRAME FRAME-KONTO
DO:
   RAD_KONTO = INPUT RAD_KONTO.   
   ASSIGN
   BRW_K1:HIDDEN = TRUE
   BRW_K2:HIDDEN = TRUE   
   BRW_K3:HIDDEN = TRUE      
   BRW_K4:HIDDEN = TRUE      
   BRW_K5:HIDDEN = TRUE.
/*    RAD_KONTO = RAD_KONTO. */
   RUN sethidden_UI IN brwproc[RAD_KONTO] (INPUT FALSE).
   RUN openbdynspec_UI IN brwproc[RAD_KONTO].
/*    RUN open_UI. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-GRUND
DO:
   ASSIGN
   FRAME FRAME-NAMN:HIDDEN = TRUE
   FRAME FRAME-KONTO:HIDDEN = TRUE
   FRAME FRAME-STRANG:HIDDEN = TRUE.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      {&WINDOW-NAME}:TITLE = "Benämningar i Guru"
      FRAME FRAME-NAMN:HIDDEN = FALSE.
      RUN kontonamn_UI.
   END.
   IF RAD_VAL = 2 THEN DO:
      IF RAD_KONTO < 1 OR RAD_KONTO > 5 THEN RAD_KONTO = 1.
      ASSIGN
      {&WINDOW-NAME}:TITLE = "Kontoregistrering"
      FRAME FRAME-KONTO:HIDDEN = FALSE.     
      FOR EACH ktotemp WHERE ktotemp.KONTONR = " Blank":
          DELETE ktotemp. 
      END.
      RUN konto_UI (INPUT 2).
      DISPLAY RAD_KONTO WITH FRAME FRAME-KONTO.
   END.
   IF RAD_VAL = 3 THEN DO:
      ASSIGN
      {&WINDOW-NAME}:TITLE = "Skapa kontosträngar"
      FRAME FRAME-STRANG:HIDDEN = FALSE.          
      RUN kontostrang_UI.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-STRANG
&Scoped-define SELF-NAME SEL_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_OMR C-Win
ON VALUE-CHANGED OF SEL_OMR IN FRAME FRAME-STRANG
DO:
   orgnamn = INPUT SEL_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = orgnamn NO-LOCK NO-ERROR.
   org = omrtemp.OMRADE. 
   RUN open_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define BROWSE-NAME BRW_GTEXT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
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
   {ALLSTARTDYN.I}      
   RUN enable_UI.
   {FRMSIZEF.I}
  
    gurutextemp.EGENTEXT:READ-ONLY IN BROWSE BRW_GTEXT = TRUE.
    gurutextemp.EGENTEXTFULL:READ-ONLY IN BROWSE BRW_GTEXT = TRUE.
   
   RAD_VAL:DELETE("KONTO").
   Guru.GlobalaVariabler:collefth = MBTN_KTONAMN:HANDLE.
   IF Guru.Konstanter:regsekvar[5] = TRUE THEN DO:
      RAD_VAL:ADD-LAST("Kontonamn",1).
      Guru.GlobalaVariabler:colrighth = MBTN_KTO:HANDLE:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:regsekvar[6] = TRUE THEN DO:
      RAD_VAL:ADD-LAST("Konto",2).
      Guru.GlobalaVariabler:colrighth = MBTN_KTOSTRANG:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:regsekvar[7] = TRUE THEN DO:
      RAD_VAL:ADD-LAST("Kontosträng",3).
   END.
   RUN konto_UI (INPUT 0).
   IF Guru.Konstanter:regsekvar[5] = TRUE THEN RAD_VAL = 1.
   ELSE IF Guru.Konstanter:regsekvar[6] = TRUE THEN RAD_VAL = 2.
   ELSE IF Guru.Konstanter:regsekvar[7] = TRUE THEN RAD_VAL = 3.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   
   ASSIGN
   FILL-IN-FORDELM:HIDDEN = TRUE
   FILL-IN-FORDELB:HIDDEN = TRUE
   FILL-IN-MONTBER:HIDDEN = TRUE
   FILL-IN-PROJEKTLED:HIDDEN = TRUE
   FILL-IN-PROJEKTOR:HIDDEN = TRUE
   FILL-IN-DOKU:HIDDEN = TRUE.     
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      /*HÄR SELNÄT LENA*/
      ASSIGN
      FILL-IN-FORDELM:HIDDEN = FALSE
      FILL-IN-FORDELB:HIDDEN = FALSE
      FILL-IN-MONTBER:HIDDEN = FALSE
      FILL-IN-PROJEKTLED:HIDDEN = FALSE
      FILL-IN-PROJEKTOR:HIDDEN = FALSE
      FILL-IN-DOKU:HIDDEN = FALSE. 
      RUN fordelhuvhmt_UI IN knamnapph (INPUT-OUTPUT montbervar,INPUT-OUTPUT projektledvar, INPUT-OUTPUT projektorvar, INPUT-OUTPUT dokuvar ) .
      ASSIGN
      FILL-IN-MONTBER = montbervar
      FILL-IN-PROJEKTLED = projektledvar
      FILL-IN-PROJEKTOR = projektorvar
      FILL-IN-DOKU = dokuvar.
      DISPLAY FILL-IN-FORDELM FILL-IN-FORDELB FILL-IN-MONTBER  FILL-IN-PROJEKTLED FILL-IN-PROJEKTOR FILL-IN-DOKU WITH FRAME FRAME-NAMN.
   END. 
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
   {musarrow.i}
   IF Guru.Konstanter:varforetypval[27] = 1 THEN DO:
      ASSIGN  
      MBTN_KTO:HIDDEN = TRUE 
      MBTN_KTOSTRANG:HIDDEN = TRUE.
   END.   
   BRW_GTEXT:HIDDEN = FALSE.
   BTN_OK:HIDDEN = TRUE.
   FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
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
   RUN DYNBRW.P PERSISTENT SET brwproc[10]
      (INPUT BRW_GTEXT:HANDLE IN FRAME FRAME-NAMN).         
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_K1:HANDLE IN FRAME FRAME-KONTO).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_K2:HANDLE IN FRAME FRAME-KONTO).         
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_K3:HANDLE IN FRAME FRAME-KONTO).         
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_K4:HANDLE IN FRAME FRAME-KONTO).         
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_K5:HANDLE IN FRAME FRAME-KONTO).         

   RUN DYNBRW.P PERSISTENT SET brwproc[11]
      (INPUT BRW_K-1:HANDLE IN FRAME FRAME-STRANG).         
   RUN DYNBRW.P PERSISTENT SET brwproc[12]
      (INPUT BRW_K-2:HANDLE IN FRAME FRAME-STRANG).         
   RUN DYNBRW.P PERSISTENT SET brwproc[13]
      (INPUT BRW_K-3:HANDLE IN FRAME FRAME-STRANG).         
   RUN DYNBRW.P PERSISTENT SET brwproc[14]
      (INPUT BRW_K-4:HANDLE IN FRAME FRAME-STRANG).         
   RUN DYNBRW.P PERSISTENT SET brwproc[15]
      (INPUT BRW_K-5:HANDLE IN FRAME FRAME-STRANG).              
   RUN DYNBRW.P PERSISTENT SET brwproc[16]
      (INPUT BRW_STR:HANDLE IN FRAME FRAME-STRANG).         
   
   /*namn*/
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KNAMNAPP.P PERSISTENT SET knamnapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KNAMNAPP.P PERSISTENT SET knamnapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN PERSONALAPP.P PERSISTENT SET persproch.      
                     
   END.
   
    
   /*konton*/
   IF Guru.Konstanter:appcon THEN DO:                           
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN KTOAPP.P PERSISTENT SET ktoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN KTOAPP.P PERSISTENT SET ktoproch (INPUT Guru.Konstanter:globanv).   
   END.
   brwsortvar = 'ktotemp.KONTO = "K1"'.     
   RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K2"'.      
   RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K3"'.      
   RUN setcolsortvar_UI IN brwproc[3] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K4"'.      
   RUN setcolsortvar_UI IN brwproc[4] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K5"'.      
   RUN setcolsortvar_UI IN brwproc[5] (INPUT brwsortvar).
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "VIJUDID BY KONTONR").
   RUN setcolindex_UI IN brwproc[2] (INPUT "VIJUDID BY KONTONR").
   RUN setcolindex_UI IN brwproc[3] (INPUT "VIJUDID BY KONTONR").
   RUN setcolindex_UI IN brwproc[4] (INPUT "VIJUDID BY KONTONR").
   RUN setcolindex_UI IN brwproc[5] (INPUT "VIJUDID BY KONTONR").
   
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      ASSIGN                                     
      ktotemp.VIJUDID:READ-ONLY IN BROWSE BRW_K1 = TRUE    
      ktotemp.VIJUDID:READ-ONLY IN BROWSE BRW_K2 = TRUE
      ktotemp.VIJUDID:READ-ONLY IN BROWSE BRW_K3 = TRUE 
      ktotemp.VIJUDID:READ-ONLY IN BROWSE BRW_K4 = TRUE
      ktotemp.VIJUDID:READ-ONLY IN BROWSE BRW_K5 = TRUE.   
   END.
   /*strängar*/
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI C-Win 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   RUN selfocusrow_UI IN brwproc[RAD_KONTO].
   IF NOT AVAILABLE ktotemp THEN DO:
      RETURN.
   END.
   ASSIGN
   tempkonto = ktotemp.KONTO
   tempkontonr = ktotemp.KONTONR.
   tempjur = ktotemp.VIJUDID.
   
   RUN KTONY.W (INPUT FALSE, INPUT tempkonto, INPUT-OUTPUT tempkontonr,INPUT-OUTPUT tempjur).
   

   FIND FIRST ktotemp WHERE ktotemp.KONTO = tempkonto AND
       ktotemp.KONTONR = tempkontonr AND ktotemp.VIJUDID = tempjur NO-ERROR.
   IF AVAILABLE ktotemp THEN DO:
      RUN setlastrowid_UI IN brwproc[RAD_KONTO] (INPUT ROWID(ktotemp)).              
      RUN openbdynspec_UI IN brwproc[RAD_KONTO].
      RUN lastselectdyn_UI IN brwproc[RAD_KONTO].     
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avslut_UI C-Win 
PROCEDURE avslut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF Guru.Konstanter:regsekvar[5] = TRUE THEN DO:
      ASSIGN
      FILL-IN_ATRHOME = INPUT FRAME FRAME-NAMN FILL-IN_ATRHOME 
      FILL-IN_K1 = INPUT FRAME FRAME-NAMN FILL-IN_K1
      FILL-IN_K2 = INPUT FRAME FRAME-NAMN FILL-IN_K2
      FILL-IN_K3 = INPUT FRAME FRAME-NAMN FILL-IN_K3
      FILL-IN_K4 = INPUT FRAME FRAME-NAMN FILL-IN_K4
      FILL-IN_K5 =  INPUT FRAME FRAME-NAMN FILL-IN_K5.
      RUN knamnspara_UI IN knamnapph (INPUT FILL-IN_ATRHOME,INPUT FILL-IN_K1,INPUT FILL-IN_K2,
                                      INPUT FILL-IN_K3,INPUT FILL-IN_K4,INPUT FILL-IN_K5,INPUT TABLE gurutextemp).               
   END.
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:    
      /*HÄR SELNÄT LENA*/   
      FILL-IN-MONTBER = INPUT  FILL-IN-MONTBER.
      FILL-IN-PROJEKTLED = INPUT  FILL-IN-PROJEKTLED.
      FILL-IN-PROJEKTOR = INPUT FILL-IN-PROJEKTOR.
      FILL-IN-DOKU = INPUT  FILL-IN-DOKU.
       
      /*HÄR SELNÄT LENA*/
      IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED ) NE 100 THEN DO:
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED) > 100 THEN DO:
            MESSAGE "Total fördelning av kalkylens montörstid kan inte vara mer än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED ) < 100 THEN DO:
            MESSAGE "Total fördelning av kalkylens montörstid kan inte vara mindre än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.               
         musz = TRUE.
         RETURN NO-APPLY.            
      END.
      IF (  FILL-IN-PROJEKTOR + FILL-IN-DOKU ) NE 100 THEN DO:
         IF (  FILL-IN-PROJEKTOR + FILL-IN-DOKU ) > 100 THEN DO:
            MESSAGE "Total fördelning av kalkylens beredartid kan inte vara mer än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.
         IF ( FILL-IN-PROJEKTOR + FILL-IN-DOKU ) < 100 THEN DO:
            MESSAGE "Total fördelning av kalkylens beredartid kan inte vara mindre än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.               
         musz = TRUE.
         RETURN NO-APPLY.            
      END.       
      /*IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) NE 100 THEN DO:
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) > 100 THEN DO:
            MESSAGE "Total fördelning kan inte vara mer än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.
         IF ( FILL-IN-MONTBER + FILL-IN-PROJEKTLED + FILL-IN-PROJEKTOR + FILL-IN-DOKU ) < 100 THEN DO:
            MESSAGE "Total fördelning kan inte vara mindre än 100%. Ändra fördelningen under Kontonamn" VIEW-AS ALERT-BOX.
         END.               
         musz = TRUE.
         RETURN NO-APPLY.            
      END.*/   
      RUN fordelhuvspar_UI IN knamnapph (INPUT FILL-IN-MONTBER,INPUT FILL-IN-PROJEKTLED, INPUT FILL-IN-PROJEKTOR, INPUT FILL-IN-DOKU ) .
   END. 
   

   {BORTBRWPROC.I}
   IF VALID-HANDLE(knamnapph) THEN DELETE PROCEDURE knamnapph.
   IF VALID-HANDLE(ktoproch) THEN DELETE PROCEDURE ktoproch.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bortkonto_UI C-Win 
PROCEDURE bortkonto_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN selfocusrow_UI IN brwproc[RAD_KONTO].
   IF NOT AVAILABLE ktotemp THEN DO:
      RETURN.
   END.
   MESSAGE "Vill du verkligen ta bort detta konto?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE ktotemp.VIJUDID + " " + ktotemp.KONTONR 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      ASSIGN
      tempkonto = ktotemp.KONTO
      tempkontonr = ktotemp.KONTONR
      tempjur = ktotemp.VIJUDID. 

      /*RUN tabortkonto_UI IN ktoproch
         (INPUT Guru.Konstanter:globforetag, INPUT ktotemp.KONTO, INPUT ktotemp.KONTONR, 
          INPUT TABLE ktotemp, OUTPUT felmedd).*/
      
      RUN tabortkontou_UI IN ktoproch
         (INPUT Guru.Konstanter:globforetag, INPUT ktotemp.KONTO, INPUT ktotemp.KONTONR,INPUT ktotemp.VIJUDID,  
          INPUT TABLE ktotemp, OUTPUT felmedd).
      
      IF felmedd = "" THEN DO:
         FIND FIRST ktotemp WHERE ktotemp.KONTO = tempkonto AND
            ktotemp.KONTONR = tempkontonr AND ktotemp.VIJUDID = tempjur NO-LOCK NO-ERROR.
         IF AVAILABLE ktotemp THEN DO:
            DELETE ktotemp.
            RUN selnextprevrow_UI IN brwproc[RAD_KONTO].
            RUN openbdynspec_UI IN brwproc[RAD_KONTO].
            RUN lastselectdyn_UI IN brwproc[RAD_KONTO].                 
         END.
      END.
      ELSE DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Felmeddelande!".
      END.
   END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bortstrang_UI C-Win 
PROCEDURE bortstrang_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_STR:SELECT-FOCUSED-ROW() IN FRAME FRAME-STRANG NO-ERROR.
   IF NOT AVAILABLE kstrang THEN RETURN.
   brec = RECID(kstrang).  
   MESSAGE "Vill du verkligen ta bort denna kontosträng?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE kstrang.BENAMNING 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:
      FIND FIRST kstrang WHERE RECID(kstrang) = brec NO-LOCK NO-ERROR.
      ktorec = kstrang.RECTIDVIS.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KSTUPPD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 3,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).                        
      END.
      ELSE DO:
         RUN KSTUPPD.P 
         (INPUT 3,INPUT-OUTPUT ktorec,INPUT-OUTPUT TABLE kstrang).      
      END.     
      RUN openbdynspec_UI IN brwproc[16].
      FIND FIRST kstrang WHERE kstrang.RECTIDVIS = ktorec  EXCLUSIVE-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[16] (INPUT ROWID(kstrang)).              
      RUN lastselectdyn_UI IN brwproc[16].     
      DELETE kstrang.
      RUN selnextprevrow_UI IN brwproc[16].
      RUN refreshbrw_UI IN brwproc[16].
      RUN lastselectdyn_UI IN brwproc[16].        
   END.

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
  ENABLE MBTN_KTONAMN MBTN_KTO MBTN_KTOSTRANG RAD_VAL RECT-25 
      WITH FRAME FRAME-GRUND IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-GRUND}
  DISPLAY RAD_KONTO 
      WITH FRAME FRAME-KONTO IN WINDOW C-Win.
  ENABLE RAD_KONTO BRW_K2 BRW_K3 BRW_K4 BRW_K5 BRW_K1 FBTN_VPER FBTN_SKR BTN_NY 
         BTN_AND BTN_BORT BTN_AVB-KONTO 
      WITH FRAME FRAME-KONTO IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONTO}
  DISPLAY FILL-IN_ATRHOME FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 FILL-IN_K4 FILL-IN_K5 
      WITH FRAME FRAME-NAMN IN WINDOW C-Win.
  ENABLE FILL-IN-MONTBER FILL-IN-PROJEKTLED FILL-IN_ATRHOME FILL-IN-PROJEKTOR 
         FILL-IN-DOKU FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 FILL-IN_K4 FILL-IN_K5 
         BRW_GTEXT BTN_OK BTN_AVB-NAMN FILL-IN-FORDELM FILL-IN-FORDELB 
      WITH FRAME FRAME-NAMN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-NAMN}
  DISPLAY FILL-IN-VALJ SEL_OMR 
      WITH FRAME FRAME-STRANG IN WINDOW C-Win.
  ENABLE BRW_STR SEL_OMR BTN_KONTO BTN_BORT-2 FBTN_VPER-2 FBTN_SKR-2 BRW_K-1 
         BRW_K-2 BRW_K-3 BRW_K-4 BRW_K-5 BTN_AVB-STRANG 
      WITH FRAME FRAME-STRANG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-STRANG}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE format_UI C-Win 
PROCEDURE format_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF gurutextemp.PROGRAM = "AVD" THEN do:
      gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".
     /* gurutextemp.EGENTEXT:BGCOLOR IN BROWSE BRW_GTEXT = 12.*/
   END.
   IF gurutextemp.PROGRAM = "OMR" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(6)".
   IF gurutextemp.PROGRAM = "AONR" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(6)".
   IF gurutextemp.PROGRAM = "PLAN" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(6)".
   IF gurutextemp.PROGRAM = "ENR" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(8)".
   IF gurutextemp.PROGRAM = "JUR" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".
   IF gurutextemp.PROGRAM = "FAST" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(5)".
   IF gurutextemp.PROGRAM = "TILLF" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(11)".   
   IF gurutextemp.PROGRAM = "BERE" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".
   IF gurutextemp.PROGRAM = "PROJ" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".
   IF gurutextemp.PROGRAM = "ARBA" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(14)".
   IF gurutextemp.PROGRAM = "UTFO" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".   
   IF gurutextemp.PROGRAM = "BEST" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(15)".         
   IF gurutextemp.PROGRAM = "DEB" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(10)".         
   IF gurutextemp.PROGRAM = "TIDL" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(8)".           
   IF gurutextemp.PROGRAM = "UTFA" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(15)".   
   IF gurutextemp.PROGRAM = "REFBEF" THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(17)".   
   IF gurutextemp.PROGRAM = "Prio"  THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(9)".   
   IF gurutextemp.PROGRAM = "Arbart"  THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(10)".   
   IF gurutextemp.PROGRAM = "AONAMN"  THEN gurutextemp.EGENTEXT:FORMAT IN BROWSE BRW_GTEXT = "X(13)".   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontonamn_UI C-Win 
PROCEDURE kontonamn_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   /*namn*/
   FIND FIRST gurutextemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gurutextemp THEN DO:
      RUN knamnhmt_UI IN knamnapph (OUTPUT FILL-IN_ATRHOME,OUTPUT FILL-IN_K1,OUTPUT FILL-IN_K2,
                                 OUTPUT FILL-IN_K3,OUTPUT FILL-IN_K4,OUTPUT FILL-IN_K5,OUTPUT TABLE gurutextemp).               
      RUN openbdyn_UI IN brwproc[10] (INPUT "").
      

   END.
              
   DISPLAY FILL-IN_ATRHOME FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 FILL-IN_K4 FILL-IN_K5 WITH FRAME FRAME-NAMN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontorad_UI C-Win 
PROCEDURE kontorad_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   FIND FIRST kbtemp NO-LOCK NO-ERROR.
   IF VALID-HANDLE(ktoproch) THEN RUN kbhmt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp).                  
   FIND FIRST kbtemp NO-ERROR.  
   IF AVAILABLE kbtemp THEN DO:
      RAD_KONTO:ADD-LAST(CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2)),1) IN FRAME FRAME-KONTO.
      RAD_KONTO:ADD-LAST(CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2)),2).
      RAD_KONTO:ADD-LAST(CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2)),3).
      RAD_KONTO:ADD-LAST(CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2)),4).
      RAD_KONTO:ADD-LAST(CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)),5). 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontostrang_UI C-Win 
PROCEDURE kontostrang_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   /*strängar*/
   vadgora = 1.
   FIND FIRST jurtemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurtemp THEN DO:
      IF VALID-HANDLE(ktoproch) THEN RUN jurhmt_UI IN ktoproch
      (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE jurtemp).  
   END. 
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).    
   IF VALID-HANDLE(ktoproch) THEN RUN kbhmt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp).
   FIND FIRST kbtemp NO-ERROR.
   FIND FIRST omrtemp NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      status-ok = SEL_OMR:DELETE("") IN FRAME FRAME-STRANG.
      {OMRHMT.I}
      FOR EACH judavdtemp,         
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
         status-ok = SEL_OMR:ADD-LAST(omrtemp.NAMN).
      END.
   END.
   FIND FIRST omrtemp NO-LOCK NO-ERROR.
   ASSIGN
   org = omrtemp.OMRADE
   orgnamn = omrtemp.NAMN
   SEL_OMR = orgnamn
   SEL_OMR:SELECTED = TRUE
   vadgora = 1.
   FIND FIRST kstrang WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kstrang THEN DO:  
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KSTHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT 0,INPUT-OUTPUT TABLE kstrang,OUTPUT jid).                        
      END.
      ELSE DO:
         RUN KSTHMT.P 
         (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT 0,INPUT-OUTPUT TABLE kstrang,OUTPUT jid ).      
      END.   
   END.
   FIND FIRST ktotemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ktotemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN KHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).                        
      END.
      ELSE DO:
         RUN KHMT.P 
         (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).      
      END.        
   END.
   FIND FIRST ktotemp WHERE ktotemp.KONTO = "K1" AND ktotemp.KONTONR = " Blank" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ktotemp THEN DO:
      IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
         FOR EACH jurtemp:   
            CREATE ktotemp.   
            ASSIGN
            ktotemp.KONTO = "K1"
            ktotemp.KONTONR = " Blank"   
            ktotemp.JUDID = jurtemp.JUDID.   
            CREATE ktotemp.   
            ASSIGN
            ktotemp.KONTO = "K2"
            ktotemp.KONTONR = " Blank"   
            ktotemp.JUDID = jurtemp.JUDID.   
            CREATE ktotemp.   
            ASSIGN
            ktotemp.KONTO = "K3"
            ktotemp.KONTONR = " Blank"   
            ktotemp.JUDID = jurtemp.JUDID.   
            CREATE ktotemp.   
            ASSIGN
            ktotemp.KONTO = "K4"
            ktotemp.KONTONR = " Blank"   
            ktotemp.JUDID = jurtemp.JUDID.   
            CREATE ktotemp.   
            ASSIGN
            ktotemp.KONTO = "K5"
            ktotemp.KONTONR = " Blank"   
            ktotemp.JUDID = jurtemp.JUDID.   
         END.
      END.
      ELSE DO:
         CREATE ktotemp.   
         ASSIGN
         ktotemp.KONTO = "K1"
         ktotemp.KONTONR = " Blank"   
         ktotemp.JUDID = "".   
         CREATE ktotemp.   
         ASSIGN
         ktotemp.KONTO = "K2"
         ktotemp.KONTONR = " Blank"   
         ktotemp.JUDID = "".   
         CREATE ktotemp.   
         ASSIGN
         ktotemp.KONTO = "K3"
         ktotemp.KONTONR = " Blank"   
         ktotemp.JUDID = "".   
         CREATE ktotemp.   
         ASSIGN
         ktotemp.KONTO = "K4"
         ktotemp.KONTONR = " Blank"   
         ktotemp.JUDID = "".   
         CREATE ktotemp.   
         ASSIGN
         ktotemp.KONTO = "K5"
         ktotemp.KONTONR = " Blank"   
         ktotemp.JUDID = "".   
      END.
   END.
   BRW_STR:TITLE = "Färdiga kontosträngar för " + Guru.Konstanter:gomrl. 
   FILL-IN-VALJ = "Välj " + LC(Guru.Konstanter:gomrl).
   ASSIGN 
   SEL_K1:SCREEN-VALUE = " Blank"
   SEL_K2:SCREEN-VALUE = " Blank"
   SEL_K3:SCREEN-VALUE = " Blank"
   SEL_K4:SCREEN-VALUE = " Blank"
   SEL_K5:SCREEN-VALUE = " Blank".
   ASSIGN
   kstrang.K1:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   kstrang.K2:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   kstrang.K3:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   kstrang.K4:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   kstrang.K5:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2))
   kstrang.BENAMNING:LABEL IN BROWSE BRW_STR = "Benämning"
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-1 = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-2 = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-3 = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-4 = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   ktotemp.KONTONR:LABEL IN BROWSE BRW_K-5 = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)).
   RUN open_UI.
   DISPLAY SEL_OMR FILL-IN-VALJ WITH FRAME FRAME-STRANG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konto_UI C-Win 
PROCEDURE konto_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   /*konton*/  
   FIND FIRST kbtemp NO-LOCK NO-ERROR.
   IF vad = 1 THEN DO:
      RAD_KONTO:DELETE(CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))) IN FRAME FRAME-KONTO.
      RAD_KONTO:DELETE(CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))).
      RAD_KONTO:DELETE(CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))).
      RAD_KONTO:DELETE(CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))).
      RAD_KONTO:DELETE(CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2))).             
      RETURN.
   END.
   IF VALID-HANDLE(ktoproch) THEN RUN kbhmt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp).                  
   FIND FIRST kbtemp NO-ERROR.  
   IF vad = 0 THEN DO:
      musz = RAD_KONTO:DELETE("") IN FRAME FRAME-KONTO.
      RUN kontorad_UI.      
      RAD_KONTO = 1.
   END.
   FIND FIRST jurtemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurtemp THEN DO:
      IF VALID-HANDLE(ktoproch) THEN RUN jurhmt_UI IN ktoproch
      (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE jurtemp).  
   END.     
   FIND FIRST ktotemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ktotemp THEN DO:
      IF VALID-HANDLE(ktoproch) THEN RUN ktohamt_UI IN ktoproch
      (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).  
   END.
   IF AVAILABLE kbtemp THEN DO:
      ASSIGN
      ktotemp.KONTONR:LABEL IN BROWSE BRW_K1 = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
      ktotemp.KONTONR:LABEL IN BROWSE BRW_K2 = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
      ktotemp.KONTONR:LABEL IN BROWSE BRW_K3 = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
      ktotemp.KONTONR:LABEL IN BROWSE BRW_K4 = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
      ktotemp.KONTONR:LABEL IN BROWSE BRW_K5 = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)).
   END.   
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K1 = TRUE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K2 = TRUE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K3 = TRUE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K4 = TRUE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K5 = TRUE.
   END.
   ELSE DO:
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K1 = FALSE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K2 = FALSE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K3 = FALSE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K4 = FALSE.
      ktotemp.VIJUDID:VISIBLE IN BROWSE BRW_K5 = FALSE.
   END.
   IF vad > 1 THEN APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME FRAME-KONTO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI C-Win 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN selfocusrow_UI IN brwproc[RAD_KONTO].
   
   ASSIGN
   tempkonto = "K" + STRING(RAD_KONTO)
   tempkontonr = ""
   tempjur = "".
   RUN KTONY.W (INPUT TRUE, INPUT tempkonto, INPUT-OUTPUT tempkontonr,INPUT-OUTPUT tempjur).
   
   IF musz = FALSE THEN DO:
      FIND FIRST ktotemp WHERE ktotemp.KONTO = tempkonto AND 
         ktotemp.KONTONR = tempkontonr AND ktotemp.VIJUDID = tempjur NO-ERROR.
      IF AVAILABLE ktotemp THEN DO:
         RUN setlastrowid_UI IN brwproc[RAD_KONTO] (INPUT ROWID(ktotemp)).              
         RUN openbdynspec_UI IN brwproc[RAD_KONTO].
         RUN lastselectdyn_UI IN brwproc[RAD_KONTO].     
      END.           
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI C-Win 
PROCEDURE open_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   vadgora = 2.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KSTHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT omrtemp.AVDELNINGNR ,INPUT-OUTPUT TABLE kstrang,OUTPUT jid).                        
   END.
   ELSE DO:
      RUN KSTHMT.P 
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT omrtemp.AVDELNINGNR, INPUT-OUTPUT TABLE kstrang,OUTPUT jid).      
   END.                                            
   brwsortvar = 'ktotemp.JUDID = "' + jid + '" AND ktotemp.KONTO = "K1" '.
   RUN setcolsortvar_UI IN brwproc[11] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.JUDID = "' + jid + '" AND ktotemp.KONTO = "K2" '.
   RUN setcolsortvar_UI IN brwproc[12] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.JUDID = "' + jid + '" AND ktotemp.KONTO = "K3" '.
   RUN setcolsortvar_UI IN brwproc[13] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.JUDID = "' + jid + '" AND ktotemp.KONTO = "K4" '.
   RUN setcolsortvar_UI IN brwproc[14] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.JUDID = "' + jid + '" AND ktotemp.KONTO = "K5" '.
   RUN setcolsortvar_UI IN brwproc[15] (INPUT brwsortvar).
   brwsortvar = 'kstrang.OMRADE = "' + org + '"'.
   RUN setcolsortvar_UI IN brwproc[16] (INPUT brwsortvar).
   RUN openbdynspec_UI IN brwproc[11].                 
   RUN openbdynspec_UI IN brwproc[12].                 
   RUN openbdynspec_UI IN brwproc[13].                 
   RUN openbdynspec_UI IN brwproc[14].                 
   RUN openbdynspec_UI IN brwproc[15].                 
   RUN openbdynspec_UI IN brwproc[16].                 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utkonto_UI C-Win 
PROCEDURE utkonto_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                    
   str = "======.==================================".
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   CREATE tidut.
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.            
   SUBSTRING(tidut.UT,1) = "Konton i GURU med kontorubrik ".  
   IF RAD_KONTO = 1 THEN SUBSTRING(tidut.UT,31) = kbtemp.K1.
   IF RAD_KONTO = 2 THEN SUBSTRING(tidut.UT,31) = kbtemp.K2.
   IF RAD_KONTO = 3 THEN SUBSTRING(tidut.UT,31) = kbtemp.K3.
   IF RAD_KONTO = 4 THEN SUBSTRING(tidut.UT,31) = kbtemp.K4.
   IF RAD_KONTO = 5 THEN SUBSTRING(tidut.UT,31) = kbtemp.K5.
   CREATE tidut.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "KONTO".
   SUBSTRING(tidut.UT,8) = "BENÄMNING".                                
    
   CREATE tidut.
   ASSIGN tidut.UT = str.
   IF RAD_KONTO = 1 THEN DO:
      GET FIRST BRW_K1 NO-LOCK.
      DO WHILE AVAILABLE(ktotemp):
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = ktotemp.KONTONR.  
         SUBSTRING(tidut.UT,8) = ktotemp.BENAMNING.      
         GET NEXT BRW_K1 NO-LOCK.
      END.              
   END.
   IF RAD_KONTO = 2 THEN DO:
    GET FIRST BRW_K2 NO-LOCK.
      DO WHILE AVAILABLE(ktotemp):
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = ktotemp.KONTONR.  
         SUBSTRING(tidut.UT,8) = ktotemp.BENAMNING.      
         GET NEXT BRW_K2 NO-LOCK.
      END.                    
   END.
   IF RAD_KONTO = 3 THEN DO:
      GET FIRST BRW_K3 NO-LOCK.
      DO WHILE AVAILABLE(ktotemp):
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = ktotemp.KONTONR.  
         SUBSTRING(tidut.UT,8) = ktotemp.BENAMNING.      
         GET NEXT BRW_K3 NO-LOCK.
      END.                 
   END.
   IF RAD_KONTO = 4 THEN DO:
      GET FIRST BRW_K4 NO-LOCK.
      DO WHILE AVAILABLE(ktotemp):
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = ktotemp.KONTONR.  
         SUBSTRING(tidut.UT,8) = ktotemp.BENAMNING.      
         GET NEXT BRW_K4 NO-LOCK.
      END.              
   END.
   IF RAD_KONTO = 5 THEN DO:
      GET FIRST BRW_K5 NO-LOCK.
      DO WHILE AVAILABLE(ktotemp):
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = ktotemp.KONTONR.  
         SUBSTRING(tidut.UT,8) = ktotemp.BENAMNING.      
         GET NEXT BRW_K5 NO-LOCK.
      END.              
   END.
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Konton i GURU").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utstrang_UI C-Win 
PROCEDURE utstrang_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}                                               
   str = "===========================================================================.======.======.======.======.======.".
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   CREATE tidut.
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.   
   SUBSTRING(tidut.UT,1) = "Kontosträngar".   
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "BENÄMNING"
   SUBSTRING(tidut.UT,77) = kbtemp.K1
   SUBSTRING(tidut.UT,84) = kbtemp.K2
   SUBSTRING(tidut.UT,91) = kbtemp.K3
   SUBSTRING(tidut.UT,98) = kbtemp.K4  
   SUBSTRING(tidut.UT,105) = kbtemp.K5.                                
   /*SUBSTRING(tidut.UT,27) = kbtemp.K1
   SUBSTRING(tidut.UT,34) = kbtemp.K2
   SUBSTRING(tidut.UT,41) = kbtemp.K3
   SUBSTRING(tidut.UT,48) = kbtemp.K4  
   SUBSTRING(tidut.UT,55) = kbtemp.K5.*/
   
   CREATE tidut.
   ASSIGN tidut.UT = str.
   omrvar = "".
   OPEN QUERY okq FOR EACH omrtemp USE-INDEX OMR NO-LOCK,
   EACH kstrang WHERE kstrang.OMRADE = omrtemp.OMRADE 
   USE-INDEX KONTO NO-LOCK.
   GET FIRST okq NO-LOCK.
   DO WHILE AVAILABLE(kstrang):
      IF omrvar NE omrtemp.OMRADE THEN DO:
         CREATE tidut.
         ASSIGN                     
         SUBSTRING(tidut.UT,1) = "KONTOSTRÄNGAR FÖR " + CAPS(Guru.Konstanter:gomrk)
         SUBSTRING(tidut.UT,26) = omrtemp.OMRADE
         SUBSTRING(tidut.UT,33) = omrtemp.NAMN. 
         omrvar = omrtemp.OMRADE.
      END.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = kstrang.BENAMNING
      SUBSTRING(tidut.UT,77) = kstrang.K1
      SUBSTRING(tidut.UT,84) = kstrang.K2
      SUBSTRING(tidut.UT,91) = kstrang.K3
      SUBSTRING(tidut.UT,98) = kstrang.K4  
      SUBSTRING(tidut.UT,105) = kstrang.K5.
      /*SUBSTRING(tidut.UT,27) = kstrang.K1
      SUBSTRING(tidut.UT,34) = kstrang.K2
      SUBSTRING(tidut.UT,41) = kstrang.K3
      SUBSTRING(tidut.UT,48) = kstrang.K4  
      SUBSTRING(tidut.UT,55) = kstrang.K5.*/
      GET NEXT okq NO-LOCK.
   END.  
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

