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

  Created: 10/15/96 -  1:50 pm

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
{ALLDEF.I}
{GLOBVAR2DEL1.I}

DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildirq AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.*/
/*DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE rowvar AS ROWID NO-UNDO.*/
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE katnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE bortfraga AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamnuppxml AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynxml AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlxapph AS HANDLE NO-UNDO.
DEFINE VARIABLE filnamnz AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjkat AS INTEGER NO-UNDO.

DEFINE VARIABLE inlant AS INTEGER NO-UNDO.

DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO.
DEFINE VARIABLE hjfrag AS CHARACTER NO-UNDO.

DEFINE VARIABLE E1 AS INTEGER NO-UNDO.
DEFINE VARIABLE E2 AS INTEGER NO-UNDO.
DEFINE VARIABLE B1 AS INTEGER NO-UNDO.
DEFINE VARIABLE B2 AS INTEGER NO-UNDO.
DEFINE VARIABLE ENH1 AS INTEGER NO-UNDO.
DEFINE VARIABLE ENH2 AS INTEGER NO-UNDO.
DEFINE VARIABLE NP1 AS INTEGER NO-UNDO.
DEFINE VARIABLE NP2 AS INTEGER NO-UNDO.
DEFINE VARIABLE BP1 AS INTEGER NO-UNDO.
DEFINE VARIABLE BP2 AS INTEGER NO-UNDO.
DEFINE VARIABLE decivar AS INTEGER NO-UNDO.
DEFINE VARIABLE pos2 AS INTEGER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.


DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.


{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{MTRLTAB.I}
{SOKDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
DEFINE TEMP-TABLE mtrltempfavo NO-UNDO LIKE mtrltemp.
DEFINE TEMP-TABLE mkattmp   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL   
   FIELD NPRIS              AS DECIMAL
   FIELD NETTO              AS LOGICAL.   
DEFINE TEMP-TABLE felex NO-UNDO LIKE mkattmp.
DEFINE TEMP-TABLE felmex   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
DEFINE TEMP-TABLE tempnamn NO-UNDO
      FIELD NAMNH AS HANDLE
      FIELD TORGTH AS HANDLE
      FIELD NODTAB AS CHARACTER.

DEFINE TEMP-TABLE Frekventa NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER   
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.

DEFINE TEMP-TABLE Ahlsell_x0020_Frekventa NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.

DEFINE TEMP-TABLE Onninen_x0020_Frekventa NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.
   
   
DEFINE TEMP-TABLE Ovrigt NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.

DEFINE TEMP-TABLE Ahlsell_x0020_Hela NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.
DEFINE TEMP-TABLE Onninen_x0020_Hela NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.

DEFINE TEMP-TABLE Nexans NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER   
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.

   
DEFINE TEMP-TABLE NKT NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER      
   FIELD Enhet     AS CHARACTER
   FIELD Pris AS DECIMAL.
   
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

{TIDUTTT.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
IF Guru.Konstanter:appcon THEN DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
END.
ELSE DO:
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
END.                  
EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
CREATE inextradatatemp.          
ASSIGN
inextradatatemp.PROGRAM = "FAVO"                   
inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
inextradatatemp.HUVUDINT =  ?.   
RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
FIND FIRST extradatatemp NO-LOCK NO-ERROR.
IF AVAILABLE extradatatemp THEN DO:
   Guru.Konstanter:alltidmax = extradatatemp.SOKLOG[10].         
   IF extradatatemp.SOKCHAR[1] = "" THEN extradatatemp.SOKCHAR[1] = "AMERICAN".
   SESSION:NUMERIC-FORMAT = extradatatemp.SOKCHAR[1].
   Guru.Konstanter:globnetprissortvar = extradatatemp.SOKINT[4].
END.
EMPTY TEMP-TABLE inextradatatemp NO-ERROR.

{AMERICANEUROPEAN.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define BROWSE-NAME BRW_AFRE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahlsell_x0020_Frekventa Ahlsell_x0020_Hela ~
Frekventa mtrltemp Nexans NKT Onninen_x0020_Frekventa Onninen_x0020_Hela ~
ovrigt

/* Definitions for BROWSE BRW_AFRE                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AFRE Ahlsell_x0020_Frekventa.Enummer ~
Ahlsell_x0020_Frekventa.Benamning Ahlsell_x0020_Frekventa.Enhet ~
Ahlsell_x0020_Frekventa.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AFRE 
&Scoped-define QUERY-STRING-BRW_AFRE FOR EACH Ahlsell_x0020_Frekventa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AFRE OPEN QUERY BRW_AFRE FOR EACH Ahlsell_x0020_Frekventa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AFRE Ahlsell_x0020_Frekventa
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AFRE Ahlsell_x0020_Frekventa


/* Definitions for BROWSE BRW_AHHELA                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_AHHELA Ahlsell_x0020_Hela.Enummer ~
Ahlsell_x0020_Hela.Benamning Ahlsell_x0020_Hela.Enhet ~
Ahlsell_x0020_Hela.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AHHELA 
&Scoped-define QUERY-STRING-BRW_AHHELA FOR EACH Ahlsell_x0020_Hela NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AHHELA OPEN QUERY BRW_AHHELA FOR EACH Ahlsell_x0020_Hela NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AHHELA Ahlsell_x0020_Hela
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AHHELA Ahlsell_x0020_Hela


/* Definitions for BROWSE BRW_FRE                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FRE Frekventa.Enummer ~
Frekventa.Benamning Frekventa.Enhet Frekventa.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRE 
&Scoped-define QUERY-STRING-BRW_FRE FOR EACH Frekventa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FRE OPEN QUERY BRW_FRE FOR EACH Frekventa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FRE Frekventa
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRE Frekventa


/* Definitions for BROWSE BRW_LEVMTRL                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_LEVMTRL mtrltemp.LEVKOD mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS ~
mtrltemp.KUND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEVMTRL mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS ~
mtrltemp.KUND 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LEVMTRL mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LEVMTRL mtrltemp
&Scoped-define QUERY-STRING-BRW_LEVMTRL FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_LEVMTRL OPEN QUERY BRW_LEVMTRL FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_LEVMTRL mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEVMTRL mtrltemp


/* Definitions for BROWSE BRW_NEX                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_NEX Nexans.Enummer Nexans.Benamning ~
Nexans.Enhet Nexans.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NEX 
&Scoped-define QUERY-STRING-BRW_NEX FOR EACH Nexans NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_NEX OPEN QUERY BRW_NEX FOR EACH Nexans NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_NEX Nexans
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NEX Nexans


/* Definitions for BROWSE BRW_NKT                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_NKT NKT.Enummer NKT.Benamning NKT.Enhet ~
NKT.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NKT 
&Scoped-define QUERY-STRING-BRW_NKT FOR EACH NKT NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_NKT OPEN QUERY BRW_NKT FOR EACH NKT NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_NKT NKT
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NKT NKT


/* Definitions for BROWSE BRW_OFRE                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OFRE Onninen_x0020_Frekventa.Enummer ~
Onninen_x0020_Frekventa.Benamning Onninen_x0020_Frekventa.Enhet ~
Onninen_x0020_Frekventa.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OFRE 
&Scoped-define QUERY-STRING-BRW_OFRE FOR EACH Onninen_x0020_Frekventa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OFRE OPEN QUERY BRW_OFRE FOR EACH Onninen_x0020_Frekventa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OFRE Onninen_x0020_Frekventa
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OFRE Onninen_x0020_Frekventa


/* Definitions for BROWSE BRW_ONHELA                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ONHELA Onninen_x0020_Hela.Enummer ~
Onninen_x0020_Hela.Benamning Onninen_x0020_Hela.Enhet ~
Onninen_x0020_Hela.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ONHELA 
&Scoped-define QUERY-STRING-BRW_ONHELA FOR EACH Onninen_x0020_Hela NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_ONHELA OPEN QUERY BRW_ONHELA FOR EACH Onninen_x0020_Hela NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_ONHELA Onninen_x0020_Hela
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ONHELA Onninen_x0020_Hela


/* Definitions for BROWSE BRW_OVR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OVR ovrigt.Enummer ovrigt.Benamning ~
ovrigt.Enhet ovrigt.Pris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OVR 
&Scoped-define QUERY-STRING-BRW_OVR FOR EACH ovrigt NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OVR OPEN QUERY BRW_OVR FOR EACH ovrigt NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OVR ovrigt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OVR ovrigt


/* Definitions for FRAME FRAME-KAT                                      */

/* Definitions for FRAME FRAME-XML                                      */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_INK2EX FBTN_INK1EX FBTN_IMPEX FBTN_EXP ~
FBTN_EXMA FBTN_MARKEX 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON FBTN_ASC 
     LABEL "Import asc-fil" 
     SIZE 14 BY 1 TOOLTIP "Elektroskandia format".

DEFINE BUTTON FBTN_EXMA 
     LABEL "Export märkta" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till excel".

DEFINE BUTTON FBTN_EXP 
     LABEL "Export exel" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till excel".

DEFINE BUTTON FBTN_IMPEX 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt)".

DEFINE BUTTON FBTN_INK1EX 
     LABEL "Imp Nexans Exel" 
     SIZE 14 BY 1 TOOLTIP "Import netto Nexans excel. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_INK2EX 
     LABEL "Imp NKT Exel" 
     SIZE 14 BY 1 TOOLTIP "Import netto NKT excel. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_MARKEX 
     LABEL "Märkta Exel" 
     SIZE 14 BY 1 TOOLTIP "Märker alla enr från excel i leverantörens materielkatalog. Enr i första kolumn".

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_ASCF 
     LABEL "asc-fil" 
     SIZE 7.25 BY 2.33 TOOLTIP "Läs in Elektroskandia mtrl fil i asc format".

DEFINE BUTTON MBTN_EXCELF 
     LABEL "xcel-Format" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_KAT 
     LABEL "Katalog" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_SKVF 
     LABEL "Skv-Format" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_XMLF 
     LABEL "Xml-Format" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTRENS 
     LABEL "Rensa från listan" 
     SIZE 12 BY 1 TOOLTIP "Markerade poster rensas från visningen.".

DEFINE BUTTON BTN_HMTRLF 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KAB1 
     LABEL "Hämta Nexans" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KAB2 
     LABEL "Hämta NKT" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MTRLF 
     LABEL "spara favorit" 
     SIZE 14 BY 1 TOOLTIP "Ska du lägga till favoriter hämta upp dina gamla favoriter först!".

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TEXT 
     LABEL "Text" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPP AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT_STOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.5 BY 2.29
     BGCOLOR 8 .

DEFINE BUTTON FBTN_BENUPPDAT 
     LABEL "Benämning skv" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning".

DEFINE BUTTON FBTN_IMP 
     LABEL "Import skv" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt, Färg 1 = Ja, Färg 2 = ?)".

DEFINE BUTTON FBTN_INKEX 
     LABEL "Imp kabel -skv" 
     SIZE 14 BY 1 TOOLTIP "Import netto kabel skv. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_INOEX 
     LABEL "Imp övrigt -skv" 
     SIZE 14 BY 1 TOOLTIP "Import netto övrigt skv. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_MARKSKV 
     LABEL "Märkta skv" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, märkt(Färg 1 = Ja Färg 2 = ?)".

DEFINE BUTTON FBTN_TUPPL 
     LABEL "Test skv mot upplägg" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON BTN_MNXML 
     LABEL "Ny märkning" 
     SIZE 14 BY 1 TOOLTIP "Tar bort den gamla katalogen och skapar ny katalog".

DEFINE BUTTON BTN_MUXML 
     LABEL "Uppdatera märkning" 
     SIZE 14 BY 1 TOOLTIP "Uppdaterar den redan inlästa katalogen".

DEFINE BUTTON BTN_NYK 
     LABEL "Ny katalog" 
     SIZE 14 BY 1 TOOLTIP "Tar bort den gamla katalogen och skapar ny katalog".

DEFINE BUTTON BTN_UPPDK 
     LABEL "Uppdatera katalog" 
     SIZE 14 BY 1 TOOLTIP "Uppdaterar den redan inlästa katalogen".

DEFINE BUTTON FBTN_EXPC 
     LABEL "Export xml" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till xml-fil".

DEFINE BUTTON FBTN_IMPXML 
     LABEL "Imp Hela xml" 
     SIZE 14 BY 1 TOOLTIP "Importera katalogen från xml-fil".

DEFINE BUTTON FBTN_IMXNEX 
     LABEL "Imp Nexans xml" 
     SIZE 14 BY 1 TOOLTIP "Importera katalogen från xml-fil".

DEFINE BUTTON FBTN_IMXNKT 
     LABEL "Imp NKT xml" 
     SIZE 14 BY 1 TOOLTIP "Importera katalogen från xml-fil".

DEFINE BUTTON FBTN_MARKXML 
     LABEL "Märk Frekventa xml" 
     SIZE 14 BY 1 TOOLTIP "Skapar och märker alla enr från xml-fil i leverantörens materielkatalog.".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AFRE FOR 
      Ahlsell_x0020_Frekventa SCROLLING.

DEFINE QUERY BRW_AHHELA FOR 
      Ahlsell_x0020_Hela SCROLLING.

DEFINE QUERY BRW_FRE FOR 
      Frekventa SCROLLING.

DEFINE QUERY BRW_LEVMTRL FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_NEX FOR 
      Nexans SCROLLING.

DEFINE QUERY BRW_NKT FOR 
      NKT SCROLLING.

DEFINE QUERY BRW_OFRE FOR 
      Onninen_x0020_Frekventa SCROLLING.

DEFINE QUERY BRW_ONHELA FOR 
      Onninen_x0020_Hela SCROLLING.

DEFINE QUERY BRW_OVR FOR 
      ovrigt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AFRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AFRE WINDOW-1 _STRUCTURED
  QUERY BRW_AFRE NO-LOCK DISPLAY
      Ahlsell_x0020_Frekventa.Enummer FORMAT "X(11)":U
      Ahlsell_x0020_Frekventa.Benamning FORMAT "X(256)":U WIDTH 57
      Ahlsell_x0020_Frekventa.Enhet FORMAT "X(5)":U
      Ahlsell_x0020_Frekventa.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster AH Frekventa.Om resultatet är ok välj Ny katalog eller".

DEFINE BROWSE BRW_AHHELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AHHELA WINDOW-1 _STRUCTURED
  QUERY BRW_AHHELA NO-LOCK DISPLAY
      Ahlsell_x0020_Hela.Enummer FORMAT "X(11)":U
      Ahlsell_x0020_Hela.Benamning FORMAT "X(256)":U WIDTH 57
      Ahlsell_x0020_Hela.Enhet FORMAT "X(5)":U
      Ahlsell_x0020_Hela.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster Ahlsell.Om resultatet är ok välj Ny katalog eller Uppdatera katalog för skarp inläsning".

DEFINE BROWSE BRW_FRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRE WINDOW-1 _STRUCTURED
  QUERY BRW_FRE NO-LOCK DISPLAY
      Frekventa.Enummer FORMAT "X(11)":U
      Frekventa.Benamning FORMAT "X(256)":U WIDTH 57
      Frekventa.Enhet FORMAT "X(5)":U
      Frekventa.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster Frekventa.Om resultatet är ok välj Ny katalog eller Uppdatera katalog för skarp inläsning. ".

DEFINE BROWSE BRW_LEVMTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEVMTRL WINDOW-1 _STRUCTURED
  QUERY BRW_LEVMTRL NO-LOCK DISPLAY
      mtrltemp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 57
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.BPRIS FORMAT ">>>>>99.99":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
      mtrltemp.KUND COLUMN-LABEL "Märkt" FORMAT "Ja/Nej":U
  ENABLE
      mtrltemp.Enr
      mtrltemp.Benamning
      mtrltemp.Enhet
      mtrltemp.BPRIS
      mtrltemp.NPRIS
      mtrltemp.KUND
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 98 BY 19
         TITLE "Materiel för vald leverantör. Varje post sparas när du lämnar raden.".

DEFINE BROWSE BRW_NEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NEX WINDOW-1 _STRUCTURED
  QUERY BRW_NEX NO-LOCK DISPLAY
      Nexans.Enummer FORMAT "X(11)":U
      Nexans.Benamning FORMAT "X(256)":U WIDTH 55
      Nexans.Enhet FORMAT "X(5)":U
      Nexans.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning Nexans".

DEFINE BROWSE BRW_NKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NKT WINDOW-1 _STRUCTURED
  QUERY BRW_NKT NO-LOCK DISPLAY
      NKT.Enummer FORMAT "X(11)":U
      NKT.Benamning FORMAT "X(256)":U WIDTH 55
      NKT.Enhet FORMAT "X(5)":U
      NKT.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning NKT".

DEFINE BROWSE BRW_OFRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OFRE WINDOW-1 _STRUCTURED
  QUERY BRW_OFRE NO-LOCK DISPLAY
      Onninen_x0020_Frekventa.Enummer FORMAT "X(11)":U
      Onninen_x0020_Frekventa.Benamning FORMAT "X(256)":U WIDTH 57
      Onninen_x0020_Frekventa.Enhet FORMAT "X(5)":U
      Onninen_x0020_Frekventa.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster Frekventa.Om resultatet är ok välj Ny katalog eller Upp".

DEFINE BROWSE BRW_ONHELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ONHELA WINDOW-1 _STRUCTURED
  QUERY BRW_ONHELA NO-LOCK DISPLAY
      Onninen_x0020_Hela.Enummer FORMAT "X(11)":U
      Onninen_x0020_Hela.Benamning FORMAT "X(256)":U WIDTH 57
      Onninen_x0020_Hela.Enhet FORMAT "X(5)":U
      Onninen_x0020_Hela.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster Onninen.Om resultatet är ok välj Ny katalog eller Uppdatera katalog för skarp inläsning".

DEFINE BROWSE BRW_OVR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OVR WINDOW-1 _STRUCTURED
  QUERY BRW_OVR NO-LOCK DISPLAY
      ovrigt.Enummer FORMAT "X(11)":U
      ovrigt.Benamning FORMAT "X(256)":U WIDTH 55
      ovrigt.Enhet FORMAT "X(5)":U
      ovrigt.Pris FORMAT ">>>>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 98 BY 19
         TITLE "Test inläsning 20 poster Hela.Om resultatet är ok välj Ny katalog eller Uppdatera katalog för skarp inläsning.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-GRUND
     MBTN_KAT AT ROW 1 COL 1
     MBTN_EXCELF AT ROW 1 COL 20.38
     MBTN_SKVF AT ROW 1 COL 36.88
     MBTN_XMLF AT ROW 1 COL 52.88
     MBTN_ASCF AT ROW 1 COL 64.5 WIDGET-ID 2
     BTN_AVB AT ROW 28.21 COL 100.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.88 BY 28.42.

DEFINE FRAME FRAME-KAT
     BRW_LEVMTRL AT ROW 1.33 COL 1.5
     BTN_MTRLF AT ROW 12.29 COL 99.75 WIDGET-ID 2
     BTN_HMTRLF AT ROW 13.38 COL 99.75 WIDGET-ID 4
     BTN_NY AT ROW 20.79 COL 10.88
     BTN_BORT AT ROW 20.79 COL 24.5
     BTN_BORTRENS AT ROW 20.79 COL 38.13 WIDGET-ID 6
     BTN_KAB1 AT ROW 20.79 COL 51.75
     BTN_KAB2 AT ROW 20.79 COL 65.38
     BTN_TEXT AT ROW 20.79 COL 79
     FILL-IN-ENR AT ROW 22.29 COL 16 COLON-ALIGNED
     FILL-IN-BEN AT ROW 22.29 COL 44 COLON-ALIGNED
     RAD_SOK AT ROW 23.29 COL 18.5 NO-LABEL
     FILL-IN-UPP AT ROW 22.29 COL 71.5 COLON-ALIGNED NO-LABEL
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .83 AT ROW 22.29 COL 2.5
     RECT_STOR AT ROW 21.96 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 114 BY 24.

DEFINE FRAME FRAME-XML
     BRW_NKT AT ROW 1.33 COL 1.5
     BRW_NEX AT ROW 1.33 COL 1.5
     BRW_FRE AT ROW 1.33 COL 1.5
     BRW_AHHELA AT ROW 1.33 COL 1.5
     BRW_AFRE AT ROW 1.33 COL 1.5
     BRW_OFRE AT ROW 1.33 COL 1.5
     BRW_ONHELA AT ROW 1.33 COL 1.5
     BRW_OVR AT ROW 1.33 COL 1.5
     FBTN_IMPXML AT ROW 7.46 COL 99.75
     FBTN_MARKXML AT ROW 8.71 COL 99.75
     FBTN_IMXNEX AT ROW 9.71 COL 99.75
     FBTN_IMXNKT AT ROW 11.21 COL 99.75
     FBTN_EXPC AT ROW 13.13 COL 99.75
     BTN_MNXML AT ROW 21 COL 7.5
     BTN_MUXML AT ROW 21 COL 24
     BTN_UPPDK AT ROW 21 COL 24
     BTN_NYK AT ROW 21.08 COL 7.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 113 BY 21.5.

DEFINE FRAME FRAME-FEXCELF
     FBTN_INK2EX AT ROW 2.38 COL 99.75
     FBTN_INK1EX AT ROW 3.5 COL 99.75
     FBTN_IMPEX AT ROW 5.5 COL 99.75
     FBTN_EXP AT ROW 7.54 COL 99.75
     FBTN_EXMA AT ROW 8.75 COL 100 WIDGET-ID 2
     FBTN_MARKEX AT ROW 10 COL 99.75
     "" VIEW-AS TEXT
          SIZE 25 BY 2 AT ROW 3.75 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 113 BY 18.5.

DEFINE FRAME FRAME-SKV
     FBTN_IMP AT ROW 9 COL 99.75
     FBTN_MARKSKV AT ROW 10 COL 99.75 WIDGET-ID 2
     FBTN_INKEX AT ROW 11.25 COL 99.75
     FBTN_INOEX AT ROW 13.25 COL 99.75
     FBTN_BENUPPDAT AT ROW 14.46 COL 99.75 WIDGET-ID 4
     FBTN_TUPPL AT ROW 20.08 COL 99.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 113 BY 21.75.

DEFINE FRAME FRAME-ASC
     FBTN_ASC AT ROW 9 COL 99.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 113 BY 21.75 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: Ahlsell_x0020_Frekventa T "?" NO-UNDO temp-db Ahlsell_x0020_Frekventa
      TABLE: Ahlsell_x0020_Hela T "?" NO-UNDO temp-db Ahlsell_x0020_Hela
      TABLE: Frekventa T "?" NO-UNDO temp-db Frekventa
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: Nexans T "?" NO-UNDO temp-db Nexans
      TABLE: NKT T "?" NO-UNDO temp-db NKT
      TABLE: Onninen_x0020_Frekventa T "?" NO-UNDO temp-db Onninen_x0020_Frekventa
      TABLE: Onninen_x0020_Hela T "?" NO-UNDO temp-db Onninen_x0020_Hela
      TABLE: ovrigt T "?" NO-UNDO temp-db ovrigt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppdatera materielkatalog"
         HEIGHT             = 28.42
         WIDTH              = 114.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 114.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 114.88
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ASC:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-FEXCELF:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-KAT:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-SKV:FRAME = FRAME FRAME-GRUND:HANDLE
       FRAME FRAME-XML:FRAME = FRAME FRAME-GRUND:HANDLE.

/* SETTINGS FOR FRAME FRAME-ASC
   NOT-VISIBLE                                                          */
ASSIGN 
       FBTN_ASC:HIDDEN IN FRAME FRAME-ASC           = TRUE.

/* SETTINGS FOR FRAME FRAME-FEXCELF
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FBTN_EXMA:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

ASSIGN 
       FBTN_EXP:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

ASSIGN 
       FBTN_IMPEX:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

ASSIGN 
       FBTN_INK1EX:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

ASSIGN 
       FBTN_INK2EX:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

ASSIGN 
       FBTN_MARKEX:HIDDEN IN FRAME FRAME-FEXCELF           = TRUE.

/* SETTINGS FOR FRAME FRAME-GRUND
   NOT-VISIBLE                                                          */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-KAT:MOVE-AFTER-TAB-ITEM (MBTN_ASCF:HANDLE IN FRAME FRAME-GRUND)
       XXTABVALXX = FRAME FRAME-XML:MOVE-BEFORE-TAB-ITEM (BTN_AVB:HANDLE IN FRAME FRAME-GRUND)
       XXTABVALXX = FRAME FRAME-FEXCELF:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-XML:HANDLE)
       XXTABVALXX = FRAME FRAME-SKV:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-FEXCELF:HANDLE)
       XXTABVALXX = FRAME FRAME-ASC:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-SKV:HANDLE)
       XXTABVALXX = FRAME FRAME-KAT:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ASC:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-KAT
                                                                        */
/* BROWSE-TAB BRW_LEVMTRL RECT_STOR FRAME-KAT */
ASSIGN 
       BRW_LEVMTRL:MAX-DATA-GUESS IN FRAME FRAME-KAT         = 40000
       BRW_LEVMTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KAT = TRUE.

ASSIGN 
       BTN_TEXT:HIDDEN IN FRAME FRAME-KAT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UPP IN FRAME FRAME-KAT
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-UPP:HIDDEN IN FRAME FRAME-KAT           = TRUE.

ASSIGN 
       RECT_STOR:HIDDEN IN FRAME FRAME-KAT           = TRUE.

/* SETTINGS FOR FRAME FRAME-SKV
   NOT-VISIBLE                                                          */
ASSIGN 
       FBTN_BENUPPDAT:HIDDEN IN FRAME FRAME-SKV           = TRUE.

ASSIGN 
       FBTN_IMP:HIDDEN IN FRAME FRAME-SKV           = TRUE.

ASSIGN 
       FBTN_MARKSKV:HIDDEN IN FRAME FRAME-SKV           = TRUE.

ASSIGN 
       FBTN_TUPPL:HIDDEN IN FRAME FRAME-SKV           = TRUE.

/* SETTINGS FOR FRAME FRAME-XML
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_NKT 1 FRAME-XML */
/* BROWSE-TAB BRW_NEX BRW_NKT FRAME-XML */
/* BROWSE-TAB BRW_FRE BRW_NEX FRAME-XML */
/* BROWSE-TAB BRW_AHHELA BRW_FRE FRAME-XML */
/* BROWSE-TAB BRW_AFRE BRW_AHHELA FRAME-XML */
/* BROWSE-TAB BRW_OFRE BRW_AFRE FRAME-XML */
/* BROWSE-TAB BRW_ONHELA BRW_OFRE FRAME-XML */
/* BROWSE-TAB BRW_OVR BRW_ONHELA FRAME-XML */
ASSIGN 
       BRW_AFRE:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_AHHELA:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_FRE:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_NEX:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_NKT:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_OFRE:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_ONHELA:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BRW_OVR:HIDDEN  IN FRAME FRAME-XML                = TRUE.

ASSIGN 
       BTN_MNXML:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       BTN_MUXML:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       BTN_NYK:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       BTN_UPPDK:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       FBTN_EXPC:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       FBTN_IMPXML:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       FBTN_IMXNEX:HIDDEN IN FRAME FRAME-XML           = TRUE.

ASSIGN 
       FBTN_IMXNKT:HIDDEN IN FRAME FRAME-XML           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AFRE
/* Query rebuild information for BROWSE BRW_AFRE
     _TblList          = "Temp-Tables.Ahlsell_x0020_Frekventa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Ahlsell_x0020_Frekventa.Enummer
"Ahlsell_x0020_Frekventa.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Ahlsell_x0020_Frekventa.Benamning
"Ahlsell_x0020_Frekventa.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Ahlsell_x0020_Frekventa.Enhet
"Ahlsell_x0020_Frekventa.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Ahlsell_x0020_Frekventa.Pris
"Ahlsell_x0020_Frekventa.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AFRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AHHELA
/* Query rebuild information for BROWSE BRW_AHHELA
     _TblList          = "Temp-Tables.Ahlsell_x0020_Hela"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Ahlsell_x0020_Hela.Enummer
"Ahlsell_x0020_Hela.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Ahlsell_x0020_Hela.Benamning
"Ahlsell_x0020_Hela.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Ahlsell_x0020_Hela.Enhet
"Ahlsell_x0020_Hela.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Ahlsell_x0020_Hela.Pris
"Ahlsell_x0020_Hela.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AHHELA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRE
/* Query rebuild information for BROWSE BRW_FRE
     _TblList          = "Temp-Tables.Frekventa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Frekventa.Enummer
"Frekventa.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Frekventa.Benamning
"Frekventa.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Frekventa.Enhet
"Frekventa.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Frekventa.Pris
"Frekventa.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEVMTRL
/* Query rebuild information for BROWSE BRW_LEVMTRL
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = leverant
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ">>>>>99.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.mtrltemp.KUND
"mtrltemp.KUND" "Märkt" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEVMTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NEX
/* Query rebuild information for BROWSE BRW_NEX
     _TblList          = "Temp-Tables.Nexans"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Nexans.Enummer
"Nexans.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Nexans.Benamning
"Nexans.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "55" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Nexans.Enhet
"Nexans.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Nexans.Pris
"Nexans.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NEX */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NKT
/* Query rebuild information for BROWSE BRW_NKT
     _TblList          = "Temp-Tables.NKT"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.NKT.Enummer
"NKT.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.NKT.Benamning
"NKT.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "55" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.NKT.Enhet
"NKT.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.NKT.Pris
"NKT.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OFRE
/* Query rebuild information for BROWSE BRW_OFRE
     _TblList          = "Temp-Tables.Onninen_x0020_Frekventa"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Onninen_x0020_Frekventa.Enummer
"Onninen_x0020_Frekventa.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Onninen_x0020_Frekventa.Benamning
"Onninen_x0020_Frekventa.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Onninen_x0020_Frekventa.Enhet
"Onninen_x0020_Frekventa.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Onninen_x0020_Frekventa.Pris
"Onninen_x0020_Frekventa.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OFRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ONHELA
/* Query rebuild information for BROWSE BRW_ONHELA
     _TblList          = "Temp-Tables.Onninen_x0020_Hela"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.Onninen_x0020_Hela.Enummer
"Onninen_x0020_Hela.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.Onninen_x0020_Hela.Benamning
"Onninen_x0020_Hela.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.Onninen_x0020_Hela.Enhet
"Onninen_x0020_Hela.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.Onninen_x0020_Hela.Pris
"Onninen_x0020_Hela.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ONHELA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OVR
/* Query rebuild information for BROWSE BRW_OVR
     _TblList          = "Temp-Tables.ovrigt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ovrigt.Enummer
"ovrigt.Enummer" ? "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ovrigt.Benamning
"ovrigt.Benamning" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "55" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ovrigt.Enhet
"ovrigt.Enhet" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ovrigt.Pris
"ovrigt.Pris" ? ">>>>>>99.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OVR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ASC
/* Query rebuild information for FRAME FRAME-ASC
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ASC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FEXCELF
/* Query rebuild information for FRAME FRAME-FEXCELF
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FEXCELF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KAT
/* Query rebuild information for FRAME FRAME-KAT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KAT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-SKV
/* Query rebuild information for FRAME FRAME-SKV
     _Query            is NOT OPENED
*/  /* FRAME FRAME-SKV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-XML
/* Query rebuild information for FRAME FRAME-XML
     _Query            is NOT OPENED
*/  /* FRAME FRAME-XML */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LEVMTRL
&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME BRW_LEVMTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEVMTRL WINDOW-1
ON ROW-LEAVE OF BRW_LEVMTRL IN FRAME FRAME-KAT /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:
  
  IF AVAILABLE mtrltemp THEN DO:
     {EUROPEANAMERICAN.I} 
     RUN visa_UI.    
     ASSIGN     
     mtrltemp.Enr = INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr
     mtrltemp.Benamning = INPUT BROWSE BRW_LEVMTRL mtrltemp.Benamning
     mtrltemp.Enhet = LC(INPUT BROWSE BRW_LEVMTRL mtrltemp.Enhet)
     mtrltemp.Bpris = INPUT BROWSE BRW_LEVMTRL mtrltemp.Bpris
     mtrltemp.NPRIS = INPUT BROWSE BRW_LEVMTRL mtrltemp.NPRIS.
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.
     CREATE emtrltemp.     
     IF mtrltemp.NPRIS = 0 THEN DO:
        mtrltemp.NPRIS = mtrltemp.BPRIS.           
     END.
     BUFFER-COPY mtrltemp TO emtrltemp. 
     RUN admsparmtrl_UI IN mtrlbapph ( INPUT-OUTPUT TABLE emtrltemp ). 
     FIND FIRST emtrltemp NO-ERROR.
     IF AVAILABLE emtrltemp THEN DO:
         FIND FIRST mtrltemp WHERE mtrltemp.LEVKOD = emtrltemp.LEVKOD AND   
         mtrltemp.ENR = emtrltemp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:
            mtrltemp.MTRLROW = emtrltemp.MTRLROW.          
            DELETE emtrltemp.
         END.
     END.
     {AMERICANEUROPEAN.I}
     RUN visa_UI. 
  END.
  
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEVMTRL WINDOW-1
ON VALUE-CHANGED OF BRW_LEVMTRL IN FRAME FRAME-KAT /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:  
   status-ok = BRW_LEVMTRL:SELECT-FOCUSED-ROW() NO-ERROR.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enr IN BROWSE BRW_LEVMTRL /* Enr */
DO:
   status-ok = BRW_LEVMTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.    
   IF status-ok = TRUE THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         DISPLAY mtrltemp.ENR WITH BROWSE BRW_LEVMTRL.         
      END.
   END.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enr IN BROWSE BRW_LEVMTRL /* Enr */
DO:   
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr = "" THEN DO:     
         MESSAGE  Guru.Konstanter:genk + " får inte vara blankt. Vill Ni ta bort raden?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
         IF svar THEN DO TRANSACTION:
            bortfraga = TRUE.
            APPLY "CHOOSE" TO BTN_BORT IN FRAME {&FRAME-NAME}.
            bortfraga = FALSE.
            svar = FALSE.
            RETURN NO-APPLY.
         END.
         ELSE DO:
            DISPLAY mtrltemp.Enr WITH BROWSE BRW_LEVMTRL.         
            RETURN NO-APPLY.      
         END.        
      END.    
      IF mtrltemp.Enr NE INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr THEN DO:        
         kollenr = INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr.      
         RUN kollenr_UI IN mtrlbapph (INPUT mtrltemp.MTRLROW, INPUT kollenr,INPUT leverant, OUTPUT TABLE felmeddtemp ).    
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         DISPLAY mtrltemp.Enr WITH BROWSE BRW_LEVMTRL.    
         APPLY "ENTRY" TO mtrltemp.Enr IN BROWSE BRW_LEVMTRL.
         RETURN NO-APPLY.      
      END.                  
      mtrltemp.Enr = INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr.
      DISPLAY mtrltemp.Enr WITH BROWSE BRW_LEVMTRL.          
   END.
   {AMERICANEUROPEAN.I}
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Benamning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Benamning IN BROWSE BRW_LEVMTRL /* Benämning */
DO:   
  IF AVAILABLE mtrltemp THEN DO:
     {EUROPEANAMERICAN.I}
     RUN visa_UI.
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Benamning IN BROWSE BRW_LEVMTRL /* Benämning */
DO:   
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_LEVMTRL mtrltemp.BENAMNING = "" THEN DO:     
         MESSAGE  "Benämning får inte vara blankt." VIEW-AS ALERT-BOX.
         DISPLAY mtrltemp.BENAMNING WITH BROWSE BRW_LEVMTRL.    
         RETURN NO-APPLY.      
      END.          
      IF mtrltemp.BENAMNING NE INPUT BROWSE BRW_LEVMTRL mtrltemp.BENAMNING THEN DO: 
         /*rowvar = ROWID(mtrltemp).*/
         mtrltemp.BENAMNING = INPUT BROWSE BRW_LEVMTRL mtrltemp.BENAMNING.           
         DISPLAY mtrltemp.BENAMNING  WITH BROWSE BRW_LEVMTRL.    
      END. 
   END.
   /*ELSE rowvar = ?.*/
    {AMERICANEUROPEAN.I}
     RUN visa_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enhet IN BROWSE BRW_LEVMTRL /* Enhet */
DO:
   IF AVAILABLE mtrltemp THEN DO:
     {EUROPEANAMERICAN.I}
     RUN visa_UI.
  END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enhet IN BROWSE BRW_LEVMTRL /* Enhet */
DO:   
   /*IF mtrltemp.ENHET NE INPUT BROWSE BRW_LEVMTRL mtrltemp.ENHET THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.ENHET = LC(INPUT BROWSE BRW_LEVMTRL mtrltemp.ENHET).           
      DISPLAY mtrltemp.ENHET WITH BROWSE BRW_LEVMTRL.   
   END.
    {AMERICANEUROPEAN.I}
     RUN visa_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.BPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.BPRIS IN BROWSE BRW_LEVMTRL /* Brutto pris */
DO:
   IF AVAILABLE mtrltemp THEN DO:
     {EUROPEANAMERICAN.I}
     RUN visa_UI.
  END.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.BPRIS IN BROWSE BRW_LEVMTRL /* Brutto pris */
DO:
   /*IF mtrltemp.BPRIS NE INPUT BROWSE BRW_LEVMTRL mtrltemp.BPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.BPRIS = INPUT BROWSE BRW_LEVMTRL mtrltemp.BPRIS.           
      DISPLAY mtrltemp.BPRIS WITH BROWSE BRW_LEVMTRL.        
   END.
    {AMERICANEUROPEAN.I}
     RUN visa_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.NPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.NPRIS IN BROWSE BRW_LEVMTRL /* Netto pris */
DO:
  IF AVAILABLE mtrltemp THEN DO:
     {EUROPEANAMERICAN.I}
     RUN visa_UI.
  END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.NPRIS IN BROWSE BRW_LEVMTRL /* Netto pris */
DO:
   /*IF mtrltemp.NPRIS NE INPUT BROWSE BRW_LEVMTRL mtrltemp.NPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.NPRIS = INPUT BROWSE BRW_LEVMTRL mtrltemp.NPRIS.           
      DISPLAY mtrltemp.NPRIS WITH BROWSE BRW_LEVMTRL.             
   END.
    {AMERICANEUROPEAN.I}
     RUN visa_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.KUND BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.KUND IN BROWSE BRW_LEVMTRL /* Märkt */
DO:
  IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.KUND = INPUT BROWSE BRW_LEVMTRL mtrltemp.KUND.           
      DISPLAY mtrltemp.KUND WITH BROWSE BRW_LEVMTRL.             
   END.
    {AMERICANEUROPEAN.I}
     RUN visa_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.KUND BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF mtrltemp.KUND IN BROWSE BRW_LEVMTRL /* Märkt */
DO:
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_LEVMTRL mtrltemp.KUND = TRUE THEN mtrltemp.KUND = FALSE.
      IF INPUT BROWSE BRW_LEVMTRL mtrltemp.KUND = FALSE THEN mtrltemp.KUND = TRUE.        
      DISPLAY mtrltemp.KUND WITH BROWSE BRW_LEVMTRL.             
   END.
   {AMERICANEUROPEAN.I}
   RUN visa_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-GRUND /* Avsluta */
DO:
  

   /*APPLY "VALUE-CHANGED" TO BRW_LEVMTRL IN FRAME FRAME-FEXCELF.   */
  APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-KAT /* Ta bort */
DO:
   
   RUN bort_UI.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTRENS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTRENS WINDOW-1
ON CHOOSE OF BTN_BORTRENS IN FRAME FRAME-KAT /* Rensa från listan */
DO:
   
   RUN sel_back_UI IN brwh.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HMTRLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HMTRLF WINDOW-1
ON CHOOSE OF BTN_HMTRLF IN FRAME FRAME-KAT /* HÄMTA favorit */
DO:
   RUN hmtfavoriter_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KAB1 WINDOW-1
ON CHOOSE OF BTN_KAB1 IN FRAME FRAME-KAT /* Hämta Nexans */
DO:   
  {muswait.i}   
  RUN mtrlhmtkab_UI IN mtrlbapph (INPUT leverant,INPUT 1, OUTPUT TABLE mtrltemp ).    
  RUN openbdynspec_UI IN brwh.
  RUN lastselectdyn_UI IN brwh.       
  RUN title_UI IN brwh.
  {musarrow.i}
 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KAB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KAB2 WINDOW-1
ON CHOOSE OF BTN_KAB2 IN FRAME FRAME-KAT /* Hämta NKT */
DO:
  {muswait.i}      
  RUN mtrlhmtkab_UI IN mtrlbapph (INPUT leverant,INPUT 2, OUTPUT TABLE mtrltemp ).    
  RUN openbdynspec_UI IN brwh.
  RUN lastselectdyn_UI IN brwh.
  RUN title_UI IN brwh.
  {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME BTN_MNXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MNXML WINDOW-1
ON CHOOSE OF BTN_MNXML IN FRAME FRAME-XML /* Ny märkning */
DO:
      
   MESSAGE "Vill du verkligen ta bort gamla märkningar och skapa nya märkningar för " + levtemp.LEVNAMN + " ?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Märkning xml-katalog för " + levtemp.LEVNAMN UPDATE svar7 AS LOGICAL.         
   IF svar7 = TRUE  THEN DO:          
      {muswait.i}   
      IF hjkat = 2  THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Ahlsell_x0020_Frekventa NO-ERROR. 
         EMPTY TEMP-TABLE Onninen_x0020_Frekventa NO-ERROR. 
         EMPTY TEMP-TABLE Frekventa NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.         
         IF (Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "Celpa") AND levtemp.LEVNAMN BEGINS "ahlsell" THEN DO:      
            FOR EACH Ahlsell_x0020_Frekventa WHERE Ahlsell_x0020_Frekventa.Enummer = "" :
               DELETE Ahlsell_x0020_Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Ahlsell_x0020_Frekventa,INPUT leverant, INPUT TRUE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE IF (Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa") AND levtemp.LEVNAMN BEGINS "Onninen" THEN DO:      
            FOR EACH Onninen_x0020_Frekventa WHERE Onninen_x0020_Frekventa.Enummer = "" :
               DELETE Onninen_x0020_Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Onninen_x0020_Frekventa,INPUT leverant, INPUT TRUE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE DO:
            FOR EACH Frekventa WHERE Frekventa.Enummer = "" :
               DELETE Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Frekventa,INPUT leverant, INPUT TRUE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".
            RUN openfel_UI.                      
                 
         END.   
         MESSAGE STRING(inlant) + " Frekventa poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.               
      END.      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = leverant.                    
                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN DO:
         CREATE extradatatemp.
         ASSIGN
         extradatatemp.PROGRAM = "INLKAT"                   
         extradatatemp.HUVUDCH = leverant
         extradatatemp.SOKLOG[1] = FALSE.
      END.
      ASSIGN
      extradatatemp.SOKDATE[1] = TODAY 
      extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
      extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
      extradatatemp.SOKLOG[1] =  TRUE.
      RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      hjkat = 0.
      
      {musarrow.i}
   END.   
   BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME BTN_MTRLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MTRLF WINDOW-1
ON CHOOSE OF BTN_MTRLF IN FRAME FRAME-KAT /* spara favorit */
DO:
   RUN sparaaofavoriter_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME BTN_MUXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MUXML WINDOW-1
ON CHOOSE OF BTN_MUXML IN FRAME FRAME-XML /* Uppdatera märkning */
DO:
   MESSAGE "Vill du verkligen uppdatera befintliga märkningar med vald fil för " + levtemp.LEVNAMN + " ?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Uppdatering av befintliga märkningar med xml-fil för " + levtemp.LEVNAMN UPDATE svar7 AS LOGICAL.         
   IF svar7 = TRUE  THEN DO:          
      {muswait.i}
      IF hjkat = 2  THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Frekventa NO-ERROR. 

         EMPTY TEMP-TABLE Ahlsell_x0020_Frekventa NO-ERROR. 
         EMPTY TEMP-TABLE Onninen_x0020_Frekventa NO-ERROR. 
         EMPTY TEMP-TABLE Frekventa NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         
         IF (Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa") AND levtemp.LEVNAMN BEGINS "ahlsell" THEN DO:      
            FOR EACH Ahlsell_x0020_Frekventa WHERE Ahlsell_x0020_Frekventa.Enummer = "" :
               DELETE Ahlsell_x0020_Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Ahlsell_x0020_Frekventa,INPUT leverant, INPUT FALSE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE IF (Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa") AND levtemp.LEVNAMN BEGINS "Onninen" THEN DO:      
            FOR EACH Onninen_x0020_Frekventa WHERE Onninen_x0020_Frekventa.Enummer = "" :
               DELETE Onninen_x0020_Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Onninen_x0020_Frekventa,INPUT leverant, INPUT FALSE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE DO:
            FOR EACH Frekventa WHERE Frekventa.Enummer = "" :
               DELETE Frekventa.
            END.
            RUN fremkatxml_UI IN mtrlxapph (INPUT TABLE Frekventa,INPUT leverant, INPUT FALSE,INPUT TRUE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.

         
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".
            RUN openfel_UI.                      
                     
         END.   
         MESSAGE STRING(inlant) + " Frekventa poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = leverant.                    
                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN DO:
         CREATE extradatatemp.
         ASSIGN
         extradatatemp.PROGRAM = "INLKAT"                   
         extradatatemp.HUVUDCH = leverant
         extradatatemp.SOKLOG[1] = FALSE.
      END.
      ASSIGN
      extradatatemp.SOKDATE[1] = TODAY 
      extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
      extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
      extradatatemp.SOKLOG[1] =  TRUE.
      RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      hjkat = 0.      
      {musarrow.i}
   END.
   BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-KAT /* Ny */
DO:  
   CREATE mtrltemp.
   mtrltemp.LEVKOD = leverant.
   RUN setlastrowid_UI IN brwh (INPUT ROWID(mtrltemp)).
   RUN openbdynspec_UI IN brwh.
   RUN lastselectdyn_UI IN brwh.
   ASSIGN
   
   mtrltemp.KALKNR = 0
   mtrltemp.ENR = ""
   mtrltemp.BENAMNING = ""
   mtrltemp.MTRLROW = ?.
   APPLY "ENTRY" TO mtrltemp.ENR IN BROWSE BRW_LEVMTRL.                                                                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME BTN_NYK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYK WINDOW-1
ON CHOOSE OF BTN_NYK IN FRAME FRAME-XML /* Ny katalog */
DO:
      
   MESSAGE "Vill du verkligen ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Inläsning ny xml-katalog för " + levtemp.LEVNAMN UPDATE svar7 AS LOGICAL.         
   IF svar7 = TRUE  THEN DO:          
      {muswait.i}   
      IF hjkat = 1 THEN DO:      
         
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Ahlsell_x0020_Hela NO-ERROR. 
         EMPTY TEMP-TABLE Onninen_x0020_Hela NO-ERROR. 
         EMPTY TEMP-TABLE Ovrigt NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         
         FOR EACH Ahlsell_x0020_Hela WHERE Ahlsell_x0020_Hela.Enummer = "" :
            DELETE Ahlsell_x0020_Hela.
         END.
         FOR EACH Onninen_x0020_Hela WHERE Onninen_x0020_Hela.Enummer = "" :
            DELETE Onninen_x0020_Hela.
         END.
         FOR EACH Ovrigt WHERE Ovrigt.Enummer = "" :
            DELETE Ovrigt.
         END.
         IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" AND levtemp.LEVNAMN BEGINS "ahlsell" THEN DO:      
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Ahlsell_x0020_Hela,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" AND levtemp.LEVNAMN BEGINS "Onninen" THEN DO:      
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Onninen_x0020_Hela,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE DO:
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Ovrigt,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".  
            RUN openfel_UI.                    
                     
         END.   
         MESSAGE STRING(inlant) + " poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      ELSE IF hjkat = 3 THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Nexans NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         FOR EACH Nexans WHERE Nexans.Enummer = "" :
            DELETE Nexans.
         END.
         RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Nexans,INPUT leverant, INPUT TRUE,INPUT TRUE,OUTPUT inlant,INPUT 1,OUTPUT TABLE felmex ).                   
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".   
            RUN openfel_UI.                   
                    
         END.   
         MESSAGE STRING(inlant) + " Nexans poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      ELSE IF hjkat = 4 THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE NKT NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         FOR EACH NKT WHERE NKT.Enummer = "" :
            DELETE NKT.
         END.
         RUN mkatxml_UI IN mtrlxapph (INPUT TABLE NKT,INPUT leverant, INPUT TRUE,INPUT TRUE,OUTPUT inlant,INPUT 2,OUTPUT TABLE felmex ).                   
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
            RUN openfel_UI.                     
                    
         END.   
         MESSAGE STRING(inlant) + " NKT poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = leverant.                    
                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN DO:
         CREATE extradatatemp.
         ASSIGN
         extradatatemp.PROGRAM = "INLKAT"                   
         extradatatemp.HUVUDCH = leverant
         extradatatemp.SOKLOG[1] = FALSE.
      END.
      ASSIGN
      extradatatemp.SOKDATE[1] = TODAY 
      extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
      extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
      extradatatemp.SOKLOG[1] =  TRUE.
      RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      hjkat = 0.
      
      {musarrow.i}
   END.   
   BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME BTN_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TEXT WINDOW-1
ON CHOOSE OF BTN_TEXT IN FRAME FRAME-KAT /* Text */
DO:                                        
   antal_valda = BRW_LEVMTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_LEVMTRL:FETCH-SELECTED-ROW(antal_raknare).
      /*{muswait.i}   */
      {AVBGOM.I}
      EMPTY TEMP-TABLE emtrltemp.
      CREATE emtrltemp.
      BUFFER-COPY mtrltemp TO emtrltemp.
      RUN VISATEXTV.W (INPUT TABLE emtrltemp).
      {AVBFRAM.I}
      antal_raknare = antal_raknare + 1.
     /* {musarrow.i}        */
   END.                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME BTN_UPPDK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPDK WINDOW-1
ON CHOOSE OF BTN_UPPDK IN FRAME FRAME-XML /* Uppdatera katalog */
DO:
   MESSAGE "Vill du verkligen uppdatera befintlig katalog med vald fil för " + levtemp.LEVNAMN + " ?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Uppdatering av befintlig katalog med xml-fil för " + levtemp.LEVNAMN UPDATE svar7 AS LOGICAL.         
   IF svar7 = TRUE  THEN DO:          
      {muswait.i}
     
      IF hjkat = 1 THEN DO:
         
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Ahlsell_x0020_Hela NO-ERROR. 
         EMPTY TEMP-TABLE Onninen_x0020_Hela NO-ERROR. 
         EMPTY TEMP-TABLE Ovrigt NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         FOR EACH Ahlsell_x0020_Hela WHERE Ahlsell_x0020_Hela.Enummer = "" :
            DELETE Ahlsell_x0020_Hela.
         END.
         FOR EACH Onninen_x0020_Hela WHERE Onninen_x0020_Hela.Enummer = "" :
            DELETE Onninen_x0020_Hela.
         END.
         FOR EACH Ovrigt WHERE Ovrigt.Enummer = "" :
            DELETE Ovrigt.
         END.
         IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" AND levtemp.LEVNAMN BEGINS "ahlsell" THEN DO:      
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Ahlsell_x0020_Hela,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" AND levtemp.LEVNAMN BEGINS "Onninen" THEN DO:      
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Onninen_x0020_Hela,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         ELSE DO:
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Ovrigt,INPUT leverant, INPUT TRUE,INPUT FALSE,OUTPUT inlant,INPUT 0,OUTPUT TABLE felmex ).                   
         END.
         
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".
            RUN openfel_UI.                      
                               
         END.   
         MESSAGE STRING(inlant) + " poster för " + levtemp.LEVNAMN + " är nu inläst. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      ELSE IF hjkat = 3 THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE Nexans NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         FOR EACH Nexans WHERE Nexans.Enummer = "" :
            DELETE Nexans.
         END.
         RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Nexans,INPUT leverant, INPUT FALSE,INPUT TRUE,OUTPUT inlant,INPUT 1,OUTPUT TABLE felmex ).                   
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
            RUN openfel_UI.          
         END.   
         MESSAGE STRING(inlant) + " Nexans poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      ELSE IF hjkat = 4 THEN DO:      
         EMPTY TEMP-TABLE felmex NO-ERROR. 
         EMPTY TEMP-TABLE NKT NO-ERROR. 
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
         RUN xmlslut_UI IN dynxml.
         FOR EACH NKT WHERE NKT.Enummer = "" :
            DELETE NKT.
         END.
         RUN mkatxml_UI IN mtrlxapph (INPUT TABLE NKT,INPUT leverant, INPUT FALSE,INPUT TRUE,OUTPUT inlant,INPUT 2,OUTPUT TABLE felmex ).                   
         FIND FIRST felex NO-ERROR.
         IF AVAILABLE felex THEN DO:               
            felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
            RUN openfel_UI.       
         END.   
         MESSAGE STRING(inlant) + " NKT poster för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen i fliken Katalog. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.      
         
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = leverant.                    
                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN DO:
         CREATE extradatatemp.
         ASSIGN
         extradatatemp.PROGRAM = "INLKAT"                   
         extradatatemp.HUVUDCH = leverant
         extradatatemp.SOKLOG[1] = FALSE.
      END.
      ASSIGN
      extradatatemp.SOKDATE[1] = TODAY 
      extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
      extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
      extradatatemp.SOKLOG[1] =  TRUE.
      RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      hjkat = 0.      
      {musarrow.i}
   END.
   BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ASC
&Scoped-define SELF-NAME FBTN_ASC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ASC WINDOW-1
ON CHOOSE OF FBTN_ASC IN FRAME FRAME-ASC /* Import asc-fil */
DO: 
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning asc-fil för " + levtemp.LEVNAMN + "." UPDATE svar5 AS LOGICAL.      
   CASE svar5:          
      WHEN TRUE OR WHEN FALSE THEN DO:   
          
         {muswait.i}                        
         DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir         
         TITLE          "Välj den fil som motsvarar materielkatalog"         
         FILTERS        "Semikolon filer (*.asc)"  "*.asc"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY         
         UPDATE OKvald.         
         IF OKvald = TRUE THEN DO:    
            fildirq =     REPLACE(fildir,"asc","q").
                            
            EMPTY TEMP-TABLE tidin NO-ERROR. 
         /*  
            utfil = "C:\protemp11\nyelekt.txt".
         wtidvar = "C:\protemp11\nyelekt.q".*/
            kommando = SEARCH("quoter.exe").      
            OS-COMMAND SILENT VALUE(kommando) VALUE(fildir) > VALUE(fildirq).   
            IF OS-ERROR > 0 THEN DO:         
               RETURN.
            END.               
            INPUT FROM VALUE(fildirq) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
            
            REPEAT:
               SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.
               CREATE tidin.
               ASSIGN tidin.TIN = words.
            END.
            INPUT CLOSE. 
            /*OS-DELETE VALUE(wtidvar).*/
            
            FOR EACH tidin WHERE tidin.TIN = "":
               DELETE tidin.
            END.            
            decivar = 2.    
            ASSIGN
            E1 = 19
            E2 = 25
            B1 = 26
            B2 = 60
            ENH1 = 61
            ENH2 = 62
            NP1 = 96
            NP2 = 104
            BP1 = 84
            BP2 = 92
            pos2 = 104.
            
            IF Guru.Konstanter:appcon THEN DO:
               RUN CREMTRLU.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                             (INPUT decivar, INPUT pos2, INPUT leverant, INPUT E1, INPUT E2, INPUT B1, 
                              INPUT B2, INPUT ENH1, INPUT ENH2, INPUT BP1, INPUT BP2, INPUT NP1, 
                              INPUT NP2, svar5, INPUT TABLE tidin ).
           END.
           ELSE DO:
              RUN CREMTRLU.P  (INPUT decivar, INPUT pos2, INPUT leverant, INPUT E1, INPUT E2, INPUT B1, 
                              INPUT B2, INPUT ENH1, INPUT ENH2, INPUT BP1, INPUT BP2, INPUT NP1, 
                              INPUT NP2, svar5, INPUT TABLE tidin ).
           END.                       
         
                                      
         END.             
         {musarrow.i}      
      
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = leverant.                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = leverant
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               RUN openfel_UI.       
            END.   
            
            
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SKV
&Scoped-define SELF-NAME FBTN_BENUPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BENUPPDAT WINDOW-1
ON CHOOSE OF FBTN_BENUPPDAT IN FRAME FRAME-SKV /* Benämning skv */
DO:           
     
   MESSAGE "Vill du uppdatera benämningar på befintligt materiel för " + levtemp.LEVNAMN + " katalogen  ? - Svara Ja"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Uppdatering benömning enr för " + levtemp.LEVNAMN + ". Ordning: Enr,benämning" UPDATE svar14 AS LOGICAL.      
   CASE svar14:
      WHEN TRUE OR WHEN FALSE THEN DO:   
         
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den skv som har enummer med benämningar som skall uppdatera vald materielkatalog"
         FILTERS        "All Files (*.skv)"  "*.skv"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                               
            {muswait.i}                                 
            RUN IMPMTRLBENSKVNM.P (INPUT filnamn, INPUT leverant).              
            {musarrow.i}            
         END.       
      END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BENUPPDAT WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_BENUPPDAT IN FRAME FRAME-SKV /* Benämning skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define SELF-NAME FBTN_EXMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXMA WINDOW-1
ON CHOOSE OF FBTN_EXMA IN FRAME FRAME-FEXCELF /* Export märkta */
DO: 
  {muswait.i}
   {AVBGOM.I}
   IF Guru.Konstanter:varforetypval[38] NE 15 THEN DO:
      MESSAGE "Vill du exportera märkningar för färg 1 (Märkt=Ja)? - Svara Ja" SKIP
     "Vill du exportera märkningar för färg 2 (Märkt=?)? - Svara Nej"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för färg1/ färg2." UPDATE svar9 AS LOGICAL.      
      CASE svar9:
         WHEN TRUE OR WHEN FALSE THEN DO:
         END.
         OTHERWISE RETURN.
      END CASE.               
   END.
   ELSE svar9 = TRUE.
   IF svar9 = TRUE THEN RUN EXPMTRLEXCV.P (INPUT leverant, INPUT  2).
   ELSE RUN EXPMTRLEXCV.P (INPUT leverant, INPUT  3).
   {AVBFRAM.I}   
   {musarrow.i}                                                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXP WINDOW-1
ON CHOOSE OF FBTN_EXP IN FRAME FRAME-FEXCELF /* Export exel */
DO: 
  {muswait.i}
   {AVBGOM.I}   
   RUN EXPMTRLEXCV.P (INPUT leverant, INPUT  1).
   {AVBFRAM.I}   
   {musarrow.i}                                                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME FBTN_EXPC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXPC WINDOW-1
ON CHOOSE OF FBTN_EXPC IN FRAME FRAME-XML /* Export xml */
DO: 
   
   {muswait.i}
   RUN EXPKATT.P (INPUT leverant, OUTPUT filnamnz).
   {musarrow.i}
   MESSAGE filnamnz VIEW-AS ALERT-BOX.
   /*RUN EXPMTRLEXCV.P (INPUT leverant).*/
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SKV
&Scoped-define SELF-NAME FBTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMP WINDOW-1
ON CHOOSE OF FBTN_IMP IN FRAME FRAME-SKV /* Import skv */
DO: 
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning skv-katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt)" UPDATE svar5 AS LOGICAL.      
   CASE svar5:          
      WHEN TRUE OR WHEN FALSE THEN DO:   
         IF Guru.Konstanter:varforetypval[38] NE 15 THEN DO:
            IF Guru.Konstanter:globforetag = "krin" OR Guru.Konstanter:globforetag = "gran" THEN DO:
               IF  Guru.Konstanter:globforetag = "gran" THEN hjfrag = "Vill du skapa märkningar för färg 1 (Märkt=A) och färg 2 (Märkt=B)  ?".
               ELSE hjfrag = "Vill du skapa märkningar för färg 1 (Märkt=Ja) och färg 2 (Märkt=?)  ?".                             
               MESSAGE hjfrag VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för färg1 och färg2." UPDATE svar12 AS LOGICAL.
                     
               CASE svar12:
                  WHEN TRUE OR WHEN FALSE THEN DO:
                  END.
                  OTHERWISE RETURN.
               END CASE.   
            END.   
            ELSE svar12 = FALSE.   
            IF svar12 = FALSE THEN DO:
               MESSAGE "Vill du skapa märkningar för färg 1 (Märkt=Ja)? - Svara Ja" SKIP
              "Vill du skapa märkningar för färg 2 (Märkt=?)? - Svara Nej"
               VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för färg1/ färg2." UPDATE svar10 AS LOGICAL.      
               CASE svar10:
                  WHEN TRUE OR WHEN FALSE THEN DO:
                  END.
                  OTHERWISE RETURN.
               END CASE.
            END.                  
         END.
         ELSE assign svar10 = TRUE  svar12 = FALSE.          
          
          
         {muswait.i}                        
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den fil som motsvarar materielkatalog"         
         FILTERS        "Semikolon filer (*.skv;*.txt)"  "*.skv;*.txt"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         MUST-EXIST         
         USE-FILENAME
         UPDATE OKvald.
         IF OKvald = TRUE THEN DO:         
               EMPTY TEMP-TABLE felmex NO-ERROR. 
               RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar5,INPUT 0,OUTPUT TABLE felmex,INPUT svar10, INPUT svar12).                          
         END.             
         {musarrow.i}      
      
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = leverant.                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = leverant
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               RUN openfel_UI.       
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define SELF-NAME FBTN_IMPEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMPEX WINDOW-1
ON CHOOSE OF FBTN_IMPEX IN FRAME FRAME-FEXCELF /* Import Exel */
DO:      
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt)" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:
         
         IF Guru.Konstanter:varforetypval[38] NE 15 THEN DO:
            MESSAGE "Vill du skapa märkningar för färg 1 (Märkt=Ja)? - Svara Ja" SKIP
           "Vill du skapa märkningar för färg 2 (Märkt=?)? - Svara Nej"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för färg1/ färg2." UPDATE svar11 AS LOGICAL.      
            CASE svar11:
               WHEN TRUE OR WHEN FALSE THEN DO:
               END.
               OTHERWISE RETURN.
            END CASE.               
         END.
         ELSE svar11 = TRUE.   
         
         
         DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in som materielkatalog"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                
            {muswait.i}
            EMPTY TEMP-TABLE mkattmp NO-ERROR. 
            EMPTY TEMP-TABLE felex NO-ERROR. 
            FILL-IN-UPP = "Nu börjar inläsning från excel till temporär tabell  " + STRING(TIME,"HH:MM").  
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME FRAME-FEXCELF NO-LABELS.                           
            RUN MKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex,INPUT svar11 ).   
            FILL-IN-UPP = "Nu börjar inläsning från temporär tabell till databas  " + STRING(TIME,"HH:MM").  
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME FRAME-FEXCELF NO-LABELS.
            
            RUN mkatexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant, INPUT svar2 ).             
            FILL-IN-UPP = " " .  
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME FRAME-FEXCELF NO-LABELS.
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = leverant.                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = leverant
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".  
                              
               {AMERICANEUROPEAN.I}
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               {EUROPEANAMERICAN.I}
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Ny katalog för " + levtemp.LEVNAMN + " är nu inläst. För att visa den - använd sökfunktionen. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.

            /*{ENRHMT2.I}
            RUN initsok_UI (INPUT 2,INPUT "#").*/
            {musarrow.i}
         END.
      END.
      OTHERWISE RETURN.
   END CASE.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMPEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_IMPEX IN FRAME FRAME-FEXCELF /* Import Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME FBTN_IMPXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMPXML WINDOW-1
ON CHOOSE OF FBTN_IMPXML IN FRAME FRAME-XML /* Imp Hela xml */
DO: 
   hjkat = 1.   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den xml-fil med Övrigt materiel som ska läsas in i vald materielkatalog"
   FILTERS        "All Files (*.xml)"  "*.xml"   
   INITIAL-DIR    SESSION:TEMP-DIRECTORY
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      
      EMPTY TEMP-TABLE felmex NO-ERROR.       
      filnamnuppxml = filnamn.  
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
      CREATE tempnamn.        
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
         IF levtemp.LEVNAMN BEGINS "ahlsell"  THEN DO:
            EMPTY TEMP-TABLE Ahlsell_x0020_Hela NO-ERROR. 
            tempnamn.NAMNH = BUFFER Ahlsell_x0020_Hela:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Ahlsell_x0020_Hela:HANDLE.
         END.
         ELSE IF levtemp.LEVNAMN BEGINS "Onninen"  THEN DO:
            EMPTY TEMP-TABLE Onninen_x0020_Hela NO-ERROR. 
            tempnamn.NAMNH = BUFFER Onninen_x0020_Hela:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Onninen_x0020_Hela:HANDLE.
         END.
         ELSE DO:
            EMPTY TEMP-TABLE Ovrigt NO-ERROR. 
            tempnamn.NAMNH = BUFFER Ovrigt:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Ovrigt:HANDLE.
         END.
      END.
      ELSE DO:
         EMPTY TEMP-TABLE Ovrigt NO-ERROR. 
         tempnamn.NAMNH = BUFFER Ovrigt:HANDLE.       
         tempnamn.TORGTH = TEMP-TABLE Ovrigt:HANDLE.
      END.
      /*CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER Ovrigt:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE Ovrigt:HANDLE.*/
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      RUN intest_UI IN dynxml (INPUT TRUE).
      RUN linebort_UI IN dynxml (INPUT filnamnuppxml).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.            
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
         IF levtemp.LEVNAMN BEGINS "ahlsell"  THEN DO:
            FOR EACH Ahlsell_x0020_Hela WHERE Ahlsell_x0020_Hela.Enummer = "" :
               DELETE Ahlsell_x0020_Hela.
            END.
            RUN openbdynspec_UI IN brwproc[6].           
            BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = FALSE.
            BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
         END.
         ELSE IF levtemp.LEVNAMN BEGINS "onninen"  THEN DO:
            FOR EACH Onninen_x0020_Hela WHERE Onninen_x0020_Hela.Enummer = "" :
               DELETE Onninen_x0020_Hela.
            END.
            RUN openbdynspec_UI IN brwproc[7].    
            BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = FALSE.
         END.
         ELSE DO:
            FOR EACH Ovrigt WHERE Ovrigt.Enummer = "" :
               DELETE Ovrigt.
            END.
            RUN openbdynspec_UI IN brwproc[2].    
            BRW_OVR:HIDDEN IN FRAME FRAME-XML = FALSE.
            BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
         END.
      END.
      ELSE DO:
         FOR EACH Ovrigt WHERE Ovrigt.Enummer = "" :
            DELETE Ovrigt.
         END.
         RUN openbdynspec_UI IN brwproc[2].       
         BRW_OVR:HIDDEN IN FRAME FRAME-XML = FALSE.
         BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
         BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      END.      
      BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.      
      BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_NYK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IMXNEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMXNEX WINDOW-1
ON CHOOSE OF FBTN_IMXNEX IN FRAME FRAME-XML /* Imp Nexans xml */
DO: 
   hjkat = 3.   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den xml-fil med Nexans materiel som ska läsas in i vald materielkatalog"
   FILTERS        "All Files (*.xml)"  "*.xml"   
   INITIAL-DIR    SESSION:TEMP-DIRECTORY
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE felmex NO-ERROR. 
      EMPTY TEMP-TABLE Nexans NO-ERROR. 
      filnamnuppxml = filnamn.  
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER Nexans:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE Nexans:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      RUN intest_UI IN dynxml (INPUT TRUE).
      RUN linebort_UI IN dynxml (INPUT filnamnuppxml).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.      
      FOR EACH Nexans WHERE Nexans.Enummer = "" :
         DELETE Nexans.
      END.
      
      RUN openbdynspec_UI IN brwproc[4].           
      BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NEX:HIDDEN IN FRAME FRAME-XML = FALSE.
      BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_NYK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IMXNKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMXNKT WINDOW-1
ON CHOOSE OF FBTN_IMXNKT IN FRAME FRAME-XML /* Imp NKT xml */
DO: 
   hjkat = 4.   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den xml-fil med NKT materiel som ska läsas in i vald materielkatalog"
   FILTERS        "All Files (*.xml)"  "*.xml"   
   INITIAL-DIR    SESSION:TEMP-DIRECTORY
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE felmex NO-ERROR. 
      EMPTY TEMP-TABLE NKT NO-ERROR. 
      filnamnuppxml = filnamn.  
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER NKT:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE NKT:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      RUN intest_UI IN dynxml (INPUT TRUE).
      RUN linebort_UI IN dynxml (INPUT filnamnuppxml).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.      
      FOR EACH NKT WHERE NKT.Enummer = "" :
         DELETE NKT.
      END.
      
      RUN openbdynspec_UI IN brwproc[5].           
      BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NKT:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_NYK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define SELF-NAME FBTN_INK1EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK1EX WINDOW-1
ON CHOOSE OF FBTN_INK1EX IN FRAME FRAME-FEXCELF /* Imp Nexans Exel */
DO:      
   
   IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "elpa" THEN katnamn = "kablar".
   IF Guru.Konstanter:globforetag = "gran"  OR Guru.Konstanter:globforetag = "Celpa" THEN katnamn = "Nexans".
   MESSAGE "Vill du ta bort den gamla nettopriskatalogen " + katnamn + " och skapa ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
   "Vill du uppdatera/komplettera den befintliga nettopriskatalogen " + katnamn + "  för " + levtemp.LEVNAMN + " ? - Svara Nej"
   SKIP 
   SKIP
   "Om 'Ja' väljs raderas alla poster som är inlästa som " + katnamn + " sedan tidigare och sedan läses den nya filen in ."
   SKIP
   SKIP
   "Om 'Nej' väljs så uppdateras poster som sedan tidigare lästs in från " + katnamn + " vid träff, eventuellt nya artiklar läggs upp."
   SKIP
   "Gamla artiklar som inte finns i nya filen ligger kvar ej uppdaterade."   
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning nettopris " + katnamn + " katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:

         DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in som nettopriskatalog " + katnamn 
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                
            {muswait.i}
            EMPTY TEMP-TABLE mkattmp NO-ERROR. 
            EMPTY TEMP-TABLE felex NO-ERROR. 
            RUN MKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex ).   
            FOR EACH mkattmp:
               mkattmp.NETTO = TRUE.
            END.
            RUN nettokabovrexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant, INPUT svar2,INPUT 1 ). 
           
            IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:            
               IF levtemp.LEVNAMN = "nexans"  THEN DO:              
                  /*Mats J vill att bara stora lista skall uppdatera datum- inte nexan och nkt inne i ahsell, onninen,
                  däremot om nexans har en egen katalog*/
                  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
                  EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
                  CREATE inextradatatemp.          
                  ASSIGN
                  inextradatatemp.PROGRAM = "INLKAT"                   
                  inextradatatemp.HUVUDCH = leverant.                    
                                      
                  RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
                  FIND FIRST extradatatemp NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE extradatatemp THEN DO:
                     CREATE extradatatemp.
                     ASSIGN
                     extradatatemp.PROGRAM = "INLKAT"                   
                     extradatatemp.HUVUDCH = leverant
                     extradatatemp.SOKLOG[1] = FALSE.
                  END.
                  ASSIGN
                  extradatatemp.SOKDATE[1] = TODAY 
                  extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
                  extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
                  extradatatemp.SOKLOG[1] =  TRUE.
                  RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
                  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
                  EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
               END.
            END.
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               {AMERICANEUROPEAN.I}
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               {EUROPEANAMERICAN.I}
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " är nu inläst. För att visa den - använd Hämta  " + katnamn + "." VIEW-AS ALERT-BOX.

            
            {musarrow.i}
         END.
      END.
      OTHERWISE RETURN.
   END CASE.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK1EX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INK1EX IN FRAME FRAME-FEXCELF /* Imp Nexans Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INK2EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK2EX WINDOW-1
ON CHOOSE OF FBTN_INK2EX IN FRAME FRAME-FEXCELF /* Imp NKT Exel */
DO:      
   
   IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "elpa" THEN katnamn = "övrigt".
   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "Celpa" THEN katnamn = "NKT".
   MESSAGE "Vill du ta bort den gamla nettopriskatalogen " + katnamn + " och skapa ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga nettopriskatalogen " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Nej"
   SKIP 
   SKIP
   "Om 'Ja' väljs raderas alla poster som är inlästa som " + katnamn + " sedan tidigare och sedan läses den nya filen in ."
   SKIP
   SKIP
   "Om 'Nej' väljs så uppdateras poster som sedan tidigare lästs in från " + katnamn + " vid träff, eventuellt nya artiklar läggs upp."
   SKIP
   "Gamla artiklar som inte finns i nya filen ligger kvar ej uppdaterade."
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning nettopris " + katnamn + " katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:         

         DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in som nettopriskatalog " + katnamn 
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                
            {muswait.i}
            EMPTY TEMP-TABLE mkattmp NO-ERROR. 
            EMPTY TEMP-TABLE felex NO-ERROR. 
            RUN MKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex ).   
            FOR EACH mkattmp:
               mkattmp.NETTO = TRUE.
            END.
            IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:            
               IF levtemp.LEVNAMN = "NKT" THEN DO:              
                  /*Mats J vill att bara stora lista skall uppdatera datum- inte nexan och nkt inne i ahsell, onninen,
                  däremot om nkt har en egen katalog*/
                  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
                  EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
                  CREATE inextradatatemp.          
                  ASSIGN
                  inextradatatemp.PROGRAM = "INLKAT"                   
                  inextradatatemp.HUVUDCH = leverant.                    
                                      
                  RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
                  FIND FIRST extradatatemp NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE extradatatemp THEN DO:
                     CREATE extradatatemp.
                     ASSIGN
                     extradatatemp.PROGRAM = "INLKAT"                   
                     extradatatemp.HUVUDCH = leverant
                     extradatatemp.SOKLOG[1] = FALSE.
                  END.
                  ASSIGN
                  extradatatemp.SOKDATE[1] = TODAY 
                  extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
                  extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
                  extradatatemp.SOKLOG[1] =  TRUE.
                  RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
                  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
                  EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
               END.
            END.
            RUN nettokabovrexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant, INPUT svar2,INPUT 2 ). 
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               {AMERICANEUROPEAN.I}
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               {EUROPEANAMERICAN.I}
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " är nu inläst. För att visa den - använd Hämta  " + katnamn + "." VIEW-AS ALERT-BOX.
            {musarrow.i}
         END.
      END.
      OTHERWISE RETURN.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK2EX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INK2EX IN FRAME FRAME-FEXCELF /* Imp NKT Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SKV
&Scoped-define SELF-NAME FBTN_INKEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INKEX WINDOW-1
ON CHOOSE OF FBTN_INKEX IN FRAME FRAME-SKV /* Imp kabel -skv */
DO:      
   IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "KRIN" THEN katnamn = "kablar".
   IF Guru.Konstanter:globforetag = "Cgran" OR Guru.Konstanter:globforetag = "Celpa" THEN katnamn = "Nexans".   
   
   
   MESSAGE "Vill du ta bort den gamla nettopriskatalogen " + katnamn + " och skapa ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga nettopriskatalogen " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning nettopris " + katnamn + " katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:
         {muswait.i}                
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den fil som Ni vill läsa in som nettopriskatalog kablar"         
         FILTERS        "Semikolon filer (*.skv;*.txt)"  "*.skv;*.txt"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         MUST-EXIST         
         USE-FILENAME
         UPDATE OKvald.
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE felmex NO-ERROR. 
            RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar2,INPUT 1,OUTPUT TABLE felmex, INPUT TRUE , INPUT FALSE  ).                     
         END.             
         {musarrow.i}      
      
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = leverant.                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = leverant
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               {AMERICANEUROPEAN.I}
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               {EUROPEANAMERICAN.I}
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
      OTHERWISE RETURN.
   END CASE.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INKEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INKEX IN FRAME FRAME-SKV /* Imp kabel -skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INOEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INOEX WINDOW-1
ON CHOOSE OF FBTN_INOEX IN FRAME FRAME-SKV /* Imp övrigt -skv */
DO:      
   IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "KRIN" THEN katnamn = "övrigt".
   IF Guru.Konstanter:globforetag = "cgran" OR Guru.Konstanter:globforetag = "celpa" THEN katnamn = "NKT".
   
   MESSAGE "Vill du ta bort den gamla nettopriskatalogen " + katnamn + " och skapa ny nettopriskatalog " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga nettopriskatalogen " + katnamn + " för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning nettopris " + katnamn + " katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:         
         {muswait.i}       
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den fil som Ni vill läsa in som nettopriskatalog övrigt"         
         FILTERS        "Semikolon filer (*.skv;*.txt)"  "*.skv;*.txt"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         MUST-EXIST         
         USE-FILENAME
         UPDATE OKvald.
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE felmex NO-ERROR. 
            RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar2,INPUT 2,OUTPUT TABLE felmex, INPUT TRUE , INPUT FALSE ).                     
         END.             
         {musarrow.i}      
      
         IF OKvald = TRUE THEN DO:         
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = leverant.                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = leverant
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               {AMERICANEUROPEAN.I}
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               {EUROPEANAMERICAN.I}
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
      OTHERWISE RETURN.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INOEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INOEX IN FRAME FRAME-SKV /* Imp övrigt -skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define SELF-NAME FBTN_MARKEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKEX WINDOW-1
ON CHOOSE OF FBTN_MARKEX IN FRAME FRAME-FEXCELF /* Märkta Exel */
DO:           
   IF Guru.Konstanter:varforetypval[38] NE 15 THEN DO:
      MESSAGE "Vill du skapa märkningar för färg 1 (Märkt=Ja)? - Svara Ja" SKIP
     "Vill du skapa märkningar för färg 2 (Märkt=?)? - Svara Nej"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för färg1/ färg2." UPDATE svar8 AS LOGICAL.      
      CASE svar8:
         WHEN TRUE OR WHEN FALSE THEN DO:
         END.
         OTHERWISE RETURN.
      END CASE.               
   END.
   ELSE svar8 = TRUE.   
   MESSAGE "Vill du först ta bort gamla märkningar och sedan skapa nya märkningar för "  + levtemp.LEVNAMN + " katalog  ? - Svara Ja" SKIP
     "Vill du uppdatera/komplettera med märkningar för " + levtemp.LEVNAMN + " katalogen  ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för " + levtemp.LEVNAMN + ". Ordning: Enr" UPDATE svar3 AS LOGICAL.      
   CASE svar3:
      WHEN TRUE OR WHEN FALSE THEN DO:

         MESSAGE "Vill du uppdatera nettopris från excelfilen? I så fall måste ordning i excelfilen vara: Enr, nettopris." VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för " + levtemp.LEVNAMN + ". Ordning: Enr" UPDATE svar4 AS LOGICAL.      
         CASE svar4:
         WHEN TRUE OR WHEN FALSE THEN DO:               
            DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
            fildir = SESSION:TEMP-DIRECTORY.
            OS-CREATE-DIR VALUE(fildir) NO-ERROR.
            mappvar = fildir.
            SYSTEM-DIALOG GET-FILE fildir
            TITLE          "Välj den excelfil som har enummer som skall märkas i vald materielkatalog"
            FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
            INITIAL-DIR    mappvar
            UPDATE OKvald.      
            IF OKvald = TRUE THEN DO:                
               {muswait.i}
               EMPTY TEMP-TABLE mkattmp NO-ERROR. 
               RUN MMARKEXELIN.P (INPUT fildir, INPUT svar4 , INPUT svar8,  OUTPUT TABLE mkattmp ).               
               RUN mmarkexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant,INPUT svar3,INPUT svar4,INPUT svar8 ,OUTPUT TABLE felex ). 
               FIND FIRST felex NO-ERROR.
               IF AVAILABLE felex THEN DO:               
                  felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
                  {AMERICANEUROPEAN.I}
                  OUTPUT TO VALUE(felfil).               
                  PUT "Dessa enummer från excelfilen fick inte träff i katalogen. Var god kontrollera!" AT 6.         
                  PUT SKIP.         
                  FOR EACH felex:                  
                     PUT UNFORMATTED felex.ENR AT 6.  
                     PUT SKIP.
                  END.
                  OUTPUT CLOSE.
                  {EUROPEANAMERICAN.I}
                  RUN OPENDOC.P (felfil,"","",NO).         
               END.   
               MESSAGE "Märkningar för " + levtemp.LEVNAMN + " är nu inlästa. För att visa dem - använd sökfunktionen. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.
               /*{ENRHMT2.I}
               RUN initsok_UI (INPUT 2,INPUT "#").*/
               {musarrow.i}
            END.
         END.
         OTHERWISE RETURN.
         END CASE.
      END.
      OTHERWISE RETURN.
   END CASE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_MARKEX IN FRAME FRAME-FEXCELF /* Märkta Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SKV
&Scoped-define SELF-NAME FBTN_MARKSKV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKSKV WINDOW-1
ON CHOOSE OF FBTN_MARKSKV IN FRAME FRAME-SKV /* Märkta skv */
DO:           
      
   MESSAGE "Vill du först ta bort gamla märkningar och sedan skapa nya märkningar för "  + levtemp.LEVNAMN + " katalog  ? - Svara Ja" SKIP
     "Vill du uppdatera/komplettera med märkningar för " + levtemp.LEVNAMN + " katalogen  ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Märkning för " + levtemp.LEVNAMN + ". Ordning: Enr" UPDATE svar13 AS LOGICAL.      
   CASE svar13:
      WHEN TRUE OR WHEN FALSE THEN DO:   
         
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den skv som har enummer som skall märkas i vald materielkatalog"
         FILTERS        "All Files (*.skv)"  "*.skv"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                               
            {muswait.i}                                 
            RUN IMPMTRLSKVNM.P (INPUT filnamn, INPUT leverant,INPUT svar13).              
            {musarrow.i}            
         END.       
      END.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKSKV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_MARKSKV IN FRAME FRAME-SKV /* Märkta skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-XML
&Scoped-define SELF-NAME FBTN_MARKXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKXML WINDOW-1
ON CHOOSE OF FBTN_MARKXML IN FRAME FRAME-XML /* Märk Frekventa xml */
DO:      
   hjkat = 2.   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den xml-fil som innehåller Frekventa materiel som skall märkas i vald materielkatalog"
   FILTERS        "All Files (*.xml)"  "*.xml"   
   INITIAL-DIR    SESSION:TEMP-DIRECTORY
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      
      EMPTY TEMP-TABLE felmex NO-ERROR. 
      filnamnuppxml = filnamn.  
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
      CREATE tempnamn.        
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
         IF levtemp.LEVNAMN BEGINS "ahlsell"  THEN DO:
            EMPTY TEMP-TABLE Ahlsell_x0020_Frekventa NO-ERROR. 
            tempnamn.NAMNH = BUFFER Ahlsell_x0020_Frekventa:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Ahlsell_x0020_Frekventa:HANDLE.
         END.
         ELSE IF levtemp.LEVNAMN BEGINS "Onninen"  THEN DO:
            EMPTY TEMP-TABLE Onninen_x0020_Frekventa NO-ERROR. 
            tempnamn.NAMNH = BUFFER Onninen_x0020_Frekventa:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Onninen_x0020_Frekventa:HANDLE.
         END.
         ELSE DO:
            EMPTY TEMP-TABLE Frekventa NO-ERROR. 
            tempnamn.NAMNH = BUFFER Frekventa:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Frekventa:HANDLE.
         END.
      END.
      ELSE DO:
         EMPTY TEMP-TABLE Frekventa NO-ERROR. 
         tempnamn.NAMNH = BUFFER Frekventa:HANDLE.       
         tempnamn.TORGTH = TEMP-TABLE Frekventa:HANDLE.
      END.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      RUN intest_UI IN dynxml (INPUT TRUE).
      RUN linebort_UI IN dynxml (INPUT filnamnuppxml).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.      
      
      IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
         IF levtemp.LEVNAMN BEGINS "ahlsell"  THEN DO:
            FOR EACH Ahlsell_x0020_Frekventa WHERE Ahlsell_x0020_Frekventa.Enummer = "" :
               DELETE Ahlsell_x0020_Frekventa.
            END.
            
            RUN openbdynspec_UI IN brwproc[8].           
            BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_AFRE:HIDDEN IN FRAME FRAME-XML = FALSE.
            BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
         END.
         ELSE IF levtemp.LEVNAMN BEGINS "onninen"  THEN DO:
            FOR EACH Onninen_x0020_Frekventa WHERE Onninen_x0020_Frekventa.Enummer = "" :
               DELETE Onninen_x0020_Frekventa.
            END.
            RUN openbdynspec_UI IN brwproc[9].    
            BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_OFRE:HIDDEN IN FRAME FRAME-XML = FALSE.
         END.
         ELSE DO:
            FOR EACH Frekventa WHERE Frekventa.Enummer = "" :
               DELETE Frekventa.
            END.
            RUN openbdynspec_UI IN brwproc[3].    
            BRW_FRE:HIDDEN IN FRAME FRAME-XML = FALSE.
            BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
            BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
         END.
      END.
      ELSE DO:
         FOR EACH Frekventa WHERE Frekventa.Enummer = "" :
            DELETE Frekventa.
         END.
         RUN openbdynspec_UI IN brwproc[3].       
         BRW_FRE:HIDDEN IN FRAME FRAME-XML = FALSE.
         BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
         BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
      END.       
      BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
      BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
      BTN_MNXML:HIDDEN IN FRAME FRAME-XML = FALSE.
      BTN_MUXML:HIDDEN IN FRAME FRAME-XML = FALSE.
      {musarrow.i}
   END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKXML WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_MARKXML IN FRAME FRAME-XML /* Märk Frekventa xml */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SKV
&Scoped-define SELF-NAME FBTN_TUPPL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TUPPL WINDOW-1
ON CHOOSE OF FBTN_TUPPL IN FRAME FRAME-SKV /* Test skv mot upplägg */
DO: 
   
   {muswait.i}                        
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den fil som motsvarar materielkatalog"         
   FILTERS        "Semikolon filer (*.skv;*.txt)"  "*.skv;*.txt"
   INITIAL-DIR    SESSION:TEMP-DIRECTORY
   MUST-EXIST         
   USE-FILENAME
   UPDATE OKvald.
   IF OKvald = TRUE THEN DO:         
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
         EMPTY TEMP-TABLE felmex NO-ERROR.                
         {AVBGOM.I}
         RUN IMPMTRLTE.W (INPUT filnamn,INPUT 0,OUTPUT TABLE felmex,OUTPUT TABLE tidut).  
         {AVBFRAM.I}

      END.
      
   END.             
   {musarrow.i}      

   
   FIND FIRST felmex NO-ERROR.
   IF AVAILABLE felmex THEN DO:
      felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
      {AMERICANEUROPEAN.I}
      OUTPUT TO VALUE(felfil).
      PUT "Dessa rader kommer ej att gå att läsa in från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
      PUT SKIP.         
      FOR EACH felmex:           
         PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
         PUT SKIP.
      END.
      OUTPUT CLOSE.
      {EUROPEANAMERICAN.I}
      RUN OPENDOC.P (felfil,"","",NO).         
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KAT
&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-KAT /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME FRAME-KAT.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-KAT /* Benämning */
DO:
   RUN reado_UI (INPUT TRUE).
   {muswait.i}
   {BENHMT2.I}   
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}

   FIND FIRST mtrltemp NO-LOCK NO-ERROR.
   RUN reado_UI (INPUT FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-KAT /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME FRAME-KAT.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-KAT /* Enr */
DO:
   RUN reado_UI (INPUT TRUE).
   {muswait.i}   
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
   
   
   FIND FIRST mtrltemp NO-LOCK NO-ERROR.
   /*
   IF AVAILABLE mtrltemp THEN DO:
      RUN setlastrowid_UI IN brwh (INPUT ROWID(mtrltemp)).
      RUN lastselectdyn_UI IN brwh.
      /*APPLY "ENTRY" TO mtrltemp.ENR IN BROWSE BRW_LEVMTRL.      */
   END.
   */
   RUN reado_UI (INPUT FALSE).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME MBTN_ASCF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_ASCF WINDOW-1
ON CHOOSE OF MBTN_ASCF IN FRAME FRAME-GRUND /* asc-fil */
DO:
   RUN btnvit_UI (INPUT MBTN_ASCF:HANDLE).

   ASSIGN
   FRAME FRAME-FEXCELF:HIDDEN = TRUE
   FRAME FRAME-KAT:HIDDEN = TRUE 
   FRAME FRAME-SKV:HIDDEN = TRUE 
   FRAME FRAME-XML:HIDDEN = TRUE
   FRAME FRAME-ASC:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_EXCELF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_EXCELF WINDOW-1
ON CHOOSE OF MBTN_EXCELF IN FRAME FRAME-GRUND /* xcel-Format */
DO:
   RUN btnvit_UI (INPUT MBTN_EXCELF:HANDLE).
   ASSIGN
   FRAME FRAME-FEXCELF:HIDDEN = FALSE
   FRAME FRAME-KAT:HIDDEN = TRUE 
   FRAME FRAME-SKV:HIDDEN = TRUE 
   FRAME FRAME-XML:HIDDEN = TRUE
   FRAME FRAME-ASC:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KAT WINDOW-1
ON CHOOSE OF MBTN_KAT IN FRAME FRAME-GRUND /* Katalog */
DO:
   RUN btnvit_UI (INPUT MBTN_KAT:HANDLE).

   ASSIGN
   FRAME FRAME-FEXCELF:HIDDEN = TRUE
   FRAME FRAME-KAT:HIDDEN = FALSE  
   FRAME FRAME-SKV:HIDDEN = TRUE 
   FRAME FRAME-XML:HIDDEN = TRUE
   FRAME FRAME-ASC:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_SKVF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_SKVF WINDOW-1
ON CHOOSE OF MBTN_SKVF IN FRAME FRAME-GRUND /* Skv-Format */
DO:
   RUN btnvit_UI (INPUT MBTN_SKVF:HANDLE).

   ASSIGN
   FRAME FRAME-FEXCELF:HIDDEN = TRUE
   FRAME FRAME-KAT:HIDDEN = TRUE 
   FRAME FRAME-SKV:HIDDEN = FALSE 
   FRAME FRAME-XML:HIDDEN = TRUE
   FRAME FRAME-ASC:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_XMLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_XMLF WINDOW-1
ON CHOOSE OF MBTN_XMLF IN FRAME FRAME-GRUND /* Xml-Format */
DO:
   RUN btnvit_UI (INPUT MBTN_XMLF:HANDLE).

   ASSIGN
   FRAME FRAME-FEXCELF:HIDDEN = TRUE
   FRAME FRAME-KAT:HIDDEN = TRUE 
   FRAME FRAME-SKV:HIDDEN = TRUE 
   FRAME FRAME-XML:HIDDEN = FALSE
   FRAME FRAME-ASC:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-FEXCELF
&Scoped-define BROWSE-NAME BRW_AFRE
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   IF VALID-HANDLE(brwh) THEN DELETE PROCEDURE brwh.   
   /*
   {BORTBRWPROC.I}
   */
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
   &Scoped-define FRAME-NAME FRAME-GRUND
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}   
   
   FBTN_IMPEX:LABEL = "Import Excel".
   FBTN_EXP:LABEL = "Export Excel".
   FBTN_MARKEX:LABEL = "Märkta Excel".
   bortfraga = FALSE.
      
   FIND FIRST levtemp WHERE levtemp.LEVKOD = leverant NO-LOCK NO-ERROR.                                                          
   ASSIGN WINDOW-1:TITLE = "Uppdatering av katalog för leverantör " + levtemp.LEVNAMN.      
   RUN setorgtitle_UI IN brwh (INPUT "Materiellista huvudleverantör: " + levtemp.LEVNAMN + ". " + "Varje post sparas när du lämnar raden.").         
   RUN enable_UI.   
   
   
   IF levtemp.LEVNAMN BEGINS "Elektroskandia" THEN DO:
      MBTN_ASCF:HIDDEN = FALSE.
      FRAME FRAME-ASC:HIDDEN = FALSE.
   END.   
   ELSE DO:
      MBTN_ASCF:HIDDEN = TRUE.
      FRAME FRAME-ASC:HIDDEN = TRUE.
   END.   
   
   
   {FRMSIZEF.I}        
   IF leverant = "99" THEN DO:
      ASSIGN
      BTN_TEXT:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      BTN_TEXT:HIDDEN = TRUE.      
   END.
   ASSIGN
   hjkat = 0   
   FBTN_EXP:HIDDEN IN FRAME FRAME-FEXCELF = FALSE   
   FBTN_EXPC:HIDDEN IN FRAME FRAME-XML = FALSE
   FBTN_EXMA:HIDDEN IN FRAME FRAME-FEXCELF = FALSE
   FBTN_IMP:HIDDEN IN FRAME FRAME-SKV = TRUE
   FBTN_INKEX:HIDDEN IN FRAME FRAME-SKV = TRUE
   FBTN_INOEX:HIDDEN IN FRAME FRAME-SKV = TRUE
   FBTN_INK1EX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE
   FBTN_INK2EX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE
   FBTN_IMPXML:HIDDEN IN FRAME FRAME-XML = TRUE
   FBTN_MARKXML:HIDDEN IN FRAME FRAME-XML = TRUE
   FBTN_IMXNEX:HIDDEN IN FRAME FRAME-XML = TRUE
   FBTN_IMXNKT:HIDDEN IN FRAME FRAME-XML = TRUE
   BTN_KAB1:HIDDEN IN FRAME FRAME-KAT = TRUE
   BTN_KAB2:HIDDEN IN FRAME FRAME-KAT = TRUE.
   FBTN_TUPPL:HIDDEN IN FRAME FRAME-SKV = TRUE.
   BRW_FRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OFRE:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_OVR:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_AHHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_ONHELA:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NEX:HIDDEN IN FRAME FRAME-XML = TRUE.
   BRW_NKT:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_NYK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_UPPDK:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MNXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   BTN_MUXML:HIDDEN IN FRAME FRAME-XML = TRUE.
   /*lägg in för alla 20140226 lena*/
   FBTN_IMP:HIDDEN IN FRAME FRAME-SKV = FALSE.
   
   IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:      
      ASSIGN
      FBTN_IMP:LABEL IN FRAME FRAME-SKV = "Import -skv"
      FBTN_INKEX:LABEL IN FRAME FRAME-SKV = "Imp kabel -skv"
      FBTN_INOEX:LABEL IN FRAME FRAME-SKV = "Imp övrigt -skv"      
      BTN_KAB1:LABEL IN FRAME FRAME-KAT = "Hämta kabel"      
      BTN_KAB2:LABEL IN FRAME FRAME-KAT = "Hämta övrigt"      
      FBTN_MARKEX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE      
      FBTN_IMP:HIDDEN IN FRAME FRAME-SKV = FALSE
      FBTN_INKEX:HIDDEN IN FRAME FRAME-SKV = FALSE
      FBTN_INOEX:HIDDEN IN FRAME FRAME-SKV = FALSE
      BTN_KAB1:HIDDEN IN FRAME FRAME-KAT = FALSE.
      BTN_KAB2:HIDDEN IN FRAME FRAME-KAT = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "KRIN"  THEN DO:      
      ASSIGN
      FBTN_IMP:LABEL IN FRAME FRAME-SKV = "Import -skv"
      FBTN_INKEX:LABEL IN FRAME FRAME-SKV = "Imp kabel -skv"
      FBTN_INOEX:LABEL IN FRAME FRAME-SKV = "Imp övrigt -skv"      
      BTN_KAB1:LABEL IN FRAME FRAME-KAT = "Hämta kabel"      
      BTN_KAB2:LABEL IN FRAME FRAME-KAT = "Hämta övrigt"            
      FBTN_IMP:HIDDEN IN FRAME FRAME-SKV = FALSE
      FBTN_INKEX:HIDDEN IN FRAME FRAME-SKV = FALSE
      FBTN_INOEX:HIDDEN IN FRAME FRAME-SKV = FALSE
      BTN_KAB1:HIDDEN IN FRAME FRAME-KAT = FALSE.
      BTN_KAB2:HIDDEN IN FRAME FRAME-KAT = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      ASSIGN
      FBTN_INK1EX:LABEL IN FRAME FRAME-FEXCELF = "Import Nexans excel"
      FBTN_INK2EX:LABEL IN FRAME FRAME-FEXCELF = "Import NKT excel"      
      FBTN_MARKEX:HIDDEN IN FRAME FRAME-FEXCELF = FALSE
      FBTN_IMPEX:HIDDEN IN FRAME FRAME-FEXCELF = FALSE
      FBTN_IMP:HIDDEN IN FRAME FRAME-SKV = FALSE 
      FBTN_INK1EX:HIDDEN IN FRAME FRAME-FEXCELF = FALSE
      FBTN_INK2EX:HIDDEN IN FRAME FRAME-FEXCELF = FALSE.      
      BTN_KAB1:HIDDEN IN FRAME FRAME-KAT = FALSE.
      BTN_KAB2:HIDDEN IN FRAME FRAME-KAT = FALSE.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
         IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN FBTN_TUPPL:HIDDEN IN FRAME FRAME-SKV = FALSE.
         ASSIGN
         FBTN_IMPXML:HIDDEN IN FRAME FRAME-XML = FALSE
         FBTN_MARKXML:HIDDEN IN FRAME FRAME-XML = FALSE
         FBTN_IMXNEX:HIDDEN IN FRAME FRAME-XML = FALSE
         FBTN_IMXNKT:HIDDEN IN FRAME FRAME-XML = FALSE.
      END.
      IF levtemp.LEVNAMN = "NKT" THEN DO:         
         /*så att det bara går att importera nkt för leverantör nkt*/
         ASSIGN
         FBTN_IMPEX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE         
         FBTN_INK1EX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE
         FBTN_IMPXML:HIDDEN IN FRAME FRAME-XML = TRUE
         FBTN_IMXNEX:HIDDEN IN FRAME FRAME-XML = TRUE.         
         BTN_KAB1:HIDDEN IN FRAME FRAME-KAT = TRUE.
      END.
      IF levtemp.LEVNAMN = "nexans" THEN DO:
         /*så att det bara går att importera nexans för leverantör nexans*/
         ASSIGN
         FBTN_IMPEX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE         
         FBTN_INK2EX:HIDDEN IN FRAME FRAME-FEXCELF = TRUE         
         FBTN_IMPXML:HIDDEN IN FRAME FRAME-XML = TRUE
         FBTN_IMXNKT:HIDDEN IN FRAME FRAME-XML = TRUE.
         BTN_KAB2:HIDDEN IN FRAME FRAME-KAT = TRUE.         
      END.
   END.
   MBTN_KAT:LOAD-IMAGE("BILDER\xbtn_katalog.gif") NO-ERROR.
   MBTN_EXCELF:LOAD-IMAGE("BILDER\xbtn_xl.gif") NO-ERROR.
   MBTN_SKVF:LOAD-IMAGE("BILDER\xbtn_skv.gif") NO-ERROR.
   MBTN_XMLF:LOAD-IMAGE("BILDER\xbtn_xml.gif") NO-ERROR.
   MBTN_ASCF:LOAD-IMAGE("BILDER\xbtn_asc.gif") NO-ERROR.
   
   RUN buttplace_UI.
   ON 'VALUE-CHANGED' OF FRAME FRAME-GRUND PERSISTENT RUN buttplace_UI IN THIS-PROCEDURE.
   RUN setapplyvc IN framesizeh (INPUT FRAME FRAME-GRUND:HANDLE ).

   /*FRAME-KAT*/
   Guru.GlobalaVariabler:collefth = ?.     
   
   Guru.GlobalaVariabler:colrighth = BTN_HMTRLF:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_MTRLF:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   /*FRAME-FEXCELF*/
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_IMPEX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMPEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_MARKEX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
    
   IF FBTN_EXP:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXP:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END. 
   IF FBTN_EXMA:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXMA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.  
 
   IF FBTN_INK1EX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INK1EX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END. 
   IF FBTN_INK2EX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INK2EX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END. 
   
   /*FRAME-SKV*/
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_IMP:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMP:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_MARKSKV:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKSKV:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INKEX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INKEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INOEX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INOEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_BENUPPDAT:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_BENUPPDAT:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   
   IF FBTN_TUPPL:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_TUPPL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   
   /*FRAME-XML*/
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_IMPXML:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMPXML:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_MARKXML:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKXML:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.  
   IF FBTN_IMXNEX:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMXNEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_IMXNKT:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMXNKT:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_EXPC:HIDDEN = NO THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXPC:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   
   mtrltemp.ENR:LABEL IN BROWSE BRW_LEVMTRL = Guru.Konstanter:genk.
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.  
   APPLY "CHOOSE" TO MBTN_KAT.
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
   RUN DYNBRW.P PERSISTENT SET brwh (INPUT BRW_LEVMTRL:HANDLE IN FRAME FRAME-KAT). 
   RUN StartSokEnrLev_UI IN brwh (INPUT THIS-PROCEDURE).      
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_OVR:HANDLE IN FRAME FRAME-XML). 
   /*RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).*/      
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_FRE:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[3] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_NEX:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[4] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_NKT:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[5] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_AHHELA:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[6] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[7] (INPUT BRW_ONHELA:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[7] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[8] (INPUT BRW_AFRE:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[8] (INPUT THIS-PROCEDURE).*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[9] (INPUT BRW_OFRE:HANDLE IN FRAME FRAME-XML).
   /*RUN StartSokEnrLev_UI IN brwproc[9] (INPUT THIS-PROCEDURE).*/       
   RUN setdefaultcol_UI IN brwh (INPUT 1).
   RUN setcolsortvar_UI IN brwh (INPUT " WHERE mtrltemp.KALKNR = " + STRING(0) + " AND mtrltemp.LEVKOD = '" + STRING(leverant) + "'"). 
   RUN setcolindex_UI IN brwh (INPUT "ENR").
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN IMPMXMLAPP.P PERSISTENT SET mtrlxapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
      RUN IMPMXMLAPP.P PERSISTENT SET mtrlxapph. 
   END.      
   {UTXKMLKOLL.I}
   RUN VALUE(vxmlpro) PERSISTENT SET dynxml (INPUT TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autofavo_UI WINDOW-1 
PROCEDURE autofavo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*CCC*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   DEFINE VARIABLE antanurval AS INTEGER NO-UNDO.
   ASSIGN
   antal_valda = BRW_LEVMTRL:NUM-SELECTED-ROWS IN FRAME FRAME-KAT NO-ERROR. 
   antal_raknare = 1.
   RUN getnumresult_UI IN brwh (OUTPUT antanurval).
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
   EMPTY TEMP-TABLE emtrltemp.
  
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_LEVMTRL:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
      mtrl_rowid = ROWID(mtrltemp).  
      IF bortfraga = FALSE THEN DO:
         MESSAGE "Vill du ta bort mtrl - " + mtrltemp.ENR + " " + mtrltemp.BENAMNING VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
      END.
      ELSE DO:
         svar = TRUE.
      END.      
      IF svar THEN DO:             
         CREATE emtrltemp.
         BUFFER-COPY mtrltemp TO emtrltemp.
         sprow = ROWID(mtrltemp). 
         DELETE mtrltemp.                                  
         IF antal_raknare = antal_valda THEN DO:
            IF antanurval = antal_valda THEN.
            ELSE DO:              
               RUN selnextprevrow_UI IN brwh.               
            END.
         END.  
         svar = FALSE.        
      END.           
      antal_raknare = antal_raknare + 1.   
   END. 
   FIND FIRST emtrltemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE emtrltemp THEN DO:
      RUN admbortmtrl_UI IN mtrlbapph (INPUT TABLE emtrltemp,OUTPUT TABLE felmeddtemp ).       
   END.
   FOR EACH felmeddtemp: 
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.                        
   END.
   EMPTY TEMP-TABLE emtrltemp.  
   RUN openbdynspec_UI IN brwh.
   RUN lastselectdyn_UI IN brwh.            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI WINDOW-1 
PROCEDURE btnvit_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER btnh AS HANDLE NO-UNDO.
   DEFINE VARIABLE btnlabel AS CHARACTER NO-UNDO.
   IF btnnovit NE ? THEN DO:
      btnlabel = btnnovit:IMAGE.
      btnlabel = REPLACE(btnlabel,"_vit.gif",".gif"). 
      btnnovit:LOAD-IMAGE (btnlabel) NO-ERROR.
   END.
   btnlabel = btnh:IMAGE.
   btnlabel = REPLACE(btnlabel,".gif","_vit.gif"). 
   btnh:LOAD-IMAGE (btnlabel) NO-ERROR.
   btnnovit = btnh.   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttplace_UI WINDOW-1 
PROCEDURE buttplace_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:collefth = MBTN_KAT:HANDLE IN FRAME FRAME-GRUND. 
   Guru.GlobalaVariabler:colrighth = MBTN_EXCELF:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   Guru.GlobalaVariabler:colrighth = MBTN_SKVF:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   Guru.GlobalaVariabler:colrighth = MBTN_XMLF:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = MBTN_ASCF:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
  ENABLE MBTN_KAT MBTN_EXCELF MBTN_SKVF MBTN_XMLF MBTN_ASCF BTN_AVB 
      WITH FRAME FRAME-GRUND IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-GRUND}
  ENABLE FBTN_ASC 
      WITH FRAME FRAME-ASC IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ASC}
  ENABLE FBTN_INK2EX FBTN_INK1EX FBTN_IMPEX FBTN_EXP FBTN_EXMA FBTN_MARKEX 
      WITH FRAME FRAME-FEXCELF IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FEXCELF}
  DISPLAY FILL-IN-ENR FILL-IN-BEN RAD_SOK 
      WITH FRAME FRAME-KAT IN WINDOW WINDOW-1.
  ENABLE RECT_STOR BRW_LEVMTRL BTN_MTRLF BTN_HMTRLF BTN_NY BTN_BORT 
         BTN_BORTRENS BTN_KAB1 BTN_KAB2 BTN_TEXT FILL-IN-ENR FILL-IN-BEN 
         RAD_SOK 
      WITH FRAME FRAME-KAT IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KAT}
  ENABLE FBTN_IMP FBTN_MARKSKV FBTN_INKEX FBTN_INOEX FBTN_BENUPPDAT FBTN_TUPPL 
      WITH FRAME FRAME-SKV IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SKV}
  ENABLE BRW_NKT BRW_NEX BRW_FRE BRW_AHHELA BRW_AFRE BRW_OFRE BRW_ONHELA 
         BRW_OVR FBTN_IMPXML FBTN_MARKXML FBTN_IMXNEX FBTN_IMXNKT FBTN_EXPC 
         BTN_MNXML BTN_MUXML BTN_UPPDK BTN_NYK 
      WITH FRAME FRAME-XML IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-XML}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtfavoriter_UI WINDOW-1 
PROCEDURE hmtfavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.  
   tthandle = TEMP-TABLE mtrltemp:HANDLE.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.
   RUN ttcopy_UI (INPUT TABLE-HANDLE tthandle).
   ttbuffh:EMPTY-TEMP-TABLE() NO-ERROR.
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "MTRLPRIS" +  STRING(leverant) /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "MTRL" /*Tabellnamn*/
   sparaladdatemp.FALTVALAO = "ENR" /*CHARACTER field*/
   sparaladdatemp.FALTVALDEL = "LEVKOD" /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 

   RUN laddabrw_UI IN brwh (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp). 
  /* RUN aokomp_UI (INPUT 1).*/
   RUN ttjmf_UI (INPUT-OUTPUT ttbuffh).
   RUN openbdyn_UI IN brwh (INPUT ""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_LEVMTRL" THEN valbrw = 1.  
   
   status-ok = BRW_LEVMTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-KAT  NO-ERROR.
   IF status-ok THEN DO:
      valenr = mtrltemp.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
      NO-LOCK NO-ERROR.      
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(leverant) + "' USE-INDEX LEV ".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwh (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(leverant) + "' USE-INDEX LEV ".
     tth = TEMP-TABLE mtrltemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwh (INPUT TABLE valsoktemp).  
  END.
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
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openfel_UI WINDOW-1 
PROCEDURE openfel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {AMERICANEUROPEAN.I}
   OUTPUT TO VALUE(felfil).               
   PUT "Dessa enummer från xmlfilen fick inte träff i katalogen. Var god kontrollera!" AT 6.         
   PUT SKIP.         
   FOR EACH felex:                  
      PUT UNFORMATTED felex.ENR AT 6.  
      PUT SKIP.
   END.
   OUTPUT CLOSE.
   RUN OPENDOC.P (felfil,"","",NO).
   {EUROPEANAMERICAN.I}
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reado_UI WINDOW-1 
PROCEDURE reado_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER reado AS LOGICAL NO-UNDO.
   ASSIGN
   mtrltemp.ENR:READ-ONLY IN BROWSE BRW_LEVMTRL = reado
   mtrltemp.ENR:READ-ONLY IN BROWSE BRW_LEVMTRL = reado       
   mtrltemp.BENAMNING:READ-ONLY IN BROWSE BRW_LEVMTRL = reado
   mtrltemp.ENHET:READ-ONLY IN BROWSE BRW_LEVMTRL = reado 
   mtrltemp.BPRIS:READ-ONLY IN BROWSE BRW_LEVMTRL = reado 
   mtrltemp.NPRIS:READ-ONLY IN BROWSE BRW_LEVMTRL = reado
   mtrltemp.KUND:READ-ONLY IN BROWSE BRW_LEVMTRL = reado.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparaaofavoriter_UI WINDOW-1 
PROCEDURE sparaaofavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   tthandle = TEMP-TABLE mtrltemp:HANDLE.
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "MTRLPRIS" + STRING(leverant) /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "MTRL" /*Tabellnamn*/
   sparaladdatemp.FALTVALAO = "ENR" /*CHARACTER field*/
   sparaladdatemp.FALTVALDEL = "LEVKOD" /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/
   RUN sparabrw_UI IN brwh  (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttcopy_UI WINDOW-1 
PROCEDURE ttcopy_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE-HANDLE tthandle.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.  
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(ttbuffh).

   ttcopyh:TEMP-TABLE-PREPARE("ttkopia"). 
   ttbuffcopyh = ttcopyh:DEFAULT-BUFFER-HANDLE.

   komcop = "FOR EACH " + ttbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttqh.
   ttqh:SET-BUFFERS(ttbuffh).
   ttqh:QUERY-PREPARE(komcop).
   ttqh:QUERY-OPEN().
   ttqh:GET-FIRST(NO-LOCK).
   DO WHILE ttqh:QUERY-OFF-END = FALSE:
      ttbuffcopyh:BUFFER-CREATE().
      ttbuffcopyh:BUFFER-COPY(ttbuffh).
      ttqh:GET-NEXT(NO-LOCK).
   END.
   ttqh:QUERY-CLOSE.
   DELETE OBJECT ttqh NO-ERROR.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttjmf_UI WINDOW-1 
PROCEDURE ttjmf_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER favbuffh AS HANDLE.
   DEFINE VARIABLE compsave AS LOGICAL NO-UNDO.
   DEFINE VARIABLE komfav AS CHARACTER NO-UNDO.
   DEFINE VARIABLE komcop AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ttcopqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE ttfavqh AS HANDLE NO-UNDO.
   komcop = "FOR EACH " + ttbuffcopyh:TABLE + " NO-LOCK.".
   komfav = "FOR EACH " + favbuffh:TABLE + " NO-LOCK.".
   CREATE QUERY ttcopqh.
   ttcopqh:SET-BUFFERS(ttbuffcopyh).
   ttcopqh:QUERY-PREPARE(komcop).
   ttcopqh:QUERY-OPEN().
   CREATE QUERY ttfavqh.
   ttfavqh:SET-BUFFERS(favbuffh).
   ttfavqh:QUERY-PREPARE(komfav).
   ttfavqh:QUERY-OPEN().
   ttcopqh:GET-FIRST(NO-LOCK).
   DO WHILE ttcopqh:QUERY-OFF-END = FALSE:
      ttfavqh:GET-FIRST(NO-LOCK).
      compsave = FALSE.
      DO WHILE ttfavqh:QUERY-OFF-END = FALSE:
         IF favbuffh:BUFFER-COMPARE(ttbuffcopyh) THEN DO:
            compsave = TRUE.       
            ttfavqh:GET-LAST(NO-LOCK).
            ttfavqh:GET-NEXT(NO-LOCK).
         END.         
         ttfavqh:GET-NEXT(NO-LOCK).
      END.                  
      IF compsave = TRUE THEN DO:
         ttbuffcopyh:BUFFER-DELETE().  
      END.
      ELSE DO:
         favbuffh:BUFFER-CREATE().
         favbuffh:BUFFER-COPY(ttbuffcopyh).
      END.
      ttcopqh:GET-NEXT(NO-LOCK).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF AVAILABLE mtrltemp THEN DO:
      DISPLAY
      mtrltemp.ENR       
      mtrltemp.BENAMNING
      mtrltemp.ENHET 
      mtrltemp.BPRIS 
      mtrltemp.NPRIS 
      WITH BROWSE BRW_LEVMTRL.         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

