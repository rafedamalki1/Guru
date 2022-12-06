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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fastighbet AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardetidvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER akerkabrec2 AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER volskogtabrec AS RECID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER stpnr AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER bestandnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{SOKDEF.I}
/*{EGENBEN.I}*/
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
{MARKVARD.I}
{VOLUPPSK.I}
DEFINE BUFFER revbuff FOR revtemp.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE  VARIABLE vardrec AS RECID NO-UNDO.
DEFINE  VARIABLE skogrec AS RECID NO-UNDO.
DEFINE  VARIABLE fastvrec AS RECID NO-UNDO.
DEFINE  VARIABLE kabrec AS RECID NO-UNDO.
DEFINE  VARIABLE volrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE ffs AS INTEGER NO-UNDO.

/* DEFINE VARIABLE bestandnr AS INTEGER NO-UNDO. */

DEFINE VARIABLE ersatt AS INTEGER NO-UNDO.
DEFINE VARIABLE lplus AS INTEGER NO-UNDO.
DEFINE VARIABLE ivvarde AS INTEGER NO-UNDO.
DEFINE VARIABLE bplus AS INTEGER NO-UNDO.
DEFINE VARIABLE eh AS INTEGER NO-UNDO.
DEFINE VARIABLE yh AS INTEGER NO-UNDO.
DEFINE VARIABLE yf AS INTEGER NO-UNDO.
DEFINE VARIABLE yk AS INTEGER NO-UNDO.
DEFINE VARIABLE ykoll AS INTEGER NO-UNDO.
DEFINE VARIABLE ymin AS INTEGER NO-UNDO.
DEFINE VARIABLE imon AS INTEGER NO-UNDO.
DEFINE VARIABLE iyr AS INTEGER NO-UNDO.
DEFINE VARIABLE btillagg AS INTEGER NO-UNDO.
DEFINE VARIABLE ltillagg AS INTEGER NO-UNDO.
DEFINE VARIABLE eb AS INTEGER NO-UNDO.
DEFINE VARIABLE emin AS INTEGER NO-UNDO.
DEFINE VARIABLE omravd AS INTEGER NO-UNDO.
DEFINE VARIABLE aovar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE ortvar AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fkommun AS CHARACTER NO-UNDO.   
DEFINE VARIABLE fvaker AS CHARACTER NO-UNDO.   
DEFINE VARIABLE hjarst AS INTEGER NO-UNDO.
DEFINE VARIABLE hjar AS INTEGER NO-UNDO.
DEFINE VARIABLE indexup AS DECIMAL NO-UNDO.
DEFINE VARIABLE paslagfaktor AS DECIMAL NO-UNDO.
DEFINE VARIABLE skogkrha AS INTEGER NO-UNDO.
DEFINE VARIABLE kskapkr AS INTEGER NO-UNDO.
DEFINE VARIABLE nstnakerkr AS INTEGER NO-UNDO.
DEFINE VARIABLE nstnovrigkr AS INTEGER NO-UNDO.
DEFINE VARIABLE andvardspecapph AS HANDLE NO-UNDO.
DEFINE VARIABLE skprotapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KOMKAB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kabkomtemp stnksktemp volskogstamp

/* Definitions for BROWSE BRW_KOMKAB                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMKAB kabkomtemp.kabkom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMKAB 
&Scoped-define QUERY-STRING-BRW_KOMKAB FOR EACH kabkomtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOMKAB OPEN QUERY BRW_KOMKAB FOR EACH kabkomtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOMKAB kabkomtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMKAB kabkomtemp


/* Definitions for BROWSE BRW_KOMTR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMTR kabkomtemp.kabkom 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMTR 
&Scoped-define QUERY-STRING-BRW_KOMTR FOR EACH kabkomtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOMTR OPEN QUERY BRW_KOMTR FOR EACH kabkomtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOMTR kabkomtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMTR kabkomtemp


/* Definitions for BROWSE BRW_KSKSTN                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_KSKSTN stnksktemp.PRIS ~
stnksktemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KSKSTN 
&Scoped-define QUERY-STRING-BRW_KSKSTN FOR EACH stnksktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KSKSTN OPEN QUERY BRW_KSKSTN FOR EACH stnksktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KSKSTN stnksktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KSKSTN stnksktemp


/* Definitions for BROWSE BRW_STAMP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_STAMP volskogstamp.DIAM ~
volskogstamp.TANT volskogstamp.STTM3 volskogstamp.TM3 volskogstamp.GANT ~
volskogstamp.STGM3 volskogstamp.GM3 volskogstamp.BANT volskogstamp.STBM3 ~
volskogstamp.BM3 volskogstamp.OANT volskogstamp.STOM3 volskogstamp.OM3 ~
volskogstamp.OKORR volskogstamp.SUMMAANT volskogstamp.SUMMAM3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STAMP volskogstamp.TANT ~
volskogstamp.GANT volskogstamp.BANT volskogstamp.OANT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STAMP volskogstamp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STAMP volskogstamp
&Scoped-define QUERY-STRING-BRW_STAMP FOR EACH volskogstamp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_STAMP OPEN QUERY BRW_STAMP FOR EACH volskogstamp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_STAMP volskogstamp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STAMP volskogstamp


/* Definitions for FRAME FRAME-ANDAKAB                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-ANDAKAB ~
    ~{&OPEN-QUERY-BRW_KOMKAB}

/* Definitions for FRAME FRAME-ANDTRAFO                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-ANDTRAFO ~
    ~{&OPEN-QUERY-BRW_KOMTR}~
    ~{&OPEN-QUERY-BRW_KSKSTN}

/* Definitions for FRAME FRAME-ANDVOL                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-ANDVOL ~
    ~{&OPEN-QUERY-BRW_STAMP}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_REG BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(60)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1.

DEFINE VARIABLE FILL-IN_BETECKNING AS CHARACTER FORMAT "X(30)" 
     LABEL "Fastighetsbeteckning" 
     VIEW-AS FILL-IN 
     SIZE 30.38 BY 1.

DEFINE VARIABLE FILL-IN_FASTPRIS AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Fast pris" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FLKAB AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Flera kablar" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_KRONOR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Kronor" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_LAKAB1 AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Längd i meter" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE FILL-IN_LAKAB2 AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "Bredd" 
     VIEW-AS FILL-IN 
     SIZE 6.88 BY 1.

DEFINE VARIABLE TOG_SAM AS LOGICAL INITIAL no 
     LABEL "Samförläggning hsp lsp" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.5 BY .79 NO-UNDO.

DEFINE VARIABLE CMB_SORT AS CHARACTER FORMAT "X(10)":U 
     LABEL "Å/B/I" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Åker","Betesmark","Impediment" 
     DROP-DOWN-LIST
     SIZE 13.63 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-VARNING AS CHARACTER FORMAT "X(256)":U INITIAL "OBS! Om VP Vattenfall används- använd Guru's summor istället." 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN_BORTINT AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Borttaget intrång" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LAKER1 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Längd L1" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LAKER2 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Bredd L2" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LAKER3 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Närmaste fältkant L3" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LAKER4 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Näst närmaste fältkant L4" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LAKER5 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Utstr längs kant L5" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_STOLPNR AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Stolpnr" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER/aker2.gif":U
     SIZE 27.13 BY 10.75.

DEFINE VARIABLE CMB_BARRLOV AS CHARACTER FORMAT "X(4)":U INITIAL "10.0" 
     LABEL "Barr-Löv" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "10.0","9.1","8.2","7.3","6.4","5.5","4.6","3.7","2.8","1.9","0.10" 
     DROP-DOWN-LIST
     SIZE 8.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_BARRLOV-2 AS CHARACTER FORMAT "X(4)":U INITIAL "10.0" 
     LABEL "Barr-Löv" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "10.0","9.1","8.2","7.3","6.4","5.5","4.6","3.7","2.8","1.9","0.10" 
     DROP-DOWN-LIST
     SIZE 8.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_BESTTYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Besttyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 = plantskog","2 = röjningsskog","3 = massavedsskog","4 = yngre timmerskog","5a = äldre timmerskog","5b = kalmark" 
     DROP-DOWN-LIST
     SIZE 24.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_BESTTYP-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Besttyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 = plantskog","2 = röjningsskog","3 = massavedsskog","4 = yngre timmerskog","5a = äldre timmerskog","5b = kalmark" 
     DROP-DOWN-LIST
     SIZE 24.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_MARKTYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marktyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "I   = mycket god","II  = medelgod","III = dålig","Imp = impediment" 
     DROP-DOWN-LIST
     SIZE 24.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_MARKTYP-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marktyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "I   = mycket god","II  = medelgod","III = dålig","Imp = impediment" 
     DROP-DOWN-LIST
     SIZE 24.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_MLANGD AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Medelhöjd" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","22","24","26","28","30" 
     DROP-DOWN-LIST
     SIZE 8 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_OMRSKOG AS CHARACTER FORMAT "X(20)":U 
     LABEL "Skogområde" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Norrlands inland","Norrlands kustland","Svealand","Götaland" 
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_SLENH AS DECIMAL FORMAT ">9.9":U INITIAL 1 
     LABEL "Slutenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1.0" 
     DROP-DOWN-LIST
     SIZE 8.38 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROCENT AS INTEGER FORMAT ">>9":U INITIAL 100 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROCENT-2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BESTAND AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Bestånd" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_BREDD AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Bredd" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_HA AS DECIMAL FORMAT "->>>>9.999" INITIAL 0 
     LABEL "Areal HA" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_HA-2 AS DECIMAL FORMAT "->>>>9.999" INITIAL 0 
     LABEL "Areal HA" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_LANGD AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Längd" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE TOG_AVTST AS LOGICAL INITIAL no 
     LABEL "Avtal Svensk Energi- Storskogsbolag" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .79 NO-UNDO.

DEFINE VARIABLE EDT_HJALPSTN AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 37 BY 4 NO-UNDO.

DEFINE VARIABLE FILL-IN_LTRAFO1 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_LTRAFO2 AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Pris/st" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_LTRAFO3 AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Typ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_LTRAFO4 AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Enl. ritning nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE TOG_KSK AS LOGICAL INITIAL no 
     LABEL "Kabelskåp" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_TILLFSKADA AS LOGICAL INITIAL no 
     LABEL "Tillfällig skada" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BHKL AS CHARACTER FORMAT "X(3)":U 
     LABEL "Björk hkl" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "T1","T2","T3","T4","T5","T6","T7","T8" 
     DROP-DOWN-LIST
     SIZE 6.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_DIAM AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Diam" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "8","10","12","14","16","18","20","22","24","26","28","30","32","34","36","38","40","42","44","46" 
     DROP-DOWN-LIST
     SIZE 6.88 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_GHKL AS CHARACTER FORMAT "X(3)":U 
     LABEL "Gran hkl" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11" 
     DROP-DOWN-LIST
     SIZE 6.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_OHKL AS CHARACTER FORMAT "X(3)":U 
     LABEL "Övriga hkl" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "T1","T2","T3","T4","T5","T6","T7","T8" 
     DROP-DOWN-LIST
     SIZE 6.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_THKL AS CHARACTER FORMAT "X(3)":U 
     LABEL "Tall hkl" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "T1","T2","T3","T4","T5","T6","T7","T8" 
     DROP-DOWN-LIST
     SIZE 6.25 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN_BANT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Björk ant" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_GANT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Gran ant" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_OANT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Övriga ant" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_OKORR AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Korr %" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_TANT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Tall antal" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE CMB_ZON AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Zon" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "         1","         2" 
     DROP-DOWN-LIST
     SIZE 8 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-KRM AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Kr / m" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-METER AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "meter" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOMKAB FOR 
      kabkomtemp SCROLLING.

DEFINE QUERY BRW_KOMTR FOR 
      kabkomtemp SCROLLING.

DEFINE QUERY BRW_KSKSTN FOR 
      stnksktemp SCROLLING.

DEFINE QUERY BRW_STAMP FOR 
      volskogstamp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOMKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMKAB DIALOG-1 _STRUCTURED
  QUERY BRW_KOMKAB NO-LOCK DISPLAY
      kabkomtemp.kabkom COLUMN-LABEL "Kommentarer" FORMAT "x(256)":U
            WIDTH 41.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 44 BY 8.5
         TITLE "Dubbelklicka för val av kommentarer" ROW-HEIGHT-CHARS .38 FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KOMTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMTR DIALOG-1 _STRUCTURED
  QUERY BRW_KOMTR NO-LOCK DISPLAY
      kabkomtemp.kabkom COLUMN-LABEL "Kommentarer" FORMAT "x(256)":U
            WIDTH 33.63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 36.5 BY 6.75
         TITLE "Dubbelklicka för val av kommentarer" ROW-HEIGHT-CHARS .38 FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KSKSTN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KSKSTN DIALOG-1 _STRUCTURED
  QUERY BRW_KSKSTN NO-LOCK DISPLAY
      stnksktemp.PRIS COLUMN-LABEL "Pris/st" FORMAT ">>>>9":U
      stnksktemp.BENAMNING COLUMN-LABEL "Station/Kabelskåp" FORMAT "x(256)":U
            WIDTH 40.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 50.5 BY 7.75
         TITLE "Dubbelklicka för val av station/kabelskåp" ROW-HEIGHT-CHARS .38 FIT-LAST-COLUMN.

DEFINE BROWSE BRW_STAMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STAMP DIALOG-1 _STRUCTURED
  QUERY BRW_STAMP NO-LOCK DISPLAY
      volskogstamp.DIAM COLUMN-LABEL "Dia!cm" FORMAT ">9":U
      volskogstamp.TANT COLUMN-LABEL "Tall!antal" FORMAT ">>9":U
            WIDTH 5
      volskogstamp.STTM3 COLUMN-LABEL "m3/st" FORMAT ">>9.99":U
      volskogstamp.TM3 COLUMN-LABEL "m3" FORMAT ">>9.99":U
      volskogstamp.GANT COLUMN-LABEL "Gran!antal" FORMAT ">>9":U
            WIDTH 5
      volskogstamp.STGM3 COLUMN-LABEL "m3/st" FORMAT ">>9.99":U
      volskogstamp.GM3 COLUMN-LABEL "m3" FORMAT ">>9.99":U
      volskogstamp.BANT COLUMN-LABEL "Björk!antal" FORMAT ">>9":U
            WIDTH 5
      volskogstamp.STBM3 COLUMN-LABEL "m3/st" FORMAT ">>9.99":U
      volskogstamp.BM3 COLUMN-LABEL "m3" FORMAT ">>9.99":U
      volskogstamp.OANT COLUMN-LABEL "Övriga!antal" FORMAT ">>9":U
            WIDTH 6
      volskogstamp.STOM3 COLUMN-LABEL "m3/st" FORMAT ">>9.99":U
      volskogstamp.OM3 COLUMN-LABEL "m3" FORMAT ">>9.99":U
      volskogstamp.OKORR COLUMN-LABEL "Korr %" FORMAT ">>9":U WIDTH 5
      volskogstamp.SUMMAANT COLUMN-LABEL "Summa!antal" FORMAT ">>>>9":U
      volskogstamp.SUMMAM3 COLUMN-LABEL "Summa!m3" FORMAT ">>>>9.99":U
            WIDTH 8
  ENABLE
      volskogstamp.TANT
      volskogstamp.GANT
      volskogstamp.BANT
      volskogstamp.OANT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 120 BY 20
         TITLE "Stamräkningslängd".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_REG AT ROW 25.25 COL 95
     BTN_AVS AT ROW 25.25 COL 109.88
     SPACE(1.61) SKIP(0.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra värdering":L.

DEFINE FRAME FRAME-ANDVOL
     FILL-IN_OKORR AT ROW 1.25 COL 117.5 COLON-ALIGNED
     FILL-IN_BETECKNING AT ROW 1.5 COL 24 COLON-ALIGNED
          LABEL "Fastighetsbeteckning" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 30.38 BY 1
     CMB_THKL AT ROW 1.5 COL 62 COLON-ALIGNED
     CMB_GHKL AT ROW 1.5 COL 79 COLON-ALIGNED
     CMB_BHKL AT ROW 1.5 COL 100 COLON-ALIGNED
     CMB_OHKL AT ROW 1.5 COL 110.5 COLON-ALIGNED
     CMB_DIAM AT ROW 2.5 COL 24 COLON-ALIGNED
     FILL-IN_TANT AT ROW 3.54 COL 24 COLON-ALIGNED
     BRW_STAMP AT ROW 4.25 COL 3.5 WIDGET-ID 400
     FILL-IN_GANT AT ROW 4.5 COL 24 COLON-ALIGNED
     FILL-IN_BANT AT ROW 5.5 COL 23.5 COLON-ALIGNED
     FILL-IN_OANT AT ROW 6.25 COL 23.5 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.5 BY 23.75.

DEFINE FRAME FRAME-ANDAKAB
     FILL-IN_BETECKNING AT ROW 1.5 COL 21.5 COLON-ALIGNED
     FILL-IN_LAKAB1 AT ROW 2.54 COL 21.5 COLON-ALIGNED
     FILL-IN_FLKAB AT ROW 3.58 COL 21.5 COLON-ALIGNED
     FILL-IN_LAKAB2 AT ROW 3.58 COL 35.13 COLON-ALIGNED
     FILL-IN_FASTPRIS AT ROW 4.63 COL 21.5 COLON-ALIGNED
     FILL-IN_KRONOR AT ROW 4.63 COL 35.13 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 5.71 COL 21.5 COLON-ALIGNED
     BRW_KOMKAB AT ROW 7.25 COL 62 WIDGET-ID 100
     TOG_SAM AT ROW 7.54 COL 23.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106 BY 15.25.

DEFINE FRAME FRAME-ANDSKOG
     FILL-IN_BETECKNING AT ROW 1.5 COL 22.5 COLON-ALIGNED
          LABEL "Fastighetsbeteckning" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 30.38 BY 1
     FILL-IN_BESTAND AT ROW 2.54 COL 22.5 COLON-ALIGNED
     FILL-IN_LANGD AT ROW 3.58 COL 22.5 COLON-ALIGNED
     FILL-IN_BREDD AT ROW 4.63 COL 22.5 COLON-ALIGNED
     FILL-IN-PROCENT AT ROW 5.67 COL 22.5 COLON-ALIGNED
     FILL-IN-PROCENT-2 AT ROW 5.67 COL 57 COLON-ALIGNED
     FILL-IN_HA AT ROW 6.71 COL 22.5 COLON-ALIGNED
     FILL-IN_HA-2 AT ROW 6.71 COL 57 COLON-ALIGNED
     CMB_MARKTYP AT ROW 7.75 COL 22.5 COLON-ALIGNED
     CMB_MARKTYP-2 AT ROW 7.75 COL 57 COLON-ALIGNED
     CMB_BESTTYP AT ROW 8.79 COL 22.5 COLON-ALIGNED
     CMB_BESTTYP-2 AT ROW 8.79 COL 57 COLON-ALIGNED
     CMB_BARRLOV AT ROW 9.83 COL 22.5 COLON-ALIGNED
     CMB_BARRLOV-2 AT ROW 9.83 COL 57 COLON-ALIGNED
     CMB_SLENH AT ROW 10.88 COL 22.5 COLON-ALIGNED
     CMB_MLANGD AT ROW 11.92 COL 22.5 COLON-ALIGNED
     FILL-IN_FASTPRIS AT ROW 12.96 COL 22.5 COLON-ALIGNED
          LABEL "Fast pris" FORMAT "Ja/Nej"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     FILL-IN_KRONOR AT ROW 14 COL 22.5 COLON-ALIGNED
          LABEL "Kronor" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     FILL-IN_BENAMNING AT ROW 15.04 COL 22.5 COLON-ALIGNED
          LABEL "Kommentar" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     TOG_AVTST AT ROW 2.54 COL 55.5
     CMB_OMRSKOG AT ROW 3.5 COL 53.88 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.5 BY 15.5.

DEFINE FRAME FRAME-REV
     FILL-IN_BETECKNING AT ROW 1.5 COL 21.5 COLON-ALIGNED
          LABEL "Fastighetsbeteckning" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 30.38 BY 1
     CMB_ZON AT ROW 2.58 COL 21.5 COLON-ALIGNED
     FILL-IN-KRM AT ROW 3.67 COL 21.5 COLON-ALIGNED
     FILL-IN-METER AT ROW 4.75 COL 21.5 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 1
         SIZE 87 BY 15.25.

DEFINE FRAME FRAME-ANDAKER
     FILL-IN_BETECKNING AT ROW 1.5 COL 26.13 COLON-ALIGNED
          LABEL "Fastighetsbeteckning" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 30.38 BY 1
     FILL-IN_STOLPNR AT ROW 2.58 COL 26.13 COLON-ALIGNED
     CMB_SORT AT ROW 3.71 COL 26.13 COLON-ALIGNED
     FILL-IN_LAKER1 AT ROW 4.63 COL 26.13 COLON-ALIGNED
     FILL-IN_LAKER2 AT ROW 5.63 COL 26.13 COLON-ALIGNED
     FILL-IN_LAKER3 AT ROW 6.63 COL 26.13 COLON-ALIGNED
     FILL-IN_LAKER4 AT ROW 7.58 COL 27.5 COLON-ALIGNED
     FILL-IN_LAKER5 AT ROW 8.63 COL 26.13 COLON-ALIGNED
     FILL-IN_BORTINT AT ROW 9.71 COL 26.13 COLON-ALIGNED
     FILL-IN_FASTPRIS AT ROW 10.71 COL 26.13 COLON-ALIGNED
          LABEL "Fastpris" FORMAT "Ja/Nej"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     FILL-IN_KRONOR AT ROW 10.71 COL 39.75 COLON-ALIGNED
          LABEL "Kronor" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     FILL-IN_BENAMNING AT ROW 11.79 COL 26 COLON-ALIGNED
          LABEL "Kommentar" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     FILL-IN-VARNING AT ROW 13 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     IMAGE-1 AT ROW 1.54 COL 62.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89 BY 13.5.

DEFINE FRAME FRAME-ANDTRAFO
     FILL-IN_BETECKNING AT ROW 1.5 COL 21.5 COLON-ALIGNED
          LABEL "Fastighetsbeteckning" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 30.38 BY 1
     BRW_KSKSTN AT ROW 1.5 COL 56 WIDGET-ID 300
     TOG_TILLFSKADA AT ROW 1.67 COL 59.5
     FILL-IN_STOLPNR AT ROW 2.75 COL 21.5 COLON-ALIGNED
          LABEL "Nr" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     FILL-IN_LTRAFO4 AT ROW 3.75 COL 90 COLON-ALIGNED
     FILL-IN_LTRAFO1 AT ROW 4 COL 21.5 COLON-ALIGNED
     FILL-IN_LTRAFO2 AT ROW 5.25 COL 21.5 COLON-ALIGNED
     FILL-IN_LTRAFO3 AT ROW 5.25 COL 37 COLON-ALIGNED WIDGET-ID 6
     FILL-IN_KRONOR AT ROW 6.5 COL 21.5 COLON-ALIGNED
          LABEL "Kronor" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     TOG_KSK AT ROW 8.79 COL 23.5
     BRW_KOMTR AT ROW 9.25 COL 70 WIDGET-ID 200
     FILL-IN_BENAMNING AT ROW 9.96 COL 21.5 COLON-ALIGNED
          LABEL "Kommentar" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 45.5 BY 1
     EDT_HJALPSTN AT ROW 11.75 COL 15 NO-LABEL WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106 BY 15.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: kabkomtemp T "?" NO-UNDO temp-db kabkomtemp
      TABLE: stnksktemp T "?" NO-UNDO temp-db stnksktemp
      TABLE: volskogstamp T "?" NO-UNDO temp-db volskogstamp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANDAKAB:FRAME = FRAME DIALOG-1:HANDLE
       FRAME FRAME-ANDAKER:FRAME = FRAME DIALOG-1:HANDLE
       FRAME FRAME-ANDSKOG:FRAME = FRAME DIALOG-1:HANDLE
       FRAME FRAME-ANDTRAFO:FRAME = FRAME DIALOG-1:HANDLE
       FRAME FRAME-ANDVOL:FRAME = FRAME DIALOG-1:HANDLE
       FRAME FRAME-REV:FRAME = FRAME DIALOG-1:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-REV:MOVE-BEFORE-TAB-ITEM (BTN_REG:HANDLE IN FRAME DIALOG-1)
       XXTABVALXX = FRAME FRAME-ANDTRAFO:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-REV:HANDLE)
       XXTABVALXX = FRAME FRAME-ANDAKER:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ANDTRAFO:HANDLE)
       XXTABVALXX = FRAME FRAME-ANDVOL:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ANDAKER:HANDLE)
       XXTABVALXX = FRAME FRAME-ANDSKOG:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ANDVOL:HANDLE)
       XXTABVALXX = FRAME FRAME-ANDAKAB:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ANDSKOG:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANDAKAB
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KOMKAB FILL-IN_BENAMNING FRAME-ANDAKAB */
ASSIGN 
       FRAME FRAME-ANDAKAB:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-ANDAKAB
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOG_SAM IN FRAME FRAME-ANDAKAB
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_SAM:HIDDEN IN FRAME FRAME-ANDAKAB           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANDAKER
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-ANDAKER:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VARNING IN FRAME FRAME-ANDAKER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-ANDAKER
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LAKER4 IN FRAME FRAME-ANDAKER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LAKER4:HIDDEN IN FRAME FRAME-ANDAKER           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LAKER5 IN FRAME FRAME-ANDAKER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LAKER5:HIDDEN IN FRAME FRAME-ANDAKER           = TRUE.

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME FRAME-ANDAKER
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-1:HIDDEN IN FRAME FRAME-ANDAKER           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANDSKOG
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME FRAME-ANDSKOG:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_BARRLOV-2 IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_BARRLOV-2:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_BESTTYP-2 IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_BESTTYP-2:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_MARKTYP-2 IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_MARKTYP-2:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OMRSKOG IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       CMB_OMRSKOG:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PROCENT-2 IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-PROCENT-2:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-ANDSKOG
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_HA-2 IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN_HA-2:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

ASSIGN 
       FILL-IN_KRONOR:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_AVTST IN FRAME FRAME-ANDSKOG
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_AVTST:HIDDEN IN FRAME FRAME-ANDSKOG           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANDTRAFO
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KSKSTN FILL-IN_BETECKNING FRAME-ANDTRAFO */
/* BROWSE-TAB BRW_KOMTR TOG_KSK FRAME-ANDTRAFO */
ASSIGN 
       FRAME FRAME-ANDTRAFO:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDT_HJALPSTN IN FRAME FRAME-ANDTRAFO
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-ANDTRAFO
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LTRAFO3 IN FRAME FRAME-ANDTRAFO
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LTRAFO3:HIDDEN IN FRAME FRAME-ANDTRAFO           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LTRAFO4 IN FRAME FRAME-ANDTRAFO
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LTRAFO4:HIDDEN IN FRAME FRAME-ANDTRAFO           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_KSK IN FRAME FRAME-ANDTRAFO
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_KSK:HIDDEN IN FRAME FRAME-ANDTRAFO           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_TILLFSKADA IN FRAME FRAME-ANDTRAFO
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_TILLFSKADA:HIDDEN IN FRAME FRAME-ANDTRAFO           = TRUE.

/* SETTINGS FOR FRAME FRAME-ANDVOL
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_STAMP FILL-IN_TANT FRAME-ANDVOL */
ASSIGN 
       FRAME FRAME-ANDVOL:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-ANDVOL
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-REV
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-REV:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KRM IN FRAME FRAME-REV
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-REV
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMKAB
/* Query rebuild information for BROWSE BRW_KOMKAB
     _TblList          = "Temp-Tables.kabkomtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kabkomtemp.kabkom
"kabkomtemp.kabkom" "Kommentarer" "x(256)" "character" ? ? ? ? ? ? no ? no no "41.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KOMKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMTR
/* Query rebuild information for BROWSE BRW_KOMTR
     _TblList          = "Temp-Tables.kabkomtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kabkomtemp.kabkom
"kabkomtemp.kabkom" "Kommentarer" "x(256)" "character" ? ? ? ? ? ? no ? no no "33.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KOMTR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KSKSTN
/* Query rebuild information for BROWSE BRW_KSKSTN
     _TblList          = "Temp-Tables.stnksktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.stnksktemp.PRIS
"stnksktemp.PRIS" "Pris/st" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.stnksktemp.BENAMNING
"stnksktemp.BENAMNING" "Station/Kabelskåp" "x(256)" "character" ? ? ? ? ? ? no ? no no "40.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KSKSTN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STAMP
/* Query rebuild information for BROWSE BRW_STAMP
     _TblList          = "Temp-Tables.volskogstamp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.volskogstamp.DIAM
"DIAM" "Dia!cm" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.volskogstamp.TANT
"TANT" "Tall!antal" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.volskogstamp.STTM3
"STTM3" "m3/st" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.volskogstamp.TM3
"TM3" "m3" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.volskogstamp.GANT
"GANT" "Gran!antal" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.volskogstamp.STGM3
"STGM3" "m3/st" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.volskogstamp.GM3
"GM3" "m3" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.volskogstamp.BANT
"BANT" "Björk!antal" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.volskogstamp.STBM3
"STBM3" "m3/st" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.volskogstamp.BM3
"BM3" "m3" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.volskogstamp.OANT
"OANT" "Övriga!antal" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.volskogstamp.STOM3
"STOM3" "m3/st" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.volskogstamp.OM3
"OM3" "m3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.volskogstamp.OKORR
"OKORR" "Korr %" ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.volskogstamp.SUMMAANT
"SUMMAANT" "Summa!antal" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.volskogstamp.SUMMAM3
"SUMMAM3" "Summa!m3" ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_STAMP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDAKAB
/* Query rebuild information for FRAME FRAME-ANDAKAB
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDAKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDAKER
/* Query rebuild information for FRAME FRAME-ANDAKER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDAKER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDSKOG
/* Query rebuild information for FRAME FRAME-ANDSKOG
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDSKOG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDTRAFO
/* Query rebuild information for FRAME FRAME-ANDTRAFO
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDTRAFO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDVOL
/* Query rebuild information for FRAME FRAME-ANDVOL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDVOL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-REV
/* Query rebuild information for FRAME FRAME-REV
     _Query            is NOT OPENED
*/  /* FRAME FRAME-REV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra värdering */
DO:
   RUN tempbort_UI.
   IF VALID-HANDLE(andvardspecapph) THEN DELETE PROCEDURE andvardspecapph.   
   IF VALID-HANDLE(skprotapph) THEN DELETE PROCEDURE skprotapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra värdering */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOMKAB
&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME BRW_KOMKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOMKAB DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_KOMKAB IN FRAME FRAME-ANDAKAB /* Dubbelklicka för val av kommentarer */
DO:
   status-ok = BRW_KOMKAB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING + " " + kabkomtemp.KABKOM.
   DISPLAY FILL-IN_BENAMNING WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOMTR
&Scoped-define FRAME-NAME FRAME-ANDTRAFO
&Scoped-define SELF-NAME BRW_KOMTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOMTR DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_KOMTR IN FRAME FRAME-ANDTRAFO /* Dubbelklicka för val av kommentarer */
DO:
   status-ok = BRW_KOMTR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING + " " + kabkomtemp.KABKOM.
   DISPLAY FILL-IN_BENAMNING WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KSKSTN
&Scoped-define SELF-NAME BRW_KSKSTN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KSKSTN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_KSKSTN IN FRAME FRAME-ANDTRAFO /* Dubbelklicka för val av station/kabelskåp */
DO:
   status-ok = BRW_KSKSTN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   FILL-IN_LTRAFO2 = stnksktemp.PRIS.
   FILL-IN_LTRAFO3 = stnksktemp.ID.
   FILL-IN_KRONOR = FILL-IN_LTRAFO1 * FILL-IN_LTRAFO2. 
   DISPLAY FILL-IN_LTRAFO2 FILL-IN_KRONOR WITH FRAME {&FRAME-NAME}.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i} 
   IF val = 1  THEN DO:
      RUN btnreg_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   IF val = 2  THEN DO:
      RUN btnreg2_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   IF  val = 3 THEN DO:
      RUN btnreg3_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   IF val = 4 OR val = 5 OR val = 7 THEN DO:
      RUN btnreg4_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   IF val = 6 THEN DO:
      RUN btnreg6_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(andvardspecapph) THEN DELETE PROCEDURE andvardspecapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME CMB_BARRLOV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BARRLOV DIALOG-1
ON VALUE-CHANGED OF CMB_BARRLOV IN FRAME FRAME-ANDSKOG /* Barr-Löv */
DO:
  CMB_BARRLOV = INPUT CMB_BARRLOV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BARRLOV-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BARRLOV-2 DIALOG-1
ON VALUE-CHANGED OF CMB_BARRLOV-2 IN FRAME FRAME-ANDSKOG /* Barr-Löv */
DO:
  CMB_BARRLOV-2 = INPUT CMB_BARRLOV-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESTTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESTTYP DIALOG-1
ON VALUE-CHANGED OF CMB_BESTTYP IN FRAME FRAME-ANDSKOG /* Besttyp */
DO:
  CMB_BESTTYP = INPUT CMB_BESTTYP.
  IF CMB_BESTTYP = "5a = äldre timmerskog" OR CMB_BESTTYP = "5b = kalmark" THEN DO:
     CMB_BARRLOV:HIDDEN = TRUE.
     CMB_SLENH:HIDDEN = TRUE.
  END.
  ELSE DO:
     CMB_BARRLOV:HIDDEN = FALSE.
     CMB_SLENH:HIDDEN = FALSE.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESTTYP-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESTTYP-2 DIALOG-1
ON VALUE-CHANGED OF CMB_BESTTYP-2 IN FRAME FRAME-ANDSKOG /* Besttyp */
DO:
  CMB_BESTTYP-2 = INPUT CMB_BESTTYP-2.
  IF CMB_BESTTYP-2 = "5a = äldre timmerskog" OR CMB_BESTTYP-2 = "5b = kalmark" THEN DO:
     CMB_BARRLOV-2:HIDDEN = TRUE.     
  END.
  ELSE DO:
     CMB_BARRLOV-2:HIDDEN = FALSE.     
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDVOL
&Scoped-define SELF-NAME CMB_BHKL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BHKL DIALOG-1
ON ENTRY OF CMB_BHKL IN FRAME FRAME-ANDVOL /* Björk hkl */
DO:
   CMB_BHKL = INPUT CMB_BHKL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DIAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DIAM DIALOG-1
ON ENTRY OF CMB_DIAM IN FRAME FRAME-ANDVOL /* Diam */
DO:
   CMB_DIAM = INPUT CMB_DIAM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_GHKL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_GHKL DIALOG-1
ON ENTRY OF CMB_GHKL IN FRAME FRAME-ANDVOL /* Gran hkl */
DO:
   CMB_GHKL = INPUT CMB_GHKL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME CMB_MARKTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MARKTYP DIALOG-1
ON VALUE-CHANGED OF CMB_MARKTYP IN FRAME FRAME-ANDSKOG /* Marktyp */
DO:
  CMB_MARKTYP = INPUT CMB_MARKTYP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MARKTYP-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MARKTYP-2 DIALOG-1
ON VALUE-CHANGED OF CMB_MARKTYP-2 IN FRAME FRAME-ANDSKOG /* Marktyp */
DO:
  CMB_MARKTYP-2 = INPUT CMB_MARKTYP-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDVOL
&Scoped-define SELF-NAME CMB_OHKL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OHKL DIALOG-1
ON ENTRY OF CMB_OHKL IN FRAME FRAME-ANDVOL /* Övriga hkl */
DO:
   CMB_OHKL = INPUT CMB_OHKL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME CMB_OMRSKOG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMRSKOG DIALOG-1
ON LEAVE OF CMB_OMRSKOG IN FRAME FRAME-ANDSKOG /* Skogområde */
DO:
   CMB_OMRSKOG = INPUT CMB_OMRSKOG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMRSKOG DIALOG-1
ON VALUE-CHANGED OF CMB_OMRSKOG IN FRAME FRAME-ANDSKOG /* Skogområde */
DO:
   CMB_OMRSKOG = INPUT CMB_OMRSKOG. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_SLENH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_SLENH DIALOG-1
ON VALUE-CHANGED OF CMB_SLENH IN FRAME FRAME-ANDSKOG /* Slutenhet */
DO:
  CMB_SLENH = INPUT CMB_SLENH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKER
&Scoped-define SELF-NAME CMB_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_SORT DIALOG-1
ON VALUE-CHANGED OF CMB_SORT IN FRAME FRAME-ANDAKER /* Å/B/I */
DO:
  CMB_SORT = INPUT CMB_SORT.
  IF CMB_SORT = "BETESMARK" OR CMB_SORT = "IMPEDIMENT" THEN DO:
     ASSIGN  FILL-IN_LAKER3:HIDDEN = TRUE.
  END.
  ELSE DO:
     ASSIGN  FILL-IN_LAKER3:HIDDEN = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDVOL
&Scoped-define SELF-NAME CMB_THKL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_THKL DIALOG-1
ON ENTRY OF CMB_THKL IN FRAME FRAME-ANDVOL /* Tall hkl */
DO:
   CMB_THKL = INPUT CMB_THKL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-REV
&Scoped-define SELF-NAME CMB_ZON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ZON DIALOG-1
ON VALUE-CHANGED OF CMB_ZON IN FRAME FRAME-REV /* Zon */
DO:
   CMB_ZON = INPUT CMB_ZON.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "REVADM"                   
   inextradatatemp.HUVUDINT = 1                    
   inextradatatemp.HUVUDCH = "1".                          
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp  NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO: 
      IF CMB_ZON = 1 THEN revtemp.KRPM = extradatatemp.SOKDEC[2].
      IF CMB_ZON = 2 THEN revtemp.KRPM = extradatatemp.SOKDEC[3].            
      FILL-IN-KRM = revtemp.KRPM.         
      DISPLAY FILL-IN-KRM WITH FRAME FRAME-REV. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-METER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-METER DIALOG-1
ON LEAVE OF FILL-IN-METER IN FRAME FRAME-REV /* meter */
DO:
  FILL-IN-METER = INPUT FILL-IN-METER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN-PROCENT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PROCENT DIALOG-1
ON LEAVE OF FILL-IN-PROCENT IN FRAME FRAME-ANDSKOG /* Procent */
DO:
  &Scoped-define FRAME-NAME 
  FILL-IN-PROCENT = INPUT FILL-IN-PROCENT.  
  FILL-IN_HA = (FILL-IN-PROCENT / 100) * FILL-IN_LANGD * FILL-IN_BREDD / 10000.
  DISPLAY FILL-IN_HA  WITH FRAME FRAME-ANDSKOG.
  IF FILL-IN-PROCENT < 100 AND FILL-IN-PROCENT > 0 THEN DO:
     ASSIGN
     FILL-IN-PROCENT-2 = 100 - FILL-IN-PROCENT     
     FILL-IN_HA-2 = (FILL-IN-PROCENT-2 / 100) * FILL-IN_LANGD * FILL-IN_BREDD / 10000.        
     ASSIGN  CMB_MARKTYP-2:SCREEN-VALUE  IN FRAME FRAME-ANDSKOG = "I   = mycket god".
     ASSIGN  CMB_BESTTYP-2:SCREEN-VALUE = "1 = plantskog".                
     FILL-IN-PROCENT-2:HIDDEN  = FALSE.
     CMB_MARKTYP-2:HIDDEN = FALSE.
     CMB_BESTTYP-2:HIDDEN = FALSE.
     CMB_BARRLOV-2:HIDDEN = FALSE.     
     FILL-IN_HA-2:HIDDEN = FALSE.                
     DISPLAY FILL-IN-PROCENT-2 CMB_MARKTYP-2 CMB_BESTTYP-2 CMB_BARRLOV-2  FILL-IN_HA-2 WITH FRAME FRAME-ANDSKOG.
     
  END.
  ELSE DO:
     ASSIGN
     FILL-IN-PROCENT-2 = 0
     FILL-IN_HA-2 = 0.
     FILL-IN-PROCENT-2:HIDDEN IN FRAME FRAME-ANDSKOG = TRUE.
     CMB_MARKTYP-2:HIDDEN = TRUE.
     CMB_BESTTYP-2:HIDDEN = TRUE.
     CMB_BARRLOV-2:HIDDEN = TRUE.     
     FILL-IN_HA-2:HIDDEN = TRUE.                     
     
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PROCENT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PROCENT-2 DIALOG-1
ON LEAVE OF FILL-IN-PROCENT-2 IN FRAME FRAME-ANDSKOG /* Procent */
DO:
  FILL-IN-PROCENT-2 = INPUT FILL-IN-PROCENT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME FILL-IN_BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BENAMNING DIALOG-1
ON LEAVE OF FILL-IN_BENAMNING IN FRAME FRAME-ANDAKAB /* Kommentar */
DO:
  FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN_BESTAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTAND DIALOG-1
ON ENTRY OF FILL-IN_BESTAND IN FRAME FRAME-ANDSKOG /* Bestånd */
DO:
   FILL-IN_BESTAND = INPUT FILL-IN_BESTAND.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKER
&Scoped-define SELF-NAME FILL-IN_BORTINT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BORTINT DIALOG-1
ON LEAVE OF FILL-IN_BORTINT IN FRAME FRAME-ANDAKER /* Borttaget intrång */
DO:
   FILL-IN_BORTINT = INPUT FILL-IN_BORTINT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BORTINT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_BORTINT IN FRAME FRAME-ANDAKER /* Borttaget intrång */
DO:   
   IF INPUT FILL-IN_BORTINT = TRUE THEN FILL-IN_BORTINT = FALSE.
   IF INPUT FILL-IN_BORTINT = FALSE THEN FILL-IN_BORTINT = TRUE.
   DISPLAY FILL-IN_BORTINT /*FILL-IN_KRONOR*/  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN_BREDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BREDD DIALOG-1
ON LEAVE OF FILL-IN_BREDD IN FRAME FRAME-ANDSKOG /* Bredd */
DO:
  FILL-IN_BREDD = INPUT FILL-IN_BREDD.  
  FILL-IN_HA = FILL-IN_LANGD * FILL-IN_BREDD / 10000.
  DISPLAY FILL-IN_HA WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKER
&Scoped-define SELF-NAME FILL-IN_FASTPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON LEAVE OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDAKER /* Fastpris */
DO:
  FILL-IN_FASTPRIS = INPUT FILL-IN_FASTPRIS.
  IF FILL-IN_FASTPRIS = FALSE THEN DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = FALSE.
      DISPLAY FILL-IN_FASTPRIS FILL-IN_KRONOR  WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDAKER /* Fastpris */
DO:   
   IF INPUT FILL-IN_FASTPRIS = TRUE THEN FILL-IN_FASTPRIS = FALSE.
   IF INPUT FILL-IN_FASTPRIS = FALSE THEN FILL-IN_FASTPRIS = TRUE.
   IF FILL-IN_FASTPRIS = TRUE THEN DO:
      ASSIGN FILL-IN_KRONOR:HIDDEN = FALSE.
   END.   
   ELSE DO:     
      ASSIGN FILL-IN_KRONOR:HIDDEN = TRUE.
   END.     
   DISPLAY FILL-IN_FASTPRIS /*FILL-IN_KRONOR*/  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME FILL-IN_FASTPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON LEAVE OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDAKAB /* Fast pris */
DO:
  FILL-IN_FASTPRIS = INPUT FILL-IN_FASTPRIS.
  IF FILL-IN_FASTPRIS = FALSE THEN DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = FALSE.
      DISPLAY FILL-IN_FASTPRIS FILL-IN_KRONOR  WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDAKAB /* Fast pris */
DO:   
   IF INPUT FILL-IN_FASTPRIS = TRUE THEN FILL-IN_FASTPRIS = FALSE.
   IF INPUT FILL-IN_FASTPRIS = FALSE THEN FILL-IN_FASTPRIS = TRUE.
   IF FILL-IN_FASTPRIS = TRUE THEN DO:
      ASSIGN FILL-IN_KRONOR:HIDDEN = FALSE.
   END.   
   ELSE DO:     
      ASSIGN FILL-IN_KRONOR:HIDDEN = TRUE.
   END.     
   DISPLAY FILL-IN_FASTPRIS /*FILL-IN_KRONOR*/  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN_FASTPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON LEAVE OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDSKOG /* Fast pris */
DO:
   FILL-IN_FASTPRIS = INPUT FILL-IN_FASTPRIS.
   IF FILL-IN_FASTPRIS = FALSE THEN DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN_KRONOR:HIDDEN = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTPRIS DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_FASTPRIS IN FRAME FRAME-ANDSKOG /* Fast pris */
DO:
  IF INPUT FILL-IN_FASTPRIS = TRUE THEN FILL-IN_FASTPRIS = FALSE.
   IF INPUT FILL-IN_FASTPRIS = FALSE THEN FILL-IN_FASTPRIS = TRUE.
   IF FILL-IN_FASTPRIS = TRUE THEN DO:
      ASSIGN FILL-IN_KRONOR:HIDDEN = FALSE.
   END.   
   ELSE DO:     
      ASSIGN FILL-IN_KRONOR:HIDDEN = TRUE.
   END.     
   DISPLAY FILL-IN_FASTPRIS /*FILL-IN_KRONOR*/  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME FILL-IN_FLKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FLKAB DIALOG-1
ON LEAVE OF FILL-IN_FLKAB IN FRAME FRAME-ANDAKAB /* Flera kablar */
DO:
  FILL-IN_FLKAB = INPUT FRAME {&FRAME-NAME} FILL-IN_FLKAB.
  IF FILL-IN_FLKAB = FALSE THEN DO:
      ASSIGN  FILL-IN_LAKAB2:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN_LAKAB2:HIDDEN = FALSE.
      DISPLAY FILL-IN_FLKAB FILL-IN_LAKAB2  WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FLKAB DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_FLKAB IN FRAME FRAME-ANDAKAB /* Flera kablar */
DO:
   IF INPUT FILL-IN_FLKAB = TRUE THEN FILL-IN_FLKAB = FALSE.
   IF INPUT FILL-IN_FLKAB = FALSE THEN FILL-IN_FLKAB = TRUE.
   IF FILL-IN_FLKAB = TRUE THEN DO:
      ASSIGN FILL-IN_LAKAB2:HIDDEN = FALSE.
   END.   
   ELSE DO:     
      ASSIGN FILL-IN_LAKAB2:HIDDEN = TRUE.
   END.     
   DISPLAY FILL-IN_FLKAB  WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN_HA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_HA DIALOG-1
ON LEAVE OF FILL-IN_HA IN FRAME FRAME-ANDSKOG /* Areal HA */
DO:
  FILL-IN_HA = INPUT FILL-IN_HA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_HA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_HA-2 DIALOG-1
ON LEAVE OF FILL-IN_HA-2 IN FRAME FRAME-ANDSKOG /* Areal HA */
DO:
  FILL-IN_HA = INPUT FILL-IN_HA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME FILL-IN_KRONOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KRONOR DIALOG-1
ON LEAVE OF FILL-IN_KRONOR IN FRAME FRAME-ANDAKAB /* Kronor */
DO:
   FILL-IN_KRONOR = INPUT FILL-IN_KRONOR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LAKAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKAB1 DIALOG-1
ON ENTRY OF FILL-IN_LAKAB1 IN FRAME FRAME-ANDAKAB /* Längd i meter */
DO:
   FILL-IN_LAKAB1 = INPUT FILL-IN_LAKAB1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKAB1 DIALOG-1
ON LEAVE OF FILL-IN_LAKAB1 IN FRAME FRAME-ANDAKAB /* Längd i meter */
DO:
  FILL-IN_LAKAB1 = INPUT FILL-IN_LAKAB1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LAKAB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKAB2 DIALOG-1
ON ENTRY OF FILL-IN_LAKAB2 IN FRAME FRAME-ANDAKAB /* Bredd */
DO:
   FILL-IN_LAKAB2 = INPUT FILL-IN_LAKAB2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKAB2 DIALOG-1
ON LEAVE OF FILL-IN_LAKAB2 IN FRAME FRAME-ANDAKAB /* Bredd */
DO:
   FILL-IN_LAKAB2 = INPUT FILL-IN_LAKAB2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKER
&Scoped-define SELF-NAME FILL-IN_LAKER1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKER1 DIALOG-1
ON LEAVE OF FILL-IN_LAKER1 IN FRAME FRAME-ANDAKER /* Längd L1 */
DO:
   FILL-IN_LAKER1 = INPUT FILL-IN_LAKER1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LAKER2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LAKER2 DIALOG-1
ON LEAVE OF FILL-IN_LAKER2 IN FRAME FRAME-ANDAKER /* Bredd L2 */
DO:
   FILL-IN_LAKER2 = INPUT FRAME {&FRAME-NAME} FILL-IN_LAKER2.      
   IF FILL-IN_LAKER2 > FILL-IN_LAKER1 THEN DO:
      MESSAGE "Bredden kan inte vara större än längden!" VIEW-AS ALERT-BOX.        
      APPLY "ENTRY" TO FILL-IN_LAKER3 IN FRAME {&FRAME-NAME}.
      &Scoped-define FRAME-NAME DIALOG-1
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME FILL-IN_LANGD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LANGD DIALOG-1
ON LEAVE OF FILL-IN_LANGD IN FRAME FRAME-ANDSKOG /* Längd */
DO:
  FILL-IN_LANGD = INPUT FILL-IN_LANGD. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDTRAFO
&Scoped-define SELF-NAME FILL-IN_LTRAFO1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LTRAFO1 DIALOG-1
ON LEAVE OF FILL-IN_LTRAFO1 IN FRAME FRAME-ANDTRAFO /* Antal */
DO:
  FILL-IN_LTRAFO1 = INPUT FILL-IN_LTRAFO1.
  IF FILL-IN_LTRAFO2 > 0 THEN DO:
     FILL-IN_KRONOR = FILL-IN_LTRAFO1 * FILL-IN_LTRAFO2. 
     DISPLAY FILL-IN_KRONOR WITH FRAME {&FRAME-NAME}.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LTRAFO2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LTRAFO2 DIALOG-1
ON LEAVE OF FILL-IN_LTRAFO2 IN FRAME FRAME-ANDTRAFO /* Pris/st */
DO:
   FILL-IN_LTRAFO2 = INPUT FILL-IN_LTRAFO2.
   IF FILL-IN_LTRAFO2 > 0 THEN DO:
     FILL-IN_KRONOR = FILL-IN_LTRAFO1 * FILL-IN_LTRAFO2. 
     DISPLAY FILL-IN_KRONOR WITH FRAME {&FRAME-NAME}.
  END.
   /*IF FILL-IN_LTRAFO2 > FILL-IN_LTRAFO1 THEN DO:
      MESSAGE "Bredden kan inte vara större än längden!" VIEW-AS ALERT-BOX.        
      APPLY "ENTRY" TO FILL-IN_LTRAFO2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LTRAFO3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LTRAFO3 DIALOG-1
ON LEAVE OF FILL-IN_LTRAFO3 IN FRAME FRAME-ANDTRAFO /* Typ */
DO:
   FILL-IN_LTRAFO2 = INPUT FILL-IN_LTRAFO2.
   IF FILL-IN_LTRAFO2 > 0 THEN DO:
     FILL-IN_KRONOR = FILL-IN_LTRAFO1 * FILL-IN_LTRAFO2. 
     DISPLAY FILL-IN_KRONOR WITH FRAME {&FRAME-NAME}.
  END.
   /*IF FILL-IN_LTRAFO2 > FILL-IN_LTRAFO1 THEN DO:
      MESSAGE "Bredden kan inte vara större än längden!" VIEW-AS ALERT-BOX.        
      APPLY "ENTRY" TO FILL-IN_LTRAFO2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_STOLPNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STOLPNR DIALOG-1
ON LEAVE OF FILL-IN_STOLPNR IN FRAME FRAME-ANDTRAFO /* Nr */
DO:
   FILL-IN_STOLPNR = INPUT FILL-IN_STOLPNR.
   IF vart = "NYA" THEN DO:
      RUN stpnrcheck IN andvardspecapph (INPUT val,INPUT valvardnr,INPUT fastighbet,
                                         INPUT FILL-IN_STOLPNR,
                                         OUTPUT TABLE felmeddtemp). 
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.  
         DELETE felmeddtemp.
         APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKER
&Scoped-define SELF-NAME FILL-IN_STOLPNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STOLPNR DIALOG-1
ON LEAVE OF FILL-IN_STOLPNR IN FRAME FRAME-ANDAKER /* Stolpnr */
DO:
   FILL-IN_STOLPNR = INPUT FILL-IN_STOLPNR.
   IF vart = "NYA" THEN DO:
      RUN stpnrcheck IN andvardspecapph (INPUT val,INPUT valvardnr,INPUT fastighbet,
                                         INPUT FILL-IN_STOLPNR,
                                         OUTPUT TABLE felmeddtemp). 
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.  
         DELETE felmeddtemp.
         APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDSKOG
&Scoped-define SELF-NAME TOG_AVTST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVTST DIALOG-1
ON VALUE-CHANGED OF TOG_AVTST IN FRAME FRAME-ANDSKOG /* Avtal Svensk Energi- Storskogsbolag */
DO:
  TOG_AVTST = INPUT TOG_AVTST.
  
  RUN stskog_UI.
  /*IF TOG_AVTST = TRUE THEN DO:
     CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
     FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
     FILL-IN-PROCENT = 100.
  END.
  ELSE DO:
     CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.    
     FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDTRAFO
&Scoped-define SELF-NAME TOG_KSK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KSK DIALOG-1
ON VALUE-CHANGED OF TOG_KSK IN FRAME FRAME-ANDTRAFO /* Kabelskåp */
DO:
  TOG_KSK  = INPUT TOG_KSK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDAKAB
&Scoped-define SELF-NAME TOG_SAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SAM DIALOG-1
ON VALUE-CHANGED OF TOG_SAM IN FRAME FRAME-ANDAKAB /* Samförläggning hsp lsp */
DO:
  TOG_SAM = INPUT TOG_SAM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDTRAFO
&Scoped-define SELF-NAME TOG_TILLFSKADA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TILLFSKADA DIALOG-1
ON VALUE-CHANGED OF TOG_TILLFSKADA IN FRAME FRAME-ANDTRAFO /* Tillfällig skada */
DO:
  TOG_TILLFSKADA = INPUT TOG_TILLFSKADA.
  IF TOG_TILLFSKADA = TRUE THEN DO:
     FILL-IN_STOLPNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     /*FILL-IN_LTRAFO4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.*/

  END.
  ELSE DO:     
     FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  END.   
  
  FILL-IN_STOLPNR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  
  /*ELSE DO:
     FILL-IN_STOLPNR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     /*FILL-IN_LTRAFO4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.*/
     FILL-IN-RITNING:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  END.*/       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KOMKAB
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   EMPTY TEMP-TABLE stnksktemp NO-ERROR. 
   RUN stnkskhamt_UI  IN andvardspecapph (OUTPUT TABLE stnksktemp).
   RUN kskapnatbelopp_UI IN skprotapph (OUTPUT kskapkr, OUTPUT nstnakerkr  , OUTPUT nstnovrigkr).
   IF Guru.Konstanter:globforetag = "KRIN" THEN DO:
      EDT_HJALPSTN = "Kabelskåp i skog " + string(kskapkr) + " kr " + CHR(10)
                  + "Kabelskåp i impediment " + string(700) + " kr " + CHR(10)
                  +   "Kabelskåp i bete " + string(1100) + " kr " + CHR(10)
                  +   "Kabelskåp i åker " + string(2200) + " kr " + CHR(10).
   END.
   ELSE EDT_HJALPSTN = "Kabelskåp " + string(kskapkr) + " kr " + CHR(10).
      
   EDT_HJALPSTN = EDT_HJALPSTN + "Nätstation i åkermark " +  string(nstnakerkr) + " kr " + CHR(10)
                 + "Nätstation i övrig mark " +  string(nstnovrigkr) + " kr " + CHR(10).
  IF varforetypval[40] = 1 THEN DO:
     EDT_HJALPSTN = EDT_HJALPSTN + "Inkl ExprL".
  END.
  ELSE DO:
     EDT_HJALPSTN = EDT_HJALPSTN + "Exkl ExprL".
  END.                           
              
   IF val = 4 OR val = 5 OR val = 7 THEN DO:            
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN EXKABAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valvardnr,INPUT fastighbet,OUTPUT omravd,OUTPUT aovar,OUTPUT delnrvar,OUTPUT ortvar,OUTPUT fkommun,OUTPUT fvaker).
      END.
      ELSE DO:
         RUN EXKABAPP.P 
         (INPUT valvardnr,INPUT fastighbet,OUTPUT omravd,OUTPUT aovar,OUTPUT delnrvar,OUTPUT ortvar,OUTPUT fkommun,OUTPUT fvaker).
      END.
   END.

   
    
   status-ok = CMB_SLENH:LIST-ITEMS = "".   
   status-ok = CMB_SLENH:delete("1.0").
   CMB_SLENH:DELIMITER = "$".  
   IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:  
      status-ok = CMB_SLENH:ADD-LAST("1.0").   
      status-ok = CMB_SLENH:ADD-LAST("0.9").   
      status-ok = CMB_SLENH:ADD-LAST("0.8").   
      status-ok = CMB_SLENH:ADD-LAST("0.7").   
      status-ok = CMB_SLENH:ADD-LAST("0.6").   
      status-ok = CMB_SLENH:ADD-LAST("0.5").
      CMB_SLENH:SCREEN-VALUE = "1.0".
   END.
   ELSE DO:
      status-ok = CMB_SLENH:ADD-LAST("1,0").   
      status-ok = CMB_SLENH:ADD-LAST("0,9").   
      status-ok = CMB_SLENH:ADD-LAST("0,8").   
      status-ok = CMB_SLENH:ADD-LAST("0,7").   
      status-ok = CMB_SLENH:ADD-LAST("0,6").   
      status-ok = CMB_SLENH:ADD-LAST("0,5"). 
      CMB_SLENH:SCREEN-VALUE = "1,0".     
   END.
   RUN enable_UI.
   /*IF Guru.Konstanter:globforetag = "umea" THEN EDT_HJALPSTN:HIDDEN = TRUE.*/
   EDT_HJALPSTN:HIDDEN = TRUE.
   IF Guru.Konstanter:globforetag = "umea" THEN BRW_KSKSTN:HIDDEN = TRUE.
   FILL-IN-VARNING:HIDDEN = TRUE.
   
   {FRMSIZED.I}
   RUN openbdynspec_UI IN brwproc[3].
   RUN grundtid_UI.
   
   IF varforetypval[40] = 1 THEN DO:
      RUN setorgtitle_UI IN brwproc[3] (INPUT "Dubbelklicka för val av station/kabelskåp. Inkl ExprL ").      
   END.
   ELSE DO:
      RUN setorgtitle_UI IN brwproc[3] (INPUT "Dubbelklicka för val av station/kabelskåp. Exkl ExprL ").      
   END.
   EMPTY TEMP-TABLE kabkomtemp NO-ERROR. 
   RUN kabkomhamt_UI  IN andvardspecapph (OUTPUT TABLE kabkomtemp).
      
   /*storskog*/
   IF varforetypval[42] = 1 THEN DO:                
      IF val = 1 THEN DO:    
                          
         &Scoped-define FRAME-NAME FRAME-ANDSKOG
         TOG_AVTST = FALSE.
         IF vart = "and" THEN DO:
            IF AVAILABLE skogvardtemp THEN DO:
              IF skogvardtemp.MARKTYP = "Norrlands inland" OR  skogvardtemp.MARKTYP = "Norrlands kustland" OR skogvardtemp.MARKTYP = "Svealand" OR skogvardtemp.MARKTYP = "Götaland" 
              OR skogvardtemp.MARKTYP = "Norrlands kustland ej Gävleborgs län" OR skogvardtemp.MARKTYP = "Tillväxtområde 3 (2009 års skognorm)" OR skogvardtemp.MARKTYP = "Tillväxtområde 4A (2009 års skognorm)" OR
              skogvardtemp.MARKTYP = "Tillväxtområde 4B (2009 års skognorm)"  THEN DO: 
                 TOG_AVTST = TRUE.
              END.   
            END.
         END.
                       
         DISPLAY TOG_AVTST  WITH FRAME {&FRAME-NAME}.
         RUN stskog_UI.
         TOG_AVTST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
         /*CMB_OMRSKOG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. */
      END.   
   END.
   ELSE DO:
      &Scoped-define FRAME-NAME FRAME-ANDSKOG
      TOG_AVTST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      CMB_OMRSKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   END.
   IF val = 4 THEN DO:      
      DEBUGGER:SET-BREAK().
      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "kabkomtemp.VAD = 'KABEL'").      
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST kabkomtemp WHERE kabkomtemp.KABKOM = FILL-IN_BENAMNING NO-ERROR.
      IF AVAILABLE kabkomtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kabkomtemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.       
   END.
   IF val = 5 THEN DO:
      
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "kabkomtemp.VAD = 'STATION'").      
      RUN openbdynspec_UI IN brwproc[2].
      FIND FIRST kabkomtemp WHERE kabkomtemp.KABKOM = FILL-IN_BENAMNING NO-ERROR.
      IF AVAILABLE kabkomtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(kabkomtemp)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.         
   END.   
   IF Guru.Konstanter:globforetag = "UMEA"  THEN.
   ELSE DO:
      IF val = 5 THEN DO:
         &Scoped-define FRAME-NAME FRAME-ANDTRAFO                   
         
         /* för att skilja ut kabelskåpen ur summan  används inte längre 20140428*/      
         /*TOG_KSK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE TOG_KSK WITH FRAME {&FRAME-NAME}.
         DISPLAY TOG_KSK WITH FRAME {&FRAME-NAME}.*/
         
         &Scoped-define FRAME-NAME DIALOG-1
      END.
   END.   
   IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      IF val = 5 OR val = 7 THEN DO:            
         &Scoped-define FRAME-NAME FRAME-ANDTRAFO
         IF val = 5 THEN DO:
             IF FILL-IN_STOLPNR = 0 THEN   TOG_TILLFSKADA = TRUE.
             ELSE TOG_TILLFSKADA = FALSE.
             FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
             FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.  
         IF val = 7 THEN DO:
            TOG_TILLFSKADA = TRUE.
            BRW_KOMTR:HIDDEN  IN FRAME {&FRAME-NAME} = TRUE.
         END.    
         /*TOG_TILLFSKADA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.         
         ENABLE TOG_TILLFSKADA WITH FRAME {&FRAME-NAME}.
         DISPLAY TOG_TILLFSKADA WITH FRAME {&FRAME-NAME}.*/
         IF TOG_TILLFSKADA = TRUE THEN DO:         
            FILL-IN_STOLPNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            /*FILL-IN_LTRAFO4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         */
            
         END.
           
         &Scoped-define FRAME-NAME DIALOG-1
      END.
   END.   
   
   /*IF  Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "celpa" THEN DO:      
      IF val = 4 OR val = 5 OR val = 7 THEN DO:                              
         IF val = 5 THEN DO:
            &Scoped-define FRAME-NAME FRAME-ANDTRAFO
            /* för att skilja ut kabelskåpen ur summan  används inte längre 20140428*/
            TOG_KSK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            ENABLE TOG_KSK WITH FRAME {&FRAME-NAME}.
            DISPLAY TOG_KSK WITH FRAME {&FRAME-NAME}.
            &Scoped-define FRAME-NAME DIALOG-1
         END.         
         IF omravd = 3 THEN DO:      
            /* borttaget 20140425 lena finns inte i några avtal längre
            IF val = 4 THEN DO:
               &Scoped-define FRAME-NAME FRAME-ANDAKAB
               TOG_LSP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
               ENABLE TOG_LSP WITH FRAME {&FRAME-NAME}.
               DISPLAY TOG_LSP WITH FRAME {&FRAME-NAME}.
               &Scoped-define FRAME-NAME DIALOG-1
            END.*/            
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK"
   OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "elpa" THEN DO:            
      IF val = 5 THEN DO:
         &Scoped-define FRAME-NAME FRAME-ANDTRAFO            
         /* för att skilja ut kabelskåpen ur summan  används inte längre 20140428*/      
         TOG_KSK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE TOG_KSK WITH FRAME {&FRAME-NAME}.
         DISPLAY TOG_KSK WITH FRAME {&FRAME-NAME}.
         
         &Scoped-define FRAME-NAME DIALOG-1
      END.                  
      /*borttaget 20140425 lena finns inte i några avtal längre
      IF val = 4 THEN DO:
         &Scoped-define FRAME-NAME FRAME-ANDAKAB
         TOG_LSP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         TOG_SAM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE TOG_LSP TOG_SAM WITH FRAME {&FRAME-NAME}.
         DISPLAY TOG_LSP TOG_SAM WITH FRAME {&FRAME-NAME}.
         &Scoped-define FRAME-NAME DIALOG-1
      END.*/                     
   END.
   IF Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "NYLB" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" 
   OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "elpa" THEN DO:            
      IF val = 5 THEN DO:        
         &Scoped-define FRAME-NAME FRAME-ANDTRAFO         
         /* för att skilja ut kabelskåpen ur summan  används inte längre 20140428*/
         TOG_KSK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE TOG_KSK WITH FRAME {&FRAME-NAME}.
         DISPLAY TOG_KSK WITH FRAME {&FRAME-NAME}.
         &Scoped-define FRAME-NAME DIALOG-1
      END.                        
   END.*/    
   RUN goma_UI.
   {DIA_M_SLUT.I}
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
      (INPUT  BRW_KOMKAB:HANDLE IN FRAME FRAME-ANDAKAB).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT  BRW_KOMTR:HANDLE IN FRAME FRAME-ANDTRAFO).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT  BRW_KSKSTN:HANDLE IN FRAME FRAME-ANDTRAFO).   
   
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING").
   RUN setcolindex_UI IN brwproc[3] (INPUT "ORDNING").
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANDVARDSPECAPP.P PERSISTENT SET andvardspecapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANDVARDSPECAPP.P PERSISTENT SET andvardspecapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
   RUN SKAPPROTOU7.P PERSISTENT SET skprotapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPPROTOU7.P PERSISTENT SET skprotapph.
   END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg2_UI DIALOG-1 
PROCEDURE btnreg2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   IF val = 2 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      ASSIGN   
      musz = FALSE    
      CMB_DIAM = INPUT FRAME {&FRAME-NAME} CMB_DIAM 
      FILL-IN_TANT = INPUT FILL-IN_TANT 
      CMB_THKL = INPUT CMB_THKL    
      FILL-IN_GANT = INPUT FILL-IN_GANT 
      CMB_GHKL = INPUT CMB_GHKL   
      FILL-IN_BANT = INPUT FILL-IN_BANT 
      CMB_BHKL = INPUT CMB_BHKL    
      FILL-IN_OANT = INPUT FILL-IN_OANT 
      CMB_OHKL = INPUT CMB_OHKL
      FILL-IN_OKORR = INPUT FILL-IN_OKORR.    
      IF CMB_DIAM = 0 THEN DO:
         MESSAGE "Diameter kan inte vara blank!" VIEW-AS ALERT-BOX.   
         musz = TRUE.
         APPLY "ENTRY" TO CMB_DIAM IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      FIND FIRST volymbertemp WHERE volymbertemp.DIAM = CMB_DIAM USE-INDEX TRAD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE volymbertemp THEN DO:
         MESSAGE "Diameter finns inte i tabell!" VIEW-AS ALERT-BOX.   
         musz = TRUE.
         APPLY "ENTRY" TO CMB_DIAM IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec AND
      volskogtemp.DIAM = 0 AND volskogtemp.OM3 > 0 NO-LOCK NO-ERROR.
      IF AVAILABLE volskogtemp THEN DO:
         MESSAGE "Total volym redan ifylld! Blanka den om du istället vill få den beräknad" VIEW-AS ALERT-BOX.   
         musz = TRUE.
         APPLY "ENTRY" TO CMB_DIAM IN FRAME {&FRAME-NAME}.
         RETURN.
      END.   
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec NO-LOCK NO-ERROR.
      ASSIGN 
      volskogtemp.DIAM = CMB_DIAM
      volskogtemp.TANT = FILL-IN_TANT
      volskogtemp.THKL = CMB_THKL  
      volskogtemp.TM3 = 0
      volskogtemp.TKR = 0
      volskogtemp.GANT = FILL-IN_GANT     
      volskogtemp.GHKL = CMB_GHKL
      volskogtemp.GM3 = 0
      volskogtemp.GKR = 0
      volskogtemp.BANT = FILL-IN_BANT  
      volskogtemp.BHKL = CMB_BHKL
      volskogtemp.BM3 = 0
      volskogtemp.BKR = 0
      volskogtemp.OANT = FILL-IN_OANT
      volskogtemp.OHKL = CMB_OHKL
      volskogtemp.OM3 = 0
      volskogtemp.OKR = 0
      volskogtemp.OKORR = FILL-IN_OKORR.
      IF volskogtemp.TANT > 0 THEN DO:
         FIND FIRST volymbertemp WHERE volymbertemp.GOMR = fastighettemp.GOMRTALL AND volymbertemp.TRAD = "TALL"
         AND volymbertemp.DIAM = volskogtemp.DIAM USE-INDEX TRAD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE volymbertemp THEN DO:
            MESSAGE "Datat finns inte upplagt i tabell!" fastighettemp.GOMRTALL volskogtemp.DIAM VIEW-AS ALERT-BOX.   
            musz = TRUE.
            APPLY "ENTRY" TO CMB_THKL IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
         ELSE IF volskogtemp.THKL = "T1" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G1 * volskogtemp.TANT.
         END.      
         ELSE IF volskogtemp.THKL = "T2" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G2 * volskogtemp.TANT.         
         END.     
         ELSE IF volskogtemp.THKL = "T3" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G3 * volskogtemp.TANT.           
         END.     
         ELSE IF volskogtemp.THKL = "T4" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G4 * volskogtemp.TANT.   
         END.     
         ELSE IF volskogtemp.THKL = "T5" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G5 * volskogtemp.TANT.         
         END.     
         ELSE IF volskogtemp.THKL = "T6" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G6 * volskogtemp.TANT.    
         END.     
         ELSE IF volskogtemp.THKL = "T7" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G7 * volskogtemp.TANT.      
         END.     
         ELSE IF volskogtemp.THKL = "T8" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G8 * volskogtemp.TANT.
         END.     
         ELSE IF volskogtemp.THKL = "T9" THEN DO:
            ASSIGN volskogtemp.TM3 = volymbertemp.G9 * volskogtemp.TANT.
         END.     
         FIND FIRST volkrtemp WHERE volkrtemp.DIAM = volskogtemp.DIAM USE-INDEX DIAM NO-LOCK NO-ERROR.
         IF AVAILABLE volkrtemp THEN DO:
            ASSIGN volskogtemp.TKR = volkrtemp.TALL * volskogtemp.TM3.
         END.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      IF volskogtemp.GANT > 0 THEN DO:
         FIND FIRST volymbertemp WHERE volymbertemp.GOMR = fastighettemp.GOMRGRAN AND volymbertemp.TRAD = "GRAN"
         AND volymbertemp.DIAM = volskogtemp.DIAM USE-INDEX TRAD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE volymbertemp THEN DO:
            MESSAGE "Datat finns inte upplagt i tabell!" fastighettemp.GOMRGRAN volskogtemp.DIAM VIEW-AS ALERT-BOX.   
            musz = TRUE.
            APPLY "ENTRY" TO CMB_GHKL IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
         ELSE IF volskogtemp.GHKL = "G1" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G1 * volskogtemp.GANT.
         END.      
         ELSE IF volskogtemp.GHKL = "G2" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G2 * volskogtemp.GANT.         
         END.     
         ELSE IF volskogtemp.GHKL = "G3" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G3 * volskogtemp.GANT.           
         END.     
         ELSE IF volskogtemp.GHKL = "G4" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G4 * volskogtemp.GANT.   
         END.     
         ELSE IF volskogtemp.GHKL = "G5" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G5 * volskogtemp.GANT.         
         END.     
         ELSE IF volskogtemp.GHKL = "G6" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G6 * volskogtemp.GANT.    
         END.     
         ELSE IF volskogtemp.GHKL = "G7" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G7 * volskogtemp.GANT.      
         END.     
         ELSE IF volskogtemp.GHKL = "G8" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G8 * volskogtemp.GANT.
         END.     
         ELSE IF volskogtemp.GHKL = "G9" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G9 * volskogtemp.GANT.
         END.     
         ELSE IF volskogtemp.GHKL = "G10" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G10 * volskogtemp.GANT.
         END.     
         ELSE IF volskogtemp.GHKL = "G11" THEN DO:
            ASSIGN volskogtemp.GM3 = volymbertemp.G11 * volskogtemp.GANT.
         END.               
         FIND FIRST volkrtemp WHERE volkrtemp.DIAM = volskogtemp.DIAM USE-INDEX DIAM NO-LOCK NO-ERROR.
         IF AVAILABLE volkrtemp THEN DO:
            ASSIGN volskogtemp.GKR = volkrtemp.GRAN * volskogtemp.GM3.
         END.
      END.   
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      IF volskogtemp.BANT > 0 THEN DO:
         FIND FIRST volymbertemp WHERE volymbertemp.GOMR = fastighettemp.GOMRGRAN AND volymbertemp.TRAD = "BJÖRK"
         AND volymbertemp.DIAM = volskogtemp.DIAM USE-INDEX TRAD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE volymbertemp THEN DO:
            MESSAGE "Datat finns inte upplagt i tabell!" fastighettemp.GOMRGRAN volskogtemp.DIAM VIEW-AS ALERT-BOX.   
            musz = TRUE.
            APPLY "ENTRY" TO CMB_BHKL IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
         ELSE IF volskogtemp.BHKL = "T1" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G1 * volskogtemp.BANT.
         END.      
         ELSE IF volskogtemp.BHKL = "T2" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G2 * volskogtemp.BANT.         
         END.     
         ELSE IF volskogtemp.BHKL = "T3" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G3 * volskogtemp.BANT.           
         END.     
         ELSE IF volskogtemp.BHKL = "T4" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G4 * volskogtemp.BANT.   
         END.     
         ELSE IF volskogtemp.BHKL = "T5" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G5 * volskogtemp.BANT.         
         END.     
         ELSE IF volskogtemp.BHKL = "T6" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G6 * volskogtemp.BANT.    
         END.     
         ELSE IF volskogtemp.BHKL = "T7" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G7 * volskogtemp.BANT.      
         END.     
         ELSE IF volskogtemp.BHKL = "T8" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G8 * volskogtemp.BANT.
         END.     
         ELSE IF volskogtemp.BHKL = "T9" THEN DO:
            ASSIGN volskogtemp.BM3 = volymbertemp.G9 * volskogtemp.BANT.
         END.     
         FIND FIRST volkrtemp WHERE volkrtemp.DIAM = volskogtemp.DIAM USE-INDEX DIAM NO-LOCK NO-ERROR.
         IF AVAILABLE volkrtemp THEN DO:
            ASSIGN volskogtemp.BKR = volkrtemp.BJORK * volskogtemp.BM3.
         END.
      END.   
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      IF volskogtemp.OANT > 0 THEN DO:
         FIND FIRST volymbertemp WHERE volymbertemp.GOMR = fastighettemp.GOMRGRAN AND volymbertemp.TRAD = "BJÖRK"
         AND volymbertemp.DIAM = volskogtemp.DIAM USE-INDEX TRAD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE volymbertemp THEN DO:
            MESSAGE "Datat finns inte upplagt i tabell!" fastighettemp.GOMRGRAN volskogtemp.DIAM VIEW-AS ALERT-BOX.   
            musz = TRUE.
            APPLY "ENTRY" TO CMB_OHKL IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
         ELSE IF volskogtemp.OHKL = "T1" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G1 * volskogtemp.OANT.
         END.      
         ELSE IF volskogtemp.OHKL = "T2" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G2 * volskogtemp.OANT.         
         END.     
         ELSE IF volskogtemp.OHKL = "T3" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G3 * volskogtemp.OANT.           
         END.     
         ELSE IF volskogtemp.OHKL = "T4" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G4 * volskogtemp.OANT.   
         END.     
         ELSE IF volskogtemp.OHKL = "T5" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G5 * volskogtemp.OANT.         
         END.     
         ELSE IF volskogtemp.OHKL = "T6" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G6 * volskogtemp.OANT.    
         END.     
         ELSE IF volskogtemp.OHKL = "T7" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G7 * volskogtemp.OANT.      
         END.     
         ELSE IF volskogtemp.OHKL = "T8" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G8 * volskogtemp.OANT.
         END.     
         ELSE IF volskogtemp.OHKL = "T9" THEN DO:
            ASSIGN volskogtemp.OM3 = volymbertemp.G9 * volskogtemp.OANT.
         END.     
         FIND FIRST volkrtemp WHERE volkrtemp.DIAM = volskogtemp.DIAM USE-INDEX DIAM NO-LOCK NO-ERROR.
         IF AVAILABLE volkrtemp THEN DO:
            ASSIGN volskogtemp.OKR = volkrtemp.BJORK * volskogtemp.OM3 * volskogtemp.OKORR / 100.
         END.
      END.
      RUN sparavol IN andvardspecapph (INPUT volskogtemp.VOLSKOGREC,INPUT TABLE volskogtemp).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg3_UI DIALOG-1 
PROCEDURE btnreg3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   IF val = 3 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      ASSIGN   
      musz = FALSE    
      FILL-IN_STOLPNR = INPUT FRAME {&FRAME-NAME} FILL-IN_STOLPNR 
      FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING 
      CMB_SORT = INPUT CMB_SORT    
      FILL-IN_LAKER1 = INPUT FILL-IN_LAKER1     
      FILL-IN_LAKER2 = INPUT FILL-IN_LAKER2 
      FILL-IN_LAKER3 = INPUT FILL-IN_LAKER3 
      /*FILL-IN_L4 = INPUT FILL-IN_L4
      FILL-IN_L5 = INPUT FILL-IN_L5*/
      FILL-IN_FASTPRIS = INPUT FILL-IN_FASTPRIS
      FILL-IN_KRONOR = INPUT FILL-IN_KRONOR. 
      IF FILL-IN_STOLPNR = 0 THEN DO:
         MESSAGE "Stolpnr kan inte vara blank!" VIEW-AS ALERT-BOX.
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      IF FILL-IN_LAKER1 = 0  THEN DO:
         MESSAGE "Längd kan inte vara 0!" VIEW-AS ALERT-BOX.  
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LAKER1 IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      IF FILL-IN_LAKER2 = 0 THEN DO:
         MESSAGE "Bredd kan inte vara 0!" VIEW-AS ALERT-BOX. 
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LAKER2 IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      IF FILL-IN_LAKER2 > FILL-IN_LAKER1 THEN DO:
         MESSAGE "Bredden kan inte vara större än längden!" VIEW-AS ALERT-BOX.   
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LAKER2 IN FRAME {&FRAME-NAME}.
         RETURN.
      END.  
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      IF vart = "NYA" THEN DO:
         RUN stpnrcheck IN andvardspecapph (INPUT val,INPUT valvardnr,INPUT fastighbet,
                                          INPUT FILL-IN_STOLPNR,OUTPUT TABLE felmeddtemp). 

         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX. 
            DELETE felmeddtemp.
            musz = TRUE.
            APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
            RETURN.  
         END.                       
      END.  
      &Scoped-define FRAME-NAME FRAME-ANDAKER      
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
      akervardtemp.BETECKNING = fastighbet AND 
      akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 0 NO-LOCK NO-ERROR.
      ASSIGN 
      akervardtemp.STOLPNR = FILL-IN_STOLPNR
      /*akervardtemp.BENAMNING = FILL-IN_BENAMNING*/
      akervardtemp.BENAMNING = FILL-IN_BENAMNING      
      /* RITNING ANVÄNDS EJ LENA 20140428
      SUBSTRING(akervardtemp.BENAMNING,60,20) = FILL-IN-RITNING*/
      akervardtemp.SORT = CMB_SORT  
      akervardtemp.L1 = FILL-IN_LAKER1
      akervardtemp.L2 = FILL-IN_LAKER2
      akervardtemp.L3 = FILL-IN_LAKER3     
/*      AKERVARD.L4 = FILL-IN_L4
      AKERVARD.L5 = FILL-IN_L5*/
      akervardtemp.FASTPRIS = FILL-IN_FASTPRIS
      bplus = 0
      lplus = 0. 
      stpnr = FILL-IN_STOLPNR.
      IF akervardtemp.FASTPRIS = TRUE AND akervardtemp.KRONOR GE 0 THEN DO:       
         ASSIGN akervardtemp.KRONOR = FILL-IN_KRONOR.
      END.
      ELSE DO:         
         DEBUGGER:SET-BREAK().
         
         FIND FIRST kindextemp WHERE kindextemp.AR = YEAR(akervardtemp.VARDEPKT) AND
         kindextemp.MANAD = MONTH(akervardtemp.VARDEPKT) USE-INDEX AR NO-LOCK NO-ERROR.  
         ivvarde = kindextemp.VARDE.
         FIND FIRST markdivtemp NO-LOCK NO-ERROR.       
         IF akervardtemp.SORT = "ÅKER" OR akervardtemp.SORT = "BETESMARK" OR akervardtemp.SORT = "IMPEDIMENT" THEN DO:
            IF akervardtemp.L2 LE 7 THEN DO:
               FIND FIRST prodakertemp WHERE prodakertemp.PRODOMR = fastighettemp.PAKER AND
               prodakertemp.BREDD = akervardtemp.L2 USE-INDEX OMR NO-LOCK NO-ERROR.               
            END.
            ELSE DO:
               FIND FIRST prodakertemp WHERE prodakertemp.PRODOMR = fastighettemp.PAKER AND
               prodakertemp.BREDD = 7 USE-INDEX OMR NO-LOCK NO-ERROR.
               ASSIGN bplus = akervardtemp.L2 - 7.
            END.   
            IF akervardtemp.L1 = 1 THEN ASSIGN ersatt = prodakertemp.L1. 
            ELSE IF akervardtemp.L1 = 2 THEN ASSIGN ersatt = prodakertemp.L2.
            ELSE IF akervardtemp.L1 = 3 THEN ASSIGN ersatt = prodakertemp.L3.
            ELSE IF akervardtemp.L1 = 4 THEN ASSIGN ersatt = prodakertemp.L4.
            ELSE IF akervardtemp.L1 = 5 THEN ASSIGN ersatt = prodakertemp.L5.
            ELSE IF akervardtemp.L1 = 6 THEN ASSIGN ersatt = prodakertemp.L6.
            ELSE IF akervardtemp.L1 = 7 THEN ASSIGN ersatt = prodakertemp.L7.
            ELSE IF akervardtemp.L1 = 8 THEN ASSIGN ersatt = prodakertemp.L8.
            ELSE IF akervardtemp.L1 = 9 THEN ASSIGN ersatt = prodakertemp.L9.
            ELSE IF akervardtemp.L1 = 10 THEN ASSIGN ersatt = prodakertemp.L10.
            ELSE IF akervardtemp.L1 = 11 THEN ASSIGN ersatt = prodakertemp.L11.
            ELSE IF akervardtemp.L1 = 12 THEN ASSIGN ersatt = prodakertemp.L12.
            ELSE IF akervardtemp.L1 = 13 THEN ASSIGN ersatt = prodakertemp.L13.
            ELSE IF akervardtemp.L1 = 14 THEN ASSIGN ersatt = prodakertemp.L14.
            ELSE IF akervardtemp.L1 = 15 THEN ASSIGN ersatt = prodakertemp.L15.
            ELSE IF akervardtemp.L1 = 16 THEN ASSIGN ersatt = prodakertemp.L16.
            ELSE IF akervardtemp.L1 = 17 THEN ASSIGN ersatt = prodakertemp.L17.
            ELSE IF akervardtemp.L1 = 18 THEN ASSIGN ersatt = prodakertemp.L18.
            ELSE IF akervardtemp.L1 = 19 THEN ASSIGN ersatt = prodakertemp.L19.
            ELSE IF akervardtemp.L1 = 20 THEN ASSIGN ersatt = prodakertemp.L20.
            ELSE DO:
               ersatt = prodakertemp.L20.
              ASSIGN lplus = akervardtemp.L1 - 20.
            END.
            /*IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "tect" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"
            OR Guru.Konstanter:globforetag = "umea" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" 
            OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:*/
            /*ELSE IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "tect" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"
            OR Guru.Konstanter:globforetag = "umea" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" 
            OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:*/
            
            
            IF Guru.Konstanter:globforetag = "umea" THEN DO:                                            
               /* pristidpunkt 200603*/
               IF prodakertemp.PRODOMR = "NN" THEN DO:
                  ltillagg = 705.
                  btillagg = 470.
               END.
               IF prodakertemp.PRODOMR = "NÖ" THEN DO:
                  ltillagg = 665.
                  btillagg = 443.
               END. 
               IF prodakertemp.PRODOMR = "GSS" THEN DO:
                  ltillagg = 1094.
                  btillagg = 730.
               END.
               IF prodakertemp.PRODOMR = "GMB" THEN DO:
                  ltillagg = 1008.
                  btillagg = 672.
               END.
               IF prodakertemp.PRODOMR = "GNS" THEN DO:
                  ltillagg = 943.
                  btillagg = 629.
               END.
               IF prodakertemp.PRODOMR = "GSK" THEN DO:
                  ltillagg = 765.
                  btillagg = 510.
               END.              
               IF prodakertemp.PRODOMR = "NO" THEN DO:
                  ltillagg = 665.
                  btillagg = 443.
               END.
               IF prodakertemp.PRODOMR = "SS" THEN DO:
                  ltillagg = 952.
                  btillagg = 634.
               END.
               IF prodakertemp.PRODOMR = "SSk" THEN DO:
                  ltillagg = 780.
                  btillagg = 520.
               END.                           
            END.
            ELSE DO:
               /*VATTENFALL HAR KOMMIT MED NYA SIFFROR I vp2015 SOM ALLA UTOM UMEÅ får inlagda */            
               /* pristidpunkt 1501*/
               IF prodakertemp.PRODOMR = "NN" THEN DO:
                  ltillagg = 729.
                  btillagg = 498.
               END.
               IF prodakertemp.PRODOMR = "NÖ" THEN DO:
                  ltillagg = 669.
                  btillagg = 485.
               END. 
               IF prodakertemp.PRODOMR = "GSS" THEN DO:
                  ltillagg = 1175.
                  btillagg = 747.
               END.
               IF prodakertemp.PRODOMR = "GMB" THEN DO:
                  ltillagg = 1065.
                  btillagg = 758.
               END.
               IF prodakertemp.PRODOMR = "GNS" THEN DO:
                  ltillagg = 990.
                  btillagg = 643.
               END.
               IF prodakertemp.PRODOMR = "GSK" THEN DO:
                  ltillagg = 797.
                  btillagg = 525.
               END.              
               /*IF prodakertemp.PRODOMR = "NO" THEN DO:
                  ltillagg = 665.
                  btillagg = 443.
               END.*/
               IF prodakertemp.PRODOMR = "SS" THEN DO:
                  ltillagg = 1000.
                  btillagg = 672.
               END.
               IF prodakertemp.PRODOMR = "SSk" THEN DO:
                  ltillagg = 820.
                  btillagg = 546.
               END.                           
            END.
            /*
            ELSE DO:            
               /*pristidpunk 9201*/
               IF prodakertemp.PRODOMR = "NN" THEN DO:
                  ltillagg = 366.
                  btillagg = 244.
               END.
               IF prodakertemp.PRODOMR = "NÖ" THEN DO:
                  ltillagg = 342.
                  btillagg = 228.
               END. 
               IF prodakertemp.PRODOMR = "GSS" THEN DO:
                  ltillagg = 582.
                  btillagg = 388.
               END.
               IF prodakertemp.PRODOMR = "GMB" THEN DO:
                  ltillagg = 528.
                  btillagg = 352.
               END.
               IF prodakertemp.PRODOMR = "GNS" THEN DO:
                  ltillagg = 501.
                  btillagg = 334.
               END.
               IF prodakertemp.PRODOMR = "GSK" THEN DO:
                  ltillagg = 402.
                  btillagg = 268.
               END.              
               IF prodakertemp.PRODOMR = "NO" THEN DO:
                  ltillagg = 342.
                  btillagg = 228.
               END.
               IF prodakertemp.PRODOMR = "SS" THEN DO:
                  ltillagg = 503.
                  btillagg = 335.
               END.
               IF prodakertemp.PRODOMR = "SSk" THEN DO:
                  ltillagg = 410.
                  btillagg = 273.
               END.                        
            END.*/
            DEBUGGER:SET-BREAK().
            IF bplus > 0 THEN ASSIGN ersatt = ersatt + bplus * btillagg.  
            IF lplus > 0 THEN ASSIGN ersatt = ersatt + lplus * ltillagg.

            IF varforetypval[40] = 1 THEN paslagfaktor = 0.25.
            ELSE paslagfaktor = 0.      
      
             ASSIGN eh = ersatt * ivvarde / markdivtemp.IPVARDE.
             /* om man räknar med dessa värden stämmer summorna mot VP2015   kpi okt 2014/ kpi 200603
            eh = ersatt * 314.02 / 282.89.*/            
            FIND FIRST akerproctemp WHERE akerproctemp.PRODOMR = fastighettemp.PAKER AND
            akerproctemp.L3MIN LE akervardtemp.L3 AND akerproctemp.L3MAX GE akervardtemp.L3
            USE-INDEX PRODOMR NO-LOCK NO-ERROR.      
            IF AVAILABLE akervardtemp THEN akervardtemp.PROCENT = akerproctemp.PROCENT.
            IF akervardtemp.SORT = "ÅKER" THEN DO:
               ASSIGN akervardtemp.KRONOR = eh * akerproctemp.PROCENT / 100 +  ( paslagfaktor * 0.66 * eh * akerproctemp.PROCENT / 100).
            END.
            ELSE IF akervardtemp.SORT = "BETESMARK" THEN DO:
               ASSIGN akervardtemp.KRONOR = eh * 15 / 100 +  ( paslagfaktor * 0.66 * eh * 15 / 100).
               akervardtemp.PROCENT = 15.
            END.
            ELSE  IF akervardtemp.SORT = "IMPEDIMENT" THEN DO:
               ASSIGN akervardtemp.KRONOR = eh * 10 / 100 +  ( paslagfaktor * 0.66 * eh * 10 / 100).
               akervardtemp.PROCENT = 10.
            END.                    
            
         /*   yf = (AKERVARD.L1 + 60 ) * (AKERVARD.L2 + 60 ).
            yk = AKERVARD.L3 * (AKERVARD.L5 + 200 ).
            yh = AKERVARD.L3 * (AKERVARD.L4 + 100 ).
            IF yf > yk THEN ykoll = yk.
            ELSE ykoll = yf.
            IF ykoll > yh THEN ymin = yh.
            ELSE ymin = ykoll. 
            eb = ( 100 + ( 10 * AKERVARD.L1)) * ivvarde / 331 + ( VARDAKER.AKER * ymin * ivvarde / MARKDIV.ITVARDE ).
            IF eh > eb THEN emin = eb.
            ELSE emin = eh.
            ASSIGN AKERVARD.KR = emin.*/            
            
            
         END.
         /*ELSE IF AKERVARD.SORT = "BETESMARK" THEN DO:
            ASSIGN AKERVARD.KR = ( 100 + (10 * AKERVARD.L1 ))* ivvarde / 331 +( VARDAKER.BETE * ( AKERVARD.L1 + 20 ) * (AKERVARD.L2 + 20 )* ivvarde / MARKDIV.ITVARDE ).
         END.
         ELSE  IF AKERVARD.SORT = "IMPEDIMENT" THEN DO:
             ASSIGN AKERVARD.KR = ( 100 + ( 10 * AKERVARD.L1 )* ivvarde / 331 ).
         END.      */
      END.    
      IF FILL-IN_BORTINT = TRUE THEN DO:
         IF akervardtemp.KRONOR > 0 THEN DO:
            ASSIGN akervardtemp.KRONOR = (-1) * akervardtemp.KRONOR.
         END.   
         akervardtemp.FASTPRIS = TRUE.
      END.                
      RUN sparaaker IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,INPUT stpnr,INPUT TABLE akervardtemp).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg4_UI DIALOG-1 
PROCEDURE btnreg4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF val = 4 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKAB
      ASSIGN   
      musz = FALSE        
      FILL-IN_FLKAB     = INPUT FRAME {&FRAME-NAME} FILL-IN_FLKAB  
      FILL-IN_LAKAB1    = INPUT FILL-IN_LAKAB1     
      FILL-IN_LAKAB2    = INPUT FILL-IN_LAKAB2 
      FILL-IN_FASTPRIS  = INPUT FILL-IN_FASTPRIS
      FILL-IN_KRONOR    = INPUT FILL-IN_KRONOR
      FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING.
      /*borttaget 20140425 lena finns inte i några avtal längre
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "celpa" THEN DO:      
         IF omravd = 3 THEN DO:      
            TOG_LSP = INPUT TOG_LSP.    
         END.
      END.*/
/*      IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "elpa" THEN DO:               
         TOG_LSP = INPUT TOG_LSP.             
         TOG_SAM = INPUT TOG_SAM.             
      END.*/

      IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "ELPA" THEN val = val.
      ELSE IF FILL-IN_LAKAB1 = 0 AND FILL-IN_FASTPRIS = FALSE THEN DO:
         MESSAGE "Längd kan inte vara blank!" VIEW-AS ALERT-BOX. 
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LAKAB1 IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDAKAB
      IF FILL-IN_FLKAB = TRUE AND FILL-IN_LAKAB2 = 0 THEN DO:
         MESSAGE "Bredd kan inte vara blank!" VIEW-AS ALERT-BOX.
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LAKAB2 IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDAKAB
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:   
         IF FILL-IN_FLKAB = TRUE AND FILL-IN_LAKAB2 LE 1 THEN DO:
            MESSAGE "Bredd skall inte registreras om den är mindre än 1 meter!" VIEW-AS ALERT-BOX.   
            musz = TRUE.
            APPLY "ENTRY" TO FILL-IN_LAKAB2 IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
      END.
      &Scoped-define FRAME-NAME FRAME-ANDAKAB    
      CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKLOG[1] = FILL-IN_FLKAB
      valsoktemp.SOKINT[1] = FILL-IN_LAKAB1
      valsoktemp.SOKDECI[1] = FILL-IN_LAKAB2 
      valsoktemp.SOKLOG[2] = FILL-IN_FASTPRIS
      valsoktemp.SOKINT[2] = FILL-IN_KRONOR 
      valsoktemp.SOKCHAR[1] = FILL-IN_BENAMNING.
      /*borttaget 20140425 lena finns inte i några avtal längre
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "celpa" THEN DO:      
         IF omravd = 3 THEN DO:      
            valsoktemp.SOKLOG[3] = TOG_LSP.
         END.
      END.*/
      
      /*IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "elpa" THEN DO:               
       /*  valsoktemp.SOKLOG[3] = TOG_LSP.*/        
         valsoktemp.SOKLOG[4] = TOG_SAM.        
      END.*/
      RUN sparakab IN andvardspecapph (INPUT vart,INPUT valvardnr,INPUT fastighbet,
                                       INPUT-OUTPUT akerkabrec2,
                                       INPUT TABLE valsoktemp,
                                       INPUT-OUTPUT TABLE akerkabtemp,
                                       INPUT-OUTPUT TABLE akervardtemp).     
   END.
   IF val = 5 OR val = 7 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO
      ASSIGN   
      musz = FALSE    
      FILL-IN_STOLPNR = INPUT FRAME {&FRAME-NAME} FILL-IN_STOLPNR 
      FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING
      /*FILL-IN_BETECKNING = INPUT FILL-IN_BETECKNING */ 
      FILL-IN_LTRAFO1 = INPUT FILL-IN_LTRAFO1     
      FILL-IN_LTRAFO2 = INPUT FILL-IN_LTRAFO2       
      /*FILL-IN_LTRAFO4 = INPUT FILL-IN_LTRAFO4   
      FILL-IN-RITNING = INPUT FILL-IN-RITNING*/
      FILL-IN_KRONOR = INPUT FILL-IN_KRONOR.
      /*IF Guru.Konstanter:globforetag = "UMEA" THEN.
      ELSE DO:      
         TOG_KSK = INPUT TOG_KSK.             
      END.*/
      IF FILL-IN_STOLPNR = 0 AND TOG_TILLFSKADA = FALSE THEN DO:
         MESSAGE "Nr kan inte vara blank!" VIEW-AS ALERT-BOX.   
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO
      /*IF FILL-IN_LTRAFO2 > FILL-IN_LTRAFO1 THEN DO:
         MESSAGE "Bredden kan inte vara större än längden!" VIEW-AS ALERT-BOX. 
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_LTRAFO2 IN FRAME {&FRAME-NAME}.
         RETURN.
      END.*/  
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO
      IF vart = "NYA" THEN DO:
         RUN stpnrcheck IN andvardspecapph (INPUT val,INPUT valvardnr,INPUT fastighbet,INPUT FILL-IN_STOLPNR,OUTPUT TABLE felmeddtemp). 
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX. 
            DELETE felmeddtemp.
            musz = TRUE.
            APPLY "ENTRY" TO FILL-IN_STOLPNR IN FRAME {&FRAME-NAME}.
            RETURN.   
         END.               
      END.  
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO      
      stpnr = FILL-IN_STOLPNR.      
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
      akervardtemp.BETECKNING = fastighbet AND 
      akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 1 NO-LOCK NO-ERROR.
      IF AVAILABLE akervardtemp THEN DO:
         ASSIGN 
         akervardtemp.STOLPNR = FILL-IN_STOLPNR
         akervardtemp.BETECKNING = FILL-IN_BETECKNING
         akervardtemp.BENAMNING = FILL-IN_BENAMNING
         /* RITNING ANVÄNDS EJ 20140428 LENA      
         SUBSTRING(akervardtemp.BENAMNING,60,20) = FILL-IN-RITNING*/
         akervardtemp.L1 = FILL-IN_LTRAFO1
         akervardtemp.L2 = FILL-IN_LTRAFO2         
         akervardtemp.L3 = FILL-IN_LTRAFO3
         akervardtemp.L5 = 1
         akervardtemp.FASTPRIS = TRUE
         akervardtemp.KRONOR = FILL-IN_KRONOR
         bplus = 0
         lplus = 0
         akervardtemp.L4 = 0.
         IF Guru.Konstanter:globforetag = "UMEA" THEN.
         ELSE DO:                          
            FIND FIRST stnksktemp WHERE stnksktemp.TYP = 1 AND stnksktemp.PRIS = akervardtemp.L2 NO-ERROR.
            IF AVAILABLE stnksktemp THEN ASSIGN akervardtemp.L4 = 1.               
            /*IF TOG_KSK = TRUE THEN ASSIGN akervardtemp.L4 = 1.*/                           
         END.
         IF TOG_TILLFSKADA = TRUE THEN DO:
            ASSIGN
            akervardtemp.STOLPNR = 0
            stpnr = 0 
            FILL-IN_STOLPNR = 0 .            
            RUN sparatillfskada IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,INPUT stpnr,INPUT TABLE akervardtemp). 
         END.
         ELSE DO:         
            RUN sparatrafo IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,INPUT stpnr,INPUT TABLE akervardtemp). 
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg6_UI DIALOG-1 
PROCEDURE btnreg6_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   &Scoped-define FRAME-NAME FRAME-REV
   ASSIGN   
   musz = FALSE    
   CMB_ZON = INPUT FRAME {&FRAME-NAME} CMB_ZON     
   FILL-IN-METER = INPUT FILL-IN-METER.        
   
   IF vart = "NYA" THEN DO:         
      RUN revcheck IN andvardspecapph (INPUT val,INPUT valvardnr,INPUT fastighbet,INPUT CMB_ZON,OUTPUT TABLE felmeddtemp). 
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX. 
         DELETE felmeddtemp.
         musz = TRUE.
         APPLY "ENTRY" TO CMB_ZON IN FRAME {&FRAME-NAME}.
         RETURN.   
      END.               
   END.     
   stpnr = CMB_ZON.      
   IF vart = "NYA" THEN DO:         
      FIND FIRST revtemp WHERE revtemp.VARDNR = valvardnr AND 
      revtemp.BETECKNING = fastighbet AND 
      revtemp.ZON = stpnr NO-ERROR.
      IF NOT AVAILABLE revtemp THEN DO:
         FIND FIRST revtemp WHERE revtemp.VARDNR = valvardnr AND 
         revtemp.BETECKNING = fastighbet  NO-ERROR.
         IF AVAILABLE revtemp THEN DO:
            revtemp.ZON = stpnr.
         END.
      END.
   END.
    
   FIND FIRST revtemp WHERE revtemp.VARDNR = valvardnr AND 
   revtemp.BETECKNING = fastighbet AND 
   revtemp.ZON = stpnr  NO-LOCK NO-ERROR.
   IF AVAILABLE revtemp THEN DO:
      ASSIGN 
      revtemp.BETECKNING = FILL-IN_BETECKNING      
      revtemp.ZON = CMB_ZON      
      revtemp.KRPM = FILL-IN-KRM
      revtemp.METER = FILL-IN-METER.            
      revtemp.KRONOR = revtemp.METER * revtemp.KRPM.      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "REVAVTAL"                      
      inextradatatemp.HUVUDINT = revtemp.VARDNR
      inextradatatemp.HUVUDCH = revtemp.BETECKNING
      inextradatatemp.SOKINT[1] = revtemp.ZON
      inextradatatemp.SOKDEC[1] = revtemp.KRPM
      inextradatatemp.SOKDEC[2] = revtemp.METER
      inextradatatemp.SOKDEC[3] = revtemp.KRONOR.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   END.
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg_UI DIALOG-1 
PROCEDURE btnreg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF val = 1 THEN DO: 
      &Scoped-define FRAME-NAME FRAME-ANDSKOG
      ASSIGN   
      musz = FALSE  
      FILL-IN_BESTAND = INPUT FRAME {&FRAME-NAME} FILL-IN_BESTAND 
      FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING 
      FILL-IN_LANGD = INPUT FILL-IN_LANGD  
      FILL-IN_BREDD = INPUT FILL-IN_BREDD 
      FILL-IN-PROCENT = INPUT FILL-IN-PROCENT
      CMB_MARKTYP = INPUT CMB_MARKTYP 
      CMB_BESTTYP = INPUT CMB_BESTTYP 
      CMB_BARRLOV = INPUT CMB_BARRLOV 
      CMB_SLENH = INPUT CMB_SLENH       
      FILL-IN_FASTPRIS = INPUT FILL-IN_FASTPRIS
      FILL-IN_KRONOR = INPUT FILL-IN_KRONOR
      CMB_MLANGD = INPUT CMB_MLANGD. 
      TOG_AVTST = INPUT TOG_AVTST. 
      CMB_OMRSKOG = INPUT CMB_OMRSKOG. 
      RUN hmtkrha_UI IN andvardspecapph (INPUT CMB_OMRSKOG, INPUT Guru.Konstanter:globforetag, OUTPUT skogkrha).
      IF FILL-IN-PROCENT < 100 AND FILL-IN-PROCENT > 0 THEN DO:
         ASSIGN
         FILL-IN-PROCENT-2 = INPUT FILL-IN-PROCENT-2
         CMB_MARKTYP-2 = INPUT CMB_MARKTYP-2 
         CMB_BESTTYP-2 = INPUT CMB_BESTTYP-2 
         CMB_BARRLOV-2 = INPUT CMB_BARRLOV-2.          
      END.
      IF FILL-IN_BESTAND = 0 THEN DO:
         MESSAGE "Bestånd kan inte vara blank!" VIEW-AS ALERT-BOX.
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_BESTAND IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      IF CMB_MLANGD > 0 THEN DO:      
         FIND FIRST volskogtemp  WHERE volskogtemp.VARDNR = valvardnr AND
         volskogtemp.BETECKNING = fastighbet AND volskogtemp.DIAM GE 0 USE-INDEX VARDNR NO-LOCK NO-ERROR.
         IF AVAILABLE volskogtemp  THEN DO:
            MESSAGE "Volymberäkning för fastigheten är gjord via volymberäkning och går ej att göra här också. Sätt medelhöjen till 0 " VIEW-AS ALERT-BOX.            
            musz = TRUE.
            APPLY "ENTRY" TO CMB_MLANGD IN FRAME {&FRAME-NAME}.
            RETURN.
         END.
      END.
      IF TOG_AVTST = TRUE THEN DO:
          ASSIGN FILL-IN-PROCENT = 100 FILL-IN-PROCENT-2 = 0.
      END.
      IF TOG_AVTST = TRUE AND FILL-IN_FASTPRIS = TRUE  THEN DO:
         MESSAGE "För Storskog går det ej att lägga in fasta belopp " VIEW-AS ALERT-BOX.            
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_FASTPRIS IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
      IF TOG_AVTST = FALSE  AND FILL-IN_FASTPRIS = FALSE  THEN DO:
          /*UMEÅ har fortfarande förenklad skogsnorm Ture 20150528*/
         IF Guru.Konstanter:globforetag = "UMEA" THEN .
         ELSE IF Guru.Konstanter:globforetag = "KRIN" THEN .
         ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN .
         ELSE DO:
            MESSAGE "För Förenklad skogsnorm går det bara att lägga in fasta belopp " VIEW-AS ALERT-BOX.            
            musz = TRUE.
            APPLY "ENTRY" TO FILL-IN_FASTPRIS IN FRAME {&FRAME-NAME}.
            RETURN.
         END.   
      END.
      IF TOG_AVTST = FALSE  AND FILL-IN_FASTPRIS = TRUE AND FILL-IN_KRONOR = 0  THEN DO:
         MESSAGE "Fasta beloppet måste vara mer än 0 kronor " VIEW-AS ALERT-BOX.            
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN_KRONOR IN FRAME {&FRAME-NAME}.
         RETURN.
      END.    
      IF vart = "NYA" THEN DO:
         /*Kontroll om bestandet redan finns upplagt*/
         RUN skogkoll IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,INPUT FILL-IN_BESTAND,
                                       OUTPUT TABLE felmeddtemp).
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            musz = TRUE.
            APPLY "ENTRY" TO FILL-IN_BESTAND IN FRAME {&FRAME-NAME}. 
            RETURN.
         END.
         bestandnr = FILL-IN_BESTAND.
      END.
      
      
      &Scoped-define FRAME-NAME FRAME-ANDSKOG
      FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
      skogvardtemp.BETECKNING = fastighbet AND 
      skogvardtemp.BESTAND = bestandnr NO-LOCK NO-ERROR.    
      IF AVAILABLE skogvardtemp THEN DO:
         ASSIGN 
         skogvardtemp.BESTAND   = FILL-IN_BESTAND 
         skogvardtemp.BENAMNING = FILL-IN_BENAMNING
         skogvardtemp.LANGD     = FILL-IN_LANGD 
         skogvardtemp.BREDD     = FILL-IN_BREDD 
         skogvardtemp.HA        = FILL-IN_HA  
         skogvardtemp.BARRLOV   = CMB_BARRLOV                
         skogvardtemp.SLENH     = CMB_SLENH
         skogvardtemp.PROCENT   = FILL-IN-PROCENT. 
         
         
         ASSIGN 
         skogvardtemp.FASTPRIS = FILL-IN_FASTPRIS.
         IF CMB_MARKTYP BEGINS "I   =" THEN ASSIGN skogvardtemp.MARKTYP = "I".
         ELSE IF CMB_MARKTYP BEGINS "II  =" THEN ASSIGN skogvardtemp.MARKTYP = "II".
         ELSE IF CMB_MARKTYP BEGINS "III =" THEN ASSIGN skogvardtemp.MARKTYP = "III".
         ELSE IF CMB_MARKTYP BEGINS "Imp =" THEN ASSIGN skogvardtemp.MARKTYP = "Imp".
         IF CMB_BESTTYP BEGINS "1" THEN ASSIGN skogvardtemp.BESTTYP = "1".
         ELSE IF CMB_BESTTYP BEGINS "2" THEN ASSIGN skogvardtemp.BESTTYP = "2".
         ELSE IF CMB_BESTTYP BEGINS "3" THEN ASSIGN skogvardtemp.BESTTYP = "3".
         ELSE IF CMB_BESTTYP BEGINS "4" THEN ASSIGN skogvardtemp.BESTTYP = "4".
         ELSE IF CMB_BESTTYP BEGINS "5A" THEN ASSIGN skogvardtemp.BESTTYP = "5A".
         ELSE IF CMB_BESTTYP BEGINS "5B" THEN ASSIGN skogvardtemp.BESTTYP = "5B".      
         IF TOG_AVTST = TRUE THEN skogvardtemp.MARKTYP = CMB_OMRSKOG.
         RUN skogv_UI.
         IF vart = "NYA" AND FILL-IN-PROCENT < 100 AND FILL-IN-PROCENT > 0 THEN DO:
            CREATE skogvardtemp.
            ASSIGN 
            skogvardtemp.VARDNR = varderingtemp.VARDNR
            skogvardtemp.BETECKNING = fastighettemp.BETECKNING. 
            skogvardtemp.BESTAND = bestandnr.
            ASSIGN          
            skogvardtemp.BENAMNING = FILL-IN_BENAMNING
            skogvardtemp.LANGD     = FILL-IN_LANGD 
            skogvardtemp.BREDD     = FILL-IN_BREDD 
            skogvardtemp.HA        = FILL-IN_HA-2  
            skogvardtemp.BARRLOV   = CMB_BARRLOV-2                
            skogvardtemp.SLENH     = CMB_SLENH
            skogvardtemp.PROCENT   = FILL-IN-PROCENT-2.          
            ASSIGN 
            skogvardtemp.FASTPRIS = FILL-IN_FASTPRIS.
            IF CMB_MARKTYP-2 BEGINS "I   =" THEN ASSIGN skogvardtemp.MARKTYP = "I".
            ELSE IF CMB_MARKTYP-2 BEGINS "II  =" THEN ASSIGN skogvardtemp.MARKTYP = "II".
            ELSE IF CMB_MARKTYP-2 BEGINS "III =" THEN ASSIGN skogvardtemp.MARKTYP = "III".
            ELSE IF CMB_MARKTYP-2 BEGINS "Imp =" THEN ASSIGN skogvardtemp.MARKTYP = "Imp".
            IF CMB_BESTTYP-2 BEGINS "1" THEN ASSIGN skogvardtemp.BESTTYP = "1".
            ELSE IF CMB_BESTTYP-2 BEGINS "2" THEN ASSIGN skogvardtemp.BESTTYP = "2".
            ELSE IF CMB_BESTTYP-2 BEGINS "3" THEN ASSIGN skogvardtemp.BESTTYP = "3".
            ELSE IF CMB_BESTTYP-2 BEGINS "4" THEN ASSIGN skogvardtemp.BESTTYP = "4".
            ELSE IF CMB_BESTTYP-2 BEGINS "5A" THEN ASSIGN skogvardtemp.BESTTYP = "5A".
            ELSE IF CMB_BESTTYP-2 BEGINS "5B" THEN ASSIGN skogvardtemp.BESTTYP = "5B".      
            IF TOG_AVTST = TRUE THEN skogvardtemp.MARKTYP = CMB_OMRSKOG.
            RUN skogv_UI.
         END.
         ELSE IF vart = "AND" AND FILL-IN-PROCENT < 100 AND FILL-IN-PROCENT > 0 THEN DO:
            FIND NEXT skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
            skogvardtemp.BETECKNING = fastighbet AND 
            skogvardtemp.BESTAND = bestandnr NO-LOCK NO-ERROR.   
            IF AVAILABLE skogvardtemp  THEN DO:
               ASSIGN          
               skogvardtemp.BENAMNING = FILL-IN_BENAMNING
               skogvardtemp.LANGD     = FILL-IN_LANGD 
               skogvardtemp.BREDD     = FILL-IN_BREDD 
               skogvardtemp.HA        = FILL-IN_HA-2  
               skogvardtemp.BARRLOV   = CMB_BARRLOV-2                
               skogvardtemp.SLENH     = CMB_SLENH
               skogvardtemp.PROCENT   = FILL-IN-PROCENT-2.          
               ASSIGN 
               skogvardtemp.FASTPRIS = FILL-IN_FASTPRIS.
               IF CMB_MARKTYP-2 BEGINS "I   =" THEN ASSIGN skogvardtemp.MARKTYP = "I".
               ELSE IF CMB_MARKTYP-2 BEGINS "II  =" THEN ASSIGN skogvardtemp.MARKTYP = "II".
               ELSE IF CMB_MARKTYP-2 BEGINS "III =" THEN ASSIGN skogvardtemp.MARKTYP = "III".
               ELSE IF CMB_MARKTYP-2 BEGINS "Imp =" THEN ASSIGN skogvardtemp.MARKTYP = "Imp".
               IF CMB_BESTTYP-2 BEGINS "1" THEN ASSIGN skogvardtemp.BESTTYP = "1".
               ELSE IF CMB_BESTTYP-2 BEGINS "2" THEN ASSIGN skogvardtemp.BESTTYP = "2".
               ELSE IF CMB_BESTTYP-2 BEGINS "3" THEN ASSIGN skogvardtemp.BESTTYP = "3".
               ELSE IF CMB_BESTTYP-2 BEGINS "4" THEN ASSIGN skogvardtemp.BESTTYP = "4".
               ELSE IF CMB_BESTTYP-2 BEGINS "5A" THEN ASSIGN skogvardtemp.BESTTYP = "5A".
               ELSE IF CMB_BESTTYP-2 BEGINS "5B" THEN ASSIGN skogvardtemp.BESTTYP = "5B".      
               IF TOG_AVTST = TRUE THEN skogvardtemp.MARKTYP = CMB_OMRSKOG.
               RUN skogv_UI.
            END.
            ELSE DO:
               CREATE skogvardtemp.
               ASSIGN 
               skogvardtemp.VARDNR = varderingtemp.VARDNR
               skogvardtemp.BETECKNING = fastighettemp.BETECKNING. 
               skogvardtemp.BESTAND = bestandnr.
               ASSIGN          
               skogvardtemp.BENAMNING = FILL-IN_BENAMNING
               skogvardtemp.LANGD     = FILL-IN_LANGD 
               skogvardtemp.BREDD     = FILL-IN_BREDD 
               skogvardtemp.HA        = FILL-IN_HA-2  
               skogvardtemp.BARRLOV   = CMB_BARRLOV-2                
               skogvardtemp.SLENH     = CMB_SLENH
               skogvardtemp.PROCENT   = FILL-IN-PROCENT-2.          
               ASSIGN 
               skogvardtemp.FASTPRIS = FILL-IN_FASTPRIS.
               IF CMB_MARKTYP-2 BEGINS "I   =" THEN ASSIGN skogvardtemp.MARKTYP = "I".
               ELSE IF CMB_MARKTYP-2 BEGINS "II  =" THEN ASSIGN skogvardtemp.MARKTYP = "II".
               ELSE IF CMB_MARKTYP-2 BEGINS "III =" THEN ASSIGN skogvardtemp.MARKTYP = "III".
               ELSE IF CMB_MARKTYP-2 BEGINS "Imp =" THEN ASSIGN skogvardtemp.MARKTYP = "Imp".
               IF CMB_BESTTYP-2 BEGINS "1" THEN ASSIGN skogvardtemp.BESTTYP = "1".
               ELSE IF CMB_BESTTYP-2 BEGINS "2" THEN ASSIGN skogvardtemp.BESTTYP = "2".
               ELSE IF CMB_BESTTYP-2 BEGINS "3" THEN ASSIGN skogvardtemp.BESTTYP = "3".
               ELSE IF CMB_BESTTYP-2 BEGINS "4" THEN ASSIGN skogvardtemp.BESTTYP = "4".
               ELSE IF CMB_BESTTYP-2 BEGINS "5A" THEN ASSIGN skogvardtemp.BESTTYP = "5A".
               ELSE IF CMB_BESTTYP-2 BEGINS "5B" THEN ASSIGN skogvardtemp.BESTTYP = "5B".      
               IF TOG_AVTST = TRUE THEN skogvardtemp.MARKTYP = CMB_OMRSKOG.
               RUN skogv_UI.
            END.
         END.
         ELSE DO:
            /* om det inte längre är 2 delbestånd , ta bort det andra*/
            FIND NEXT skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
            skogvardtemp.BETECKNING = fastighbet AND 
            skogvardtemp.BESTAND = bestandnr NO-ERROR.   
            IF AVAILABLE skogvardtemp  THEN DO:
               DELETE skogvardtemp.
            END.
         END.         
         IF CMB_MLANGD = 0 THEN DO:
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "FSVOL"                   
            inextradatatemp.HUVUDINT = valvardnr
            inextradatatemp.HUVUDCH = fastighbet
            inextradatatemp.SOKINT[1] = bestandnr.                           
            RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).  
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR.            
         END.
         ELSE DO:         
            FIND FIRST voluppsk WHERE voluppsk.GRANTALL = "Tall" AND voluppsk.SLENH = CMB_SLENH AND
            voluppsk.MEDHOJD = CMB_MLANGD  NO-LOCK NO-ERROR.
            IF AVAILABLE  voluppsk THEN DO:
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FSVOL"                   
               inextradatatemp.HUVUDINT = valvardnr                    
               inextradatatemp.HUVUDCH = fastighbet                    
               inextradatatemp.SOKINT[1] = bestandnr
               inextradatatemp.SOKCHAR[1] = "Tall"
               inextradatatemp.SOKDEC[1] = CMB_SLENH 
               inextradatatemp.SOKINT[2] = CMB_MLANGD                  
               inextradatatemp.SOKINT[3] = voluppsk.TABELLV
               inextradatatemp.SOKDEC[2] = voluppsk.TABELLV * FILL-IN_LANGD * FILL-IN_BREDD / 10000.                                           
               inextradatatemp.SOKDEC[3] = FILL-IN_LANGD * FILL-IN_BREDD / 10000.                                           
               RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
            END.
            ELSE DO:
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "FSVOL"                   
               inextradatatemp.HUVUDINT = valvardnr
               inextradatatemp.HUVUDCH = fastighbet
               inextradatatemp.SOKINT[1] = bestandnr.                           
               RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).  
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR.            
            END.
         END.         
         DEBUGGER:SET-BREAK().
         RUN sparaskog IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,
                                           INPUT bestandnr,INPUT vart,INPUT TABLE skogvardtemp). 
   
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddtemp.
            MUSZ = TRUE.
            RETURN.
         END.
      END.
   END.   
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
  HIDE FRAME FRAME-ANDAKAB.
  HIDE FRAME FRAME-ANDAKER.
  HIDE FRAME FRAME-ANDSKOG.
  HIDE FRAME FRAME-ANDTRAFO.
  HIDE FRAME FRAME-ANDVOL.
  HIDE FRAME FRAME-REV.
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
  ENABLE BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
  DISPLAY FILL-IN_BETECKNING FILL-IN_LAKAB1 FILL-IN_FLKAB FILL-IN_LAKAB2 
          FILL-IN_FASTPRIS FILL-IN_KRONOR FILL-IN_BENAMNING 
      WITH FRAME FRAME-ANDAKAB.
  ENABLE FILL-IN_LAKAB1 FILL-IN_FLKAB FILL-IN_LAKAB2 FILL-IN_FASTPRIS 
         FILL-IN_KRONOR FILL-IN_BENAMNING BRW_KOMKAB 
      WITH FRAME FRAME-ANDAKAB.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDAKAB}
  DISPLAY FILL-IN_BETECKNING FILL-IN_STOLPNR CMB_SORT FILL-IN_LAKER1 
          FILL-IN_LAKER2 FILL-IN_LAKER3 FILL-IN_BORTINT FILL-IN_FASTPRIS 
          FILL-IN_KRONOR FILL-IN_BENAMNING FILL-IN-VARNING 
      WITH FRAME FRAME-ANDAKER.
  ENABLE FILL-IN_STOLPNR CMB_SORT FILL-IN_LAKER1 FILL-IN_LAKER2 FILL-IN_LAKER3 
         FILL-IN_BORTINT FILL-IN_FASTPRIS FILL-IN_KRONOR FILL-IN_BENAMNING 
      WITH FRAME FRAME-ANDAKER.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDAKER}
  DISPLAY FILL-IN_BETECKNING FILL-IN_BESTAND FILL-IN_LANGD FILL-IN_BREDD 
          FILL-IN-PROCENT FILL-IN_HA CMB_MARKTYP CMB_BESTTYP CMB_BARRLOV 
          CMB_SLENH CMB_MLANGD FILL-IN_FASTPRIS FILL-IN_KRONOR FILL-IN_BENAMNING 
      WITH FRAME FRAME-ANDSKOG.
  ENABLE FILL-IN_BESTAND FILL-IN_LANGD FILL-IN_BREDD FILL-IN-PROCENT 
         FILL-IN-PROCENT-2 FILL-IN_HA FILL-IN_HA-2 CMB_MARKTYP CMB_MARKTYP-2 
         CMB_BESTTYP CMB_BESTTYP-2 CMB_BARRLOV CMB_BARRLOV-2 CMB_SLENH 
         CMB_MLANGD FILL-IN_FASTPRIS FILL-IN_KRONOR FILL-IN_BENAMNING TOG_AVTST 
         CMB_OMRSKOG 
      WITH FRAME FRAME-ANDSKOG.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDSKOG}
  DISPLAY FILL-IN_BETECKNING FILL-IN_STOLPNR FILL-IN_LTRAFO1 FILL-IN_LTRAFO2 
          FILL-IN_KRONOR FILL-IN_BENAMNING EDT_HJALPSTN 
      WITH FRAME FRAME-ANDTRAFO.
  ENABLE BRW_KSKSTN FILL-IN_STOLPNR FILL-IN_LTRAFO1 FILL-IN_LTRAFO2 
         FILL-IN_KRONOR BRW_KOMTR FILL-IN_BENAMNING 
      WITH FRAME FRAME-ANDTRAFO.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDTRAFO}
  DISPLAY FILL-IN_OKORR FILL-IN_BETECKNING CMB_THKL CMB_GHKL CMB_BHKL CMB_OHKL 
          CMB_DIAM FILL-IN_TANT FILL-IN_GANT FILL-IN_BANT FILL-IN_OANT 
      WITH FRAME FRAME-ANDVOL.
  ENABLE FILL-IN_OKORR CMB_THKL CMB_GHKL CMB_BHKL CMB_OHKL CMB_DIAM 
         FILL-IN_TANT BRW_STAMP FILL-IN_GANT FILL-IN_BANT FILL-IN_OANT 
      WITH FRAME FRAME-ANDVOL.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDVOL}
  DISPLAY FILL-IN_BETECKNING CMB_ZON FILL-IN-KRM FILL-IN-METER 
      WITH FRAME FRAME-REV.
  ENABLE CMB_ZON FILL-IN-METER 
      WITH FRAME FRAME-REV.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-REV}
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
   IF val = 1 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDSKOG
      IF skogvardtemp.FASTPRIS = FALSE THEN DO:
         ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END. 
      ELSE DO:
         ASSIGN FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.  
   END.
   IF val = 2 THEN DO:
   END.
   IF val = 3 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      IF akervardtemp.SORT = "BETESMARK" OR akervardtemp.SORT = "IMPEDIMENT" THEN DO:
         ASSIGN  FILL-IN_LAKER3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ELSE DO:
         ASSIGN  FILL-IN_LAKER3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      IF akervardtemp.FASTPRIS = FALSE THEN DO:
         ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END. 
      ELSE DO:
         ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.  
   END.
   IF val = 4 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKAB      
      IF AVAILABLE akerkabtemp AND akerkabtemp.FASTPRIS = FALSE THEN DO:
         ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END. 
      ELSE DO:
         ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.  
      IF AVAILABLE akerkabtemp AND akerkabtemp.FLKAB = FALSE THEN DO:
         ASSIGN  FILL-IN_LAKAB2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END. 
      ELSE DO:
         ASSIGN  FILL-IN_LAKAB2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.  
      IF NOT AVAILABLE akerkabtemp THEN DO: 
         ASSIGN FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         ASSIGN  FILL-IN_LAKAB2:HIDDEN = TRUE.         
      END.
   END.
   IF val = 5 OR val = 7 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO
           
      /*FILL-IN_LTRAFO1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN_LTRAFO2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.*/
      /*FILL-IN_LTRAFO4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */
         
      ASSIGN  FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   FRAME FRAME-ANDSKOG:VISIBLE = FALSE.
   FRAME FRAME-ANDVOL:VISIBLE = FALSE.
   FRAME FRAME-ANDAKER:VISIBLE = FALSE.
   FRAME FRAME-ANDTRAFO:VISIBLE = FALSE.
   FRAME FRAME-ANDAKAB:VISIBLE = FALSE.
   FRAME FRAME-REV:VISIBLE = FALSE.
   /*andskog.w*/
   DEBUGGER:SET-BREAK().
   IF val = 1 THEN DO:      
      FIND FIRST voluppsk NO-ERROR.
      IF NOT AVAILABLE voluppsk THEN DO:
         {VOLMHOJD.I}         
      END.      
      &Scoped-define FRAME-NAME FRAME-ANDSKOG
      /*RUN laddaskog IN andvardspecapph (INPUT valvardnr,OUTPUT TABLE vardskogtemp,OUTPUT TABLE markdivtemp).*/
      RUN laddaskog2 IN andvardspecapph (INPUT valvardnr,OUTPUT TABLE vardskogtemp).
      FRAME FRAME-ANDSKOG:VISIBLE = TRUE.     
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr AND 
      fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.

      /* nystorskog 20140224*/
      /* tidigare "Norrlands inland","Norrlands kustland","Svealand","Götaland"*/
      status-ok = CMB_OMRSKOG:DELETE("Norrlands kustland").
      status-ok = CMB_OMRSKOG:DELETE("Svealand").
      status-ok = CMB_OMRSKOG:DELETE("Götaland").
      /*NYA 20140224*/
      status-ok = CMB_OMRSKOG:ADD-LAST("Norrlands kustland ej Gävleborgs län").
      status-ok = CMB_OMRSKOG:ADD-LAST("Tillväxtområde 3 (2009 års skognorm)").
      status-ok = CMB_OMRSKOG:ADD-LAST("Tillväxtområde 4A (2009 års skognorm)").
      status-ok = CMB_OMRSKOG:ADD-LAST("Tillväxtområde 4B (2009 års skognorm)").
       
      IF Guru.Konstanter:globforetag = "umea" THEN  CMB_OMRSKOG:SCREEN-VALUE = "Norrlands kustland ej Gävleborgs län".   
      ELSE CMB_OMRSKOG:SCREEN-VALUE = "Norrlands inland".      
       
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "cELPA"  THEN.
      ELSE DO:
         ASSIGN                  
         CMB_MLANGD:HIDDEN = TRUE.
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FSVOL"                   
      inextradatatemp.HUVUDINT = valvardnr                    
      inextradatatemp.HUVUDCH = fastighbet.                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp WHERE extradatatemp.SOKINT[1] = bestandnr NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN         
         CMB_MLANGD = extradatatemp.SOKINT[2].                     
      END.          
      ELSE DO:         
         ASSIGN         
         CMB_MLANGD = 0. 
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      FILL-IN-PROCENT-2:HIDDEN  = TRUE.
      CMB_MARKTYP-2:HIDDEN = TRUE.
      CMB_BESTTYP-2:HIDDEN = TRUE.
      CMB_BARRLOV-2:HIDDEN = TRUE.      
      FILL-IN_HA-2:HIDDEN = TRUE.                      
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.  
      IF vart = "NYA" THEN DO:
         RUN nybestandnr IN andvardspecapph (INPUT varderingtemp.VARDNR,INPUT fastighettemp.BETECKNING,OUTPUT bestandnr).   
         CREATE skogvardtemp.
         ASSIGN 
         skogvardtemp.VARDNR = varderingtemp.VARDNR
         skogvardtemp.BETECKNING = fastighettemp.BETECKNING. 
         skogvardtemp.BESTAND = bestandnr.
         ASSIGN FILL-IN_BESTAND = bestandnr.         
         /*ASSIGN FILL-IN_FASTPRIS = FALSE.*/
         /*Ändrat till att det alltid är FAST PRIS = TRUE  20140822 Lena*/ 
          IF Guru.Konstanter:globforetag = "UMEA" THEN ASSIGN FILL-IN_FASTPRIS = FALSE.
          ELSE IF Guru.Konstanter:globforetag = "KRIN" THEN ASSIGN FILL-IN_FASTPRIS = FALSE.
          ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN ASSIGN FILL-IN_FASTPRIS = FALSE.
          ELSE ASSIGN FILL-IN_FASTPRIS = TRUE. 
         /*UMEÅ har fortfarande förenklad skogsnorm Ture 20150528*/
         
         skogvardtemp.FASTPRIS = FILL-IN_FASTPRIS. 
         ASSIGN  CMB_MARKTYP:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "I   = mycket god".
         ASSIGN  CMB_BESTTYP:SCREEN-VALUE = "1 = plantskog".         
      END.   
      ELSE DO:                    
         FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
         skogvardtemp.BETECKNING = fastighbet AND 
         skogvardtemp.BESTAND = bestandnr NO-LOCK NO-ERROR.    
         ASSIGN
         FILL-IN_BESTAND = skogvardtemp.BESTAND
         FILL-IN_BENAMNING =  skogvardtemp.BENAMNING
         FILL-IN_LANGD = skogvardtemp.LANGD
         FILL-IN_BREDD = skogvardtemp.BREDD      
         CMB_BARRLOV = skogvardtemp.BARRLOV 
         CMB_SLENH = skogvardtemp.SLENH
         FILL-IN_HA = skogvardtemp.HA
         FILL-IN-PROCENT = skogvardtemp.PROCENT
         FILL-IN_FASTPRIS = skogvardtemp.FASTPRIS  
         FILL-IN_KRONOR = skogvardtemp.KRONOR.
         IF skogvardtemp.PROCENT = 0 THEN FILL-IN-PROCENT = 100.
         IF skogvardtemp.MARKTYP = "I" THEN ASSIGN  CMB_MARKTYP = "I   = mycket god".
         ELSE IF skogvardtemp.MARKTYP = "II" THEN ASSIGN  CMB_MARKTYP = "II  = medelgod".
         ELSE IF skogvardtemp.MARKTYP = "III" THEN ASSIGN  CMB_MARKTYP = "III = dålig".
         ELSE IF skogvardtemp.MARKTYP = "Imp" THEN ASSIGN  CMB_MARKTYP = "Imp = impediment".
         
         IF skogvardtemp.MARKTYP = "Norrlands inland" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Norrlands inland".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Norrlands kustland" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Norrlands kustland".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Svealand" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Svealand".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Götaland" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Götaland".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Norrlands kustland ej Gävleborgs län" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Norrlands kustland ej Gävleborgs län".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Tillväxtområde 3 (2009 års skognorm)" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Tillväxtområde 3 (2009 års skognorm)".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Tillväxtområde 4A (2009 års skognorm)" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Tillväxtområde 4A (2009 års skognorm)".
         END.
         ELSE IF skogvardtemp.MARKTYP = "Tillväxtområde 4B (2009 års skognorm)" THEN DO:
            ASSIGN TOG_AVTST = TRUE
            CMB_OMRSKOG = "Tillväxtområde 4B (2009 års skognorm)".
         END.
        
      
         IF skogvardtemp.BESTTYP  = "1" THEN ASSIGN CMB_BESTTYP = "1 = plantskog".
         ELSE IF skogvardtemp.BESTTYP  = "2" THEN ASSIGN CMB_BESTTYP = "2 = röjningsskog".
         ELSE IF skogvardtemp.BESTTYP  = "3" THEN ASSIGN CMB_BESTTYP = "3 = massavedsskog".
         ELSE IF skogvardtemp.BESTTYP  = "4" THEN ASSIGN CMB_BESTTYP = "4 = yngre timmerskog".
         ELSE IF skogvardtemp.BESTTYP  = "5a" THEN ASSIGN CMB_BESTTYP = "5a = äldre timmerskog".
         ELSE IF skogvardtemp.BESTTYP  = "5b" THEN ASSIGN CMB_BESTTYP = "5b = kalmark".         
         IF FILL-IN-PROCENT < 100 AND FILL-IN-PROCENT > 0 THEN DO:
            FIND NEXT skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
            skogvardtemp.BETECKNING = fastighbet AND 
            skogvardtemp.BESTAND = bestandnr NO-LOCK NO-ERROR.   
            IF AVAILABLE skogvardtemp  THEN DO:
               ASSIGN
               CMB_BARRLOV-2 = skogvardtemp.BARRLOV                
               FILL-IN_HA-2 = skogvardtemp.HA
               FILL-IN-PROCENT-2 = skogvardtemp.PROCENT.                              
               IF skogvardtemp.MARKTYP = "I" THEN ASSIGN  CMB_MARKTYP-2 = "I   = mycket god".
               ELSE IF skogvardtemp.MARKTYP = "II" THEN ASSIGN  CMB_MARKTYP-2 = "II  = medelgod".
               ELSE IF skogvardtemp.MARKTYP = "III" THEN ASSIGN  CMB_MARKTYP-2 = "III = dålig".
               ELSE IF skogvardtemp.MARKTYP = "Imp" THEN ASSIGN  CMB_MARKTYP-2 = "Imp = impediment".
               IF skogvardtemp.BESTTYP  = "1" THEN ASSIGN CMB_BESTTYP-2 = "1 = plantskog".
               ELSE IF skogvardtemp.BESTTYP  = "2" THEN ASSIGN CMB_BESTTYP-2 = "2 = röjningsskog".
               ELSE IF skogvardtemp.BESTTYP  = "3" THEN ASSIGN CMB_BESTTYP-2 = "3 = massavedsskog".
               ELSE IF skogvardtemp.BESTTYP  = "4" THEN ASSIGN CMB_BESTTYP-2 = "4 = yngre timmerskog".
               ELSE IF skogvardtemp.BESTTYP  = "5a" THEN ASSIGN CMB_BESTTYP-2 = "5a = äldre timmerskog".
               ELSE IF skogvardtemp.BESTTYP  = "5b" THEN ASSIGN CMB_BESTTYP-2 = "5b = kalmark".               
               FILL-IN-PROCENT-2:HIDDEN  = FALSE.
               CMB_MARKTYP-2:HIDDEN = FALSE.
               CMB_BESTTYP-2:HIDDEN = FALSE.
               CMB_BARRLOV-2:HIDDEN = FALSE.               
               FILL-IN_HA-2:HIDDEN = FALSE.                               
               DISPLAY FILL-IN-PROCENT-2 CMB_MARKTYP-2 CMB_BESTTYP-2 CMB_BARRLOV-2 FILL-IN_HA FILL-IN_HA-2 WITH FRAME FRAME-ANDSKOG.
            END.
         END.        
      END.   
      DISPLAY FILL-IN_BETECKNING FILL-IN_BESTAND FILL-IN_BENAMNING FILL-IN_LANGD FILL-IN_BREDD FILL-IN-PROCENT FILL-IN_FASTPRIS 
      FILL-IN_KRONOR CMB_MARKTYP CMB_BESTTYP CMB_BARRLOV CMB_SLENH FILL-IN_HA WITH FRAME FRAME-ANDSKOG.      
      IF Guru.Konstanter:globforetag = "cELPA" THEN DISPLAY  CMB_MLANGD WITH FRAME FRAME-ANDSKOG.            
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" /*AND Guru.Konstanter:globanv = "THLE"*/ THEN DISPLAY  CMB_MLANGD WITH FRAME FRAME-ANDSKOG.            
      /*storskog*/
      IF varforetypval[42] = 1 THEN DO:            
         DISP TOG_AVTST CMB_OMRSKOG WITH FRAME FRAME-ANDSKOG.
      END.
      RUN stskog_UI.
      IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         /*UMEÅ har fortfarande förenklad skogsnorm Ture 20150528*/
         IF TOG_AVTST = TRUE THEN DO:
            CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
            FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            FILL-IN-PROCENT = 100.
         END.
         ELSE DO:
            CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
            FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.  
      END.   
   END.   
   IF val = 2 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDVOL
      RUN laddavol IN andvardspecapph (INPUT vart,INPUT valvardnr,INPUT fastighbet,
                                       INPUT-OUTPUT volskogtabrec,
                                       OUTPUT TABLE volymbertemp,
                                       OUTPUT TABLE volkrtemp).
      FRAME FRAME-ANDVOL:VISIBLE = TRUE.
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR  = valvardnr AND fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.  
      IF vart = "NYA" THEN DO:
         CREATE volskogtemp.
         ASSIGN 
         volskogtemp.VARDNR = valvardnr
         volskogtemp.BETECKNING = fastighbet
         volskogtemp.VOLSKOGREC = volskogtabrec.
         ASSIGN FILL-IN_OKORR = 100.  
         ASSIGN  CMB_THKL:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "T1".
         ASSIGN  CMB_GHKL:SCREEN-VALUE = "G1".
         ASSIGN  CMB_BHKL:SCREEN-VALUE = "T1".   
         ASSIGN  CMB_OHKL:SCREEN-VALUE = "T1".        
         ASSIGN  CMB_DIAM:SCREEN-VALUE  = "8".
         CMB_DIAM = INPUT CMB_DIAM. 
      END.   
      ELSE DO:        
         FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec NO-LOCK NO-ERROR.    
         ASSIGN
         CMB_DIAM = volskogtemp.DIAM
         FILL-IN_TANT =  volskogtemp.TANT
         CMB_THKL = volskogtemp.THKL   
         FILL-IN_GANT = volskogtemp.GANT
         CMB_GHKL = volskogtemp.GHKL
         FILL-IN_BANT = volskogtemp.BANT
         CMB_BHKL = volskogtemp.BHKL
         FILL-IN_OANT = volskogtemp.OANT
         CMB_OHKL = volskogtemp.OHKL
         FILL-IN_OKORR = volskogtemp.OKORR.
      END.     
      DISPLAY FILL-IN_BETECKNING CMB_DIAM FILL-IN_TANT CMB_THKL FILL-IN_GANT 
      CMB_GHKL FILL-IN_BANT CMB_BHKL FILL-IN_OANT CMB_OHKL FILL-IN_OKORR 
      WITH FRAME FRAME-ANDVOL.
   END.
   /*andaker.w*/
   IF val = 3 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKER
      FRAME FRAME-ANDAKER:VISIBLE = TRUE.
      /*RUN laddaaker IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,
                                       OUTPUT TABLE prodakertemp,
                                       OUTPUT TABLE markdivtemp,
                                       OUTPUT TABLE akerproctemp,
                                       OUTPUT TABLE kindextemp).*/
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR  = valvardnr AND fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.  
      FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
      akervardtemp.BETECKNING = fastighbet AND akervardtemp.L5 = 0 AND akervardtemp.STOLPNR NE 0  NO-LOCK NO-ERROR.      
      IF vart = "NYA" THEN DO:
         RUN hmtakernr IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,OUTPUT stpnr).
         CREATE akervardtemp.
         ASSIGN 
         akervardtemp.VARDNR = valvardnr 
         akervardtemp.BETECKNING = fastighbet
         akervardtemp.STOLPNR = stpnr
         akervardtemp.L5 = 0.
         imon = INTEGER(SUBSTR(vardetidvar,3,2)).  
         iyr = INTEGER(SUBSTRING(vardetidvar,1,2)).
         IF iyr > 50 THEN ASSIGN  iyr = iyr + 1900.     
         ELSE ASSIGN  iyr = iyr + 2000.                 
         ASSIGN akervardtemp.VARDEPKT = DATE(imon,01,iyr).  
         ASSIGN 
         FILL-IN_STOLPNR = stpnr
         FILL-IN_FASTPRIS = FALSE.    
         ASSIGN  CMB_SORT:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "ÅKER". 
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND 
         akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 0 NO-LOCK NO-ERROR.
      END.   
      ELSE DO:                
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND
         akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 0 NO-LOCK NO-ERROR.      
         ASSIGN
         FILL-IN_STOLPNR = akervardtemp.STOLPNR
         FILL-IN_BENAMNING =  SUBSTRING(akervardtemp.BENAMNING,1,60)           
         FILL-IN_LAKER1 = akervardtemp.L1
         FILL-IN_LAKER2 = akervardtemp.L2
         FILL-IN_LAKER3 = akervardtemp.L3
         FILL-IN_LAKER4 = akervardtemp.L4
         FILL-IN_LAKER5 = akervardtemp.L5
         CMB_SORT = akervardtemp.SORT
         FILL-IN_FASTPRIS = akervardtemp.FASTPRIS
         FILL-IN_KRONOR = akervardtemp.KRONOR.
         IF FILL-IN_KRONOR < 0 THEN FILL-IN_BORTINT = TRUE.      
      END.   
      DISPLAY FILL-IN_BETECKNING FILL-IN_STOLPNR CMB_SORT FILL-IN_LAKER1 
      FILL-IN_LAKER2 FILL-IN_LAKER3 FILL-IN_BORTINT FILL-IN_FASTPRIS 
      FILL-IN_KRONOR FILL-IN_BENAMNING WITH FRAME FRAME-ANDAKER.
   END.
   IF val = 4 THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDAKAB
      FRAME FRAME-ANDAKAB:VISIBLE = TRUE.
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR  = valvardnr AND fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "UMEA"  THEN DO:            
         ASSIGN   FILL-IN_FLKAB:LABEL IN FRAME {&FRAME-NAME} = "> 1m mellan yttersta kablarna" .               
      END.
      ELSE DO:           
         ASSIGN   FILL-IN_FLKAB:LABEL IN FRAME {&FRAME-NAME} = "Tillägg bredd > 1m mellan yttersta kablarna" .               
      END.
      IF vart = "NYA" THEN DO:  
         ASSIGN FILL-IN_FLKAB = FALSE.
         ASSIGN FILL-IN_FASTPRIS = FALSE.           
      END.   
      ELSE DO:           
         FIND FIRST akerkabtemp WHERE akerkabtemp.AKERKABREC = akerkabrec2 NO-LOCK NO-ERROR. 
         IF AVAILABLE akerkabtemp THEN DO:
            ASSIGN             
            FILL-IN_LAKAB1 = akerkabtemp.L1
            FILL-IN_LAKAB2 = akerkabtemp.L2
            FILL-IN_FLKAB = akerkabtemp.FLKAB    
            FILL-IN_FASTPRIS = akerkabtemp.FASTPRIS
            FILL-IN_KRONOR = akerkabtemp.KRONOR.                 
            IF akerkabtemp.FASTPRIS = FALSE THEN DO: 
               FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr 
               AND akervardtemp.BETECKNING = fastighbet AND akervardtemp.L5 = akerkabtemp.L1
               AND akervardtemp.L1 = 0 AND akervardtemp.L2 = 0 AND akervardtemp.L3 = 0 
               AND akervardtemp.STOLPNR = 0 NO-LOCK NO-ERROR. 
            END.
            ELSE DO:
               FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr 
               AND akervardtemp.BETECKNING = fastighbet AND akervardtemp.L5 = akerkabtemp.KRONOR
               AND akervardtemp.L1 = 0 AND akervardtemp.L2 = 0 AND akervardtemp.L3 = 0 
               AND akervardtemp.STOLPNR = 0 NO-LOCK NO-ERROR. 
            END.
            IF AVAILABLE akervardtemp THEN DO:               
               ASSIGN FILL-IN_BENAMNING = TRIM(akervardtemp.BENAMNING).
               /* RITNING ANVÄNDS EJ 20140428 LENA
               FILL-IN-RITNING = SUBSTRING(akervardtemp.BENAMNING,60,20).*/
               /*borttaget 20140425 lena finns inte i några avtal längre               
               IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "celpa" THEN DO:      
                  IF omravd = 3 THEN DO:      
                     IF akervardtemp.L4 = 2 THEN TOG_LSP = TRUE.
                     ELSE TOG_LSP = FALSE.                                        
                  END.
               END.
               IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" 
               OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                        
                  IF akervardtemp.L4 = 2 THEN DO: 
                     TOG_LSP = TRUE.
                     TOG_SAM = FALSE.
                  END.
                  ELSE IF akervardtemp.L4 = 3 THEN DO: 
                     TOG_LSP = FALSE.
                     TOG_SAM = TRUE.
                  END.
                  ELSE IF akervardtemp.L4 = 4 THEN DO: 
                     TOG_LSP = TRUE.
                     TOG_SAM = TRUE.
                  END.
                  ELSE DO:
                      TOG_LSP = FALSE.
                      TOG_SAM = FALSE.
                  END.
               END.*/
            END.            
         END.         
      END.   
      DISPLAY FILL-IN_BETECKNING FILL-IN_LAKAB1 FILL-IN_FLKAB FILL-IN_LAKAB2 
      FILL-IN_FASTPRIS FILL-IN_KRONOR FILL-IN_BENAMNING WITH FRAME FRAME-ANDAKAB.
   END.
   IF  val = 5 OR val = 7  THEN DO:
      &Scoped-define FRAME-NAME FRAME-ANDTRAFO
      FRAME FRAME-ANDTRAFO:VISIBLE = TRUE.
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR  = valvardnr AND fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.  
      IF vart = "NYA" THEN DO:
         RUN hmttrafonr IN andvardspecapph (INPUT valvardnr,INPUT fastighbet,OUTPUT stpnr).
         CREATE akervardtemp.
         ASSIGN 
         akervardtemp.VARDNR = valvardnr
         akervardtemp.BETECKNING = fastighbet
         akervardtemp.STOLPNR = stpnr
         akervardtemp.L1 = 1
         akervardtemp.L2 = 0
         akervardtemp.L3 = 0         
         akervardtemp.L5 = 1.
         ASSIGN         
         FILL-IN_STOLPNR = stpnr
         FILL-IN_BETECKNING = fastighbet
         FILL-IN_LTRAFO1 = akervardtemp.L1
         FILL-IN_LTRAFO2 = akervardtemp.L2
         FILL-IN_LTRAFO3 = akervardtemp.L3. 
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND 
         akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 1 NO-LOCK NO-ERROR. 
      END.   
      ELSE DO:          
               
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND 
         akervardtemp.STOLPNR = stpnr AND akervardtemp.L5 = 1 NO-LOCK NO-ERROR.
         
         IF akervardtemp.L1 = 0 AND akervardtemp.L2 = 0 THEN DO:
            /*FIXA FÖR GAMLA REGISTRERINGAR SOM INTE HAR ANTAL OCH PRIS LENA 20151023*/
            IF akervardtemp.KRONOR > 0 THEN DO:
               ASSIGN
               akervardtemp.L1 = 1
               akervardtemp.L2 = akervardtemp.KRONOR.              
               IF akervardtemp.L4 = 1 THEN DO:                  
                  FIND FIRST stnksktemp WHERE stnksktemp.TYP = 1 NO-LOCK NO-ERROR.
                  IF AVAILABLE stnksktemp THEN DO:
                     IF INTEGER(akervardtemp.KRONOR / stnksktemp.PRIS) = akervardtemp.KRONOR / stnksktemp.PRIS THEN DO:
                        ASSIGN 
                        akervardtemp.L1 = akervardtemp.KRONOR / stnksktemp.PRIS  
                        akervardtemp.L2 = stnksktemp.PRIS
                        akervardtemp.L3 = stnksktemp.ID.
                     END.      
                  END.    
               END.
               ELSE DO:
                  FIND FIRST stnksktemp WHERE stnksktemp.TYP = 2 NO-LOCK NO-ERROR.
                  IF AVAILABLE stnksktemp THEN DO:
                     IF INTEGER(akervardtemp.KRONOR / stnksktemp.PRIS) = akervardtemp.KRONOR / stnksktemp.PRIS THEN DO:
                        ASSIGN 
                        akervardtemp.L1 = akervardtemp.KRONOR / stnksktemp.PRIS  
                        akervardtemp.L2 = stnksktemp.PRIS
                        akervardtemp.L3 = stnksktemp.ID.
                     END.                        
                     ELSE DO:
                        FIND FIRST stnksktemp WHERE stnksktemp.TYP = 3 NO-LOCK NO-ERROR.
                        IF AVAILABLE stnksktemp THEN DO:
                           IF INTEGER(akervardtemp.KRONOR / stnksktemp.PRIS) = akervardtemp.KRONOR / stnksktemp.PRIS THEN DO:
                              ASSIGN 
                              akervardtemp.L1 = akervardtemp.KRONOR / stnksktemp.PRIS  
                              akervardtemp.L2 = stnksktemp.PRIS
                              akervardtemp.L3 = stnksktemp.ID.
                           END.      
                        END.
                     END.
                  END.            
               END.   
            END.    
         END.
                  
         ASSIGN
         FILL-IN_STOLPNR = akervardtemp.STOLPNR
         FILL-IN_BETECKNING = akervardtemp.BETECKNING
         FILL-IN_BENAMNING =  TRIM(akervardtemp.BENAMNING)     
         /* RITNING ANVÄNDS EJ 20140428 LENA
         FILL-IN-RITNING =  SUBSTRING(akervardtemp.BENAMNING,60,20)*/
         /*FILL-IN_LTRAFO1 = antal FILL-IN_LTRAFO2 = pris/st ändrat 20151023*/     
         FILL-IN_LTRAFO1 = akervardtemp.L1
         FILL-IN_LTRAFO2 = akervardtemp.L2      
         FILL-IN_LTRAFO3 = akervardtemp.L3                  
         FILL-IN_KRONOR = akervardtemp.KRONOR.      
         IF akervardtemp.STOLPNR = 0 THEN DO:            
            TOG_TILLFSKADA = TRUE.            
         END.
         
         FIND FIRST stnksktemp WHERE stnksktemp.ID = akervardtemp.L3 NO-ERROR.
         IF AVAILABLE stnksktemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(stnksktemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.         
             
                 
         /*IF Guru.Konstanter:globforetag = "UMEA" THEN. 
         ELSE DO:         
            IF akervardtemp.L4 = 1 THEN TOG_KSK = TRUE.
            ELSE TOG_KSK = FALSE.
         END.*/        
            
         
      END.   
      DISPLAY FILL-IN_BETECKNING FILL-IN_STOLPNR FILL-IN_LTRAFO1 FILL-IN_LTRAFO2 
      /*FILL-IN-RITNING*/ FILL-IN_KRONOR FILL-IN_BENAMNING WITH FRAME FRAME-ANDTRAFO.
   END.
   
   IF val = 6 THEN DO:
      DEBUGGER:SET-BREAK().
      &Scoped-define FRAME-NAME FRAME-REV
      FRAME FRAME-REV:VISIBLE = TRUE.
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR  = valvardnr AND fastvardtemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         ASSIGN FILL-IN_BETECKNING = fastvardtemp.BETECKNING.
         FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
      END.
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.  
      IF vart = "NYA" THEN DO:
         FIND FIRST revbuff  WHERE revbuff.BETECKNING = fastighbet AND  revbuff.ZON = 1 NO-LOCK NO-ERROR.
         IF AVAILABLE revbuff THEN DO:
            FIND FIRST revbuff  WHERE revbuff.BETECKNING = fastighbet AND revbuff.ZON = 2 NO-LOCK NO-ERROR.
            IF AVAILABLE revbuff THEN DO:
               /*skall ej kunna hända. Max två reg per fastighet*/
            END.
            ELSE stpnr = 2.
         END.
         ELSE stpnr = 1.         
         CREATE revtemp.
         ASSIGN 
         revtemp.VARDNR = valvardnr
         revtemp.BETECKNING = fastighbet
         revtemp.ZON = stpnr         
         FILL-IN_BETECKNING = fastighbet
         CMB_ZON = stpnr.          
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "REVADM"                   
         inextradatatemp.HUVUDINT = 1                    
         inextradatatemp.HUVUDCH = "1".                          
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp  NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO: 
            IF CMB_ZON = 1 THEN revtemp.KRPM = extradatatemp.SOKDEC[2].
            IF CMB_ZON = 2 THEN revtemp.KRPM = extradatatemp.SOKDEC[3].            
            FILL-IN-KRM = revtemp.KRPM.         
         END.
      END.   
      ELSE DO:                
         FIND FIRST revtemp WHERE revtemp.VARDNR = valvardnr AND 
         revtemp.BETECKNING = fastighbet AND 
         revtemp.ZON = stpnr NO-LOCK NO-ERROR.         
         ASSIGN         
         CMB_ZON = revtemp.ZON
         FILL-IN_BETECKNING = revtemp.BETECKNING            
         FILL-IN-METER = revtemp.METER.         
         FILL-IN-KRM = revtemp.KRPM.         
      END.   
      DISPLAY FILL-IN_BETECKNING CMB_ZON FILL-IN-KRM FILL-IN-METER WITH FRAME FRAME-REV.         
      IF vart = "and" THEN DISABLE CMB_ZON WITH FRAME FRAME-REV.
   

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skogv_UI DIALOG-1 
PROCEDURE skogv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   &Scoped-define FRAME-NAME FRAME-ANDSKOG   
   IF varforetypval[40] = 1 THEN paslagfaktor = 1.25.
   ELSE paslagfaktor = 1.      
   IF skogvardtemp.FASTPRIS = TRUE THEN DO:       
      ASSIGN skogvardtemp.KRONOR = FILL-IN_KRONOR.
   END.
   ELSE DO:      
      IF TOG_AVTST = TRUE THEN DO:
         /* nystorskog from 20140224 nytt år = 2013*/
         hjar = 2013.
                 
         /*indexuppräkning med "föregående års värde" / 2007
         hjar = 2007.*/
         indexup = 1.
         FIND FIRST kindextemp WHERE kindextemp.AR = hjar AND
         kindextemp.MANAD = 10 USE-INDEX AR NO-LOCK NO-ERROR.  
         IF AVAILABLE kindextemp  THEN DO:
            hjarst = kindextemp.VARDE.            
            hjar = hjar + 1.
            REPEAT:             
               FIND FIRST kindextemp WHERE kindextemp.AR = hjar AND
               kindextemp.MANAD = 10 USE-INDEX AR NO-LOCK NO-ERROR.  
               IF AVAILABLE kindextemp  THEN DO:
                  indexup = kindextemp.VARDE / hjarst.                  
               END.
               ELSE LEAVE.
               IF hjar GE YEAR(TODAY) - 1 THEN LEAVE.
               ELSE DO:
                  ASSIGN
                  hjar = hjar + 1.                
               END.
            END.
          END.
          DEBUGGER:SET-BREAK().
          /* använd indexuppräknade värde från start istället, nya varje år - alla utom Umeå.*/
          IF Guru.Konstanter:globforetag = "umea" THEN.
          ELSE indexup = 1.
                    
          skogvardtemp.KR-HA =  skogkrha.
          IF varforetypval[40] = 1 THEN DO:                                     
          /*IF Guru.Konstanter:globforetag = "pica"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "tect" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:*/          
             /*OBS  här ingår indexuppräkning 20131017 enligt Magnus Sjöström Picab*/                       
             skogvardtemp.KR-HA = skogvardtemp.KR-HA * indexup * paslagfaktor.
          END.
          ELSE DO:
             skogvardtemp.KR-HA = skogvardtemp.KR-HA * indexup.
          END.      
          /*IF Guru.Konstanter:globforetag = "pica"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "tect" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:          
             /*ej påskrivet avtal*/
             /*tect ändrat 20111109 mail annica Oscarsson som kollat med Niclas Molander och Bo Thorngran*/
             /*2011*/
             IF CMB_OMRSKOG = "Norrlands inland" THEN skogvardtemp.KR-HA = 23100.
             ELSE IF CMB_OMRSKOG = "Norrlands kustland" THEN skogvardtemp.KR-HA = 28300.
             ELSE IF CMB_OMRSKOG = "Svealand" THEN skogvardtemp.KR-HA = 31200.
             ELSE IF CMB_OMRSKOG = "Götaland" THEN skogvardtemp.KR-HA = 33900. 
             /* påslag 25% även för storskog enligt picab . Ej påskrivet*/             
             skogvardtemp.KR-HA = skogvardtemp.KR-HA * indexup * paslagfaktor.                          
          END.
          ELSE DO:          
             /*2010             */
             IF CMB_OMRSKOG = "Norrlands inland" THEN skogvardtemp.KR-HA = 10200.
             ELSE IF CMB_OMRSKOG = "Norrlands kustland" THEN skogvardtemp.KR-HA = 14500.
             ELSE IF CMB_OMRSKOG = "Svealand" THEN skogvardtemp.KR-HA = 18400.
             ELSE IF CMB_OMRSKOG = "Götaland" THEN skogvardtemp.KR-HA = 23600.             
             skogvardtemp.KR-HA = skogvardtemp.KR-HA * indexup.             
          END.*/          
          ASSIGN skogvardtemp.KRONOR = skogvardtemp.PROCENT / 100 * skogvardtemp.KR-HA * skogvardtemp.LANGD * skogvardtemp.BREDD / 10000.           
      END.
      ELSE DO:
         
          IF skogvardtemp.BESTTYP = "5A" OR skogvardtemp.BESTTYP = "5B" THEN DO:
             FIND FIRST vardskogtemp WHERE vardskogtemp.VARDOMR = fastighettemp.VSKOG AND
             vardskogtemp.BESTAND = "5" AND            
             vardskogtemp.MARKTYP = skogvardtemp.MARKTYP  USE-INDEX VARDOMR NO-LOCK NO-ERROR.
          END.
          ELSE DO: 
             FIND FIRST vardskogtemp WHERE vardskogtemp.VARDOMR = fastighettemp.VSKOG AND
             vardskogtemp.BESTAND = skogvardtemp.BESTTYP AND
             vardskogtemp.SLUTENHET = skogvardtemp.SLENH AND
             vardskogtemp.MARKTYP = skogvardtemp.MARKTYP  USE-INDEX VARDOMR NO-LOCK NO-ERROR.
          END.   
          IF NOT AVAILABLE vardskogtemp THEN DO:         
             MESSAGE "VSKOG" fastighettemp.VSKOG "BESTTYP" skogvardtemp.BESTTYP "MARKTYP" skogvardtemp.MARKTYP "SLENH" skogvardtemp.SLENH VIEW-AS ALERT-BOX.
             MESSAGE "Denna kombination finns ej upplagd i tabellen!" VIEW-AS ALERT-BOX.   
             musz = TRUE.
             APPLY "ENTRY" TO FILL-IN_BESTAND IN FRAME {&FRAME-NAME}.
             &Scoped-define FRAME-NAME DIALOG-1
             APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
             RETURN.
          END.       
          FIND FIRST markdivtemp NO-LOCK NO-ERROR.
          IF AVAILABLE markdivtemp THEN DO:
             /*IF fastighettemp.VSKOG = "N" THEN ffs = markdivtemp.FAKTORFS - 1. 
             IF fastighettemp.VSKOG = "S" THEN ffs = markdivtemp.FAKTORFS. 
             IF fastighettemp.VSKOG = "G" THEN ffs = markdivtemp.FAKTORFS + 1. */
             /*2008 läggs alla faktorer lika för N S G*/             
             ffs = markdivtemp.FAKTORFS. 
             IF skogvardtemp.BESTTYP = "5A" OR skogvardtemp.BESTTYP = "5B" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B10-0 * ffs.
             END.   
             ELSE IF skogvardtemp.BARRLOV = "10.0" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B10-0 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "9.1" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B9-1 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "8.2" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B8-2 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "7.3" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B7-3 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "6.4" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B6-4 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "5.5" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B5-5 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "4.6" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B4-6 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "3.7" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B3-7 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "2.8" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B2-8 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "1.9" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B1-9 * ffs.
             END.
             ELSE IF skogvardtemp.BARRLOV = "0.10" THEN DO:
                ASSIGN skogvardtemp.KR-HA = vardskogtemp.B0-10 * ffs.
             END.       
             ASSIGN skogvardtemp.KRONOR = paslagfaktor * skogvardtemp.PROCENT / 100 * skogvardtemp.KR-HA * skogvardtemp.LANGD * skogvardtemp.BREDD / 10000. 
          END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stskog_UI DIALOG-1 
PROCEDURE stskog_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
  DEBUGGER:SET-BREAK().
  &Scoped-define FRAME-NAME FRAME-ANDSKOG
  IF TOG_AVTST = TRUE THEN DO:
     CMB_OMRSKOG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
     CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
     FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     FILL-IN-PROCENT = 100.
     FILL-IN_HA = FILL-IN_LANGD * FILL-IN_BREDD / 10000.
     DISPLAY FILL-IN_HA WITH FRAME {&FRAME-NAME}.
     ASSIGN
     FILL-IN-PROCENT-2 = 0
     FILL-IN_HA-2 = 0.
     FILL-IN-PROCENT-2:HIDDEN IN FRAME FRAME-ANDSKOG = TRUE.
     CMB_MARKTYP-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BESTTYP-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     CMB_BARRLOV-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
     FILL-IN_HA-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     FILL-IN_FASTPRIS = FALSE.  
     FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     DISPLAY FILL-IN_FASTPRIS WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:     
     IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "KRIN" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         /*UMEÅ har fortfarande förenklad skogsnorm Ture 20150528*/
        IF CMB_MARKTYP:SCREEN-VALUE = "" OR CMB_MARKTYP:SCREEN-VALUE = ?  THEN  CMB_MARKTYP:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "I   = mycket god".
        IF CMB_BESTTYP:SCREEN-VALUE = "" OR CMB_BESTTYP:SCREEN-VALUE = ? THEN  CMB_BESTTYP:SCREEN-VALUE = "1 = plantskog".         
        IF CMB_MARKTYP-2:SCREEN-VALUE = "" OR CMB_MARKTYP-2:SCREEN-VALUE = ? THEN  CMB_MARKTYP-2:SCREEN-VALUE  IN FRAME FRAME-ANDSKOG = "I   = mycket god".
        IF CMB_BESTTYP-2:SCREEN-VALUE = "" OR CMB_BESTTYP-2:SCREEN-VALUE = ? THEN  CMB_BESTTYP-2:SCREEN-VALUE = "1 = plantskog".                
        CMB_OMRSKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
        CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
        FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        
        DISPLAY CMB_MARKTYP CMB_BESTTYP WITH FRAME FRAME-ANDSKOG.
     END.
     ELSE DO:
        CMB_OMRSKOG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        CMB_MARKTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        CMB_BESTTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        CMB_BARRLOV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        CMB_SLENH:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        CMB_MLANGD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
        FILL-IN-PROCENT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        FILL-IN_FASTPRIS = TRUE.
        FILL-IN_KRONOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        DISPLAY FILL-IN_FASTPRIS FILL-IN_KRONOR WITH FRAME {&FRAME-NAME}.   
     END.      
     
  END.
  IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "cELPA"  THEN.
  ELSE ASSIGN CMB_MLANGD:HIDDEN = TRUE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tempbort_UI DIALOG-1 
PROCEDURE tempbort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   IF val = 1 THEN DO:
      IF vart = "NYA" THEN DO:
         FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
         skogvardtemp.BETECKNING = fastighbet AND 
         skogvardtemp.BESTAND = bestandnr NO-ERROR.    
         /*FIND FIRST skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.*/
         IF AVAILABLE skogvardtemp THEN DO:
            DELETE skogvardtemp.
         END.
         FIND NEXT skogvardtemp WHERE skogvardtemp.VARDNR = valvardnr AND 
         skogvardtemp.BETECKNING = fastighbet AND 
         skogvardtemp.BESTAND = bestandnr NO-ERROR.             
         IF AVAILABLE skogvardtemp THEN DO:
            DELETE skogvardtemp.
         END.
      END.
   END.
   IF val = 2 THEN DO:
      IF vart = "NYA" THEN DO:
         RUN bortvolskog IN andvardspecapph (volskogtemp.VOLSKOGREC).
         FIND FIRST volskogtemp WHERE volskogtemp.VOLSKOGREC = volskogtabrec NO-ERROR.    
         IF AVAILABLE volskogtemp THEN DO:
            DELETE volskogtemp.
         END.
      END.
   END.
   IF val = 3 THEN DO:
      IF vart = "NYA" THEN DO:         
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND akervardtemp.STOLPNR = stpnr AND 
         akervardtemp.L5 = 0 NO-ERROR.
         IF AVAILABLE akervardtemp THEN DO:
            DELETE akervardtemp.
         END.     
      END.
   END.
   IF val = 5 OR val = 7 THEN DO:
      IF vart = "NYA" THEN DO:
         FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = valvardnr AND 
         akervardtemp.BETECKNING = fastighbet AND akervardtemp.STOLPNR = stpnr AND 
         akervardtemp.L5 = 1  NO-ERROR.
         IF AVAILABLE akervardtemp THEN DO:            
            DELETE akervardtemp.
         END.     
      END.
   END.
   IF val = 6 THEN DO:
      IF vart = "NYA" THEN DO:
         FIND FIRST revtemp WHERE revtemp.VARDNR = valvardnr AND 
         revtemp.BETECKNING = fastighbet AND revtemp.ZON = stpnr NO-ERROR.
         IF AVAILABLE revtemp THEN DO:            
            DELETE revtemp.
         END.     
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE volber_UI DIALOG-1 
PROCEDURE volber_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER grta AS CHARACTER.
   DEFINE INPUT PARAMETER senh AS DECIMAL.
   DEFINE INPUT PARAMETER hd AS INTEGER.
   DEFINE INPUT PARAMETER tabv AS INTEGER.   
   CREATE voluppsk.
   ASSIGN
   voluppsk.GRANTALL          = grta 
   voluppsk.SLENH             = senh 
   voluppsk.MEDHOJD           = hd 
   voluppsk.TABELLV           = tabv.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

