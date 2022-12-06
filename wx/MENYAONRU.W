&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
{AOTIDKOPP.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{FAKTTYPDEF.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{PHMT.I}
{ANSPROJBER.I}
{ARBATE.I}
{DIRDEF.I}
{AONRDEF.I}

{AVTPLANTEMP.I}
/*{EGENBEN.I}*/
{OMRTEMPW.I}
{FAKTIN.I}
{KALKIN.I}
{MARKVARDIN.I}
{BERIN.I}
{SOKDEF.I}
{HOPPSEK2W.I}
{EXTRATAB.I}  





&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{AVDTEMP.I}
{BESTKUNDALLT.I}
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE tanint AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tanval AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE tanlangd AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE qsort AS CHARACTER FORMAT "x(65)" NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE omrbildvar AS CHARACTER  NO-UNDO.
DEFINE NEW SHARED VARIABLE rubrikvar AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE NEW SHARED VARIABLE saonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aoplan AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE avbryt AS LOGICAL NO-UNDO.                           
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE labelvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE valtrecid AS RECID NO-UNDO.
DEFINE VARIABLE valdarec AS RECID NO-UNDO.
DEFINE VARIABLE valdarec2 AS RECID NO-UNDO.
DEFINE VARIABLE brwakt AS LOGICAL NO-UNDO.

DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tillbakaaonr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tillbakadelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE hjdelvar AS INTEGER NO-UNDO.
DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vardnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE projvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bortvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoavslut AS DATE NO-UNDO.
DEFINE VARIABLE bortaoapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fastdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr aotidslagtemp valdaao

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT utsokaonr.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


/* Definitions for BROWSE BRW_AOTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AOTID aotidslagtemp.AONR ~
aotidslagtemp.DELNR aotidslagtemp.TIDLAGE aotidslagtemp.AKTIVITET1 ~
aotidslagtemp.DAT1 aotidslagtemp.AKTIVITET2 aotidslagtemp.DAT2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOTID 
&Scoped-define QUERY-STRING-BRW_AOTID FOR EACH aotidslagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AOTID OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOTID aotidslagtemp


/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR valdaao.AONR valdaao.DELNR ~
valdaao.ORT valdaao.OMRADE valdaao.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR 
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH valdaao NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH valdaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR valdaao


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_AONR}~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_ALLOVER RECT-50 BTN_ALLBACK RECT-57 ~
RECT-60 BTN_BACK BTN_AVB BTN_OVER BTN_TIDPLAN BRW_AOTID BTN_NVE-3 BTN_NVE-4 ~
FILL-IN-AVSTARTD FILL-IN-AVSLUTD TOG_PAGA TOG_AVSLUTADE BTN_FVE-3 BTN_FVE-4 ~
TOG_TILLF TOG_FASTA TOG_HUVNR CMB_JURP CMB_PROJ CMB_FAK CMB_AVD CMB_BERE ~
FILL-IN-K1 CMB_OMR CMB_ANSV FILL-IN-K2 CMB_BESORG BTN_HAMT BRW_AONR ~
BRW_VAONR FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR FILL-IN-REF 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-MELL FILL-IN-AVSTARTD FILL-IN-OCH ~
FILL-IN-AVSLUTD TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA TOG_HUVNR ~
CMB_JURP CMB_PROJ CMB_FAK CMB_AVD CMB_BERE FILL-IN-K1 CMB_OMR CMB_ANSV ~
FILL-IN-K2 CMB_BESORG FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR ~
FILL-IN-REF FILL-IN-AOTEXT FILL-IN-KTO FILL-IN-AOTEXT-2 FILL-IN-AOTEXT-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkbrw C-Win 
FUNCTION checkbrw RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 10 BY 1 TOOLTIP "Ta bort ett nytt aonr".

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 10 BY 1 TOOLTIP "Skapa ett nytt aonr.".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_TIDPLAN 
     LABEL "Tidplan" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_AVROP 
     LABEL "Best-Avrop":L 
     SIZE 12 BY 1 TOOLTIP "Besställning/Avrop".

DEFINE BUTTON FBTN_AVSAONR 
     LABEL "Avsluta aonr":L 
     SIZE 12 BY 1 TOOLTIP "Avsluta aonr:et. Ingen tidskrivning skall kan registreras mera".

DEFINE BUTTON FBTN_BER 
     LABEL "Bereda":L 
     SIZE 12 BY 1 TOOLTIP "Beredningsmodul".

DEFINE BUTTON FBTN_BYTPNR 
     LABEL "Byt aonr":L 
     SIZE 12 BY 1 TOOLTIP "Byt ett aonr till ett annat och flytta gjorda kopplingar".

DEFINE BUTTON FBTN_FAKT 
     LABEL "Fakturera":L 
     SIZE 12 BY 1 TOOLTIP "Fakturarutinen".

DEFINE BUTTON FBTN_KALK 
     LABEL "Kalkylera":L 
     SIZE 12 BY 1 TOOLTIP "Kalkylera olika typer av kalkyler".

DEFINE BUTTON FBTN_KOPI 
     LABEL "Kopiera":L 
     SIZE 12 BY 1 TOOLTIP "Kopiera ett aonr".

DEFINE BUTTON FBTN_KOST 
     LABEL "Kostnadsreg":L 
     SIZE 12 BY 1 TOOLTIP "Registrera externa kostnader eller visa kostnader från andra system.".

DEFINE BUTTON FBTN_MARK 
     LABEL "Markvärdera":L 
     SIZE 12 BY 1 TOOLTIP "Markvärderingsmodul".

DEFINE BUTTON FBTN_RAPP 
     LABEL "Rapporter":L 
     SIZE 12 BY 1 TOOLTIP "Rapport sammanstälning".

DEFINE BUTTON FBTN_STATUS 
     LABEL "Status":L 
     SIZE 12 BY 1 TOOLTIP "Tidlägen - Milstolpar".

DEFINE BUTTON FBTN_UNDER 
     LABEL "Underindela":L 
     SIZE 12 BY 1 TOOLTIP "Underindela dvs skapa nya delnr".

DEFINE BUTTON FBTN_UPP 
     LABEL "Aonr huvud":L 
     SIZE 12 BY 1 TOOLTIP "Här kan du ändra allt som rör aonr:et".

DEFINE BUTTON FBTN_VISAO 
     LABEL "Visa":L 
     SIZE 12 BY 1 TOOLTIP "Visa/skriv ut allt som rör aonr:et".

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARBART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsart" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BERE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beställare/Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(35)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PROJ AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av arbetsorder" 
      VIEW-AS TEXT 
     SIZE 36.63 BY 1
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Sök på:" 
      VIEW-AS TEXT 
     SIZE 8 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
      VIEW-AS TEXT 
     SIZE 6.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-K1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTOK1" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-K2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTO" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-KTO AS CHARACTER FORMAT "X(256)":U INITIAL "Urval konto" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REF AS CHARACTER FORMAT "X(256)" 
     LABEL "Ref.nr beställare" 
     VIEW-AS FILL-IN 
     SIZE 13.88 BY .83 TOOLTIP "Sök på Ref.nr Beställare".

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(256)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83 TOOLTIP "Sök på benämning" NO-UNDO.

DEFINE VARIABLE FILL-IN_SAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Sök på Aonr"
     FGCOLOR 2  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PAAV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pågående", 1,
"Avslutade", 2
     SIZE 20 BY 1 TOOLTIP "Utsökningen ska gälla antingen pågående eller avslutade eller både och." NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 124.5 BY .5
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58.5 BY 1.21.

DEFINE VARIABLE TOG_AVSLUTADE AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 TOOLTIP "Utsökningen ska gälla avslutade mellan angivna datum." NO-UNDO.

DEFINE VARIABLE TOG_FASTA AS LOGICAL INITIAL no 
     LABEL "Fasta" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_HUVNR AS LOGICAL INITIAL no 
     LABEL "Endast huvudnummer" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_PAGA AS LOGICAL INITIAL no 
     LABEL "Pågående" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 TOOLTIP "Utsökningen ska gälla pågående." NO-UNDO.

DEFINE VARIABLE TOG_TILLF AS LOGICAL INITIAL no 
     LABEL "Tillfälliga" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_AOTID FOR 
      aotidslagtemp SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      valdaao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 30
      utsokaonr.BEREDARE FORMAT "x(256)":U WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 62 BY 13.04
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_AOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOTID C-Win _STRUCTURED
  QUERY BRW_AOTID NO-LOCK DISPLAY
      aotidslagtemp.AONR FORMAT "X(6)":U
      aotidslagtemp.DELNR COLUMN-LABEL "cDELNR" FORMAT "999":U
      aotidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(256)":U
            WIDTH 25
      aotidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 20
      aotidslagtemp.DAT1 FORMAT "99/99/99":U
      aotidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(256)":U
            WIDTH 20
      aotidslagtemp.DAT2 FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 98 BY 25.25
         TITLE "Tidlägen".

DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR C-Win _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 30
      valdaao.OMRADE FORMAT "x(6)":U
      valdaao.BEREDARE FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 58.5 BY 13.04
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_ALLOVER AT ROW 16.88 COL 63
     BTN_ALLBACK AT ROW 23.13 COL 63
     FBTN_UPP AT ROW 1.08 COL 15.75
     FBTN_UNDER AT ROW 1.08 COL 27.75
     BTN_BACK AT ROW 21.04 COL 63
     FBTN_KOPI AT ROW 1.08 COL 39.75
     FBTN_VISAO AT ROW 1.08 COL 51.75
     FBTN_AVSAONR AT ROW 1.08 COL 63.75
     FBTN_RAPP AT ROW 1.08 COL 75.75
     FBTN_KOST AT ROW 1.08 COL 87.75
     FBTN_BYTPNR AT ROW 1.08 COL 99.75
     BTN_AVB AT ROW 1.08 COL 112.75
     FBTN_BER AT ROW 2.08 COL 15.75
     FBTN_KALK AT ROW 2.08 COL 27.75
     FBTN_MARK AT ROW 2.08 COL 39.75
     FBTN_FAKT AT ROW 2.08 COL 51.75
     FBTN_AVROP AT ROW 2.08 COL 63.75
     FBTN_STATUS AT ROW 2.08 COL 75.75
     BTN_OVER AT ROW 18.96 COL 63
     BTN_TIDPLAN AT ROW 2.08 COL 87.75
     BRW_AOTID AT ROW 4 COL 12
     BTN_NVE-3 AT ROW 4.54 COL 59.13
     BTN_NVE-4 AT ROW 4.54 COL 76.5
     FILL-IN-MELL AT ROW 4.75 COL 40 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSTARTD AT ROW 4.75 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 4.75 COL 60.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD AT ROW 4.75 COL 64.5 COLON-ALIGNED NO-LABEL
     TOG_PAGA AT ROW 4.92 COL 3
     TOG_AVSLUTADE AT ROW 4.92 COL 23
     BTN_FVE-3 AT ROW 5.33 COL 59.13
     BTN_FVE-4 AT ROW 5.33 COL 76.5
     TOG_TILLF AT ROW 5.83 COL 3
     TOG_FASTA AT ROW 5.83 COL 23
     TOG_HUVNR AT ROW 6.75 COL 3
     CMB_JURP AT ROW 7.71 COL 9.5
     CMB_PROJ AT ROW 7.75 COL 58.5 COLON-ALIGNED
     CMB_FAK AT ROW 7.75 COL 100 COLON-ALIGNED
     CMB_AVD AT ROW 8.92 COL 8.5
     CMB_BERE AT ROW 8.96 COL 58.5 COLON-ALIGNED
     FILL-IN-K1 AT ROW 9.83 COL 100 COLON-ALIGNED
     CMB_OMR AT ROW 10.13 COL 11.5
     CMB_ANSV AT ROW 10.17 COL 58.5 COLON-ALIGNED
     FILL-IN-K2 AT ROW 10.92 COL 100 COLON-ALIGNED
     CMB_BESORG AT ROW 11.33 COL 17.5 COLON-ALIGNED
     CMB_ARBART AT ROW 11.38 COL 58.5 COLON-ALIGNED
     BTN_HAMT AT ROW 12.46 COL 21
     BRW_AONR AT ROW 13.54 COL 1
     BRW_VAONR AT ROW 13.54 COL 67
     RAD_FAST AT ROW 22.71 COL 100.63 NO-LABEL
     FILL-IN-AR AT ROW 23.83 COL 106 COLON-ALIGNED NO-LABEL
     RAD_PAAV AT ROW 24.21 COL 104.63 NO-LABEL
     BTN_NY AT ROW 26.71 COL 85.13
     BTN_BORT AT ROW 26.71 COL 97.63
     FILL-IN_SAONR AT ROW 26.75 COL 14.88 COLON-ALIGNED
     FILL-IN_ORT AT ROW 26.75 COL 40.38 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 27.96 COL 77.75 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR AT ROW 27.96 COL 87.63 COLON-ALIGNED NO-LABEL
     FILL-IN-REF AT ROW 27.96 COL 109.13 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-AOTEXT AT ROW 3.75 COL 2.25 NO-LABEL
     FILL-IN-KTO AT ROW 9.08 COL 91.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AOTEXT-2 AT ROW 26.88 COL 2.38 NO-LABEL
     FILL-IN-AOTEXT-3 AT ROW 28 COL 67.75 NO-LABEL
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.08 COL 1.25
          FGCOLOR 1 FONT 17
     RECT-50 AT ROW 26.63 COL 1
     RECT-57 AT ROW 3.08 COL 1
     RECT-60 AT ROW 27.83 COL 67
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 28.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: jurperstemp T "?" NO-UNDO temp-db jurperstemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.54
         WIDTH              = 124.88
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
                                                                        */
/* BROWSE-TAB BRW_AOTID BTN_TIDPLAN DEFAULT-FRAME */
/* BROWSE-TAB BRW_AONR BTN_HAMT DEFAULT-FRAME */
/* BROWSE-TAB BRW_VAONR BRW_AONR DEFAULT-FRAME */
ASSIGN 
       BRW_AONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_AONR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_AONR:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_AOTID:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_AOTID:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_VAONR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_VAONR:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR BUTTON BTN_BORT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_TIDPLAN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARBART IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_ARBART:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME DEFAULT-FRAME
   SHARED ALIGN-L                                                       */
/* SETTINGS FOR BUTTON FBTN_AVROP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_AVROP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_AVSAONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_AVSAONR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_BER IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_BER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_BYTPNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_BYTPNR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_FAKT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_FAKT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_KALK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_KALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_KOPI IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_KOPI:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_KOST IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_KOST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_MARK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_MARK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_RAPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_RAPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_STATUS IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_STATUS:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_UNDER IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_UNDER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_UPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_UPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISAO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISAO:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KTO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PAAV IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PAAV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.utsokaonr.BEREDARE
"utsokaonr.BEREDARE" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOTID
/* Query rebuild information for BROWSE BRW_AOTID
     _TblList          = "Temp-Tables.aotidslagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.aotidslagtemp.AONR
     _FldNameList[2]   > Temp-Tables.aotidslagtemp.DELNR
"aotidslagtemp.DELNR" "cDELNR" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.aotidslagtemp.TIDLAGE
"aotidslagtemp.TIDLAGE" "Tidläge" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.aotidslagtemp.AKTIVITET1
"aotidslagtemp.AKTIVITET1" "Aktivitet" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.aotidslagtemp.DAT1
     _FldNameList[6]   > Temp-Tables.aotidslagtemp.AKTIVITET2
"aotidslagtemp.AKTIVITET2" "Aktivitet" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.aotidslagtemp.DAT2
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaao.DELNR
"valdaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.valdaao.OMRADE
     _FldNameList[5]   = Temp-Tables.valdaao.BEREDARE
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
 
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON ENTRY OF BRW_AONR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAONR
&Scoped-define SELF-NAME BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR C-Win
ON ENTRY OF BRW_VAONR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   IF AVAILABLE valdaao THEN BRW_VAONR:TOOLTIP = valdaao.ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VAONR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   APPLY "CHOOSE" TO FBTN_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR C-Win
ON VALUE-CHANGED OF BRW_VAONR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   
   
   IF AVAILABLE valdaao THEN DO:
      ASSIGN
      BRW_VAONR:TOOLTIP = valdaao.ORT
      Guru.GlobalaVariabler:plusaonr = valdaao.AONR
      Guru.GlobalaVariabler:plusdnr = valdaao.DELNR.
   END.
   ELSE DO:
      Guru.GlobalaVariabler:plusaonr = ?.
   END.
   /*
   OUTPUT TO c:\protemp9\t.txt.
   FOR EACH valdaao WHERE valdaao.aonr = "004005" BY valdaao.delnr:
      EXPORT valdaao.
   END.
   OUTPUT CLOSE.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   
   vartpro = "".
   MESSAGE "Vill du spara dina valda " LC(Guru.Konstanter:gaok) " för snabbare åtkomst ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   IF val = TRUE THEN DO:
      tthandle = TEMP-TABLE valdaao:HANDLE.
      FIND FIRST sparaladdatemp NO-ERROR.
      IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
      ASSIGN
      sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
      sparaladdatemp.BENAMNING = "AONR" /*Benämnings sufix, i detta fall ELPAO$STOR*/
      sparaladdatemp.TABVAL = "AONRTAB" /*Tabellnamn*/
      sparaladdatemp.FALTVALAO = "AONR" /*Character field*/
      sparaladdatemp.FALTVALDEL = "DELNR" /*Integer field*/
      sparaladdatemp.FALTVALDATE = "".  /*DATE field*/
      RUN sparabrw_UI IN brwproc[{&RIGHT-BROWSE}] 
         (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).
   END.        
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 C-Win
ON CHOOSE OF BTN_FVE-3 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.   
   FILL-IN-AVSTARTD = FILL-IN-AVSTARTD - 1.      
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 C-Win
ON CHOOSE OF BTN_FVE-4 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.   
   FILL-IN-AVSLUTD = FILL-IN-AVSLUTD - 1.      
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME DEFAULT-FRAME /* Hämta och visa urval */
DO:
   
   BTN_HAMT:LOAD-IMAGE ("BILDER\btn_hamt_over.gif") IN FRAME {&FRAME-NAME}.    
   
   RUN utvald_UI.
   RUN allaao_UI.
   RUN title_UI IN brwproc[{&LEFT-BROWSE}].
   BTN_HAMT:LOAD-IMAGE ("BILDER\btn_hamt.gif") IN FRAME {&FRAME-NAME}.    
   
/*    RUN goma_UI. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 C-Win
ON CHOOSE OF BTN_NVE-3 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.   
   FILL-IN-AVSTARTD = FILL-IN-AVSTARTD + 1.        
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 C-Win
ON CHOOSE OF BTN_NVE-4 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.   
   FILL-IN-AVSLUTD = FILL-IN-AVSLUTD + 1.        
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   RUN ny_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TIDPLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TIDPLAN C-Win
ON CHOOSE OF BTN_TIDPLAN IN FRAME DEFAULT-FRAME /* Tidplan */
DO:
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   {muswait.i}
   {AVBGOM.I}
    RUN TIDSPLAN.W.
   {AVBFRAM.I}   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME DEFAULT-FRAME /* Avdelning */
DO:
     
   CMB_AVD = INPUT CMB_AVD.      
   
   {CMB_AVDN1.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESORG C-Win
ON LEAVE OF CMB_BESORG IN FRAME DEFAULT-FRAME /* Beställare/Kund */
DO:
   CMB_BESORG = INPUT CMB_BESORG.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FAK C-Win
ON VALUE-CHANGED OF CMB_FAK IN FRAME DEFAULT-FRAME /* Fakturakategori */
DO:
   CMB_FAK = INPUT CMB_FAK.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP C-Win
ON VALUE-CHANGED OF CMB_JURP IN FRAME DEFAULT-FRAME /* Juridisp */
DO:
     
   CMB_JURP = INPUT CMB_JURP.      
   
   {CMB_JURP.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME DEFAULT-FRAME /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVROP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVROP C-Win
ON CHOOSE OF FBTN_AVROP IN FRAME DEFAULT-FRAME /* Best-Avrop */
DO: 
   RUN avrop_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVSAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVSAONR C-Win
ON CHOOSE OF FBTN_AVSAONR IN FRAME DEFAULT-FRAME /* Avsluta aonr */
DO:
   /*IF brwval() = TRUE THEN RETURN NO-APPLY.*/
   RUN avsluta_UI. 
   IF checkbrw() = TRUE THEN DO:
      RUN changesize_UI IN brwproc[{&ARROWS}] (INPUT TRUE). /*Input TRUE = vänster browser*/ 
   END.
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BER C-Win
ON CHOOSE OF FBTN_BER IN FRAME DEFAULT-FRAME /* Bereda */
DO: 
   RUN ber_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BYTPNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BYTPNR C-Win
ON CHOOSE OF FBTN_BYTPNR IN FRAME DEFAULT-FRAME /* Byt aonr */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN bytaonr_UI.
   IF checkbrw() = TRUE THEN DO:
      RUN changesize_UI IN brwproc[{&ARROWS}] (INPUT TRUE). /*Input TRUE = vänster browser*/ 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FAKT C-Win
ON CHOOSE OF FBTN_FAKT IN FRAME DEFAULT-FRAME /* Fakturera */
DO: 
   RUN fakt_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KALK C-Win
ON CHOOSE OF FBTN_KALK IN FRAME DEFAULT-FRAME /* Kalkylera */
DO: 
   RUN kalk_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOPI C-Win
ON CHOOSE OF FBTN_KOPI IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN kopi_UI.
   IF checkbrw() = TRUE THEN DO:
      RUN changesize_UI IN brwproc[{&ARROWS}] (INPUT TRUE). /*Input TRUE = vänster browser*/ 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOST C-Win
ON CHOOSE OF FBTN_KOST IN FRAME DEFAULT-FRAME /* Kostnadsreg */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN kost_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARK C-Win
ON CHOOSE OF FBTN_MARK IN FRAME DEFAULT-FRAME /* Markvärdera */
DO: 
   vartpro = "".
   RUN mark_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_RAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_RAPP C-Win
ON CHOOSE OF FBTN_RAPP IN FRAME DEFAULT-FRAME /* Rapporter */
DO:    
   RUN rapp_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_STATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_STATUS C-Win
ON CHOOSE OF FBTN_STATUS IN FRAME DEFAULT-FRAME /* Status */
DO: 
   IF FBTN_STATUS:LABEL = "Status" THEN DO:     
      {muswait.i}
      RUN laddaaotid IN aonrapph 
      (INPUT TABLE valdaao, OUTPUT TABLE aotidslagtemp).
      RUN openbdyn_UI IN brwproc[5] (INPUT "").
      {musarrow.i}
      ASSIGN
      FILL-IN-KTO      :HIDDEN = TRUE
      FBTN_STATUS:LABEL = "Åter"
      BRW_AOTID        :HIDDEN = FALSE
      FILL-IN-AOTEXT-2 :HIDDEN = TRUE
      FILL-IN-AOTEXT-3 :HIDDEN = TRUE
      BRW_AONR         :HIDDEN = TRUE
      BRW_VAONR        :HIDDEN = TRUE
      BTN_ALLBACK      :HIDDEN = TRUE
      BTN_ALLOVER      :HIDDEN = TRUE
      BTN_BACK         :HIDDEN = TRUE
      BTN_BORT         :HIDDEN = TRUE  
      BTN_FVE-3        :HIDDEN = TRUE 
      BTN_FVE-4        :HIDDEN = TRUE 
      BTN_HAMT         :HIDDEN = TRUE 
      BTN_NVE-3        :HIDDEN = TRUE 
      BTN_NVE-4        :HIDDEN = TRUE 
      BTN_NY           :HIDDEN = TRUE 
      BTN_OVER         :HIDDEN = TRUE 
      CMB_ANSV         :HIDDEN = TRUE 
      CMB_ARBART       :HIDDEN = TRUE 
      CMB_AVD          :HIDDEN = TRUE 
      CMB_BERE         :HIDDEN = TRUE 
      CMB_BESORG       :HIDDEN = TRUE 
      CMB_FAK          :HIDDEN = TRUE 
      CMB_JURP         :HIDDEN = TRUE 
      CMB_OMR          :HIDDEN = TRUE 
      CMB_PROJ         :HIDDEN = TRUE 
      FILL-IN_DELNR    :HIDDEN = TRUE 
      FILL-IN_EAONR    :HIDDEN = TRUE 
      FILL-IN_ORT      :HIDDEN = TRUE 
      FILL-IN_SAONR    :HIDDEN = TRUE 
      FILL-IN-AOTEXT   :HIDDEN = TRUE 
      FILL-IN-AR       :HIDDEN = TRUE 
      FILL-IN-AVSLUTD  :HIDDEN = TRUE 
      FILL-IN-AVSTARTD :HIDDEN = TRUE 
      FILL-IN-K1       :HIDDEN = TRUE 
      FILL-IN-K2       :HIDDEN = TRUE 
      FILL-IN-MELL     :HIDDEN = TRUE 
      FILL-IN-OCH      :HIDDEN = TRUE 
      FILL-IN-REF      :HIDDEN = TRUE 
      RAD_FAST         :HIDDEN = TRUE 
      RAD_PAAV         :HIDDEN = TRUE 
      TOG_AVSLUTADE    :HIDDEN = TRUE 
      TOG_FASTA        :HIDDEN = TRUE 
      TOG_HUVNR        :HIDDEN = TRUE 
      TOG_PAGA         :HIDDEN = TRUE 
      TOG_TILLF        :HIDDEN = TRUE 
      BRW_AONR         :HIDDEN = TRUE 
      RECT-50          :HIDDEN = TRUE 
      RECT-60          :HIDDEN = TRUE.
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globanv = "SEMON" THEN BTN_TIDPLAN:HIDDEN = TRUE.
      DISABLE 
      FBTN_AVROP FBTN_AVSAONR FBTN_BER FBTN_BYTPNR FBTN_FAKT FBTN_KALK FBTN_KOPI FBTN_KOST 
      FBTN_MARK FBTN_RAPP FBTN_UNDER FBTN_UPP FBTN_VISAO   
      WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:
      ASSIGN
      FBTN_STATUS:LABEL = "Status".
      RUN goma_UI.
      ASSIGN
      BRW_AONR:HIDDEN = FALSE
      BRW_AOTID        :HIDDEN = TRUE.
      RUN sek_UI.
      DISPLAY 
      FILL-IN-KTO      
      FILL-IN_DELNR   
      FILL-IN_EAONR   
      FILL-IN_ORT     
      FILL-IN_SAONR   
      FILL-IN-AOTEXT  
      FILL-IN-AVSLUTD 
      FILL-IN-AVSTARTD
      FILL-IN-K1      
      FILL-IN-K2      
      FILL-IN-MELL    
      FILL-IN-OCH     
      FILL-IN-REF     
      FILL-IN-AOTEXT-2
      FILL-IN-AOTEXT-3
      RECT-50          
      RECT-60   
      BTN_FVE-3   
      BTN_FVE-4   
      BTN_HAMT    
      BTN_NVE-3   
      BTN_NVE-4   
      BRW_AONR
      BRW_VAONR   
      BTN_ALLBACK 
      BTN_ALLOVER 
      BTN_BACK    
      CMB_AVD
      CMB_OMR CMB_BESORG CMB_FAK CMB_ANSV CMB_BERE CMB_PROJ FILL-IN-AVSLUTD
      FILL-IN-AVSTARTD   
      TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA 
      TOG_HUVNR
      WITH FRAME {&FRAME-NAME}. 
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globanv = "SEMON" THEN DISPLAY BTN_TIDPLAN
         WITH FRAME {&FRAME-NAME}. 
      FIND FIRST jurperstemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.
      ELSE CMB_JURP:HIDDEN = FALSE.
      IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      IF labelvar2 = "" THEN FILL-IN-K2:HIDDEN = TRUE.
      ELSE FILL-IN-K2:HIDDEN = FALSE.      
      IF labelvar1 = "" THEN FILL-IN-K1:HIDDEN = TRUE.
      ELSE FILL-IN-K1:HIDDEN = FALSE.      
      IF labelvar1 = "" AND labelvar2 = ""  THEN FILL-IN-KTO:HIDDEN  = TRUE.      

   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_UNDER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_UNDER C-Win
ON CHOOSE OF FBTN_UNDER IN FRAME DEFAULT-FRAME /* Underindela */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN under_UI.
   IF checkbrw() = TRUE THEN DO:
      RUN changesize_UI IN brwproc[{&ARROWS}] (INPUT TRUE). /*Input TRUE = vänster browser*/ 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_UPP C-Win
ON CHOOSE OF FBTN_UPP IN FRAME DEFAULT-FRAME /* Aonr huvud */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   {AVBGOM.I}
   RUN andra_UI.
   {AVBFRAM.I}
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT brwakt).
   IF brwakt = TRUE THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = aonrvar AND
      utsokaonr.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAO C-Win
ON CHOOSE OF FBTN_VISAO IN FRAME DEFAULT-FRAME /* Visa */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN visao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAO C-Win
ON MOUSE-MENU-CLICK OF FBTN_VISAO IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {AVBGOM.I}
   RUN xhtml2.p.
   {AVBFRAM.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR C-Win
ON LEAVE OF FILL-IN-AR IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON LEAVE OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSTARTD = FILL-IN-AVSLUTD.
      DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD
   regdatum = INPUT FILL-IN-AVSLUTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSLUTD = regdatum.
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSTARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON LEAVE OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSLUTD = FILL-IN-AVSTARTD.
      DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD
   regdatum = INPUT FILL-IN-AVSTARTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSTARTD = regdatum.
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-REF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-REF C-Win
ON ANY-KEY OF FILL-IN-REF IN FRAME DEFAULT-FRAME /* Ref.nr beställare */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-REF IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-REF C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-REF IN FRAME DEFAULT-FRAME /* Ref.nr beställare */
DO:
   {muswait.i}
   FILL-IN-REF = INPUT FILL-IN-REF.   
   status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE valdaao THEN DO:
      ASSIGN
      tillbakaaonr = valdaao.AONR
      tillbakadelnr = valdaao.DELNR.
   END.
   ELSE tillbakaaonr = ?.      
   RUN refhmtaen IN aonrapph (INPUT FILL-IN-REF, OUTPUT TABLE evaldaao, OUTPUT TABLE felmeddtemp,
                              OUTPUT aonrvar,OUTPUT delnrvar).
   FOR EACH evaldaao:
      FIND FIRST valdaao WHERE valdaao.AONR = evaldaao.AONR AND valdaao.DELNR = evaldaao.DELNR
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdaao THEN DO:
         CREATE valdaao.
         BUFFER-COPY evaldaao TO valdaao.
      END.
   END.
   EMPTY TEMP-TABLE evaldaao NO-ERROR. 

   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      FIND FIRST valdaao WHERE valdaao.AONR = tillbakaaonr AND 
      valdaao.DELNR = tillbakadelnr NO-LOCK NO-ERROR. 
      IF AVAILABLE valdaao THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
         RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].        
      END.
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.         
   END.
   ELSE DO:
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF AVAILABLE valdaao THEN DO:      
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
   END.     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON ANY-KEY OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   {TRYCKS.I}
  /* MESSAGE KEYLABEL(LASTKEY). */
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON ENTRY OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   /*BTN_EXTRA:DEFAULT = TRUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON LEAVE OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
    /*
   BTN_EXTRA:DEFAULT = FALSE. 
   BTN_VISA:DEFAULT = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   ASSIGN
   FILL-IN_EAONR = INPUT FRAME {&FRAME-NAME} FILL-IN_EAONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR
   aonrvar = FILL-IN_EAONR
   delnrvar = FILL-IN_DELNR.   
   status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE valdaao THEN DO:
      ASSIGN
      tillbakaaonr = valdaao.AONR
      tillbakadelnr = valdaao.DELNR.
   END.
   ELSE tillbakaaonr = ?.
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR. 
   IF AVAILABLE valdaao THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].        
   END.
   ELSE DO:
      RUN aonrhmtaen IN aonrapph (INPUT aonrvar,INPUT delnrvar,
      OUTPUT TABLE valdaao APPEND,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         FIND FIRST valdaao WHERE valdaao.AONR = tillbakaaonr AND 
         valdaao.DELNR = tillbakadelnr NO-LOCK NO-ERROR. 
         IF AVAILABLE valdaao THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].        
         END.
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.         
      END.
      ELSE DO:
         RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
         FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
         valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
         IF AVAILABLE valdaao THEN DO:      
            RUN aokomp_UI (INPUT 2).
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.  
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR C-Win
ON ANY-KEY OF FILL-IN_EAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EAONR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR C-Win
ON MOUSE-MENU-CLICK OF FILL-IN_EAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   
   /*
   ASSIGN
   tanint = false
   tanlangd = 25.
   
   RUN TANGENT.W.
   {AVBFRAM.I}
   FILL-IN_EAONR = tanval.
   DISPLAY FILL-IN_EAONR WITH FRAME {&FRAME-NAME}.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_EAONR = INPUT FILL-IN_EAONR.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON LEAVE OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Ort/Benämning */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SAONR C-Win
ON LEAVE OF FILL-IN_SAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_SAONR = INPUT FILL-IN_SAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST C-Win
ON VALUE-CHANGED OF RAD_FAST IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RAD_FAST = INPUT RAD_FAST. 
/*    RUN goma_UI. */
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PAAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PAAV C-Win
ON VALUE-CHANGED OF RAD_PAAV IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RAD_PAAV = INPUT RAD_PAAV. 
/*    RUN goma_UI. */
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AVSLUTADE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVSLUTADE C-Win
ON VALUE-CHANGED OF TOG_AVSLUTADE IN FRAME DEFAULT-FRAME /* Avslutade */
DO:
   {muswait.i}                
   TOG_AVSLUTADE = INPUT TOG_AVSLUTADE. 
   RUN goma_UI.
   {musarrow.i}               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FASTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FASTA C-Win
ON VALUE-CHANGED OF TOG_FASTA IN FRAME DEFAULT-FRAME /* Fasta */
DO:
   {muswait.i}
   TOG_FASTA = INPUT TOG_FASTA. 
/*    RUN goma_UI. */
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_HUVNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_HUVNR C-Win
ON VALUE-CHANGED OF TOG_HUVNR IN FRAME DEFAULT-FRAME /* Endast huvudnummer */
DO:
   TOG_HUVNR = INPUT TOG_HUVNR.
   FIND FIRST uppvaltemp NO-ERROR.      
   IF TOG_HUVNR = TRUE THEN uppvaltemp.DELNRKOLL = FALSE.
   IF TOG_HUVNR = FALSE THEN uppvaltemp.DELNRKOLL = TRUE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PAGA C-Win
ON VALUE-CHANGED OF TOG_PAGA IN FRAME DEFAULT-FRAME /* Pågående */
DO:
   {muswait.i}                
   TOG_PAGA = INPUT TOG_PAGA. 
/*    RUN goma_UI. */
   {musarrow.i}               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TILLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TILLF C-Win
ON VALUE-CHANGED OF TOG_TILLF IN FRAME DEFAULT-FRAME /* Tillfälliga */
DO:
   {muswait.i}
   TOG_TILLF = INPUT TOG_TILLF. 
/*    RUN goma_UI. */
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
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
   IF VALID-HANDLE(bortaoapph) THEN DELETE PROCEDURE bortaoapph NO-ERROR.      
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph NO-ERROR.      
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.      
   IF VALID-HANDLE(nyttaoapph2) THEN DELETE PROCEDURE nyttaoapph2 NO-ERROR.
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
   {OMRHMT.I}
   {ALLSTARTDYN.I}   
   {JURPAVDSTART2.I}   
   ASSIGN
   kalknrvar = ?
   kalktypvar = ?.
   status-ok = CMB_OMR:ADD-FIRST("Alla").   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:   
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   CMB_OMR = omrtemp.NAMN. 
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
      CMB_OMR = "Alla".
      ASSIGN CMB_OMR:SCREEN-VALUE = "Alla".
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      CMB_OMR = INPUT CMB_OMR.
   END.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   bestvad = 2.
   {BESTHMT.I}
   IF varforetypval[3] >= 1 THEN.   
   ELSE DO:
      OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
      GET FIRST oq NO-LOCK.
      DO WHILE AVAILABLE(omrtemp):
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE NO-ERROR.
         IF NOT AVAILABLE bestkundallt THEN DO:
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
            CREATE bestkundextra.
            ASSIGN 
            bestkundextra.VIBESTID = omrtemp.OMRADE 
            bestkundextra.BESTID = omrtemp.OMRADE 
            bestkundextra.BESTNAMN = omrtemp.NAMN
            bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
         END.
         GET NEXT oq NO-LOCK.
      END.
   END.   
   status-ok = CMB_BESORG:ADD-FIRST("Alla").
   FOR EACH bestkundallt USE-INDEX BESTID NO-LOCK:
      status-ok = CMB_BESORG:ADD-LAST(bestkundallt.BESTNAMN).
   END.
   status-ok = CMB_BESORG:ADD-LAST("Samma som ovan").   
   RUN main1_UI.
   C-Win:TITLE = Guru.Konstanter:gaol + "hantering".
   &Scoped-define FORMATNAMN FILL-IN_EAONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_SAONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}      
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   /*
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMATSUND.I}       
   &Scoped-define FORMATNAMNOMR valdaao.OMRADE
   &Scoped-define BROWSE-NAME BRW_VAONR
   {OMRAOFORMATSUND.I} 
   */
   CREATE uppvaltemp.
   IF TOG_HUVNR = TRUE THEN uppvaltemp.DELNRKOLL = FALSE.
   IF TOG_HUVNR = FALSE THEN uppvaltemp.DELNRKOLL = TRUE.    
   ASSIGN         
   uppvaltemp.ENDBEST = ?  
   /* uppvaltemp.DELNRKOLL = TRUE*/
   uppvaltemp.VISPERAR = TRUE
   uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
   uppvaltemp.SLUTDATUM  = TODAY
   uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
   uppvaltemp.AVSLUTSLUT = TODAY
   uppvaltemp.TILLFALLFAST = 1
   SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
   uppvaltemp.BEREDARE = "ALLA"
   uppvaltemp.ARBANSVARIG = "ALLA"
   uppvaltemp.BESTNAMN = "ALLA"
   uppvaltemp.OMRNAMN = CMB_OMR
   uppvaltemp.AVDNAMN = "ALLA"
   uppvaltemp.AVDNR = "ALLA"
   uppvaltemp.FAKTTYP = "ALLA"
   uppvaltemp.PAAV = 1.
   ASSIGN
   TOG_PAGA = TRUE      
   TOG_AVSLUTADE = FALSE 
   TOG_TILLF = TRUE     
   TOG_FASTA = FALSE.     
   {TOGTIFA.I}
   ASSIGN
   FILL-IN-MELL = "mellan" 
   FILL-IN-OCH = "och".
   RUN enable_UI.     
   {FRMSIZE.I}   
   CMB_ARBART = "ALLA".
   FIND FIRST aarttemp NO-ERROR.
   IF AVAILABLE aarttemp THEN DO:   
      CMB_ARBART:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
      CMB_ARBART:SCREEN-VALUE = "Alla".
      ENABLE CMB_ARBART WITH FRAME {&FRAME-NAME}.
   END.
   RUN VISMEDDU.W.
   RUN sek_UI.
   RUN ladda_UI.  
   GET FIRST BRW_VAONR NO-LOCK.
   IF varforetypval[3] >= 1 THEN musz = musz.
   ELSE CMB_FAK:HIDDEN = TRUE.
   RUN goma_UI.
   ASSIGN
   BRW_AONR:HIDDEN = FALSE
   BRW_AOTID        :HIDDEN = TRUE.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globanv = "SEMON" THEN BTN_TIDPLAN:HIDDEN = FALSE.
   ELSE BTN_TIDPLAN:HIDDEN = TRUE.
   {musarrow.i}   
   {WIN_M_SLUT.I}
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.   
   IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF labelvar2 = "" THEN FILL-IN-K2:HIDDEN = TRUE.
   IF labelvar1 = "" THEN FILL-IN-K1:HIDDEN = TRUE.
   IF labelvar1 = "" AND labelvar2 = ""  THEN FILL-IN-KTO:HIDDEN  = TRUE.      
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI C-Win 
PROCEDURE allaao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   
   EMPTY TEMP-TABLE valsoktemp NO-ERROR.      
   EMPTY TEMP-TABLE utsokaonr NO-ERROR.
   CREATE valsoktemp.    
   ASSIGN
   valsoktemp.SOKCHAR[1] = CMB_ARBART.
   /*valsoktemp.SOKCHAR[2] = LC(Guru.Konstanter:gaok) + " till excel".*/
   FIND FIRST aarttemp WHERE aarttemp.ARBBENAMNING = CMB_ARBART USE-INDEX ARBARTKOD NO-LOCK NO-ERROR.
   IF AVAILABLE aarttemp THEN DO:           
      valsoktemp.SOKINT[1] = aarttemp.ARBARTKOD.
   END.
   IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
   valsoktemp.SOKCHAR[3] = "Alla".
   FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.        
   IF AVAILABLE jurperstemp THEN valsoktemp.SOKCHAR[3] = jurperstemp.JUDID.   
   RUN aonrhmt IN aonrapph 
   (INPUT " ",INPUT TABLE valsoktemp,
   INPUT-OUTPUT TABLE uppvaltemp,INPUT-OUTPUT TABLE utsokaonr). 

   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   RUN fetchselrowid_UI IN brwproc[{&LEFT-BROWSE}].
   {musarrow.i}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VAONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[{&LEFT-BROWSE}]  (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[{&LEFT-BROWSE}] (INPUT TRUE).
   RUN dynprogextra IN brwproc[{&RIGHT-BROWSE}]  (INPUT "rowdispextra2_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN brwproc[{&RIGHT-BROWSE}] (INPUT TRUE).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_AOTID:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}]
      (INPUT BRW_AONR:HANDLE, INPUT BRW_VAONR:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   
   IF Guru.Konstanter:appcon THEN DO:
       /*FAKTFOR*/   
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
       /*FAKTFOR*/
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph.
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
   END.
   
   tthandle = TEMP-TABLE valdaao:HANDLE.
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "AONR" /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "AONRTAB" /*Tabellnamn*/
   sparaladdatemp.FALTVALAO = "AONR" /*CHARACTER field*/
   sparaladdatemp.FALTVALDEL = "DELNR" /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/
   RUN laddabrw_UI IN brwproc[{&RIGHT-BROWSE}] 
      (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp). 
   RUN aokomp_UI (INPUT 1).
   
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_ORT:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ORT").
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_SAONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").      
  
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
   IF Guru.Konstanter:aonrsekvar[2] = FALSE THEN DO:
      MESSAGE "Du är inte behörig att göra ändringar på " LC(Guru.Konstanter:gaol) "."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF NOT AVAILABLE valdaao THEN RETURN.
   ASSIGN
   aonrvar = valdaao.AONR
   valnr = valdaao.AONR
   delnrvar = valdaao.DELNR
   valdarec = RECID(valdaao).
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   {muswait.i}
   {NYTTAONR.I}   
   RUN andraort IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE evaldaao).
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
   evaldaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   BUFFER-COPY evaldaao TO valdaao.
   RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.

   /* undernummer skall ha samma område och beställare som huvudnumret för tidskrivande
   företag*/  
   IF varforetypval[2] = 0 THEN DO:   
      IF valdaao.DELNR = 0 THEN RUN omrkoll_UI (INPUT valdaao.OMRADE, INPUT valdaao.BESTID).
   END.
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   RUN gorlika_UI IN brwproc[{&ARROWS}] (INPUT "AONR", INPUT "DELNR").
   RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}]. 
   RUN relaonr IN aonrapph.   
   {AVBFRAM.I}
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokomp_UI C-Win 
PROCEDURE aokomp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FOR EACH valdaao NO-LOCK:
         RUN komplettaonr IN aonrapph 
         (INPUT valdaao.AONR,INPUT valdaao.DELNR,
         OUTPUT aonrrec,OUTPUT projvar,OUTPUT bortvar).
         ASSIGN
         valdaao.PROJEKTOR = projvar
         valdaao.AONRREC = aonrrec
         valdaao.TABORT = bortvar.      
      END.
      FOR EACH valdaao,
      EACH omrtemp WHERE omrtemp.OMRADE = valdaao.OMRADE:
         valdaao.AVDELNINGNR = omrtemp.AVDELNINGNR.
      END.
   END.
   ELSE IF vad = 2 THEN DO:
      RUN komplettaonr IN aonrapph 
         (INPUT valdaao.AONR,INPUT valdaao.DELNR,
         OUTPUT aonrrec,OUTPUT projvar,OUTPUT bortvar).
      ASSIGN
      valdaao.PROJEKTOR = projvar
      valdaao.AONRREC = aonrrec
      valdaao.TABORT = bortvar.      
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = valdaao.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         valdaao.AVDELNINGNR = omrtemp.AVDELNINGNR.
      END.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avfram_UI C-Win 
PROCEDURE avfram_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {AVBFRAM.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avrop_UI C-Win 
PROCEDURE avrop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF NOT AVAILABLE valdaao THEN RETURN.
   RAD_FAST = valdaao.FASTAAONR.
   valdarec = RECID(valdaao).
   valdarec2 = valdarec.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec. 
   CREATE uppaonrtemp.
   BUFFER-COPY valdaao TO uppaonrtemp.
   {muswait.i}       
   {AVBGOM.I}
   RUN AVROPU.W (INPUT aonrvar,INPUT delnrvar).
   {AVBFRAM.I}
   FIND FIRST uppaonrtemp NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp THEN DELETE uppaonrtemp.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI C-Win 
PROCEDURE avsluta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF NOT AVAILABLE valdaao THEN RETURN.
   IF AVAILABLE valdaao THEN DO:
      ASSIGN
      aonrvar = valdaao.AONR
      delnrvar = valdaao.DELNR.
      RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
      ASSIGN aonrrec2 = aonrrec. 
      ASSIGN
      valdarec = RECID(valdaao)
      valdarec2 = valdarec.   
      {AVBGOM.I}
      RUN hursortq_UI IN brwproc[{&RIGHT-BROWSE}] (OUTPUT qsort).
      RUN AVSLUTAONR.W (INPUT-OUTPUT TABLE valdaao).
      {AVBFRAM.I}
    
   END.
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ber_UI C-Win 
PROCEDURE ber_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF NOT AVAILABLE valdaao THEN RETURN.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR
   RAD_FAST = valdaao.FASTAAONR
   valdarec = RECID(valdaao)
   valdarec2 = valdarec.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   EMPTY TEMP-TABLE berintemp NO-ERROR.    
   FOR EACH valdaao:
      CREATE berintemp.
      BUFFER-COPY valdaao TO berintemp.
   END.
   {AVBGOM.I}
   RUN MENYBEREU.W (INPUT 2, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp).
   {AVBFRAM.I}      
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END. 
   FIND valdaao WHERE RECID(valdaao) = valdarec2 NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].   
   END.

   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI C-Win 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   valdarec2 = RECID(valdaao)
   valtrecid = RECID(valdaao)
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR.
   IF valdaao.PLANNR = ? OR valdaao.PLANNR = "" THEN musz = musz.
   ELSE DO:  
      MESSAGE Guru.Konstanter:gaok + "är kopplat till " Guru.Konstanter:gplk + " " valdaao.PLANNR + " årtal: " + STRING(valdaao.ARTAL) 
      VIEW-AS ALERT-BOX. 
      RETURN.
   END.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   musz = FALSE.
   /*Niklas 2004-11-18*/
   /*
   {AVBGOM.I}
   RUN TABORTAO.W (INPUT aonrvar,INPUT delnrvar).
   {AVBFRAM.I}
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   */
   MESSAGE "Vill du ta bort " + Guru.Konstanter:gaok + ":" aonrvar + " Delnr:" + STRING(delnrvar)  " med benämning:" valdaao.ORT "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
   IF svar = FALSE THEN RETURN.
   {muswait.i}
   RUN btnok IN bortaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT musz).
   IF musz = TRUE THEN DO:      
      MESSAGE Guru.Konstanter:gaol + " har tidskrivning och kan ej tas bort."      
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      musz = FALSE.
      SESSION:SET-WAIT-STATE("").
      RETURN.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN BOAOAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT aonrrec, OUTPUT musz).
   END.
   ELSE DO:
      RUN BOAOAPP.P
      (INPUT aonrrec, OUTPUT musz).
   END.   
   IF musz = TRUE THEN DO:      
      MESSAGE Guru.Konstanter:gaol " har koppling(ar) som måste tas bort innan borttag är möjligt."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      musz = FALSE.
      SESSION:SET-WAIT-STATE("").
      RETURN.
   END.   
   IF musz = FALSE THEN DO:
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.       
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBAONR"                   
      inextrakopptemp.KOPPLACHAR1 = aonrvar
      inextrakopptemp.KOPPLAINT1 =  delnrvar      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "AOTIDPERS"
      inextrakopptemp.KOPPLACHAR1 = aonrvar    
      inextrakopptemp.KOPPLAINT1 = delnrvar 
      inextrakopptemp.KOPPLACHAR2 = ?
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).   
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "AODAGBOK"                   
      inextrakopptemp.KOPPLACHAR1 = aonrvar               
      inextrakopptemp.KOPPLAINT1 = delnrvar
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.   
      RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp). 
      RUN refbort IN aonrapph (INPUT aonrvar,INPUT delnrvar).
   END.   
  
   IF musz = FALSE THEN DO:        
      RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
      /*
      status-ok = {&BROWSE-NAME}:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN valdarec2 = RECID(valdaao).
      ELSE DO:          
         status-ok = {&BROWSE-NAME}:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
         IF status-ok = TRUE THEN valdarec2 = RECID(valdaao).          
         ELSE valdarec2 = valdarec.
      END.
      */
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
         IF Guru.Konstanter:appcon THEN DO:
            RUN MEDDAONR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT aonrrec, INPUT 2,INPUT "",INPUT "",INPUT "",OUTPUT skick,OUTPUT efel).
         END.         
      END.
      /*
      FIND valdaao WHERE RECID(valdaao) = valtrecid NO-ERROR.
      */
      FIND utsokaonr WHERE utsokaonr.AONR = valdaao.AONR AND
      utsokaonr.DELNR = valdaao.DELNR NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         DELETE utsokaonr.
         RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
      END.
      DELETE valdaao.       
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      /*
      FIND valdaao WHERE RECID(valdaao) = valdarec2
      NO-LOCK NO-ERROR.         
      
      IF AVAILABLE valdaao THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).              
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].                   
      END.      
      */
   END.
   musz = FALSE. 
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytaonr_UI C-Win 
PROCEDURE bytaonr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   IF NOT AVAILABLE valdaao THEN RETURN.
   RAD_FAST = valdaao.FASTAAONR.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR.
   IF valdaao.DELNR NE 0 THEN DO:
      MESSAGE "Byte måste göras för huvudnumret. Det vill säga delnr 0. Samtliga
      undernummer byts då."  VIEW-AS ALERT-BOX.
      RETURN.
   END.
   RUN bytaokoll IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN.
   END.  
   {muswait.i}
   valdarec2 = RECID(valdaao).
   kalkrec = ?.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   {AVBGOM.I}            
   RUN NYTTAONRU.W (INPUT TRUE,INPUT TRUE, INPUT 3, 
                   INPUT-OUTPUT aonrvar, INPUT-OUTPUT delnrvar, INPUT ?,INPUT ? ).   
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:
      FIND FIRST valdaao WHERE RECID(valdaao) = valdarec2 NO-LOCK NO-ERROR.
      RUN nyao_UI (INPUT FALSE).
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      /*Om vänstra browsern ska uppdateras med det nya aonr:et*/
/*       IF AVAILABLE valdaao THEN DO:                                     */
/*          FIND FIRST utsokaonr WHERE utsokaonr.AONR = valdaao.AONR AND */
/*          utsokaonr.DELNR = valdaao.DELNR NO-LOCK NO-ERROR.             */
/*          IF NOT AVAILABLE utsokaonr THEN DO:                               */
/*             CREATE utsokaonr.                                              */
/*          END.                                                               */
/*          BUFFER-COPY valdaao TO utsokaonr.                             */
/*       END.                                                                  */
/*       RUN gorlika_UI IN brwproc[{&ARROWS}] (INPUT "AONR", INPUT "DELNR"). */
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].      
   END.
   RUN relaonr IN aonrapph.
   musz = FALSE.
   {musarrow.i}
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
  DISPLAY FILL-IN-MELL FILL-IN-AVSTARTD FILL-IN-OCH FILL-IN-AVSLUTD TOG_PAGA 
          TOG_AVSLUTADE TOG_TILLF TOG_FASTA TOG_HUVNR CMB_JURP CMB_PROJ CMB_FAK 
          CMB_AVD CMB_BERE FILL-IN-K1 CMB_OMR CMB_ANSV FILL-IN-K2 CMB_BESORG 
          FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR FILL-IN-REF 
          FILL-IN-AOTEXT FILL-IN-KTO FILL-IN-AOTEXT-2 FILL-IN-AOTEXT-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTN_ALLOVER RECT-50 BTN_ALLBACK RECT-57 RECT-60 BTN_BACK BTN_AVB 
         BTN_OVER BTN_TIDPLAN BRW_AOTID BTN_NVE-3 BTN_NVE-4 FILL-IN-AVSTARTD 
         FILL-IN-AVSLUTD TOG_PAGA TOG_AVSLUTADE BTN_FVE-3 BTN_FVE-4 TOG_TILLF 
         TOG_FASTA TOG_HUVNR CMB_JURP CMB_PROJ CMB_FAK CMB_AVD CMB_BERE 
         FILL-IN-K1 CMB_OMR CMB_ANSV FILL-IN-K2 CMB_BESORG BTN_HAMT BRW_AONR 
         BRW_VAONR FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR 
         FILL-IN-REF 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fakt_UI C-Win 
PROCEDURE fakt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE faonrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fdelnr AS INTEGER NO-UNDO.
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE valdaao THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ASSIGN
   faonrvar  = valdaao.AONR 
   fdelnr = valdaao.DELNR
   vartpro = "".
   IF varforetypval[9] = 1 THEN fdelnr = 0.
   EMPTY TEMP-TABLE faktintemp NO-ERROR.    
   GET FIRST BRW_VAONR NO-LOCK.
   DO WHILE AVAILABLE(valdaao): 
      IF varforetypval[9] = 1 THEN DO:
         FIND FIRST faktintemp WHERE faktintemp.AONR = valdaao.AONR AND faktintemp.DELNR = 0
         NO-ERROR.
         IF NOT AVAILABLE faktintemp THEN DO:
            CREATE faktintemp.
            ASSIGN
            faktintemp.ORT  = valdaao.ORT
            faktintemp.AONR = valdaao.AONR    
            faktintemp.DELNR = 0.  
         END.
      END.
      ELSE DO:
         CREATE faktintemp.
         ASSIGN
         faktintemp.ORT  = valdaao.ORT
         faktintemp.AONR = valdaao.AONR    
         faktintemp.DELNR = valdaao.DELNR.  
      END.
      
      GET NEXT BRW_VAONR NO-LOCK.
   END.
   {AVBGOM.I}
   RUN MENYFAKTU.w (INPUT 2, INPUT faonrvar, INPUT fdelnr, INPUT TABLE faktintemp).
   {AVBFRAM.I}
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_AVSLUTADE = TRUE THEN DO:
      ASSIGN
      BTN_FVE-3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_FVE-4:HIDDEN = FALSE 
      BTN_NVE-3:HIDDEN = FALSE 
      BTN_NVE-4:HIDDEN = FALSE
      FILL-IN-MELL:HIDDEN = FALSE 
      FILL-IN-OCH:HIDDEN = FALSE 
      FILL-IN-AVSLUTD:HIDDEN = FALSE 
      FILL-IN-AVSTARTD:HIDDEN = FALSE.       
   END.
   ELSE DO:
      ASSIGN
      BTN_FVE-3:HIDDEN = TRUE 
      BTN_FVE-4:HIDDEN = TRUE 
      BTN_NVE-3:HIDDEN = TRUE 
      BTN_NVE-4:HIDDEN = TRUE 
      FILL-IN-MELL:HIDDEN = TRUE 
      FILL-IN-OCH:HIDDEN = TRUE 
      FILL-IN-AVSLUTD:HIDDEN = TRUE 
      FILL-IN-AVSTARTD:HIDDEN = TRUE.
   END.
   DISPLAY TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalk_UI C-Win 
PROCEDURE kalk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE kalkaonrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalkdelnr AS INTEGER NO-UNDO.
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE valdaao THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ASSIGN
   kalkaonrvar  = valdaao.AONR 
   kalkdelnr = valdaao.DELNR
   RAD_FAST = valdaao.FASTAAONR
   valdarec = RECID(valdaao)
   valdarec2 = valdarec       
   vartpro = "".
   EMPTY TEMP-TABLE kalkintemp NO-ERROR.    
   GET FIRST BRW_VAONR NO-LOCK.
   DO WHILE AVAILABLE(valdaao): 
      CREATE kalkintemp.
      ASSIGN
      kalkintemp.AONR = valdaao.AONR    
      kalkintemp.DELNR = valdaao.DELNR.  
      IF kalkaonrvar = valdaao.AONR AND kalkdelnr = valdaao.DELNR THEN kalkintemp.REPO = TRUE.               
      GET NEXT BRW_VAONR NO-LOCK.
   END.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   {AVBGOM.I}
   RUN MENYKALKU.W (INPUT 2,INPUT kalknrvar, INPUT kalktypvar,INPUT TABLE kalkintemp).
   kalknrvar  = ?.
   kalktypvar = ?.
   {AVBFRAM.I}
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopi_UI C-Win 
PROCEDURE kopi_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   RAD_FAST = valdaao.FASTAAONR.
   {muswait.i}
   valdarec2 = RECID(valdaao).
   kalkrec = ?.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   {AVBGOM.I}            
   RUN NYTTAONRU.W (INPUT TRUE,INPUT TRUE, INPUT 2,
                   INPUT-OUTPUT aonrvar, INPUT-OUTPUT delnrvar, INPUT ?,INPUT ?).
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:   
      RUN nyao_UI (INPUT FALSE).
      RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}]. 
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR. 
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}]. 
   END.
   RUN relaonr IN aonrapph.
   musz = FALSE.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kost_UI C-Win 
PROCEDURE kost_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR
   valdarec = RECID(valdaao)
   valdarec2 = valdarec.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   {muswait.i}
   {AVBGOM.I}
   RUN KOSTANDRA.W (INPUT aonrvar,INPUT delnrvar).
   
   {AVBFRAM.I}         
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND faktyptemp WHERE faktyptemp.FAKTTYP = uppvaltemp.FAKTTYP NO-ERROR.
   CMB_FAK = faktyptemp.VIFAKTTYP.
   ASSIGN     
   /*CMB_ARTAL = YEAR(uppvaltemp.STARTDATUM)*/ 
   CMB_BESORG = uppvaltemp.BESTNAMN
   CMB_OMR = uppvaltemp.OMRNAMN
   FILL-IN-AVSLUTD = uppvaltemp.AVSLUTSLUT 
   FILL-IN-AVSTARTD = uppvaltemp.AVSLUTSTART
   CMB_PROJ = SUBSTRING(uppvaltemp.PROJEKTOR,1,24)
   CMB_ANSV = uppvaltemp.ARBANSVARIG 
   CMB_BERE = uppvaltemp.BESTNAMN.
   IF uppvaltemp.PAAV = 3 THEN DO:
      ASSIGN
      TOG_PAGA = TRUE     
      TOG_AVSLUTADE = TRUE.
   END.
   ELSE IF uppvaltemp.PAAV = 2 THEN DO:
      ASSIGN
      TOG_PAGA = FALSE     
      TOG_AVSLUTADE = TRUE.
   END.
   ELSE IF uppvaltemp.PAAV = 1 THEN DO:
      ASSIGN
      TOG_PAGA = TRUE     
      TOG_AVSLUTADE = FALSE.
   END.
   ELSE DO:
      ASSIGN
      TOG_PAGA = FALSE     
      TOG_AVSLUTADE = FALSE.
   END.
   IF uppvaltemp.TILLFALLFAST = 3 THEN DO:
      ASSIGN
      TOG_TILLF = TRUE     
      TOG_FASTA = TRUE.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 2 THEN DO:
      ASSIGN
      TOG_TILLF = FALSE     
      TOG_FASTA = TRUE.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 1 THEN DO:
      ASSIGN
      TOG_TILLF = TRUE     
      TOG_FASTA = FALSE.
   END.
   ELSE DO:
      ASSIGN
      TOG_TILLF = FALSE     
      TOG_FASTA = FALSE.
   END.   
   DISPLAY CMB_OMR CMB_BESORG CMB_FAK CMB_ANSV CMB_BERE CMB_PROJ FILL-IN-AVSLUTD
   FILL-IN-AVSTARTD   
   TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA WITH FRAME {&FRAME-NAME}.
/*    RUN goma_UI. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main1_UI C-Win 
PROCEDURE main1_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANSBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
   END.
   ELSE DO:
      RUN ANSBER.P 
      (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ARBARTS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE aarttemp).
   END.
   ELSE DO:
      RUN ARBARTS.P 
      (OUTPUT TABLE aarttemp).                  
   END.
    musz = FALSE.
   status-ok = CMB_ANSV:ADD-FIRST("Alla") IN FRAME {&FRAME-NAME}. 
   FOR EACH ansvaraotemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_ANSV:ADD-LAST(ansvaraotemp.NAMN).
   END.   
   status-ok = CMB_PROJ:ADD-FIRST("Alla").
   FOR EACH projtemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_PROJ:ADD-LAST(projtemp.NAMN).
   END.
   status-ok = CMB_BERE:ADD-FIRST("Alla").
   FOR EACH beredartemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_BERE:ADD-LAST(beredartemp.NAMN).
   END.
   FOR EACH faktyptemp : 
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP).      
   END.   
   status-ok = CMB_ARBART:ADD-FIRST("Alla").
   FOR EACH aarttemp USE-INDEX ARBARTKOD NO-LOCK:
      status-ok = CMB_ARBART:ADD-LAST(aarttemp.ARBBENAMNING).
   END.
   RUN labelk2 IN aonrapph (OUTPUT labelvar2).   
   ASSIGN
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL= Guru.Konstanter:gavdk
   FILL-IN-K2:LABEL = labelvar2
   FILL-IN-AOTEXT = "Gör urval av " + LC(Guru.Konstanter:gaol) +  ":"
   BTN_ALLBACK:TOOLTIP = "Alla valda " + LC(Guru.Konstanter:gaok) + " tas bort från vallistan"   
   BTN_ALLOVER:TOOLTIP = "Alla " + LC(Guru.Konstanter:gaok) + " väljs"
   FBTN_AVSAONR:TOOLTIP = "Avsluta " + LC(Guru.Konstanter:gaok) + ":et. Ingen tidskrivning skall kan registreras mera"
   BTN_BORT:TOOLTIP = "Ta bort ett " + LC(Guru.Konstanter:gaok)
   BTN_HAMT:TOOLTIP = "Dina val ovan avgör vilka " + LC(Guru.Konstanter:gaok) +  " du får i listan nedan."
   BTN_NY:TOOLTIP = "Skapa ett nytt " + LC(Guru.Konstanter:gaok)
   FBTN_UNDER:TOOLTIP = "Underindela dvs skapa nya delnrr kopierar ett " + LC(Guru.Konstanter:gaok)
   FBTN_KOPI:TOOLTIP = "Kopierar ett " + LC(Guru.Konstanter:gaok)
   FBTN_VISAO:TOOLTIP = "Visa/skriv ut allt som rör " + LC(Guru.Konstanter:gaok) + ":et"
   FBTN_UPP:TOOLTIP = "Här kan du ändra allt som rör " + LC(Guru.Konstanter:gaok) + ":et"
   FILL-IN_EAONR:TOOLTIP = "Tryck RETURN här för välja enstaka " + LC(Guru.Konstanter:gaok) 
   FILL-IN_SAONR:LABEL = Guru.Konstanter:gaok
   FILL-IN_EAONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-REF:LABEL = Guru.Konstanter:grefbefk
   CMB_PROJ:LABEL = gprojl
   CMB_BERE:LABEL = Guru.Konstanter:gberek 
   CMB_ANSV:LABEL = Guru.Konstanter:garbal
   CMB_OMR:LABEL = Guru.Konstanter:gomrk   
   CMB_BESORG:LABEL = RIGHT-TRIM(SUBSTRIN(Guru.Konstanter:gbestk,1,12)).
   RUN cellabel_UI IN brwproc[1] (INPUT "BEREDARE", INPUT Guru.Konstanter:gberek).
   RUN cellabel_UI IN brwproc[2] (INPUT "BEREDARE", INPUT Guru.Konstanter:gberek).
   /*
   ASSIGN
   utsokaonr.BEREDARE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gberek 
   valdaao.BEREDARE:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gberek.
   */
      /*
   utsokaonr.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   utsokaonr.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok 
   valdaao.AONR:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gaok. 
   */
   RUN labelk1 IN aonrapph (OUTPUT labelvar1).
   FILL-IN-K1:LABEL = labelvar1.   
   ASSIGN
   FBTN_UPP:LABEL = Guru.Konstanter:gaok + " huvud" 
   FBTN_BYTPNR:LABEL = "Byt " + Guru.Konstanter:gaok  
   FBTN_AVSAONR:LABEL = "Avsluta " + LC(Guru.Konstanter:gaok).  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mark_UI C-Win 
PROCEDURE mark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE valdaao THEN RETURN.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   RAD_FAST = valdaao.FASTAAONR.
   ASSIGN
   aonrvar = valdaao.AONR
   delnrvar = valdaao.DELNR
   valdarec = RECID(valdaao)
   valdarec2 = valdarec.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   ASSIGN aonrrec2 = aonrrec.
   EMPTY TEMP-TABLE markintemp NO-ERROR.    
   FOR EACH valdaao:
      CREATE markintemp.
      BUFFER-COPY valdaao TO markintemp.
   END.
   {AVBGOM.I}
   RUN MENYMARKUN.W (INPUT 2, INPUT aonrvar,INPUT delnrvar,INPUT TABLE markintemp).  
   {AVBFRAM.I}      
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END. 
   FIND valdaao WHERE RECID(valdaao) = valdarec2 NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].   
      /*
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = aonrvar AND 
      utsokaonr.DELNR = delnrvar NO-LOCK NO-ERROR.       
      BUFFER-COPY valdaao TO utsokaonr.  
      RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").      
      */
   END.      
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyao_UI C-Win 
PROCEDURE nyao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER andradelnr AS LOGICAL NO-UNDO.
   IF musz = FALSE THEN musz = musz.      
   ELSE DO:
      RUN avfram_UI.
      RUN delaonr IN aonrapph (INPUT aonrvar, INPUT delnrvar).      
      musz = FALSE.
      RETURN.
   END.
   /*FORETIDLAGE*/
   status-ok = SESSION:SET-WAIT-STATE("GENERAL").
   {FORETIDL.I}
   {AVBFRAM.I}     
   IF musz = FALSE THEN DO:
      IF andradelnr = TRUE THEN DO:
         ASSIGN delnrvar = hjdelvar.
         andradelnr = FALSE.
      END.            
      RUN skapaonr IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE evaldaao).       
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdaao THEN CREATE valdaao.
      FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
      evaldaao.DELNR = delnrvar NO-LOCK NO-ERROR.      
      BUFFER-COPY evaldaao TO valdaao.
      RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}]. 
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR. 
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}]. 
   END.
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
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   kalkrec = ?.
   {muswait.i}
   {AVBGOM.I}
      
   IF musz = FALSE THEN DO:
      RUN NYTTAONRU.W (INPUT TRUE,INPUT FALSE, INPUT 1, 
                      INPUT-OUTPUT aonrvar, INPUT-OUTPUT delnrvar, INPUT ?,INPUT ? ). 
      
   END.   
   RUN nyao_UI (INPUT FALSE).
   RUN relaonr IN aonrapph.
   /*IF Guru.Konstanter:globforetag = "XSUND" OR  Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /*Skapa 6 undernummer om aonrserien är 39 eller 78*/
      IF aonrvar BEGINS "39" OR aonrvar BEGINS "78"  THEN DO:
      END.
   END.*/
   IF Guru.Konstanter:globforetag = "SUND"  THEN DO:
      /*GG 2007-07-02 - Alla nya projektnummer ska ha tidplan automatiskt upplagt*/
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
        (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").                  
      END.

      /*CCCCCSkapa 6 undernummer om aonrserien är 39 eller 78*/      
      IF aonrvar BEGINS "39" OR aonrvar BEGINS "78"  THEN DO:
         IF delnrvar = 0 THEN DO:
            fastdelnr = 0.
            REPEAT:            
               fastdelnr = fastdelnr + 1.
               IF fastdelnr > 6 THEN LEAVE.
               RUN underfasta_UI (INPUT fastdelnr).
            END.
            RUN andraort IN aonrapph (INPUT aonrvar,INPUT 0,OUTPUT TABLE evaldaao).
            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = 0 NO-LOCK NO-ERROR.
            FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
            evaldaao.DELNR = 0 NO-LOCK NO-ERROR.
            BUFFER-COPY evaldaao TO valdaao.

            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.
      IF aonrvar BEGINS "34" THEN DO:         
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.
      IF aonrvar BEGINS "38" THEN DO:         
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /*CCCCCSkapa 6 undernummer om aonrserien är 39 eller 78*/      
      IF aonrvar BEGINS "30"  THEN DO:         
         IF delnrvar = 0 THEN DO:
            fastdelnr = 0.            
            REPEAT:            
               fastdelnr = fastdelnr + 1.
               IF fastdelnr > 6 THEN LEAVE.
               RUN underfasta_UI (INPUT fastdelnr).
            END.
            RUN andraort IN aonrapph (INPUT aonrvar,INPUT 0,OUTPUT TABLE evaldaao).
            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = 0 NO-LOCK NO-ERROR.
            FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
            evaldaao.DELNR = 0 NO-LOCK NO-ERROR.
            BUFFER-COPY evaldaao TO valdaao.
            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.
      IF aonrvar BEGINS "31" THEN DO:         
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.
   END.
   musz = FALSE.
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrkoll_UI C-Win 
PROCEDURE omrkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER omrkoll AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bestkoll AS CHARACTER NO-UNDO.
   FOR EACH valdaao WHERE valdaao.AONR = aonrvar AND valdaao.OMRADE NE omrkoll:
      valdaao.OMRADE = omrkoll.
   END.
   FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar AND utsokaonr.OMRADE NE omrkoll:
      utsokaonr.OMRADE = omrkoll.
   END.
   FOR EACH valdaao WHERE valdaao.AONR = aonrvar AND valdaao.BESTID NE bestkoll:
      valdaao.BESTID = bestkoll.
   END.
   FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar AND utsokaonr.BESTID NE bestkoll:
      utsokaonr.BESTID = bestkoll.
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp_UI C-Win 
PROCEDURE rapp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {AVBGOM.I}
   RUN hursortq_UI IN brwproc[{&RIGHT-BROWSE}] (OUTPUT qsort).
   RUN AORAPPORT.W.
   {AVBFRAM.I}
  
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra2_UI C-Win 
PROCEDURE rowdispextra2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   &Scoped-define FORMATNAMNOMR valdaao.OMRADE
   &Scoped-define FORMATNAMNTAB valdaao
   &Scoped-define BROWSE-NAME BRW_VAONR
   {OMRAOFORMATSUND.I} 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI C-Win 
PROCEDURE rowdispextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE 
   &Scoped-define FORMATNAMNTAB utsokaonr
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMATSUND.I}         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI C-Win 
PROCEDURE sek_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   Guru.GlobalaVariabler:collefth = ?.
   IF Guru.Konstanter:aonrsekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:aonrsekvar[2] = TRUE THEN DO:
      ENABLE FBTN_UPP WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_UPP:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   IF Guru.Konstanter:aonrsekvar[5] = TRUE THEN DO:
      ENABLE FBTN_UNDER WITH FRAME {&FRAME-NAME}.
      Guru.GlobalaVariabler:colrighth = FBTN_UNDER:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   IF Guru.Konstanter:aonrsekvar[5] = TRUE THEN DO:
      ENABLE FBTN_KOPI WITH FRAME {&FRAME-NAME}.
      Guru.GlobalaVariabler:colrighth = FBTN_KOPI:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
            
   END.
   IF Guru.Konstanter:aonrsekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
   IF Guru.Konstanter:aonrsekvar[4] = TRUE THEN DO:
      ENABLE FBTN_VISAO WITH FRAME {&FRAME-NAME}.     
      Guru.GlobalaVariabler:colrighth = FBTN_VISAO:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
                 
   END.
   IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
      ENABLE FBTN_AVSAONR WITH FRAME {&FRAME-NAME}.           
      Guru.GlobalaVariabler:colrighth = FBTN_AVSAONR:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   IF Guru.Konstanter:aonrsekvar[7] = TRUE THEN DO:
      ENABLE FBTN_RAPP WITH FRAME {&FRAME-NAME}.              
      Guru.GlobalaVariabler:colrighth = FBTN_RAPP:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   IF Guru.Konstanter:aonrsekvar[8] = TRUE THEN DO:
      ENABLE FBTN_KOST WITH FRAME {&FRAME-NAME}.              
      Guru.GlobalaVariabler:colrighth = FBTN_KOST:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.  
   IF Guru.Konstanter:aonrsekvar[11] = TRUE THEN DO:
      ENABLE FBTN_BYTPNR WITH FRAME {&FRAME-NAME}.           
      Guru.GlobalaVariabler:colrighth = FBTN_BYTPNR:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   Guru.GlobalaVariabler:collefth = ?.
   /*IF Guru.Konstanter:aonrsekvar[10] = TRUE THEN ENABLE BTN_VISAV WITH FRAME {&FRAME-NAME}.   */          
   IF Guru.Konstanter:hoppsekvar[2] = TRUE THEN DO:
      IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
         ENABLE FBTN_BER WITH FRAME {&FRAME-NAME}.
         Guru.GlobalaVariabler:colrighth = FBTN_BER:HANDLE.           
         RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
         
      END.   
   END.
   IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:
      ENABLE FBTN_KALK WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_KALK:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END. 
   IF Guru.Konstanter:hoppsekvar[12] = TRUE THEN DO:         
      ENABLE FBTN_MARK WITH FRAME {&FRAME-NAME}.           
      Guru.GlobalaVariabler:colrighth = FBTN_MARK:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   
   IF Guru.Konstanter:hoppsekvar[10] = TRUE AND varforetypval[3] = 2 THEN DO: 
      ENABLE FBTN_FAKT WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_FAKT:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" THEN DO:
      IF Guru.Konstanter:aonrsekvar[1] = TRUE THEN DO:
         ENABLE FBTN_AVROP WITH FRAME {&FRAME-NAME}.
         Guru.GlobalaVariabler:colrighth = FBTN_AVROP:HANDLE.           
         RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
         
      END.
   END.   
   ENABLE FBTN_STATUS WITH FRAME {&FRAME-NAME}.
   Guru.GlobalaVariabler:colrighth = FBTN_STATUS:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE underfasta_UI C-Win 
PROCEDURE underfasta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER fastdelnr AS INTEGER NO-UNDO.      
   {muswait.i}
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND valdaao.DELNR = 0 NO-LOCK NO-ERROR.
   ASSIGN 
   hjdelvar = fastdelnr
   valnr = valdaao.AONR.
   /* HÄMTA RECID AONRTAB - TEMPORÄR LÖSNING */
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).       
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" THEN DO:
      IF delnrvar = 0 THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.
   END.
   RUN under IN aonrapph (INPUT aonrrec,INPUT hjdelvar).   
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT hjdelvar,OUTPUT aonrrec).    
   RUN nyao_UI (INPUT TRUE).   
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   musz = FALSE.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).    
   /*istället för NYTTAONR.i, utan run NYTTAONR.W*/
   {NYTTAONRFAST.I}
   RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].        
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      RUN andraort IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE evaldaao).
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
      evaldaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      BUFFER-COPY evaldaao TO valdaao.
      RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].      
   END.
   RUN relaonr IN aonrapph.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE under_UI C-Win 
PROCEDURE under_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE valdaao THEN RETURN.
   RUN hamtaavslut IN aonrapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,OUTPUT aoavslut). 
   valdaao.AONRAVDATUM = aoavslut.
   IF aoavslut NE 01/01/91 THEN DO:
      MESSAGE "Det går ej att underindela ett avslutat " Guru.Konstanter:gaok "!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF Guru.Konstanter:aonrsekvar[2] = FALSE THEN DO:
      MESSAGE "Du är inte behörig att göra ändringar på " LC(Guru.Konstanter:gaol) "."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   {muswait.i}
   ASSIGN 
   valdarec = RECID(valdaao)
   aonrvar = valdaao.AONR
   valnr = valdaao.AONR
   delnrvar = valdaao.DELNR.
   /* HÄMTA RECID AONRTAB - TEMPORÄR LÖSNING */
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec). 
   /* sista delnr? */
   RUN sistadelnr IN aonrapph (INPUT aonrvar,INPUT valdaao.OMRADE,OUTPUT hjdelvar).
   hjdelvar = hjdelvar + 1.
   {AVBGOM.I}
   RUN DELNRAND.W (INPUT-OUTPUT hjdelvar, OUTPUT musz, INPUT aonrvar).
   {AVBFRAM.I}
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.      
   DEBUGGER:SET-BREAK().
   RUN under IN aonrapph (INPUT aonrrec,INPUT hjdelvar).   
   
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT hjdelvar,OUTPUT aonrrec).    
   RUN nyao_UI (INPUT TRUE).      
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      MESSAGE "Underindelat " LC(Guru.Konstanter:gaok) " lagrat. Gör ändringar via - " Guru.Konstanter:gaok " huvud."
      VIEW-AS ALERT-BOX.    
   END.
   valdarec = RECID(valdaao).
   RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].    
   musz = FALSE.
   RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).    
   {NYTTAONR.I}
   RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].   
   FIND FIRST valdaao WHERE RECID(valdaao) = valdarec NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      RUN andraort IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE evaldaao).
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      FIND FIRST evaldaao WHERE evaldaao.AONR = aonrvar AND 
      evaldaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      BUFFER-COPY evaldaao TO valdaao.
      RUN refreshbrw_UI IN brwproc[{&RIGHT-BROWSE}].
      FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
      valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].      
   END.
   RUN relaonr IN aonrapph.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI C-Win 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN   
   CMB_ANSV = INPUT FRAME {&FRAME-NAME} CMB_ANSV 
   CMB_BERE = INPUT FRAME {&FRAME-NAME} CMB_BERE  
   CMB_PROJ = INPUT FRAME {&FRAME-NAME} CMB_PROJ  
   CMB_BESORG = INPUT FRAME {&FRAME-NAME} CMB_BESORG
   CMB_FAK = INPUT FRAME {&FRAME-NAME} CMB_FAK   
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR
   CMB_ARBART = INPUT FRAME {&FRAME-NAME} CMB_ARBART
   FILL-IN-K1 = INPUT FILL-IN-K1
   FILL-IN-K2 = INPUT FILL-IN-K2.
   IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   FIND FIRST uppvaltemp NO-ERROR.      
   ASSIGN         
   uppvaltemp.VISPERAR = FALSE
   uppvaltemp.AVSLUTSTART = FILL-IN-AVSTARTD
   uppvaltemp.AVSLUTSLUT = FILL-IN-AVSLUTD   
   uppvaltemp.BESTNAMN = CMB_BESORG
   uppvaltemp.BEREDARE = CMB_BERE
   SUBSTRING(uppvaltemp.PROJEKTOR,1,24) = CMB_PROJ
   uppvaltemp.ARBANSVARIG = CMB_ANSV
   uppvaltemp.AVDNAMN = CMB_AVD.
   
   IF TOG_PAGA = TRUE AND TOG_AVSLUTADE = TRUE THEN DO:
      uppvaltemp.PAAV = 3.
   END.
   ELSE IF TOG_PAGA = FALSE AND TOG_AVSLUTADE = TRUE THEN DO:
      uppvaltemp.PAAV = 2.
   END.
   ELSE IF TOG_PAGA = TRUE AND TOG_AVSLUTADE = FALSE THEN DO:
      uppvaltemp.PAAV = 1.
   END.
   ELSE DO:
      ASSIGN
      TOG_PAGA = TRUE 
      TOG_AVSLUTADE = TRUE
      uppvaltemp.PAAV = 3.      
   END.
   IF TOG_TILLF = TRUE AND TOG_FASTA = TRUE THEN DO:
      uppvaltemp.TILLFALLFAST = 3.
   END.
   ELSE IF TOG_TILLF = FALSE AND TOG_FASTA = TRUE THEN DO:
      uppvaltemp.TILLFALLFAST = 2.
   END.
   ELSE IF TOG_TILLF = TRUE AND TOG_FASTA = FALSE THEN DO:
      uppvaltemp.TILLFALLFAST = 1.
   END.
   ELSE DO:
      ASSIGN
      TOG_TILLF = TRUE 
      TOG_FASTA = TRUE
      uppvaltemp.TILLFALLFAST = 3.      
   END.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
   IF AVAILABLE omrtemp THEN uppvaltemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvaltemp.OMRADE = "Alla".

   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_BESORG NO-LOCK NO-ERROR.
   IF AVAILABLE bestkundallt THEN uppvaltemp.BESTID = bestkundallt.BESTID.
   ELSE DO:
      IF CMB_BESORG = "Samma som ovan" THEN uppvaltemp.BESTID = uppvaltemp.OMRADE.
      ELSE uppvaltemp.BESTID = CMB_BESORG. 
   END.

   

   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   uppvaltemp.FAKTTYP = faktyptemp.FAKTTYP.   

   FIND FIRST beredartemp WHERE beredartemp.NAMN = CMB_BERE NO-ERROR.
   IF AVAILABLE beredartemp THEN uppvaltemp.BEREDARE = beredartemp.PERSONALKOD.
   ELSE uppvaltemp.BEREDARE = "ALLA".
   FIND FIRST projtemp WHERE projtemp.NAMN = CMB_PROJ NO-ERROR.
   IF AVAILABLE projtemp THEN SUBSTRING(uppvaltemp.PROJEKTOR,1,24) = projtemp.PERSONALKOD.
   ELSE SUBSTRING(uppvaltemp.PROJEKTOR,1,24) = "ALLA".
   SUBSTRING(uppvaltemp.PROJEKTOR,25,10) = FILL-IN-K2.
   SUBSTRING(uppvaltemp.PROJEKTOR,35,10) = FILL-IN-K1.
   FIND FIRST ansvaraotemp WHERE ansvaraotemp.NAMN = CMB_ANSV NO-ERROR.
   IF AVAILABLE ansvaraotemp THEN uppvaltemp.ARBANSVARIG = ansvaraotemp.PERSONALKOD.
   ELSE uppvaltemp.ARBANSVARIG = "ALLA".
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN uppvaltemp.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE uppvaltemp.AVDNR = "ALLA".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visao_UI C-Win 
PROCEDURE visao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_VAONR
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   IF antal_valda = 0 THEN RETURN.
   {muswait.i}
   antal_raknare = 1.
   DO TRANSACTION:
      DO WHILE antal_raknare LE antal_valda:
         status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).       
         ASSIGN 
         valdarec = RECID(valdaao)
         aonrvar = valdaao.AONR
         delnrvar = valdaao.DELNR.
         CREATE aonrtemp.
         BUFFER-COPY valdaao TO aonrtemp.
         ASSIGN aonrtemp.AONR = valdaao.AONR
         aonrtemp.DELNR = valdaao.DELNR
         aonrtemp.OMRADE = valdaao.OMRADE.
         RUN hamtaorec IN aonrapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
         ASSIGN
         aonrtemp.AONRTABREC = aonrrec.                 
         antal_raknare = antal_raknare + 1.
      END.
   END.        
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "SUND" THEN DO:      
      RUN AORUBRIK.W.
   END.
   {AVBGOM.I}      
   RUN VISAAONR.W.
   EMPTY TEMP-TABLE aonrtemp NO-ERROR.    
   {AVBFRAM.I}
   {musarrow.i}                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   IF leftbrwakt = TRUE THEN 
      antal_valda =   BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   ELSE antal_valda = BRW_VAONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Inget " + LC(Guru.Konstanter:gaok) + " är valt!" VIEW-AS ALERT-BOX.
      RETURN TRUE.                
   END.
   IF leftbrwakt = TRUE THEN DO:
      EMPTY TEMP-TABLE valdaao NO-ERROR.       
      RUN sel_over_UI IN brwproc[{&ARROWS}].
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   END.
   ELSE BRW_VAONR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkbrw C-Win 
FUNCTION checkbrw RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   RETURN leftbrwakt.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

