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

  Created: 11/19/96 -  4:02 pm

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






/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{LAGERTRUM.I}
{AOBESTMTRLDEP.I}

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER uttagbnr AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR lplagertrum.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eaobest_mtrl.
DEFINE NEW SHARED VARIABLE levtyp AS INTEGER NO-UNDO.  
DEFINE NEW SHARED VARIABLE levtill AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE utg AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_best AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE VARIABLE leveransapph AS HANDLE NO-UNDO.
DEFINE VARIABLE leveransaoapph AS HANDLE NO-UNDO.
DEFINE VARIABLE best_nr_koll  AS INTEGER NO-UNDO.
DEFINE VARIABLE lev_nr_koll  AS INTEGER NO-UNDO.
DEFINE VARIABLE best_namn_koll AS CHARACTER NO-UNDO. 
DEFINE VARIABLE danv AS CHARACTER NO-UNDO.
DEFINE VARIABLE lplats AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE depaapph AS HANDLE NO-UNDO.
DEFINE VARIABLE lpls AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyplats AS LOGICAL NO-UNDO.
DEFINE VARIABLE lpl AS INTEGER NO-UNDO.
DEFINE VARIABLE lpc AS CHARACTER NO-UNDO.
DEFINE VARIABLE aobrow AS ROWID.
DEFINE VARIABLE lpvisa AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE kolltxt AS CHARACTER NO-UNDO.
DEFINE VARIABLE redkopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE typbest AS CHARACTER NO-UNDO.
DEFINE VARIABLE annandeptrum AS CHARACTER NO-UNDO.

&Scoped-define NEW   
&Scoped-define SHARED  
{bestnrtab.I}
{LEVNRTAB.I}
{AObestnrtab.I}
{AOLEVNRTAB.I}
{LAGERPLKOPP.I}
{UAOTEMP.I}
DEFINE BUFFER lagerplkoppbuff FOR lagerplkopp.
DEFINE TEMP-TABLE utnr NO-UNDO
    FIELD BESTNR AS INTEGER   
    FIELD DEPNR AS INTEGER
    FIELD PROJDEPNR AS INTEGER. 


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AOBEST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aobest_nr_tab aolev_nr_tab best_nr_tab ~
lev_nr_tab LAGERPLKOPP uaotemp

/* Definitions for BROWSE BRW_AOBEST                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_AOBEST aobest_nr_tab.LEVERANSDATUM ~
aobest_nr_tab.AONRAONR aobest_nr_tab.AONRDELNR aobest_nr_tab.Depnr ~
aobest_nr_tab.Bestnr aobest_nr_tab.TRUMMAPLATS aobest_nr_tab.INKBESTID ~
aobest_nr_tab.ORDERNUMMER aobest_nr_tab.Bestdatum aobest_nr_tab.KLOCKAN ~
aobest_nr_tab.BESTALLARE aobest_nr_tab.LEVNAMN aobest_nr_tab.REST ~
aobest_nr_tab.LAGERPLATSI aobest_nr_tab.LAGERPLATSC aobest_nr_tab.TRUMMANR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOBEST 
&Scoped-define QUERY-STRING-BRW_AOBEST FOR EACH aobest_nr_tab NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AOBEST OPEN QUERY BRW_AOBEST FOR EACH aobest_nr_tab NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AOBEST aobest_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOBEST aobest_nr_tab


/* Definitions for BROWSE BRW_AOLEV                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AOLEV aolev_nr_tab.AONRAONR ~
aolev_nr_tab.AONRDELNR aolev_nr_tab.Depnr aolev_nr_tab.Bestnr ~
aolev_nr_tab.TRUMMAPLATS aolev_nr_tab.INKBESTID aolev_nr_tab.ORDERNUMMER ~
aolev_nr_tab.Bestdatum aolev_nr_tab.KLOCKAN aolev_nr_tab.BESTALLARE ~
aolev_nr_tab.LEVNAMN aolev_nr_tab.LEVERANSDATUM aolev_nr_tab.LEVDATUM ~
aolev_nr_tab.LAGERPLATSI aolev_nr_tab.LAGERPLATSC aolev_nr_tab.UTTAGET ~
aolev_nr_tab.TRUMMANR aolev_nr_tab.UTDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOLEV 
&Scoped-define QUERY-STRING-BRW_AOLEV FOR EACH aolev_nr_tab NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AOLEV OPEN QUERY BRW_AOLEV FOR EACH aolev_nr_tab NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AOLEV aolev_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOLEV aolev_nr_tab


/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST best_nr_tab.Bestnr ~
best_nr_tab.Bestdatum best_nr_tab.LEVNAMN best_nr_tab.REST best_nr_tab.AONR ~
best_nr_tab.DELNR best_nr_tab.BESTALLARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST 
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH best_nr_tab NO-LOCK ~
    BY best_nr_tab.Bestnr ~
       BY best_nr_tab.LEVNAMN
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH best_nr_tab NO-LOCK ~
    BY best_nr_tab.Bestnr ~
       BY best_nr_tab.LEVNAMN.
&Scoped-define TABLES-IN-QUERY-BRW_BEST best_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST best_nr_tab


/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV lev_nr_tab.Bestnr ~
lev_nr_tab.Bestdatum lev_nr_tab.LEVNAMN lev_nr_tab.AONR lev_nr_tab.DELNR ~
lev_nr_tab.BESTALLARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV 
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH lev_nr_tab NO-LOCK ~
    BY lev_nr_tab.Bestnr
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH lev_nr_tab NO-LOCK ~
    BY lev_nr_tab.Bestnr.
&Scoped-define TABLES-IN-QUERY-BRW_LEV lev_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV lev_nr_tab


/* Definitions for BROWSE BRW_LPLATS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_LPLATS LAGERPLKOPP.LAGERPLATSI ~
LAGERPLKOPP.LAGERPLATSC LAGERPLKOPP.AONRAONR LAGERPLKOPP.AONRDELNR ~
LAGERPLKOPP.depnr LAGERPLKOPP.BESTNR LAGERPLKOPP.TRUMMAPLATS ~
LAGERPLKOPP.INKBESTID LAGERPLKOPP.ORDERNUMMER LAGERPLKOPP.Bestdatum ~
LAGERPLKOPP.LEVERANSDATUM LAGERPLKOPP.LEVDATUM LAGERPLKOPP.BESTALLARE ~
LAGERPLKOPP.REST LAGERPLKOPP.UTTAGET LAGERPLKOPP.UTDATUM ~
LAGERPLKOPP.TRUMMANR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LPLATS LAGERPLKOPP.LAGERPLATSC 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LPLATS LAGERPLKOPP
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LPLATS LAGERPLKOPP
&Scoped-define QUERY-STRING-BRW_LPLATS FOR EACH LAGERPLKOPP NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LPLATS OPEN QUERY BRW_LPLATS FOR EACH LAGERPLKOPP NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LPLATS LAGERPLKOPP
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LPLATS LAGERPLKOPP


/* Definitions for BROWSE BRW_UTAO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_UTAO uaotemp.AONR uaotemp.DELNR ~
uaotemp.Depnr uaotemp.Bestnr uaotemp.BESTALLARE uaotemp.Enr ~
uaotemp.BENAMNING uaotemp.Antal uaotemp.LAGERPLATSI uaotemp.LAGERPLATSC 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UTAO 
&Scoped-define QUERY-STRING-BRW_UTAO FOR EACH uaotemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_UTAO OPEN QUERY BRW_UTAO FOR EACH uaotemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_UTAO uaotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UTAO uaotemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL2 IMAGE-16 TOG_VUTT BRW_LPLATS ~
RAD_VAL BRW_LEV BRW_UTAO BRW_BEST BRW_AOBEST BRW_AOLEV btn_over btn_back ~
FBTN_MOT FBTN_VISA BTN_NY BTN_BORT BTN_VILP BTN_UTLP FILL-IN-AAONR ~
FILL-IN-TRUMMAPLATS BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL2 TOG_VUTT RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Ta bort koppling av aonr till vald pplats".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1 TOOLTIP "Ta bort vald pplats".

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1 TOOLTIP "Skapa ny pplats".

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Koppla aonr till vald pplats":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_UTLP 
     LABEL "Till Byggplats" 
     SIZE 16.5 BY 1 TOOLTIP "Skicka markerad beställning/uttag/trumma till Byggplats".

DEFINE BUTTON BTN_VILP 
     LABEL "Visa P-plats" 
     SIZE 16.5 BY 1.

DEFINE BUTTON FBTN_MOT 
     LABEL "Mottagit" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AAONR AS CHARACTER FORMAT "X(40)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRUMMAPLATS AS CHARACTER FORMAT "X(11)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-VPROJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VTRUMLP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VTRUMMA AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.5 BY 1
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-16
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8.5 BY .83.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ej färdiga lev.", 1,
"Färdiga lev.", 2
     SIZE 40.38 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lager", 1,
"LagerProjekt", 2,
"Projektlager", 3
     SIZE 51.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_VUTT AS LOGICAL INITIAL no 
     LABEL "Visa även uttagna P-platser" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.63 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AOBEST FOR 
      aobest_nr_tab SCROLLING.

DEFINE QUERY BRW_AOLEV FOR 
      aolev_nr_tab SCROLLING.

DEFINE QUERY BRW_BEST FOR 
      best_nr_tab SCROLLING.

DEFINE QUERY BRW_LEV FOR 
      lev_nr_tab SCROLLING.

DEFINE QUERY BRW_LPLATS FOR 
      LAGERPLKOPP SCROLLING.

DEFINE QUERY BRW_UTAO FOR 
      uaotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AOBEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOBEST WINDOW-1 _STRUCTURED
  QUERY BRW_AOBEST NO-LOCK DISPLAY
      aobest_nr_tab.LEVERANSDATUM COLUMN-LABEL "Önskad!leveransdag" FORMAT "99/99/99":U
      aobest_nr_tab.AONRAONR COLUMN-LABEL "Aonr" FORMAT "x(10)":U
            WIDTH 6
      aobest_nr_tab.AONRDELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      aobest_nr_tab.Depnr COLUMN-LABEL "Från!Depå" FORMAT ">>>":U
      aobest_nr_tab.Bestnr COLUMN-LABEL "Uttag!nr" FORMAT ">>>>>9":U
      aobest_nr_tab.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "x(8)":U
      aobest_nr_tab.INKBESTID FORMAT "->>>>>>9":U
      aobest_nr_tab.ORDERNUMMER COLUMN-LABEL "Ordernummer" FORMAT "x(256)":U
            WIDTH 12
      aobest_nr_tab.Bestdatum COLUMN-LABEL "Beställt!datum" FORMAT "99/99/99":U
      aobest_nr_tab.KLOCKAN COLUMN-LABEL "Tid" FORMAT "x(256)":U
            WIDTH 8
      aobest_nr_tab.BESTALLARE COLUMN-LABEL "Beställt! av" FORMAT "x(12)":U
            WIDTH 8
      aobest_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(20)":U
            WIDTH 10
      aobest_nr_tab.REST FORMAT "Ja/Nej":U
      aobest_nr_tab.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT ">>>>9":U
      aobest_nr_tab.LAGERPLATSC COLUMN-LABEL "P-plats!namn" FORMAT "x(15)":U
      aobest_nr_tab.TRUMMANR COLUMN-LABEL "Trumma!nr" FORMAT ">>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 59.25 BY 21.88
         TITLE "Ej färdiga projektleveranser".

DEFINE BROWSE BRW_AOLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOLEV WINDOW-1 _STRUCTURED
  QUERY BRW_AOLEV NO-LOCK DISPLAY
      aolev_nr_tab.AONRAONR COLUMN-LABEL "Aonr" FORMAT "x(10)":U
            WIDTH 6
      aolev_nr_tab.AONRDELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      aolev_nr_tab.Depnr COLUMN-LABEL "Från!Depå" FORMAT ">>>":U
      aolev_nr_tab.Bestnr COLUMN-LABEL "Uttag!nr" FORMAT ">>>>>9":U
      aolev_nr_tab.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "x(8)":U
      aolev_nr_tab.INKBESTID COLUMN-LABEL "Bestnr" FORMAT "->>>>>>9":U
      aolev_nr_tab.ORDERNUMMER COLUMN-LABEL "Ordernummer" FORMAT "x(15)":U
      aolev_nr_tab.Bestdatum COLUMN-LABEL "Beställt!datum" FORMAT "99/99/99":U
      aolev_nr_tab.KLOCKAN COLUMN-LABEL "Tid" FORMAT "x(9)":U WIDTH 8
      aolev_nr_tab.BESTALLARE COLUMN-LABEL "Beställt!av" FORMAT "x(12)":U
            WIDTH 8
      aolev_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(20)":U
            WIDTH 10
      aolev_nr_tab.LEVERANSDATUM COLUMN-LABEL "Önskad!leveransdag" FORMAT "99/99/99":U
      aolev_nr_tab.LEVDATUM COLUMN-LABEL "Levererat!datum" FORMAT "99/99/99":U
      aolev_nr_tab.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT ">>>>9":U
      aolev_nr_tab.LAGERPLATSC COLUMN-LABEL "P-plats! namn" FORMAT "x(15)":U
      aolev_nr_tab.UTTAGET COLUMN-LABEL "Uttaget" FORMAT "Ja/Nej":U
      aolev_nr_tab.TRUMMANR COLUMN-LABEL "Trumma!nr" FORMAT ">>>>9":U
      aolev_nr_tab.UTDATUM COLUMN-LABEL "Uttaget!datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 59.25 BY 20
         TITLE "Färdiga projektleveranser".

DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST WINDOW-1 _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      best_nr_tab.Bestnr FORMAT ">>>>>9":U
      best_nr_tab.Bestdatum FORMAT "99/99/99":U
      best_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(256)":U
            WIDTH 15
      best_nr_tab.REST FORMAT "Ja/Nej":U
      best_nr_tab.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      best_nr_tab.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      best_nr_tab.BESTALLARE FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 59.5 BY 20
         TITLE "Ej färdiga leveranser".

DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      lev_nr_tab.Bestnr FORMAT ">>>>>9":U
      lev_nr_tab.Bestdatum FORMAT "99/99/99":U
      lev_nr_tab.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(25)":U
      lev_nr_tab.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      lev_nr_tab.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      lev_nr_tab.BESTALLARE FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 59.5 BY 20.5
         TITLE "Färdiga leveranser".

DEFINE BROWSE BRW_LPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LPLATS WINDOW-1 _STRUCTURED
  QUERY BRW_LPLATS NO-LOCK DISPLAY
      LAGERPLKOPP.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT ">>>>9":U
            WIDTH 6
      LAGERPLKOPP.LAGERPLATSC COLUMN-LABEL "P-plats! namn" FORMAT "x(256)":U
            WIDTH 12
      LAGERPLKOPP.AONRAONR COLUMN-LABEL "Aonr" FORMAT "x(12)":U
            WIDTH 6
      LAGERPLKOPP.AONRDELNR COLUMN-LABEL "Delnr" FORMAT ">>9":U
      LAGERPLKOPP.depnr COLUMN-LABEL "Från!depå" FORMAT ">>>9":U
      LAGERPLKOPP.BESTNR COLUMN-LABEL "Uttag!nr" FORMAT ">>>>>9":U
      LAGERPLKOPP.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "x(8)":U
      LAGERPLKOPP.INKBESTID COLUMN-LABEL "Bestnr" FORMAT "->>>>>>9":U
      LAGERPLKOPP.ORDERNUMMER COLUMN-LABEL "Ordernummer" FORMAT "x(256)":U
            WIDTH 12
      LAGERPLKOPP.Bestdatum COLUMN-LABEL "Beställt!datum" FORMAT "99/99/99":U
      LAGERPLKOPP.LEVERANSDATUM COLUMN-LABEL "Önskad!leveransdag" FORMAT "99/99/99":U
      LAGERPLKOPP.LEVDATUM COLUMN-LABEL "Levererat!datum" FORMAT "99/99/99":U
      LAGERPLKOPP.BESTALLARE FORMAT "x(12)":U
      LAGERPLKOPP.REST COLUMN-LABEL "Rest" FORMAT "Ja/Nej":U
      LAGERPLKOPP.UTTAGET COLUMN-LABEL "Uttaget" FORMAT "Ja/Nej":U
      LAGERPLKOPP.UTDATUM COLUMN-LABEL "Uttaget!datum" FORMAT "99/99/99":U
      LAGERPLKOPP.TRUMMANR COLUMN-LABEL "Trumma!nr" FORMAT ">>>>9":U
  ENABLE
      LAGERPLKOPP.LAGERPLATSC
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 57.5 BY 24.25
         TITLE "Projektlager".

DEFINE BROWSE BRW_UTAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UTAO WINDOW-1 _STRUCTURED
  QUERY BRW_UTAO NO-LOCK DISPLAY
      uaotemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      uaotemp.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      uaotemp.Depnr COLUMN-LABEL "Från! depå" FORMAT ">>>":U
      uaotemp.Bestnr COLUMN-LABEL "Uttag!nr" FORMAT ">>>>>9":U
      uaotemp.BESTALLARE COLUMN-LABEL "Uttaget!av" FORMAT "x(12)":U
      uaotemp.Enr FORMAT "X(11)":U
      uaotemp.BENAMNING FORMAT "x(256)":U WIDTH 20
      uaotemp.Antal FORMAT ">>>>>9":U
      uaotemp.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT ">>>>>9":U
      uaotemp.LAGERPLATSC COLUMN-LABEL "P-platsnamn" FORMAT "x(20)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 59.5 BY 20.5
         TITLE "Uttag projektnummer - tryck pil höger för att koppla till P-plats".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_VAL2 AT ROW 1 COL 4 NO-LABEL
     TOG_VUTT AT ROW 1.21 COL 67.25 WIDGET-ID 18
     BRW_LPLATS AT ROW 2 COL 67 WIDGET-ID 300
     RAD_VAL AT ROW 2.5 COL 4 NO-LABEL
     BRW_LEV AT ROW 3.75 COL 1.5
     BRW_UTAO AT ROW 3.75 COL 1.5 WIDGET-ID 600
     BRW_BEST AT ROW 4.38 COL 1.5
     BRW_AOBEST AT ROW 4.38 COL 1.5 WIDGET-ID 100
     BRW_AOLEV AT ROW 4.38 COL 1.5 WIDGET-ID 200
     FILL-IN-VTRUMLP AT ROW 10.5 COL 1.5 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     FILL-IN-VPROJ AT ROW 11.88 COL 1.5 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     btn_over AT ROW 12.17 COL 61.75 WIDGET-ID 14
     FILL-IN-VTRUMMA AT ROW 13.5 COL 1.5 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btn_back AT ROW 15.67 COL 61.75 WIDGET-ID 12
     FBTN_MOT AT ROW 26.54 COL 2
     FBTN_VISA AT ROW 26.54 COL 16.5
     BTN_NY AT ROW 26.54 COL 67 WIDGET-ID 10
     BTN_BORT AT ROW 26.54 COL 79.5 WIDGET-ID 8
     BTN_VILP AT ROW 26.54 COL 92 WIDGET-ID 20
     BTN_UTLP AT ROW 26.54 COL 108.88 WIDGET-ID 16
     FILL-IN-AAONR AT ROW 28 COL 74.88 COLON-ALIGNED WIDGET-ID 28
     FILL-IN-TRUMMAPLATS AT ROW 28 COL 102 COLON-ALIGNED WIDGET-ID 4
     BTN_AVB AT ROW 28.17 COL 111.5
     IMAGE-16 AT ROW 28 COL 61.63 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.29
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: aobest_nr_tab T "?" NO-UNDO temp-db aobest_nr_tab
      TABLE: aolev_nr_tab T "?" NO-UNDO temp-db aolev_nr_tab
      TABLE: LAGERPLKOPP T "?" NO-UNDO temp-db LAGERPLKOPP
      TABLE: uaobest_nr_tab T "?" NO-UNDO temp-db uaobest_nr_tab
      TABLE: uaotemp T "?" NO-UNDO temp-db uaotemp
      TABLE: ? T "?" NO-UNDO temp-db best_nr_tab
      TABLE: ? T "?" NO-UNDO temp-db lev_nr_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.46
         WIDTH              = 125.25
         MAX-HEIGHT         = 28.46
         MAX-WIDTH          = 162.38
         VIRTUAL-HEIGHT     = 28.46
         VIRTUAL-WIDTH      = 162.38
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_LPLATS TOG_VUTT FRAME-A */
/* BROWSE-TAB BRW_LEV RAD_VAL FRAME-A */
/* BROWSE-TAB BRW_UTAO BRW_LEV FRAME-A */
/* BROWSE-TAB BRW_BEST BRW_UTAO FRAME-A */
/* BROWSE-TAB BRW_AOBEST BRW_BEST FRAME-A */
/* BROWSE-TAB BRW_AOLEV BRW_AOBEST FRAME-A */
ASSIGN 
       BRW_AOBEST:HIDDEN  IN FRAME FRAME-A                = TRUE.

ASSIGN 
       BRW_AOLEV:HIDDEN  IN FRAME FRAME-A                = TRUE.

ASSIGN 
       BRW_BEST:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_BEST:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_BEST:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_LEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_LEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_LEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_LPLATS:HIDDEN  IN FRAME FRAME-A                = TRUE.

ASSIGN 
       BRW_UTAO:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AAONR IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-AAONR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TRUMMAPLATS IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-TRUMMAPLATS:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VPROJ IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VPROJ:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VTRUMLP IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VTRUMLP:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VTRUMMA IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VTRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       IMAGE-16:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOBEST
/* Query rebuild information for BROWSE BRW_AOBEST
     _TblList          = "Temp-Tables.aobest_nr_tab"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.aobest_nr_tab.LEVERANSDATUM
"aobest_nr_tab.LEVERANSDATUM" "Önskad!leveransdag" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aobest_nr_tab.AONRAONR
"aobest_nr_tab.AONRAONR" "Aonr" "x(10)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aobest_nr_tab.AONRDELNR
"aobest_nr_tab.AONRDELNR" "Delnr" "999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aobest_nr_tab.Depnr
"aobest_nr_tab.Depnr" "Från!Depå" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aobest_nr_tab.Bestnr
"aobest_nr_tab.Bestnr" "Uttag!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aobest_nr_tab.TRUMMAPLATS
"aobest_nr_tab.TRUMMAPLATS" "Trumma!namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.aobest_nr_tab.INKBESTID
     _FldNameList[8]   > Temp-Tables.aobest_nr_tab.ORDERNUMMER
"aobest_nr_tab.ORDERNUMMER" "Ordernummer" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.aobest_nr_tab.Bestdatum
"aobest_nr_tab.Bestdatum" "Beställt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.aobest_nr_tab.KLOCKAN
"aobest_nr_tab.KLOCKAN" "Tid" "x(256)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.aobest_nr_tab.BESTALLARE
"aobest_nr_tab.BESTALLARE" "Beställt! av" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.aobest_nr_tab.LEVNAMN
"aobest_nr_tab.LEVNAMN" "Leverantör" "x(20)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = Temp-Tables.aobest_nr_tab.REST
     _FldNameList[14]   > Temp-Tables.aobest_nr_tab.LAGERPLATSI
"aobest_nr_tab.LAGERPLATSI" "P-plats" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.aobest_nr_tab.LAGERPLATSC
"aobest_nr_tab.LAGERPLATSC" "P-plats!namn" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.aobest_nr_tab.TRUMMANR
"aobest_nr_tab.TRUMMANR" "Trumma!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOBEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOLEV
/* Query rebuild information for BROWSE BRW_AOLEV
     _TblList          = "Temp-Tables.aolev_nr_tab"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.aolev_nr_tab.AONRAONR
"aolev_nr_tab.AONRAONR" "Aonr" "x(10)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aolev_nr_tab.AONRDELNR
"aolev_nr_tab.AONRDELNR" "Delnr" "999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aolev_nr_tab.Depnr
"aolev_nr_tab.Depnr" "Från!Depå" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aolev_nr_tab.Bestnr
"aolev_nr_tab.Bestnr" "Uttag!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aolev_nr_tab.TRUMMAPLATS
"aolev_nr_tab.TRUMMAPLATS" "Trumma!namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aolev_nr_tab.INKBESTID
"aolev_nr_tab.INKBESTID" "Bestnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.aolev_nr_tab.ORDERNUMMER
"aolev_nr_tab.ORDERNUMMER" "Ordernummer" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.aolev_nr_tab.Bestdatum
"aolev_nr_tab.Bestdatum" "Beställt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.aolev_nr_tab.KLOCKAN
"aolev_nr_tab.KLOCKAN" "Tid" "x(9)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.aolev_nr_tab.BESTALLARE
"aolev_nr_tab.BESTALLARE" "Beställt!av" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.aolev_nr_tab.LEVNAMN
"aolev_nr_tab.LEVNAMN" "Leverantör" "x(20)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.aolev_nr_tab.LEVERANSDATUM
"aolev_nr_tab.LEVERANSDATUM" "Önskad!leveransdag" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.aolev_nr_tab.LEVDATUM
"aolev_nr_tab.LEVDATUM" "Levererat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.aolev_nr_tab.LAGERPLATSI
"aolev_nr_tab.LAGERPLATSI" "P-plats" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.aolev_nr_tab.LAGERPLATSC
"aolev_nr_tab.LAGERPLATSC" "P-plats! namn" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.aolev_nr_tab.UTTAGET
"aolev_nr_tab.UTTAGET" "Uttaget" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.aolev_nr_tab.TRUMMANR
"aolev_nr_tab.TRUMMANR" "Trumma!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.aolev_nr_tab.UTDATUM
"aolev_nr_tab.UTDATUM" "Uttaget!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.best_nr_tab"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.best_nr_tab.Bestnr|yes,Temp-Tables.best_nr_tab.LEVNAMN|yes"
     _FldNameList[1]   = Temp-Tables.best_nr_tab.Bestnr
     _FldNameList[2]   = Temp-Tables.best_nr_tab.Bestdatum
     _FldNameList[3]   > Temp-Tables.best_nr_tab.LEVNAMN
"best_nr_tab.LEVNAMN" "Leverantör" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.best_nr_tab.REST
     _FldNameList[5]   > Temp-Tables.best_nr_tab.AONR
"best_nr_tab.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.best_nr_tab.DELNR
"best_nr_tab.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.best_nr_tab.BESTALLARE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.lev_nr_tab"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.lev_nr_tab.Bestnr|yes"
     _FldNameList[1]   = Temp-Tables.lev_nr_tab.Bestnr
     _FldNameList[2]   = Temp-Tables.lev_nr_tab.Bestdatum
     _FldNameList[3]   > Temp-Tables.lev_nr_tab.LEVNAMN
"lev_nr_tab.LEVNAMN" "Leverantör" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.lev_nr_tab.AONR
"lev_nr_tab.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.lev_nr_tab.DELNR
"lev_nr_tab.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.lev_nr_tab.BESTALLARE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LPLATS
/* Query rebuild information for BROWSE BRW_LPLATS
     _TblList          = "Temp-Tables.LAGERPLKOPP"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.LAGERPLKOPP.LAGERPLATSI
"LAGERPLKOPP.LAGERPLATSI" "P-plats" ">>>>9" "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.LAGERPLKOPP.LAGERPLATSC
"LAGERPLKOPP.LAGERPLATSC" "P-plats! namn" "x(256)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.LAGERPLKOPP.AONRAONR
"LAGERPLKOPP.AONRAONR" "Aonr" "x(12)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.LAGERPLKOPP.AONRDELNR
"LAGERPLKOPP.AONRDELNR" "Delnr" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.LAGERPLKOPP.depnr
"LAGERPLKOPP.depnr" "Från!depå" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.LAGERPLKOPP.BESTNR
"LAGERPLKOPP.BESTNR" "Uttag!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.LAGERPLKOPP.TRUMMAPLATS
"LAGERPLKOPP.TRUMMAPLATS" "Trumma!namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.LAGERPLKOPP.INKBESTID
"LAGERPLKOPP.INKBESTID" "Bestnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.LAGERPLKOPP.ORDERNUMMER
"LAGERPLKOPP.ORDERNUMMER" "Ordernummer" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.LAGERPLKOPP.Bestdatum
"LAGERPLKOPP.Bestdatum" "Beställt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.LAGERPLKOPP.LEVERANSDATUM
"LAGERPLKOPP.LEVERANSDATUM" "Önskad!leveransdag" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.LAGERPLKOPP.LEVDATUM
"LAGERPLKOPP.LEVDATUM" "Levererat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = Temp-Tables.LAGERPLKOPP.BESTALLARE
     _FldNameList[14]   > Temp-Tables.LAGERPLKOPP.REST
"LAGERPLKOPP.REST" "Rest" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.LAGERPLKOPP.UTTAGET
"LAGERPLKOPP.UTTAGET" "Uttaget" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.LAGERPLKOPP.UTDATUM
"LAGERPLKOPP.UTDATUM" "Uttaget!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.LAGERPLKOPP.TRUMMANR
"LAGERPLKOPP.TRUMMANR" "Trumma!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LPLATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UTAO
/* Query rebuild information for BROWSE BRW_UTAO
     _TblList          = "Temp-Tables.uaotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.uaotemp.AONR
"uaotemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.uaotemp.DELNR
"uaotemp.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.uaotemp.Depnr
"uaotemp.Depnr" "Från! depå" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.uaotemp.Bestnr
"uaotemp.Bestnr" "Uttag!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.uaotemp.BESTALLARE
"uaotemp.BESTALLARE" "Uttaget!av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.uaotemp.Enr
     _FldNameList[7]   > Temp-Tables.uaotemp.BENAMNING
"uaotemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = Temp-Tables.uaotemp.Antal
     _FldNameList[9]   > Temp-Tables.uaotemp.LAGERPLATSI
"uaotemp.LAGERPLATSI" "P-plats" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.uaotemp.LAGERPLATSC
"uaotemp.LAGERPLATSC" "P-platsnamn" "x(20)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UTAO */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_AOBEST
&Scoped-define SELF-NAME BRW_AOBEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AOBEST WINDOW-1
ON VALUE-CHANGED OF BRW_AOBEST IN FRAME FRAME-A /* Ej färdiga projektleveranser */
DO:
  IF AVAILABLE aobest_nr_tab THEN DO:
      ASSIGN
      aobrow = ROWID(aobest_nr_tab)         
      vald_lev = aobest_nr_tab.LEVKOD.
      IF aobest_nr_tab.INKBESTID > 0 THEN DO:
         vald_best = aobest_nr_tab.INKBESTID.         
         FIND FIRST lagerplkopp  WHERE lagerplkopp.INKBESTID = aobest_nr_tab.INKBESTID AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.
      END.
      ELSE IF aobest_nr_tab.BESTNR > 0 THEN DO:
         vald_best = aobest_nr_tab.BESTNR.
         FIND FIRST lagerplkopp  WHERE lagerplkopp.BESTNR = aobest_nr_tab.BESTNR AND lagerplkopp.DEPNR = aobest_nr_tab.DEPNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.
      END.
      ELSE IF aobest_nr_tab.TRUMMANR > 0 THEN DO:
         /*samma trumma kan plockas ut flera gånger - sök på aonr delnr också*/
         
         vald_best = aobest_nr_tab.TRUMMANR.
         FIND FIRST lagerplkopp  WHERE lagerplkopp.TRUMMANR = aobest_nr_tab.TRUMMANR  AND lagerplkopp.DEPNR = aobest_nr_tab.DEPNR
         AND lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR  AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.
      END.      
      IF AVAILABLE lagerplkopp  THEN DO:            
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                  
         RUN lastselectdyn_UI IN brwproc[5].         
         
      END.   
      FIND FIRST aobest_nr_tab WHERE ROWID(aobest_nr_tab) = aobrow  NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(aobest_nr_tab)).                  
      RUN lastselectdyn_UI IN brwproc[3].
      
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AOLEV
&Scoped-define SELF-NAME BRW_AOLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AOLEV WINDOW-1
ON VALUE-CHANGED OF BRW_AOLEV IN FRAME FRAME-A /* Färdiga projektleveranser */
DO:
   IF AVAILABLE aolev_nr_tab THEN DO:
      ASSIGN   
      aobrow = ROWID(aolev_nr_tab)
      vald_best = aolev_nr_tab.INKBESTID.
      IF aolev_nr_tab.INKBESTID > 0 THEN DO:
         FIND FIRST lagerplkopp  WHERE lagerplkopp.INKBESTID = aolev_nr_tab.INKBESTID AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aolev_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aolev_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.         
      END.
      ELSE IF aolev_nr_tab.BESTNR > 0 THEN DO:
         FIND FIRST lagerplkopp  WHERE lagerplkopp.BESTNR = aolev_nr_tab.BESTNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aolev_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aolev_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.
      END.
      ELSE IF aolev_nr_tab.TRUMMANR > 0 THEN DO:
         FIND FIRST lagerplkopp  WHERE lagerplkopp.TRUMMANR = aolev_nr_tab.TRUMMANR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = aolev_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = aolev_nr_tab.AONRDELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.
      END.      
      IF AVAILABLE lagerplkopp  THEN DO:            
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                  
         RUN lastselectdyn_UI IN brwproc[5].              
      END.  
      FIND FIRST aolev_nr_tab WHERE ROWID(aolev_nr_tab) = aobrow  NO-LOCK NO-ERROR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST WINDOW-1
ON VALUE-CHANGED OF BRW_BEST IN FRAME FRAME-A /* Ej färdiga leveranser */
DO: 
   IF AVAILABLE best_nr_tab THEN DO:
      ASSIGN   
      vald_best = best_nr_tab.BESTNR
      vald_lev = best_nr_tab.LEVKOD.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV WINDOW-1
ON VALUE-CHANGED OF BRW_LEV IN FRAME FRAME-A /* Färdiga leveranser */
DO:         
   IF AVAILABLE lev_nr_tab THEN DO:
      ASSIGN   
      vald_best = lev_nr_tab.BESTNR.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LPLATS
&Scoped-define SELF-NAME BRW_LPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LPLATS WINDOW-1
ON ROW-LEAVE OF BRW_LPLATS IN FRAME FRAME-A /* Projektlager */
DO:
   
  IF AVAILABLE lagerplkopp THEN DO:
     lagerplkopp.LAGERPLATSC = INPUT BROWSE BRW_LPLATS lagerplkopp.LAGERPLATSC.                   
     DISPLAY lagerplkopp.LAGERPLATSC WITH BROWSE BRW_LPLATS.                        
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LPLATS WINDOW-1
ON VALUE-CHANGED OF BRW_LPLATS IN FRAME FRAME-A /* Projektlager */
DO:
   
   status-ok = BRW_LPLATS:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE lagerplkopp THEN DO:
      Guru.GlobalaVariabler:GuruVlplats = lagerplkopp.LAGERPLATSI.               
   END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LAGERPLKOPP.LAGERPLATSC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LAGERPLKOPP.LAGERPLATSC BRW_LPLATS _BROWSE-COLUMN WINDOW-1
ON ENTRY OF LAGERPLKOPP.LAGERPLATSC IN BROWSE BRW_LPLATS /* P-plats! namn */
DO:
   
  status-ok = BRW_LPLATS:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LAGERPLKOPP.LAGERPLATSC BRW_LPLATS _BROWSE-COLUMN WINDOW-1
ON LEAVE OF LAGERPLKOPP.LAGERPLATSC IN BROWSE BRW_LPLATS /* P-plats! namn */
DO:
   
  IF AVAILABLE lagerplkopp THEN DO:      
      IF lagerplkopp.LAGERPLATSC NE INPUT BROWSE BRW_LPLATS lagerplkopp.LAGERPLATSC THEN DO:
         lpls = INPUT BROWSE BRW_LPLATS lagerplkopp.LAGERPLATSC.
         IF lpls NE ""  THEN DO:
            EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
            CREATE elagerplkopp.
            BUFFER-COPY lagerplkopp TO elagerplkopp. 
            RUN kollplats_UI IN leveransaoapph ( INPUT TABLE elagerplkopp, INPUT lpls, OUTPUT felmed ). 
            IF felmed NE "" THEN DO:
               MESSAGE felmed VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO lagerplkopp.LAGERPLATSC IN BROWSE BRW_LPLATS.      
               RETURN NO-APPLY.
            END.   
         END.   
            
         lagerplkopp.LAGERPLATSC = INPUT BROWSE BRW_LPLATS lagerplkopp.LAGERPLATSC.
         DISPLAY lagerplkopp.LAGERPLATSC WITH BROWSE BRW_LPLATS. 
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                
         EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
          CREATE elagerplkopp.          
          BUFFER-COPY lagerplkopp TO elagerplkopp.                  
          RUN sparlpl_UI IN leveransaoapph ( INPUT TABLE elagerplkopp ).      
          FOR EACH  lagerplkopp WHERE lagerplkopp.PROJDEPNR = elagerplkopp.PROJDEPNR AND lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI  EXCLUSIVE-LOCK:   
             lagerplkopp.LAGERPLATSC = elagerplkopp.LAGERPLATSC.
          END.        
                   
          IF TOG_VUTT = FALSE THEN RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).            
          ELSE RUN setcolsortvar_UI IN brwproc[5] (INPUT "" ).    
          RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").                     
          RUN openbdynspec_UI IN brwproc[5].  
          RUN lastselectdyn_UI IN brwproc[5].     
          RUN resetcolindex_UI IN brwproc[5].                      
      END.
      
      
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:     
   RUN selfocusrow_UI IN brwproc[5].    
   kolltxt = "".
   annandeptrum = "".
   IF lagerplkopp.DEPNR NE Guru.GlobalaVariabler:GuruVdepnr AND  lagerplkopp.TRUMMANR > 0 THEN DO:
      /*FIND FIRST depatemp WHERE depatemp.DEP-NR = Guru.GlobalaVariabler:GuruVdepnr NO-ERROR.*/
      FIND FIRST depatemp WHERE depatemp.DEP-NR = lagerplkopp.DEPNR NO-ERROR.          
      annandeptrum = STRING(lagerplkopp.DEPNR) + " " + depatemp.BENAMNING.
   END.
           
   IF AVAILABLE lagerplkopp THEN DO:
      IF lagerplkopp.AONRAONR = "" THEN DO:
         MESSAGE "Pplats är inte bokad för något projekt" VIEW-AS ALERT-BOX.     
         RETURN.
      END.
      ELSE IF annandeptrum NE "" THEN DO:
         
         MESSAGE "Trumman tillhör depå: " annandeptrum + ". Gå till detta lager för att koppla bort/koppla om" VIEW-AS ALERT-BOX.     
         RETURN.
      END.            
      ELSE IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0 OR lagerplkopp.TRUMMANR > 0 THEN DO:
         IF lagerplkopp.INKBESTID > 0 THEN DO:    
            kolltxt = "Är det korrekt att du vill ta bort markerad beställning för projektnummer:"  + lagerplkopp.AONRAONR + " " + STRING(lagerplkopp.AONRDELNR) 
            + " " + STRING(lagerplkopp.INKBESTID) + " från pplats: " +
            STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.
           
         END.
         ELSE IF lagerplkopp.BESTNR > 0 THEN DO:   
            kolltxt = "Är det korrekt att du vill ta bort markerat uttag " + STRING(lagerplkopp.BESTNR) + " för projektnummer:"  + lagerplkopp.AONRAONR + " " + STRING(lagerplkopp.AONRDELNR) 
            +  " från depå " +  STRING(lagerplkopp.DEPNR)  + " från pplats: " +
            STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.
           
         END.
         ELSE IF lagerplkopp.TRUMMANR > 0 THEN DO:   
            kolltxt = "Är det korrekt att du vill ta bort markerad trumma " + STRING(lagerplkopp.TRUMMAPLATS) + " för projektnummer:"  + lagerplkopp.AONRAONR + " " + STRING(lagerplkopp.AONRDELNR) 
            +  " från depå " +  STRING(lagerplkopp.DEPNR)  + " från pplats: " +
            STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.
           
         END.  
         IF kolltxt NE "" THEN DO:    
            MESSAGE kolltxt VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE svar3 AS LOGICAL.     
            IF svar3 THEN DO:
               EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
               CREATE elagerplkopp.
               BUFFER-COPY lagerplkopp TO elagerplkopp.              
               RUN bortlagerkopp_UI IN leveransaoapph ( INPUT-OUTPUT TABLE elagerplkopp).
               
               IF lagerplkopp.INKBESTID > 0 THEN DO:
                  FIND FIRST aobest_nr_tab WHERE aobest_nr_tab.INKBESTID = lagerplkopp.INKBESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE aobest_nr_tab THEN DO:
                     ASSIGN 
                     aobest_nr_tab.LAGERPLATSI = 0
                     aobest_nr_tab.LAGERPLATSC = "".                     
                  END.
                  FIND FIRST aolev_nr_tab WHERE aolev_nr_tab.INKBESTID = lagerplkopp.INKBESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE aolev_nr_tab THEN DO:
                     ASSIGN 
                     aolev_nr_tab.LAGERPLATSI = 0
                     aolev_nr_tab.LAGERPLATSC = "".                     
                  END.
               END.
               ELSE IF lagerplkopp.BESTNR > 0 THEN DO:                  
                  FIND FIRST aobest_nr_tab WHERE aobest_nr_tab.DEPNR = lagerplkopp.DEPNR and aobest_nr_tab.BESTNR = lagerplkopp.BESTNR NO-LOCK NO-ERROR.
                  IF AVAILABLE aobest_nr_tab THEN DO:
                     ASSIGN 
                     aobest_nr_tab.LAGERPLATSI = 0
                     aobest_nr_tab.LAGERPLATSC = "".                     
                  END.                                                      
                  FIND FIRST aolev_nr_tab WHERE aolev_nr_tab.DEPNR = lagerplkopp.DEPNR and aolev_nr_tab.BESTNR = lagerplkopp.BESTNR NO-LOCK NO-ERROR.
                  IF AVAILABLE aolev_nr_tab THEN DO:
                     ASSIGN 
                     aolev_nr_tab.LAGERPLATSI = 0
                     aolev_nr_tab.LAGERPLATSC = "".                     
                  END.
               END.
               ELSE IF lagerplkopp.TRUMMANR > 0 THEN DO:                  
                  FIND FIRST lagertrum WHERE lagertrum.DEPNR = lagerplkopp.DEPNR AND lagertrum.TRUMMANR = lagerplkopp.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
                  IF AVAILABLE lagertrum THEN DO:
                     ASSIGN
                     lagertrum.LAGERPLATSI = 0
                     lagertrum.LAGERPLATSC = "".
                  END.   
                  FIND FIRST aotrum WHERE aotrum.DEPNR = lagerplkopp.DEPNR AND aotrum.TRUMMANR = lagerplkopp.TRUMMANR NO-LOCK NO-ERROR.
                  IF AVAILABLE aotrum THEN DO:
                     ASSIGN
                     aotrum.LAGERPLATSI = 0
                     aotrum.LAGERPLATSC = "".
                  END.   
                  RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
                  FIND FIRST aobest_nr_tab WHERE aobest_nr_tab.DEPNR = lagerplkopp.DEPNR and aobest_nr_tab.TRUMMANR = lagerplkopp.TRUMMANR NO-LOCK NO-ERROR.
                  IF AVAILABLE aobest_nr_tab THEN DO:
                     ASSIGN 
                     aobest_nr_tab.LAGERPLATSI = 0
                     aobest_nr_tab.LAGERPLATSC = "".                     
                  END.
                  FIND FIRST aolev_nr_tab WHERE aolev_nr_tab.DEPNR = lagerplkopp.DEPNR and aolev_nr_tab.TRUMMANR = lagerplkopp.TRUMMANR NO-LOCK NO-ERROR.
                  IF AVAILABLE aolev_nr_tab THEN DO:
                     ASSIGN 
                     aolev_nr_tab.LAGERPLATSI = 0
                     aolev_nr_tab.LAGERPLATSC = "".                     
                  END.
               END.      
               FIND FIRST elagerplkopp NO-LOCK NO-ERROR.
               IF AVAILABLE elagerplkopp THEN DO:
                  ASSIGN
                  lagerplkopp.AONRAONR = ""   
                  lagerplkopp.AONRDELNR =    0
                  lagerplkopp.INKBESTID   =   0
                  lagerplkopp.TRUMMANR   =   0
                  lagerplkopp.ORDERNUMMER = ""
                  lagerplkopp.BESTDATUM = ?
                  lagerplkopp.BESTALLARE = ""
                  lagerplkopp.BESTNR   =   0
                  lagerplkopp.LEVDATUM   =   ?
                  lagerplkopp.LEVERANSDATUM   =   ? 
                  lagerplkopp.DEPNR =    0.    
               END.   
               ELSE DELETE lagerplkopp.    
               RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).    
               RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").        
               RUN openbdynspec_UI IN brwproc[5].  
               RUN lastselectdyn_UI IN brwproc[5].                    
               RUN resetcolindex_UI IN brwproc[5].
                            
               RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").
               RUN openbdynspec_UI IN brwproc[3].  
               RUN lastselectdyn_UI IN brwproc[3].
               RUN resetcolindex_UI IN brwproc[3].
               RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
               RUN openbdynspec_UI IN brwproc[4].  
               RUN lastselectdyn_UI IN brwproc[4].
               RUN resetcolindex_UI IN brwproc[4].
              
            END.    
         END.
      END.
      ELSE DO:

         /*bara bokad med projekt , ingen best eller uttag*/                        
         MESSAGE "Är det korrekt att du vill ta bort markerat projektnummer:"  + lagerplkopp.AONRAONR + " " + STRING(lagerplkopp.AONRDELNR) 
         + " från pplats: " +
         STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar7 AS LOGICAL.     
         IF svar7 THEN DO:
            EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
            CREATE elagerplkopp.
            BUFFER-COPY lagerplkopp TO elagerplkopp.              
            RUN bortlagerkopp_UI IN leveransaoapph ( INPUT-OUTPUT TABLE elagerplkopp).
                 
            FIND FIRST elagerplkopp NO-LOCK NO-ERROR.
            IF AVAILABLE elagerplkopp THEN DO:
               ASSIGN
               lagerplkopp.AONRAONR = ""   
               lagerplkopp.AONRDELNR =    0
               lagerplkopp.INKBESTID   =   0
               lagerplkopp.ORDERNUMMER = ""
               lagerplkopp.BESTDATUM = ?
               lagerplkopp.BESTALLARE = ""
               lagerplkopp.BESTNR   =   0
               lagerplkopp.LEVDATUM   =   ?
               lagerplkopp.LEVERANSDATUM   =   ? 
               lagerplkopp.DEPNR =    0.    
            END.   
            ELSE DELETE lagerplkopp.    
            RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).     
            RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").       
            RUN openbdynspec_UI IN brwproc[5].  
            RUN lastselectdyn_UI IN brwproc[5].    
            RUN resetcolindex_UI IN brwproc[5].
                                    
         END.
      END.      
   END.      
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:  
   
   
   RUN selfocusrow_UI IN brwproc[5].             
   IF AVAILABLE lagerplkopp THEN DO:
      IF lagerplkopp.AONRAONR NE "" THEN DO:         
         MESSAGE "Det ligger ett beställning på denna pplats. För att ta bort lagerplasen, pila bort den/de beställningar som är kopplade först." VIEW-AS ALERT-BOX.     
         RETURN.
      END.   
      
      EMPTY TEMP-TABLE elagerplkopp  NO-ERROR.  
      CREATE elagerplkopp.
      BUFFER-COPY lagerplkopp TO elagerplkopp.
      RUN bortlplats_UI IN leveransaoapph ( INPUT TABLE elagerplkopp).
      DELETE lagerplkopp.
      RUN selnextprevrow_UI IN brwproc[5].  
      RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").
      RUN openbdynspec_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].
      RUN resetcolindex_UI IN brwproc[5].
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:                                        
    {muswait.i}   
   RUN nylplats_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruProjdepnr, OUTPUT lplats).    
   CREATE lagerplkopp.
   ASSIGN
   lagerplkopp.LAGERPLATSI = lplats
   lagerplkopp.DEPNR = 0
   lagerplkopp.PROJDEPNR = Guru.GlobalaVariabler:GuruProjdepnr. 
   Guru.GlobalaVariabler:GuruVlplats = lagerplkopp.LAGERPLATSI.
   RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).
   RUN openbdynspec_UI IN brwproc[5].
   RUN lastselectdyn_UI IN brwproc[5].
   RUN resetcolindex_UI IN brwproc[5].   
      
   APPLY "ENTRY" TO lagerplkopp.LAGERPLATSC IN BROWSE BRW_LPLATS.   
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A /* Koppla aonr till vald pplats */
DO:
   
   DO:
      IF RAD_VAL = 1 THEN DO:   
         RUN selfocusrow_UI IN brwproc[3].
         RUN selfocusrow_UI IN brwproc[5].          
         IF AVAILABLE aobest_nr_tab THEN DO:
            IF AVAILABLE lagerplkopp THEN DO:
               redkopp = FALSE.               
               IF lagerplkopp.AONRAONR NE "" THEN DO:
                  IF lagerplkopp.AONRAONR NE  aobest_nr_tab.AONRAONR OR lagerplkopp.AONRDELNR NE  aobest_nr_tab.AONRDELNR THEN DO:
                     redkopp = TRUE.
                  END.
               END.                                           
               IF aobest_nr_tab.LAGERPLATSI > 0 AND aobest_nr_tab.LAGERPLATSI NE lagerplkopp.LAGERPLATSI THEN DO:                 
                  MESSAGE "Vald beställning har redan en pplats kopplad. Vill du även koppla vald pplats till samma projekt?" VIEW-AS ALERT-BOX
                  QUESTION BUTTONS YES-NO UPDATE svar5 AS LOGICAL.         
                  IF svar5 THEN DO:
                     IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0 OR lagerplkopp.TRUMMANR > 0 THEN DO:
                        lpl = lagerplkopp.LAGERPLATSI.
                        lpc = lagerplkopp.LAGERPLATSC.
                        CREATE lagerplkopp.
                        ASSIGN 
                        lagerplkopp.LAGERPLATSI = lpl  
                        lagerplkopp.LAGERPLATSC = lpc.
                     END.   
                     ASSIGN
                     lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR
                     lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR                     
                     lagerplkopp.BESTDATUM = aobest_nr_tab.BESTDATUM
                     lagerplkopp.BESTALLARE = aobest_nr_tab.BESTALLARE
                     lagerplkopp.DEPNR = aobest_nr_tab.DEPNR
                     lagerplkopp.PROJDEPNR = Guru.GlobalaVariabler:GuruProjdepnr.                     
                     EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
                     CREATE elagerplkopp.
                     BUFFER-COPY lagerplkopp TO elagerplkopp.              
                     RUN splagerkopp_UI IN leveransaoapph ( INPUT TABLE elagerplkopp).      
                     RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                  
                     RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).
                     RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").
                     RUN openbdynspec_UI IN brwproc[5].  
                     RUN lastselectdyn_UI IN brwproc[5].
                     RUN resetcolindex_UI IN brwproc[5].
                                    
                  END.                   
               END.   
               ELSE IF aobest_nr_tab.LAGERPLATSI > 0 AND aobest_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI THEN DO:
                  MESSAGE "Beställningen är redan kopplad till vald pplats." VIEW-AS ALERT-BOX.     
                  RETURN.
               END.
               ELSE IF redkopp = TRUE THEN DO:                                                
                  MESSAGE "Ett annat projektnummer är redan kopplat till denna pplats. Ta bort denna koppling innan det går att koppla nytt projektnummer." VIEW-AS ALERT-BOX.     
                  RETURN.
               END.                 
               ELSE DO:     
                  IF aobest_nr_tab.INKBESTID > 0 THEN DO:    
                     kolltxt = "Är det korrekt att du vill lägga markerad beställning för projektnummer:"  + aobest_nr_tab.AONRAONR + " " + STRING(aobest_nr_tab.AONRDELNR) 
                     + " " + STRING(aobest_nr_tab.INKBESTID) + " på pplats: " +
                     STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                    
                  END.
                  ELSE IF aobest_nr_tab.BESTNR > 0 THEN DO:   
                     kolltxt =   "Är det korrekt att du vill lägga markerat uttag " + STRING(aobest_nr_tab.BESTNR) + " för projektnummer:"  + aobest_nr_tab.AONRAONR + " " + STRING(aobest_nr_tab.AONRDELNR) 
                     + " på pplats: " +  STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                 
                  END.
                  ELSE IF aobest_nr_tab.TRUMMANR > 0 THEN DO:   
                     kolltxt =   "Är det korrekt att du vill lägga markerad TRUMMA " + STRING(aobest_nr_tab.TRUMMANR) + " för projektnummer:"  + aobest_nr_tab.AONRAONR + " " + STRING(aobest_nr_tab.AONRDELNR) 
                     + " på pplats: " +  STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                 
                  END.  
                  IF kolltxt NE "" THEN DO:    
                     MESSAGE kolltxt VIEW-AS ALERT-BOX             
                     QUESTION BUTTONS YES-NO UPDATE svar3 AS LOGICAL.         
                     IF svar3 THEN DO:
                        IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0 OR lagerplkopp.TRUMMANR > 0 THEN DO:
                           lpl = lagerplkopp.LAGERPLATSI.
                           lpc = lagerplkopp.LAGERPLATSC.
                           CREATE lagerplkopp.
                           ASSIGN 
                           lagerplkopp.LAGERPLATSI = lpl  
                           lagerplkopp.LAGERPLATSC = lpc.
                        END.   
                        ASSIGN
                        lagerplkopp.AONRAONR = aobest_nr_tab.AONRAONR
                        lagerplkopp.AONRDELNR = aobest_nr_tab.AONRDELNR
                        lagerplkopp.INKBESTID = aobest_nr_tab.INKBESTID
                        lagerplkopp.BESTNR = aobest_nr_tab.BESTNR
                        lagerplkopp.TRUMMANR = aobest_nr_tab.TRUMMANR
                        lagerplkopp.TRUMMAPLATS = aobest_nr_tab.TRUMMAPLATS
                        lagerplkopp.ORDERNUMMER = aobest_nr_tab.ORDERNUMMER
                        lagerplkopp.BESTDATUM = aobest_nr_tab.BESTDATUM
                        lagerplkopp.LEVDATUM = aobest_nr_tab.LEVDATUM
                        lagerplkopp.LEVERANSDATUM = aobest_nr_tab.LEVERANSDATUM
                        lagerplkopp.BESTALLARE = aobest_nr_tab.BESTALLARE
                        lagerplkopp.DEPNR = aobest_nr_tab.DEPNR
                        lagerplkopp.PROJDEPNR = Guru.GlobalaVariabler:GuruProjdepnr
                        aobest_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                        aobest_nr_tab.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                        
                        IF aobest_nr_tab.INKBESTID > 0 THEN DO:
                           FIND FIRST aolev_nr_tab WHERE aolev_nr_tab.inkbestid = aobest_nr_tab.INKBESTID  NO-LOCK NO-ERROR.                        
                           IF AVAILABLE aolev_nr_tab THEN DO:
                              ASSIGN 
                              aolev_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              aolev_nr_tab.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                           END.
                           FIND FIRST aobest_nr_tabbuff WHERE aobest_nr_tabbuff.INKBESTID = aobest_nr_tab.INKBESTID AND aobest_nr_tabbuff.REST = TRUE NO-LOCK NO-ERROR.
                           IF AVAILABLE aobest_nr_tabbuff THEN DO:
                              FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = aobest_nr_tab.INKBESTID AND lagerplkopp.UTTAGET = FALSE NO-ERROR.
                              IF AVAILABLE lagerplkopp  THEN DO:
                                 lagerplkopp.REST = TRUE.
                              END.
                              ELSE lagerplkopp.REST = FALSE.
                           END.   
                        END.
                        IF aobest_nr_tab.TRUMMANR > 0 THEN DO:
                           /*ännu ej aokopp = true*/
                           FIND FIRST lagertrum WHERE lagertrum.DEPNR = lagerplkopp.DEPNR AND lagertrum.TRUMMANR = lagerplkopp.TRUMMANR AND lagertrum.TRUMMA = "S"  NO-LOCK NO-ERROR.
                           IF AVAILABLE lagertrum THEN DO:
                              ASSIGN
                              lagertrum.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              lagertrum.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                           END.                              
                           FIND FIRST aotrum WHERE  aotrum.DEPNR = lagerplkopp.DEPNR AND aotrum.TRUMMANR = lagerplkopp.TRUMMANR NO-LOCK NO-ERROR.
                           IF AVAILABLE aotrum THEN DO:
                              ASSIGN
                              aotrum.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              aotrum.LAGERPLATSC = lagerplkopp.LAGERPLATSC.                              
                           END.   
                           RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
                        END.                                                  
                        EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
                        CREATE elagerplkopp.
                        BUFFER-COPY lagerplkopp TO elagerplkopp.              
                        RUN splagerkopp_UI IN leveransaoapph ( INPUT TABLE elagerplkopp).
                        RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).   
                        RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).
                        RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").
                        RUN openbdynspec_UI IN brwproc[5].  
                        RUN lastselectdyn_UI IN brwproc[5].
                        RUN resetcolindex_UI IN brwproc[5].                                                            
                        RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").
                        RUN openbdynspec_UI IN brwproc[3].  
                        RUN lastselectdyn_UI IN brwproc[3].
                        RUN resetcolindex_UI IN brwproc[3].                        
                     END.   
                  END.
               END.   
            END.
         END.                 
      END.
      ELSE IF RAD_VAL = 2 THEN DO:
         RUN selfocusrow_UI IN brwproc[4].
         RUN selfocusrow_UI IN brwproc[5].          
         IF AVAILABLE aolev_nr_tab THEN DO:
            IF AVAILABLE lagerplkopp THEN DO:
               kolltxt = "".
               redkopp = FALSE.
               IF lagerplkopp.AONRAONR NE  "" THEN DO: 
                  IF lagerplkopp.AONRAONR NE  aolev_nr_tab.AONRAONR OR lagerplkopp.AONRDELNR NE  aolev_nr_tab.AONRDELNR THEN DO:
                     redkopp = TRUE.
                  END.
               END.                                       
               IF aolev_nr_tab.LAGERPLATSI > 0 AND aolev_nr_tab.LAGERPLATSI NE lagerplkopp.LAGERPLATSI THEN DO:
                  MESSAGE "Vald beställning har redan en pplats kopplad. Vill du även koppla vald pplats till samma projekt?" VIEW-AS ALERT-BOX
                  QUESTION BUTTONS YES-NO UPDATE svar9 AS LOGICAL.         
                  IF svar9 THEN DO:
                     IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0 OR lagerplkopp.TRUMMANR > 0 THEN DO:
                        lpl = lagerplkopp.LAGERPLATSI.
                        lpc = lagerplkopp.LAGERPLATSC.
                        CREATE lagerplkopp.
                        ASSIGN 
                        lagerplkopp.LAGERPLATSI = lpl  
                        lagerplkopp.LAGERPLATSC = lpc.
                     END.   
                     ASSIGN
                     lagerplkopp.AONRAONR = aolev_nr_tab.AONRAONR
                     lagerplkopp.AONRDELNR = aolev_nr_tab.AONRDELNR                     
                     lagerplkopp.BESTDATUM = aolev_nr_tab.BESTDATUM
                     lagerplkopp.BESTALLARE = aolev_nr_tab.BESTALLARE
                     lagerplkopp.DEPNR = aolev_nr_tab.DEPNR
                     lagerplkopp.PROJDEPNR = Guru.GlobalaVariabler:GuruProjdepnr.                     
                     EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
                     CREATE elagerplkopp.
                     BUFFER-COPY lagerplkopp TO elagerplkopp.              
                     RUN splagerkopp_UI IN leveransaoapph ( INPUT TABLE elagerplkopp).      
                     RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                  
                     RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).
                     RUN openbdynspec_UI IN brwproc[5].  
                     RUN lastselectdyn_UI IN brwproc[5].
                     RUN resetcolindex_UI IN brwproc[5].
                                    
                  END.                   
               END.                                 
               ELSE IF aolev_nr_tab.LAGERPLATSI > 0  AND aolev_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI  THEN DO:
                  MESSAGE "Beställningen är redan kopplad till vald pplats." VIEW-AS ALERT-BOX.     
                  RETURN.
               END.                     
               ELSE IF redkopp = TRUE THEN DO:               
                  MESSAGE "Ett annat projektnummer är redan kopplat till denna pplats. Ta bort denna koppling innan det går att koppla nytt projektnummer." VIEW-AS ALERT-BOX.     
                  RETURN.
               END.   
               ELSE DO:
                  IF aolev_nr_tab.INKBESTID > 0 THEN DO:    
                     kolltxt = "Är det korrekt att du vill lägga markerad beställning för projektnummer:"  + aolev_nr_tab.AONRAONR + " " + STRING(aolev_nr_tab.AONRDELNR) 
                     + " " + STRING(aolev_nr_tab.INKBESTID) + " på pplats: " +
                     STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                    
                  END.
                  ELSE IF aolev_nr_tab.BESTNR > 0 THEN DO:   
                     kolltxt =   "Är det korrekt att du vill lägga markerat uttag " + STRING(aolev_nr_tab.BESTNR) + " för projektnummer:"  + aolev_nr_tab.AONRAONR + " " + STRING(aolev_nr_tab.AONRDELNR) 
                     + " på pplats: " +  STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                 
                  END.
                  ELSE IF aolev_nr_tab.TRUMMANR > 0 THEN DO:   
                     kolltxt =   "Är det korrekt att du vill lägga markerad TRUMMA " + STRING(aolev_nr_tab.TRUMMANR) + " för projektnummer:"  + aolev_nr_tab.AONRAONR + " " + STRING(aolev_nr_tab.AONRDELNR) 
                     + " på pplats: " +  STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC.                 
                  END.  
                  IF kolltxt NE "" THEN DO:    
                     MESSAGE kolltxt VIEW-AS ALERT-BOX
                     QUESTION BUTTONS YES-NO UPDATE svar4 AS LOGICAL.     
                     IF svar4 THEN DO:               
                        IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0  OR lagerplkopp.TRUMMANR > 0 THEN DO:
                           lpl = lagerplkopp.LAGERPLATSI.
                           lpc = lagerplkopp.LAGERPLATSC.
                           CREATE lagerplkopp.
                           ASSIGN 
                           lagerplkopp.LAGERPLATSI = lpl  
                           lagerplkopp.LAGERPLATSC = lpc.
                        END.   
                        ASSIGN
                        lagerplkopp.AONRAONR = aolev_nr_tab.AONRAONR
                        lagerplkopp.AONRDELNR = aolev_nr_tab.AONRDELNR
                        lagerplkopp.INKBESTID = aolev_nr_tab.INKBESTID
                        lagerplkopp.BESTNR = aolev_nr_tab.BESTNR
                        lagerplkopp.TRUMMANR = aolev_nr_tab.TRUMMANR
                        lagerplkopp.TRUMMAPLATS = aolev_nr_tab.TRUMMAPLATS
                        lagerplkopp.ORDERNUMMER = aolev_nr_tab.ORDERNUMMER
                        lagerplkopp.BESTDATUM = aolev_nr_tab.BESTDATUM
                        lagerplkopp.LEVDATUM = aolev_nr_tab.LEVDATUM
                        lagerplkopp.LEVERANSDATUM = aolev_nr_tab.LEVERANSDATUM
                        lagerplkopp.BESTALLARE = aolev_nr_tab.BESTALLARE
                        lagerplkopp.DEPNR = aolev_nr_tab.DEPNR
                        lagerplkopp.PROJDEPNR = Guru.GlobalaVariabler:GuruProjdepnr
                        aolev_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                        aolev_nr_tab.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                        IF aolev_nr_tab.INKBESTID > 0 THEN DO:
                           FIND FIRST aobest_nr_tab WHERE aobest_nr_tab.inkbestid = aolev_nr_tab.INKBESTID  NO-LOCK NO-ERROR.
                           IF AVAILABLE aobest_nr_tab THEN DO:
                              ASSIGN 
                              aobest_nr_tab.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              aobest_nr_tab.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                           END.   
                        END.                        
                        IF aolev_nr_tab.TRUMMANR > 0 THEN DO:
                           /*ännu ej aokopp = true*/
                           FIND FIRST lagertrum WHERE lagertrum.DEPNR = lagerplkopp.DEPNR AND lagertrum.TRUMMANR = lagerplkopp.TRUMMANR AND lagertrum.TRUMMA = "S"  NO-LOCK NO-ERROR.
                           IF AVAILABLE lagertrum THEN DO:
                              ASSIGN
                              lagertrum.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              lagertrum.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                           END.
                              
                           FIND FIRST aotrum WHERE  aotrum.DEPNR = lagerplkopp.DEPNR AND aotrum.TRUMMANR = lagerplkopp.TRUMMANR NO-LOCK NO-ERROR.
                           IF AVAILABLE aotrum THEN DO:
                              ASSIGN
                              aotrum.LAGERPLATSI = lagerplkopp.LAGERPLATSI
                              aotrum.LAGERPLATSC = lagerplkopp.LAGERPLATSC.
                              
                           END.   
                           RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
                        END.
                           
                        EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
                        CREATE elagerplkopp.
                        BUFFER-COPY lagerplkopp TO elagerplkopp.              
                        RUN splagerkopp_UI IN leveransaoapph ( INPUT TABLE elagerplkopp).
                        RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).   
                        RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).
                        RUN openbdynspec_UI IN brwproc[5].  
                        RUN lastselectdyn_UI IN brwproc[5].
                        RUN resetcolindex_UI IN brwproc[5].
                                    
                        RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
                        RUN openbdynspec_UI IN brwproc[4].  
                        RUN lastselectdyn_UI IN brwproc[4].
                        RUN resetcolindex_UI IN brwproc[4].
                     END.   
                  END.   
               END.
            END.   
         END.   
      END.   
      ELSE DO:
        MESSAGE "Markera en order på vänster sida och en pplats på höger sida, för att koppla order till pplats" VIEW-AS ALERT-BOX.     
        RETURN.
      END.
   END.      
            
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UTLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UTLP WINDOW-1
ON CHOOSE OF BTN_UTLP IN FRAME FRAME-A /* Till Byggplats */
DO:
  RUN selfocusrow_UI IN brwproc[5].          
   
   IF AVAILABLE lagerplkopp THEN DO:
      Guru.GlobalaVariabler:GuruVlplats = lagerplkopp.LAGERPLATSI.
      IF lagerplkopp.AONRAONR = ""  THEN DO:
         MESSAGE "Markerad rad är inte kopplad till projekt och kan inte tas ut till byggplats." VIEW-AS ALERT-BOX.     
         RETURN.
      END.
      IF lagerplkopp.AONRAONR NE  "" AND lagerplkopp.BESTNR = 0 AND lagerplkopp.INKBESTID = 0 AND lagerplkopp.TRUMMANR = 0 THEN DO:
         MESSAGE "Markerad rad är inte kopplad till någon beställning/uttag/trumma och kan inte tas ut till byggplats.
         För att ta bort bokning av projekt- tryck pil tillbaka." VIEW-AS ALERT-BOX.     
         RETURN.
      END.
      IF lagerplkopp.LEVDATUM = ? THEN DO:
         MESSAGE "Markerad rad är inte mottagen och kan inte tas ut till byggplats. Gör mottagning innan du tar ut från pplats. Om beställningen är tom - tryck pil tillbaka" VIEW-AS ALERT-BOX.     
         RETURN.
      END.
      /*IF lagerplkopp.REST = TRUE THEN DO:
         MESSAGE "Markerad rad har artiklar som är restrapporterade och kan inte skickas till Byggplats innan restrapporteringen är mottagen." VIEW-AS ALERT-BOX.     
         RETURN.
      END.*/        
      IF lagerplkopp.REST = TRUE THEN DO:
         MESSAGE "Vill du ta skicka allt på markerad rad förutom restrapporterade artiklar till Byggplats?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar11 AS LOGICAL.        
         IF svar11 THEN DO:
            EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
            CREATE elagerplkopp.
            BUFFER-COPY lagerplkopp TO elagerplkopp.         
            RUN utlagerkopprest_UI IN leveransaoapph ( INPUT-OUTPUT TABLE elagerplkopp).
            FIND FIRST elagerplkopp  WHERE NO-LOCK NO-ERROR.
            FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = elagerplkopp.INKBESTID  AND lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND lagerplkopp.UTTAGET = FALSE  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE lagerplkopp THEN DO:
               lagerplkopp.LEVDATUM = ?.
            END.   
           
            /*FIND FIRST elagerplkopp  WHERE NO-LOCK NO-ERROR.
             
               FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = elagerplkopp.INKBESTID  AND lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND lagerplkopp.UTTAGET = FALSE  EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE lagerplkopp THEN DO:
                  ASSIGN
                  lagerplkopp.UTTAGET = TRUE
                  lagerplkopp.UTDATUM = TODAY.                  
               END.
            END.   
            FIND FIRST lagerplkopp  WHERE lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE lagerplkopp THEN DO:
               CREATE lagerplkopp.
               ASSIGN
               lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI
               lagerplkopp.LAGERPLATSC = elagerplkopp.LAGERPLATSC
               lagerplkopp.DEPNR = 0
               lagerplkopp.PROJDEPNR = elagerplkopp.PROJDEPNR.            
            END.*/   
            
            FOR EACH aolev_nr_tab WHERE aolev_nr_tab.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND aolev_nr_tab.UTTAGET = FALSE :
               ASSIGN
               aolev_nr_tab.UTTAGET = TRUE
               aolev_nr_tab.UTDATUM = TODAY.                           
            END.         
           
            /*RUN selnextprevrow_UI IN brwproc[5].*/         
            RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).            
            RUN openbdynspec_UI IN brwproc[5].  
            RUN lastselectdyn_UI IN brwproc[5].
            RUN resetcolindex_UI IN brwproc[5].
            RUN setcolsortvar_UI IN brwproc[4] (INPUT "" ).             
            RUN openbdynspec_UI IN brwproc[4].  
            RUN lastselectdyn_UI IN brwproc[4].
            RUN resetcolindex_UI IN brwproc[4].
            RETURN.
         END.            
         ELSE RETURN.
      END.   
          
      MESSAGE "Är det korrekt att du vill att markerad rad på pplats: "
      STRING(lagerplkopp.LAGERPLATSI) + " " + lagerplkopp.LAGERPLATSC + " skall leveraras ut till byggplats för projektnummer: "  
      + lagerplkopp.AONRAONR + " " + STRING(lagerplkopp.AONRDELNR) VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar4 AS LOGICAL.         
      IF svar4 THEN DO:
         EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
         CREATE elagerplkopp.
         BUFFER-COPY lagerplkopp TO elagerplkopp.         
         RUN utlagerkopp_UI IN leveransaoapph ( INPUT-OUTPUT TABLE elagerplkopp).
         FIND FIRST elagerplkopp  WHERE NO-LOCK NO-ERROR.
         IF elagerplkopp.BESTNR > 0 OR elagerplkopp.INKBESTID > 0  OR elagerplkopp.TRUMMANR > 0  THEN DO:  
            FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = elagerplkopp.INKBESTID AND lagerplkopp.BESTNR = elagerplkopp.BESTNR
            AND lagerplkopp.TRUMMANR = elagerplkopp.TRUMMANR AND lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND lagerplkopp.UTTAGET = FALSE  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE lagerplkopp THEN DO:
               ASSIGN
               lagerplkopp.UTTAGET = TRUE
               lagerplkopp.UTDATUM = TODAY.                  
            END.
         END.   
         FIND FIRST lagerplkopp  WHERE lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp THEN DO:
            CREATE lagerplkopp.
            ASSIGN
            lagerplkopp.LAGERPLATSI = elagerplkopp.LAGERPLATSI
            lagerplkopp.LAGERPLATSC = elagerplkopp.LAGERPLATSC
            lagerplkopp.DEPNR = 0
            lagerplkopp.PROJDEPNR = elagerplkopp.PROJDEPNR.            
         END.   
         
         FOR EACH aolev_nr_tab WHERE aolev_nr_tab.LAGERPLATSI = elagerplkopp.LAGERPLATSI AND aolev_nr_tab.UTTAGET = FALSE :
            ASSIGN
            aolev_nr_tab.UTTAGET = TRUE
            aolev_nr_tab.UTDATUM = TODAY.            
         END.         
         RUN selnextprevrow_UI IN brwproc[5].         
         RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).            
         RUN openbdynspec_UI IN brwproc[5].  
         RUN lastselectdyn_UI IN brwproc[5].
         RUN resetcolindex_UI IN brwproc[5].
         RUN setcolsortvar_UI IN brwproc[4] (INPUT "" ).             
         RUN openbdynspec_UI IN brwproc[4].  
         RUN lastselectdyn_UI IN brwproc[4].
         RUN resetcolindex_UI IN brwproc[4].
                  

      END.                 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VILP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VILP WINDOW-1
ON CHOOSE OF BTN_VILP IN FRAME FRAME-A /* Visa P-plats */
DO:
   utg = FALSE.
   RUN  LPLVISA.W (OUTPUT lpvisa).     
   IF levtill = 3 THEN DO:
      IF lpvisa = 1 THEN levtill = 33.
      ELSE IF lpvisa = 2 THEN levtill = 32.
      ELSE IF lpvisa = 3 THEN levtill = 3.
      IF lpvisa = 1 OR lpvisa = 2 OR lpvisa = 3 THEN DO: 
      
         FIND FIRST lagerplkopp   NO-ERROR.
         IF AVAILABLE lagerplkopp  THEN DO:  
            APPLY "VALUE-CHANGED" TO BRW_LPLATS     IN FRAME FRAME-A.   
            Guru.GlobalaVariabler:GuruVlplats = lagerplkopp.LAGERPLATSI.              
            IF lagerplkopp.INKBESTID > 0  THEN DO:                         
               vald_best =  lagerplkopp.INKBESTID.              
            END.
            ELSE IF  lagerplkopp.BESTNR > 0 THEN DO:
               utg = TRUE.            
               vald_best =  lagerplkopp.BESTNR.                
            END.
            ELSE IF  lagerplkopp.TRUMMANR > 0 THEN DO:
               utg = ?.            
               vald_best =  lagerplkopp.TRUMMANR.                
            END.
            IF musz = TRUE THEN musz = FALSE.   
            ASSIGN    
            skrivut = FALSE.   
            {AVBGOM.I}
            RUN VISALEVEV.W.
            {AVBFRAM.I}                              
         END.
         ELSE DO:
            MESSAGE "Det finns inte några pplats att visa."   VIEW-AS ALERT-BOX.
         END.
      END.   
      levtill = 3.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MOT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MOT WINDOW-1
ON CHOOSE OF FBTN_MOT IN FRAME FRAME-A /* Mottagit */
DO:   
   {muswait.i} 
   IF RAD_VAL2 = 1 OR RAD_VAL2 = 2 THEN DO:    
   
      
      IF RAD_VAL = 2 THEN DO:
         MESSAGE "Funktionen går ej att utföra på 'färdiga leveranser'." 
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:       
         APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.   
         IF AVAILABLE best_nr_tab THEN DO:     
<<<<<<< HEAD
            EMPTY TEMP-TABLE felmeddtemp  NO-ERROR.
            IF musz = TRUE THEN musz = FALSE. 
            RUN anvdepkollti_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr ,INPUT globanv ,OUTPUT danv,OUTPUT TABLE felmeddtemp ).
=======
            EMPTY TEMP-TABLE felmeddtemp  NO-ERROR. 
            RUN anvdepkollti_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr ,INPUT Guru.Konstanter:globanv ,OUTPUT danv,OUTPUT TABLE felmeddtemp ).
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
            FIND FIRST felmeddtemp NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
               IF svar THEN DO:
                  DELETE felmeddtemp.
                  depatitta = TRUE.
                  {AVBGOM.I}
                  RUN DEPMOTV.W.
                  {AVBFRAM.I}   
                  depatitta = FALSE.
               END.
               ELSE DO:                  
                  DELETE felmeddtemp.
                  RETURN NO-APPLY.
               END.         
            END.  
            ELSE DO:
               RUN anvdeplas_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.Konstanter:globanv, INPUT "Uttag" ).
               {AVBGOM.I}
               RUN DEPMOTV.W.
               {AVBFRAM.I}  
               RUN anvdeplbort_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr ).
            END.
            IF musz = FALSE THEN DO:
               RUN besthuvhmt_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab).                
               FIND FIRST best_nr_tab WHERE best_nr_tab.Bestnr >= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
               IF NOT AVAILABLE best_nr_tab THEN DO:
                  FIND PREV best_nr_tab WHERE best_nr_tab.Bestnr <= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
               END.
               IF AVAILABLE best_nr_tab THEN DO:
                  RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_nr_tab)).
               END.        
               RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
               RUN openbdynspec_UI IN brwproc[1].  
               RUN lastselectdyn_UI IN brwproc[1].
               FIND FIRST lev_nr_tab WHERE lev_nr_tab.Bestnr = vald_best NO-LOCK NO-ERROR.
               IF AVAILABLE lev_nr_tab THEN DO:
                  RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(lev_nr_tab)).
               END.
               RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
               RUN openbdynspec_UI IN brwproc[2]. 
               RUN lastselectdyn_UI IN brwproc[2].
      
               IF RAD_VAL2 = 1 THEN DO:     
                  EMPTY TEMP-TABLE best_nr_tab NO-ERROR.
                  EMPTY TEMP-TABLE lev_nr_tab NO-ERROR.
                  RUN besthuvhmt_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab).                
                  FIND FIRST best_nr_tab WHERE best_nr_tab.Bestnr >= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE best_nr_tab THEN DO:
                     FIND PREV best_nr_tab WHERE best_nr_tab.Bestnr <= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
                  END.
                  IF AVAILABLE best_nr_tab THEN DO:
                     RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_nr_tab)).
                  END.
                  RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
                  RUN openbdynspec_UI IN brwproc[1].
                  RUN lastselectdyn_UI IN brwproc[1].
                  FIND FIRST lev_nr_tab WHERE lev_nr_tab.Bestnr = vald_best NO-LOCK NO-ERROR.
                  IF AVAILABLE lev_nr_tab THEN DO:
                     RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(lev_nr_tab)).
                  END.
                  RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
                  RUN openbdynspec_UI IN brwproc[2]. 
                  RUN lastselectdyn_UI IN brwproc[2].
                  ASSIGN
                  best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BEST = FALSE
                  best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BEST = FALSE
                  lev_nr_tab.AONR:VISIBLE IN BROWSE BRW_LEV = FALSE
                  lev_nr_tab.DELNR:VISIBLE IN BROWSE BRW_LEV = FALSE.
               END.
               ELSE DO:
                  EMPTY TEMP-TABLE best_nr_tab NO-ERROR.
                  EMPTY TEMP-TABLE lev_nr_tab NO-ERROR.
                  RUN besthuvhmt2_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab). 
                  FIND FIRST best_nr_tab WHERE best_nr_tab.Bestnr >= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE best_nr_tab THEN DO:
                     FIND PREV best_nr_tab WHERE best_nr_tab.Bestnr <= vald_best USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
                  END.
                  IF AVAILABLE best_nr_tab THEN DO:
                     RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_nr_tab)).
                  END.
                  RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
                  RUN openbdynspec_UI IN brwproc[1]. 
                  RUN lastselectdyn_UI IN brwproc[1].
                  FIND FIRST lev_nr_tab WHERE lev_nr_tab.Bestnr = vald_best NO-LOCK NO-ERROR.
                  IF AVAILABLE lev_nr_tab THEN DO:
                     RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(lev_nr_tab)).
                  END.
                  RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
                  RUN openbdynspec_UI IN brwproc[2].
                  RUN lastselectdyn_UI IN brwproc[2].
                  ASSIGN
                  best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BEST = TRUE
                  best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BEST = TRUE
                  lev_nr_tab.AONR:VISIBLE IN BROWSE BRW_LEV = TRUE
                  lev_nr_tab.DELNR:VISIBLE IN BROWSE BRW_LEV = TRUE.
               END.   
            END.
         END.      
         ELSE DO: 
            MESSAGE "Det finns inga 'ej färdiga leveranser' att utföra funktionen på."
            VIEW-AS ALERT-BOX.
         END.                  
      END.      
   END.
   ELSE IF RAD_VAL2 = 3 THEN DO:    
      IF RAD_VAL = 2 THEN DO:
         MESSAGE "Funktionen går ej att utföra på 'färdiga leveranser'." 
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:                 
         APPLY "VALUE-CHANGED" TO BRW_AOBEST IN FRAME FRAME-A.   
         IF AVAILABLE aobest_nr_tab THEN DO:       
            EMPTY TEMP-TABLE eaobest_nr_tab NO-ERROR.
            IF musz = TRUE THEN musz = FALSE.
            CREATE eaobest_nr_tab.
            BUFFER-COPY aobest_nr_tab TO eaobest_nr_tab.
            
            IF aobest_nr_tab.LAGERPLATSI = 0 THEN DO:
               MESSAGE "Du måste tilldela orden en pplats innan du kan göra mottagning"
               VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.   
            /* uttag skall göras mottagning för lena 20190514
            IF aobest_nr_tab.BESTNR > 0 THEN DO:
               MESSAGE "Detta är ett uttag. Det skall ej göras mottagning för"
               VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.*/     
            EMPTY TEMP-TABLE felmeddtemp  NO-ERROR. 
            RUN anvdepkollti_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr ,INPUT Guru.Konstanter:globanv ,OUTPUT danv,OUTPUT TABLE felmeddtemp ).
            FIND FIRST felmeddtemp NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO UPDATE svar2 AS LOGICAL.         
               IF svar2 THEN DO:
                  DELETE felmeddtemp.
                  depatitta = TRUE.
                  {AVBGOM.I}
                  RUN DEPMOTVAO.W (INPUT TABLE eaobest_nr_tab).
                  {AVBFRAM.I}   
                  depatitta = FALSE.
               END.
               ELSE DO:                  
                  DELETE felmeddtemp.
                  RETURN NO-APPLY.
               END.         
            END.  
            ELSE DO:
<<<<<<< HEAD
               /*uppdaterar bara projektlageret - inte depå*/               
=======
               /*uppdaterar bara projektlageret - inte depå
               RUN anvdeplas_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.Konstanter:globanv, INPUT "Uttag" ).*/
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
               {AVBGOM.I}
               RUN DEPMOTVAO.W (INPUT TABLE eaobest_nr_tab).
               {AVBFRAM.I}                 
            END.  
            DEBUGGER:SET-BREAK().
            IF musz = FALSE THEN DO:          
               EMPTY TEMP-TABLE aobest_nr_tab NO-ERROR.
               EMPTY TEMP-TABLE aolev_nr_tab NO-ERROR.                       
               RUN aobesthuvhmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE aobest_nr_tab, OUTPUT TABLE aolev_nr_tab).
               FIND FIRST eaobest_nr_tab NO-ERROR.
               FIND FIRST aolev_nr_tab WHERE aolev_nr_tab.INKBESTID = eaobest_nr_tab.INKBESTID AND aolev_nr_tab.BESTNR = eaobest_nr_tab.BESTNR AND
               aolev_nr_tab.TRUMMANR = eaobest_nr_tab.TRUMMANR AND aolev_nr_tab.AONRAONR = eaobest_nr_tab.AONRAONR AND aolev_nr_tab.AONRDELNR = eaobest_nr_tab.AONRDELNR AND
               aolev_nr_tab.DEPNR = Guru.GlobalaVariabler:GuruVdepnr   NO-ERROR.
               IF eaobest_nr_tab.TRUMMANR > 0 THEN DO:
                  FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = eaobest_nr_tab.INKBESTID AND lagerplkopp.BESTNR = eaobest_nr_tab.BESTNR AND
                  lagerplkopp.TRUMMANR = eaobest_nr_tab.TRUMMANR AND lagerplkopp.AONRAONR = eaobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = eaobest_nr_tab.AONRDELNR AND
                  lagerplkopp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr AND lagerplkopp.BESTDATUM  = eaobest_nr_tab.BESTDATUM    NO-ERROR.                                     
               END.
               ELSE DO:                  
                  FIND FIRST lagerplkopp WHERE lagerplkopp.INKBESTID = eaobest_nr_tab.INKBESTID AND lagerplkopp.BESTNR = eaobest_nr_tab.BESTNR AND
                  lagerplkopp.TRUMMANR = eaobest_nr_tab.TRUMMANR AND lagerplkopp.AONRAONR = eaobest_nr_tab.AONRAONR AND lagerplkopp.AONRDELNR = eaobest_nr_tab.AONRDELNR AND
                  lagerplkopp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr  AND lagerplkopp.UTTAGET = FALSE NO-ERROR.            
               END.
               IF AVAILABLE aolev_nr_tab THEN DO:               
                  IF AVAILABLE lagerplkopp  THEN DO:
                     lagerplkopp.LEVDATUM = aolev_nr_tab.LEVDATUM.
                  END.
               END.
               IF AVAILABLE lagerplkopp  THEN DO:
                  FIND FIRST aobest_nr_tabbuff WHERE aobest_nr_tabbuff.INKBESTID = vald_best AND aobest_nr_tabbuff.REST = TRUE NO-LOCK NO-ERROR.
                  IF AVAILABLE aobest_nr_tabbuff THEN DO:
                     lagerplkopp.REST = TRUE.
                     RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(aobest_nr_tabbuff)).
                  END.
                  ELSE DO:
                     lagerplkopp.REST = FALSE.
                     RUN selnextprevrow_UI IN brwproc[3].
                   
                  END.    
               END. 
               RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").
               RUN openbdynspec_UI IN brwproc[3].
               RUN lastselectdyn_UI IN brwproc[3]. 
               RUN resetcolindex_UI IN brwproc[3].
                          
               RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
               RUN openbdynspec_UI IN brwproc[4].                          
               RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).            
               RUN openbdynspec_UI IN brwproc[5].  
               RUN lastselectdyn_UI IN brwproc[5].
               RUN resetcolindex_UI IN brwproc[5].
            END.    
         END.   
         ELSE DO: 
            MESSAGE "Det finns inga 'ej färdiga leveranser' att utföra funktionen på."
            VIEW-AS ALERT-BOX.
         END. 
      END.
   END.             
   musz = FALSE.       
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:
   {muswait.i}
   IF RAD_VAL2 = 1 OR RAD_VAL2 = 2 THEN DO:
      IF levtyp = 1 THEN DO: 
         FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE best_nr_tab THEN DO:
            APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.  
            IF musz = TRUE THEN musz = FALSE.   
            ASSIGN    
            skrivut = FALSE.   
            {AVBGOM.I}
            RUN VISALEVEV.W.
            {AVBFRAM.I} 
         END.
         ELSE DO:
            MESSAGE "Det finns inte några 'ej färdiga leveranser' att visa." 
            VIEW-AS ALERT-BOX.
         END.
      END.   
      ELSE DO:
         FIND FIRST lev_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE lev_nr_tab THEN DO:
            APPLY "VALUE-CHANGED" TO BRW_LEV IN FRAME FRAME-A.  
            IF musz = TRUE THEN musz = FALSE.   
            ASSIGN    
            skrivut = FALSE.   
            {AVBGOM.I}
            RUN VISALEVEV.W.
            {AVBFRAM.I} 
         END.
         ELSE DO:
            MESSAGE "Det finns inte några 'färdiga leveranser' att visa." 
            VIEW-AS ALERT-BOX.
         END.
      END.
   END.
   ELSE IF RAD_VAL2 = 3  THEN DO:    
      IF levtyp = 1 THEN DO: 
         FIND FIRST aobest_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE aobest_nr_tab THEN DO:            
            APPLY "VALUE-CHANGED" TO BRW_AOBEST IN FRAME FRAME-A.  
            IF musz = TRUE THEN musz = FALSE.
            utg = FALSE.
            
            IF aobest_nr_tab.INKBESTID > 0 THEN.   
            ELSE IF aobest_nr_tab.BESTNR > 0 THEN DO:
                utg = TRUE.
                vald_best = aobest_nr_tab.BESTNR.
            END.   
            ELSE IF aobest_nr_tab.TRUMMANR > 0 THEN DO:
                utg = ?.
                vald_best = aobest_nr_tab.TRUMMANR.
            END.   
                           
            ASSIGN    
            skrivut = FALSE.   
            {AVBGOM.I}
            RUN VISALEVEV.W.
            {AVBFRAM.I}             
         END.
         ELSE DO:
            MESSAGE "Det finns inte några 'ej färdiga leveranser' att visa." 
            VIEW-AS ALERT-BOX.
         END.
      END.   
      ELSE DO:
         FIND FIRST aolev_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE aolev_nr_tab THEN DO:
            APPLY "VALUE-CHANGED" TO BRW_AOLEV IN FRAME FRAME-A.  
            IF musz = TRUE THEN musz = FALSE.
            utg = FALSE.
            
            IF aolev_nr_tab.INKBESTID > 0 THEN.   
            ELSE IF aolev_nr_tab.BESTNR > 0 THEN DO:
                utg = TRUE.
                vald_best = aolev_nr_tab.BESTNR.
            END.
            ELSE IF aolev_nr_tab.TRUMMANR > 0 THEN DO:
                utg = ?.
                vald_best = aolev_nr_tab.TRUMMANR.
            END.                      
            ASSIGN    
            skrivut = FALSE.   
            {AVBGOM.I}
            RUN VISALEVEV.W.
            {AVBFRAM.I} 
            levtill = 3.              
         END.
         ELSE DO:
            MESSAGE "Det finns inte några 'färdiga leveranser' att visa." 
            VIEW-AS ALERT-BOX.
         END.
      END.
   END.               
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AAONR WINDOW-1
ON ANY-KEY OF FILL-IN-AAONR IN FRAME FRAME-A /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-AAONR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TRUMMAPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRUMMAPLATS WINDOW-1
ON ANY-KEY OF FILL-IN-TRUMMAPLATS IN FRAME FRAME-A /* Trumma namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TRUMMAPLATS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   RAD_VAL = INPUT RAD_VAL.
   levtill = RAD_VAL2.
   IF RAD_VAL2 = 1 OR RAD_VAL2 = 2 THEN DO:
      ASSIGN
      BRW_AOLEV:HIDDEN = TRUE
      BRW_AOBEST:HIDDEN = TRUE
      BRW_UTAO:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = TRUE
      BTN_OVER:HIDDEN = TRUE
      BTN_BACK:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UTLP:HIDDEN = TRUE
      BTN_VILP:HIDDEN = TRUE
      TOG_VUTT:HIDDEN = TRUE
      FILL-IN-AAONR:HIDDEN = TRUE
      FILL-IN-TRUMMAPLATS:HIDDEN = TRUE
      IMAGE-16:HIDDEN = TRUE. 
      IF RAD_VAL = 1 THEN DO: 
         ASSIGN
         BRW_LEV:HIDDEN = TRUE
         BRW_BEST:HIDDEN = FALSE
         levtyp = 1.         
         FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE best_nr_tab THEN   
         APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.
      END.
      ELSE IF RAD_VAL = 2 THEN DO:
         ASSIGN
         BRW_LEV:HIDDEN = FALSE
         BRW_BEST:HIDDEN = TRUE
         levtyp = 2.
         FIND FIRST lev_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE lev_nr_tab THEN 
         APPLY "VALUE-CHANGED" TO BRW_LEV IN FRAME FRAME-A.
      END.     
   END.   
   ELSE IF RAD_VAL2 = 3 THEN DO:
      
      ASSIGN 
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE.
      IF Guru.Konstanter:globniv = 0 THEN.
      ELSE IF Guru.Konstanter:globniv = 15 THEN.
      ELSE DO:   
         IF depatemp.ANVANDARE NE Guru.Konstanter:globanv THEN DO:
            ASSIGN 
            BTN_NY:HIDDEN = TRUE
            BTN_BORT:HIDDEN = TRUE.   
         END.            
      END.
      ASSIGN
      BRW_LEV:HIDDEN = TRUE 
      BRW_BEST:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      /*BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE*/
      BTN_UTLP:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE
      BRW_UTAO:HIDDEN = TRUE
      FILL-IN-AAONR:HIDDEN = FALSE
      FILL-IN-TRUMMAPLATS:HIDDEN = FALSE
      IMAGE-16:HIDDEN = FALSE.
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).
      RUN openbdynspec_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].
      RUN resetcolindex_UI IN brwproc[5].
      IF RAD_VAL = 1 THEN DO: 
         ASSIGN
         BRW_AOLEV:HIDDEN = TRUE
         BRW_AOBEST:HIDDEN = FALSE
         levtyp = 1.
         FIND FIRST aobest_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE aobest_nr_tab THEN   
         APPLY "VALUE-CHANGED" TO BRW_AOBEST IN FRAME FRAME-A.
      END.
      ELSE IF RAD_VAL = 2 THEN DO:
         ASSIGN
         BRW_AOLEV:HIDDEN = FALSE
         BRW_AOBEST:HIDDEN = TRUE
         levtyp = 2.
         FIND FIRST aolev_nr_tab NO-LOCK NO-ERROR.
         IF AVAILABLE aolev_nr_tab THEN 
         APPLY "VALUE-CHANGED" TO BRW_AOLEV IN FRAME FRAME-A.
      END.
   END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL2 WINDOW-1
ON VALUE-CHANGED OF RAD_VAL2 IN FRAME FRAME-A
DO:
   RAD_VAL2 = INPUT RAD_VAL2.
   levtill = RAD_VAL2.
   IF RAD_VAL2 = 1 OR RAD_VAL2 = 2 THEN DO:
      ASSIGN      
      BRW_AOLEV:HIDDEN = TRUE
      BRW_AOBEST:HIDDEN = TRUE
      BRW_UTAO:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = TRUE
      BTN_OVER:HIDDEN = TRUE
      BTN_BACK:HIDDEN = TRUE      
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UTLP:HIDDEN = TRUE
      BTN_VILP:HIDDEN = TRUE
      TOG_VUTT:HIDDEN = TRUE
      FILL-IN-AAONR:HIDDEN = TRUE
      FILL-IN-TRUMMAPLATS:HIDDEN = TRUE
      IMAGE-16:HIDDEN = TRUE.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN 
         BRW_LEV:HIDDEN = TRUE   
         BRW_BEST:HIDDEN = FALSE.
      END.   
      ELSE IF RAD_VAL = 2 THEN DO:
         ASSIGN 
         BRW_LEV:HIDDEN = FALSE    
         BRW_BEST:HIDDEN = TRUE.
      END.   
      
   END.
   ELSE IF RAD_VAL2 = 3 THEN DO:
      ASSIGN 
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE.
      IF Guru.Konstanter:globniv = 0 THEN.
      ELSE IF Guru.Konstanter:globniv = 15 THEN.
      ELSE DO:   
         IF depatemp.ANVANDARE NE Guru.Konstanter:globanv THEN DO:
            ASSIGN 
            BTN_NY:HIDDEN = TRUE
            BTN_BORT:HIDDEN = TRUE.   
         END.            
      END.
      ASSIGN
      BRW_LEV:HIDDEN = TRUE 
      BRW_BEST:HIDDEN = TRUE      
      BRW_AOLEV:HIDDEN = FALSE 
      BRW_AOBEST:HIDDEN = FALSE
      BRW_UTAO:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      /*BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE*/
      BTN_UTLP:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE
      FILL-IN-AAONR:HIDDEN = FALSE
      FILL-IN-TRUMMAPLATS:HIDDEN = FALSE
      IMAGE-16:HIDDEN = FALSE.
      IF RAD_VAL = 1 THEN DO:
         ASSIGN 
         BRW_AOLEV:HIDDEN = TRUE   
         BRW_AOBEST:HIDDEN = FALSE.
      END.   
      ELSE IF RAD_VAL = 2 THEN DO:
         ASSIGN 
         BRW_AOLEV:HIDDEN = FALSE    
         BRW_AOBEST:HIDDEN = TRUE.
      END.
   END.   
      
   IF RAD_VAL2 = 1 THEN DO:     
      EMPTY TEMP-TABLE best_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE lev_nr_tab NO-ERROR.      
      RUN besthuvhmt_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab).                
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].         
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2]. 
      ASSIGN
      best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BEST = FALSE
      best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BEST = FALSE
      lev_nr_tab.AONR:VISIBLE IN BROWSE BRW_LEV = FALSE
      lev_nr_tab.DELNR:VISIBLE IN BROWSE BRW_LEV = FALSE.
   END.
   ELSE IF RAD_VAL2 = 2 THEN DO:
      EMPTY TEMP-TABLE best_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE lev_nr_tab NO-ERROR.
      RUN besthuvhmt2_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab).                
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].         
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2]. 
      ASSIGN
      best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BEST = TRUE
      best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BEST = TRUE
      lev_nr_tab.AONR:VISIBLE IN BROWSE BRW_LEV = TRUE
      lev_nr_tab.DELNR:VISIBLE IN BROWSE BRW_LEV = TRUE.
   END.
   ELSE IF RAD_VAL2 = 3 THEN DO:
      EMPTY TEMP-TABLE aobest_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE aolev_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE lagerplkopp NO-ERROR.
      RUN aobesthuvhmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE aobest_nr_tab, OUTPUT TABLE aolev_nr_tab).
      /*snatdepå styr bara till ett projektlager*/
           
      RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE lagerplkopp).
      FIND FIRST lagerplkopp NO-LOCK NO-ERROR.                    
      
      RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").
      RUN openbdynspec_UI IN brwproc[3].         
      RUN resetcolindex_UI IN brwproc[3].
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
      RUN openbdynspec_UI IN brwproc[4]. 
      RUN resetcolindex_UI IN brwproc[4].
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].
      RUN resetcolindex_UI IN brwproc[5].
   END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VUTT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VUTT WINDOW-1
ON VALUE-CHANGED OF TOG_VUTT IN FRAME FRAME-A /* Visa även uttagna P-platser */
DO:
   TOG_VUTT = INPUT TOG_VUTT.
   IF TOG_VUTT = FALSE THEN DO:
      ASSIGN      
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      BTN_UTLP:HIDDEN = FALSE.
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = FALSE" ).                  
   END.
   ELSE DO:
      ASSIGN   
      BTN_OVER:HIDDEN = TRUE
      BTN_BACK:HIDDEN = TRUE
      BTN_VILP:HIDDEN = TRUE
      BTN_UTLP:HIDDEN = TRUE.
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "" ).               
   END.
   RUN openbdynspec_UI IN brwproc[5].  
   RUN lastselectdyn_UI IN brwproc[5].
   RUN resetcolindex_UI IN brwproc[5].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AOBEST
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(leveransapph) THEN DELETE PROCEDURE leveransapph.
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
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
   /*SNATDEPÅ*/
   FIND FIRST depatemp WHERE depatemp.DEP-NR = Guru.GlobalaVariabler:GuruVdepnr.
   ASSIGN WINDOW-1:TITLE = "Leveransstatus för depå - " + depatemp.BENAMNING.
   /*IF varifran = 2 THEN .       /*uttag*/
   ELSE IF varifran = 3 THEN .  /*trumma*/
   ELSE IF varifran = 4 THEN .  /*inkop AHLSELL*/
   ELSE IF varifran = 5 THEN .  /*inkop uttag */*/
    
   DO:  
      RAD_VAL2 = 1.
      RUN besthuvhmt_UI IN leveransapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE best_nr_tab, OUTPUT TABLE lev_nr_tab).                
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].         
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].         
          
      ASSIGN
      RAD_VAL = 1
      levtyp = 1
      levtill = 1.
      ASSIGN
      best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BEST = FALSE
      best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BEST = FALSE
      lev_nr_tab.AONR:VISIBLE IN BROWSE BRW_LEV = FALSE
      lev_nr_tab.DELNR:VISIBLE IN BROWSE BRW_LEV = FALSE.
   END.   
   FILL-IN-AAONR:LABEL = Guru.Konstanter:gaok.  
   RUN enable_UI.  
   IF globforetag = "ELPA" OR globforetag = "LULE" OR globforetag = "BODE" OR globforetag = "KRAF" OR globforetag = "SNAT" THEN.
   ELSE DO:
      RAD_VAL2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF globforetag = "SNAT" THEN  status-ok = RAD_VAL2:DELETE("LagerProjekt").
   
   {FRMSIZE.I}
<<<<<<< HEAD
=======
   DEBUGGER:SET-BREAK().
   /*IF varifran = 2 THEN DO:
      ASSIGN 
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE.
      IF Guru.Konstanter:globniv = 0 THEN.
      ELSE IF Guru.Konstanter:globniv = 15 THEN.
      ELSE DO:   
         IF depatemp.ANVANDARE NE Guru.Konstanter:globanv THEN DO:
            ASSIGN 
            BTN_NY:HIDDEN = TRUE
            BTN_BORT:HIDDEN = TRUE.   
         END.            
      END.      
      levtill = 3.
      /*uttag*/            
      ASSIGN
      FILL-IN-VTRUMLP:HIDDEN = TRUE
      FILL-IN-VTRUMMA:HIDDEN = TRUE
      FILL-IN-VPROJ:HIDDEN = TRUE
      RAD_VAL:HIDDEN = TRUE
      RAD_VAL2:HIDDEN = TRUE
      BRW_AOLEV:HIDDEN = TRUE
      BRW_AOBEST:HIDDEN = TRUE      
      BRW_LEV:HIDDEN = TRUE
      BRW_BEST:HIDDEN = TRUE 
      FBTN_MOT:HIDDEN = TRUE
      FBTN_VISA:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      /*BTN_NY:HIDDEN = TRUE 
      BTN_BORT:HIDDEN = TRUE*/
      BTN_UTLP:HIDDEN = TRUE 
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE
      BRW_UTAO:HIDDEN = FALSE
      FILL-IN-AAONR:HIDDEN = FALSE
      FILL-IN-TRUMMAPLATS:HIDDEN = FALSE
      IMAGE-16:HIDDEN = FALSE.
     
      EMPTY TEMP-TABLE lagerplkopp NO-ERROR. 
      /*snatdepå styr bara till ett projektlager*/  
      RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE lagerplkopp).        
      /*RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE lagerplkopp).*/                      
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].      
      EMPTY TEMP-TABLE uaotemp  NO-ERROR. 
      RUN uaohmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr, INPUT uttagbnr,OUTPUT TABLE uaotemp).                      
      RUN setcolsortvar_UI IN brwproc[6] (INPUT "" ).      
      RUN openbdynspec_UI IN brwproc[6].
      RUN lastselectdyn_UI IN brwproc[6].
      FIND FIRST uaotemp  NO-LOCK NO-ERROR.
      IF AVAILABLE uaotemp THEN DO:         
         aobrow = ROWID(uaotemp).
         /*vald_best = uaotemp.INKBESTID.*/
         FIND FIRST lagerplkopp  WHERE lagerplkopp.DEPNR = uaotemp.DEPNR AND lagerplkopp.BESTNR = uaotemp.BESTNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.DEPNR = uaotemp.DEPNR AND lagerplkopp.AONRAONR = uaotemp.AONR AND lagerplkopp.AONRDELNR = uaotemp.DELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.   
         IF AVAILABLE lagerplkopp  THEN DO:                       
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                     
            RUN lastselectdyn_UI IN brwproc[5]. 
         END.  
         FIND FIRST uaotemp WHERE ROWID(uaotemp) = aobrow  NO-LOCK NO-ERROR.
         IF AVAILABLE uaotemp THEN DO:
            
            RUN setorgtitle_UI IN brwproc[6] (INPUT "Uttag - tryck pil höger för att koppla projektnummer:  " + uaotemp.AONR + " " + STRING(uaotemp.DELNR) + " till P-plats").            
         END.   
      END.   
      
                             
   END.
   ELSE IF varifran = 3 THEN DO:
      /*trummor*/       
      ASSIGN 
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE.
      IF Guru.Konstanter:globniv = 0 THEN.
      ELSE IF Guru.Konstanter:globniv = 15 THEN.
      ELSE DO:   
         IF depatemp.ANVANDARE NE Guru.Konstanter:globanv THEN DO:
            ASSIGN 
            BTN_NY:HIDDEN = TRUE
            BTN_BORT:HIDDEN = TRUE.   
         END.            
      END.
      levtill = 3.           
      ASSIGN
      FILL-IN-VTRUMLP:HIDDEN = FALSE
      FILL-IN-VTRUMMA:HIDDEN = FALSE
      FILL-IN-VPROJ:HIDDEN = FALSE
      RAD_VAL:HIDDEN = TRUE
      RAD_VAL2:HIDDEN = TRUE
      BRW_AOLEV:HIDDEN = TRUE
      BRW_AOBEST:HIDDEN = TRUE      
      BRW_LEV:HIDDEN = TRUE
      BRW_BEST:HIDDEN = TRUE 
      FBTN_MOT:HIDDEN = TRUE
      FBTN_VISA:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      /*BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE*/
      BTN_UTLP:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE
      BRW_UTAO:HIDDEN = TRUE
      FILL-IN-AAONR:HIDDEN = FALSE
      FILL-IN-TRUMMAPLATS:HIDDEN = FALSE
      IMAGE-16:HIDDEN = FALSE.
      FIND FIRST lplagertrum  NO-LOCK NO-ERROR.
      FILL-IN-VTRUMLP = " Tryck pil höger för att koppla trumma till P-plats".
      FILL-IN-VPROJ = " Valt projektnr: " + lplagertrum.AONR + " " + STRING(lplagertrum.DELNR).
      FILL-IN-VTRUMMA =  "Vald Trumma namn: " + lplagertrum.TRUMMAPLATS + "  " + lplagertrum.ENR + " Lager: " + STRING(lplagertrum.LAGMETER) + " m".
      /*FILL-IN-VTRUMMA = "Vald trumma: " + STRING(lplagertrum.TRUMMANR) + " " + lplagertrum.TRUMMAPLATS + " " + lplagertrum.ENR + " Lager: " + STRING(lplagertrum.LAGMETER) + " m".*/ 
      
      
      DISPLAY FILL-IN-VTRUMLP FILL-IN-VPROJ FILL-IN-VTRUMMA  WITH FRAME FRAME-A. 
      EMPTY TEMP-TABLE lagerplkopp NO-ERROR. 
      /*snatdepå styr bara till ett projektlager*/  
      RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE lagerplkopp).        
      /*RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE lagerplkopp).*/                      
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].      
      
      FIND FIRST lplagertrum  NO-LOCK NO-ERROR.
      IF AVAILABLE lplagertrum THEN DO:         
         aobrow = ROWID(lplagertrum).
         
         FIND FIRST lagerplkopp  WHERE lagerplkopp.DEPNR = lplagertrum.DEPNR AND lagerplkopp.TRUMMANR = lplagertrum.TRUMMANR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.DEPNR = lplagertrum.DEPNR AND lagerplkopp.AONRAONR = lplagertrum.AONR AND lagerplkopp.AONRDELNR = lplagertrum.DELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.   
         IF AVAILABLE lagerplkopp  THEN DO:                       
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                     
            RUN lastselectdyn_UI IN brwproc[5]. 
         END.  
      END.                       
   END.*/   
   /*ELSE IF varifran = 4 THEN DO:
      /*inkop AHLSELL*/
      levtyp = 1.
      levtill = 3.
      ASSIGN
      RAD_VAL2 = 3
      RAD_VAL = 1
      RAD_VAL2:HIDDEN = TRUE 
      RAD_VAL:HIDDEN = TRUE
      FBTN_MOT:HIDDEN = TRUE  
      BRW_LEV:HIDDEN = TRUE 
      BRW_BEST:HIDDEN = TRUE      
      BRW_AOLEV:HIDDEN = FALSE 
      BRW_AOBEST:HIDDEN = FALSE
      BRW_UTAO:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE
      BTN_UTLP:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE.
            
      ASSIGN 
      BRW_AOLEV:HIDDEN = TRUE   
      BRW_AOBEST:HIDDEN = FALSE.
      EMPTY TEMP-TABLE aobest_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE aolev_nr_tab NO-ERROR.
      EMPTY TEMP-TABLE lagerplkopp NO-ERROR.
      /*RUN aobesthuvhmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE aobest_nr_tab, OUTPUT TABLE aolev_nr_tab).
      /*snatdepå styr bara till ett projektlager*/
      */
      EMPTY TEMP-TABLE aobest_nr_tab NO-ERROR. 
      FOR EACH eaobest_mtrl:
                  
         FIND FIRST aobest_nr_tab WHERE aobest_nr_tab.DEPNR = eaobest_mtrl.DEPNR AND aobest_nr_tab.INKBESTID = eaobest_mtrl.INKBESTID AND
         aobest_nr_tab.BESTNR = eaobest_mtrl.BESTNR AND aobest_nr_tab.TRUMMANR = eaobest_mtrl.TRUMMANR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  aobest_nr_tab THEN DO:     
            CREATE aobest_nr_tab.
            BUFFER-COPY eaobest_mtrl TO aobest_nr_tab.               
         END.         
      END.     
      RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE lagerplkopp).
      FIND FIRST lagerplkopp NO-LOCK NO-ERROR.
      
  
      /*RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE aobest_nr_tab.INKBESTID = '" + STRING(uttagbnr) + "'").*/
      RUN openbdynspec_UI IN brwproc[3].         
      
       
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].
      
      APPLY "VALUE-CHANGED" TO BRW_AOBEST IN FRAME FRAME-A.
      
   END.
   ELSE IF  varifran = 5 THEN DO:
      /*inkop UTTAG*/
      levtill = 3.

      ASSIGN
      RAD_VAL2 = 3
      RAD_VAL = 1
      RAD_VAL2:HIDDEN = TRUE 
      RAD_VAL:HIDDEN = TRUE
      FBTN_MOT:HIDDEN = TRUE  
      BRW_LEV:HIDDEN = TRUE 
      BRW_BEST:HIDDEN = TRUE      
      BRW_AOLEV:HIDDEN = TRUE   
      BRW_AOBEST:HIDDEN = TRUE
      FBTN_VISA:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = FALSE
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE
      BTN_UTLP:HIDDEN = FALSE
      BTN_VILP:HIDDEN = FALSE
      TOG_VUTT:HIDDEN = FALSE.
            
      ASSIGN       
      BRW_UTAO:HIDDEN = FALSE.
      EMPTY TEMP-TABLE uaotemp NO-ERROR.      
      EMPTY TEMP-TABLE lagerplkopp NO-ERROR.
      
      /*snatdepå styr bara till ett projektlager*/
      
      
      RUN lphmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE lagerplkopp).
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].
      DEBUGGER:SET-BREAK().
      EMPTY TEMP-TABLE uaotemp  NO-ERROR.
      FOR EACH eaobest_mtrl WHERE eaobest_mtrl.BESTNR > 0 :                  
         FIND FIRST uaotemp WHERE uaotemp.DEPNR = eaobest_mtrl.DEPNR AND  uaotemp.BESTNR = eaobest_mtrl.BESTNR AND uaotemp.ENR = eaobest_mtrl.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE  uaotemp THEN DO:     
            CREATE uaotemp.
            BUFFER-COPY eaobest_mtrl TO uaotemp.
            ASSIGN 
            uaotemp.AONR = eaobest_mtrl.AONRAONR
            uaotemp.DELNR = eaobest_mtrl.AONRDELNR.
                           
         END.         
      END. 
                            
      RUN setcolsortvar_UI IN brwproc[6] (INPUT "" ).      
      RUN openbdynspec_UI IN brwproc[6].
      RUN lastselectdyn_UI IN brwproc[6].
      FIND FIRST uaotemp  NO-LOCK NO-ERROR.
      IF AVAILABLE uaotemp THEN DO:         
         aobrow = ROWID(uaotemp).
         /*vald_best = uaotemp.INKBESTID.*/
         FIND FIRST lagerplkopp  WHERE lagerplkopp.BESTNR = uaotemp.BESTNR AND lagerplkopp.DEPNR = uaotemp.DEPNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE lagerplkopp  THEN DO:           
            FIND FIRST lagerplkopp  WHERE lagerplkopp.AONRAONR = uaotemp.AONR AND lagerplkopp.AONRDELNR = uaotemp.DELNR AND lagerplkopp.UTTAGET = FALSE NO-LOCK NO-ERROR.
         END.            
         IF AVAILABLE lagerplkopp  THEN DO:                       
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).                     
            RUN lastselectdyn_UI IN brwproc[5]. 
         END.  
         FIND FIRST uaotemp WHERE ROWID(uaotemp) = aobrow  NO-LOCK NO-ERROR.
         IF AVAILABLE uaotemp THEN DO:
            
            RUN setorgtitle_UI IN brwproc[6] (INPUT "Uttag - tryck pil höger för att koppla projektnummer:  " + uaotemp.AONR + " " + STRING(uaotemp.DELNR) + " till P-plats").            
         END.   
      END.         
      FIND FIRST lagerplkopp NO-LOCK NO-ERROR.
         
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "lagerplkopp.UTTAGET = false" ).      
      RUN openbdynspec_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].
      
      /*APPLY "VALUE-CHANGED" TO BRW_AOBEST IN FRAME FRAME-A.*/
      
   END.   */
>>>>>>> branch 'master' of file:///\\server05\delad\REMOTEGURU\GuruRemote.git
      
   DO:  
      FIND FIRST best_nr_tab NO-LOCK NO-ERROR.
      IF AVAILABLE best_nr_tab THEN 
      APPLY "VALUE-CHANGED" TO BRW_BEST IN FRAME FRAME-A.
      ASSIGN
      FILL-IN-VTRUMLP:HIDDEN = TRUE
      FILL-IN-VTRUMMA:HIDDEN = TRUE
      FILL-IN-VPROJ:HIDDEN = TRUE
      BRW_LEV:HIDDEN = TRUE
      BRW_AOLEV:HIDDEN = TRUE
      BRW_AOBEST:HIDDEN = TRUE
      BRW_UTAO:HIDDEN = TRUE
      BRW_LPLATS:HIDDEN = TRUE
      BTN_OVER:HIDDEN = TRUE
      BTN_BACK:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UTLP:HIDDEN = TRUE
      BTN_VILP:HIDDEN = TRUE
      TOG_VUTT:HIDDEN = TRUE
      FILL-IN-AAONR:HIDDEN = TRUE
      FILL-IN-TRUMMAPLATS:HIDDEN = TRUE
      IMAGE-16:HIDDEN = TRUE.
    END.   
   
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_AOBEST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_AOLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_LPLATS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6]
      (INPUT BRW_UTAO:HANDLE IN FRAME {&FRAME-NAME}).   
            
   RUN setcolindex_UI IN brwproc[1] (INPUT " BESTNR DESCENDING ").
   RUN setcolindex_UI IN brwproc[2] (INPUT " BESTNR DESCENDING ").   
   /*sortera ej färdiga bara på aonr snat 20190618*/
   RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").
   
   RUN setcolindex_UI IN brwproc[4] (INPUT " LEVDATUM DESCENDING BY AONRAONR BY AONRDELNR ").      
   
   
   RUN dynprogextra IN brwproc[3] (INPUT "rowdispkom_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[3] (INPUT TRUE).
   RUN dynprogextra IN brwproc[4] (INPUT "rowdispkom_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[4] (INPUT TRUE).
   RUN dynprogextra IN brwproc[5] (INPUT "rowdispkom_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[5] (INPUT TRUE).
   /*ÄNDRAD SORTERING 20200616 LENA*/
   RUN setcolindex_UI IN brwproc[5] (INPUT "LAGERPLATSC BY LAGERPLATSI").  
   RUN addfillin_UI IN brwproc[5] 
      (INPUT FILL-IN-TRUMMAPLATS:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TRUMMAPLATS").      
   RUN addfillin_UI IN brwproc[5] 
      (INPUT FILL-IN-AAONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONRAONR").
   RUN addmenuitem_UI IN brwproc[5] (INPUT BRW_LPLATS:HANDLE,INPUT "Ta bort mottagning för markerad rad ",INPUT "btnomott_UI").
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPV.P PERSISTENT SET leveransapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnomott_UI WINDOW-1 
PROCEDURE btnomott_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
      
   status-ok = BRW_LPLATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE lagerplkopp THEN RETURN.
   IF lagerplkopp.UTTAGET = TRUE THEN DO:
      MESSAGE "Det går inte att ta bort mottagning om raden är skickad till Byggplats"
         VIEW-AS ALERT-BOX.
         RETURN.   
   END.   
   
   MESSAGE "Vill du ta bort mottagning för markerad rad?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar10 AS LOGICAL.        
   IF svar10 THEN DO:
      IF lagerplkopp.INKBESTID > 0 OR lagerplkopp.BESTNR > 0 OR lagerplkopp.TRUMMANR > 0 THEN DO:
         IF lagerplkopp.LEVDATUM = ? THEN DO:
            MESSAGE "Det finns inget på denna P-plats som är mottaget"
            VIEW-AS ALERT-BOX.
            RETURN.
         END.
         ELSE DO:
            EMPTY TEMP-TABLE elagerplkopp NO-ERROR.
            ASSIGN
            lagerplkopp.LEVDATUM = ?  
            lagerplkopp.REST = FALSE.             
            CREATE elagerplkopp.
            BUFFER-COPY lagerplkopp TO elagerplkopp.
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(lagerplkopp)).
            RUN lastselectdyn_UI IN brwproc[5].
            RUN resetcolindex_UI IN brwproc[5].
            RUN tabortmott_UI IN leveransaoapph ( INPUT TABLE elagerplkopp, OUTPUT felmed ).            
            EMPTY TEMP-TABLE aobest_nr_tab NO-ERROR.
            EMPTY TEMP-TABLE aolev_nr_tab NO-ERROR.                        
            RUN aobesthuvhmt_UI IN leveransaoapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,OUTPUT TABLE aobest_nr_tab, OUTPUT TABLE aolev_nr_tab).
            RUN setcolindex_UI IN brwproc[3] (INPUT "AONRAONR BY AONRDELNR ").            
            RUN openbdynspec_UI IN brwproc[3].    
            RUN resetcolindex_UI IN brwproc[3].        
            RUN openbdynspec_UI IN brwproc[4].
            RUN resetcolindex_UI IN brwproc[4].         
         END.            
      END.   
      ELSE DO:
         MESSAGE "Det finns inget på denna P-plats att ta bort mottagning för"
         VIEW-AS ALERT-BOX.
         RETURN.   
      END.    
   END.
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
  DISPLAY RAD_VAL2 TOG_VUTT RAD_VAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_VAL2 IMAGE-16 TOG_VUTT BRW_LPLATS RAD_VAL BRW_LEV BRW_UTAO 
         BRW_BEST BRW_AOBEST BRW_AOLEV btn_over btn_back FBTN_MOT FBTN_VISA 
         BTN_NY BTN_BORT BTN_VILP BTN_UTLP FILL-IN-AAONR FILL-IN-TRUMMAPLATS 
         BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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

    IF brwh:NAME = "BRW_LPLATS" THEN DO:  
       IF AVAILABLE lagerplkopp THEN DO:   
          IF lagerplkopp.INKBESTID > 0 AND lagerplkopp.LEVDATUM NE ? THEN DO:                       
               /*färga LEVEREADE INKÖP som har kommentar grön*/
               lagerplkopp.INKBESTID:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LEVDATUM:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSI:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSC:BGCOLOR IN BROWSE BRW_LPLATS = 10.
          END.
          /* uttaget bara grön om  mottagen lena 20190514*/
          IF lagerplkopp.BESTNR > 0 AND lagerplkopp.LEVDATUM NE ?  THEN DO:                       
               /*färga UTTAG grön*/
               lagerplkopp.BESTNR:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LEVDATUM:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSI:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSC:BGCOLOR IN BROWSE BRW_LPLATS = 10.
          END.
          /* trumma bara grön om mottagen lena 20190514*/
          IF lagerplkopp.TRUMMAPLATS NE "" AND lagerplkopp.LEVDATUM NE ? THEN DO:                       
               /*färga TRUMMA grön*/
               lagerplkopp.TRUMMAPLATS:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LEVDATUM:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSI:BGCOLOR IN BROWSE BRW_LPLATS = 10.
               lagerplkopp.LAGERPLATSC:BGCOLOR IN BROWSE BRW_LPLATS = 10.
          END.         
          IF lagerplkopp.REST = TRUE THEN DO:
             lagerplkopp.REST:BGCOLOR IN BROWSE BRW_LPLATS = 12.
          END.   
      END.
   END.   
   ELSE IF brwh:NAME = "BRW_AOLEV" THEN DO:        
      IF AVAILABLE aolev_nr_tab THEN DO:   
         IF aolev_nr_tab.LAGERPLATSI = 0 THEN DO:                       
            /*färga UTTAG LEV som har plockats bort från pplats röd
            färga mottaget inköp  som har plockats bort från pplats röd
            måste gå att ta bort koppling om man vill byta pplats*/            
            aolev_nr_tab.LAGERPLATSI:BGCOLOR IN BROWSE BRW_AOLEV = 12.            
            aolev_nr_tab.LAGERPLATSC:BGCOLOR IN BROWSE BRW_AOLEV = 12.
            IF aolev_nr_tab.BESTNR > 0 THEN aolev_nr_tab.BESTNR:BGCOLOR IN BROWSE BRW_AOLEV = 12.
            IF aolev_nr_tab.INKBESTID > 0 THEN aolev_nr_tab.INKBESTID:BGCOLOR IN BROWSE BRW_AOLEV = 12.
            IF aolev_nr_tab.TRUMMAPLATS NE "" THEN aolev_nr_tab.TRUMMAPLATS:BGCOLOR IN BROWSE BRW_AOLEV = 12.
         END.
                  
      END.
   END.
   ELSE IF brwh:NAME = "BRW_AOBEST" THEN DO:        
      IF AVAILABLE aobest_nr_tab THEN DO:
         IF aobest_nr_tab.REST = TRUE THEN aobest_nr_tab.REST:BGCOLOR IN BROWSE BRW_AOBEST = 12.
         
         IF aobest_nr_tab.LAGERPLATSI = 0 THEN DO:                       
            /*färga UTTAG LEV som har plockats bort från pplats röd
            färga mottaget inköp  som har plockats bort från pplats röd
            måste gå att ta bort koppling om man vill byta pplats*/            
            aobest_nr_tab.LAGERPLATSI:BGCOLOR IN BROWSE BRW_AOBEST = 12.            
            aobest_nr_tab.LAGERPLATSC:BGCOLOR IN BROWSE BRW_AOBEST = 12.
            IF aobest_nr_tab.BESTNR > 0 THEN aobest_nr_tab.BESTNR:BGCOLOR IN BROWSE BRW_AOBEST = 12.
            IF aobest_nr_tab.INKBESTID > 0 THEN aobest_nr_tab.INKBESTID:BGCOLOR IN BROWSE BRW_AOBEST = 12.
            IF aobest_nr_tab.TRUMMAPLATS NE "" THEN aobest_nr_tab.TRUMMAPLATS:BGCOLOR IN BROWSE BRW_AOBEST = 12.
         END.                     
      END.
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
  IF AVAILABLE lagerplkopp THEN DO:     
      
      DISPLAY
      lagerplkopp.LAGERPLATSI        
      lagerplkopp.LAGERPLATSc
      lagerplkopp.AONRAONR
      lagerplkopp.AONRDELNR 
      lagerplkopp.BESTDATUM
      lagerplkopp.LEVDATUM
      lagerplkopp.BESTALLARE  WITH BROWSE BRW_LPLATS.                
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

