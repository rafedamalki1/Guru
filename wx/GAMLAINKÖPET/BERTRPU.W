&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/22/96 - 10:33 am

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
&Scoped-define NEW
{ANVTEMPS.I}
{GLOBVAR2DEL1.I}
{PERSONALTEMP.I}
&Scoped-define SHARED SHARED
{BESTMTRL.I}
{HOPPSEK2W.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{BESTSTATTEMP.I}
{LEVKONTAKTTEMP.I}
{DEPSEKTEMP.I}
{KUNDNRTEMP.I}


{LEVTRPTEMP.I}
{BEREDNINGTEMP.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
{ANMARKTEMP.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE trp_mtrl    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL
    FIELD OPRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER        
    FIELD DBEST AS CHARACTER    
    INDEX ENR IS PRIMARY ENR ASCENDING. 

DEFINE {&NEW} {&SHARED} TEMP-TABLE skapa_mtrl 
   FIELD LEVNAMN AS CHARACTER
   FIELD LKONTAKT AS CHARACTER
   FIELD LTELE AS CHARACTER
   FIELD LADR AS CHARACTER
   FIELD LPNR AS CHARACTER
   FIELD LORT AS CHARACTER   
   FIELD FORE AS CHARACTER
   FIELD KADR AS CHARACTER  
   FIELD KPNR AS CHARACTER
   FIELD KORT AS CHARACTER
   FIELD BOX AS CHARACTER FORMAT "X(5)"
   FIELD FAX AS CHARACTER
   FIELD KIKONTAKT AS CHARACTER                            
   FIELD KITELE AS CHARACTER
   FIELD KTKONTAKT AS CHARACTER  
   FIELD KTTELE AS CHARACTER      
   FIELD DATUM AS DATE
   FIELD MARK AS CHARACTER FORMAT "X(35)"
   FIELD L1 AS CHARACTER FORMAT "X(50)"
   FIELD L2 AS CHARACTER FORMAT "X(50)"
   FIELD L3 AS CHARACTER FORMAT "X(50)"
   FIELD KOM AS CHARACTER FORMAT "X(40)"
   FIELD KUNDNR AS INTEGER FORMAT 99999
   FIELD AVIS AS LOGICAL
   FIELD BESTNR AS CHARACTER
   FIELD KIMOBIL AS CHARACTER
   FIELD KTMOBIL AS CHARACTER
   FIELD DEPA AS CHARACTER
   FIELD LEVTID AS CHARACTER
   FIELD AVISPERS AS CHARACTER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE lev_temp
   FIELD LEVKOD AS CHARACTER.   

DEFINE NEW SHARED VARIABLE detvar AS LOGICAL NO-UNDO.       
DEFINE NEW SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE mailvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE edivar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kontaktrow AS ROWID NO-UNDO. 
DEFINE NEW SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE priset AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE delbest AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bestvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE gammal2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE klockansek AS INTEGER NO-UNDO.  
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
/*LENA*/
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leve AS CHARACTER NO-UNDO.   
DEFINE VARIABLE omra AS CHARACTER NO-UNDO.   
DEFINE VARIABLE valomra AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE levtrprow AS ROWID NO-UNDO.
DEFINE VARIABLE bestoff AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ediben AS CHARACTER NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
DEFINE VARIABLE bestalldvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ejbestalldvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE bertrpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE bernrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE hitem AS Com-handle NO-UNDO.
DEFINE VARIABLE num AS integer NO-UNDO.
DEFINE VARIABLE my1hand AS HANDLE NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO. 
DEFINE VARIABLE medvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE valev AS CHARACTER NO-UNDO.
DEFINE BUFFER kolevtrptemp FOR levtrptemp.     
DEFINE VARIABLE plled AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollib AS INTEGER NO-UNDO.
DEFINE VARIABLE ahkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE ktpers AS INTEGER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}


/* DEFINE NEW SHARED TEMP-TABLE trp_mtrl   */
/*     FIELD ENR AS CHARACTER              */
/*     FIELD BENAMNING AS CHARACTER        */
/*     FIELD ENHET AS CHARACTER            */
/*     FIELD PRIS AS DECIMAL               */
/*     FIELD OPRIS AS DECIMAL              */
/*     FIELD ANTAL AS INTEGER              */
/*     FIELD BESTANT AS INTEGER            */
/*     FIELD LEVKOD AS CHARACTER           */
/*     FIELD BERLEV AS CHARACTER           */
/*     FIELD DBEST AS CHARACTER            */
/*     INDEX ENR IS PRIMARY ENR ASCENDING. */
    
/* DEFINE NEW SHARED TEMP-TABLE skapa_mtrl    */
/*    FIELD LEVNAMN AS CHARACTER              */
/*    FIELD LKONTAKT AS CHARACTER             */
/*    FIELD LTELE AS CHARACTER                */
/*    FIELD LADR AS CHARACTER                 */
/*    FIELD LPNR AS CHARACTER                 */
/*    FIELD LORT AS CHARACTER                 */
/*    FIELD FORE AS CHARACTER                 */
/*    FIELD KADR AS CHARACTER                 */
/*    FIELD KPNR AS CHARACTER                 */
/*    FIELD KORT AS CHARACTER                 */
/*    FIELD BOX AS CHARACTER FORMAT "X(5)"    */
/*    FIELD FAX AS CHARACTER                  */
/*    FIELD KIKONTAKT AS CHARACTER            */
/*    FIELD KITELE AS CHARACTER               */
/*    FIELD KTKONTAKT AS CHARACTER            */
/*    FIELD KTTELE AS CHARACTER               */
/*    FIELD DATUM AS DATE                     */
/*    FIELD MARK AS CHARACTER FORMAT "X(35)"  */
/*    FIELD L1 AS CHARACTER FORMAT "X(50)"    */
/*    FIELD L2 AS CHARACTER FORMAT "X(50)"    */
/*    FIELD L3 AS CHARACTER FORMAT "X(50)"    */
/*    FIELD KOM AS CHARACTER FORMAT "X(40)"   */
/*    FIELD KUNDNR AS INTEGER FORMAT 99999    */
/*    FIELD AVIS AS LOGICAL                   */
/*    FIELD BESTNR AS CHARACTER               */
/*    FIELD KIMOBIL AS CHARACTER              */
/*    FIELD KTMOBIL AS CHARACTER              */
/*    FIELD DEPA AS CHARACTER                 */
/*    FIELD LEVTID AS CHARACTER               */
/*    FIELD AVISPERS AS CHARACTER.            */

   
/* DEFINE NEW SHARED TEMP-TABLE lev_temp  */
/*    FIELD LEVKOD AS CHARACTER.          */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_ANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastanmtemp personaltemp

/* Definitions for BROWSE BRW_ANM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANM fastanmtemp.OMRADE ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANM 
&Scoped-define QUERY-STRING-BRW_ANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANM OPEN QUERY BRW_ANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANM fastanmtemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for FRAME FRAME-B                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-B ~
    ~{&OPEN-QUERY-BRW_ANM}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-58 FILL-IN-LEVNAMN CMB_KONT ~
FILL-IN-LTELE BRW_PERS FILL-IN-LADR FILL-IN-LFAX FILL-IN-LPNR FILL-IN-LORT ~
BTN_NY BTN_ANDRA BTN_BORT CMB_LEV FBTN_MAIL CMB_DEPA FBTN_AHLSELL ~
FILL-IN-BESTNR FILL-IN-KUNDNR FBTN_EDI FBTN_DEPA FILL-IN-IB FBTN_VISA ~
FILL-IN-FOR FILL-IN-FAX FBTN_SKRIV FILL-IN-KIKONTAKT FILL-IN-KITELE ~
FBTN_AVSTATUS FILL-IN-KIMOBIL FILL-IN-KIEPOST FBTN_VSTATUS ~
FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL FILL-IN-PLKONTAKT ~
FILL-IN-PLMOBIL FILL-IN-DATUM BRW_ANM FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 ~
FILL-IN-MARK FILL-IN-KOM TOG_KAB FILL-IN-AVIS FILL-IN-AVISPERS BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LEVNAMN CMB_KONT FILL-IN-LTELE ~
FILL-IN-LADR FILL-IN-LFAX FILL-IN-LPNR FILL-IN-LORT CMB_LEV FILL-IN-STATUS ~
CMB_DEPA FILL-IN-BESTNR FILL-IN-KUNDNR FILL-IN-IB FILL-IN-FOR FILL-IN-FAX ~
FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL FILL-IN-KIEPOST ~
FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL FILL-IN-PLKONTAKT ~
FILL-IN-PLMOBIL FILL-IN-DATUM FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-MARK ~
FILL-IN-KOM TOG_KAB FILL-IN-AVIS FILL-IN-AVISPERS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-FILL-IN-KIKONTAKT 
       MENU-ITEM m_Vlj_Personal LABEL "Välj Personal" .

DEFINE MENU POPUP-MENU-FILL-IN-KTKONTAKT 
       MENU-ITEM m_Vlj_Personal2 LABEL "Välj Personal" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_AHLSELL 
     LABEL "Best.Ahlsell" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_AVSTATUS 
     LABEL "Återställ status" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_DEPA 
     LABEL "Skicka beställning" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_EDI 
     LABEL "Best.Elektroskandia" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_MAIL 
     LABEL "Best.Onninen" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VSTATUS 
     LABEL "Visa status" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_DEPA AS CHARACTER FORMAT "X(40)":U 
     LABEL "Depåadresser" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 21.38 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KONT AS CHARACTER FORMAT "X(40)":U 
     LABEL "Kontaktperson" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(25)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVIS AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Avisering" 
     VIEW-AS FILL-IN 
     SIZE 4.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVISPERS AS CHARACTER FORMAT "X(30)":U 
     LABEL "Namn + telnr" 
     VIEW-AS FILL-IN 
     SIZE 27.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTNR AS CHARACTER FORMAT "X(20)":U 
     LABEL "Best.nr" 
     VIEW-AS FILL-IN 
     SIZE 22.63 BY 1.21
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     LABEL "Önskad leveransdag" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR AS CHARACTER FORMAT "X(35)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 34.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IB AS CHARACTER FORMAT "X(256)":U 
     LABEL "IB-nr" 
     VIEW-AS FILL-IN 
     SIZE 8.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIEPOST AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-postadress" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIKONTAKT AS CHARACTER FORMAT "X(50)":U 
     LABEL "Kontaktperson inköp" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 TOOLTIP "För att få vallista på personer tryck högra mustangenten." NO-UNDO.

DEFINE VARIABLE FILL-IN-KIMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltele" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KITELE AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tele" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOM AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommentarer" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KTKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson teknik" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 TOOLTIP "För att få vallista på personer tryck högra mustangenten." NO-UNDO.

DEFINE VARIABLE FILL-IN-KTMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltele" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KTTELE AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tele" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KUNDNR AS INTEGER FORMAT "99999":U INITIAL 0 
     LABEL "Kundnummer" 
     VIEW-AS FILL-IN 
     SIZE 15.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L1 AS CHARACTER FORMAT "X(35)":U 
     LABEL "Leveransadress" 
     VIEW-AS FILL-IN 
     SIZE 33.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L2 AS CHARACTER FORMAT "999 99":U 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L3 AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 15.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LADR AS CHARACTER FORMAT "X(20)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVNAMN AS CHARACTER FORMAT "X(25)":U 
     LABEL "Leverantör" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LFAX AS CHARACTER FORMAT "X(12)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 19.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LPNR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LTELE AS CHARACTER FORMAT "X(12)":U 
     LABEL "Tele" 
     VIEW-AS FILL-IN 
     SIZE 19.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MARK AS CHARACTER FORMAT "X(35)":U 
     LABEL "Märkning" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Projektledare" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltele" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STATUS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 22.63 BY 1.21
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 2.

DEFINE VARIABLE TOG_KAB AS LOGICAL INITIAL no 
     LABEL "OBS! SPECIFIKA KABELLÄNGDER" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANM FOR 
      fastanmtemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANM WINDOW-2 _STRUCTURED
  QUERY BRW_ANM NO-LOCK DISPLAY
      fastanmtemp.OMRADE COLUMN-LABEL "Lev-id" FORMAT "x(6)":U
      fastanmtemp.ANMARK COLUMN-LABEL "Anmärkning" FORMAT "X(256)":U
            WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 31 BY 7.5
         TITLE "Dubbel klicka för val.".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS WINDOW-2 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(256)":U
            WIDTH 4
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
            WIDTH 12.5
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 19.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 9.75
         FONT 4
         TITLE "Välj Personal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     FILL-IN-LEVNAMN AT ROW 1.21 COL 22 COLON-ALIGNED
     CMB_KONT AT ROW 2.38 COL 22 COLON-ALIGNED
     FILL-IN-LTELE AT ROW 2.38 COL 63.63 COLON-ALIGNED
     BRW_PERS AT ROW 2.5 COL 28
     FILL-IN-LADR AT ROW 3.63 COL 22 COLON-ALIGNED
     FILL-IN-LFAX AT ROW 3.63 COL 63.63 COLON-ALIGNED
     FILL-IN-LPNR AT ROW 4.79 COL 22 COLON-ALIGNED
     FILL-IN-LORT AT ROW 4.79 COL 63.63 COLON-ALIGNED
     BTN_NY AT ROW 6.92 COL 26.5
     BTN_ANDRA AT ROW 6.92 COL 40.63
     BTN_BORT AT ROW 6.92 COL 54.75
     CMB_LEV AT ROW 8.25 COL 22 COLON-ALIGNED
     FILL-IN-STATUS AT ROW 8.25 COL 63.63 COLON-ALIGNED
     FBTN_MAIL AT ROW 8.5 COL 101.5
     CMB_DEPA AT ROW 9.38 COL 22 COLON-ALIGNED
     FBTN_AHLSELL AT ROW 9.67 COL 101.5
     FILL-IN-BESTNR AT ROW 9.75 COL 63.63 COLON-ALIGNED
     FILL-IN-KUNDNR AT ROW 10.5 COL 22 COLON-ALIGNED
     FBTN_EDI AT ROW 10.83 COL 101.5
     FBTN_DEPA AT ROW 11.08 COL 101.5
     FILL-IN-IB AT ROW 11.25 COL 63.63 COLON-ALIGNED
     FBTN_VISA AT ROW 12 COL 101.5
     FILL-IN-FOR AT ROW 12.42 COL 22 COLON-ALIGNED
     FILL-IN-FAX AT ROW 12.42 COL 63.63 COLON-ALIGNED
     FBTN_SKRIV AT ROW 13.17 COL 101.5
     FILL-IN-KIKONTAKT AT ROW 13.58 COL 22 COLON-ALIGNED
     FILL-IN-KITELE AT ROW 13.58 COL 63.63 COLON-ALIGNED
     FBTN_AVSTATUS AT ROW 14.33 COL 101.5
     FILL-IN-KIMOBIL AT ROW 14.75 COL 22 COLON-ALIGNED
     FILL-IN-KIEPOST AT ROW 14.75 COL 63.63 COLON-ALIGNED
     FBTN_VSTATUS AT ROW 15.5 COL 101.5
     FILL-IN-KTKONTAKT AT ROW 15.92 COL 22 COLON-ALIGNED
     FILL-IN-KTTELE AT ROW 15.92 COL 63.63 COLON-ALIGNED
     FILL-IN-KTMOBIL AT ROW 17.08 COL 22 COLON-ALIGNED
     FILL-IN-PLKONTAKT AT ROW 18.25 COL 22 COLON-ALIGNED
     FILL-IN-PLMOBIL AT ROW 18.25 COL 63.5 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 19.42 COL 22 COLON-ALIGNED
     BRW_ANM AT ROW 19.5 COL 83
     FILL-IN-L1 AT ROW 20.58 COL 22 COLON-ALIGNED
     FILL-IN-L2 AT ROW 20.58 COL 56.25 COLON-ALIGNED NO-LABEL
     FILL-IN-L3 AT ROW 20.58 COL 63.63 COLON-ALIGNED NO-LABEL
     FILL-IN-MARK AT ROW 21.75 COL 22 COLON-ALIGNED
     FILL-IN-KOM AT ROW 23.17 COL 22 COLON-ALIGNED
     TOG_KAB AT ROW 24.29 COL 24
     FILL-IN-AVIS AT ROW 25.79 COL 22 COLON-ALIGNED
     FILL-IN-AVISPERS AT ROW 25.79 COL 41.38 COLON-ALIGNED
     BTN_AVB AT ROW 28 COL 101.5
     "Kontaktperson:" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 6.21 COL 24.63
     RECT-58 AT ROW 6 COL 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.25 BY 28.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Leverans och transport"
         HEIGHT             = 28
         WIDTH              = 115.25
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BRW_PERS FILL-IN-LTELE FRAME-B */
/* BROWSE-TAB BRW_ANM FILL-IN-DATUM FRAME-B */
ASSIGN 
       BRW_ANM:HIDDEN  IN FRAME FRAME-B                = TRUE
       BRW_ANM:MAX-DATA-GUESS IN FRAME FRAME-B         = 1000
       BRW_ANM:COLUMN-RESIZABLE IN FRAME FRAME-B       = TRUE.

ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME FRAME-B                = TRUE.

ASSIGN 
       FILL-IN-KIKONTAKT:POPUP-MENU IN FRAME FRAME-B       = MENU POPUP-MENU-FILL-IN-KIKONTAKT:HANDLE.

ASSIGN 
       FILL-IN-KTKONTAKT:POPUP-MENU IN FRAME FRAME-B       = MENU POPUP-MENU-FILL-IN-KTKONTAKT:HANDLE.

/* SETTINGS FOR FILL-IN FILL-IN-STATUS IN FRAME FRAME-B
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastanmtemp.OMRADE
"OMRADE" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.fastanmtemp.ANMARK
"ANMARK" "Anmärkning" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" "x(256)" "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "12.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_ANM
&Scoped-define SELF-NAME BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANM WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF BRW_ANM IN FRAME FRAME-B /* Dubbel klicka för val. */
DO:
   IF AVAILABLE fastanmtemp THEN DO:
      IF FILL-IN-KOM = "" THEN FILL-IN-KOM = fastanmtemp.ANMARK.
      ELSE FILL-IN-KOM = FILL-IN-KOM + " " + fastanmtemp.ANMARK.
   END.
   DISPLAY FILL-IN-KOM WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS WINDOW-2
ON LEAVE OF BRW_PERS IN FRAME FRAME-B /* Välj Personal */
DO:
   BRW_PERS:HIDDEN = TRUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS WINDOW-2
ON VALUE-CHANGED OF BRW_PERS IN FRAME FRAME-B /* Välj Personal */
DO:
   IF ktpers = 1 THEN DO:   
      ASSIGN
      FILL-IN-KIKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
      FILL-IN-KITELE = personaltemp.TELEFON
      FILL-IN-KIMOBIL = personaltemp.MOBILTEL
      FILL-IN-KIEPOST = SUBSTRING(personaltemp.PERSONSOK,20)
      FILL-IN-AVISPERS = SUBSTRING(personaltemp.FORNAMN,1,1) + "." +
      SUBSTRING(personaltemp.EFTERNAMN,1,8) + personaltemp.MOBILTEL.
      IF FILL-IN-AVIS = TRUE THEN DO:
        DISPLAY FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
      END.
      DISPLAY FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL FILL-IN-KIEPOST WITH FRAME {&FRAME-NAME}.
   END.
   IF ktpers = 2 THEN DO:   
      ASSIGN
      FILL-IN-KTKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
      FILL-IN-KTTELE = personaltemp.TELEFON
      FILL-IN-KTMOBIL = personaltemp.MOBILTEL.            
      DISPLAY FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL  WITH FRAME {&FRAME-NAME}.
   END.

   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA WINDOW-2
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-B /* Ändra */
DO:
   RUN btnandra_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:
   RUN btnavb_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-2
ON CHOOSE OF BTN_BORT IN FRAME FRAME-B /* Ta bort */
DO:
   RUN btnbort_UI.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-2
ON CHOOSE OF BTN_NY IN FRAME FRAME-B /* Ny */
DO:
   RUN btnny_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DEPA WINDOW-2
ON VALUE-CHANGED OF CMB_DEPA IN FRAME FRAME-B /* Depåadresser */
DO:
   RUN vccmbdepa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_KONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KONT WINDOW-2
ON VALUE-CHANGED OF CMB_KONT IN FRAME FRAME-B /* Kontaktperson */
DO:     
   RUN vccmbkont_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-B /* Leverantörer */
DO:  
   RUN cmblev_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AHLSELL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AHLSELL WINDOW-2
ON CHOOSE OF FBTN_AHLSELL IN FRAME FRAME-B /* Best.Ahlsell */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE    
   leverant = INPUT CMB_LEV.  
   IF gammal2 = FALSE THEN DO:  
      detvar = TRUE.          
   END.
   ELSE DO:
      detvar = FALSE.
   END.
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.                              
   IF leverant = "Ahlsell" THEN DO:
      musz = musz.
   END.
   ELSE DO:
      MESSAGE "Denna funktion är bara i drift för Ahlsell."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF FILL-IN-BESTNR = "" THEN DO:
      MESSAGE "Beställningsnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-BESTNR IN FRAME {&FRAME-NAME}.
      RETURN. 
   END.
   
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      /*DE HAR KUNDNUMMER 307800*/
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 7 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.
   ELSE DO:
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 5 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END. 
   
   IF  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FILL-IN-IB = INPUT FILL-IN-IB.      
      kollib = INTEGER(FILL-IN-IB) NO-ERROR.
      IF FILL-IN-IB NE "" AND kollib = 0 THEN DO:
         MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.
      IF LENGTH(FILL-IN-IB) > 6  THEN DO:      
         MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.
   
   IF FILL-IN-FOR = " " THEN DO:
      MESSAGE "Företag får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-KIKONTAKT = " " THEN DO:
      MESSAGE "Kontaktperson inköp får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KIKONTAKT IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF FILL-IN-PLKONTAKT = " " THEN DO:
         MESSAGE "Projektledare får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-PLKONTAKT IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.
   END.
   IF FILL-IN-L1 = " " THEN DO:
      MESSAGE "Leveransadress får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L1 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-L2 = " " THEN DO:
      MESSAGE "Postnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.    
   IF FILL-IN-L2 = ? THEN DO:
      MESSAGE "Postnummer får ej vara '?'."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END. 
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
       RUN attestera_UI IN bertrpapph (INPUT beredningtemp.BERAONR,INPUT beredningtemp.OMRADE, INPUT datvar, OUTPUT medvar).         
       IF medvar NE "" THEN DO:
         MESSAGE medvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN-status = "Mailbeställd" OR FILL-IN-status = "Elektroniskt beställd" THEN DO:
      MESSAGE "Beställning är redan utförd. För att kunna skicka igen,
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka mail 
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.        
   RUN kolltrp_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
      RETURN NO-APPLY. 
   END. 
   /*
   RUN kolltrp2_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,INPUT levtrptemp.BESTNR,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
      RETURN NO-APPLY. 
   END.
   */
   /*
   FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
   AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
   IF AVAILABLE levtrptemp THEN DO:
      FIND FIRST kolevtrptemp WHERE kolevtrptemp.BERNR = levtrptemp.BERNR
      AND kolevtrptemp.OMRADE = levtrptemp.OMRADE AND kolevtrptemp.BERBESTNR = levtrptemp.BERBESTNR 
      kolevtrptemp.LEVKOD NE levtrptemp.LEVKOD USE-INDEX BERNR2 NO-LOCK NO-ERROR.
      IF AVAILABLE kolevtrptemp THEN DO:
         FIND FIRST  WHERE NO-LOCK NO-ERROR.
         MESSAGE ""
         VIEW-AS ALERT-BOX.         
         RETURN. 
      END.
   END.*/

   DO TRANSACTION:
      IF AVAILABLE beredningtemp THEN DO:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      END.   
      IF AVAILABLE levtrptemp THEN DO:
         FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
         IF AVAILABLE depatemp THEN DO:
            ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
         END.       
         RUN levtrp_UI.
         ASSIGN      
         levtrptemp.BESTALLD = "Elektroniskt beställd".
         CREATE elevtrptemp.
         BUFFER-COPY levtrptemp TO elevtrptemp.
      END.   
   END.
   DO TRANSACTION:   
      CREATE beststattemp.
      ASSIGN 
      beststattemp.BESTNR = levtrptemp.BESTNR
      beststattemp.BERNR = beredningtemp.BERNR
      beststattemp.OMRADE = beredningtemp.OMRADE
      beststattemp.ANVANDARE = Guru.Konstanter:globanv
      beststattemp.BESTALLD = "Elektroniskt beställd"
      beststattemp.DATUM = TODAY
      beststattemp.BERDATUM = datvar
      beststattemp.LEVKOD = vald_lev.
      sekunder = TIME.
      RUN SEKTIM.P.
      beststattemp.TID = nytid.
      CREATE ebeststattemp.
      BUFFER-COPY beststattemp TO ebeststattemp.
   END.   
   RUN tlage_UI.   
   RUN skapamtrl_UI.         
   RUN mtrl_UI.
   ASSIGN
   bestoff = "Beställning"
   mailvar = TRUE
   edivar = FALSE.
   FIND FIRST elevtrptemp NO-ERROR.
   FIND FIRST ebeststattemp NO-ERROR.
   RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                               INPUT TABLE ebeststattemp).
   FIND FIRST ebeststattemp NO-ERROR.
   IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   epostvar = INPUT FILL-IN-KIEPOST.
   
   RUN AHLBEST.P (INPUT bestoff).
   
   ASSIGN
   mailvar = FALSE
   bestvar = TRUE.
   IF AVAILABLE beredningtemp THEN DO TRANSACTION:
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE levtrptemp THEN DO:
         ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
         DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      END.      
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR EXCLUSIVE-LOCK NO-ERROR. 
      IF AVAILABLE levtrp2temp THEN DO:            
         ASSIGN 
         levtrp2temp.BESTALLD = "Elektroniskt beställd".
      END.      
   END.   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVSTATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVSTATUS WINDOW-2
ON CHOOSE OF FBTN_AVSTATUS IN FRAME FRAME-B /* Återställ status */
DO:
   RUN btnavstatus_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_DEPA WINDOW-2
ON CHOOSE OF FBTN_DEPA IN FRAME FRAME-B /* Skicka beställning */
DO:           
   {muswait.i}   
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE    
   leverant = INPUT CMB_LEV.  
   IF gammal2 = FALSE THEN DO:  
      detvar = TRUE.          
   END.
   ELSE DO:
      detvar = FALSE.
   END.
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
      
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
   IF FILL-IN-MARK:LABEL = "E-post" THEN DO:
      musz = FALSE.
      RUN EPOSTKOLL.P (INPUT FILL-IN-MARK,OUTPUT musz).
      IF musz = FALSE THEN DO:         
         APPLY "ENTRY" TO FILL-IN-MARK IN FRAME {&FRAME-NAME}.  
         RETURN.         
      END.
      musz = FALSE.
   END.
      
   IF FILL-IN-L2 = ? THEN DO:
      MESSAGE "Postnummer får ej vara '?'."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END. 
   
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.                   
   IF FILL-IN-status = "Beställning skickad"  THEN DO:
      MESSAGE "Beställning är redan utförd. För att kunna skicka igen återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka, återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.
   DO TRANSACTION:
      IF AVAILABLE beredningtemp THEN DO:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      END.   
      IF AVAILABLE levtrptemp THEN DO:
         FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
         IF AVAILABLE depatemp THEN DO:
            ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
         END.       
         RUN levtrp_UI.
         ASSIGN      
         levtrptemp.BESTALLD = "Beställning skickad".
         CREATE elevtrptemp.
         BUFFER-COPY levtrptemp TO elevtrptemp.
      END.   
   END.   
   CREATE beststattemp.
   ASSIGN 
   beststattemp.BESTNR = levtrptemp.BESTNR
   beststattemp.BERNR = beredningtemp.BERNR
   beststattemp.OMRADE = beredningtemp.OMRADE
   beststattemp.ANVANDARE = Guru.Konstanter:globanv
   beststattemp.BESTALLD = "Beställning skickad" 
   beststattemp.DATUM = TODAY
   beststattemp.BERDATUM = datvar
   beststattemp.LEVKOD = vald_lev.
   sekunder = TIME.
   RUN SEKTIM.P.
   beststattemp.TID = nytid. 
   CREATE ebeststattemp.
   BUFFER-COPY beststattemp TO ebeststattemp.
   RUN tlage_UI.   
   RUN skapamtrl_UI.         
   RUN mtrl_UI.   
   ASSIGN
   bestoff = "Projektbeställning"
   mailvar = FALSE
   edivar = TRUE. 
   FIND FIRST elevtrptemp NO-ERROR.
   FIND FIRST ebeststattemp NO-ERROR.
   RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                               INPUT TABLE ebeststattemp).
   FIND FIRST ebeststattemp NO-ERROR.
   IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   epostvar = INPUT FILL-IN-KIEPOST.
   {AVBGOM.I}
   RUN BESTUMEA.W (INPUT bestoff).
   {AVBFRAM.I}
   
   ASSIGN
   edivar = FALSE
   bestvar = TRUE.
   IF AVAILABLE beredningtemp THEN DO:
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
      IF AVAILABLE levtrptemp THEN DO:
         ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
         DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      END.         
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR NO-LOCK NO-ERROR. 
      IF AVAILABLE levtrp2temp THEN DO:            
         ASSIGN 
         levtrp2temp.BESTALLD = "Beställning skickad".
      END.
   END.   
   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EDI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EDI WINDOW-2
ON CHOOSE OF FBTN_EDI IN FRAME FRAME-B /* Best.Elektroskandia */
DO:           
   {muswait.i}   
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE    
   leverant = INPUT CMB_LEV.  
   IF gammal2 = FALSE THEN DO:  
      detvar = TRUE.          
   END.
   ELSE DO:
      detvar = FALSE.
   END.
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
      
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   IF leverant = "Elektroskandia" OR leverant = "Elektroskandia avrop" THEN.
   ELSE DO:      
      MESSAGE "EDI-funktionen är bara i drift för Elektroskandia."
      VIEW-AS ALERT-BOX.     
      RETURN. 
   END.
   IF FILL-IN-BESTNR = " " THEN DO:
      MESSAGE "Beställningsnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-BESTNR IN FRAME {&FRAME-NAME}.
      RETURN. 
   END.      
   IF FILL-IN-BESTNR = "0" THEN DO:
      MESSAGE "Beställningsnummer får ej vara 0."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-BESTNR IN FRAME {&FRAME-NAME}.
      RETURN. 
   END.  
   IF FILL-IN-KUNDNR = 0 THEN DO:
      MESSAGE "Felaktigt kundnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.
   
   IF LENGTH(STRING(FILL-IN-KUNDNR)) NE 5 THEN DO:
      MESSAGE "Felaktigt kundnummer. Kundnr:et skall vara 5 siffror"
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.
   IF FILL-IN-FOR = " " THEN DO:
      MESSAGE "Företag får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-KIKONTAKT = " " THEN DO:
      MESSAGE "Kontaktperson inköp får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KIKONTAKT IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-L1 = " " THEN DO:
      MESSAGE "Leveransadress får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L1 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.    
   IF FILL-IN-L2 = " " THEN DO:
      MESSAGE "Postnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.    
   IF FILL-IN-L2 = ? THEN DO:
      MESSAGE "Postnummer får ej vara '?'."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END. 
      
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.               
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "ELPAC" THEN DO:
       RUN attestera_UI IN bertrpapph (INPUT beredningtemp.BERAONR,INPUT beredningtemp.OMRADE, INPUT datvar, OUTPUT medvar).         
       IF medvar NE "" THEN DO:
         MESSAGE medvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN-status = "EDI-beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd med EDI. För att kunna skicka EDI igen
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka EDI 
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.
   DO TRANSACTION:
      IF AVAILABLE beredningtemp THEN DO:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      END.   
      IF AVAILABLE levtrptemp THEN DO:
         FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
         IF AVAILABLE depatemp THEN DO:
            ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
         END.       
         RUN levtrp_UI.
         ASSIGN      
         levtrptemp.BESTALLD = "EDI-beställd".
         CREATE elevtrptemp.
         BUFFER-COPY levtrptemp TO elevtrptemp.
      END.   
   END.   
   CREATE beststattemp.
   ASSIGN 
   beststattemp.BESTNR = levtrptemp.BESTNR
   beststattemp.BERNR = beredningtemp.BERNR
   beststattemp.OMRADE = beredningtemp.OMRADE
   beststattemp.ANVANDARE = Guru.Konstanter:globanv
   beststattemp.BESTALLD = "EDI-beställd"
   beststattemp.DATUM = TODAY
   beststattemp.BERDATUM = datvar
   beststattemp.LEVKOD = vald_lev.
   sekunder = TIME.
   RUN SEKTIM.P.
   beststattemp.TID = nytid. 
   CREATE ebeststattemp.
   BUFFER-COPY beststattemp TO ebeststattemp.
   RUN tlage_UI.   
   RUN skapamtrl_UI.         
   RUN mtrl_UI.   
   ASSIGN
   bestoff = "Projektbeställning"
   mailvar = FALSE
   edivar = TRUE. 
   FIND FIRST elevtrptemp NO-ERROR.
   FIND FIRST ebeststattemp NO-ERROR.
   RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                               INPUT TABLE ebeststattemp).
   FIND FIRST ebeststattemp NO-ERROR.
   IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   epostvar = INPUT FILL-IN-KIEPOST.
   {AVBGOM.I} 
      
   RUN ELEKTBEST.P (INPUT bestoff).
  
   
   {AVBFRAM.I}
   ASSIGN
   edivar = FALSE
   bestvar = TRUE.
   IF AVAILABLE beredningtemp THEN DO:
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
      IF AVAILABLE levtrptemp THEN DO:
         ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
         DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      END.         
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR NO-LOCK NO-ERROR. 
      IF AVAILABLE levtrp2temp THEN DO:            
         ASSIGN 
         levtrp2temp.BESTALLD = "EDI-beställd".
      END.      
   END.   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MAIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MAIL WINDOW-2
ON CHOOSE OF FBTN_MAIL IN FRAME FRAME-B /* Best.Onninen */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE    
   leverant = INPUT CMB_LEV.  
   IF gammal2 = FALSE THEN DO:  
      detvar = TRUE.          
   END.
   ELSE DO:
      detvar = FALSE.
   END.
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.                              
   IF leverant = "Onninen" OR leverant = "Onninen avrop" THEN DO:
      musz = musz.
   END.
   ELSE DO:
      MESSAGE "Mailfunktionen är bara i drift för Onninen."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF FILL-IN-BESTNR = "" THEN DO:
      MESSAGE "Beställningsnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-BESTNR IN FRAME {&FRAME-NAME}.
      RETURN. 
   END.
   
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.   
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      /*DE HAR KUNDNUMMER 307800*/
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 6 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.
   ELSE DO:
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 5 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END. 
   
   IF  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FILL-IN-IB = INPUT FILL-IN-IB.      
      kollib = INTEGER(FILL-IN-IB) NO-ERROR.
      IF FILL-IN-IB NE "" AND kollib = 0 THEN DO:
         MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.
      IF LENGTH(FILL-IN-IB) > 6  THEN DO:      
         MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.
   
   IF FILL-IN-FOR = " " THEN DO:
      MESSAGE "Företag får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-KIKONTAKT = " " THEN DO:
      MESSAGE "Kontaktperson inköp får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KIKONTAKT IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF  Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF FILL-IN-PLKONTAKT = " " THEN DO:
         MESSAGE "Projektledare får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-PLKONTAKT IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.
   END.
   IF FILL-IN-L1 = " " THEN DO:
      MESSAGE "Leveransadress får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L1 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-L2 = " " THEN DO:
      MESSAGE "Postnummer får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.    
   IF FILL-IN-L2 = ? THEN DO:
      MESSAGE "Postnummer får ej vara '?'."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L2 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END. 
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
       RUN attestera_UI IN bertrpapph (INPUT beredningtemp.BERAONR,INPUT beredningtemp.OMRADE, INPUT datvar, OUTPUT medvar).         
       IF medvar NE "" THEN DO:
         MESSAGE medvar VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN-status = "Mailbeställd" OR FILL-IN-status = "Elektroniskt beställd" THEN DO:
      MESSAGE "Beställning är redan utförd. För att kunna skicka igen,
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka mail 
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.        
   RUN kolltrp_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
      RETURN NO-APPLY. 
   END.    
   /*
   RUN kolltrp2_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,INPUT levtrptemp.BESTNR,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
      RETURN NO-APPLY. 
   END.
   */
   /*
   FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
   AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
   IF AVAILABLE levtrptemp THEN DO:
      FIND FIRST kolevtrptemp WHERE kolevtrptemp.BERNR = levtrptemp.BERNR
      AND kolevtrptemp.OMRADE = levtrptemp.OMRADE AND kolevtrptemp.BERBESTNR = levtrptemp.BERBESTNR 
      kolevtrptemp.LEVKOD NE levtrptemp.LEVKOD USE-INDEX BERNR2 NO-LOCK NO-ERROR.
      IF AVAILABLE kolevtrptemp THEN DO:
         FIND FIRST  WHERE NO-LOCK NO-ERROR.
         MESSAGE ""
         VIEW-AS ALERT-BOX.         
         RETURN. 
      END.
   END.*/

   DO TRANSACTION:
      IF AVAILABLE beredningtemp THEN DO:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      END.   
      IF AVAILABLE levtrptemp THEN DO:
         FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
         IF AVAILABLE depatemp THEN DO:
            ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
         END.       
         RUN levtrp_UI.
         ASSIGN      
         levtrptemp.BESTALLD = "Elektroniskt beställd".
         /*lena töm*/
         EMPTY TEMP-TABLE elevtrptemp  NO-ERROR. 
         CREATE elevtrptemp.
         BUFFER-COPY levtrptemp TO elevtrptemp.
      END.   
   END.
   DO TRANSACTION:   
      CREATE beststattemp.
      ASSIGN 
      beststattemp.BESTNR = levtrptemp.BESTNR
      beststattemp.BERNR = beredningtemp.BERNR
      beststattemp.OMRADE = beredningtemp.OMRADE
      beststattemp.ANVANDARE = Guru.Konstanter:globanv
      beststattemp.BESTALLD = "Elektroniskt beställd"
      beststattemp.DATUM = TODAY
      beststattemp.BERDATUM = datvar
      beststattemp.LEVKOD = vald_lev.
      sekunder = TIME.
      RUN SEKTIM.P.
      beststattemp.TID = nytid.

      CREATE ebeststattemp.
      BUFFER-COPY beststattemp TO ebeststattemp.
   END.   
   RUN tlage_UI.   
   RUN skapamtrl_UI.         
   RUN mtrl_UI.
   ASSIGN
   bestoff = "Beställning"
   mailvar = TRUE
   edivar = FALSE.
   FIND FIRST elevtrptemp NO-ERROR.
   FIND FIRST ebeststattemp NO-ERROR.
   RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                               INPUT TABLE ebeststattemp).
   FIND FIRST ebeststattemp NO-ERROR.
   IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   epostvar = INPUT FILL-IN-KIEPOST.
   {AVBGOM.I}        
   RUN MAILBERU.W (INPUT bestoff).
   {AVBFRAM.I}
   ASSIGN
   mailvar = FALSE
   bestvar = TRUE.
   IF AVAILABLE beredningtemp THEN DO TRANSACTION:
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE levtrptemp THEN DO:
         ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
         DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      END.      
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR EXCLUSIVE-LOCK NO-ERROR. 
      IF AVAILABLE levtrp2temp THEN DO:            
         ASSIGN 
         levtrp2temp.BESTALLD = "Elektroniskt beställd".
      END.      
   END.   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO: 
      APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.      
      IF Guru.Konstanter:globforetag = "gran" THEN DO:
         IF ( INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
            MESSAGE "Felaktigt kundnummer."
            VIEW-AS ALERT-BOX.     
            APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
            RETURN.       
         END.  
      END.
      IF ( INPUT FILL-IN-DATUM LE TODAY OR
      INPUT FILL-IN-DATUM = "") THEN DO:
         MESSAGE "Datum måste vara större än idag."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
         RETURN. 
      END.   
      
      IF gammal2 = FALSE THEN DO:  
         detvar = TRUE.          
      END.
      ELSE DO:
         detvar = FALSE.
      END.      
      {muswait.i}
      DO TRANSACTION:
         IF AVAILABLE beredningtemp THEN DO:
            FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
            AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
         END.   
         IF AVAILABLE levtrptemp THEN DO:
            FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
            IF AVAILABLE depatemp THEN DO:
               ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
            END. 
            RUN levtrp_UI.       
            CREATE elevtrptemp.
            BUFFER-COPY levtrptemp TO elevtrptemp.
         END.   
      END.   
      RUN skapamtrl_UI.                 
      RUN mtrl_UI.   
      ASSIGN
      mailvar = FALSE
      edivar = FALSE
      skrivut = TRUE
      priset = FALSE.
      MESSAGE "OBS! Vill du skriva ut med pris?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         priset = TRUE.
         IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN priset = FALSE.
      END.
      ELSE DO:
         priset = FALSE.
      END.   
      FIND FIRST elevtrptemp NO-ERROR.
      RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                               INPUT TABLE ebeststattemp).
      FIND FIRST elevtrptemp NO-ERROR.
      IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
      epostvar = INPUT FILL-IN-KIEPOST.
      {AVBGOM.I}
      RUN BERBESTU.W (INPUT FILL-IN-STATUS).
      {AVBFRAM.I}       
      IF AVAILABLE beredningtemp THEN DO:         
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
         IF AVAILABLE levtrptemp THEN DO:            
            /* fler best per dag*/
            FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
            levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
            levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR NO-LOCK NO-ERROR. 
            IF AVAILABLE levtrp2temp THEN DO:
               IF levtrp2temp.BESTALLD = "Ej beställd" OR levtrp2temp.BESTALLD = "" OR 
               levtrp2temp.BESTALLD = "Mailoffert" THEN DO:                 
                  ASSIGN
                  ejbestalldvar = TRUE.
               END.
               ELSE DO:
                  ASSIGN
                  ejbestalldvar = FALSE
                  bestalldvar = levtrp2temp.BESTALLD.                     
               END.
            END.            
            IF ejbestalldvar = TRUE THEN DO:
               IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
                  RUN attestera_UI IN bertrpapph (INPUT beredningtemp.BERAONR,INPUT beredningtemp.OMRADE, INPUT datvar, OUTPUT medvar).         
                  IF medvar NE "" THEN DO:
                     MESSAGE medvar VIEW-AS ALERT-BOX TITLE "Meddelande".
                     RETURN NO-APPLY.
                  END.
               END.
               MESSAGE "Har du skickat beställningen?" VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO TITLE "Status" UPDATE svar1 AS LOGICAL.         
               IF svar1 THEN DO: 
                  FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
                  AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE levtrptemp THEN DO:
                     ASSIGN levtrptemp.BESTALLD = "Manuellt beställd".
                     CREATE elevtrptemp.
                     BUFFER-COPY levtrptemp TO elevtrptemp.
                     ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
                     DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
                  END.            
                  /* fler best per dag*/
                  FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
                  levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
                  levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR EXCLUSIVE-LOCK NO-ERROR. 
                  IF AVAILABLE levtrp2temp THEN DO:
                     ASSIGN 
                     levtrp2temp.BESTALLD = "Manuellt beställd".
                     CREATE elevtrp2temp.
                     BUFFER-COPY levtrp2temp TO elevtrp2temp.
                  END.           
                  bestvar = TRUE.     
                  CREATE beststattemp.
                  ASSIGN 
                  beststattemp.BESTNR = levtrptemp.BESTNR
                  beststattemp.BERNR = beredningtemp.BERNR
                  beststattemp.OMRADE = beredningtemp.OMRADE
                  beststattemp.ANVANDARE = Guru.Konstanter:globanv
                  beststattemp.BESTALLD = "Manuellt beställd"
                  beststattemp.DATUM = TODAY
                  beststattemp.BERDATUM = datvar
                  beststattemp.LEVKOD = vald_lev.
                  sekunder = TIME.
                  RUN SEKTIM.P.
                  beststattemp.TID = nytid.
                  CREATE ebeststattemp.
                  BUFFER-COPY beststattemp TO ebeststattemp.
                  FIND FIRST elevtrptemp NO-ERROR.
                  FIND FIRST elevtrp2temp NO-ERROR.
                  FIND FIRST ebeststattemp NO-ERROR.
                  RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                                              INPUT TABLE ebeststattemp).
                  FIND FIRST elevtrptemp NO-ERROR.
                  IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
                  FIND FIRST elevtrp2temp NO-ERROR.
                  IF AVAILABLE elevtrp2temp THEN DELETE elevtrp2temp.
                  FIND FIRST ebeststattemp NO-ERROR.
                  IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
                  RUN tlage_UI.                  
               END.                 
            END.                                        
            ELSE DO:             
               MESSAGE "OBS! Beställningen är: " +  bestalldvar VIEW-AS ALERT-BOX.
            END.               
         END.                                                             
      END.         
   END.     
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO: 
   IF musz = TRUE THEN musz = FALSE.
   {muswait.i}   
   ASSIGN    
   skrivut = FALSE.
   
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.                   
   IF Guru.Konstanter:globforetag = "gran" THEN DO:
      IF FILL-IN-LEVNAMN = "DEPÅ" OR FILL-IN-LEVNAMN = "SPEC.MTRL" THEN.
      ELSE DO:      
         IF ( INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
            MESSAGE "Felaktigt kundnummer."
            VIEW-AS ALERT-BOX.     
            APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
            RETURN.       
         END.  
      END.
   END.
   IF FILL-IN-STATUS = "Ej beställd" THEN DO:   
      IF ( INPUT FILL-IN-DATUM LE TODAY OR
      INPUT FILL-IN-DATUM = "") THEN DO:
         MESSAGE "Datum måste vara större än idag."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
         RETURN. 
      END.   
   END.   
   
   IF gammal2 = FALSE THEN DO:
      detvar = TRUE.                           
   END.
   ELSE DO:
      detvar = FALSE.
   END.
   
   IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
      RUN kolltrp_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY. 
      END.    
      /*
      RUN kolltrp2_UI IN bertrpapph (INPUT vald_lev,INPUT beredningtemp.BERNR,INPUT beredningtemp.OMRADE,INPUT FILL-IN-BESTNR,INPUT levtrptemp.BESTNR,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY. 
      END.
      */
   END.
   DO TRANSACTION:
      IF AVAILABLE beredningtemp THEN DO:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
      END.   
      IF AVAILABLE levtrptemp THEN DO:
         FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
         IF AVAILABLE depatemp THEN DO:
            ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
         END.    
         RUN levtrp_UI.
         CREATE elevtrptemp.
         BUFFER-COPY levtrptemp TO elevtrptemp.
      END.   
   END.   
   RUN skapamtrl_UI.     
   RUN mtrl_UI.
   ASSIGN
   mailvar = FALSE
   edivar = FALSE
   priset = FALSE.
   
   MESSAGE "OBS! Vill du visa med pris?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      priset = TRUE.
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN priset = FALSE.
   END.
   ELSE DO:
      priset = FALSE.
   END.
   FIND FIRST elevtrptemp NO-ERROR.
   RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                                              INPUT TABLE ebeststattemp).
   FIND FIRST elevtrptemp NO-ERROR.
   IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
   epostvar = INPUT FILL-IN-KIEPOST.            
   IF FILL-IN-STATUS NE "Ej beställd" THEN MESSAGE "OBS! Beställningen är: " +  FILL-IN-STATUS VIEW-AS ALERT-BOX.

   {AVBGOM.I}      
   RUN BERBESTU.W (INPUT FILL-IN-STATUS).
   {AVBFRAM.I}
   IF AVAILABLE beredningtemp THEN DO:      
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
      IF AVAILABLE levtrptemp THEN DO:  
         /* fler best per dag*/
         FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
         levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
         levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR NO-LOCK NO-ERROR. 
         IF AVAILABLE levtrp2temp THEN DO:
            IF levtrp2temp.BESTALLD = "Ej beställd" OR levtrp2temp.BESTALLD = "" OR 
            levtrp2temp.BESTALLD = "Mailoffert" THEN DO:
               ASSIGN
               ejbestalldvar = TRUE.
            END.
            ELSE DO:
               ASSIGN
               ejbestalldvar = FALSE
               bestalldvar = levtrp2temp.BESTALLD.                     
            END.
         END.         
         IF ejbestalldvar = TRUE THEN DO:
            IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
                RUN attestera_UI IN bertrpapph (INPUT beredningtemp.BERAONR,INPUT beredningtemp.OMRADE, INPUT datvar, OUTPUT medvar).         
                IF medvar NE "" THEN DO:
                  MESSAGE medvar VIEW-AS ALERT-BOX TITLE "Meddelande".
                  RETURN NO-APPLY.
               END.
            END.
            MESSAGE "Har du skickat beställningen?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Status" UPDATE svar1 AS LOGICAL.                     
            IF svar1 THEN DO:
               FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
               AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE levtrptemp THEN DO:                  
                  ASSIGN levtrptemp.BESTALLD = "Manuellt beställd".
                  CREATE elevtrptemp.
                  BUFFER-COPY levtrptemp TO elevtrptemp.
                  ASSIGN FILL-IN-STATUS = levtrptemp.BESTALLD.
                  DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
               END.                      
               /* fler best per dag*/
               FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
               levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
               levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR NO-LOCK NO-ERROR. 
               IF AVAILABLE levtrp2temp THEN DO:                  
                  ASSIGN 
                  levtrp2temp.BESTALLD = "Manuellt beställd".
                  CREATE elevtrp2temp.
                  BUFFER-COPY levtrp2temp TO elevtrp2temp.
               END.                                 
               
               bestvar = TRUE.                           
               CREATE beststattemp.
               ASSIGN 
               beststattemp.BESTNR = levtrptemp.BESTNR
               beststattemp.BERNR = beredningtemp.BERNR
               beststattemp.OMRADE = beredningtemp.OMRADE
               beststattemp.ANVANDARE = Guru.Konstanter:globanv
               beststattemp.BESTALLD = "Manuellt beställd"
               beststattemp.DATUM = TODAY
               beststattemp.BERDATUM = datvar
               beststattemp.LEVKOD = vald_lev.
               sekunder = TIME.
               RUN SEKTIM.P.                             
               beststattemp.TID = nytid.                  
               CREATE ebeststattemp.
               BUFFER-COPY beststattemp TO ebeststattemp.               
               FIND FIRST elevtrptemp NO-ERROR.
               FIND FIRST elevtrp2temp NO-ERROR.
               FIND FIRST ebeststattemp NO-ERROR.
               RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                                          INPUT TABLE ebeststattemp).
               
               FIND FIRST elevtrptemp NO-ERROR.
               IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
               FIND FIRST elevtrp2temp NO-ERROR.
               IF AVAILABLE elevtrp2temp THEN DELETE elevtrp2temp.
               FIND FIRST ebeststattemp NO-ERROR.
               IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.
               RUN tlage_UI.              
            END.
         END.         
         ELSE DO:             
            /*
            MESSAGE "OBS! Beställningen är: " +  bestalldvar VIEW-AS ALERT-BOX.
            */
         END.               
      END.   
   END.         
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VSTATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VSTATUS WINDOW-2
ON CHOOSE OF FBTN_VSTATUS IN FRAME FRAME-B /* Visa status */
DO:
   RUN btnvstatus_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVIS WINDOW-2
ON LEAVE OF FILL-IN-AVIS IN FRAME FRAME-B /* Avisering */
DO:
  FILL-IN-AVIS = INPUT FILL-IN-AVIS.  
  IF FILL-IN-AVIS = TRUE THEN DO:
     ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
     ENABLE FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
     DISPLAY FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
  END.   
  ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVIS WINDOW-2
ON MOUSE-SELECT-CLICK OF FILL-IN-AVIS IN FRAME FRAME-B /* Avisering */
DO:
   IF INPUT FILL-IN-AVIS = TRUE THEN FILL-IN-AVIS = FALSE.
   IF INPUT FILL-IN-AVIS = FALSE THEN FILL-IN-AVIS = TRUE.
   DISPLAY FILL-IN-AVIS WITH FRAME {&FRAME-NAME}.
   FILL-IN-AVIS = INPUT FILL-IN-AVIS.  
   IF FILL-IN-AVIS = TRUE THEN DO:
      ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
      ENABLE FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BESTNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BESTNR WINDOW-2
ON LEAVE OF FILL-IN-BESTNR IN FRAME FRAME-B /* Best.nr */
DO:
  FILL-IN-BESTNR = INPUT FILL-IN-BESTNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM IN FRAME FRAME-B /* Önskad leveransdag */
DO:
   
   ASSIGN  
   regdatum = FILL-IN-DATUM.
   IF regdatum = ? THEN regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM = regdatum.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FOR WINDOW-2
ON LEAVE OF FILL-IN-FOR IN FRAME FRAME-B /* Företag */
DO:
   RUN onlfillinfor_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-IB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-IB WINDOW-2
ON ENTRY OF FILL-IN-IB IN FRAME FRAME-B /* IB-nr */
DO:
  /*CCC*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-IB WINDOW-2
ON LEAVE OF FILL-IN-IB IN FRAME FRAME-B /* IB-nr */
DO:
   
   FILL-IN-IB = INPUT FILL-IN-IB.      
   kollib = INTEGER(FILL-IN-IB) NO-ERROR.
   IF FILL-IN-IB NE "" AND kollib = 0 THEN DO:     
      MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
      RETURN NO-APPLY.                 .       
   END.
   IF LENGTH(FILL-IN-IB) > 6  THEN DO:      
      MESSAGE "IB-nummer skall vara numeriskt 6 positioner!" VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-IB IN FRAME {&FRAME-NAME}.  
      RETURN NO-APPLY.                 .       
   END.  
   IF FILL-IN-IB = "" AND kollib = 0 THEN DO:     
      FILL-IN-IB:FORMAT = "X(6)".
   END.
   ELSE FILL-IN-IB:FORMAT = "999999". 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KIEPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KIEPOST WINDOW-2
ON ENTRY OF FILL-IN-KIEPOST IN FRAME FRAME-B /* E-postadress */
DO:   
   /*
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KIEPOST WINDOW-2
ON LEAVE OF FILL-IN-KIEPOST IN FRAME FRAME-B /* E-postadress */
DO:
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST.
   epostvar = INPUT FILL-IN-KIEPOST.
   IF FILL-IN-KIEPOST NE "" THEN DO:   
      musz = FALSE.
      RUN EPOSTKOLL.P (INPUT FILL-IN-KIEPOST,OUTPUT musz).   
      IF musz = FALSE THEN DO:
         APPLY "ENTRY" TO FILL-IN-KIEPOST IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                 
      END.      
      musz = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOM WINDOW-2
ON LEAVE OF FILL-IN-KOM IN FRAME FRAME-B /* Kommentarer */
DO:
   FILL-IN-KOM = INPUT FILL-IN-KOM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KUNDNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KUNDNR WINDOW-2
ON LEAVE OF FILL-IN-KUNDNR IN FRAME FRAME-B /* Kundnummer */
DO:
  FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vlj_Personal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vlj_Personal WINDOW-2
ON CHOOSE OF MENU-ITEM m_Vlj_Personal /* Välj Personal */
DO:
   ktpers = 1.
   BRW_PERS:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE.
   BRW_PERS:MOVE-TO-TOP ().   
   FIND FIRST personaltemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:      
      BRW_PERS:HIDDEN = TRUE.
      RETURN NO-APPLY.
   END.
   ELSE RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).        
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   
   APPLY "ENTRY" TO BRW_PERS.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vlj_Personal2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vlj_Personal2 WINDOW-2
ON CHOOSE OF MENU-ITEM m_Vlj_Personal2 /* Välj Personal */
DO:
   ktpers = 2.
   BRW_PERS:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE.
   BRW_PERS:MOVE-TO-TOP ().   
   FIND FIRST personaltemp WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:      
      BRW_PERS:HIDDEN = TRUE.
      RETURN NO-APPLY.
   END.
   ELSE RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).        
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   
   APPLY "ENTRY" TO BRW_PERS.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KAB WINDOW-2
ON VALUE-CHANGED OF TOG_KAB IN FRAME FRAME-B /* OBS! SPECIFIKA KABELLÄNGDER */
DO:
   TOG_KAB = INPUT TOG_KAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANM
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(bertrpapph) THEN DELETE PROCEDURE bertrpapph.
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
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   EMPTY TEMP-TABLE levtrptemp NO-ERROR. 
   EMPTY TEMP-TABLE beststattemp NO-ERROR. 
   EMPTY TEMP-TABLE levtrp2temp NO-ERROR. 
   EMPTY TEMP-TABLE depsektemp NO-ERROR. 
   EMPTY TEMP-TABLE levkontakttemp NO-ERROR. 
   EMPTY TEMP-TABLE kundnrtemp NO-ERROR. 
   EMPTY TEMP-TABLE personaltemp NO-ERROR. 
   {ALLSTARTDYN.I}   
  
   RUN huvud_UI.
   
   RUN enable_UI.   
   {FRMSIZE.I}     
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      DISABLE FILL-IN-KUNDNR WITH FRAME {&FRAME-NAME}.
   END.
   DISABLE FILL-IN-BESTNR WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ENABLE FILL-IN-BESTNR WITH FRAME {&FRAME-NAME}.
   END.     
   IF FILL-IN-AVIS = TRUE THEN ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
   ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.

   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "CSUND" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "BORL" OR
   Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /*Best.Onninen*/
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE DO:
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      /*Skicka beställning*/
      FILL-IN-MARK:LABEL = "E-post".
      FBTN_DEPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:      
      FBTN_DEPA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "cVAST"  OR Guru.Konstanter:globforetag = "cVSAB"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /* Best.Elektroskandia*/
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      RUN kollaha_UI IN bertrpapph (INPUT Guru.Konstanter:globanv,OUTPUT ahkoll).      
      IF ahkoll = TRUE THEN FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ELSE FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   ELSE FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   /*IF Guru.Konstanter:globforetag = "cgran"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /* Best.Ahlsell*/
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "gran"  AND Guru.Konstanter:globanv = "M22537" THEN DO:
      /* Best.Ahlsell*/
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "gran"  AND Guru.Konstanter:globanv = "LEIAKR" THEN DO:
      /* Best.Ahlsell*/
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "gran"  AND Guru.Konstanter:globanv = "P12110" THEN DO:
      /* Best.Ahlsell*/
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "gran"  AND Guru.Konstanter:globanv = "elpao" THEN DO:
      /* Best.Ahlsell*/
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      FBTN_AHLSELL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.*/
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FILL-IN-IB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      FILL-IN-PLKONTAKT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      FILL-IN-PLMOBIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN FILL-IN-IB = ""
      FILL-IN-PLKONTAKT = ""
      FILL-IN-PLMOBIL = "".
      FILL-IN-IB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-PLKONTAKT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-PLMOBIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.

   /* städning Lena 20080820*/
   
   FILL-IN-AVISPERS:HIDDEN = TRUE. 
   {musarrow.i}
   APPLY "VALUE-CHANGED" TO CMB_LEV.   
   IF finns = FALSE THEN DO:
      APPLY "VALUE-CHANGED" TO CMB_DEPA.
   END.
 
   ASSIGN
   FILL-IN-STATUS:FORMAT = "X(256)"
   FILL-IN-BESTNR:FORMAT = "X(20)" 
   /*FILL-IN-BOX:FORMAT = "X(4)" */
   FILL-IN-FAX:FORMAT = "X(12)" 
   /*FILL-IN-KADR:FORMAT = "X(30)"*/
   FILL-IN-KIEPOST:FORMAT = "X(80)" 
   FILL-IN-KIKONTAKT:FORMAT = "X(25)"
   FILL-IN-IB:FORMAT = "999999" 
   FILL-IN-KIMOBIL:FORMAT = "X(15)" 
   FILL-IN-KITELE:FORMAT = "X(12)" 
   /*FILL-IN-KORT:FORMAT = "X(256)" 
   FILL-IN-KPNR:FORMAT = "X(6)" */
   FILL-IN-KTKONTAKT:FORMAT = "X(25)" 
   FILL-IN-KTMOBIL:FORMAT = "X(15)" 
   FILL-IN-KTTELE:FORMAT = "X(12)"
   FILL-IN-LADR:FORMAT = "X(20)" 
   FILL-IN-LEVNAMN:FORMAT = "X(25)" 
   FILL-IN-LFAX:FORMAT = "X(12)" 
   FILL-IN-LORT:FORMAT = "X(256)" 
   FILL-IN-LPNR:FORMAT = "X(6)" 
   FILL-IN-LTELE:FORMAT = "X(12)"   
   FILL-IN-PLKONTAKT:FORMAT = "X(25)" 
   FILL-IN-PLMOBIL:FORMAT = "X(15)".
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "cELPA" {GLOBVES.I} THEN DO:
      ASSIGN     
      FILL-IN-AVISPERS:FORMAT = "X(34)"             
      FILL-IN-FOR:FORMAT = "X(25)"                   
      FILL-IN-L1:FORMAT = "X(25)"       
      FILL-IN-L3:FORMAT = "X(20)"       
      FILL-IN-MARK:FORMAT = "X(20)". 
   END.
   ELSE DO:   
      ASSIGN      
      FILL-IN-AVISPERS:FORMAT = "X(30)"            
      FILL-IN-FOR:FORMAT = "X(35)"             
      FILL-IN-L1:FORMAT = "X(35)"       
      FILL-IN-L3:FORMAT = "X(35)"          
      FILL-IN-MARK:FORMAT = "X(35)". 
   END.
   /*IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      FILL-IN-KOM:FORMAT = "X(256)".
   END.
   ELSE DO:         
      FILL-IN-KOM:FORMAT = "X(80)".
   END.*/
   FILL-IN-KOM:FORMAT = "X(256)".
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "UMEA" THEN DO:
      FILL-IN-DATUM:WIDTH-CHAR = FILL-IN-DATUM:WIDTH-CHAR + 3.
      FILL-IN-DATUM:FORMAT = "9999/99/99".
   END.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN APPLY "ENTRY" TO FILL-IN-FOR.
   ELSE APPLY "ENTRY" TO FILL-IN-KUNDNR.
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_MAIL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MAIL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_DEPA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_DEPA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_AHLSELL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_AHLSELL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_EDI:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EDI:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.

   IF FBTN_VISA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_SKRIV:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_AVSTATUS:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_AVSTATUS:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_VSTATUS:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_VSTATUS:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.  
   BRW_PERS:HIDDEN = TRUE. 
   RUN openbdynspec_UI IN brwproc[2].
   RUN avmark_UI IN brwproc[2].
   
   fastanmtemp.OMRADE:LABEL IN BROWSE BRW_ANM = "Lev-id".
   {WIN_M_SLUT.I}
         
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/  
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_ANM:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERTRPAPP.P PERSISTENT SET bertrpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERTRPAPP.P PERSISTENT SET bertrpapph.
   END. 
   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " ").
   END.
          
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   tthandle = TEMP-TABLE depatemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEPA", INPUT "").
   tthandle = TEMP-TABLE levkontakttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVKONTAKT", INPUT "").   
   tthandle = TEMP-TABLE kundnrtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KUNDNR", INPUT "").   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnandra_UI WINDOW-2 
PROCEDURE btnandra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   CMB_KONT = INPUT FRAME {&FRAME-NAME} CMB_KONT.
   FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev AND
   levkontakttemp.LEVKONTAKT = CMB_KONT NO-LOCK NO-ERROR.
   IF AVAILABLE levkontakttemp THEN DO:
      ASSIGN
      musz = FALSE
      andra = TRUE
      kontaktrow = ROWID(levkontakttemp)      
      status-ok = CMB_KONT:DELETE(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
      
      RUN NYLEVKONU.W.
      
      FIND levkontakttemp WHERE ROWID(levkontakttemp) = kontaktrow NO-LOCK NO-ERROR.
      status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
      CMB_KONT:SCREEN-VALUE = levkontakttemp.LEVKONTAKT.
      APPLY "VALUE-CHANGED" TO CMB_KONT.      
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnavb_UI WINDOW-2 
PROCEDURE btnavb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
   AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE levtrptemp THEN DO:
      FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT FRAME {&FRAME-NAME} CMB_DEPA NO-LOCK NO-ERROR.
      IF AVAILABLE depatemp THEN DO:
         ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
      END.       
      APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
      RUN levtrp_UI.           
   END.      
   EMPTY TEMP-TABLE elevtrptemp NO-ERROR. 
   EMPTY TEMP-TABLE elevtrp2temp NO-ERROR. 
   EMPTY TEMP-TABLE ebeststattemp NO-ERROR. 
   FOR EACH levtrptemp:
      CREATE elevtrptemp.
      BUFFER-COPY levtrptemp TO elevtrptemp.
   END.
   FOR EACH levtrp2temp:
      CREATE elevtrp2temp.
      BUFFER-COPY levtrp2temp TO elevtrp2temp.
   END.
   FOR EACH beststattemp:
      CREATE ebeststattemp.
      BUFFER-COPY beststattemp TO ebeststattemp.
   END.   
   RUN spara2_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                                  INPUT TABLE ebeststattemp).
   EMPTY TEMP-TABLE elevtrptemp NO-ERROR. 
   EMPTY TEMP-TABLE elevtrp2temp NO-ERROR. 
   EMPTY TEMP-TABLE ebeststattemp NO-ERROR.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnavstatus_UI WINDOW-2 
PROCEDURE btnavstatus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill du verkligen ställa tillbaka status till ej beställd?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Status" UPDATE svar2 AS LOGICAL.         
   IF svar2 THEN DO:
      IF AVAILABLE beredningtemp THEN DO TRANSACTION:
         FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
         AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE levtrptemp THEN DO:
            ASSIGN 
            levtrptemp.BESTALLD = "Ej beställd"
            FILL-IN-STATUS = "Ej beställd".
            CREATE elevtrptemp.
            BUFFER-COPY levtrptemp TO elevtrptemp.
            DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.            
            /* fler best per dag*/
            FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
            levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
            levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR
            EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE levtrp2temp THEN DO:
               ASSIGN 
               levtrp2temp.BESTALLD = "Ej beställd".
               CREATE elevtrp2temp.
               BUFFER-COPY levtrp2temp TO elevtrp2temp.
            END.            
            CREATE beststattemp.
            ASSIGN 
            beststattemp.BESTNR = levtrptemp.BESTNR
            beststattemp.BERNR = beredningtemp.BERNR
            beststattemp.OMRADE = beredningtemp.OMRADE
            beststattemp.ANVANDARE = Guru.Konstanter:globanv
            beststattemp.BESTALLD = "Ej beställd"
            beststattemp.DATUM = TODAY
            beststattemp.LEVKOD = vald_lev
            beststattemp.BERDATUM = datvar.
            sekunder = TIME.
            RUN SEKTIM.P.
            beststattemp.TID = nytid.         
            CREATE ebeststattemp.
            BUFFER-COPY beststattemp TO ebeststattemp.
         END.   
      END.
      FIND FIRST elevtrptemp NO-ERROR.
      FIND FIRST elevtrp2temp NO-ERROR.
      FIND FIRST ebeststattemp NO-ERROR.
      RUN spara_UI IN bertrpapph (INPUT TABLE elevtrptemp,INPUT TABLE elevtrp2temp,
                                  INPUT TABLE ebeststattemp).
      FIND FIRST elevtrptemp NO-ERROR.
      IF AVAILABLE elevtrptemp THEN DELETE elevtrptemp.
      FIND FIRST elevtrp2temp NO-ERROR.
      IF AVAILABLE elevtrp2temp THEN DELETE elevtrp2temp.
      FIND FIRST ebeststattemp NO-ERROR.
      IF AVAILABLE ebeststattemp THEN DELETE ebeststattemp.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnbort_UI WINDOW-2 
PROCEDURE btnbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CMB_KONT = INPUT FRAME {&FRAME-NAME} CMB_KONT.
   FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev AND
   levkontakttemp.LEVKONTAKT = CMB_KONT NO-LOCK NO-ERROR.
   IF AVAILABLE levkontakttemp THEN DO:
      MESSAGE "Vill du ta bort kontaktperson " + levkontakttemp.LEVKONTAKT + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         status-ok = CMB_KONT:DELETE(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
         DELETE levkontakttemp.
         RUN dellevkontakt_UI IN bertrpapph (INPUT vald_lev,INPUT CMB_KONT).
         FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
         IF AVAILABLE levkontakttemp THEN DO:
            CMB_KONT:SCREEN-VALUE = levkontakttemp.LEVKONTAKT.
            APPLY "VALUE-CHANGED" TO CMB_KONT.      
         END.           
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnny_UI WINDOW-2 
PROCEDURE btnny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   
   ASSIGN
   musz = FALSE
   andra = FALSE.
   RUN NYLEVKONU.W.   
   IF musz = FALSE THEN DO:
      FIND levkontakttemp WHERE ROWID(levkontakttemp) = kontaktrow NO-LOCK NO-ERROR.
      status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
      CMB_KONT:SCREEN-VALUE = levkontakttemp.LEVKONTAKT.
      APPLY "VALUE-CHANGED" TO CMB_KONT.
   END.
   ELSE DO:
      musz = FALSE.
   END.   
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvstatus_UI WINDOW-2 
PROCEDURE btnvstatus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE beredningtemp THEN DO:
     FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
     AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.
     levtrprow = ROWID(levtrptemp).     
     
     RUN VISSTATU.W (INPUT levtrprow,INPUT levtrptemp.BESTNR,INPUT kalkrow, INPUT datvar).
     
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cmblev_UI WINDOW-2 
PROCEDURE cmblev_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   APPLY "LEAVE" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.
   /* fler best per dag*/
   
   IF AVAILABLE beredningtemp THEN DO:
      FIND FIRST levtrptemp WHERE levtrptemp.LEVKOD = vald_lev AND levtrptemp.BERNR = beredningtemp.BERNR
      AND levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 EXCLUSIVE-LOCK NO-ERROR.
   END.    
   IF AVAILABLE levtrptemp THEN DO:      
      FIND FIRST depatemp WHERE depatemp.BENAMNING = INPUT CMB_DEPA NO-LOCK NO-ERROR.
      IF AVAILABLE depatemp THEN DO:
         ASSIGN levtrptemp.DEP-NR = depatemp.DEP-NR.
      END. 
      /*uppdaterar den föregående leverantören*/
      RUN levtrp_UI.

   END.

   IF forsta = FALSE THEN DO:
      FOR EACH levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev USE-INDEX LEVKOD NO-LOCK:
         ASSIGN 
         status-ok = CMB_KONT:DELETE(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
      END.                   
   END.    
   leverant = INPUT CMB_LEV. 
   
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      CMB_DEPA:list-items = "".
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
      FOR EACH depatemp BY depatemp.BENAMNING:               
         FIND FIRST kundnrtemp WHERE kundnrtemp.DEP-NR = depatemp.DEP-NR AND kundnrtemp.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
         IF AVAILABLE kundnrtemp THEN DO:
            IF kundnrtemp.KUNDNR NE 0  THEN DO:
               status-ok = CMB_DEPA:ADD-LAST(depatemp.BENAMNING)IN FRAME {&FRAME-NAME}.                     
            END.
         END.
      END.       
   END.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.         
   
   IF forsta = FALSE THEN DO:
      FOR EACH levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev USE-INDEX LEVKOD NO-LOCK:
         ASSIGN 
         status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
      END.   
   END.
   ELSE DO:
      forsta = FALSE.
   END.   
   /*finns det någon beställning av denna valda lev*/
   IF AVAILABLE beredningtemp THEN DO:
      FIND FIRST levtrptemp WHERE levtrptemp.BERNR = beredningtemp.BERNR AND levtrptemp.OMRADE = beredningtemp.OMRADE AND
      levtrptemp.LEVKOD = vald_lev USE-INDEX BERNR2 NO-LOCK NO-ERROR.
   END.   
   /*det finns levtrp för val lev*/
   IF AVAILABLE levtrptemp THEN DO:       
      FIND FIRST depatemp WHERE depatemp.DEP-NR = levtrptemp.DEP-NR NO-LOCK NO-ERROR.
      IF AVAILABLE depatemp THEN DO:
         ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING.
      END.        
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND levtrp2temp.BERNR = beredningtemp.BERNR AND 
      levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar 
      USE-INDEX BERED NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE levtrp2temp THEN DO: 
         MESSAGE "Kontakta Elpool 090-184544" VIEW-AS ALERT-BOX.
         FIND LAST levtrp2temp WHERE 
         levtrp2temp.OMRADE = beredningtemp.OMRADE AND
         levtrp2temp.BERNR = beredningtemp.BERNR 
         USE-INDEX NR NO-LOCK NO-ERROR.            
      END.           
      IF AVAILABLE levtrp2temp THEN DO:        
         {EDIBESTNRL.I}
         FILL-IN-STATUS = levtrp2temp.BESTALLD. 
      END.
      ELSE DO:
         FILL-IN-STATUS = levtrptemp.BESTALLD.
      END.
      FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev AND
      levkontakttemp.LEVKONTAKT = levtrptemp.LEVKONTAKT NO-LOCK NO-ERROR.
      IF AVAILABLE levkontakttemp THEN DO:                  
         ASSIGN
         CMB_KONT = levtrptemp.LEVKONTAKT
         FILL-IN-LADR = levtrptemp.LEVADR
         FILL-IN-LPNR = levtrptemp.LEVPNR
         FILL-IN-LORT = levtrptemp.LEVORT
         FILL-IN-LTELE = SUBSTRING(levtrptemp.LEVTEL,1,19)
         FILL-IN-LFAX = SUBSTRING(levtrptemp.LEVTEL,20)
         CMB_KONT:SCREEN-VALUE = levtrptemp.LEVKONTAKT.         
      END.
      ELSE DO:               
         FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev 
         NO-LOCK NO-ERROR.         
         IF AVAILABLE levkontakttemp THEN DO:            
            ASSIGN
            CMB_KONT = levkontakttemp.LEVKONTAKT
            FILL-IN-LTELE = SUBSTRING(levkontakttemp.LEVTEL,1,19)
            FILL-IN-LFAX = SUBSTRING(levkontakttemp.LEVTEL,20)
            FILL-IN-LADR = levkontakttemp.LEVADR
            FILL-IN-LPNR = levkontakttemp.LEVPNR
            FILL-IN-LORT = levkontakttemp.LEVORT
            CMB_KONT:SCREEN-VALUE = levkontakttemp.LEVKONTAKT.         
         END.
      END.   
      ASSIGN
      FILL-IN-LEVNAMN  = levtemp.LEVNAMN
      FILL-IN-KUNDNR = levtrptemp.KUNDNR  
      /*FILL-IN-BESTNR = levtrptemp.BERBESTNR*/   
      FILL-IN-FOR = levtrptemp.FORE
      /*FILL-IN-KADR = levtrptemp.KADR
      FILL-IN-KPNR = levtrptemp.KPNR
      FILL-IN-BOX = levtrptemp.BOX
      FILL-IN-KORT = levtrptemp.KORT  */
      FILL-IN-FAX = levtrptemp.FAX                   
      FILL-IN-L1 = levtrptemp.L1
      FILL-IN-L2 = levtrptemp.L2 
      FILL-IN-L3 = levtrptemp.L3           
      FILL-IN-KIKONTAKT  = SUBSTRING(levtrptemp.KIKONTAKT,1,50)
      FILL-IN-KIEPOST = SUBSTRING(levtrptemp.KIKONTAKT,52)      
      FILL-IN-KITELE = levtrptemp.KITELE              
      FILL-IN-KTKONTAKT = levtrptemp.KTKONTAKT
      /*FILL-IN-PLKONTAKT = SUBSTRING(levtrptemp.KTKONTAKT,52,25)*/
      FILL-IN-KTTELE = levtrptemp.KTTELE     
      FILL-IN-KIMOBIL = levtrptemp.KIMOBIL      
      FILL-IN-KTMOBIL = levtrptemp.KTMOBIL
      /*FILL-IN-PLMOBIL = SUBSTRING(levtrptemp.KTMOBIL,30,15)*/
      FILL-IN-AVISPERS = levtrptemp.AVISPERS
      FILL-IN-DATUM = levtrptemp.DATUM  
/*       CMB_LEVERANS:SCREEN-VALUE = levtrptemp.LEVERANS */
      FILL-IN-MARK = levtrptemp.MARK
      FILL-IN-KOM = levtrptemp.KOM
      FILL-IN-AVIS = levtrptemp.AVIS.
      /* CCC IBPROJ ES nytt ib o projektledare + tel
      Ta bort första adressen från depå - skickas aldrig KADR BOX KPNR KORT */
      ASSIGN
      FILL-IN-PLKONTAKT = levtrptemp.KADR
      FILL-IN-IB = levtrptemp.KPNR      
      FILL-IN-PLMOBIL = levtrptemp.KORT.
      
      IF levtrptemp.KOM BEGINS "OBS! SPECIFIKA KABELLÄNGDER. " THEN DO:
         TOG_KAB = TRUE.
         FILL-IN-KOM = SUBSTRING(levtrptemp.KOM,30).
      END.
      ELSE DO:
         TOG_KAB = FALSE.
      END.
      
      RUN kundnrform_UI.
      DISPLAY FILL-IN-LEVNAMN FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT
      FILL-IN-LTELE FILL-IN-LFAX FILL-IN-KUNDNR FILL-IN-BESTNR FILL-IN-FOR 
      FILL-IN-FAX FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-KIKONTAKT FILL-IN-KIEPOST FILL-IN-KITELE FILL-IN-KTKONTAKT
      FILL-IN-KTTELE FILL-IN-KIMOBIL FILL-IN-KTMOBIL FILL-IN-AVISPERS FILL-IN-DATUM FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS
      FILL-IN-STATUS TOG_KAB WITH FRAME {&FRAME-NAME}.
      IF FILL-IN-AVIS = TRUE THEN ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
      ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.
      ASSIGN  FILL-IN-DATUM:HIDDEN = FALSE.      
      
   END.
   
   ELSE DO TRANSACTION:   
      /*för nästa lev*/
      CREATE levtrptemp.
      IF AVAILABLE beredningtemp THEN DO:
         ASSIGN
         levtrptemp.BERNR = beredningtemp.BERNR
         levtrptemp.OMRADE = beredningtemp.OMRADE.
      END.
      /* fler best per dag*/
      ASSIGN 
      levtrptemp.LEVKOD = vald_lev
      levtrptemp.BESTALLD = "Ej beställd"
      levtrptemp.BESTNR = 1
      levtrptemp.LEVERANS = "".
      ASSIGN
      SUBSTRING(levtrptemp.LEVERANS,1,15) = STRING(INTEGER(datvar))
      SUBSTRING(levtrptemp.LEVERANS,20,15) = STRING(klockansek).
      FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev
      NO-LOCK NO-ERROR.
      IF AVAILABLE levkontakttemp THEN DO:
         ASSIGN
         FILL-IN-LEVNAMN  = levtemp.LEVNAMN
         CMB_KONT = levkontakttemp.LEVKONTAKT
         CMB_KONT:SCREEN-VALUE = levkontakttemp.LEVKONTAKT
         FILL-IN-LTELE = SUBSTRING(levkontakttemp.LEVTEL,1,19)
         FILL-IN-LFAX = SUBSTRING(levkontakttemp.LEVTEL,20)
         FILL-IN-LADR = levkontakttemp.LEVADR
         FILL-IN-LPNR = levkontakttemp.LEVPNR
         FILL-IN-LORT = levkontakttemp.LEVORT
         FILL-IN-KUNDNR = 0
         FILL-IN-AVIS = FALSE
         FILL-IN-STATUS = "Ej beställd".
      END.      
      /* fler best per dag*/
      
      RUN bestnr_UI IN bertrpapph (INPUT beredningtemp.OMRADE,INPUT beredningtemp.BERNR, OUTPUT nyttnr).                    
      FIND LAST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR USE-INDEX NR NO-LOCK NO-ERROR. 
      IF AVAILABLE levtrp2temp THEN DO:
         IF nyttnr <= levtrp2temp.BESTNR THEN nyttnr = levtrp2temp.BESTNR + 1. 
      END.
      CREATE levtrp2temp.
      ASSIGN
      levtrp2temp.BERNR = beredningtemp.BERNR
      levtrp2temp.OMRADE = beredningtemp.OMRADE
      levtrp2temp.LEVKOD = vald_lev
      levtrp2temp.DATUM = datvar
      levtrp2temp.BESTNR = nyttnr
      levtrp2temp.BESTALLD = "Ej beställd".
      /*
      FIND LAST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR 
     /* AND levtrp2temp.BESTNR = levtrptemp.BESTNR */
      USE-INDEX NR NO-LOCK NO-ERROR.         
      IF AVAILABLE levtrp2temp THEN DO:
         nyttnr = levtrp2temp.BESTNR + 1.
         CREATE levtrp2temp.
         ASSIGN
         levtrp2temp.BERNR = beredningtemp.BERNR
         levtrp2temp.OMRADE = beredningtemp.OMRADE
         levtrp2temp.LEVKOD = vald_lev
         levtrp2temp.DATUM = datvar
         levtrp2temp.BESTNR = nyttnr
         levtrp2temp.BESTALLD = "Ej beställd".
      END.
      ELSE DO:
         CREATE levtrp2temp.
         ASSIGN
         levtrp2temp.BERNR = beredningtemp.BERNR
         levtrp2temp.OMRADE = beredningtemp.OMRADE
         levtrp2temp.LEVKOD = vald_lev
         levtrp2temp.DATUM = datvar
         levtrp2temp.BESTNR = 1
         levtrp2temp.BESTALLD = "Ej beställd"
         nyttnr = 1.         
      END.
      */
      levtrptemp.BESTNR = nyttnr.
      {EDIBESTNR.I}
      ASSIGN FILL-IN-DATUM:HIDDEN = FALSE.
      ENABLE FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.      
      FIND FIRST kundnrtemp WHERE kundnrtemp.DEP-NR = depatemp.DEP-NR AND kundnrtemp.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
      IF AVAILABLE kundnrtemp THEN ASSIGN FILL-IN-KUNDNR = kundnrtemp.KUNDNR.
      ELSE ASSIGN FILL-IN-KUNDNR = 0.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:         
         ASSIGN
         FILL-IN-KIKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
         FILL-IN-KITELE = personaltemp.TELEFON
         FILL-IN-KIMOBIL = personaltemp.MOBILTEL
         FILL-IN-KIEPOST = SUBSTRING(personaltemp.PERSONSOK,20)
         FILL-IN-AVISPERS = SUBSTRING(personaltemp.FORNAMN,1,1) + "." +
         SUBSTRING(personaltemp.EFTERNAMN,1,8) + personaltemp.MOBILTEL.
      END.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = beredningtemp.ANVANDARE NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:         
         ASSIGN
         FILL-IN-KTKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
         FILL-IN-KTTELE = personaltemp.TELEFON
         FILL-IN-KTMOBIL = personaltemp.MOBILTEL.         
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:      
         plled = "".
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "AOREF"                   
         inextradatatemp.HUVUDCH = beredningtemp.AONR.                    
         inextradatatemp.HUVUDINT = beredningtemp.DELNR.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp ). 
         FIND FIRST extradatatemp NO-ERROR.
         IF AVAILABLE extradatatemp THEN FILL-IN-IB = extradatatemp.SOKCHAR[1].
         RUN hmtplib_UI IN bertrpapph (INPUT beredningtemp.AONR,INPUT beredningtemp.DELNR,OUTPUT plled).
         FILL-IN-PLKONTAKT = plled.
      END.      
      
      TOG_KAB = FALSE.
      RUN kundnrform_UI.
      DISPLAY FILL-IN-LEVNAMN FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT
      FILL-IN-LTELE FILL-IN-LFAX FILL-IN-KUNDNR FILL-IN-BESTNR FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KTKONTAKT
      FILL-IN-KTTELE FILL-IN-KIMOBIL FILL-IN-KIEPOST FILL-IN-KTMOBIL FILL-IN-AVISPERS FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS
      FILL-IN-STATUS TOG_KAB WITH FRAME {&FRAME-NAME}.
      ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.
      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-LEVNAMN CMB_KONT FILL-IN-LTELE FILL-IN-LADR FILL-IN-LFAX 
          FILL-IN-LPNR FILL-IN-LORT CMB_LEV FILL-IN-STATUS CMB_DEPA 
          FILL-IN-BESTNR FILL-IN-KUNDNR FILL-IN-IB FILL-IN-FOR FILL-IN-FAX 
          FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL FILL-IN-KIEPOST 
          FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL FILL-IN-PLKONTAKT 
          FILL-IN-PLMOBIL FILL-IN-DATUM FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 
          FILL-IN-MARK FILL-IN-KOM TOG_KAB FILL-IN-AVIS FILL-IN-AVISPERS 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE RECT-58 FILL-IN-LEVNAMN CMB_KONT FILL-IN-LTELE BRW_PERS FILL-IN-LADR 
         FILL-IN-LFAX FILL-IN-LPNR FILL-IN-LORT BTN_NY BTN_ANDRA BTN_BORT 
         CMB_LEV FBTN_MAIL CMB_DEPA FBTN_AHLSELL FILL-IN-BESTNR FILL-IN-KUNDNR 
         FBTN_EDI FBTN_DEPA FILL-IN-IB FBTN_VISA FILL-IN-FOR FILL-IN-FAX 
         FBTN_SKRIV FILL-IN-KIKONTAKT FILL-IN-KITELE FBTN_AVSTATUS 
         FILL-IN-KIMOBIL FILL-IN-KIEPOST FBTN_VSTATUS FILL-IN-KTKONTAKT 
         FILL-IN-KTTELE FILL-IN-KTMOBIL FILL-IN-PLKONTAKT FILL-IN-PLMOBIL 
         FILL-IN-DATUM BRW_ANM FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-MARK 
         FILL-IN-KOM TOG_KAB FILL-IN-AVIS FILL-IN-AVISPERS BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   EMPTY TEMP-TABLE lev_temp NO-ERROR. 
   
   RUN kundnrform_UI.
   ASSIGN
   status-ok = CMB_LEV:DELETE("0") IN FRAME {&FRAME-NAME} 
   status-ok = CMB_KONT:DELETE("0")
   mailvar = FALSE
   edivar = FALSE
   forsta = TRUE. 
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:     
      ediben = "EP".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VSYD" THEN DO:
      ediben = "SP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VAST" THEN DO:
      ediben = "VP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VORD" THEN DO:
      ediben = "NP.".
   END.
   ELSE IF Guru.Konstanter:globforetag = "VOST" THEN DO:
      ediben = "OP.".
   END.  
   ELSE IF Guru.Konstanter:globforetag = "BORL" THEN DO:
      ediben = "P".
   END.
   ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      ediben = "P".
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ediben = "P".
   END.   
   /* fler best per dag          */   
   /* VESAB vill att huvudleverantör ska komma först  /Lena 20080908
   FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND 
   best_mtrl.DELNR = klockansek AND best_mtrl.ANTAL > 0 USE-INDEX DATUM NO-LOCK NO-ERROR. 
   IF AVAILABLE best_mtrl THEN DO:*/
   
   /*Låt beredningens leverantör komma först inte huvudleverantör lena20090121*/
   RUN hamtlev_UI IN bertrpapph (INPUT valomrade,INPUT valaonr,OUTPUT valev).
   IF valev = "" THEN DO:
      FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE huvlevtemp THEN DO:
         ASSIGN
         valev = huvlevtemp.LEVKOD.
      END.
   END.

   /*
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:         
      FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND 
      best_mtrl.DELNR = klockansek AND best_mtrl.ANTAL > 0 AND best_mtrl.LEVKOD = huvlevtemp.LEVKOD USE-INDEX DATUM NO-LOCK NO-ERROR. */
   IF valev NE "" THEN DO:   
      FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND best_mtrl.DELNR = klockansek AND 
      best_mtrl.ANTAL > 0 AND best_mtrl.LEVKOD = valev USE-INDEX DATUM NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE best_mtrl THEN DO:         
         FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND 
         best_mtrl.DELNR = klockansek AND best_mtrl.ANTAL > 0 USE-INDEX DATUM NO-LOCK NO-ERROR. 
      END.
   END.
   ELSE DO:      
      FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND 
      best_mtrl.DELNR = klockansek AND best_mtrl.ANTAL > 0 USE-INDEX DATUM NO-LOCK NO-ERROR. 
   END.
   IF AVAILABLE best_mtrl THEN DO:
      vald_lev = best_mtrl.LEVKOD.        
      FIND FIRST levtemp WHERE levtemp.LEVKOD = best_mtrl.LEVKOD NO-LOCK NO-ERROR.      
      CREATE lev_temp.
      lev_temp.LEVKOD = levtemp.LEVKOD.
      ASSIGN 
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}            
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN 
      leve = " ".      
      /* fler best per dag*/
      FOR EACH best_mtrl WHERE best_mtrl.LEVKOD NE vald_lev AND best_mtrl.DATUM = datvar AND 
      best_mtrl.DELNR = klockansek AND best_mtrl.ANTAL > 0 BY best_mtrl.LEVKOD:        
         FIND FIRST levtemp WHERE levtemp.LEVKOD = best_mtrl.LEVKOD NO-LOCK NO-ERROR.         
         IF levtemp.LEVKOD NE leve THEN DO:  
            CREATE lev_temp.
            lev_temp.LEVKOD = levtemp.LEVKOD.
            ASSIGN  
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}               
            leve = best_mtrl.LEVKOD. 
         END.                
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns inget materiel att skapa beställning på." VIEW-AS ALERT-BOX.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.  
   leverant = CMB_LEV:SCREEN-VALUE.                
   RUN kundnrform_UI.
      
   status-ok = CMB_DEPA:DELETE("0"). 
   
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
      CMB_DEPA:list-items = "".
      FOR EACH depatemp BY depatemp.BENAMNING:               
         FIND FIRST kundnrtemp WHERE kundnrtemp.DEP-NR = depatemp.DEP-NR AND kundnrtemp.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
         IF AVAILABLE kundnrtemp THEN DO:
            IF kundnrtemp.KUNDNR NE 0  THEN DO:
               status-ok = CMB_DEPA:ADD-LAST(depatemp.BENAMNING)IN FRAME {&FRAME-NAME}.                     
            END.
         END.
      END.       
   END.
   ELSE DO:   
      FOR EACH depatemp BY depatemp.BENAMNING:      
         ASSIGN
         status-ok = CMB_DEPA:ADD-LAST(depatemp.BENAMNING)IN FRAME {&FRAME-NAME}.                     
      END.       
   END.
   FIND FIRST depatemp NO-LOCK NO-ERROR.
   ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING. 
     
   /*Ladda temptabeller att arbeta med*/
   RUN laddatemptab_UI IN bertrpapph 
      (INPUT kalkrow,
      INPUT vald_lev,
      INPUT datvar,
      INPUT klockansek,
      INPUT Guru.Konstanter:globanv,
      OUTPUT TABLE beredningtemp,
      OUTPUT TABLE levtrptemp,
      OUTPUT TABLE levtrp2temp,
      OUTPUT TABLE anvandartemp,
      OUTPUT TABLE personaltemp, 
      OUTPUT TABLE depsektemp).  
   RUN laddaanm_UI IN bertrpapph (OUTPUT TABLE fastanmtemp).
   
   FIND FIRST beredningtemp NO-LOCK NO-ERROR.
   
   {&WINDOW-NAME}:TITLE = "Leverans och transport för beredning nr:" + STRING(beredningtemp.BERNR) + " benämning:" + beredningtemp.BENAMNING.   
   IF AVAILABLE beredningtemp THEN DO:
      FIND FIRST levtrptemp WHERE levtrptemp.BERNR = beredningtemp.BERNR AND levtrptemp.LEVKOD = vald_lev AND 
      levtrptemp.OMRADE = beredningtemp.OMRADE USE-INDEX BERNR2 NO-LOCK NO-ERROR.           
   END.   
   FOR EACH levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev USE-INDEX LEVKOD NO-LOCK:
      ASSIGN 
      status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
   END.
   
   IF AVAILABLE levtrptemp THEN DO:      
      finns = TRUE.
      FIND FIRST depatemp WHERE depatemp.DEP-NR = levtrptemp.DEP-NR NO-LOCK NO-ERROR.
      IF AVAILABLE depatemp THEN DO:
         ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING.
      END. 
      
      /* fler best per dag*/
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR
      USE-INDEX BERED NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE levtrp2temp THEN DO: 
                      
         RUN bestnr_UI IN bertrpapph (INPUT beredningtemp.OMRADE,INPUT beredningtemp.BERNR, OUTPUT nyttnr).                    
         CREATE levtrp2temp.
         ASSIGN
         levtrp2temp.BERNR = beredningtemp.BERNR
         levtrp2temp.OMRADE = beredningtemp.OMRADE
         levtrp2temp.LEVKOD = vald_lev
         levtrp2temp.DATUM = datvar
         levtrp2temp.BESTNR = nyttnr
         levtrp2temp.BESTALLD = "Ej beställd".
         ASSIGN
         levtrptemp.BESTALLD = "Ej beställd"
         levtrptemp.BESTNR = nyttnr
         levtrptemp.LEVERANS = "".
         ASSIGN
         SUBSTRING(levtrptemp.LEVERANS,1,15) = STRING(INTEGER(datvar))
         SUBSTRING(levtrptemp.LEVERANS,20,15) = STRING(klockansek).          
         {EDIBESTNRL.I}
         FILL-IN-STATUS = levtrp2temp.BESTALLD. 
      END. 
      ELSE DO:
         ASSIGN
         FILL-IN-BESTNR = levtrptemp.BERBESTNR
         FILL-IN-STATUS = levtrptemp.BESTALLD.
      END.
      IF levtrptemp.LEVKONTAKT NE "" THEN DO:
         FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev AND
         levkontakttemp.LEVKONTAKT = levtrptemp.LEVKONTAKT NO-LOCK NO-ERROR.
         IF AVAILABLE levkontakttemp THEN CMB_KONT = levtrptemp.LEVKONTAKT.         
         ELSE DO:
            CREATE levkontakttemp.
            ASSIGN
            levkontakttemp.LEVKOD = vald_lev
            levkontakttemp.LEVKONTAKT = levtrptemp.LEVKONTAKT
            levkontakttemp.LEVTEL = levtrptemp.LEVTEL            
            levkontakttemp.LEVADR = levtrptemp.LEVADR
            levkontakttemp.LEVPNR = levtrptemp.LEVPNR
            levkontakttemp.LEVORT = levtrptemp.LEVORT
            status-ok = CMB_KONT:ADD-LAST(levtrptemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
            CMB_KONT = levtrptemp.LEVKONTAKT.
            EMPTY TEMP-TABLE elevkontakttemp NO-ERROR.
            CREATE elevkontakttemp.
            BUFFER-COPY levkontakttemp TO elevkontakttemp.
            RUN levk_UI IN bertrpapph (INPUT vald_lev,INPUT TABLE elevkontakttemp).
            FIND FIRST elevkontakttemp NO-ERROR.
            IF AVAILABLE elevkontakttemp THEN DELETE elevkontakttemp.
         END.   
      END.
      ELSE DO:
         FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev 
         NO-LOCK NO-ERROR.
         IF AVAILABLE levkontakttemp THEN DO:
            ASSIGN         
            CMB_KONT = levkontakttemp.LEVKONTAKT
            FILL-IN-LTELE = SUBSTRING(levkontakttemp.LEVTEL,1,19)
            FILL-IN-LFAX = SUBSTRING(levkontakttemp.LEVTEL,20)
            FILL-IN-LADR = levkontakttemp.LEVADR
            FILL-IN-LPNR = levkontakttemp.LEVPNR
            FILL-IN-LORT = levkontakttemp.LEVORT.
         END. 
         ELSE DO:
            CREATE levkontakttemp.
            ASSIGN
            levkontakttemp.LEVKOD = vald_lev
            levkontakttemp.LEVTEL = levtemp.LEVTEL
            levkontakttemp.LEVADR = levtemp.LEVADR
            levkontakttemp.LEVPNR = levtemp.LEVPNR
            levkontakttemp.LEVORT = levtemp.LEVORT
            FILL-IN-LTELE = levtemp.LEVTEL
            FILL-IN-LFAX = ""
            FILL-IN-LADR = levtemp.LEVADR
            FILL-IN-LPNR = levtemp.LEVPNR
            FILL-IN-LORT = levtemp.LEVORT.            
            IF levtemp.LEVKONTAKT NE "" THEN DO:            
               levkontakttemp.LEVKONTAKT = levtemp.LEVKONTAKT.                        
            END.
            ELSE DO:
               levkontakttemp.LEVKONTAKT = "Okänd".
            END.
            status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
            CMB_KONT = levkontakttemp.LEVKONTAKT.
            EMPTY TEMP-TABLE elevkontakttemp NO-ERROR.
            CREATE elevkontakttemp.
            BUFFER-COPY levkontakttemp TO elevkontakttemp.
            RUN levk_UI IN bertrpapph (INPUT vald_lev,INPUT TABLE elevkontakttemp).
            FIND FIRST elevkontakttemp NO-ERROR.
            IF AVAILABLE elevkontakttemp THEN DELETE elevkontakttemp.

         END.
      END.
      ASSIGN
      FILL-IN-LEVNAMN = levtrptemp.LEVNAMN      
      FILL-IN-LADR = levtrptemp.LEVADR
      FILL-IN-LPNR = levtrptemp.LEVPNR
      FILL-IN-LORT = levtrptemp.LEVORT
      FILL-IN-LTELE = SUBSTRING(levtrptemp.LEVTEL,1,19)
      FILL-IN-LFAX = SUBSTRING(levtrptemp.LEVTEL,20)
      FILL-IN-KUNDNR = levtrptemp.KUNDNR     
      FILL-IN-FOR = levtrptemp.FORE
      /*FILL-IN-KADR = levtrptemp.KADR
      FILL-IN-KPNR = levtrptemp.KPNR
      FILL-IN-BOX = levtrptemp.BOX
      FILL-IN-KORT = levtrptemp.KORT   */
      FILL-IN-FAX = levtrptemp.FAX                   
      FILL-IN-L1 = levtrptemp.L1
      FILL-IN-L2 = levtrptemp.L2 
      FILL-IN-L3 = levtrptemp.L3 
      FILL-IN-KIKONTAKT  = SUBSTRING(levtrptemp.KIKONTAKT,1,50)
      FILL-IN-KIEPOST = SUBSTRING(levtrptemp.KIKONTAKT,52)
      /*FILL-IN-IB = SUBSTRING(levtrptemp.KIKONTAKT,133,6)*/
      FILL-IN-KITELE = levtrptemp.KITELE        
      FILL-IN-KTKONTAKT = levtrptemp.KTKONTAKT
      /*FILL-IN-PLKONTAKT = SUBSTRING(levtrptemp.KTKONTAKT,52,25)*/
      FILL-IN-KTTELE = levtrptemp.KTTELE     
      FILL-IN-KIMOBIL = levtrptemp.KIMOBIL      
      FILL-IN-KTMOBIL = levtrptemp.KTMOBIL
      /*FILL-IN-PLMOBIL = SUBSTRING(levtrptemp.KTMOBIL,30,15)*/
      FILL-IN-AVISPERS = levtrptemp.AVISPERS 
      FILL-IN-DATUM = levtrptemp.DATUM
      /*CMB_LEVERANS = levtrptemp.LEVERANS*/
      FILL-IN-MARK = levtrptemp.MARK
      FILL-IN-KOM = levtrptemp.KOM
      FILL-IN-AVIS = levtrptemp.AVIS.

      /* CCC IBPROJ ES nytt ib o projektledare + tel
      Ta bort första adressen från depå - skickas aldrig KADR BOX KPNR KORT */
      ASSIGN
      FILL-IN-PLKONTAKT = levtrptemp.KADR
      FILL-IN-IB = levtrptemp.KPNR      
      FILL-IN-PLMOBIL = levtrptemp.KORT.
      IF levtrptemp.KOM BEGINS "OBS! SPECIFIKA KABELLÄNGDER. " THEN DO:
         TOG_KAB = TRUE.
         FILL-IN-KOM = SUBSTRING(levtrptemp.KOM,30).
      END.
      ELSE DO:
         TOG_KAB = FALSE.
      END.
   END.
   ELSE DO TRANSACTION:      
      IF Guru.Konstanter:globforetag = "NIKLAS" OR Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         FIND FIRST depsektemp WHERE depsektemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
         IF AVAILABLE depsektemp THEN DO:
            FIND FIRST depatemp WHERE depatemp.DEP-NR = depsektemp.DEP-NR NO-LOCK NO-ERROR.
            IF AVAILABLE depatemp THEN DO:
               ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING.
            END.
            ELSE DO:
               FIND FIRST depatemp NO-LOCK NO-ERROR.
               IF AVAILABLE depatemp THEN DO:
                  ASSIGN CMB_DEPA:SCREEN-VALUE = depatemp.BENAMNING.
               END.
            END.
         END.
      END.
      finns = FALSE.
      /* fler best per dag*/
      CREATE levtrptemp.
      IF AVAILABLE beredningtemp THEN DO:
         ASSIGN
         levtrptemp.BERNR = beredningtemp.BERNR
         levtrptemp.OMRADE = beredningtemp.OMRADE.
      END.
      ASSIGN 
      levtrptemp.LEVKOD = vald_lev
      levtrptemp.BESTALLD = "Ej beställd"
      levtrptemp.BESTNR = 1
      SUBSTRING(levtrptemp.LEVERANS,1,15) = STRING(INTEGER(datvar))
      SUBSTRING(levtrptemp.LEVERANS,20,15) = STRING(klockansek).
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-LEVNAMN = levtemp.LEVNAMN.
      FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev
      NO-LOCK NO-ERROR.
      IF AVAILABLE levkontakttemp THEN DO:
         ASSIGN
         CMB_KONT = levkontakttemp.LEVKONTAKT
         FILL-IN-LTELE = SUBSTRING(levkontakttemp.LEVTEL,1,19)
         FILL-IN-LFAX = SUBSTRING(levkontakttemp.LEVTEL,20)
         FILL-IN-LADR = levkontakttemp.LEVADR
         FILL-IN-LPNR = levkontakttemp.LEVPNR
         FILL-IN-LORT = levkontakttemp.LEVORT.
      END.
      ELSE DO:
         CREATE levkontakttemp.
         ASSIGN
         levkontakttemp.LEVKOD = vald_lev
         levkontakttemp.LEVTEL = levtemp.LEVTEL
         levkontakttemp.LEVADR = levtemp.LEVADR
         levkontakttemp.LEVPNR = levtemp.LEVPNR
         levkontakttemp.LEVORT = levtemp.LEVORT
         FILL-IN-LTELE = levtemp.LEVTEL
         FILL-IN-LFAX = ""
         FILL-IN-LADR = levtemp.LEVADR
         FILL-IN-LPNR = levtemp.LEVPNR
         FILL-IN-LORT = levtemp.LEVORT.
         IF levtemp.LEVKONTAKT NE "" THEN DO:
            levkontakttemp.LEVKONTAKT = levtemp.LEVKONTAKT.
         END.
         ELSE DO:
            levkontakttemp.LEVKONTAKT = "Okänd".
         END.
         status-ok = CMB_KONT:ADD-LAST(levkontakttemp.LEVKONTAKT)IN FRAME {&FRAME-NAME}.
         CMB_KONT = levkontakttemp.LEVKONTAKT.
         EMPTY TEMP-TABLE elevkontakttemp NO-ERROR.
         CREATE elevkontakttemp.
         BUFFER-COPY levkontakttemp TO elevkontakttemp.
         RUN levk_UI IN bertrpapph (INPUT vald_lev,INPUT TABLE elevkontakttemp).
         FIND FIRST elevkontakttemp NO-ERROR.
         IF AVAILABLE elevkontakttemp THEN DELETE elevkontakttemp.
      END.      
      /* fler best per dag*/
      /*
      FIND FIRST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR AND levtrp2temp.LEVKOD = vald_lev AND
      levtrp2temp.DATUM = datvar AND levtrp2temp.BESTNR = levtrptemp.BESTNR
      USE-INDEX BERED NO-LOCK NO-ERROR.      
      */
      RUN bestnr_UI IN bertrpapph (INPUT beredningtemp.OMRADE,INPUT beredningtemp.BERNR, OUTPUT nyttnr).                    
      /*
      FIND LAST levtrp2temp WHERE levtrp2temp.OMRADE = beredningtemp.OMRADE AND
      levtrp2temp.BERNR = beredningtemp.BERNR USE-INDEX NR NO-LOCK NO-ERROR.
      */
      CREATE levtrp2temp.
      ASSIGN
      levtrp2temp.BERNR = beredningtemp.BERNR
      levtrp2temp.OMRADE = beredningtemp.OMRADE
      levtrp2temp.LEVKOD = vald_lev
      levtrp2temp.DATUM = datvar
      levtrp2temp.BESTNR = nyttnr
      levtrp2temp.BESTALLD = "Ej beställd".
      levtrptemp.BESTNR = nyttnr.
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR  Guru.Konstanter:globforetag = "tect" {GLOBVES.I} THEN DO:
         FILL-IN-MARK = SUBSTRING(beredningtemp.BENAMNING,1,20).
      END.
      {EDIBESTNR.I}
      IF Guru.Konstanter:globforetag = "CSUND"  THEN DO:
         FILL-IN-DATUM = TODAY + 1.   
         IF WEEKDAY(FILL-IN-DATUM) = 1 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
         IF WEEKDAY(FILL-IN-DATUM) = 7 THEN FILL-IN-DATUM = FILL-IN-DATUM + 2.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
         
         FILL-IN-DATUM = ?.
      END.
      ELSE DO:      
         FILL-IN-DATUM = TODAY + 7.      
         IF WEEKDAY(FILL-IN-DATUM) = 1 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
         IF WEEKDAY(FILL-IN-DATUM) = 7 THEN FILL-IN-DATUM = FILL-IN-DATUM + 2.
      END.
      /*
      REPEAT:
         IF WEEKDAY(FILL-IN-DATUM) = 1 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
         IF WEEKDAY(FILL-IN-DATUM) = 7 THEN FILL-IN-DATUM = FILL-IN-DATUM + 2.
         
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = FILL-IN-DATUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE OVERAVTAB THEN DO:
            LEAVE.
         END.      
         ELSE IF OVERAVTAB.EQDAG = 1 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
         ELSE IF OVERAVTAB.EQDAG = 7 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
         ELSE LEAVE.
      END.
      */
      FIND FIRST kundnrtemp WHERE kundnrtemp.DEP-NR = depatemp.DEP-NR AND kundnrtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      IF AVAILABLE kundnrtemp THEN ASSIGN FILL-IN-KUNDNR = kundnrtemp.KUNDNR.
      ELSE ASSIGN FILL-IN-KUNDNR = 0.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:
         ASSIGN
         FILL-IN-KIKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
         FILL-IN-KITELE = personaltemp.TELEFON
         FILL-IN-KIMOBIL = personaltemp.MOBILTEL
         FILL-IN-KIEPOST = SUBSTRING(personaltemp.PERSONSOK,20)
         FILL-IN-AVISPERS = SUBSTRING(personaltemp.FORNAMN,1,1) + "." +
         SUBSTRING(personaltemp.EFTERNAMN,1,8) + personaltemp.MOBILTEL.
      END.
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = beredningtemp.ANVANDARE NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:         
         ASSIGN
         FILL-IN-KTKONTAKT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
         FILL-IN-KTTELE = personaltemp.TELEFON
         FILL-IN-KTMOBIL = personaltemp.MOBILTEL.         
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:      
         plled = "".
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "AOREF"                   
         inextradatatemp.HUVUDCH = beredningtemp.AONR.                    
         inextradatatemp.HUVUDINT = beredningtemp.DELNR.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp ). 
         FIND FIRST extradatatemp NO-ERROR.
         IF AVAILABLE extradatatemp THEN FILL-IN-IB = extradatatemp.SOKCHAR[1].
         RUN hmtplib_UI IN bertrpapph (INPUT beredningtemp.AONR,INPUT beredningtemp.DELNR,OUTPUT plled).
         FILL-IN-PLKONTAKT = plled.
      END.
      TOG_KAB = FALSE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kundnrform_UI WINDOW-2 
PROCEDURE kundnrform_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE kundandra AS LOGICAL NO-UNDO.
   FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999999".      
   DISPLAY FILL-IN-KUNDNR WITH FRAME {&FRAME-NAME}. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      IF leverant = "Onninen" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".      
      ELSE FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "99999".      
      kundandra = TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:      
      IF leverant = "Onninen" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".      
      ELSE IF leverant = "AHLSELL" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".      
      ELSE IF leverant = "NKT" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".      
      ELSE FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = ">>>>99999".      
      kundandra = TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      IF CMB_LEV:SCREEN-VALUE = "Onninen" THEN DO:
         FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".      
         kundandra = TRUE.
      END.
      IF CMB_LEV:SCREEN-VALUE = "AHLSELL" THEN DO:
         FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".      
         kundandra = TRUE.
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".
      kundandra = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "FORS" THEN DO:
      FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".
      kundandra = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      IF CMB_LEV:SCREEN-VALUE = "Onninen" THEN DO:
         FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".      
         kundandra = TRUE.
      END.
   END.   
   IF Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF CMB_LEV:SCREEN-VALUE = "AHLSELL" THEN DO:
         FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".      
         kundandra = TRUE.
      END.
   END.   
   
   IF kundandra = FALSE THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "99999".      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levtrp_UI WINDOW-2 
PROCEDURE levtrp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   levtrptemp.LEVNAMN = FILL-IN-LEVNAMN
   levtrptemp.LEVKONTAKT = CMB_KONT 
   levtrptemp.LEVADR = FILL-IN-LADR 
   levtrptemp.LEVPNR = FILL-IN-LPNR 
   levtrptemp.LEVORT = FILL-IN-LORT 
   SUBSTRING(levtrptemp.LEVTEL,1,19) = FILL-IN-LTELE 
   SUBSTRING(levtrptemp.LEVTEL,20) = FILL-IN-LFAX
   levtrptemp.KUNDNR = FILL-IN-KUNDNR 
   levtrptemp.BERBESTNR = FILL-IN-BESTNR
   levtrptemp.FORE = FILL-IN-FOR       
   /*levtrptemp.KADR = FILL-IN-KADR 
   levtrptemp.KPNR = FILL-IN-KPNR 
   levtrptemp.BOX = FILL-IN-BOX 
   levtrptemp.KORT = FILL-IN-KORT */
   levtrptemp.FAX = FILL-IN-FAX 
   levtrptemp.L1 = FILL-IN-L1
   levtrptemp.L2 = FILL-IN-L2 
   levtrptemp.L3 = FILL-IN-L3 
   levtrptemp.KIKONTAKT = ""
   SUBSTRING(levtrptemp.KIKONTAKT,1,50) = FILL-IN-KIKONTAKT 
   SUBSTRING(levtrptemp.KIKONTAKT,52) = FILL-IN-KIEPOST 
   /*SUBSTRING(levtrptemp.KIKONTAKT,133,6) = FILL-IN-IB */
   levtrptemp.KITELE =  FILL-IN-KITELE 
   levtrptemp.KTKONTAKT = FILL-IN-KTKONTAKT 
   /*SUBSTRING(levtrptemp.KTKONTAKT,52,25) = FILL-IN-PLKONTAKT */
   levtrptemp.KTTELE = FILL-IN-KTTELE 
   levtrptemp.KIMOBIL =  FILL-IN-KIMOBIL
   levtrptemp.KTMOBIL =  FILL-IN-KTMOBIL 
   /*SUBSTRING(levtrptemp.KTMOBIL,30,15) =  FILL-IN-PLMOBIL */
   levtrptemp.AVISPERS = FILL-IN-AVISPERS 
   levtrptemp.DATUM = FILL-IN-DATUM 
   /*levtrptemp.LEVERANS = CMB_LEVERANS*/
   levtrptemp.MARK = FILL-IN-MARK 
   levtrptemp.KOM = FILL-IN-KOM 
   levtrptemp.AVIS = FILL-IN-AVIS.
   /* CCC IBPROJ ES nytt ib o projektledare + tel
   Ta bort första adressen från depå - skickas aldrig KADR BOX KPNR KORT */
   ASSIGN
   levtrptemp.KADR = FILL-IN-PLKONTAKT 
   levtrptemp.KPNR  = FILL-IN-IB 
   levtrptemp.KORT = FILL-IN-PLMOBIL .
   /* AVROP
   IF  Guru.Konstanter:globforetag = "VAST" THEN DO:
      IF vald_lev = "17" THEN DO:
         levtrptemp.KOM = "OBS! AVROP " + levtrptemp.KOM.
      END.
   END.*/

   IF TOG_KAB = TRUE THEN DO:
      levtrptemp.KOM = "OBS! SPECIFIKA KABELLÄNGDER. " + levtrptemp.KOM.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrl_UI WINDOW-2 
PROCEDURE mtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE trp_mtrl NO-ERROR.    
   /* fler best per dag*/
   FOR EACH best_mtrl WHERE best_mtrl.DATUM = datvar AND best_mtrl.DELNR = klockansek:
      CREATE trp_mtrl.
      ASSIGN      
      trp_mtrl.ENR = best_mtrl.ENR
      trp_mtrl.BENAMNING = best_mtrl.BENAMNING
      trp_mtrl.ENHET = best_mtrl.ENHET         
      trp_mtrl.PRIS = best_mtrl.PRIS
      trp_mtrl.OPRIS = best_mtrl.OPRIS
      trp_mtrl.ANTAL = best_mtrl.ANTAL        
      trp_mtrl.LEVKOD = best_mtrl.LEVKOD
      trp_mtrl.BERLEV = best_mtrl.BERLEV
      trp_mtrl.DBEST = best_mtrl.DBEST.
   END.     
   /*FOR EACH trp_mtrl WHERE NO-LOCK:
      MESSAGE "b" trp_mtrl.ENR "ANTAL" trp_mtrl.ANTAL "LEVKOD" trp_mtrl.LEVKOD "BERLEV" trp_mtrl.BERLEV "DBEST" trp_mtrl.DBEST VIEW-AS ALERT-BOX.
   END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onlfillinfor_UI WINDOW-2 
PROCEDURE onlfillinfor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   
   ASSIGN
   FILL-IN-LEVNAMN = INPUT FRAME {&FRAME-NAME} FILL-IN-LEVNAMN
   CMB_KONT = INPUT CMB_KONT 
   CMB_DEPA = INPUT CMB_DEPA 
   FILL-IN-LADR = INPUT FILL-IN-LADR 
   FILL-IN-LPNR = INPUT FILL-IN-LPNR 
   FILL-IN-LORT = INPUT FILL-IN-LORT 
   FILL-IN-LTELE = INPUT FILL-IN-LTELE 
   FILL-IN-LFAX = INPUT FILL-IN-LFAX
   FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR 
   FILL-IN-BESTNR = INPUT FILL-IN-BESTNR
   FILL-IN-FOR = INPUT FILL-IN-FOR 
   /*FILL-IN-KADR = INPUT FILL-IN-KADR 
   FILL-IN-KPNR = INPUT FILL-IN-KPNR 
   FILL-IN-BOX = INPUT FILL-IN-BOX 
   FILL-IN-KORT = INPUT FILL-IN-KORT */
   FILL-IN-FAX = INPUT FILL-IN-FAX 
   FILL-IN-L1 = INPUT FILL-IN-L1
   FILL-IN-L2 = INPUT FILL-IN-L2 
   FILL-IN-L3 = INPUT FILL-IN-L3 
   FILL-IN-KIKONTAKT = INPUT FILL-IN-KIKONTAKT 
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST 
   FILL-IN-KITELE = INPUT FILL-IN-KITELE 
   FILL-IN-KTKONTAKT  = INPUT FILL-IN-KTKONTAKT
   FILL-IN-KTTELE = INPUT FILL-IN-KTTELE 
   FILL-IN-KIMOBIL = INPUT FILL-IN-KIMOBIL
   FILL-IN-KTMOBIL = INPUT FILL-IN-KTMOBIL 
   FILL-IN-AVISPERS = INPUT FILL-IN-AVISPERS 
   FILL-IN-DATUM = INPUT FILL-IN-DATUM 
   /*CMB_LEVERANS = INPUT CMB_LEVERANS*/
   FILL-IN-MARK = INPUT FILL-IN-MARK 
   FILL-IN-KOM = INPUT FILL-IN-KOM 
   FILL-IN-AVIS = INPUT FILL-IN-AVIS
   TOG_KAB = INPUT TOG_KAB
   FILL-IN-IB = INPUT FILL-IN-IB 
   FILL-IN-PLKONTAKT = INPUT FILL-IN-PLKONTAKT 
   FILL-IN-PLMOBIL = INPUT FILL-IN-PLMOBIL. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapamtrl_UI WINDOW-2 
PROCEDURE skapamtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE skapa_mtrl NO-ERROR. 
   CREATE skapa_mtrl.
   ASSIGN
   skapa_mtrl.LEVNAMN = FILL-IN-LEVNAMN
   skapa_mtrl.LKONTAKT = CMB_KONT
   SUBSTRING(skapa_mtrl.LTELE,1,19) = FILL-IN-LTELE
   SUBSTRING(skapa_mtrl.LTELE,20) = FILL-IN-LFAX
   skapa_mtrl.LADR = FILL-IN-LADR
   skapa_mtrl.LPNR = FILL-IN-LPNR
   skapa_mtrl.LORT = FILL-IN-LORT
   skapa_mtrl.FORE = FILL-IN-FOR
   /*skapa_mtrl.KADR = FILL-IN-KADR
   skapa_mtrl.KPNR = FILL-IN-KPNR
   skapa_mtrl.KORT = FILL-IN-KORT
   skapa_mtrl.BOX = FILL-IN-BOX*/
   skapa_mtrl.FAX = FILL-IN-FAX
   skapa_mtrl.KIKONTAKT = FILL-IN-KIKONTAKT                        
   /*SUBSTRING(skapa_mtrl.KIKONTAKT,133,6) = FILL-IN-IB                        */
   skapa_mtrl.KITELE = FILL-IN-KITELE
   skapa_mtrl.KIMOBIL = FILL-IN-KIMOBIL
   skapa_mtrl.KTMOBIL = FILL-IN-KTMOBIL
   /*SUBSTRING(skapa_mtrl.KTMOBIL,30,15) = FILL-IN-PLMOBIL*/
   skapa_mtrl.KTKONTAKT = FILL-IN-KTKONTAKT
   /*SUBSTRING(skapa_mtrl.KTKONTAKT,52,25) = FILL-IN-PLKONTAKT*/
   skapa_mtrl.KTTELE = FILL-IN-KTTELE
   skapa_mtrl.DATUM = FILL-IN-DATUM
   skapa_mtrl.MARK = FILL-IN-MARK 
   skapa_mtrl.L1 = FILL-IN-L1
   skapa_mtrl.L2 = FILL-IN-L2
   skapa_mtrl.L3 = FILL-IN-L3
   skapa_mtrl.KOM = FILL-IN-KOM
   skapa_mtrl.KUNDNR = FILL-IN-KUNDNR 
   skapa_mtrl.AVIS = FILL-IN-AVIS
   /*skapa_mtrl.LEVTID = CMB_LEVERANS  */
   skapa_mtrl.AVISPERS = FILL-IN-AVISPERS
   skapa_mtrl.BESTNR = FILL-IN-BESTNR
   skapa_mtrl.DEPA = CMB_DEPA.    
   /* CCC IBPROJ ES nytt ib o projektledare + tel
      Ta bort första adressen från depå - skickas aldrig KADR BOX KPNR KORT */
   ASSIGN
   skapa_mtrl.KADR = FILL-IN-PLKONTAKT
   skapa_mtrl.KPNR =  FILL-IN-IB 
   skapa_mtrl.KORT = FILL-IN-PLMOBIL.

   IF TOG_KAB = TRUE THEN DO:
      skapa_mtrl.KOM = "OBS! SPECIFIKA KABELLÄNGDER. " + skapa_mtrl.KOM.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tlage_UI WINDOW-2 
PROCEDURE tlage_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   RUN hmtaonrrec_UI IN bertrpapph (INPUT kalkrow,OUTPUT aonrrec).
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GRAN" {GLOBVES.I} THEN DO:
      /*Handfast har lagt upp tidläge materiel med beställt leverans 
      som skall gälla*/
      /*automatisk materiel beställd även för "Onninen avrop" Handfast 20070618 */
      IF leverant = "Onninen avrop" OR leverant = "Elektroskandia avrop" THEN DO:      
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "Materiel Avrop").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "Materiel Avrop").                  
         END.
      END.
      ELSE DO:      
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "Materiel").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "Materiel").                  
         END.
      END.
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MBESTÄLLT").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MBESTÄLLT").                  
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vccmbdepa_UI WINDOW-2 
PROCEDURE vccmbdepa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN                                               
   CMB_DEPA = INPUT FRAME {&FRAME-NAME} CMB_DEPA.                
   FIND FIRST depatemp WHERE depatemp.BENAMNING = CMB_DEPA NO-LOCK NO-ERROR. 
   IF AVAILABLE depatemp THEN DO:
      ASSIGN           
      FILL-IN-FOR = depatemp.FIRMA
      /*FILL-IN-KADR:SCREEN-VALUE = depatemp.ADRESS  
      FILL-IN-KPNR:SCREEN-VALUE = depatemp.PNR
      FILL-IN-KORT:SCREEN-VALUE = depatemp.ORT*/
      FILL-IN-L1:SCREEN-VALUE = depatemp.LEVADRESS  
      FILL-IN-L2:SCREEN-VALUE = depatemp.LEVPNR
      FILL-IN-L3:SCREEN-VALUE = depatemp.LEVORT
      FILL-IN-FAX:SCREEN-VALUE = depatemp.FAXNR.
      FIND FIRST kundnrtemp WHERE kundnrtemp.DEP-NR = depatemp.DEP-NR AND kundnrtemp.LEVKOD = levtemp.LEVKOD NO-LOCK NO-ERROR.
      IF AVAILABLE kundnrtemp THEN ASSIGN FILL-IN-KUNDNR = kundnrtemp.KUNDNR.
      ELSE ASSIGN FILL-IN-KUNDNR = 0.
      DISPLAY FILL-IN-KUNDNR FILL-IN-FOR WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "NIKLAS" OR Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         FIND FIRST depsektemp WHERE depsektemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
         IF AVAILABLE depsektemp THEN DO:
            depsektemp.Dep-Nr = depatemp.DEP-NR.
         END.
         ELSE DO:
            CREATE depsektemp.
            ASSIGN
            depsektemp.ANVANDARE = Guru.Konstanter:globanv
            depsektemp.Dep-Nr = depatemp.DEP-NR.         
         END.
         RUN skapdepsek_UI IN bertrpapph (INPUT depatemp.DEP-NR, INPUT Guru.Konstanter:globanv).
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vccmbkont_UI WINDOW-2 
PROCEDURE vccmbkont_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   CMB_KONT = INPUT FRAME {&FRAME-NAME} CMB_KONT.
   FIND FIRST levkontakttemp WHERE levkontakttemp.LEVKOD = vald_lev AND
   levkontakttemp.LEVKONTAKT = CMB_KONT NO-LOCK NO-ERROR.
   IF AVAILABLE levkontakttemp THEN DO:
      ASSIGN
      FILL-IN-LTELE = SUBSTRING(levkontakttemp.LEVTEL,1,19)
      FILL-IN-LFAX = SUBSTRING(levkontakttemp.LEVTEL,20)
      FILL-IN-LADR = levkontakttemp.LEVADR
      FILL-IN-LPNR = levkontakttemp.LEVPNR
      FILL-IN-LORT = levkontakttemp.LEVORT.
      DISPLAY FILL-IN-LTELE FILL-IN-LFAX FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT
      WITH FRAME {&FRAME-NAME}.
   END.                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

