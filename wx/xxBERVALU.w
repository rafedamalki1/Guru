&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME wWin




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{DEFSOK.I}
/* {SPECMTRL.I} */
&Scoped-define SHARED SHARED
{BERANN.I}
{MTRLTEMP.I}
{KONVALTEMP.I}
{KONID.I}   
{LISTMTRL.I}   
{KOPPLINA.I}    
{FRITEMP.I}        
{PUNKTTEM.I}  
{SCHAKTTE.I}    
{SKYDDTEM.I}     
{KABTEMP.I}
{KALKTEMP2.I}
{SMTRL.I}
{BILDBERTEMP.I}
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{OMRTEMPW.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SOKDEF.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
{ANMARKTEMP.I}
{BBENAMNTEMP.I}
{KONSTRMTRL.I}
{BERSTOLP.I}
{BERBILD.I}
{ANNAMN.I}
{PARSTOLP.I}
{HOPPSEK2W.I}

{BERSKAP.I}  
&Scoped-define NEW
&Scoped-define SHARED 
{KONVAL2TEMP.I}
DEFINE TEMP-TABLE list_mtrl2 
   FIELD NUM AS INTEGER     
   FIELD KTYPKOD AS CHARACTER
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER
   FIELD LINKAB AS LOGICAL
   FIELD MODUL AS INTEGER 
   FIELD SKAPNUM AS INTEGER
   FIELD TYPBER AS LOGICAL
   FIELD SKAPMTRL AS LOGICAL
   FIELD SKAPMODUL AS INTEGER
   FIELD DIAMETER AS INTEGER
   FIELD MTRLTEXT AS CHARACTER
   FIELD SATS AS LOGICAL
   FIELD PAR AS INTEGER
   FIELD PAR2 AS CHARACTER
   INDEX ENR IS PRIMARY ENR ASCENDING
   INDEX NUM NUM ENR ASCENDING
   INDEX NUM2 NUM SKAPNUM ASCENDING.
DEFINE NEW SHARED VARIABLE kabkortvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE RAD_KOMP AS INTEGER NO-UNDO.
DEFINE VARIABLE RAD_VAL AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kon_rowid AS ROWID NO-UNDO. 
DEFINE NEW SHARED VARIABLE valnum AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valskapnum AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE snabbspar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar3 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar4 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar5 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE bildant AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE skapapar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE berval2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE xtillyvar AS CHARACTER NO-UNDO.


DEFINE SHARED VARIABLE ejanv AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE nyvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skapamtrl AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE VARIABLE komplikoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE globanv2 AS CHARACTER NO-UNDO.        
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE dirnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bild_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE kompmtrl AS LOGICAL NO-UNDO.
DEFINE VARIABLE kompmtrlnum AS INTEGER NO-UNDO.
DEFINE VARIABLE vismtrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE visval AS INTEGER NO-UNDO.
DEFINE VARIABLE entrymtrlantal AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE visamtrl NO-UNDO
   FIELD ENR AS CHARACTER FORMAT "x(11)" LABEL "Enr"    
   FIELD BENAMNING AS CHARACTER FORMAT "x(40)" LABEL "Benämning"   
   FIELD ENHET AS CHARACTER FORMAT "x(5)" LABEL "Enhet" 
   FIELD ANTAL AS INTEGER FORMAT ">>>>>9" LABEL "Antal"
   FIELD LEVKOD AS CHARACTER
   INDEX ENR IS PRIMARY ENR ASCENDING.

/*
DEFINE VARIABLE b2 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE b3 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE b4 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE b5 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE b6 AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns3 AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns4 AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns5 AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns6 AS LOGICAL NO-UNDO.
*/
DEFINE VARIABLE counter AS INTEGER NO-UNDO.        
DEFINE VARIABLE counter2 AS INTEGER NO-UNDO. 
DEFINE VARIABLE counterord AS INTEGER NO-UNDO.
DEFINE VARIABLE numval AS INTEGER NO-UNDO.
DEFINE VARIABLE cmbspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE deletenum AS INTEGER NO-UNDO. 
DEFINE VARIABLE summeter AS INTEGER NO-UNDO. 
DEFINE VARIABLE aonrrow2 AS ROWID NO-UNDO.
DEFINE VARIABLE avbrytvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE newskap AS LOGICAL NO-UNDO.
DEFINE VARIABLE trafovar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokannan AS LOGICAL NO-UNDO.
DEFINE VARIABLE avar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE cvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE valet AS LOGICAL NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE procasynch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE enrvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE charvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE charvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE laddabrwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE setcolvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE spar_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
/*ID*/
DEFINE NEW SHARED VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vallinje AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valnat AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE siff AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE andrakod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE id AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE nyttidnr AS CHARACTER.
DEFINE VARIABLE fordrow AS ROWID NO-UNDO.      
DEFINE VARIABLE linjerow AS ROWID NO-UNDO.
DEFINE VARIABLE natrow AS ROWID NO-UNDO.  
DEFINE VARIABLE brw AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE sparfor AS CHARACTER NO-UNDO.     
DEFINE VARIABLE sparlin AS CHARACTER NO-UNDO. 
DEFINE VARIABLE sparnat AS CHARACTER NO-UNDO. 
DEFINE VARIABLE sparfri1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE sparfri2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sparfri3 AS INTEGER NO-UNDO.

DEFINE VARIABLE idenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE anmenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrlenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE kalkenable AS LOGICAL NO-UNDO.
DEFINE VARIABLE sermtrl AS LOGICAL NO-UNDO.
DEFINE VARIABLE bildenable AS LOGICAL NO-UNDO.

DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE bervalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE rowkonval AS ROWID NO-UNDO.
DEFINE VARIABLE bbnamningvar AS CHARACTER EXTENT 10 NO-UNDO.
/*ID*/

/*MTRL*/
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
/*MTRL*/



/*TABELL FÖR KALKYLERING ENLIGT P3*/
DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.


   
/*TABELLER FÖR ATT KONTROLLERA KABLAR TILL UPPLAG*/   
DEFINE TEMP-TABLE upp_tab 
   FIELD ENR AS CHARACTER         
   FIELD BENAMNING AS CHARACTER
   FIELD METER AS INTEGER      
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER
   FIELD LEVKOD AS CHARACTER       
   INDEX ENR ENR ASCENDING.   
   
DEFINE TEMP-TABLE upp_tab2 
   FIELD ENR AS CHARACTER         
   FIELD BENAMNING AS CHARACTER
   FIELD METER AS INTEGER      
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER   
   FIELD TOTMETER AS INTEGER
   FIELD UPPLAG AS INTEGER    
   FIELD LEVKOD AS CHARACTER   
   INDEX ENR ENR ASCENDING 
   INDEX UPP UPPLAG ASCENDING.     
   
DEFINE TEMP-TABLE upp_tab3 
   FIELD ENR AS CHARACTER         
   FIELD BENAMNING AS CHARACTER
   FIELD METER AS INTEGER      
   FIELD PRIS AS DECIMAL
   FIELD ENHET AS CHARACTER   
   FIELD TOTMETER AS INTEGER
   FIELD UPPLAG AS INTEGER 
   FIELD LEVKOD AS CHARACTER       
   INDEX ENR ENR ASCENDING 
   INDEX UPP UPPLAG ASCENDING.                        

DEFINE NEW SHARED TEMP-TABLE del_val    
   FIELD NUM AS INTEGER   
   FIELD ORT AS CHARACTER.

   
/* DEFINE QUERY berq FOR BERVAL.          */
/* DEFINE QUERY berqmtrl FOR BERMTRL.     */
/* DEFINE QUERY berqid FOR BERID.         */
/* DEFINE QUERY berqlin FOR BERLINKAB.    */
/* DEFINE QUERY friq FOR FRIKORT.         */
/* DEFINE QUERY berqskydd FOR KSKYDD.     */
/* DEFINE QUERY berqpunkt FOR BERPUNKT.   */
/* DEFINE QUERY berqschakt FOR BERSCHAKT. */
/* DEFINE QUERY berqkab FOR SCHAKTKAB.    */
IF Guru.Konstanter:beresekvar[4] = FALSE  THEN RETURN.
DEFINE BUFFER linbuff FOR upp_tab2.
DEFINE BUFFER konbuff FOR kon_val.
DEFINE BUFFER idbuff FOR kon_id.
DEFINE BUFFER listbuff FOR list_mtrl.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BRW_B2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstvaltemp konsttemp kon_val

/* Definitions for BROWSE BRW_B2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B2 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 konstvaltemp


/* Definitions for BROWSE BRW_B3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B3 
&Scoped-define QUERY-STRING-BRW_B3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B3 OPEN QUERY BRW_B3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B3 konstvaltemp


/* Definitions for BROWSE BRW_B4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B4 
&Scoped-define QUERY-STRING-BRW_B4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B4 OPEN QUERY BRW_B4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B4 konstvaltemp


/* Definitions for BROWSE BRW_B5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B5 
&Scoped-define QUERY-STRING-BRW_B5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B5 OPEN QUERY BRW_B5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B5 konstvaltemp


/* Definitions for BROWSE BRW_B6                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B6 
&Scoped-define QUERY-STRING-BRW_B6 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B6 OPEN QUERY BRW_B6 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B6 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B6 konstvaltemp


/* Definitions for BROWSE BRW_KB2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB2 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB2 
&Scoped-define QUERY-STRING-BRW_KB2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB2 OPEN QUERY BRW_KB2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB2 konstvaltemp


/* Definitions for BROWSE BRW_KB3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB3 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB3 
&Scoped-define QUERY-STRING-BRW_KB3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB3 OPEN QUERY BRW_KB3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB3 konstvaltemp


/* Definitions for BROWSE BRW_KB4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB4 
&Scoped-define QUERY-STRING-BRW_KB4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB4 OPEN QUERY BRW_KB4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB4 konstvaltemp


/* Definitions for BROWSE BRW_KB5                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB5 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB5 
&Scoped-define QUERY-STRING-BRW_KB5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB5 OPEN QUERY BRW_KB5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB5 konstvaltemp


/* Definitions for BROWSE BRW_KB6                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB6 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB6 
&Scoped-define QUERY-STRING-BRW_KB6 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB6 OPEN QUERY BRW_KB6 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB6 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB6 konstvaltemp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konsttemp.KTYPKOD ~
konsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konsttemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val.ID2 kon_val.EXTRA1 ~
kon_val.F1 kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 kon_val.F6 ~
kon_val.EXTRA2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val


/* Definitions for FRAME FRAME-KONST                                    */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Arkiv 
       MENU-ITEM m_Snabbspara   LABEL "Snabbspara"    
       MENU-ITEM m_Ok           LABEL "Ok"            
       MENU-ITEM m_Avbryt       LABEL "Avbryt"        .

DEFINE MENU MENU-BAR-wWin MENUBAR
       SUB-MENU  m_Arkiv        LABEL "Arkiv"         
       MENU-ITEM m_Konstruktioner LABEL "Ordning"       
       MENU-ITEM m_LinorKablar  LABEL "Linor/Kablar"  
       MENU-ITEM m_Upplag-kombinationer LABEL "Upplag-kombinationer"
       MENU-ITEM m_Kopiera      LABEL "Kopiera"       
       MENU-ITEM m_Frikort      LABEL "Frikort"       
       MENU-ITEM m_Kabelskp     LABEL "Kabelskåp"     .

DEFINE MENU POPUP-MENU-BRW_B2 
       MENU-ITEM m_mtrlB2       LABEL "Visa materiel" 
       MENU-ITEM m_AvmarkeraB2  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B3 
       MENU-ITEM m_mtrlB3       LABEL "Visa materiel" 
       MENU-ITEM m_AvmarkeraB3  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B4 
       MENU-ITEM m_mtrlB4       LABEL "Visa materiel" 
       MENU-ITEM m_AvmarkeraB4  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B5 
       MENU-ITEM m_mtrlB5       LABEL "Visa materiel" 
       MENU-ITEM m_AvmarkeraB5  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_B6 
       MENU-ITEM m_mtrlB6       LABEL "Visa materiel" 
       MENU-ITEM m_AvmarkeraB6  LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_KB2 
       MENU-ITEM m_mtrlKB2      LABEL "Visa materiel" .

DEFINE MENU POPUP-MENU-BRW_KB3 
       MENU-ITEM m_mtrlKB3      LABEL "Visa materiel" .

DEFINE MENU POPUP-MENU-BRW_KB4 
       MENU-ITEM m_mtrlKB4      LABEL "Visa materiel" .

DEFINE MENU POPUP-MENU-BRW_KB5 
       MENU-ITEM m_mtrlKB5      LABEL "Visa materiel" .

DEFINE MENU POPUP-MENU-BRW_KB6 
       MENU-ITEM m_mtrlKB6      LABEL "Visa materiel" .

DEFINE MENU POPUP-MENU-BRW_KON 
       MENU-ITEM m_Visa_mtrl    LABEL "Visa materiel" 
       MENU-ITEM m_Visa_bild    LABEL "Visa bild"     .

DEFINE MENU POPUP-MENU-BRW_VAL 
       MENU-ITEM m_Visa_informationVAL LABEL "Visa information".


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_SOLEN 
     IMAGE-UP FILE "BILDER\glob":U
     LABEL "" 
     SIZE 4.5 BY 1.71 TOOLTIP "Solen - karta".

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B6 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_B2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B3 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B5 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B6 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB3 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB5 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KB6 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      konsttemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B2 wWin _STRUCTURED
  QUERY BRW_B2 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B3 wWin _STRUCTURED
  QUERY BRW_B3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B4 wWin _STRUCTURED
  QUERY BRW_B4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B5 wWin _STRUCTURED
  QUERY BRW_B5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B6 wWin _STRUCTURED
  QUERY BRW_B6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB2 wWin _STRUCTURED
  QUERY BRW_KB2 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB3 wWin _STRUCTURED
  QUERY BRW_KB3 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB4 wWin _STRUCTURED
  QUERY BRW_KB4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB5 wWin _STRUCTURED
  QUERY BRW_KB5 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB6 wWin _STRUCTURED
  QUERY BRW_KB6 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod"
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON wWin _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod"
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 40 BY 6.29.

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL wWin _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val.ID2 FORMAT "X(256)":U WIDTH 14
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 22
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 12
      kon_val.F2 FORMAT "X(256)":U WIDTH 12
      kon_val.F3 FORMAT "X(256)":U WIDTH 14
      kon_val.F4 FORMAT "X(256)":U WIDTH 12
      kon_val.F5 FORMAT "X(256)":U WIDTH 12
      kon_val.F6 FORMAT "X(256)":U WIDTH 12
      kon_val.EXTRA2 COLUMN-LABEL "+" FORMAT "X(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 118 BY 8.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.

DEFINE FRAME FRAME-KONST
     BRW_VAL AT ROW 1.25 COL 1.5
     FBTN_SOLEN AT ROW 2.54 COL 119.5
     BTN_ORDUPP AT ROW 4.67 COL 119.5
     BTN_ORDNER AT ROW 6.54 COL 119.5
     CMB_VAL AT ROW 11.5 COL 1.5 NO-LABEL
     BTN_NER AT ROW 11.5 COL 42.38
     BTN_UPP AT ROW 11.5 COL 50.75
     BRW_KON AT ROW 14 COL 1.5
     BRW_KB2 AT ROW 14 COL 42.5
     BRW_B2 AT ROW 14 COL 42.63
     BRW_B3 AT ROW 14 COL 82
     BRW_KB3 AT ROW 14 COL 82
     BRW_KB4 AT ROW 21.33 COL 1.5
     BRW_B4 AT ROW 21.33 COL 1.5
     BRW_KB5 AT ROW 21.33 COL 42.5
     BRW_B5 AT ROW 21.33 COL 42.63
     BRW_KB6 AT ROW 21.33 COL 82
     BRW_B6 AT ROW 21.33 COL 82
     FILL-IN-B1 AT ROW 13.17 COL 1.5 NO-LABEL
     FILL-IN-B2 AT ROW 13.17 COL 40.5 COLON-ALIGNED NO-LABEL
     FILL-IN-B3 AT ROW 13.17 COL 80 COLON-ALIGNED NO-LABEL
     FILL-IN-B4 AT ROW 20.5 COL 1.5 NO-LABEL
     FILL-IN-B5 AT ROW 20.5 COL 40.5 COLON-ALIGNED NO-LABEL
     FILL-IN-B6 AT ROW 20.5 COL 80 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.25 ROW 2.38
         SIZE 123.25 BY 26.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Temp-Tables and Buffers:
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: bildbertemp T "?" NO-UNDO temp-db bildbertemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: kalk_temp T "?" NO-UNDO temp-db kalk_temp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: list_mtrl T "?" NO-UNDO temp-db list_mtrl
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 28.42
         WIDTH              = 124.13
         MAX-HEIGHT         = 41.63
         MAX-WIDTH          = 146.25
         VIRTUAL-HEIGHT     = 41.63
         VIRTUAL-WIDTH      = 146.25
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-wWin:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KONST:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
                                                                        */
/* SETTINGS FOR FRAME FRAME-KONST
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_VAL 1 FRAME-KONST */
/* BROWSE-TAB BRW_KON BTN_UPP FRAME-KONST */
/* BROWSE-TAB BRW_KB2 BRW_KON FRAME-KONST */
/* BROWSE-TAB BRW_B2 BRW_KB2 FRAME-KONST */
/* BROWSE-TAB BRW_B3 BRW_B2 FRAME-KONST */
/* BROWSE-TAB BRW_KB3 BRW_B3 FRAME-KONST */
/* BROWSE-TAB BRW_KB4 BRW_KB3 FRAME-KONST */
/* BROWSE-TAB BRW_B4 BRW_KB4 FRAME-KONST */
/* BROWSE-TAB BRW_KB5 BRW_B4 FRAME-KONST */
/* BROWSE-TAB BRW_B5 BRW_KB5 FRAME-KONST */
/* BROWSE-TAB BRW_KB6 BRW_B5 FRAME-KONST */
/* BROWSE-TAB BRW_B6 BRW_KB6 FRAME-KONST */
ASSIGN 
       BRW_B2:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B2:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B2:HANDLE.

ASSIGN 
       BRW_B3:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B3:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B3:HANDLE.

ASSIGN 
       BRW_B4:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B4:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B4:HANDLE.

ASSIGN 
       BRW_B5:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B5:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B5:HANDLE.

ASSIGN 
       BRW_B6:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_B6:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_B6:HANDLE.

ASSIGN 
       BRW_KB2:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB2:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB2:HANDLE.

ASSIGN 
       BRW_KB3:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB3:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB3:HANDLE.

ASSIGN 
       BRW_KB4:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB4:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB4:HANDLE.

ASSIGN 
       BRW_KB5:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB5:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB5:HANDLE.

ASSIGN 
       BRW_KB6:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KB6:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KB6:HANDLE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_KON:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_KON:HANDLE.

ASSIGN 
       BRW_VAL:POPUP-MENU IN FRAME FRAME-KONST             = MENU POPUP-MENU-BRW_VAL:HANDLE
       BRW_VAL:MAX-DATA-GUESS IN FRAME FRAME-KONST         = 300
       BRW_VAL:COLUMN-RESIZABLE IN FRAME FRAME-KONST       = TRUE.

ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME FRAME-KONST           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-KONST
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B1 IN FRAME FRAME-KONST
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B1:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B2:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B3:HIDDEN IN FRAME FRAME-KONST           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B4 IN FRAME FRAME-KONST
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B4:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B5:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       FILL-IN-B6:HIDDEN IN FRAME FRAME-KONST           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B3
/* Query rebuild information for BROWSE BRW_B3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B4
/* Query rebuild information for BROWSE BRW_B4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B5
/* Query rebuild information for BROWSE BRW_B5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B6
/* Query rebuild information for BROWSE BRW_B6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB2
/* Query rebuild information for BROWSE BRW_KB2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB3
/* Query rebuild information for BROWSE BRW_KB3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB4
/* Query rebuild information for BROWSE BRW_KB4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB5
/* Query rebuild information for BROWSE BRW_KB5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB6
/* Query rebuild information for BROWSE BRW_KB6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.kon_val.EXTRA2
"kon_val.EXTRA2" "+" "X(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KON
&Scoped-define FRAME-NAME FRAME-KONST
&Scoped-define SELF-NAME BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KON wWin
ON MOUSE-MENU-CLICK OF BRW_KON IN FRAME FRAME-KONST
DO:
   RUN bild_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KON wWin
ON VALUE-CHANGED OF BRW_KON IN FRAME FRAME-KONST
DO: 
   RUN brwkon_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL wWin
ON MOUSE-MENU-CLICK OF BRW_VAL IN FRAME FRAME-KONST
DO:                                                                
   {muswait.i}
   RUN info_UI.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL wWin
ON VALUE-CHANGED OF BRW_VAL IN FRAME FRAME-KONST
DO:
   {muswait.i}       
   
   RUN vcbrwval_UI.
   IF RAD_VAL = 2 THEN RUN idinfo_UI.
   IF RAD_VAL = 4 THEN DO:
  
      RELEASE list_mtrl NO-ERROR.                                        
   END.
   RUN btnhidden_UI (INPUT "",INPUT 3). 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER wWin
ON CHOOSE OF BTN_NER IN FRAME FRAME-KONST /* Ner */
DO:   
   RUN btnner_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDNER wWin
ON CHOOSE OF BTN_ORDNER IN FRAME FRAME-KONST /* Ner */
DO:   
   RUN btnordner_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDUPP wWin
ON CHOOSE OF BTN_ORDUPP IN FRAME FRAME-KONST
DO:
   RUN btnordupp_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP wWin
ON CHOOSE OF BTN_UPP IN FRAME FRAME-KONST
DO:     
   RUN vadupp_UI.   
   IF RAD_KOMP = 2 THEN DO:
      IF komplikoll = FALSE THEN DO:
         BELL.         
      END.
   END.
   komplikoll = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL wWin
ON VALUE-CHANGED OF CMB_VAL IN FRAME FRAME-KONST
DO:
   CMB_VAL = INPUT CMB_VAL.
   RUN cmbval_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SOLEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SOLEN wWin
ON CHOOSE OF FBTN_SOLEN IN FRAME FRAME-KONST
DO:
   RUN rowleave_UI.
   RUN btnsolen_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraB2 wWin
ON CHOOSE OF MENU-ITEM m_AvmarkeraB2 /* Avmarkera */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  status-ok = BRW_B2:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraB3 wWin
ON CHOOSE OF MENU-ITEM m_AvmarkeraB3 /* Avmarkera */
DO:
   ASSIGN
   status-ok = BRW_B3:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraB4 wWin
ON CHOOSE OF MENU-ITEM m_AvmarkeraB4 /* Avmarkera */
DO:
   ASSIGN
   status-ok = BRW_B4:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraB5 wWin
ON CHOOSE OF MENU-ITEM m_AvmarkeraB5 /* Avmarkera */
DO:
   ASSIGN
   status-ok = BRW_B5:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraB6 wWin
ON CHOOSE OF MENU-ITEM m_AvmarkeraB6 /* Avmarkera */
DO:
   ASSIGN
   status-ok = BRW_B6:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlB2 wWin
ON CHOOSE OF MENU-ITEM m_mtrlB2 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_B2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 2.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlB3 wWin
ON CHOOSE OF MENU-ITEM m_mtrlB3 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_B3:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 3.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlB4 wWin
ON CHOOSE OF MENU-ITEM m_mtrlB4 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_B4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 4.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlB5 wWin
ON CHOOSE OF MENU-ITEM m_mtrlB5 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_B5:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 5.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlB6 wWin
ON CHOOSE OF MENU-ITEM m_mtrlB6 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_B6:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 6.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlKB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlKB2 wWin
ON CHOOSE OF MENU-ITEM m_mtrlKB2 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_KB2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 2.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlKB3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlKB3 wWin
ON CHOOSE OF MENU-ITEM m_mtrlKB3 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_KB3:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 3.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlKB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlKB4 wWin
ON CHOOSE OF MENU-ITEM m_mtrlKB4 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_KB4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 4.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlKB5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlKB5 wWin
ON CHOOSE OF MENU-ITEM m_mtrlKB5 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_KB5:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 5.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_mtrlKB6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_mtrlKB6 wWin
ON CHOOSE OF MENU-ITEM m_mtrlKB6 /* Visa materiel */
DO:  
  &Scoped-define FRAME-NAME FRAME-KONST
  ASSIGN
  status-ok = BRW_KB6:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok = TRUE THEN DO:  
     ASSIGN
     vismtrl = konstvaltemp.KVALKOD
     visval = 6.
     RUN visamtrl_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_bild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_bild wWin
ON CHOOSE OF MENU-ITEM m_Visa_bild /* Visa bild */
DO:
   RUN bild_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_informationVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_informationVAL wWin
ON CHOOSE OF MENU-ITEM m_Visa_informationVAL /* Visa information */
DO:
  {muswait.i}
  RUN info_UI.      
  {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_mtrl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_mtrl wWin
ON CHOOSE OF MENU-ITEM m_Visa_mtrl /* Visa materiel */
DO:   
   visval = 1.
   RUN visamtrl_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME POPUP-MENU-BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL POPUP-MENU-BRW_B3 wWin
ON MENU-DROP OF MENU POPUP-MENU-BRW_B3
DO:
   ASSIGN
   status-ok = BRW_B3:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME POPUP-MENU-BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL POPUP-MENU-BRW_B4 wWin
ON MENU-DROP OF MENU POPUP-MENU-BRW_B4
DO:
   ASSIGN
   status-ok = BRW_B4:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BRW_B2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Konstruktioner|Identitet|Anmärkning|Materiel|Bilder/Dokument|Kalkyl|Listor' + 'FolderTabWidth0FolderFont4HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 28.25 , 124.50 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             FRAME FRAME-KONST:HANDLE , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  VIEW FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY CMB_VAL FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 
          FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW wWin.
  ENABLE BRW_VAL FBTN_SOLEN BTN_ORDUPP BTN_ORDNER CMB_VAL BTN_NER BTN_UPP 
         BRW_KON BRW_KB2 BRW_B2 BRW_B3 BRW_KB3 BRW_KB4 BRW_B4 BRW_KB5 BRW_B5 
         BRW_KB6 BRW_B6 FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 
         FILL-IN-B6 
      WITH FRAME FRAME-KONST IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONST}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

