&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 01/28/97 -  3:43 pm

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
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{DEFSOK.I}
{pdf_StartInc.i}
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
{MARKGRUPP.I}
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
DEFINE NEW SHARED VARIABLE varbild AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE frikortapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valtab AS INTEGER NO-UNDO.
DEFINE VARIABLE gommtrl AS LOGICAL NO-UNDO.

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

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME frame-barabredd
&Scoped-define BROWSE-NAME BRW_KB4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstvaltemp kon_val

/* Definitions for BROWSE BRW_KB4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KB4 konstvaltemp.TRIMKVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KB4 
&Scoped-define QUERY-STRING-BRW_KB4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KB4 OPEN QUERY BRW_KB4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KB4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KB4 konstvaltemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val.ID2 kon_val.EXTRA1 ~
kon_val.F1 kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 kon_val.F6 ~
kon_val.EXTRA2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val


/* Definitions for FRAME frame-barabredd                                */

/* Definitions for FRAME FRAME-KONST                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 BRW_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 124.5 BY .38
     BGCOLOR 1 .

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade väljs".

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .83
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE RAD_KOMP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Skapa nytt skåp", 1,
"Komplettera skåp", 2
     SIZE 40.13 BY 1.67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KB4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KB4 WINDOW-3 _STRUCTURED
  QUERY BRW_KB4 NO-LOCK DISPLAY
      konstvaltemp.TRIMKVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U
            WIDTH 9
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 40 BY 6.29.

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL WINDOW-3 _STRUCTURED
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

DEFINE FRAME frame-barabredd
     BRW_VAL AT ROW 3.83 COL 1.5
     RECT-2 AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 28.25
         BGCOLOR 9 .

DEFINE FRAME FRAME-KONST
     RAD_KOMP AT ROW 1.08 COL 60.75 NO-LABEL
     CMB_VAL AT ROW 1.21 COL 1.5 NO-LABEL
     BTN_NER AT ROW 1.21 COL 42.38
     BTN_UPP AT ROW 1.21 COL 50.75
     BRW_KB4 AT ROW 11 COL 1.5
     FILL-IN-B4 AT ROW 10.17 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 12.88
         SIZE 124.5 BY 16.38
         BGCOLOR 12 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: bildbertemp T "?" NO-UNDO temp-db bildbertemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db kalk_temp
      TABLE: ? T "?" NO-UNDO temp-db list_mtrl
      TABLE: ? T "?" NO-UNDO temp-db kon_val
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredning"
         HEIGHT             = 28.42
         WIDTH              = 124.88
         MAX-HEIGHT         = 32.38
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 32.38
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KONST:FRAME = FRAME frame-barabredd:HANDLE.

/* SETTINGS FOR FRAME frame-barabredd
                                                                        */
/* BROWSE-TAB BRW_VAL RECT-2 frame-barabredd */
ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME frame-barabredd         = 300
       BRW_VAL:COLUMN-RESIZABLE IN FRAME frame-barabredd       = TRUE.

/* SETTINGS FOR FRAME FRAME-KONST
                                                                        */
/* BROWSE-TAB BRW_KB4 BTN_UPP FRAME-KONST */
/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-KONST
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B4 IN FRAME FRAME-KONST
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B4:HIDDEN IN FRAME FRAME-KONST           = TRUE.

ASSIGN 
       RAD_KOMP:HIDDEN IN FRAME FRAME-KONST           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KB4
/* Query rebuild information for BROWSE BRW_KB4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.TRIMKVALKOD
"konstvaltemp.TRIMKVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KB4 */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frame-barabredd
/* Query rebuild information for FRAME frame-barabredd
     _Query            is NOT OPENED
*/  /* FRAME frame-barabredd */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_KB4
&Scoped-define FRAME-NAME FRAME-KONST
&Scoped-define SELF-NAME BRW_KB4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KB4 WINDOW-3
ON MOUSE-MENU-CLICK OF BRW_KB4 IN FRAME FRAME-KONST
DO:
  RUN bildbrwkb4_UI.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER WINDOW-3
ON CHOOSE OF BTN_NER IN FRAME FRAME-KONST /* Ner */
DO:   
   CURRENT-WINDOW:WINDOW-STATE = 2.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-3
ON CHOOSE OF BTN_UPP IN FRAME FRAME-KONST
DO:     
   CURRENT-WINDOW:WINDOW-STATE = 3.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL WINDOW-3
ON VALUE-CHANGED OF CMB_VAL IN FRAME FRAME-KONST
DO:   
   RUN cmbval_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_KOMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KOMP WINDOW-3
ON VALUE-CHANGED OF RAD_KOMP IN FRAME FRAME-KONST
DO:
   RAD_KOMP = INPUT RAD_KOMP.
   RUN rowcolsel_UI IN brwproc[21] (INPUT 12,INPUT RAD_KOMP).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frame-barabredd
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   
   
   RUN disable_UI.
END.                                       
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:  
   &Scoped-define FRAME-NAME frame-barabredd
   RUN slut_UI.
          
          
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   
        
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   DEFINE VARIABLE AA AS INTEGER NO-UNDO.
   /* 
   {WIN_M_START.I}
    
   {muswait.i}
   {ALLSTARTDYN.I}
   */ 
   RUN main_UI.
   /*
   RUN laddatemptab_UI IN bervalapph (OUTPUT TABLE fastanmtemp,
                                      OUTPUT TABLE berfortemp,
                                      OUTPUT TABLE berlinjetemp,
                                      OUTPUT TABLE bernattemp).
                                        */        
   FRAME frame-barabredd:HIDDEN = FALSE.
   FRAME FRAME-konst:HIDDEN = FALSE.
   /*
   {musarrow.i}
   
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
    */
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[21] 
      (INPUT BRW_VAL:HANDLE IN FRAME frame-barabredd). 
   /*RUN setcolindex_UI IN brwproc[22] (INPUT "LEVKOD").*/
   RUN setshiftcol_UI IN brwproc[21] (INPUT FALSE).
   RUN setcolindex_UI IN brwproc[21] (INPUT "ORD"). 
   RUN brwsetupstop_UI IN brwproc[21] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERVALAPPK.P PERSISTENT SET bervalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERVALAPPK.P PERSISTENT SET bervalapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph.
   END. 
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   
   
   
   tthandle = TEMP-TABLE bbenamntemp:HANDLE. 
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   tthandle = TEMP-TABLE konsttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").
   tthandle = TEMP-TABLE levtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE LEVKOD NE '" + STRING(0) + "' AND BORTTAG = FALSE ").
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   tthandle = TEMP-TABLE berstolptemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BERSTOLP", INPUT "").   
   tthandle = TEMP-TABLE parstolptemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PARSTOLP", INPUT "").   
   tthandle = TEMP-TABLE annamntemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "ANNNAMN", INPUT "").   
/*
   RUN setmenulev_UI IN brwproc[15].
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alltbort_UI WINDOW-3 
PROCEDURE alltbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   {muswait.i}
   FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_val.NUM:
      DELETE list_mtrl.      
   END.   
   CREATE list_mtrl.
   ASSIGN
   list_mtrl.NUM = kon_val.NUM
   list_mtrl.ENR = ""
   list_mtrl.BENAMNING = "Inget materiel"
   list_mtrl.ANTAL = 0
   list_mtrl.ENHET = ""
   list_mtrl.PRIS = 0                
   list_mtrl.LINKAB = FALSE.
   RUN refreshbrw_UI IN brwproc[15].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anvbort_UI WINDOW-3 
PROCEDURE anvbort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 57   
   soktemp.SOKINT[1] = 3
   soktemp.SOKINT[2] = Guru.Konstanter:globniv
   soktemp.SOKCHAR[1] = valaonr
   soktemp.SOKCHAR[2] = valomrade
   soktemp.SOKLOG[1] = ejanv.
   {SOKANROP.I}
   ejanv = soktemp.SOKLOG[1].   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assklar_UI WINDOW-3 
PROCEDURE assklar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF Guru.Konstanter:apphandklar:COMPLETE = FALSE THEN DO: 
      MESSAGE "Du måste vänta lite innan du kan köra denna funktion!" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  ENABLE RECT-2 BRW_VAL 
      WITH FRAME frame-barabredd IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-frame-barabredd}
  DISPLAY RAD_KOMP CMB_VAL FILL-IN-B4 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  ENABLE RAD_KOMP CMB_VAL BTN_NER BTN_UPP BRW_KB4 FILL-IN-B4 
      WITH FRAME FRAME-KONST IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main_UI WINDOW-3 
PROCEDURE main_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   window-3:VIRTUAL-HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS.    
    window-3:VIRTUAL-WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS.
   {WIN_M_START.I}   
   {muswait.i}
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   EMPTY TEMP-TABLE satstemp NO-ERROR. 
   EMPTY TEMP-TABLE berstolptemp NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE kon_val NO-ERROR. 
   EMPTY TEMP-TABLE kon_id NO-ERROR. 
   EMPTY TEMP-TABLE kopp_lina NO-ERROR. 
   EMPTY TEMP-TABLE fri_temp NO-ERROR. 
   EMPTY TEMP-TABLE schakt_temp NO-ERROR. 
   EMPTY TEMP-TABLE punkt_temp NO-ERROR. 
   EMPTY TEMP-TABLE kab_temp NO-ERROR. 
   EMPTY TEMP-TABLE skydd_temp NO-ERROR. 
   EMPTY TEMP-TABLE kalk_temp NO-ERROR.     
   EMPTY TEMP-TABLE markgrupp NO-ERROR. 
   {&WINDOW-NAME}:TITLE = "".
  {BERTITLE.I}
   ASSIGN
   status-ok = CMB_VAL:DELETE("0") IN FRAME FRAME-KONST. 
   musz = FALSE.   
   
   FOR EACH konstgrptemp USE-INDEX ORD NO-LOCK:
      IF musz = FALSE THEN DO:
         FIND FIRST konsttemp WHERE konsttemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
         IF AVAILABLE konsttemp THEN DO:       
            ASSIGN
            status-ok = CMB_VAL:ADD-LAST(konstgrptemp.BENAMNING) IN FRAME FRAME-KONST    
            CMB_VAL:SCREEN-VALUE = konstgrptemp.BENAMNING
            musz = TRUE.              
         END.           
      END.
      ELSE DO:
         FIND FIRST konsttemp WHERE konsttemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
         IF AVAILABLE konsttemp THEN DO:         
         END.
      END.                      
   END. 
   IF CMB_VAL:NUM-ITEMS > 15 THEN CMB_VAL:INNER-LINES = 15.
   ELSE CMB_VAL:INNER-LINES = CMB_VAL:NUM-ITEMS.
   
   
   
   
   
   ASSIGN
   musz = FALSE
   avbrytvar = FALSE
   idenable = FALSE
   anmenable = FALSE
   bildenable = FALSE
   mtrlenable = FALSE
   kalkenable = FALSE.             
   IF nyvar = FALSE THEN DO: 
      
   END.    
   ASSIGN    
   counter = 0
   counter2 = 0   
   CMB_VAL = INPUT CMB_VAL.
   
   
   
   IF vald_lev = "" THEN DO:
      xtillyvar = "". 
      FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE huvlevtemp THEN DO:
         vald_kundlev = huvlevtemp.LEVKOD.
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0"
         AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
         vald_kundlev = levtemp.LEVKOD.
      END.
   END.
   ELSE DO:
      ASSIGN
      xtillyvar = vald_lev
      vald_kundlev = vald_lev.
   END.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.
   &Scoped-define FRAME-NAME FRAME-MTRL
   ASSIGN
   
   huvudlev = levtemp.LEVNAMN.
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      /*Niklas personlig spec_mtrl*/
   END.
   FOR EACH levtemp WHERE
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND
   levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
   
   END.
   ASSIGN
   vald_lev = vald_kundlev
   valet = FALSE
   sok = FALSE
   
   satsmtrl = FALSE
   satstrue = FALSE.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
   
         /*
      FILL-IN-SUND:LABEL IN FRAME FRAME-ID = "Littera".    
      */
      
   END. 
   RUN laddadir_UI IN bervalapph (INPUT Guru.Konstanter:globanv, OUTPUT dirnamn).
   RUN enable_UI.
   IF Guru.Konstanter:globanv = "DEMO" OR Guru.Konstanter:globanv = "ELPAO" THEN DO:
      musz = musz.
   END.
   ELSE DO:
      
   END.
   
   &Scoped-define FRAME-NAME frame-barabredd
   RUN DYNBERFRAMESIZEF.P PERSISTENT SET framesizeh
    (INPUT {&WINDOW-NAME}:HANDLE, INPUT TABLE frametemp,BRW_VAL:HANDLE).
   /*
   {FRMSIZEF.I}
    */
   
   
   
   &Scoped-define FRAME-NAME frame-barabredd
   FIND FIRST kon_val WHERE kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:         
      APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   END.  
   ELSE DO:
      {NOLABEL.I}
   END.   
   ASSIGN FRAME FRAME-KONST:HIDDEN = FALSE.
   {musarrow.i}
   
   {WIN_M_SLUT.I}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slut_UI WINDOW-3 
PROCEDURE slut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      RETURN NO-APPLY.
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

