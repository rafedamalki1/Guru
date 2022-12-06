&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
{DIRDEF.I}
DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{EXECLIN.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SUMTD.I} 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.   
DEFINE VARIABLE ingarubriker AS LOGICAL NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE berindvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE arrhjsumtid AS DECIMAL NO-UNDO.  
DEFINE VARIABLE arrhjsumotid AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsumove AS DECIMAL NO-UNDO.
DEFINE VARIABLE arrhjsumtra AS DECIMAL NO-UNDO.   
DEFINE VARIABLE arrhjsumlon AS DECIMAL NO-UNDO.   
DEFINE VARIABLE aoval AS LOGICAL NO-UNDO.
DEFINE VARIABLE utomr2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE utomr3 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE arrhjsumind AS DECIMAL NO-UNDO. 
DEFINE VARIABLE arrhjsumikost AS DECIMAL NO-UNDO.  
DEFINE VARIABLE vardelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirutfapph AS HANDLE NO-UNDO.

DEFINE VARIABLE ktotea AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotber AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotarb AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotmask AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotmtrl AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotovr AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE kbertim AS DECIMAL NO-UNDO.
DEFINE VARIABLE kmsktim AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotutr AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotutrtim AS DECIMAL NO-UNDO.

DEFINE VARIABLE karbtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE karbtim5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ktotalt5 AS DECIMAL NO-UNDO.

DEFINE VARIABLE ototea AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototber AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototarb AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototmask AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototmtrl AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototovr AS DECIMAL NO-UNDO.
DEFINE VARIABLE obertim AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE omsktim AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototutr AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototutrtim AS DECIMAL NO-UNDO.

DEFINE VARIABLE oarbtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE oarbtim5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ototalt5 AS DECIMAL NO-UNDO.

DEFINE VARIABLE planvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE xtypmtrl AS INTEGER NO-UNDO.
DEFINE VARIABLE monpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.

DEFINE VARIABLE totea       AS DECIMAL NO-UNDO.
DEFINE VARIABLE totarb      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmask     AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmasktim  AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmtrl     AS DECIMAL NO-UNDO.
DEFINE VARIABLE totovr      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totutr      AS DECIMAL NO-UNDO.
DEFINE VARIABLE totutrtim   AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbtim      AS DECIMAL NO-UNDO.
DEFINE VARIABLE msktim      AS DECIMAL NO-UNDO.
DEFINE VARIABLE bertim      AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt      AS DECIMAL NO-UNDO. 

DEFINE VARIABLE eavar   AS DECIMAL NO-UNDO.  
DEFINE VARIABLE arbvar  AS DECIMAL NO-UNDO.  
DEFINE VARIABLE mskvar  AS DECIMAL NO-UNDO.  
DEFINE VARIABLE faktvar AS DECIMAL NO-UNDO.  
DEFINE VARIABLE prisvar AS DECIMAL NO-UNDO.  
DEFINE VARIABLE antalk3 AS INTEGER NO-UNDO.
DEFINE VARIABLE ingv AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyrubrik AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kontroll AS LOGICAL NO-UNDO.


DEFINE TEMP-TABLE kalk_temp NO-UNDO
   {KALKTEMPTAB.I}
   

{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL 
   FIELD TOTPRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER      
   INDEX ENR IS PRIMARY ENR ASCENDING. 

DEFINE NEW SHARED TEMP-TABLE kkod
   FIELD KONTO AS CHARACTER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD SATS% AS INTEGER
   INDEX KNR IS PRIMARY KONTONR ASCENDING.

DEFINE NEW SHARED TEMP-TABLE slutsum1           
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER FORMAT "X(5)"
   FIELD GEOMRADE AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD PRIS AS DECIMAL    
   FIELD PRISTYP AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"  
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"         
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"   
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD"        
   FIELD IKOST AS DECIMAL
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING
   INDEX OMR AONR DELNR GEOMRADE OMRADE PERSONALKOD ASCENDING.

DEFINE NEW SHARED TEMP-TABLE slutsum2           
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER FORMAT "X(5)"
   FIELD GEOMRADE AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD PRIS AS DECIMAL    
   FIELD PRISTYP AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"  
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"             
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL" 
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD" 
   FIELD IKOST AS DECIMAL          
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING
   INDEX OMR AONR DELNR GEOMRADE OMRADE PERSONALKOD ASCENDING. 

DEFINE NEW SHARED TEMP-TABLE eko
   FIELD EDEBKRED AS LOGICAL
   FIELD ENY AS LOGICAL      
   FIELD EVERDATUM AS CHARACTER  
   FIELD EPROJEKT AS CHARACTER 
   FIELD DELNR AS INTEGER
   FIELD EORG AS CHARACTER
   FIELD EKOSTNADSSLAG AS CHARACTER    
   FIELD EBELOPP AS DECIMAL 
   FIELD EANTAL  AS DECIMAL              
   FIELD ELONTILLAGG AS CHARACTER 
   FIELD ELONTILLANTAL AS DECIMAL   
   FIELD ELONBELOPP AS DECIMAL         
   INDEX ORG IS PRIMARY EVERDATUM EORG EPROJEKT DELNR EKOSTNADSSLAG ASCENDING.   


DEFINE TEMP-TABLE eko23
   FIELD EDEBKRED AS LOGICAL
   FIELD ENY AS LOGICAL     
   FIELD EVERDATUM AS CHARACTER
   FIELD EPROJEKT AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD EORG AS CHARACTER         
   FIELD EKOSTNADSSLAG AS CHARACTER 
   FIELD EBELOPP AS DECIMAL
   FIELD EANTAL AS DECIMAL           
   FIELD ELONTILLAGG AS CHARACTER
   FIELD ELONTILLANTAL AS DECIMAL   
   FIELD ELONBELOPP AS DECIMAL      
   INDEX PERSORG IS PRIMARY EVERDATUM EPROJEKT DELNR EORG EKOSTNADSSLAG ASCENDING.   

DEFINE NEW SHARED TEMP-TABLE ejlontemp     
   FIELD PERSONALKOD AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD TYPKOD AS CHARACTER
   FIELD DATUM AS DATE
   FIELD PRISTYP AS CHARACTER
   FIELD LONKOST AS DECIMAL
   FIELD GEOMRADE AS CHARACTER 
   FIELD OMRADE AS CHARACTER
   FIELD PRIS AS DECIMAL
   FIELD PRISR AS DECIMAL
   FIELD PRISI AS DECIMAL
   FIELD AKOD AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR DATUM PRISTYP
   INDEX ORG OMRADE
   INDEX TYPKOD TYPKOD.

DEFINE NEW SHARED TEMP-TABLE konttemp
   FIELD DOMRADE AS CHARACTER
   FIELD KOMRADE AS CHARACTER
   FIELD BELOPP AS DECIMAL
   FIELD DKONT AS CHARACTER 
   FIELD KKONT AS CHARACTER       
   INDEX OMRADE IS PRIMARY DOMRADE KOMRADE
   INDEX DKONT DKONT.

DEFINE NEW SHARED TEMP-TABLE uppfoltemp   
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD KONTO AS CHARACTER
   FIELD VALTAR AS INTEGER
   FIELD PERIOD AS INTEGER
   FIELD FAST AS LOGICAL
   FIELD SAMMAN AS LOGICAL
   FIELD LISTA AS INTEGER   
   FIELD INDATUM AS DATE
   FIELD UTDATUM AS DATE.

DEFINE SHARED TEMP-TABLE aoval NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD AONRREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR.   

DEFINE TEMP-TABLE aoval2 LIKE aoval.
   
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD AS CHARACTER 
   FIELD PRIS AS DECIMAL     
   FIELD PRISTYP AS DECIMAL 
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD TIMMAR AS DECIMAL LABEL "TIMMAR"      
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"         
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"    
   FIELD LONKOST AS DECIMAL LABEL "L-KOSTNAD"     
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING. 
DEFINE BUFFER slutsumbuff FOR slutsum. 
{TIDUTTTNEW.I}
{BYTAO.I}

&Scoped-define NEW
{VISUPPTMP.I}          
   
   
DEFINE TEMP-TABLE restid2  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                     
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE. 
                
DEFINE TEMP-TABLE restid3  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                     
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.       
   
DEFINE TEMP-TABLE restid4  
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER                     
   FIELD OMRADE AS CHARACTER
   FIELD TIMMAR AS DECIMAL LABEL "RTIMMAR"                
   INDEX OMR IS PRIMARY OMRADE.
DEFINE TEMP-TABLE aokto
FIELD AONR AS CHARACTER
FIELD DELNR AS INTEGER
FIELD K1 AS CHARACTER
FIELD K2 AS CHARACTER
FIELD K3 AS CHARACTER
FIELD K4 AS CHARACTER
FIELD K5 AS CHARACTER
FIELD SATS% AS INTEGER
FIELD AONRREC AS RECID
INDEX AONR IS PRIMARY AONR DELNR.
DEFINE TEMP-TABLE kto2
FIELD K2 AS CHARACTER.

DEFINE TEMP-TABLE tidutsum NO-UNDO LIKE tidut.
DEFINE TEMP-TABLE dagtempsum NO-UNDO LIKE dagtemp.
DEFINE TEMP-TABLE slutsums1 NO-UNDO LIKE slutsum1.
DEFINE TEMP-TABLE slutsums2 NO-UNDO LIKE slutsum2.
DEFINE TEMP-TABLE restidsum NO-UNDO LIKE restid.
DEFINE TEMP-TABLE restidsum2 NO-UNDO LIKE restid2.
DEFINE TEMP-TABLE restidsum3 NO-UNDO LIKE restid3.
   
DEFINE SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 55.5 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_NAS FBTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Aktivera investering i Excel format".

DEFINE BUTTON FBTN_NAS 
     LABEL "Nästa":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(130)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     FBTN_NAS AT ROW 28.25 COL 66
     FBTN_SKRIV AT ROW 28.25 COL 81
     FBTN_EXCEL AT ROW 28.25 COL 96
     BTN_AVS AT ROW 28.25 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppföljning"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
/* SETTINGS FOR FRAME FRAME-VINST
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-VINST         = 4000.

/* SETTINGS FOR BUTTON FBTN_EXCEL IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_EXCEL:HIDDEN IN FRAME FRAME-VINST           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut.ut
"tidut.ut" ? "X(130)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON RIGHT-MOUSE-CLICK OF BRW_UT IN FRAME FRAME-VINST
DO:
   MESSAGE "Vill du visa listan i din webläsare?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Visa i din webläsare?" UPDATE svar AS LOGICAL.         
   CASE svar:
      WHEN TRUE THEN DO:
         RUN htmlut.p (INPUT vallista,INPUT TABLE tidut).
      END.
      WHEN FALSE THEN DO:
         musz = musz.
      END.
   END CASE.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON ROW-DISPLAY OF BRW_UT IN FRAME FRAME-VINST
DO:
   
   IF uppvaltemp.VALDLISTA BEGINS "Projektuppföljningslista" THEN DO:
      IF SUBSTRING(tidut.UT,27,2) = "UF" THEN
      tidut.UT:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   
   IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 28 THEN DO:
      RUN aopexcel_UI.
   END.
   IF visaupp.UPPFOLJVAL = 34 THEN DO:
      RUN aokomexcel_UI.
   END.      
   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_NAS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NAS WINDOW-2
ON CHOOSE OF FBTN_NAS IN FRAME FRAME-VINST /* Nästa */
DO:
   {muswait.i}   
   FIND FIRST tidut NO-ERROR.
   IF NOT AVAILABLE tidut THEN APPLY "WINDOW-CLOSE":U TO WINDOW-2.   
   GET FIRST BRW_UT NO-LOCK.
   OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   DEBUGGER:SET-BREAK().
   IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 28 THEN DO:      
      RUN SKRIVVAL.W (INPUT FALSE).       
      IF musz = TRUE THEN musz = FALSE. 
      ELSE DO:
         skrivut = FALSE.
         RUN aoskrivut_UI.         
      END.
   END.
   ELSE DO:
      {SKRIVUTFU.I}       
      IF musz = TRUE THEN musz = FALSE. 
      ELSE DO:  
         skrivut = FALSE.  
         RUN ut_UI.            
      END.
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(framesizeh) THEN DELETE PROCEDURE framesizeh.
   IF VALID-HANDLE(dirutfapph) THEN DELETE PROCEDURE dirutfapph.
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
   FBTN_NAS:LABEL = "Nästa " + Guru.Konstanter:gaok.  
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.
   {&WINDOW-NAME}:TITLE = visaupp.UT.
   IF AVAILABLE visaupp THEN DO:
      
      IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 28 OR visaupp.UPPFOLJVAL = 34 THEN DO:
         RUN aopoutf_UI.
      END.       

   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   IF vallista = 1 OR vallista = 4 OR vallista = 5 OR vallista = 51 OR vallista = 28 THEN DO:
      ASSIGN FBTN_EXCEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE FBTN_EXCEL WITH FRAME {&FRAME-NAME}.
   END.    
   FBTN_NAS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE .
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF FBTN_EXCEL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.           
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
       
   END.
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
    

   inder = FALSE.
   ingarubriker = FALSE.
   OPEN QUERY aq FOR EACH valdaao NO-LOCK. 
   GET FIRST aq NO-LOCK.
   GET NEXT aq  NO-LOCK.
   IF NOT AVAILABLE valdaao THEN ingarubriker = TRUE.
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  FBTN_EXCEL.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.

   {musarrow.i}   
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN DIRUTFAPPC.P PERSISTENT SET dirutfapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DIRUTFAPPC.P PERSISTENT SET dirutfapph.
   END.
   RUN anvglob_UI IN dirutfapph (INPUT Guru.Konstanter:globanv).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokomexcel_UI WINDOW-2 
PROCEDURE aokomexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 9
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 18
   utnr[4] = 23
   utnr[5] = 31
   utnr[6] = 40
   utnr[7] = 46
   utnr[8] = 52
   utnr[9] = 58
   utnr[10] = 119.
   
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   FIND FIRST tidut NO-LOCK NO-ERROR.
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   /*Rubriker*/   
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[2],6) = "ENHET/" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.            
      ELSE IF SUBSTRING(tidut.UT,utnr[2],4) = "SIGN" THEN DO:    /*Rubrikslut ANDRA */                  
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
      END.      
      ELSE DO:           
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).                 
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.      
   END.
   RUN slutexcel_UI.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopexcel_UI WINDOW-2 
PROCEDURE aopexcel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*Vilka kolumner*/
   ASSIGN
   slutbredd = 7
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 19
   utnr[4] = 31
   utnr[5] = 38
   utnr[6] = 46
   utnr[7] = 53
   utnr[8] = 61
   utnr[9] = 66
   utnr[10] = 74
   utnr[11] = 82.
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   FIND FIRST tidut NO-LOCK NO-ERROR.
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   /*Rubriker*/   
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[2],5) = "PERS." THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
         rubrikvar = TRUE.
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.      
      ELSE IF SUBSTRING(tidut.UT,utnr[3],9) = "SIGN NAMN" THEN DO:    /*Rubrikslut ANDRA */         
         nyrubrik = FALSE.
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
      END.
      ELSE IF nyrubrik = TRUE THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).        
      END.
      ELSE IF SUBSTRING(tidut.UT,1,8) = "Ingående" THEN DO:    /*Rubrik igen */   
         nyrubrik = TRUE.
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).         
      END.
      ELSE DO:
         IF rubrikvar = TRUE THEN do:
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
         END.
         ELSE DO:  
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.      
   END.
   RUN slutexcel_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopkonthuvud2_UI WINDOW-2 
PROCEDURE aopkonthuvud2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
   IF vallista = 4 THEN DO:        
      RUN aopkonthuvud2_UI IN dirutfapph (INPUT TABLE aoval2,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT kto2.K2, INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
                                     INPUT inder,INPUT Guru.Konstanter:globforetag,
                                     OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   END.
   ELSE IF vallista = 28 THEN DO:   
      RUN aopkonthuvud3_UI IN dirutfapph (INPUT TABLE aoval2,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT kto2.K2, INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
                                     INPUT inder,INPUT Guru.Konstanter:globforetag,
                                     OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   END.
   ELSE IF vallista = 34 THEN DO:   
      RUN aopkonthuvud4_UI IN dirutfapph (INPUT TABLE aoval2,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT kto2.K2, INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT Guru.Konstanter:globforetag,
                                     OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.
      musz = TRUE.
   END.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopkontsumma2_UI WINDOW-2 
PROCEDURE aopkontsumma2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.         
   IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 28 THEN DO:
      RUN aopkontsumma2_UI IN dirutfapph (INPUT TABLE aoval2,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT kto2.K2,INPUT ingv,
                                  INPUT RAD_PERIOD,INPUT vallista,INPUT bdatum,
                                  INPUT avdatum,INPUT valdelnrlog,INPUT inder,
                                  INPUT TABLE visaupp,OUTPUT TABLE tidut).    
   END.
   IF visaupp.UPPFOLJVAL = 34  THEN DO:
      RUN aopkontsumma3_UI IN dirutfapph (INPUT TABLE aoval2,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT kto2.K2,INPUT ingv,
                                  INPUT RAD_PERIOD,INPUT vallista,INPUT bdatum,
                                  INPUT avdatum,
                                  INPUT TABLE visaupp,OUTPUT TABLE tidut).    
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopoutf_UI WINDOW-2 
PROCEDURE aopoutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE konttemp NO-ERROR. 
   EMPTY TEMP-TABLE ejlontemp NO-ERROR. 
   EMPTY TEMP-TABLE eko NO-ERROR. 
   EMPTY TEMP-TABLE eko23 NO-ERROR. 
   EMPTY TEMP-TABLE dagtemp NO-ERROR. 
   EMPTY TEMP-TABLE indertemp NO-ERROR. 
   EMPTY TEMP-TABLE slutsum NO-ERROR. 
   EMPTY TEMP-TABLE slutsum1 NO-ERROR. 
   EMPTY TEMP-TABLE slutsum2 NO-ERROR. 
   EMPTY TEMP-TABLE restid NO-ERROR. 
   EMPTY TEMP-TABLE restid2 NO-ERROR. 
   EMPTY TEMP-TABLE restid3 NO-ERROR. 

   FIND FIRST aoval NO-LOCK NO-ERROR.       
   IF NOT AVAILABLE aoval THEN DO:
      MESSAGE "Det finns ingen på sökbegreppet."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
   aonummer = aoval.AONR
   delnummer = aoval.DELNR. 
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
   RUN aopkont_UI IN dirutfapph (INPUT TABLE aoval,INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),OUTPUT TABLE aokto,OUTPUT TABLE kto2).    
   IF vallista = 4 THEN DO: 
      RUN aopkonthuvudoverst_UI IN dirutfapph (INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT bdatum, INPUT avdatum ,INPUT RAD_PERIOD,OUTPUT TABLE tidut).       
   END.
   IF vallista = 28 OR vallista = 34 THEN DO: 
      RUN aopkonthuvudoverst2_UI IN dirutfapph (INPUT SUBSTRING(uppvaltemp.PROJEKTOR,25,10),INPUT bdatum, INPUT avdatum ,INPUT RAD_PERIOD,INPUT uppvaltemp.VALDLISTA,OUTPUT TABLE tidut).       
   END.
   
   IF vallista = 4 THEN DO:   
      IF RAD_PERIOD = 2 THEN DO:
         MESSAGE "Vill du visa veckonumren för ingående tidsedlar?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
         IF val1 = TRUE THEN ingv = TRUE.
         ELSE ingv = FALSE.           
      END.
   END.
   ELSE ingv = FALSE.   
str=
"=====================================================================================".   
   FOR EACH kto2 BY kto2.K2:      
      EMPTY TEMP-TABLE aoval2 NO-ERROR. 
      FOR EACH aokto WHERE aokto.K2 = kto2.K2: 
         FIND FIRST aoval2 WHERE aoval2.AONR = aokto.AONR AND aoval2.DELNR = aokto.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aoval2 THEN DO:
            CREATE aoval2.
            ASSIGN aoval2.AONR = aokto.AONR
            aoval2.DELNR = aokto.DELNR.            
         END.
      END.      
      REPEAT:
         /*SÖKER FÖRSTA MED TID*/         
         RUN aopkonthuvud2_UI.         
         IF musz = FALSE THEN DO:
            RUN aopkontsumma2_UI.
            LEAVE.       
         END.
         ELSE DO:
            musz = FALSE.
            FIND NEXT aoval NO-LOCK NO-ERROR.  
            IF AVAILABLE aoval THEN DO:     
               ASSIGN
               aonummer = aoval.AONR
               delnummer = aoval.DELNR.
            END.
            ELSE DO:    
               /*INGEN HAR TID*/
               musz = TRUE.
               LEAVE.
            END.
         END.         
      END.       
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").     
      RETURN.
   END.                 
   ELSE DO: 
     ENABLE BRW_UT WITH FRAME FRAME-VINST.
     BRW_UT:HIDDEN = FALSE.       
   END.
   inder = FALSE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoskrivut_UI WINDOW-2 
PROCEDURE aoskrivut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/                         
   FIND LAST tidut NO-LOCK NO-ERROR. 
   IF visaupp.UPPFOLJVAL = 8 OR visaupp.UPPFOLJVAL = 34 THEN RUN EKLOGL.P.
   ELSE RUN EKLOGS.P. 
           
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
  ENABLE FBTN_NAS FBTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/         
   DEFINE VARIABLE startpos AS INTEGER NO-UNDO.
   DEFINE VARIABLE sidnrpos AS INTEGER NO-UNDO.
   {PRINTLIGGANDE.I}
   FIND LAST tidut NO-LOCK NO-ERROR.
   IF visaupp.UPPFOLJVAL = 6 THEN DO:                    
                
      ASSIGN
      startpos = 6
      sidnrpos = 100.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 7 THEN DO:
     
      ASSIGN
      startpos = 6
      sidnrpos = 100.
   END. 
   ELSE IF visaupp.UPPFOLJVAL = 2 THEN DO:
     
      ASSIGN
      startpos = 6
      sidnrpos = 100.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 3 THEN DO:
     
      ASSIGN
      startpos = 1
      sidnrpos = 100.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 31 THEN DO:
      
      ASSIGN
      startpos = 1
      sidnrpos = 100.
   END.
   ELSE DO:
      
      ASSIGN
      startpos = 1
      sidnrpos = 60.
   END.
   PUT SKIP (2).   
   PUT PAGE-NUMBER AT sidnrpos SKIP.              /*RAD 3*/
   FOR EACH tidut:  
      IF SUBSTRING(tidut.UT,1,6) = "BILAGA" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT sidnrpos  SKIP. 
         PUT tidut.UT AT startpos SKIP .       
      END.
      ELSE IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT sidnrpos  SKIP. 
         PUT tidut.UT AT startpos SKIP .      
      END.
      ELSE DO:     
         IF LINE-COUNTER = 1 THEN DO:
            IF ingarubriker = TRUE AND visaupp.UPPFOLJVAL = 2 THEN DO:
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
            END.
            ELSE IF ingarubriker = TRUE AND visaupp.UPPFOLJVAL = 3 THEN DO:
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
            END.
            /* PAGE.*/
            ELSE DO:
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
               PUT UNFORMATTED str2 AT startpos SKIP.
               PUT UNFORMATTED str3 AT startpos SKIP.
               PUT UNFORMATTED str AT startpos SKIP.
            END.            
         END.         
         PUT tidut.UT AT startpos SKIP.                                          
         IF LINE-COUNTER > PAGE-SIZE THEN DO:
            IF ingarubriker = TRUE AND visaupp.UPPFOLJVAL = 2 THEN DO:
               PAGE.
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
            END.
            ELSE IF ingarubriker = TRUE AND visaupp.UPPFOLJVAL = 3 THEN DO:
               PAGE.
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
            END.
            ELSE DO:
               PAGE.
               PUT SKIP (2).
               PUT PAGE-NUMBER AT sidnrpos SKIP.
               PUT UNFORMATTED str2 AT startpos SKIP.
               PUT UNFORMATTED str3 AT startpos SKIP.
               PUT UNFORMATTED str AT startpos SKIP.
            END.                           
         END.                  
      END.                     
   END.  
   OUTPUT CLOSE.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

