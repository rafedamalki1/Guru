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
{BEFTEMP.I}
DEFINE {&NEW} SHARED TEMP-TABLE valbefattningstemp NO-UNDO LIKE befattningstemp.
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
DEFINE VARIABLE region AS LOGICAL NO-UNDO.

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
DEFINE NEW SHARED VARIABLE kontroll AS LOGICAL NO-UNDO.


DEFINE TEMP-TABLE kalk_temp NO-UNDO
   {KALKTEMPTAB.I}
   

{LISTDEF.I} 
DEFINE NEW SHARED TEMP-TABLE mtrl_temp2   
   {MTRLTEMP2TT.I}

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

   
DEFINE SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 55.5 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.
&Scoped-define NEW
{VISUPPTMP.I}          
   
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 124 BY 26.3.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     FBTN_NAS AT ROW 28.25 COL 66.5
     FBTN_SKRIV AT ROW 28.25 COL 81.5
     FBTN_EXCEL AT ROW 28.25 COL 96.5
     BTN_AVS AT ROW 28.25 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.


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
                                                                        */
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
"tidut.ut" ? "X(130)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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
   IF visaupp.UPPFOLJVAL = 1 THEN DO:
      RUN aoutexcel_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 54 THEN DO:
      RUN aopexcel_UI.
   END.   
   ELSE IF visaupp.UPPFOLJVAL = 5 THEN DO:
      RUN aodexcel_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 25 THEN DO:
      RUN k3excel_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 26 THEN DO:
      RUN teexcel_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 51 THEN DO:
      RUN aodlexcel_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 52 THEN DO:      
      RUN aodlexcelk_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 41 THEN DO:
      RUN befexcel_UI.
   END.
   ELSE DO:
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).        
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
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF visaupp.UPPFOLJVAL = 1 THEN DO:
      RUN aoutfnas_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 54 THEN DO:
      RUN aopnas_UI.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 5 OR visaupp.UPPFOLJVAL = 51 OR visaupp.UPPFOLJVAL = 52 THEN DO:
      RUN aodnasta_UI.
   END.
   ELSE DO:     
      NASTA:
      REPEAT:
         IF NOT AVAILABLE aoval THEN DO:       
            APPLY "WINDOW-CLOSE":U TO WINDOW-2.
         END.
         ELSE DO:                             
            FIND NEXT aoval USE-INDEX AONR NO-LOCK NO-ERROR.      
            IF NOT AVAILABLE aoval THEN DO:                              
               status-mus2 = SESSION:SET-WAIT-STATE("").
               LEAVE.
            END.
            ELSE DO:
               ASSIGN
               aonummer = aoval.AONR
               delnummer = aoval.DELNR.            
               EMPTY TEMP-TABLE tidut NO-ERROR. 
               RUN nasta_UI.                             
               FIND LAST tidut NO-LOCK NO-ERROR.
               IF AVAILABLE tidut THEN DO:            
                  IF skrivut = FALSE THEN DO:
                     ENABLE BRW_UT WITH FRAME FRAME-VINST.
                     BRW_UT:HIDDEN = FALSE.       
                  END.
                  RUN enable_UI.   
               END.
               ELSE DO:
                  IF Guru.Konstanter:appcon THEN DO:                           
                     RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
                  END.
                  ELSE DO:
                     RUN FELTEXTW.P  
                     (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
                  END. 
                  musz = FALSE.
                  IF NOT AVAILABLE aoval THEN DO:
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     LEAVE.
                  END.
               END.      
            END.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
         END.
         LEAVE NASTA.
      END.
   END.
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
   IF visaupp.UPPFOLJVAL = 1 OR visaupp.UPPFOLJVAL = 4 OR visaupp.UPPFOLJVAL = 5 OR 
   vallista = 8 OR visaupp.UPPFOLJVAL = 51 OR visaupp.UPPFOLJVAL = 52 OR visaupp.UPPFOLJVAL = 54 THEN DO:
      IF vallista = 8 OR visaupp.UPPFOLJVAL = 51 OR visaupp.UPPFOLJVAL = 52 THEN RUN SKRIVVAL.W (INPUT TRUE).    
      ELSE RUN SKRIVVAL.W (INPUT FALSE).       
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
      IF visaupp.UPPFOLJVAL = 1 THEN DO:
         RUN aoutf_UI.
      END.
      ELSE IF visaupp.UPPFOLJVAL = 4  OR visaupp.UPPFOLJVAL = 54 THEN DO:
         RUN aopoutf_UI.
      END. 
      ELSE IF visaupp.UPPFOLJVAL = 5 OR visaupp.UPPFOLJVAL = 51 OR visaupp.UPPFOLJVAL = 52  THEN DO:
         RUN aodtiutf_UI.
      END.
      ELSE IF visaupp.UPPFOLJVAL = 8 THEN DO:
         RUN akalkutf_UI.
      END.
      ELSE IF visaupp.UPPFOLJVAL = 2 OR visaupp.UPPFOLJVAL = 3 OR visaupp.UPPFOLJVAL = 31 OR visaupp.UPPFOLJVAL = 6 OR
      visaupp.UPPFOLJVAL = 7 OR visaupp.UPPFOLJVAL = 21  OR visaupp.UPPFOLJVAL = 60 THEN DO:
         RUN dirutf_UI.         
      END.
      ELSE IF visaupp.UPPFOLJVAL = 26 THEN DO:
         RUN transeko_UI.
      END.
      ELSE IF visaupp.UPPFOLJVAL = 330 THEN DO:
         EMPTY TEMP-TABLE tidut NO-ERROR. 
         FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.        
         excellista = 31.
         IF Guru.Konstanter:appcon THEN DO:
            RUN HAMTAOTIDL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
            OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
         END.
         ELSE DO:         
            RUN HAMTAOTIDL.P 
            (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
            OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).         
         END.
         IF excellista > 0 THEN ENABLE FBTN_EXCEL WITH FRAME {&FRAME-NAME}.
         musz = FALSE. 
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      IF visaupp.UPPFOLJVAL = 41 THEN DO:
         IF Guru.Konstanter:appcon THEN DO:
            RUN BEFAOBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE uppvaltemp, INPUT TABLE valdaao,INPUT TABLE valbefattningstemp, 
            OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
         END.
         ELSE DO:         
            RUN BEFAOBAPP.P 
            (INPUT TABLE uppvaltemp, INPUT TABLE valdaao,INPUT TABLE valbefattningstemp, 
            OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).         
         END.   
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.           
      END.
      IF visaupp.UPPFOLJVAL = 25 THEN DO:
         FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
         excellista = 25.     
         IF Guru.Konstanter:appcon THEN DO:
            RUN AOK2APP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TABLE uppvaltemp, INPUT TABLE valdaao,OUTPUT antalk3,
            OUTPUT TABLE tidut).
         END.
         ELSE DO:    
            RUN AOK2APP.P 
            (INPUT TABLE uppvaltemp, INPUT TABLE valdaao,OUTPUT antalk3, 
            OUTPUT TABLE tidut).
         END.       
         RUN k3excel_UI.
         APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I} 
   IF vallista = 2 OR vallista = 3 OR vallista = 6 OR vallista = 7 OR vallista = 8 OR 
   vallista = 25 OR vallista = 26 OR 
   vallista = 31 OR vallista = 41 OR vallista = 60 OR vallista = 330 THEN DO:
      ASSIGN FBTN_NAS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF vallista = 1 OR vallista = 4 OR vallista = 5 OR vallista = 51 OR vallista = 52 OR vallista = 41 OR visaupp.UPPFOLJVAL = 54 THEN DO:
      ASSIGN FBTN_EXCEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE FBTN_EXCEL WITH FRAME {&FRAME-NAME}.
   END. 
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF FBTN_EXCEL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.           
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   END.
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   Guru.GlobalaVariabler:colrighth = FBTN_NAS:HANDLE.           
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE akalkhuvud_UI WINDOW-2 
PROCEDURE akalkhuvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/                                
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                           
         CREATE tidut.         
         SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaol + "-kalkyl kontofördelning".               
         ASSIGN         
         SUBSTRING(tidut.UT,50) = STRING(TODAY)
         SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS").             
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "==============================================================================================================================".         
      END.   
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE akalkutf_UI WINDOW-2 
PROCEDURE akalkutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
      offert = TRUE.
   END.
   ELSE DO:
      offert = FALSE.
   END.   
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   RUN akalkhuvud_UI. 
   RUN akalklista_UI IN dirutfapph (INPUT offert,
                                    OUTPUT musz,INPUT TABLE aoval,
                                    INPUT-OUTPUT TABLE tidut).

   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      RETURN.
   END.                 
   ELSE DO:
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.       
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodexcel_UI WINDOW-2 
PROCEDURE aodexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "K"
   slutbredd = 6
   utnr[1] = 1 
   utnr[2] = 15
   utnr[3] = 26
   utnr[4] = 35
   utnr[5] = 41
   utnr[6] = 47
   utnr[7] = 54
   utnr[8] = 65
   utnr[9] = 71
   utnr[10] = 81
   utnr[11] = 99
   utnr[12] = 110
   allachar[3] = TRUE
   bladvar = 0
   iRad = 1.
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
      IF SUBSTRING(tidut.UT,utnr[3],5) = "DATUM" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
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
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
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
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   RUN slutexcel_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodhuvud_UI WINDOW-2 
PROCEDURE aodhuvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   IF visaupp.UPPFOLJVAL = 5 /*OR visaupp.UPPFOLJVAL = 60*/ THEN DO:
      RUN aodhuvud_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
                                     OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   END.
   IF visaupp.UPPFOLJVAL = 51 THEN DO:
      RUN aodlhuvud_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
                                     OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   END.
   IF visaupp.UPPFOLJVAL = 52 THEN DO:
      RUN aodkohuvud_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,INPUT bdatum,
                                      INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodlexcel_UI WINDOW-2 
PROCEDURE aodlexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "D"
   slutbredd = 180
   utnr[1] = 1 
   utnr[2] = 7
   utnr[3] = 16
   utnr[4] = 22
   allachar[2] = TRUE
   bladvar = 0
   iRad = 1.
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
      IF SUBSTRING(tidut.UT,utnr[2],5) = "DATUM" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
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
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
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
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   RUN slutexcel_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodlexcelk_UI WINDOW-2 
PROCEDURE aodlexcelk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "E"
   slutbredd = 180
   utnr[1] = 1 
   utnr[2] = 7 
   utnr[3] = 18
   utnr[4] = 27
   utnr[5] = 33
   allachar[3] = TRUE
   bladvar = 0
   iRad = 1.
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
      IF SUBSTRING(tidut.UT,utnr[3],5) = "DATUM" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
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
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
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
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   RUN slutexcel_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodnasta_UI WINDOW-2 
PROCEDURE aodnasta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   NASTA:
   REPEAT:
      IF NOT AVAILABLE aoval THEN DO:      
         LEAVE. 
      END.
      ELSE DO:                             
         FIND NEXT aoval USE-INDEX AONR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE aoval THEN DO:                           
            status-mus2 = SESSION:SET-WAIT-STATE("").
            LEAVE.
         END.
         ELSE DO:
            EMPTY TEMP-TABLE tidut NO-ERROR. 
            ASSIGN
            aonummer = aoval.AONR
            delnummer = aoval.DELNR.
            RUN aodhuvud_UI.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               NEXT NASTA.
            END.
            RUN aodsumma_UI.            
            IF skrivut = FALSE THEN DO:
               ENABLE BRW_UT WITH FRAME FRAME-VINST.
               BRW_UT:HIDDEN = FALSE.       
            END.
            RUN enable_UI.   
         END.          
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      LEAVE NASTA.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodsumma_UI WINDOW-2 
PROCEDURE aodsumma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF visaupp.UPPFOLJVAL = 5  THEN DO:
      RUN aodsumma_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,
                                  INPUT RAD_PERIOD,INPUT inder,
                                  INPUT bdatum,INPUT avdatum,
                                  OUTPUT TABLE tidut).
   END.
   IF visaupp.UPPFOLJVAL = 51 THEN DO:
      RUN aodlsumma_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,
                                  INPUT RAD_PERIOD,INPUT inder,
                                  INPUT bdatum,INPUT avdatum,
                                  OUTPUT TABLE tidut).
   END.
   IF visaupp.UPPFOLJVAL = 52 THEN DO:
      RUN aodkosumma_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,
                                  INPUT RAD_PERIOD,INPUT inder,
                                  INPUT bdatum,INPUT avdatum,
                                  OUTPUT TABLE tidut).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aodtiutf_UI WINDOW-2 
PROCEDURE aodtiutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST aoval NO-LOCK NO-ERROR.       
   ASSIGN
   aonummer = aoval.AONR
   delnummer = aoval.DELNR.    
   REPEAT:
      /*SÖKER FÖRSTA MED TID*/
      RUN aodhuvud_UI.             
      IF musz = FALSE THEN DO:
         RUN aodsumma_UI.   
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
   IF musz = TRUE THEN DO:
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.                 
   ELSE DO: 
      ENABLE BRW_UT WITH FRAME FRAME-VINST.
      BRW_UT:HIDDEN = FALSE.       
   END.
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
   utnr[11] = 82
   bladvar = 0
   iRad = 1.
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
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
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
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   RUN slutexcel_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aophuvud_UI WINDOW-2 
PROCEDURE aophuvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   RUN aophuvud_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,INPUT bdatum,
                                   INPUT avdatum,INPUT RAD_PERIOD,INPUT valdelnrlog,
                                  INPUT inder,INPUT Guru.Konstanter:globforetag,
                                  OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.
      musz = TRUE.
   END.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopnas_UI WINDOW-2 
PROCEDURE aopnas_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   NASTA:
   REPEAT:     
      IF NOT AVAILABLE aoval THEN DO:      
         LEAVE. 
      END.
      ELSE DO:                             
         FIND NEXT aoval USE-INDEX AONR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aoval THEN DO:                           
            status-mus2 = SESSION:SET-WAIT-STATE("").
            LEAVE. 
         END.
         ELSE DO:
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
            ASSIGN
            aonummer = aoval.AONR
            delnummer = aoval.DELNR.
            RUN aophuvud_UI.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               NEXT NASTA.
            END.
            RUN aopsumma_UI. 
            IF skrivut = FALSE THEN DO:
               ENABLE BRW_UT WITH FRAME FRAME-VINST.
               BRW_UT:HIDDEN = FALSE.       
            END.
            RUN enable_UI.   
         END.          
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      LEAVE NASTA.
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
str=
"=====================================================================================".
   REPEAT:
      /*SÖKER FÖRSTA MED TID*/
      RUN aophuvud_UI.
      IF musz = FALSE THEN DO:
         RUN aopsumma_UI.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopsumma_UI WINDOW-2 
PROCEDURE aopsumma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   IF visaupp.UPPFOLJVAL = 54 THEN.
   ELSE DO:
      IF RAD_PERIOD = 2 THEN DO:
         MESSAGE "Vill du visa veckonumren för ingående tidsedlar?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
      END.
    END.
    IF visaupp.UPPFOLJVAL = 54 THEN val1 = FALSE.   
   RUN aopsummau_UI IN dirutfapph (INPUT aonummer,INPUT delnummer,INPUT val1,
                               INPUT RAD_PERIOD,INPUT vallista,INPUT bdatum,
                               INPUT avdatum,INPUT valdelnrlog,INPUT inder,
                               INPUT TABLE visaupp,INPUT TABLE uppvaltemp,OUTPUT TABLE tidut).    
   
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
   IF visaupp.UPPFOLJVAL = 8 THEN RUN EKLOGL.P.
   ELSE RUN EKLOGS.P. 
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoutexcel_UI WINDOW-2 
PROCEDURE aoutexcel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "I"
   slutbredd = 10
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 21
   utnr[4] = 31
   utnr[5] = 40
   utnr[6] = 50
   utnr[7] = 59
   utnr[8] = 69
   utnr[9] = 79
   bladvar = 0
   iRad = 1.
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
      IF SUBSTRING(tidut.UT,utnr[2],4) = "ARB." THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 3). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
         rubrikvar = TRUE.
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,utnr[2],6) = "TIMMAR" THEN DO:    /*Rubrikslut ANDRA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
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
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   
   RUN slutexcel_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoutfnas_UI WINDOW-2 
PROCEDURE aoutfnas_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  NASTA:
   REPEAT:
      IF NOT AVAILABLE aoval THEN DO:      
         LEAVE.
      END.
      ELSE DO:                             
         FIND NEXT aoval USE-INDEX AONR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE aoval THEN DO:                           
            status-mus2 = SESSION:SET-WAIT-STATE("").
            LEAVE.
         END.
         ELSE DO:
            ASSIGN
            aonummer = aoval.AONR
            delnummer = aoval.DELNR.            
            EMPTY TEMP-TABLE tidut NO-ERROR. 
            
            RUN aoutnasta_UI.                             
            FIND LAST tidut NO-LOCK NO-ERROR.
            IF AVAILABLE tidut THEN DO:            
               IF skrivut = FALSE THEN DO:
                  ENABLE BRW_UT WITH FRAME FRAME-VINST.
                  BRW_UT:HIDDEN = FALSE.                        
               END.
               RUN enable_UI.   
            END.
            ELSE DO:
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
               END.
               ELSE DO:
                  RUN FELTEXTW.P  
                  (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
               END. 
               musz = FALSE.
               IF NOT AVAILABLE aoval THEN DO:
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  LEAVE.
               END.
            END.      
         END.          
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      LEAVE NASTA.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoutf_UI WINDOW-2 
PROCEDURE aoutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST aoval NO-LOCK NO-ERROR.  
   IF AVAILABLE aoval THEN DO:
      ASSIGN
      aonummer = aoval.AONR
      delnummer = aoval.DELNR.  
   END.
   RUN aoutnasta_UI.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
      END.
      ELSE DO:
         RUN FELTEXTW.P  
         (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
      END. 
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.   
   ENABLE BRW_UT WITH FRAME FRAME-VINST.  
   BRW_UT:HIDDEN = FALSE.                 
   musz = FALSE.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoutnasta_UI WINDOW-2 
PROCEDURE aoutnasta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   REPEAT:   
      EMPTY TEMP-TABLE uppfoltemp  NO-ERROR. 
      CREATE uppfoltemp.
      ASSIGN
      uppfoltemp.ANVANDARE = Guru.Konstanter:globanv
      uppfoltemp.FORETAG = Guru.Konstanter:globforetag  
      uppfoltemp.PERIOD = RAD_PERIOD      
      uppfoltemp.AONR = aonummer
      uppfoltemp.DELNR = delnummer   
      uppfoltemp.INDATUM = bdatum
      uppfoltemp.UTDATUM = avdatum.  
      {AOUTFIN.I}      
      FIND LAST tidut NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE tidut THEN DO :
         FIND NEXT aoval NO-LOCK NO-ERROR.  
         IF AVAILABLE aoval THEN DO:     
            ASSIGN
            aonummer = aoval.AONR
            delnummer = aoval.DELNR.
         END.
         ELSE DO:                
            musz = TRUE.
            LEAVE.
         END. 
      END.
      ELSE DO:               
         LEAVE.
      END.
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE befexcel_UI WINDOW-2 
PROCEDURE befexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   ASSIGN
   slutbredd = 10
   bredd[1] = 14
   bredd[2] = 20
   bredd[3] = 10
   bredd[4] = 10
   bredd[5] = 10   
   bladvar = 0
   iRad = 1.
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                                    
      IF SUBSTRING(tidut.UT,estartnr[3],9) = "ORDINARIE" THEN DO:
         LEAVE.
      END.
      
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,4) = "====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.   
      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dirutf_UI WINDOW-2 
PROCEDURE dirutf_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   IF visaupp.UPPFOLJVAL = 6 THEN DO:                    
      /*U60 Direktörsrapport*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
      excellista = 1.
      
      IF Guru.Konstanter:appcon THEN DO:
         RUN DIRAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:    
         RUN DIRAPP.P 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.                
   END.
   ELSE IF visaupp.UPPFOLJVAL = 7 THEN DO:                    
      /*U70-Aktivering av investering*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
      excellista = 3.
      IF Guru.Konstanter:appcon THEN DO:
         RUN AKINAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:         
         RUN AKINAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
         
      END.
      
   END.
   ELSE IF visaupp.UPPFOLJVAL = 2 THEN DO:
      /*U20-gaol + kostnadsreg.-kalkyl*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.   
      excellista = 4.
      IF Guru.Konstanter:appcon THEN DO:
         RUN AOKAAPPC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:         
         RUN AOKAAPPC.P 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).         
      END.
     
   END.
   ELSE IF visaupp.UPPFOLJVAL = 3 THEN DO:                         
      /*"U40- Projektuppföljningslista".*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.        
      excellista = 5.      
      MESSAGE "Vill du se verifikaten från kostnadsregistreringen?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Kostnadsregistreringar?" UPDATE svar2 AS LOGICAL.         
      CASE svar2:
         WHEN TRUE THEN DO:
            uppvaltemp.MANUPPDEL = TRUE.
         END.
         WHEN FALSE THEN DO:
            uppvaltemp.MANUPPDEL = FALSE.
         END.
      END CASE.         
      IF Guru.Konstanter:appcon THEN DO:
         RUN AOMBAPPC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:         
         RUN AOMBAPPC.P 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).                
      END.      
   END.
   
   ELSE IF visaupp.UPPFOLJVAL = 31 THEN DO:
      /*U40S- Projektuppföljningslista Sund*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.              
      excellista = 51.
      MESSAGE "Vill du se verifikaten från kostnadsregistreringen?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Kostnadsregistreringar?" UPDATE svar1 AS LOGICAL.         
      CASE svar1:
         WHEN TRUE THEN DO:
            uppvaltemp.MANUPPDEL = TRUE.
         END.
         WHEN FALSE THEN DO:
            uppvaltemp.MANUPPDEL = FALSE.
         END.
      END CASE.   
      MESSAGE "Vill du se delsumma för varje"  LC(Guru.Konstanter:gaok) "?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Delsummor?" UPDATE delnrsumma AS LOGICAL.         
      CASE svar1:
         WHEN TRUE THEN DO:
            musz = musz.
         END.
         WHEN FALSE THEN DO:
            musz = musz.
         END.
      END CASE.
      IF Guru.Konstanter:appcon THEN DO:
         RUN AOMBAPPSUNDC.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT delnrsumma,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:         
         RUN AOMBAPPSUNDC.P 
         (INPUT Guru.Konstanter:globanv,INPUT delnrsumma,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      
   END.
   
   ELSE IF visaupp.UPPFOLJVAL = 60 THEN DO:
      /*U45- Ny Projektuppföljningslista Sund*/
      FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.              
      excellista = 26.      
      IF Guru.Konstanter:appcon THEN DO:
         RUN AOMBAPPSU45C.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT delnrsumma,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:         
         RUN AOMBAPPSU45C.P 
         (INPUT Guru.Konstanter:globanv,INPUT delnrsumma,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      
   END.
   IF excellista > 0 THEN ENABLE FBTN_EXCEL WITH FRAME {&FRAME-NAME}.
   musz = FALSE. 
   ENABLE BRW_UT WITH FRAME FRAME-VINST.
   BRW_UT:HIDDEN = FALSE.       
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE k3excel_UI WINDOW-2 
PROCEDURE k3excel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   ASSIGN
   slutbredd = 4
   bredd[1] = 6  
   bredd[2] = 5  
   bredd[3] = 15 
   bredd[4] = 10. 
   i = 5.
   REPEAT:
      IF i > 200 THEN LEAVE.
      IF i > antalk3 THEN LEAVE.
      ASSIGN
      bredd[i] = 5.
      i = i + 1.        
   END.
   bladvar = 0.
   iRad = 1.
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[5],2) = "K3" THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[2],5) = "DELNR" OR SUBSTRING(tidut.UT,estartnr[2],5) = SUBSTRING(Guru.Konstanter:gdelnrk,1,5) THEN DO:
         LEAVE.
      END.
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-2 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   IF uppvaltemp.VALDLISTA BEGINS "Projektuppföljningslista" THEN DO:
      IF SUBSTRING(tidut.UT,27,2) = "UF" THEN
      tidut.UT:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE teexcel_UI WINDOW-2 
PROCEDURE teexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   ASSIGN
   slutbredd = 9
   bredd[1] = 6
   bredd[2] = 8
   bredd[3] = 3
   bredd[4] = 6
   bredd[5] = 5
   bredd[6] = 9
   bredd[7] = 9
   bredd[8] = 9
   bredd[9] = 9
   bredd[10] = 9
   bredd[11] = 9
   bladvar = 0
   iRad = 1.
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],4) = "SIGN" THEN DO:
         LEAVE.
      END.
      
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,4) = "====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.   
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 39
         bredd[6] = 13.
         allachar[4] = TRUE.
         RUN texelkost_UI.
         LEAVE.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE texelkost_UI WINDOW-2 
PROCEDURE texelkost_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("KOSTNADSREGISTRERING")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
   FIND NEXT tidut NO-LOCK NO-ERROR.
   
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transeko_UI WINDOW-2 
PROCEDURE transeko_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.        
   excellista = 26.
   IF Guru.Konstanter:appcon THEN DO:
      RUN TRANSEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
      OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
   END.
   ELSE DO:         
      RUN TRANSEKO.P 
      (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE valdaao, 
      OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).         
   END.
   IF excellista > 0 THEN ENABLE FBTN_EXCEL WITH FRAME {&FRAME-NAME}.
   musz = FALSE. 
   ENABLE BRW_UT WITH FRAME FRAME-VINST.
   BRW_UT:HIDDEN = FALSE.       

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
   FIND LAST tidut NO-LOCK NO-ERROR.
   /*PORTRAIT
   LANDSCAPE*/
   {PRINTLIGGANDE.I}
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
   ELSE IF visaupp.UPPFOLJVAL = 31 or visaupp.UPPFOLJVAL = 60  THEN DO:
     
      ASSIGN
      startpos = 1
      sidnrpos = 100.
   END.
   ELSE IF visaupp.UPPFOLJVAL = 41 THEN DO:
     
      ASSIGN
      startpos = 6
      sidnrpos = 80.
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

