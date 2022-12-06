&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



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
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
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
/*{EGENBEN.I}*/
{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{MTRLTAB.I}
{SOKDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
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
   FIELD Material-grupp  AS CHARACTER
   FIELD Pris AS DECIMAL
   FIELD Enhet     AS CHARACTER
   FIELD Leveransenhet     AS CHARACTER
   FIELD Fabrikat    AS CHARACTER
   FIELD AlternativtEnummer  AS CHARACTER.

DEFINE TEMP-TABLE Ovrigt NO-UNDO
   FIELD Enummer  AS CHARACTER
   FIELD Benamning AS CHARACTER
   FIELD Material-grupp  AS CHARACTER
   FIELD Pris AS DECIMAL
   FIELD Enhet     AS CHARACTER
   FIELD Leveransenhet     AS CHARACTER
   FIELD Fabrikat    AS CHARACTER
   FIELD AlternativtEnummer  AS CHARACTER.


DEFINE TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_LEVMTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp

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


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT_STOR BRW_LEVMTRL FBTN_KOPI FBTN_IMPEX ~
FBTN_MARKEX FBTN_EXP FBTN_EXPC FBTN_IMP FBTN_INKEX FBTN_INOEX FBTN_MARKEXA ~
FBTN_INK1EX FBTN_INK2EX FBTN_IMPXML FBTN_MARKXML FBTN_TUPPL BTN_NY BTN_BORT ~
BTN_KAB1 BTN_KAB2 BTN_TEXT FILL-IN-ENR FILL-IN-BEN RAD_SOK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENR FILL-IN-BEN RAD_SOK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_VLEV 
       MENU-ITEM m_Visa_information LABEL "Visa information via vald Leverantör".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KAB1 
     LABEL "Hämta Nexans" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KAB2 
     LABEL "Hämta NKT" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TEXT 
     LABEL "Text" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_EXP 
     LABEL "Export exel" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till excel".

DEFINE BUTTON FBTN_EXPC 
     LABEL "Export xml" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till xml-fil".

DEFINE BUTTON FBTN_IMP 
     LABEL "Import skv" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_IMPEX 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt)".

DEFINE BUTTON FBTN_IMPXML 
     LABEL "Import xml" 
     SIZE 14 BY 1 TOOLTIP "Exportera katalogen till xml-fil".

DEFINE BUTTON FBTN_INK1EX 
     LABEL "Imp Nexans Exel" 
     SIZE 14 BY 1 TOOLTIP "Import netto Nexans excel. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_INK2EX 
     LABEL "Imp NKT Exel" 
     SIZE 14 BY 1 TOOLTIP "Import netto NKT excel. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_INKEX 
     LABEL "Imp kabel -skv" 
     SIZE 14 BY 1 TOOLTIP "Import netto kabel skv. Ordning: Enr, benämning, enhet, bruttopris, nettopris".

DEFINE BUTTON FBTN_INOEX 
     LABEL "Imp övrigt -skv" 
     SIZE 14 BY 1 TOOLTIP "Import netto övrigt skv. Ordning: Enr, benämning, enhet, bruttopris, nettopri".

DEFINE BUTTON FBTN_KOPI 
     LABEL "Kopi.kata" 
     SIZE 14 BY 1 TOOLTIP "Kopiera in en annan katalog i denna katalog".

DEFINE BUTTON FBTN_MARKEX 
     LABEL "Märkta Exel" 
     SIZE 14 BY 1 TOOLTIP "Märker alla enr från excel i leverantörens materielkatalog. Enr i första kolumn".

DEFINE BUTTON FBTN_MARKEXA 
     LABEL "Märkta alt Exel" 
     SIZE 14 BY 1 TOOLTIP "Märker alla enr från excel i leverantörens materielkatalog. Enr i första kolumn".

DEFINE BUTTON FBTN_MARKXML 
     LABEL "Frekventa xml" 
     SIZE 14 BY 1 TOOLTIP "Skapar och märker alla enr från xml-fil i leverantörens materielkatalog.".

DEFINE BUTTON FBTN_TUPPL 
     LABEL "Test skv mot upplägg" 
     SIZE 14 BY 1 TOOLTIP "Ordning: Enr, benämning, enhet, bruttopris, nettopris".

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
     SIZE 42.5 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEVMTRL FOR 
      mtrltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
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
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 98 BY 22.17
         TITLE "Materiel för vald leverantör. Varje post sparas när du lämnar raden.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_LEVMTRL AT ROW 1.33 COL 1.5
     FBTN_KOPI AT ROW 3.83 COL 100
     FBTN_IMPEX AT ROW 4.96 COL 100
     FBTN_MARKEX AT ROW 6.08 COL 100
     FBTN_EXP AT ROW 7.21 COL 100
     FBTN_EXPC AT ROW 8.29 COL 100
     FBTN_IMP AT ROW 9.79 COL 100
     FBTN_INKEX AT ROW 11.25 COL 100
     FBTN_INOEX AT ROW 12.75 COL 100
     FBTN_MARKEXA AT ROW 14 COL 100
     FBTN_INK1EX AT ROW 15.5 COL 100
     FBTN_INK2EX AT ROW 16.75 COL 100
     FBTN_IMPXML AT ROW 18 COL 100
     FBTN_MARKXML AT ROW 19.25 COL 100
     FBTN_TUPPL AT ROW 20.5 COL 100
     BTN_NY AT ROW 24.33 COL 20.88
     BTN_BORT AT ROW 24.33 COL 34.5
     BTN_KAB1 AT ROW 24.33 COL 48.13
     BTN_KAB2 AT ROW 24.33 COL 61.75
     BTN_TEXT AT ROW 24.33 COL 75.5
     FILL-IN-ENR AT ROW 25.83 COL 16 COLON-ALIGNED
     FILL-IN-BEN AT ROW 27.04 COL 16 COLON-ALIGNED
     RAD_SOK AT ROW 28.21 COL 8.5 NO-LABEL
     BTN_AVB AT ROW 28.21 COL 100.63
     FILL-IN-UPP AT ROW 25.67 COL 71.5 COLON-ALIGNED NO-LABEL
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .83 AT ROW 25.83 COL 2.5
     RECT_STOR AT ROW 25.58 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.88 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_LEVMTRL RECT_STOR FRAME-A */
ASSIGN 
       BRW_LEVMTRL:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_VLEV:HANDLE
       BRW_LEVMTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_LEVMTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BTN_TEXT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_EXP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_EXPC:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_IMP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_IMPXML:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_KOPI:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_TUPPL:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UPP IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       RECT_STOR:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEVMTRL
/* Query rebuild information for BROWSE BRW_LEVMTRL
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = leverant
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "57" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ">>>>>99.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.mtrltemp.KUND
"mtrltemp.KUND" "Märkt" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEVMTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LEVMTRL
&Scoped-define SELF-NAME BRW_LEVMTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEVMTRL WINDOW-1
ON ROW-LEAVE OF BRW_LEVMTRL IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:
  IF AVAILABLE mtrltemp THEN DO:
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
     RUN visa_UI. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEVMTRL WINDOW-1
ON VALUE-CHANGED OF BRW_LEVMTRL IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
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
   
   /*CCC
   IF AVAILABLE mtrltemp THEN RUN visa_UI.
   */
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
      /*IF mtrltemp.Enr NE INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr THEN rowvar = ROWID(mtrltemp).
      ELSE rowvar = ?.*/
      mtrltemp.Enr = INPUT BROWSE BRW_LEVMTRL mtrltemp.Enr.
      DISPLAY mtrltemp.Enr WITH BROWSE BRW_LEVMTRL.          
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Benamning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Benamning IN BROWSE BRW_LEVMTRL /* Benämning */
DO:   
  IF AVAILABLE mtrltemp THEN RUN visa_UI. 
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
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enhet IN BROWSE BRW_LEVMTRL /* Enhet */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.   
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
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.BPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.BPRIS IN BROWSE BRW_LEVMTRL /* Brutto pris */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.NPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_LEVMTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.NPRIS IN BROWSE BRW_LEVMTRL /* Netto pris */
DO:
  IF AVAILABLE mtrltemp THEN RUN visa_UI.  
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
  

   /*APPLY "VALUE-CHANGED" TO BRW_LEVMTRL IN FRAME {&FRAME-NAME}.   */
  APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   
   RUN bort_UI.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KAB1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KAB1 WINDOW-1
ON CHOOSE OF BTN_KAB1 IN FRAME FRAME-A /* Hämta Nexans */
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
ON CHOOSE OF BTN_KAB2 IN FRAME FRAME-A /* Hämta NKT */
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


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:                                        
   {muswait.i}   
   CREATE mtrltemp.
   ASSIGN
   mtrltemp.LEVKOD = leverant
   mtrltemp.KALKNR = 0
   mtrltemp.ENR = ""
   mtrltemp.BENAMNING = ""
   mtrltemp.MTRLROW = ?.
   RUN setlastrowid_UI IN brwh (INPUT ROWID(mtrltemp)).
   RUN openbdynspec_UI IN brwh.
   RUN lastselectdyn_UI IN brwh.
   APPLY "ENTRY" TO mtrltemp.ENR IN BROWSE BRW_LEVMTRL.   
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TEXT WINDOW-1
ON CHOOSE OF BTN_TEXT IN FRAME FRAME-A /* Text */
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


&Scoped-define SELF-NAME FBTN_EXP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXP WINDOW-1
ON CHOOSE OF FBTN_EXP IN FRAME FRAME-A /* Export exel */
DO: 
  {muswait.i}
   {AVBGOM.I}   
   RUN EXPMTRLEXCV.P (INPUT leverant).
   {AVBFRAM.I}   
   {musarrow.i}                                                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXPC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXPC WINDOW-1
ON CHOOSE OF FBTN_EXPC IN FRAME FRAME-A /* Export xml */
DO: 
   
   {muswait.i}
   RUN EXPKATT.P (INPUT leverant, OUTPUT filnamnz).
   {musarrow.i}
   MESSAGE filnamnz VIEW-AS ALERT-BOX.
   /*RUN EXPMTRLEXCV.P (INPUT leverant).*/
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMP WINDOW-1
ON CHOOSE OF FBTN_IMP IN FRAME FRAME-A /* Import skv */
DO: 
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning skv-katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar5 AS LOGICAL.      
   CASE svar5:      
      WHEN TRUE OR WHEN FALSE THEN DO:    
         {muswait.i}                        
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den fil som motsvarar materielkatalog"         
         FILTERS        "Semikolon filer (*.skv;*.txt)"  "*.skv;*.txt"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         MUST-EXIST         
         USE-FILENAME
         UPDATE OKvald.
         IF OKvald = TRUE THEN DO:         
            IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
               EMPTY TEMP-TABLE felmex NO-ERROR. 
               RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar5,INPUT 0,OUTPUT TABLE felmex).         
            END.
            ELSE RUN IMPMTRLSKV.P (INPUT filnamn, INPUT leverant,INPUT svar5).         
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IMPEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMPEX WINDOW-1
ON CHOOSE OF FBTN_IMPEX IN FRAME FRAME-A /* Import Exel */
DO:      
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris, märkt(blank = ej märkt)" UPDATE svar2 AS LOGICAL.      
   CASE svar2:      
      WHEN TRUE OR WHEN FALSE THEN DO:
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
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
            IF Guru.Konstanter:globforetag = "celpa" THEN RUN XMKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex ).   
            ELSE RUN MKATEXELIN.P (INPUT fildir,OUTPUT TABLE mkattmp,OUTPUT TABLE felex ).   
            FILL-IN-UPP = "Nu börjar inläsning från temporär tabell till databas  " + STRING(TIME,"HH:MM").  
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
            RUN mkatexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant, INPUT svar2 ).             
            FILL-IN-UPP = " " .  
            DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
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
ON MOUSE-MENU-CLICK OF FBTN_IMPEX IN FRAME FRAME-A /* Import Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IMPXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IMPXML WINDOW-1
ON CHOOSE OF FBTN_IMPXML IN FRAME FRAME-A /* Import xml */
DO: 
   MESSAGE "Vill du ta bort den gamla katalogen och skapa ny katalog för " + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
           "Vill du uppdatera/komplettera den befintliga katalogen för " + levtemp.LEVNAMN + " ? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Inläsning xml-katalog för " + levtemp.LEVNAMN + ". Ordning: Enr, benämning, enhet, bruttopris, nettopris" UPDATE svar6 AS LOGICAL.      
   CASE svar6:      
      WHEN TRUE OR WHEN FALSE THEN DO:    
         {muswait.i}                        
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den fil som motsvarar materielkatalog"         
         FILTERS        "XML filer (*.xml)"  "*.xml"
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         MUST-EXIST         
         USE-FILENAME
         UPDATE OKvald.
         IF OKvald = TRUE THEN DO:                     
            
            EMPTY TEMP-TABLE felmex NO-ERROR. 
            filnamnuppxml = filnamn.
            /*INPUT FROM VALUE(filnamnuppxml) NO-ECHO.*/
            EMPTY TEMP-TABLE tempnamn NO-ERROR. 
            CREATE tempnamn.        
            tempnamn.NAMNH = BUFFER Ovrigt:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Ovrigt:HANDLE.
            tempnamn.NODTAB = tempnamn.NAMNH:NAME.
         
            RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
            RUN xmlslut_UI IN dynxml.
            RUN mkatxml_UI IN mtrlxapph (INPUT TABLE Ovrigt,INPUT leverant, INPUT svar6,INPUT FALSE,OUTPUT TABLE felmex ).             
         
            /*RUN IMPMTRLXML.P (INPUT filnamn, INPUT leverant,INPUT svar6,INPUT 0,INPUT FALSE,OUTPUT TABLE felmex).                     */
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från xml-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
      END.
   END.

   /*{muswait.i}
   RUN EXPKATT.P (INPUT leverant, OUTPUT filnamnz).
   {musarrow.i}
   MESSAGE filnamnz VIEW-AS ALERT-BOX.    */
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INK1EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK1EX WINDOW-1
ON CHOOSE OF FBTN_INK1EX IN FRAME FRAME-A /* Imp Nexans Exel */
DO:      
   
   IF Guru.Konstanter:globforetag = "fors" THEN katnamn = "kablar".
   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN katnamn = "Nexans".
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
           
            IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:            
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
                  extradatatemp.SOKCHAR[2] =  globanv
                  extradatatemp.SOKLOG[1] =  TRUE.
                  RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
                  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
                  EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
               END.
            END.
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
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
ON MOUSE-MENU-CLICK OF FBTN_INK1EX IN FRAME FRAME-A /* Imp Nexans Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INK2EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INK2EX WINDOW-1
ON CHOOSE OF FBTN_INK2EX IN FRAME FRAME-A /* Imp NKT Exel */
DO:      
   IF Guru.Konstanter:globforetag = "fors" THEN katnamn = "övrigt".
   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN katnamn = "NKT".
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
            IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "elpa" THEN DO:            
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
                  extradatatemp.SOKCHAR[2] =  globanv
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
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT UNFORMATTED felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
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
ON MOUSE-MENU-CLICK OF FBTN_INK2EX IN FRAME FRAME-A /* Imp NKT Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INKEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INKEX WINDOW-1
ON CHOOSE OF FBTN_INKEX IN FRAME FRAME-A /* Imp kabel -skv */
DO:      
   IF Guru.Konstanter:globforetag = "fors" THEN katnamn = "kablar".
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
            RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar2,INPUT 1,OUTPUT TABLE felmex).                     
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.

         /*DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in som nettopriskatalog kablar"
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Ny nettopriskatalog kabel för " + levtemp.LEVNAMN + " är nu inläst. För att visa den - använd sökfunktionen. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.

            
            {musarrow.i}
         END.*/
      END.
      OTHERWISE RETURN.
   END CASE.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INKEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INKEX IN FRAME FRAME-A /* Imp kabel -skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INOEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INOEX WINDOW-1
ON CHOOSE OF FBTN_INOEX IN FRAME FRAME-A /* Imp övrigt -skv */
DO:      
   IF Guru.Konstanter:globforetag = "fors" THEN katnamn = "övrigt".
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
            RUN IMPMTRLSKVN.P (INPUT filnamn, INPUT leverant,INPUT svar2,INPUT 2,OUTPUT TABLE felmex).                     
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            FIND FIRST felmex NO-ERROR.
            IF AVAILABLE felmex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felmex:           
                  PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Nu är materielkatalogen inläst." VIEW-AS ALERT-BOX.
         END.
         /*DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         OS-CREATE-DIR VALUE(fildir) NO-ERROR.
         mappvar = fildir.
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den excelfil som Ni vill läsa in som nettopriskatalog övrigt"
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
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            RUN nettokabovrexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant, INPUT svar2,INPUT 2 ). 
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).
               PUT "Dessa rader blev ej inlästa från excel till materielkatalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:           
                  PUT felex.ENR AT 6 SUBSTRING(felex.BENAMNING,1,15) AT 16 SUBSTRING(felex.ENHET,1,4) AT 36 STRING(felex.BPRIS) AT 41 STRING(felex.NPRIS) AT 52.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
               RUN OPENDOC.P (felfil,"","",NO).         
            END.   
            MESSAGE "Ny nettopriskatalog övrigt för " + levtemp.LEVNAMN + " är nu inläst. För att visa den - använd sökfunktionen. '#' -hämtar alla poster." VIEW-AS ALERT-BOX.

            
            {musarrow.i}
         END.*/
      END.
      OTHERWISE RETURN.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INOEX WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_INOEX IN FRAME FRAME-A /* Imp övrigt -skv */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOPI WINDOW-1
ON CHOOSE OF FBTN_KOPI IN FRAME FRAME-A /* Kopi.kata */
DO: 
   {muswait.i}
   {AVBGOM.I}
   RUN KOPIKATAV.W.
   {AVBFRAM.I}
   {musarrow.i}                                                              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARKEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKEX WINDOW-1
ON CHOOSE OF FBTN_MARKEX IN FRAME FRAME-A /* Märkta Exel */
DO:      
   
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
               RUN MMARKEXELIN.P (INPUT fildir, INPUT svar4 ,OUTPUT TABLE mkattmp ).               
               RUN mmarkexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant,INPUT svar3,INPUT svar4,OUTPUT TABLE felex ). 
               FIND FIRST felex NO-ERROR.
               IF AVAILABLE felex THEN DO:               
                  felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
                  OUTPUT TO VALUE(felfil).               
                  PUT "Dessa enummer från excelfilen fick inte träff i katalogen. Var god kontrollera!" AT 6.         
                  PUT SKIP.         
                  FOR EACH felex:                  
                     PUT UNFORMATTED felex.ENR AT 6.  
                     PUT SKIP.
                  END.
                  OUTPUT CLOSE.
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
ON MOUSE-MENU-CLICK OF FBTN_MARKEX IN FRAME FRAME-A /* Märkta Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARKEXA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKEXA WINDOW-1
ON CHOOSE OF FBTN_MARKEXA IN FRAME FRAME-A /* Märkta alt Exel */
DO:      
   
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
               RUN MMARKEXELIN.P (INPUT fildir, INPUT svar4 ,OUTPUT TABLE mkattmp ).               
               RUN mmarkexcel_UI IN mtrlbapph (INPUT TABLE mkattmp,INPUT leverant,INPUT svar3,INPUT svar4,OUTPUT TABLE felex ). 
               FIND FIRST felex NO-ERROR.
               IF AVAILABLE felex THEN DO:               
                  felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
                  OUTPUT TO VALUE(felfil).               
                  PUT "Dessa enummer från excelfilen fick inte träff i katalogen. Var god kontrollera!" AT 6.         
                  PUT SKIP.         
                  FOR EACH felex:                  
                     PUT SUBSTRING (felex.ENR,1,10) AT 6.  
                     PUT SKIP.
                  END.
                  OUTPUT CLOSE.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKEXA WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_MARKEXA IN FRAME FRAME-A /* Märkta alt Exel */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARKXML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKXML WINDOW-1
ON CHOOSE OF FBTN_MARKXML IN FRAME FRAME-A /* Frekventa xml */
DO:      
   
   MESSAGE "Vill du först ta bort gamla frekvent-katalogen och sedan skapa ny frekvent-katalogen för "  + levtemp.LEVNAMN + " ? - Svara Ja" SKIP
     "Vill du uppdatera/komplettera den befintliga frekvent-katalogen för " + levtemp.LEVNAMN + "? - Svara Nej"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Frekvent för " + levtemp.LEVNAMN + "." UPDATE svar6 AS LOGICAL.      
   CASE svar6:
      WHEN TRUE OR WHEN FALSE THEN DO:         
         DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
         SYSTEM-DIALOG GET-FILE filnamn
         TITLE          "Välj den excelfil som har enummer som skall märkas i vald materielkatalog"
         FILTERS        "All Files (*.xml)"  "*.xml"   
         INITIAL-DIR    SESSION:TEMP-DIRECTORY
         UPDATE OKvald.      
         IF OKvald = TRUE THEN DO:                
            {muswait.i}
            EMPTY TEMP-TABLE felmex NO-ERROR. 
            filnamnuppxml = filnamn.
            /*INPUT FROM VALUE(filnamnuppxml) NO-ECHO.*/
            EMPTY TEMP-TABLE tempnamn NO-ERROR. 
            CREATE tempnamn.        
            tempnamn.NAMNH = BUFFER Frekventa:HANDLE.       
            tempnamn.TORGTH = TEMP-TABLE Frekventa:HANDLE.
            tempnamn.NODTAB = tempnamn.NAMNH:NAME.
         
            RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
            RUN xmlslut_UI IN dynxml.
            RUN mkatoxml_UI IN mtrlxapph (INPUT TABLE Frekventa,INPUT leverant, INPUT svar6,INPUT TRUE,OUTPUT TABLE felmex ).             


            
            /*RUN IMPMTRLXML.P (INPUT filnamn, INPUT leverant,INPUT svar6,INPUT 0,INPUT TRUE,OUTPUT TABLE felmex).                     */
            FIND FIRST felex NO-ERROR.
            IF AVAILABLE felex THEN DO:               
               felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
               OUTPUT TO VALUE(felfil).               
               PUT "Dessa enummer från excelfilen fick inte träff i katalogen. Var god kontrollera!" AT 6.         
               PUT SKIP.         
               FOR EACH felex:                  
                  PUT UNFORMATTED felex.ENR AT 6.  
                  PUT SKIP.
               END.
               OUTPUT CLOSE.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARKXML WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_MARKXML IN FRAME FRAME-A /* Frekventa xml */
DO:
   
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_TUPPL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TUPPL WINDOW-1
ON CHOOSE OF FBTN_TUPPL IN FRAME FRAME-A /* Test skv mot upplägg */
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
         /*RUN IMPMTRLSKVT.P (INPUT filnamn, INPUT leverant,INPUT 0,OUTPUT TABLE felmex).         */
      END.
      
   END.             
   {musarrow.i}      

   
   FIND FIRST felmex NO-ERROR.
   IF AVAILABLE felmex THEN DO:
      felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".                      
      OUTPUT TO VALUE(felfil).
      PUT "Dessa rader kommer ej att gå att läsa in från skv-filen till materielkatalogen. Var god kontrollera!" AT 6.         
      PUT SKIP.         
      FOR EACH felmex:           
         PUT UNFORMATTED felmex.ENR AT 6 SUBSTRING(felmex.BENAMNING,1,15) AT 16 SUBSTRING(felmex.ENHET,1,4) AT 36 STRING(felmex.BPRIS) AT 41 STRING(felmex.NPRIS) AT 52.  
         PUT SKIP.
      END.
      OUTPUT CLOSE.
      RUN OPENDOC.P (felfil,"","",NO).         
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
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
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   RUN reado_UI (INPUT TRUE).
   {muswait.i}   
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
   /*ccc*/
   
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


&Scoped-define SELF-NAME m_Visa_information
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_information WINDOW-1
ON CHOOSE OF MENU-ITEM m_Visa_information /* Visa information via vald Leverantör */
DO:
   RUN infoES_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE edataapph.   
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
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}   
      
   FBTN_IMPEX:LABEL = "Import Excel".
   FBTN_EXP:LABEL = "Export Excel".
   FBTN_MARKEX:LABEL = "Märkta Excel".
   FBTN_MARKEXA:LABEL = "Märkta alt Excel".
   /*FBTN_INKEX:LABEL = "Imp kab Excel".
   FBTN_INOEX:LABEL = "Imp övr Excel".*/
   bortfraga = FALSE.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = leverant NO-LOCK NO-ERROR.                                                          
   ASSIGN WINDOW-1:TITLE = "Uppdatering av katalog för leverantör " + levtemp.LEVNAMN.   
   /*BRW_LEVMTRL:TITLE = "Materiellista : " + levtemp.LEVNAMN.                      */
   RUN setorgtitle_UI IN brwh (INPUT "Materiellista huvudleverantör: " + levtemp.LEVNAMN + ". " + "Varje post sparas när du lämnar raden.").         
   RUN enable_UI.   
   
   {FRMSIZE.I}        
   IF leverant = "99" THEN DO:
      ASSIGN
      BTN_TEXT:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      BTN_TEXT:HIDDEN = TRUE.      
   END.
   ASSIGN
   FBTN_EXP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FBTN_EXPC:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_INKEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_INOEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_INK1EX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_INK2EX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_MARKEXA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_IMPXML:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_MARKXML:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_KAB1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_KAB2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FBTN_TUPPL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ASSIGN      
      FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "LULE" AND Guru.Konstanter:globanv = "CELPA" THEN DO:
         ASSIGN         
         FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.      
      IF Guru.Konstanter:globforetag = "SUND" AND Guru.Konstanter:globanv = "ELPAO" THEN DO:
         ASSIGN         
         FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.      
      IF Guru.Konstanter:globforetag = "CELPA"  THEN DO:
         ASSIGN         
         FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.            
   END.
   IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:      
      ASSIGN
      FBTN_IMP:LABEL = "Import -skv"
      FBTN_INKEX:LABEL = "Imp kabel -skv"
      FBTN_INOEX:LABEL = "Imp övrigt -skv"      
      BTN_KAB1:LABEL = "Hämta kabel"      
      BTN_KAB2:LABEL = "Hämta övrigt"      
      FBTN_MARKEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_IMPEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_INKEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_INOEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_KAB1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      BTN_KAB2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ASSIGN
      FBTN_INK1EX:LABEL = "Import Nexans excel"
      FBTN_INK2EX:LABEL = "Import NKT excel"      
      FBTN_MARKEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_IMPEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FBTN_INK1EX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_INK2EX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      BTN_KAB1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      BTN_KAB2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         IF Guru.Konstanter:globanv = "elpao" THEN FBTN_TUPPL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         IF Guru.Konstanter:globanv = "elpao" THEN FBTN_IMPXML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         IF Guru.Konstanter:globanv = "elpao" THEN FBTN_MARKXML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.

   
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_KOPI:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_KOPI:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_IMPEX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMPEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_MARKEX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_IMP:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMP:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INKEX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INKEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INOEX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INOEX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INK1EX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INK1EX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_INK2EX:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_INK2EX:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_IMPXML:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IMPXML:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_MARKXML:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKXML:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_EXP:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXP:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_EXPC:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EXPC:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   /*IF FBTN_MARKEXA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MARKEXA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   */
   
   IF FBTN_TUPPL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_TUPPL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   

   

   mtrltemp.ENR:LABEL IN BROWSE BRW_LEVMTRL = Guru.Konstanter:genk.
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.  
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
   RUN DYNBRW.P PERSISTENT SET brwh (INPUT BRW_LEVMTRL:HANDLE IN FRAME {&FRAME-NAME}).       
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END. 
   RUN UTXMLDYN.P PERSISTENT SET dynxml (INPUT TRUE).
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
   antal_valda = BRW_LEVMTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR. 
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
         IF antal_raknare = antal_valda THEN DO:
            IF antanurval = antal_valda THEN.
            ELSE DO:              
               RUN selnextprevrow_UI IN brwh.               
            END.
         END.  
         
         DELETE mtrltemp.                                   
         
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
  DISPLAY FILL-IN-ENR FILL-IN-BEN RAD_SOK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT_STOR BRW_LEVMTRL FBTN_KOPI FBTN_IMPEX FBTN_MARKEX FBTN_EXP 
         FBTN_EXPC FBTN_IMP FBTN_INKEX FBTN_INOEX FBTN_MARKEXA FBTN_INK1EX 
         FBTN_INK2EX FBTN_IMPXML FBTN_MARKXML FBTN_TUPPL BTN_NY BTN_BORT 
         BTN_KAB1 BTN_KAB2 BTN_TEXT FILL-IN-ENR FILL-IN-BEN RAD_SOK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   status-ok = BRW_LEVMTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
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

