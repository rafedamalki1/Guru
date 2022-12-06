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

  Created: 95/05/22 -  1:44 pm

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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
/*{LEVTEMP.I}
{DEPATEMP.I}
{DEPSEKTEMP.I}
*/

{SPECMTRLTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
{HOPPSEK2W.I}

{LAGERTRUM.I}
DEFINE NEW SHARED TEMP-TABLE off_mtrl NO-UNDO     
   FIELD TOTALT AS DECIMAL.

DEFINE VARIABLE Timer                      AS Guru.Timer              NO-UNDO.
DEFINE NEW SHARED VARIABLE inv AS LOGICAL NO-UNDO.     
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE VARIABLE inventapph AS HANDLE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.  
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.    
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE colnamevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ascdesclog AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(96)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO.
DEFINE VARIABLE datvar AS DATE NO-UNDO.
DEFINE VARIABLE edatvar AS DATE NO-UNDO.
DEFINE VARIABLE redaninv AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE utlop AS INTEGER NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nytt_bestnr AS INTEGER NO-UNDO.
DEFINE VARIABLE visatrum AS LOGICAL NO-UNDO.


{TIDUTTTNEW.I}    

DEFINE TEMP-TABLE depbuff NO-UNDO
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD DEPNR AS INTEGER
   FIELD IB AS INTEGER
   FIELD SALDO AS INTEGER
   FIELD INVANT AS INTEGER
   FIELD IBDATUM AS DATE
   FIELD FACKID AS CHARACTER.

/*DEFINE QUERY mtrlq FOR MTRLDEP.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AOTR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vaotrum spec_mtrl

/* Definitions for BROWSE BRW_AOTR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AOTR vaotrum.TRUMMAPLATS vaotrum.AONR ~
vaotrum.DELNR vaotrum.enr vaotrum.stmeter vaotrum.PRIS vaotrum.TRUMMANR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOTR 
&Scoped-define QUERY-STRING-BRW_AOTR FOR EACH vaotrum NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AOTR OPEN QUERY BRW_AOTR FOR EACH vaotrum NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AOTR vaotrum
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOTR vaotrum


/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 spec_mtrl.FACKID spec_mtrl.Enr ~
spec_mtrl.Benamning spec_mtrl.SALDO spec_mtrl.Invant spec_mtrl.ENHET ~
spec_mtrl.KLAR spec_mtrl.EJMOT spec_mtrl.NPRIS spec_mtrl.SUMMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 spec_mtrl.Invant ~
spec_mtrl.KLAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL2 spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL2 spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH spec_mtrl ~
      WHERE spec_mtrl.Depnr = vald_depa NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH spec_mtrl ~
      WHERE spec_mtrl.Depnr = vald_depa NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 IMAGE-4 BRW_MTRL2 FBTN_VAR FBTN_NOLL ~
FBTN_EXCEL FBTN_SKRIV BRW_AOTR FILL-IN-TOT FILL-IN-DENR FILL-IN-DBEN ~
FBTN_SNABB FBTN_OK BTN_AVB FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TOT FILL-IN-DENR FILL-IN-DBEN ~
FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_NOLL 
     LABEL "Nollställ" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SNABB 
     LABEL "Snabbspara" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VAR 
     LABEL "Värde" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-DBEN AS CHARACTER FORMAT "X(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-DENR AS CHARACTER FORMAT "X(11)" 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACK AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOT AS DECIMAL FORMAT "->>>>>>>>>9.99":U INITIAL 0 
     LABEL "Totalt värde" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .75.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.13 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AOTR FOR 
      vaotrum SCROLLING.

DEFINE QUERY BRW_MTRL2 FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AOTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOTR WINDOW-1 _STRUCTURED
  QUERY BRW_AOTR NO-LOCK DISPLAY
      vaotrum.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "X(8)":U
            WIDTH 5
      vaotrum.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      vaotrum.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      vaotrum.enr COLUMN-LABEL "Enr" FORMAT "X(15)":U WIDTH 8
      vaotrum.stmeter COLUMN-LABEL "Lager" FORMAT ">>>>>9":U
      vaotrum.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9.99":U
      vaotrum.TRUMMANR COLUMN-LABEL "Id" FORMAT ">>>>>9":U WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING NO-SCROLLBAR-VERTICAL SIZE 27 BY 10.5
         TITLE "Trummor som belastar lager".

DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      spec_mtrl.FACKID FORMAT "X(8)":U
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.Benamning FORMAT "x(256)":U WIDTH 35
      spec_mtrl.SALDO FORMAT ">>>,>>9":U
      spec_mtrl.Invant FORMAT ">>>>9":U
      spec_mtrl.ENHET FORMAT "x(5)":U
      spec_mtrl.KLAR COLUMN-LABEL "Klar" FORMAT "Ja/Nej":U
      spec_mtrl.EJMOT COLUMN-LABEL "Ej      !mottaget" FORMAT ">>>>>9":U
            WIDTH 6
      spec_mtrl.NPRIS COLUMN-LABEL "Pris" FORMAT ">>>>>>9.99":U
      spec_mtrl.SUMMA COLUMN-LABEL "Värde" FORMAT ">>>>>>9.99":U
  ENABLE
      spec_mtrl.Invant
      spec_mtrl.KLAR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 94.5 BY 23.25
         TITLE "Inventering".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_MTRL2 AT ROW 1.5 COL 1.5
     FBTN_VAR AT ROW 1.67 COL 98.63
     FBTN_NOLL AT ROW 3.21 COL 98.63
     FBTN_EXCEL AT ROW 4.75 COL 98.63
     FBTN_SKRIV AT ROW 6.25 COL 98.63
     BRW_AOTR AT ROW 14.21 COL 97.5 WIDGET-ID 200
     FILL-IN-TOT AT ROW 25.25 COL 65 COLON-ALIGNED
     FILL-IN-DENR AT ROW 25.33 COL 14.38 COLON-ALIGNED
     FILL-IN-DBEN AT ROW 26.33 COL 14.38 COLON-ALIGNED
     FBTN_SNABB AT ROW 27.17 COL 78.88 WIDGET-ID 2
     FBTN_OK AT ROW 27.17 COL 94.13
     BTN_AVB AT ROW 27.17 COL 109.13
     FILL-IN-FACKID1 AT ROW 27.29 COL 14.38 COLON-ALIGNED
     FILL-IN-FACK AT ROW 27.29 COL 14.38 COLON-ALIGNED NO-LABEL
     FILL-IN-FACKID2 AT ROW 27.29 COL 30.5 COLON-ALIGNED
     RECT-15 AT ROW 25.08 COL 1.5
     IMAGE-4 AT ROW 25.33 COL 2.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 27.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: vaotrum T "?" NO-UNDO temp-db vaotrum
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28
         WIDTH              = 125
         MAX-HEIGHT         = 28
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_MTRL2 IMAGE-4 FRAME-A */
/* BROWSE-TAB BRW_AOTR FBTN_SKRIV FRAME-A */
ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       FILL-IN-DBEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-DENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID1:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-TOT:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOTR
/* Query rebuild information for BROWSE BRW_AOTR
     _TblList          = "Temp-Tables.vaotrum"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.vaotrum.TRUMMAPLATS
"vaotrum.TRUMMAPLATS" "Trumma!namn" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.vaotrum.AONR
"vaotrum.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.vaotrum.DELNR
"vaotrum.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.vaotrum.enr
"vaotrum.enr" "Enr" "X(15)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.vaotrum.stmeter
"vaotrum.stmeter" "Lager" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.vaotrum.PRIS
"vaotrum.PRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.vaotrum.TRUMMANR
"vaotrum.TRUMMANR" "Id" ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOTR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _Where[1]         = "spec_mtrl.Depnr = vald_depa"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.FACKID
     _FldNameList[2]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[3]   > Temp-Tables.spec_mtrl.Benamning
"Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.spec_mtrl.SALDO
     _FldNameList[5]   > Temp-Tables.spec_mtrl.Invant
"Invant" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.spec_mtrl.ENHET
     _FldNameList[7]   > Temp-Tables.spec_mtrl.KLAR
"KLAR" "Klar" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.spec_mtrl.EJMOT
"EJMOT" "Ej      !mottaget" ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.spec_mtrl.NPRIS
"NPRIS" "Pris" ">>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.spec_mtrl.SUMMA
"SUMMA" "Värde" ">>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 WINDOW-1
ON ROW-DISPLAY OF BRW_MTRL2 IN FRAME FRAME-A /* Inventering */
DO:
  /*spec_mtrl.INVANT:FORMAT IN BROWSE BRW_MTRL2 = "X(5)".  */
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 WINDOW-1
ON ROW-LEAVE OF BRW_MTRL2 IN FRAME FRAME-A /* Inventering */
DO:
  IF AVAILABLE spec_mtrl THEN DO:
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI. 
     ASSIGN     
     spec_mtrl.INVANT = INPUT BROWSE BRW_MTRL2 spec_mtrl.INVANT
     spec_mtrl.KLAR = INPUT BROWSE BRW_MTRL2 spec_mtrl.KLAR.
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME FRAME-A /* Inventering */
DO:
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.Invant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Invant BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON ANY-KEY OF spec_mtrl.Invant IN BROWSE BRW_MTRL2 /* inv-antal */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:      
         IF spec_mtrl.INVANT NE INPUT BROWSE BRW_MTRL2 spec_mtrl.INVANT THEN DO: 
            FILL-IN-TOT = FILL-IN-TOT - spec_mtrl.INVANT * spec_mtrl.NPRIS.
            spec_mtrl.INVANT = INPUT BROWSE BRW_MTRL2 spec_mtrl.INVANT.
            FILL-IN-TOT = FILL-IN-TOT + spec_mtrl.INVANT * spec_mtrl.NPRIS.
            spec_mtrl.SUMMA = spec_mtrl.INVANT * spec_mtrl.NPRIS.
            spec_mtrl.KLAR = TRUE.    
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
            /*DISPLAY spec_mtrl.INVANT spec_mtrl.KLAR WITH BROWSE BRW_MTRL2.
            RUN janej_UI.         */
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Invant BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.Invant IN BROWSE BRW_MTRL2 /* inv-antal */
DO:
  IF AVAILABLE spec_mtrl THEN DO: 
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Invant BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.Invant IN BROWSE BRW_MTRL2 /* inv-antal */
DO:
   IF AVAILABLE spec_mtrl THEN DO:      
      IF spec_mtrl.INVANT NE INPUT BROWSE BRW_MTRL2 spec_mtrl.INVANT THEN DO: 
         FILL-IN-TOT = FILL-IN-TOT - spec_mtrl.INVANT * spec_mtrl.NPRIS.
         spec_mtrl.INVANT = INPUT BROWSE BRW_MTRL2 spec_mtrl.INVANT.
         FILL-IN-TOT = FILL-IN-TOT + spec_mtrl.INVANT * spec_mtrl.NPRIS.
         spec_mtrl.SUMMA = spec_mtrl.INVANT * spec_mtrl.NPRIS.
         spec_mtrl.KLAR = TRUE.    
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
         /*DISPLAY spec_mtrl.INVANT spec_mtrl.KLAR WITH BROWSE BRW_MTRL2.
         RUN janej_UI.         */
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.Invant BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.Invant IN BROWSE BRW_MTRL2 /* inv-antal */
DO:
   APPLY "ENTRY" TO spec_mtrl.Invant IN BROWSE BRW_MTRL2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.KLAR BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.KLAR IN BROWSE BRW_MTRL2 /* Klar */
DO:
  IF AVAILABLE spec_mtrl THEN DO: 
     IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.KLAR BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.KLAR IN BROWSE BRW_MTRL2 /* Klar */
DO:
   IF AVAILABLE spec_mtrl THEN DO:   
      spec_mtrl.KLAR = INPUT BROWSE BRW_MTRL2 spec_mtrl.KLAR.             
      IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.KLAR BRW_MTRL2 _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.KLAR IN BROWSE BRW_MTRL2 /* Klar */
DO:    
   IF AVAILABLE spec_mtrl THEN DO:   
      spec_mtrl.KLAR = INPUT BROWSE BRW_MTRL2 spec_mtrl.KLAR.           

      IF spec_mtrl.KLAR = TRUE THEN spec_mtrl.KLAR = FALSE.
      ELSE spec_mtrl.KLAR = TRUE.    
      /*DISPLAY spec_mtrl.KLAR WITH BROWSE BRW_MTRL2.          */
      /*RUN janej2_UI.  */
      IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN RUN visa_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   IF depatitta = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:       
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                    
      ELSE DO:
         musz = musz.
      END.     
   END.
   ELSE DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-1
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-A /* Visa i Excel */
DO: 
   {muswait.i}  
    RUN excelproc IN brwproc[2].  
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_NOLL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NOLL WINDOW-1
ON CHOOSE OF FBTN_NOLL IN FRAME FRAME-A /* Nollställ */
DO:
   
   MESSAGE "Detta nollställer alla tidigare inventerade värden. OBS! Förbrukningslista måste tas ut före nollställning." 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Nollställning" UPDATE svar AS LOGICAL.       
   IF svar THEN DO:
      EMPTY TEMP-TABLE depbuff  NO-ERROR.       
      FOR EACH spec_mtrl:
         CREATE depbuff.
         ASSIGN
         depbuff.DEPNR = vald_depa
         depbuff.ENR = spec_mtrl.ENR
         depbuff.BENAMNING = spec_mtrl.BENAMNING
         depbuff.FACKID = spec_mtrl.FACKID
         depbuff.ENHET = spec_mtrl.ENHET
         depbuff.INVANT = spec_mtrl.INVANT
         depbuff.IB = spec_mtrl.SALDO
         depbuff.IBDATUM = spec_mtrl.INVDATUM.        
      END.                   
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "UMEA" THEN DO:
         FOR EACH spec_mtrl:
            ASSIGN              
            spec_mtrl.INVANT = 0
            spec_mtrl.INVDATUM = TODAY.            
         END.
      END.   
      ELSE DO:
         FOR EACH spec_mtrl:
            ASSIGN  
            spec_mtrl.SALDO = spec_mtrl.INVANT
            spec_mtrl.INVANT = 0
            spec_mtrl.INVDATUM = TODAY.            
         END.
      END.   
      /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SNAT" OR
      Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE"  OR Guru.Konstanter:globforetag = "KRAF" THEN DO:
         FOR EACH spec_mtrl:
            ASSIGN  
            spec_mtrl.SALDO = spec_mtrl.INVANT
            spec_mtrl.INVANT = 0
            spec_mtrl.INVDATUM = TODAY.            
         END.                                 
      END.
      ELSE DO: 
         FOR EACH spec_mtrl:
            ASSIGN              
            spec_mtrl.INVANT = 0
            spec_mtrl.INVDATUM = TODAY.            
         END.  
      END.*/     
      RUN open_UI.
   END.  
   ELSE DO:
      musz = musz.
   END.           
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}
   redaninv = FALSE.
   RUN redaninv_UI IN inventapph (INPUT vald_depa,OUTPUT redaninv).
   IF redaninv = TRUE THEN DO:
      FIND FIRST spec_mtrl NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         /*om man har gjort snabbspara när man är klar finns inga poster kvar*/
         MESSAGE "Du har redan gjort en inventering idag och får inte starta en till! "   VIEW-AS ALERT-BOX.       
         RETURN.
      END.      
   END.    
   FIND FIRST spec_mtrl WHERE spec_mtrl.KLAR = FALSE NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      MESSAGE "OBS! Du har inte inventerat alla artiklar i depå. Vill du ändå spara för att fortsätta inventeringen vid annat tillfälle?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ej färdig inventering" UPDATE svar AS LOGICAL.         
      IF svar = FALSE  THEN RETURN.   
   END.      
   IF Guru.Konstanter:varforetypval[57] = 1 THEN DO:          
      IF edatvar = ? THEN DO:
         FOR EACH trspec_mtrl:
            CREATE spec_mtrl.
            BUFFER-COPY trspec_mtrl TO spec_mtrl.
            DELETE trspec_mtrl.   
         END.
      END.
   END.                  
     
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN INVENTUTV.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT TABLE spec_mtrl, INPUT TABLE depbuff).
   END.
   ELSE DO:
      RUN INVENTUTV.P
      (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT TABLE spec_mtrl, INPUT TABLE depbuff).
   END.      
   {musarrow.i}
   APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   EMPTY TEMP-TABLE tidut  NO-ERROR. 
   
   str=
   "=======================================================================================".      
   RUN getcolname_UI IN brwproc[2] (OUTPUT colnamevar ,OUTPUT ascdesclog).   
   /*sortering = INPUT RAD_SORT. */
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:       
      {muswait.i}          
      skrivut = TRUE.
      RUN huvud_UI.
      RUN rubrik_UI.
      RUN klar_UI.      
      IF inv = TRUE THEN RUN tot_UI.   
      {AVBGOM.I}
      RUN VISAINVV.W (INPUT TABLE tidut).
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SNABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SNABB WINDOW-1
ON CHOOSE OF FBTN_SNABB IN FRAME FRAME-A /* Snabbspara */
DO:
   {muswait.i}
   /*FIND FIRST spec_mtrl WHERE spec_mtrl.KLAR = FALSE NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      MESSAGE "OBS! Du har inte inventerat alla artiklar i depå. Vill du ändå spara för att fortsätta inventeringen vid annat tillfälle?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ej färdig inventering" UPDATE svar AS LOGICAL.         
      IF svar = FALSE  THEN RETURN.   
   END.*/      
   
   redaninv = FALSE.
   RUN redaninv_UI IN inventapph (INPUT vald_depa,OUTPUT redaninv).
   IF redaninv = TRUE THEN DO:
      FIND FIRST spec_mtrl NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         /*om man har gjort snabbspara när man är klar finns inga poster kvar*/
         MESSAGE "Du har redan gjort en inventering idag och får inte starta en till! "   VIEW-AS ALERT-BOX.       
         RETURN.
      END.      
   END.    
   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN INVENTUTV.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT TABLE spec_mtrl, INPUT TABLE depbuff).
   END.
   ELSE DO:
      RUN INVENTUTV.P
      (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT TABLE spec_mtrl, INPUT TABLE depbuff).
   END.
   /*rensa redan inventerade artiklar*/      
   FOR EACH spec_mtrl WHERE spec_mtrl.KLAR = TRUE :
      DELETE spec_mtrl.
   END.   
   RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "ENR").
   RUN openbdynspec_UI IN brwproc[2].
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Inventering med startdatum " + string(datvar)).
   RUN title_UI IN brwproc[2].
   {musarrow.i}
   /*APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.*/
                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VAR WINDOW-1
ON CHOOSE OF FBTN_VAR IN FRAME FRAME-A /* Värde */
DO: 
   {muswait.i}    
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE spec_mtrl THEN RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(spec_mtrl)).         
   str=
   "=======================================================================================".      
   RUN getcolname_UI IN brwproc[2] (OUTPUT colnamevar ,OUTPUT ascdesclog).   
   /*sortering = INPUT RAD_SORT.*/
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   inv = TRUE.
   RUN huvud_UI.
   RUN rubrik_UI.
   RUN klar_UI.      
   IF inv = TRUE THEN RUN tot_UI.   
   {AVBGOM.I}
   RUN VISAINVV.W (INPUT TABLE tidut).    
   RUN openbdynspec_UI IN brwproc[2]. 
   RUN lastselectdyn_UI IN brwproc[2]. 
   
   
   {AVBFRAM.I}
   inv = FALSE. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON ANY-KEY OF FILL-IN-FACK IN FRAME FRAME-A
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACK IN FRAME FRAME-A
DO:
   ASSIGN
   FILL-IN-FACK = var1 + var2.    
   IF AVAILABLE spec_mtrl THEN mtrl_rowid = ROWID(spec_mtrl).
   IF FILL-IN-FACK = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN-FACK + '*'. 
   status-ok = BRW_MTRL2:DESELECT-ROWS().   
   FIND spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.                  
   FIND NEXT spec_mtrl WHERE spec_mtrl.FACKID MATCHES aosok 
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE spec_mtrl THEN DO:      
      FIND FIRST spec_mtrl WHERE spec_mtrl.FACKID MATCHES aosok 
      NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE spec_mtrl THEN DO: 
         MESSAGE "Det finns inget på sökbegreppet i depån."
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
      END.
   END.    
   IF AVAILABLE spec_mtrl THEN DO:  
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[2].   
      
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.                         
   APPLY "LEAVE" TO FILL-IN-FACKID2 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO: 
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
   APPLY "LEAVE" TO FILL-IN-FACKID1 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AOTR
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
   IF VALID-HANDLE(inventapph) THEN DELETE PROCEDURE inventapph.
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   
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
   
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      spec_mtrl.EJMOT:VISIBLE IN BROWSE BRW_MTRL2 = TRUE.
   END.
   ELSE spec_mtrl.EJMOT:VISIBLE IN BROWSE BRW_MTRL2 = FALSE.
   visatrum = FALSE.
   IF Guru.Konstanter:varforetypval[57] = 1 THEN DO:
      /*koll in inventering pågår*/
      /*om ett enr finns på trumma, innan inventeringen påbörjas:
        -gör uttag för de trummor som är utplockade på projekt och uppdatera lagersaldo
        - de enr som finns lagda på trumma får inte saldot förändras. Inventerat värde sätts till lagersaldo INVANT = SALDO och KLAR = TRUE*/
        
      /*Trummor som är uttagna ska inte tömmas!!
      enr på truma ska inte inventeras 
      enligt Ingrid E 20200316*/  
      
      /*edatvar = ?.
      RUN kollinv_UI IN inventapph (INPUT Guru.GlobalaVariabler:GuruVdepnr, OUTPUT  edatvar  ).
      IF edatvar = ? THEN DO:
         EMPTY TEMP-TABLE vaotrum NO-ERROR. 
         FIND FIRST aotrum NO-ERROR.
         IF AVAILABLE aotrum THEN DO:
            FOR EACH aotrum  NO-LOCK:
               CREATE vaotrum.
               BUFFER-COPY aotrum TO vaotrum.
            END.
            visatrum = TRUE.
            RUN openbdynspec_UI IN brwproc[3].   
         END.   
         ELSE DO:
            MESSAGE "Inga trummor är för tillfället uttagna -ej återlämnade"
            VIEW-AS ALERT-BOX.
         END.                
         FOR EACH aotrum WHERE  aotrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr :
            EMPTY  TEMP-TABLE eaotrum NO-ERROR.       
            CREATE eaotrum.
            BUFFER-COPY aotrum TO eaotrum.                                               
            ASSIGN
            eaotrum.UTMETER = eaotrum.STMETER
            eaotrum.DATUMIN = TODAY
            eaotrum.STMETER = 0.       
            FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR AND lagertrum.TRUMMA = "S" NO-ERROR.
            IF AVAILABLE lagertrum THEN DO:
               utlop = lagertrum.LOPNR + 1.
               ASSIGN 
               lagertrum.LAGMETER = eaotrum.STMETER
               lagertrum.UTMETER  = lagertrum.UTMETER + eaotrum.UTMETER
               lagertrum.LOPNR  = utlop
               lagertrum.AOKOPP  = FALSE
               lagertrum.LAGERPLATSI = 0
               lagertrum.LAGERPLATSC = "".
            END.
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).         
            
            CREATE lagertrum.
            ASSIGN
            lagertrum.TRUMMANR = eaotrum.TRUMMANR
            lagertrum.TRUMMAPLATS = eaotrum.TRUMMAPLATS
            lagertrum.trumma = "U" + STRING(utlop)
            lagertrum.ENR = eaotrum.ENR 
            lagertrum.BENAMNING = eaotrum.BENAMNING
            lagertrum.ENHET = eaotrum.ENHET      
            lagertrum.UTMETER = eaotrum.UTMETER
            lagertrum.LAGMETER = eaotrum.STMETER 
            lagertrum.AONR = eaotrum.AONR
            lagertrum.DELNR = eaotrum.DELNR
            lagertrum.DATUM = TODAY
            lagertrum.LOPNR  = utlop
            lagertrum.AOKOPP = FALSE
            lagertrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr
            lagertrum.LAGERPLATSI = eaotrum.LAGERPLATSI
            lagertrum.LAGERPLATSC = eaotrum.LAGERPLATSC
            lagertrum.PRIS = eaotrum.PRIS.   
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
            FOR EACH lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR:
               lagertrum.aokopp = FALSE.  
            END.
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
            EMPTY TEMP-TABLE espec_mtrl NO-ERROR. 
            EMPTY TEMP-TABLE off_mtrl NO-ERROR. 
            CREATE espec_mtrl.      
            ASSIGN
            espec_mtrl.ENR = eaotrum.ENR
            espec_mtrl.BENAMNING = eaotrum.BENAMNING
            espec_mtrl.ENHET = eaotrum.ENHET
            espec_mtrl.SALDO = eaotrum.UTMETER.                           
            FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = eaotrum.TRUMMANR AND lagertrum.trumma = "s" NO-ERROR.
            IF AVAILABLE lagertrum THEN DO:
                espec_mtrl.NPRIS = lagertrum.PRIS.
            END.
            /*försök hitta rätt FIFO-post trumma  Lena 20191022*/
            RUN uppmtrldeptr_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT eaotrum.ENR , INPUT eaotrum.UTMETER  ).                      
            RUN bestnrhmt_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT nytt_bestnr).
                          
            RUN okubesttrum_UI IN mtrlbapph (INPUT eaotrum.AONR, INPUT eaotrum.DELNR,INPUT Guru.GlobalaVariabler:GuruVdepnr , INPUT " " ,INPUT nytt_bestnr ,INPUT " ",
                                      INPUT Guru.Konstanter:globanv, INPUT TABLE espec_mtrl , INPUT TABLE off_mtrl).
            /*RUN okubest_UI IN mtrlbapph (INPUT eaotrum.AONR, INPUT eaotrum.DELNR,INPUT Guru.GlobalaVariabler:GuruVdepnr , INPUT " " ,INPUT nytt_bestnr ,INPUT " ",
                                      INPUT Guru.Konstanter:globanv, INPUT TABLE espec_mtrl , INPUT TABLE off_mtrl).*/
            /* om ej mottagen trumma -sätt mottagen och skicka till byggplats*/               
            RUN tillbyggplats_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr ,INPUT TABLE  eaotrum , INPUT utlop ).
            IF AVAILABLE aotrum THEN DELETE aotrum.
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
                                        
         END.
      END.   */   
   END.      
   
   EMPTY TEMP-TABLE depbuff  NO-ERROR. 
   EMPTY TEMP-TABLE spec_mtrl  NO-ERROR. 
   
   RUN mtrlhmt_UI IN inventapph (INPUT vald_depa,OUTPUT TABLE spec_mtrl).
   IF Guru.Konstanter:varforetypval[57] = 1 THEN DO:
      /*koll in inventering pågår*/    
      IF edatvar = ? THEN DO:
         /*om ett enr finns på trumma, innan inventeringen påbörjas:
         -gör uttag för de trummor som är utplockade på projekt och uppdatera lagersaldo
         - de enr som finns lagda på trumma får inte saldot förändras. Inventerat värde sätts till lagersaldo INVANT = SALDO och KLAR = TRUE*/
         EMPTY TEMP-TABLE trspec_mtrl NO-ERROR. 
         FOR EACH lagertrum NO-LOCK:
            FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = lagertrum.ENR  NO-ERROR.
            IF AVAILABLE spec_mtrl THEN DO:
               CREATE trspec_mtrl.
               BUFFER-COPY spec_mtrl TO trspec_mtrl.
               DELETE spec_mtrl.
            END.   
         END.
         FOR EACH trspec_mtrl:
            ASSIGN 
            trspec_mtrl.INVANT = trspec_mtrl.SALDO
            trspec_mtrl.KLAR = TRUE.
         END.   
      END.   
         
   END.      
   RUN dathmt_UI IN inventapph (INPUT vald_depa,OUTPUT datvar).          
               
   FIND depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.
   ASSIGN WINDOW-1:TITLE = "Inventering av depå - " + depatemp.BENAMNING.
   
     
   RUN enable_UI.   
   {FRMSIZE.I}           
   FILL-IN-FACK:HIDDEN = TRUE. 
   ASSIGN colnamevar = "ENR".
   BRW_MTRL2:HIDDEN = FALSE.      
   RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "ENR").
   RUN openbdynspec_UI IN brwproc[2].
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Inventering med startdatum " + string(datvar)).
   RUN title_UI IN brwproc[2].
   FBTN_NOLL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF visatrum = TRUE THEN DO:
      BRW_AOTR:HIDDEN = FALSE.
   END.   
   ELSE BRW_AOTR:HIDDEN = TRUE.
      
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL2 = FALSE.   
      spec_mtrl.SUMMA:VISIBLE IN BROWSE BRW_MTRL2 = FALSE.   
      FILL-IN-TOT:HIDDEN = TRUE.
   END.
   Guru.GlobalaVariabler:collefth = ?.

   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN FBTN_VAR:HIDDEN = TRUE.
   ELSE DO:   
      Guru.GlobalaVariabler:colrighth = FBTN_VAR:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   IF FBTN_NOLL:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_NOLL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   IF depatitta = TRUE THEN DISABLE FBTN_OK WITH FRAME {&FRAME-NAME}.
   {musarrow.i}
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
   Timer = NEW Guru.Timer("kontakt_UI",THIS-PROCEDURE).
   Timer:StartaTimer(300000). /*Interval i milisekuder*/
   Timer:Timer:Tick:Subscribe(Timer:TimerEventRun).   
   /*RELEASE spec_mtrl NO-ERROR.   */
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).      
  RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_AOTR:HANDLE IN FRAME {&FRAME-NAME}).    
         
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN INVENTAPP.P PERSISTENT SET inventapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN INVENTAPP.P PERSISTENT SET inventapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
   RUN addfillin_UI IN brwproc[2] 
      (INPUT FILL-IN-DENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN addfillin_UI IN brwproc[2] 
      (INPUT FILL-IN-DBEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
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
  DISPLAY FILL-IN-TOT FILL-IN-DENR FILL-IN-DBEN FILL-IN-FACKID1 FILL-IN-FACK 
          FILL-IN-FACKID2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-15 IMAGE-4 BRW_MTRL2 FBTN_VAR FBTN_NOLL FBTN_EXCEL FBTN_SKRIV 
         BRW_AOTR FILL-IN-TOT FILL-IN-DENR FILL-IN-DBEN FBTN_SNABB FBTN_OK 
         BTN_AVB FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:
      FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:
         IF inv = FALSE THEN DO:                   
            CREATE tidut. 
            ASSIGN        
            SUBSTRING(tidut.UT,1) = "INVENTERINGSLISTA FÖR DEPÅ"
            SUBSTRING(tidut.UT,28) = depatemp.BENAMNING
            SUBSTRING(tidut.UT,50) = STRING(TODAY)
            SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS").                                                               
            CREATE tidut.
            IF colnamevar = "FACKID" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ FACK-ID".
            END.                   
            IF colnamevar = "ENR" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ " + CAPS(Guru.Konstanter:genk).
            END.                                       
            IF colnamevar = "BENAMNING" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ BENÄMNING".
            END.                  
            CREATE tidut.                                    
            CREATE tidut.
         END.
         ELSE DO: 
            CREATE tidut. 
            ASSIGN        
            SUBSTRING(tidut.UT,1) = "INVENTERADE VÄRDEN FÖR DEPÅ"
            SUBSTRING(tidut.UT,29) = depatemp.BENAMNING
            SUBSTRING(tidut.UT,50) = STRING(TODAY)
            SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS").                                                               
            CREATE tidut.
            IF colnamevar = "FACKID" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ FACK-ID".
            END.                   
            IF colnamevar = "ENR" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ " + CAPS(Guru.Konstanter:genk).
            END.                                       
            IF colnamevar = "BENAMNING" THEN DO:
               SUBSTRING(tidut.UT,1) = "SORTERAT PÅ BENÄMNING".
            END.                  
            CREATE tidut.                                    
            CREATE tidut.
         END.
      END.
   END.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoes_UI WINDOW-1 
PROCEDURE infoes_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_MTRL2" THEN valbrw = 1. 
   IF valbrw = 1 THEN DO:
      status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.           
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE janej2_UI WINDOW-1 
PROCEDURE janej2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
    IF spec_mtrl.KLAR = TRUE THEN spec_mtrl.KLAR = FALSE.
    ELSE spec_mtrl.KLAR = TRUE.    
    DISPLAY spec_mtrl.KLAR WITH BROWSE BRW_MTRL2.          
    RUN refreshbrw_UI IN brwproc[2].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE janej_UI WINDOW-1 
PROCEDURE janej_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
    /*status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.*/
    spec_mtrl.KLAR = TRUE.
    DISPLAY spec_mtrl.KLAR WITH BROWSE BRW_MTRL2.          
    /*RUN refreshbrw_UI IN brwproc[2].*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-1 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
   IF inv = FALSE THEN DO:
      CREATE tidut.
      IF colnamevar = "FACKID" THEN DO: 
         FOR EACH spec_mtrl NO-LOCK USE-INDEX FACKID :                        
            CREATE tidut.      
            ASSIGN
            SUBSTRING(tidut.UT,1) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,20) = spec_mtrl.ENR
            SUBSTRING(tidut.UT,32) = spec_mtrl.BENAMNING                          
            SUBSTRING(tidut.UT,73) = spec_mtrl.ENHET.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = str.                   
         END.         
      END.                                              
      IF colnamevar = "ENR" THEN DO: 
         FOR EACH spec_mtrl NO-LOCK USE-INDEX ENR :        
            CREATE tidut.      
            ASSIGN         
            SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
            SUBSTRING(tidut.UT,23) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,32) = spec_mtrl.BENAMNING                          
            SUBSTRING(tidut.UT,73) = spec_mtrl.ENHET.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = str.             
         END.          
      END.                                             
      IF colnamevar = "BENAMNING" THEN DO: 
         FOR EACH spec_mtrl NO-LOCK USE-INDEX BENAMNING :                         
            CREATE tidut.      
            ASSIGN                              
            SUBSTRING(tidut.UT,1) = spec_mtrl.BENAMNING                    
            SUBSTRING(tidut.UT,52) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,61) = spec_mtrl.ENR               
            SUBSTRING(tidut.UT,73) = spec_mtrl.ENHET.
            CREATE tidut.
            SUBSTRING(tidut.UT,1) = str.               
         END.         
      END.
   END.
   ELSE DO: 
      totalt = 0.      
      IF colnamevar = "FACKID" THEN DO: 
        FOR EACH spec_mtrl NO-LOCK USE-INDEX FACKID :                            
            CREATE tidut.      
            ASSIGN
            SUBSTRING(tidut.UT,1) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,10) = spec_mtrl.ENR
            SUBSTRING(tidut.UT,22) = spec_mtrl.BENAMNING 
            SUBSTRING(tidut.UT,63) = STRING(spec_mtrl.NPRIS,">>>99.99")                          
            SUBSTRING(tidut.UT,72) = spec_mtrl.ENHET
            SUBSTRING(tidut.UT,78) = STRING(spec_mtrl.INVANT)
            SUBSTRING(tidut.UT,88) = STRING(spec_mtrl.NPRIS * spec_mtrl.INVANT,">>>>>>>9")
            totalt = totalt + spec_mtrl.NPRIS * spec_mtrl.INVANT.
            CREATE tidut.           
         END.         
      END.                                              
      IF colnamevar ="ENR" THEN DO: 
         FOR EACH spec_mtrl NO-LOCK USE-INDEX ENR :                             
            CREATE tidut.      
            ASSIGN         
            SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
            SUBSTRING(tidut.UT,13) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,22) = spec_mtrl.BENAMNING                          
            SUBSTRING(tidut.UT,63) = STRING(spec_mtrl.NPRIS,">>>99.99")                          
            SUBSTRING(tidut.UT,72) = spec_mtrl.ENHET
            SUBSTRING(tidut.UT,78) = STRING(spec_mtrl.INVANT)
            SUBSTRING(tidut.UT,88) = STRING(spec_mtrl.NPRIS * spec_mtrl.INVANT,">>>>>>>9")
            totalt = totalt + spec_mtrl.NPRIS * spec_mtrl.INVANT.
            CREATE tidut.            
         END.     
      END.                                             
      IF colnamevar = "BENAMNING" THEN DO: 
         FOR EACH spec_mtrl NO-LOCK USE-INDEX BENAMNING :                               
            CREATE tidut.      
            ASSIGN                              
            SUBSTRING(tidut.UT,1) = spec_mtrl.BENAMNING                    
            SUBSTRING(tidut.UT,42) = spec_mtrl.FACKID  
            SUBSTRING(tidut.UT,51) = spec_mtrl.ENR               
            SUBSTRING(tidut.UT,63) = STRING(spec_mtrl.NPRIS,">>>99.99")                          
            SUBSTRING(tidut.UT,72) = spec_mtrl.ENHET
            SUBSTRING(tidut.UT,78) = STRING(spec_mtrl.INVANT)
            SUBSTRING(tidut.UT,88) = STRING(spec_mtrl.NPRIS * spec_mtrl.INVANT,">>>>>>>9")
            totalt = totalt + spec_mtrl.NPRIS * spec_mtrl.INVANT.
            CREATE tidut.              
         END.       
      END.
   END.                                               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontakt_UI WINDOW-1 
PROCEDURE kontakt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Kontakt IN inventapph NO-ERROR.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-1 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-1 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   IF inv = FALSE THEN DO:
      IF colnamevar = "FACKID" THEN DO:  
         ASSIGN
         str=                                                                    
"========.=========.===========.========================================.=====".                  
         CREATE tidut.      
         ASSIGN  
         SUBSTRING(tidut.UT,1) = "FACK-ID"   
         SUBSTRING(tidut.UT,10) = "INV.ANTAL"
         SUBSTRING(tidut.UT,20) = CAPS(Guru.Konstanter:genk)                                                                       
         SUBSTRING(tidut.UT,32) = "BENÄMNING"
         SUBSTRING(tidut.UT,73) = "ENHET".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.            
      IF colnamevar = "ENR" THEN DO:  
         ASSIGN
         str=                                                                    
"===========.=========.========.========================================.=====".                  
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)
         SUBSTRING(tidut.UT,13) = "INV.ANTAL"          
         SUBSTRING(tidut.UT,23) = "FACK-ID"                                                                        
         SUBSTRING(tidut.UT,32) = "BENÄMNING"
         SUBSTRING(tidut.UT,73) = "ENHET".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.            
      IF colnamevar = "BENAMNING" THEN DO:  
         ASSIGN
         str=                                                                    
"========================================.=========.========.===========.=====".                  
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = "BENÄMNING"
         SUBSTRING(tidut.UT,42) = "INV.ANTAL"
         SUBSTRING(tidut.UT,52) = "FACK-ID"             
         SUBSTRING(tidut.UT,61) = CAPS(Guru.Konstanter:genk)                                                                                 
         SUBSTRING(tidut.UT,73) = "ENHET".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.
   END.
   ELSE DO:
      IF colnamevar = "FACKID" THEN DO:  
         ASSIGN
         str=                                                                    
"========.===========.========================================.========.=====.=========.========".                  
         CREATE tidut.      
         ASSIGN  
         SUBSTRING(tidut.UT,1) = "FACK-ID"            
         SUBSTRING(tidut.UT,10) = CAPS(Guru.Konstanter:genk)                                                                       
         SUBSTRING(tidut.UT,22) = "BENÄMNING" 
         SUBSTRING(tidut.UT,63) = "PRIS"
         SUBSTRING(tidut.UT,72) = "ENHET"
         SUBSTRING(tidut.UT,78) = "INV.ANTAL"
         SUBSTRING(tidut.UT,88) = "VÄRDE".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.            
      IF colnamevar = "ENR" THEN DO:  
         ASSIGN
         str=                                                                    
"===========.========.========================================.========.=====.=========.========".                  
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)                 
         SUBSTRING(tidut.UT,13) = "FACK-ID"                                                                        
         SUBSTRING(tidut.UT,22) = "BENÄMNING"
         SUBSTRING(tidut.UT,63) = "PRIS"
         SUBSTRING(tidut.UT,72) = "ENHET"
         SUBSTRING(tidut.UT,78) = "INV.ANTAL" 
         SUBSTRING(tidut.UT,88) = "VÄRDE".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.            
      IF colnamevar = "BENAMNING" THEN DO:  
         ASSIGN
         str=                                                                    
"========================================.========.===========.========.=====.=========.========".                  
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = "BENÄMNING"         
         SUBSTRING(tidut.UT,42) = "FACK-ID"             
         SUBSTRING(tidut.UT,51) = CAPS(Guru.Konstanter:genk)
         SUBSTRING(tidut.UT,63) = "PRIS"                                                                                 
         SUBSTRING(tidut.UT,72) = "ENHET"
         SUBSTRING(tidut.UT,78) = "INV.ANTAL"
         SUBSTRING(tidut.UT,88) = "VÄRDE".                         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str. 
      END.
   END.
                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetTimer_UI WINDOW-1 
PROCEDURE SetTimer_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* finns redan i main
   IF Guru.Konstanter:appcon = FALSE THEN RETURN.   
   IF TimerStangInte NE ? THEN RETURN.    
   TimerStangInte = NEW Guru.Timer("StangEjAppserver_UI",THIS-PROCEDURE).
   TimerStangInte:StartaTimer(1800000). /*Interval i milisekuder*/
   TimerStangInte:Timer:Tick:Subscribe(TimerStangInte:TimerEventRun).*/ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot_UI WINDOW-1 
PROCEDURE tot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
str = "===============================================================================================".
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,63) = "TOTALT INVENTERAT VÄRDE:"  
   SUBSTRING(tidut.UT,88) = STRING(totalt,">>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,88) = "========". 
        
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
   IF AVAILABLE spec_mtrl THEN DO:
      status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      DISPLAY
      spec_mtrl.SUMMA spec_mtrl.INVANT spec_mtrl.KLAR WITH BROWSE BRW_MTRL2. 
      DISPLAY FILL-IN-TOT WITH FRAME {&FRAME-NAME}.         
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

