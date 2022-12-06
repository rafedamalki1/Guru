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
/*Ändring av Mikael*/
/* DEFINE INPUT PARAMETER valrec AS RECID NO-UNDO. */
DEFINE INPUT PARAMETER typval AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/

{TTEXPIMPK.I}
&SCOPED-DEFINE SHARED SHARED
{OMRTEMPW.I}
{KALKALLTEMP.I}
&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED

DEFINE SHARED TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.
DEFINE TEMP-TABLE eevaldfasttemp NO-UNDO LIKE valdfasttemp.  
{EXTRADATA.I}
DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE eiproch AS HANDLE NO-UNDO.                  /* EXPIMPAPP.P */






DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(78)" NO-UNDO.       
DEFINE VARIABLE tmpattrlist AS CHARACTER FORMAT "X(78)" NO-UNDO. 
DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(78)" NO-UNDO.  
DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE delbernr AS CHARACTER NO-UNDO.

DEFINE VARIABLE varomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar3 AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnuppxml AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE tempomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempkalknr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE listobj AS CHARACTER NO-UNDO.
DEFINE VARIABLE typdator AS INTEGER NO-UNDO.
DEFINE VARIABLE spartypdator AS INTEGER NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO. 
DEFINE STREAM dirstrom.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivaover AS LOGICAL NO-UNDO.
DEFINE VARIABLE skapanyttnummer AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaprochspar AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE avcadaccessh AS HANDLE NO-UNDO.
DEFINE VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbhand AS CHARACTER NO-UNDO.
DEFINE VARIABLE felhand AS LOGICAL NO-UNDO.

DEFINE VARIABLE nyttomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE typ_val AS INTEGER NO-UNDO.
DEFINE VARIABLE anvandarnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE anvandarkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE benamningvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempanv AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE kon_grupp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   FIELD AVVAGNING AS INTEGER LABEL "Avvägning"
   INDEX ID IS PRIMARY ID.
DEFINE TEMP-TABLE tempnamn NO-UNDO
      FIELD NAMNH AS HANDLE
       FIELD TORGTH AS HANDLE
      FIELD NODTAB AS CHARACTER.




/* DEFINE TEMP-TABLE extravaldfasttemp      */
/*    FIELD OMRADE AS CHARACTER         */
/*    FIELD BERNR AS INTEGER            */
/*    FIELD BENAMNING AS CHARACTER      */
/*    INDEX OMR OMRADE BERNR ASCENDING. */
DEFINE TEMP-TABLE valtemp                  
   FIELD OMRADE AS CHARACTER
   FIELD KALKNR AS INTEGER   
   FIELD BENAMNING AS CHARACTER
   FIELD NYTTNR AS INTEGER INITIAL 0
   INDEX OMR OMRADE KALKNR ASCENDING.
DEFINE TEMP-TABLE imptemp NO-UNDO
   FIELD INVAR AS CHARACTER
   FIELD INDIR AS CHARACTER.
DEFINE TEMP-TABLE berfil NO-UNDO
   FIELD INDIR AS CHARACTER
   FIELD FILNAMNS AS CHARACTER
   FIELD ROWBER  AS ROWID
   INDEX ROWBER ROWBER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES extravaldfasttemp

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK extravaldfasttemp.KALKNR ~
extravaldfasttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK 
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH extravaldfasttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH extravaldfasttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK extravaldfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK extravaldfasttemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_BARBAR TOG_KALK TOG_UPP TOG_XML SEL-IMP ~
MBTN_HMT BRW_KALK SEL_XML BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KALKNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR TOG_KALK TOG_UPP TOG_XML SEL-IMP SEL_XML FILL-IN-BER ~
FILL-IN-BER-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_BARBAR      
     LABEL "Bärbar dator" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_HMT  NO-FOCUS
     LABEL "Sök kalkyl" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Välj kalkyl för import" 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KALKNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Kalkyl nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bärbardator", 1,
"Handdator", 2
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE VARIABLE SEL-IMP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 94 BY 16.5 NO-UNDO.

DEFINE VARIABLE SEL_XML AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 52.5 BY 16.5 NO-UNDO.

DEFINE VARIABLE TOG_KALK AS LOGICAL INITIAL no 
     LABEL "Kalkyl" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Kalkylupplägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_XML AS LOGICAL INITIAL no 
     LABEL "Export/import via xmlfiler" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      extravaldfasttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK C-Win _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      extravaldfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
      extravaldfasttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 75 BY 16.5
         TITLE "Välj kalkyl för import" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     FILL-IN-KALKNR AT ROW 6.08 COL 16.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 7.25 COL 16.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 8.38 COL 16.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8.38 COL 38.5 COLON-ALIGNED
     TOG_KALK AT ROW 9.38 COL 28.5
     TOG_UPP AT ROW 10.17 COL 28.5
     TOG_XML AT ROW 10.17 COL 49.5
     SEL-IMP AT ROW 11 COL 2.63 NO-LABEL
     MBTN_HMT AT ROW 7.29 COL 98.5
     BRW_KALK AT ROW 11 COL 12.13
     SEL_XML AT ROW 11 COL 60 NO-LABEL
     BTN_OK AT ROW 28.04 COL 83.5
     BTN_AVB AT ROW 28.04 COL 98.5
     FILL-IN-BER AT ROW 3.63 COL 2.5 NO-LABEL
     FILL-IN-BER-2 AT ROW 4.88 COL 2.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.38 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: extravaldfasttemp T "?" NO-UNDO TEMP-DB extravaldfasttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 28.42
         WIDTH              = 112.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 112.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 112.38
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
/* BROWSE-TAB BRW_KALK MBTN_HMT DEFAULT-FRAME */
ASSIGN 
       BRW_KALK:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BER IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BER-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-KALKNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       MBTN_BARBAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_HMT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       SEL-IMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.extravaldfasttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.extravaldfasttemp.KALKNR
"extravaldfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.extravaldfasttemp.BENAMNING
"extravaldfasttemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av byggnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av byggnummer */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   IF VALID-HANDLE(avcadaccessh) THEN RUN close_UI IN avcadaccessh.
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   {muswait.i}    
      /*
   ASSIGN
   TOG_KALK = INPUT TOG_KALK   
   SEL-IMP = INPUT SEL-IMP
   TOG_UPP = INPUT TOG_UPP.
   TOG_XML = INPUT TOG_XML.
   */
   /*Export*/
   
   IF typval = 1 THEN DO:
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/
      RUN import_UI.
   END.
   {musarrow.i}
   /*OS-DELETE VALUE(fildir).*/
   ASSIGN
   valomrade = valimpomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_BARBAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_BARBAR C-Win
ON CHOOSE OF MBTN_BARBAR IN FRAME DEFAULT-FRAME /* Bärbar dator */
DO:   
   RAD_VAL = 1.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_HMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_HMT C-Win
ON CHOOSE OF MBTN_HMT IN FRAME DEFAULT-FRAME /* Sök kalkyl */
DO:     
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "X(50)".
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   IF TOG_XML = FALSE THEN DO:
      SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj den fil som Ni vill läsa in"
      FILTERS        "All Files (*.d)"  "*.d"   
      INITIAL-DIR    mappvar
      UPDATE OKvald.         
   END.   
   ELSE DO:   
      IF typdator = 1 THEN DO:
         IF TOG_KALK = TRUE THEN DO:
            SYSTEM-DIALOG GET-FILE fildir
            TITLE          "Välj den fil som Ni villläsa in"
            FILTERS        "All Files (*§.xml)"  "*§.xml"   
            INITIAL-DIR    mappvar
            UPDATE OKvald.         
         END.
         ELSE DO:
            SYSTEM-DIALOG GET-FILE fildir
            TITLE          "Välj den fil som Ni villläsa in"
            FILTERS        "All Files (*.xml)"  "*.xml"   
            INITIAL-DIR    mappvar
            UPDATE OKvald.         
         END.
      END.
      ELSE DO:
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den fil som Ni villläsa in"
         FILTERS        "All Files (*.xml)"  "*.xml"   
         INITIAL-DIR    mappvar
         UPDATE OKvald.         
      END.
      IF OKvald = TRUE THEN SEL-IMP:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   END.
   
   IF OKvald = TRUE THEN DO:
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      EMPTY TEMP-TABLE imptemp NO-ERROR. 
      EMPTY TEMP-TABLE berfil NO-ERROR. 
      EMPTY TEMP-TABLE extravaldfasttemp NO-ERROR. 
      RUN in_UI.  
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
/*    RAD_VAL = INPUT RAD_VAL. */
   spartypdator = typdator.
   typdator = RAD_VAL.
   TOG_XML:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   mvar = "*.xml".
   IF typdator = 1 THEN DO:
      ASSIGN      
      BRW_KALK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
      mvar = "*§.xml".
      /*import*/
      IF typval = 2 THEN DO:              
         FILL-IN-BER = "Import från bärbar dator.".
         MBTN_HMT:HIDDEN = FALSE.
         IF TOG_KALK = TRUE THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ELSE SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         TOG_KALK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         /*
         IF Guru.SharedVariable:singel = TRUE THEN DO:
            ASSIGN
            TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
         ELSE DO:
            ASSIGN
            TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
         */
      END.
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till bärbar dator.".
         ASSIGN         
         TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                   
      END.
      RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
      TOG_XML:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
 
   DISPLAY FILL-IN-BER WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-IMP C-Win
ON VALUE-CHANGED OF SEL-IMP IN FRAME DEFAULT-FRAME
DO:
   
   tempnum = SEL-IMP:LOOKUP(SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   tempomr = ENTRY(tempnum, listobj, ";").
   tempkalknr = ENTRY(1, tempomr, ",").
   tempomr = ENTRY(2, tempomr, ",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KALK C-Win
ON VALUE-CHANGED OF TOG_KALK IN FRAME DEFAULT-FRAME /* Kalkyl */
DO:
   TOG_KALK = INPUT TOG_KALK.
   IF typval = 2 THEN DO:
      IF TOG_KALK = TRUE THEN DO:
         ASSIGN
         
         TOG_UPP:HIDDEN = TRUE.
      END.
      ELSE DO:
         ASSIGN
         
         TOG_UPP:HIDDEN = FALSE.
      END.
   END.
   IF (valomrade = ? OR valomrade = "") AND typval = 1 THEN DO:
      MESSAGE "Ingen kalkyl vald.".
      ASSIGN TOG_KALK = FALSE. 

      DISPLAY TOG_KALK WITH FRAME {&FRAME-NAME}.
   END.
   IF typval = 2 THEN DO:
      IF typdator = 1 THEN DO:      
         SEL-IMP:HIDDEN = NOT TOG_KALK.
      END.
      ELSE DO:
         IF TOG_KALK = TRUE THEN DO:
            ASSIGN            
            BRW_KALK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.            
         END.
         ELSE DO:
            ASSIGN            
            BRW_KALK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.          
         END.         
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP C-Win
ON VALUE-CHANGED OF TOG_UPP IN FRAME DEFAULT-FRAME /* Kalkylupplägg */
DO:
   TOG_UPP = INPUT TOG_UPP.
   IF typval = 2 THEN DO:
      IF TOG_UPP = TRUE THEN DO:
         ASSIGN
         TOG_KALK:HIDDEN = TRUE.         
      END.
      ELSE DO:
         ASSIGN
         TOG_KALK:HIDDEN = FALSE.         
      END.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_XML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_XML C-Win
ON VALUE-CHANGED OF TOG_XML IN FRAME DEFAULT-FRAME /* Export/import via xmlfiler */
DO:
   TOG_XML = INPUT TOG_XML.
   IF typval = 2 THEN DO:
      SEL-IMP:LIST-ITEMS = "".          
      RUN in_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
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
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   IF VALID-HANDLE(eiproch) THEN DELETE PROCEDURE eiproch NO-ERROR.   
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.
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
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:globomr = "" THEN DO:
      FIND FIRST omrtemp NO-LOCK NO-ERROR.
      globomr = omrtemp.OMRADE.
   END.
   skrivaover = TRUE.
   IF typval = 1 THEN DO:   
      FILL-IN-BER = "Export till bärbar dator.".
      {&WINDOW-NAME}:TITLE = "Export".
     /*Ändring av Mikael*/
      EMPTY TEMP-TABLE extravaldfasttemp NO-ERROR. 
      tthandle = TEMP-TABLE extravaldfasttemp:HANDLE.
      IF valomrade = "" OR valomrade = ? THEN DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FASTSPEC", INPUT "").
         ASSIGN
         FILL-IN-KALKNR = ?
         FILL-IN-AONR = '?'
         FILL-IN-DELNR = ? 
         FILL-IN-BEN = '?'.  
      END.
      ELSE DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FASTSPEC", INPUT " WHERE OMRADE = '" + valomrade + "'").
         FIND FIRST extravaldfasttemp WHERE extravaldfasttemp.OMRADE = valomrade AND 
         extravaldfasttemp.KALKNR = valkalknr NO-LOCK NO-ERROR.   
         ASSIGN
         FILL-IN-KALKNR = extravaldfasttemp.KALKNR
         FILL-IN-AONR = extravaldfasttemp.AONR
         FILL-IN-DELNR = extravaldfasttemp.DELNR
         FILL-IN-BEN = extravaldfasttemp.BENAMNING.   
      END.      
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Import".
      FILL-IN-BER = "Import från bärbar dator.".     
   END.
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.                                                                     
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI. 
   SEL_XML:HIDDEN = TRUE.
   {FRMSIZE.I}
   ASSIGN
   TOG_KALK = TRUE 
   TOG_UPP = FALSE
   TOG_XML = TRUE 
   TOG_KALK:HIDDEN = TRUE 
   TOG_UPP:HIDDEN = TRUE
   TOG_XML:HIDDEN = TRUE.
   IF Guru.SharedVariable:singel = FALSE THEN DO:
   /*client-server*/
      IF typval = 2 THEN DO:      
         /*import*/
         ASSIGN
         TOG_KALK = TRUE         
         TOG_UPP = FALSE         
         TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_KALK:ROW = 4. 
         DISP  TOG_KALK WITH FRAME {&FRAME-NAME}. 
         APPLY "VALUE-CHANGED" TO TOG_KALK.
      END.
      ELSE DO:
         /*export*/
         
      END.
   END.
   ELSE DO:
      /*singel*/
      IF typval = 1 THEN DO:               
      END.
      ELSE DO:
         /*import*/
         
         ASSIGN     
         TOG_KALK = TRUE.
         /*
         TOG_KALK:ROW = 4.
         ASSIGN 
         TOG_MTRL:ROW = TOG_KALK:ROW
         TOG_UPP:ROW = TOG_KALK:ROW + 1.25.
         */
         DISP TOG_KALK WITH FRAME {&FRAME-NAME}. 
         APPLY "VALUE-CHANGED" TO TOG_KALK.
      END.
   END.
   IF typval = 1 THEN DO:
      ASSIGN
      FILL-IN-BER-2:HIDDEN = TRUE
      MBTN_HMT:HIDDEN = TRUE.
   END.
   IF typval = 2 THEN DO:
      ASSIGN
      FILL-IN-KALKNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-DELNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ASSIGN
   SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   BRW_KALK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   ASSIGN
   typdator = 1
   RAD_VAL = 1.
      /*
   TOG_MTRL:ROW = TOG_UPP:ROW.
   */
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN selfill_UI.
   IF typval = 2 THEN RUN in_UI.  
   
   ASSIGN
   MBTN_BARBAR:COLUMN = 1.
   ASSIGN
   Guru.GlobalaVariabler:collefth = MBTN_BARBAR:HANDLE.
   /*IF MBTN_HANDDATOR:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_HANDDATOR:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   IF MBTN_AVCAD:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_AVCAD:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.*/
   MBTN_BARBAR:LOAD-IMAGE("BILDER\xbtn_barb.gif") NO-ERROR.
   RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   IF typval = 1 THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   ASSIGN
   TOG_KALK = TRUE 
   TOG_UPP = FALSE
   TOG_XML = TRUE 
   TOG_KALK:HIDDEN = TRUE 
   TOG_UPP:HIDDEN = TRUE
   TOG_XML:HIDDEN = TRUE.
   {musarrow.i}
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_KALK:HANDLE IN FRAME {&FRAME-NAME}).   
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "OMRADE").    
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).      
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPAPP.P PERSISTENT SET eiproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPAPP.P PERSISTENT SET eiproch.
   END. 
   tthandle = ?.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI C-Win 
PROCEDURE btnvit_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER btnh AS HANDLE NO-UNDO.
   DEFINE VARIABLE btnlabel AS CHARACTER NO-UNDO.
   IF btnnovit NE ? THEN DO:
      btnlabel = btnnovit:IMAGE.
      btnlabel = REPLACE(btnlabel,"_vit.gif",".gif"). 
      btnnovit:LOAD-IMAGE (btnlabel) NO-ERROR.
   END.
   btnlabel = btnh:IMAGE.
   btnlabel = REPLACE(btnlabel,".gif","_vit.gif"). 
   btnh:LOAD-IMAGE (btnlabel) NO-ERROR.
   btnnovit = btnh.   
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
  DISPLAY FILL-IN-KALKNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR TOG_KALK TOG_UPP 
          TOG_XML SEL-IMP SEL_XML FILL-IN-BER FILL-IN-BER-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR TOG_KALK TOG_UPP TOG_XML SEL-IMP MBTN_HMT BRW_KALK SEL_XML 
         BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export_UI C-Win 
PROCEDURE export_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   filnamn = fildir + STRING(extravaldfasttemp.KALKNR) + "!" + extravaldfasttemp.OMRADE + "!".
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   IF typdator = 1 THEN DO:         
      IF TOG_XML = TRUE THEN DO:
         RUN selxml_UI (INPUT "Kalkylen kontrolleras").   
         RUN expxml_UI.                          
      END.
      ELSE DO:
         
      END.         
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expxml_UI C-Win 
PROCEDURE expxml_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_KALK = TRUE THEN DO:
      RUN selxml_UI (INPUT "Kalkylen sammanställs").                       
      RUN skapaexport_UI IN eiproch (INPUT valkalknr, INPUT valomrade,      
                                        OUTPUT TABLE temp_kalk,   
                                        OUTPUT TABLE tempKALKBEF,   
                                        OUTPUT TABLE tempFAKTOR,  
                                        OUTPUT TABLE tempFASTKALK,    
                                        OUTPUT TABLE tempmtrl,
                                        OUTPUT TABLE tempKALKFOR, 
                                        OUTPUT TABLE tempKALKUPP,
                                        OUTPUT TABLE tempKALKKUND,  
                                        OUTPUT TABLE tempKALKAONR). 
      
   END.   
  
   IF TOG_UPP = TRUE THEN DO:           
      RUN selxml_UI (INPUT "Kalkylupplägget sammanställs").    
      RUN expupplagg_UI IN eiproch (OUTPUT TABLE p1temp,   
                                        OUTPUT TABLE p2temp,    
                                        OUTPUT TABLE p3temp,      
                                        OUTPUT TABLE p5temp,  
                                        OUTPUT TABLE lop1temp,      
                                        OUTPUT TABLE lop2temp,       
                                        OUTPUT TABLE lop3temp,      
                                        OUTPUT TABLE lop5temp,                                             
                                        OUTPUT TABLE kalkbeftemp,
                                        OUTPUT TABLE frekvenstemp,
                                        OUTPUT TABLE ebrpristemp,
                                        OUTPUT TABLE sebrpristemp).
      
   END.
   
   RUN utinxml_UI (INPUT 1).
   IF TOG_KALK = TRUE THEN MESSAGE "Kalkylen är exporterad till "  filnamn VIEW-AS ALERT-BOX.      
   
   IF typdator = 1 THEN DO:
      varforetypchar[50] = "".
      IF TOG_KALK = TRUE THEN DO:
         MESSAGE "Vill du skicka kalkylen som E-post?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
         CASE svar:
            WHEN TRUE THEN DO:
               varforetypchar[50] = filnamn.               
            END.
            WHEN FALSE THEN DO:
               filnamn = "".               
            END.
         END CASE.
      END.
      
      IF varforetypchar[50] NE ""  THEN DO:
         /*RUN EMEDD.W (INPUT FALSE).*/
         DEFINE VARIABLE infiler AS CHARACTER NO-UNDO.
         /*
         IF TOG_KALK = TRUE AND TOG_UPP = TRUE THEN infiler = filnamn + ", " + filnamnuppxml.
         */
         IF TOG_KALK = TRUE THEN infiler = filnamn.
         /*
         ELSE IF TOG_MTRL = TRUE OR TOG_UPP = TRUE THEN infiler = filnamnuppxml.*/
         
         DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
         file-info:file-name = ".".
         orgdir = file-info:full-pathname.

         RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT infiler).
         RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
      END.
      varforetypchar[50] = "".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import_UI C-Win 
PROCEDURE import_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE finnsber AS LOGICAL NO-UNDO.
   DEFINE VARIABLE finnsomr AS LOGICAL NO-UNDO.
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   IF typdator = 1 AND TOG_XML = TRUE THEN DO:        
      
      EMPTY TEMP-TABLE valtemp NO-ERROR.             
      IF typdator = 2 THEN DO:
         ASSIGN
         antal_valda = BRW_KALK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Du har inte valt någon kalkyl." VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN NO-APPLY.
         END.
         antal_raknare = 1.         
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_KALK:FETCH-SELECTED-ROW(antal_raknare).         
            CREATE valtemp.
            BUFFER-COPY extravaldfasttemp TO valtemp.
            antal_raknare = antal_raknare + 1.  
         END.   
      END.
      ELSE DO:
         IF SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? AND TOG_KALK = TRUE THEN DO: 
            MESSAGE "Du har inte valt någon kalkyl." VIEW-AS ALERT-BOX
            TITLE "Meddelande".
            IF TOG_UPP = TRUE THEN.
            ELSE RETURN NO-APPLY.
         END.
         
         
         FIND FIRST extravaldfasttemp WHERE STRING(extravaldfasttemp.KALKNR) = tempkalknr AND  extravaldfasttemp.OMRADE = tempomr 
         NO-ERROR.
         IF AVAILABLE extravaldfasttemp THEN DO:
            CREATE valtemp.
            BUFFER-COPY extravaldfasttemp TO valtemp.
            filnamn = fildir + tempkalknr + "!" + tempomr + "!" + extravaldfasttemp.BENAMNING.
         END.                 
      END.
      FIND FIRST valtemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valtemp THEN DO:
         MESSAGE "Import av kalkyl avbruten!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         SEL_XML:HIDDEN = TRUE.
         RETURN.
      END.
      IF AVAILABLE valtemp THEN DO:
         EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
         /*Finns beredningen? inköpsprotkoll*/
         IF valtemp.KALKNR = 0 THEN DO:
            skapanyttnummer = TRUE.
         END.
         ELSE DO:
            RUN typval3_UI IN eiproch (INPUT valtemp.KALKNR,INPUT valtemp.OMRADE,OUTPUT TABLE felmeddtemp).
            FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               IF felmeddtemp.FELMEDD = "" THEN DO:
                  IF felmeddtemp.VAL = 1 THEN DO:
                     ASSIGN valtemp.NYTTNR = valtemp.KALKNR.
                  END.
               END.
               ELSE DO:
                  MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar.         
               END.
            END.
            
            IF svar THEN DO:
               
               IF Guru.Konstanter:appcon THEN RUN DELFKALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT valtemp.KALKNR).                  
               ELSE RUN DELFKALK.P (INPUT valtemp.KALKNR).                  
               RUN tabortfastspec_UI IN eiproch (INPUT valtemp.KALKNR).      
               valtemp.NYTTNR = valtemp.KALKNR.
            END. 
            ELSE DO:
               skapanyttnummer = FALSE.
               
               valtemp.OMRADE = globomr.

               MESSAGE "Vill Ni skapa en ny kalkylen?"  SKIP
                       "Den kommer att hamna på " + gomrk + " " + valtemp.OMRADE + "."
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ny kalkyl?" UPDATE svar.         
               IF svar THEN DO:
                  skapanyttnummer = TRUE.
                  /*NYTT BEREDNINGSNUMMER*/              
               END.
               ELSE DO:
                  MESSAGE "Import av kalkyl " + STRING(valtemp.KALKNR) + " avbruten!"
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  DELETE valtemp.
                  SEL_XML:HIDDEN = TRUE.
                  RETURN NO-APPLY.
               END.
            END.                        
         END.
         
      END.            
      RUN utinxml_UI (INPUT 2).                 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inxml_UI C-Win 
PROCEDURE inxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE VARIABLE inber AS INTEGER NO-UNDO.
   DEFINE VARIABLE inomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inbenamning AS CHARACTER NO-UNDO.
   DEFINE VARIABLE h1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE h2 AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE extravaldfasttemp NO-ERROR. 
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   mvarhj = REPLACE(mvar,"*","").
   ASSIGN
   listobj = ""
   tempstring  = ""
   tmpfilnamn = "".
   IF fildir = "" THEN RETURN.
   
   INPUT STREAM dirstrom FROM OS-DIR(fildir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
      SET STREAM dirstrom tmpfilnamn tmpdirlist tmpattrlist NO-ERROR.
      IF tmpfilnamn MATCHES mvar THEN DO:
         
         IF INDEX(tmpfilnamn,"!",1) > 0 THEN DO:
            inber = INTEGER(SUBSTRING(tmpfilnamn,1,INDEX(tmpfilnamn,"!",1) - 1)).            
            IF INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1) > 0 THEN DO: 
               IF listobj = "" THEN listobj = ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               ELSE listobj = listobj + ";" + ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               h1 = INDEX(tmpfilnamn,"!",1) + 1.
               h2 = INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1)).
               inomrade = SUBSTRING(tmpfilnamn,h1,h2 - h1).
               inbenamning = SUBSTRING(tmpfilnamn,h2 + 1,INDEX(tmpfilnamn,mvarhj) - (h2 + 1)).
               IF inbenamning NE "" THEN DO:
                  CREATE extravaldfasttemp.
                  ASSIGN
                  /*extravaldfasttemp.KALKNR = STRING(inber)*/
                  extravaldfasttemp.OMRADE = inomrade
                  extravaldfasttemp.KALKNR = inber
                  extravaldfasttemp.BENAMNING = inbenamning.
                  IF extravaldfasttemp.OMRADE = "" THEN extravaldfasttemp.OMRADE = globomr.
                  tempstring = "Kalknr: " + STRING(extravaldfasttemp.KALKNR) + " " + gomrk + ": " + extravaldfasttemp.OMRADE + 
                  " Benm: " + extravaldfasttemp.BENAMNING.                    
                  FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE imptemp THEN DO:
                     CREATE imptemp.
                     ASSIGN
                     imptemp.INDIR = fildir
                     imptemp.INVAR = tempstring.
                     SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
                  END.
                  /*
                  RUN omrkoll_UI IN expimpapph (INPUT valdfasttemp.OMRADE,OUTPUT musz).
                  IF musz = TRUE THEN musz = FALSE.
                  ELSE valdfasttemp.OMRADE = globomr.
                  */
                  
                  CREATE berfil.
                  ASSIGN
                  berfil.INDIR = fildir
                  berfil.FILNAMNS = tmpfilnamn
                  berfil.ROWBER = ROWID(extravaldfasttemp).
               END.
            END.
         END.
      END.
   END.
   INPUT STREAM dirstrom CLOSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE in_UI C-Win 
PROCEDURE in_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   IF TOG_XML = FALSE THEN DO:
      
   END.
   ELSE DO:
      RUN inxml_UI.          
   END.
   DISPLAY SEL-IMP WITH FRAME {&FRAME-NAME}. 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selfill_UI C-Win 
PROCEDURE selfill_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      fildir = "\\SRV00486\APPL\GURU\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CLULE" THEN DO:
      fildir = "\\195.196.19.21\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
      fildir = "\\PC112\DELAD\dokument\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "STRA" THEN DO:
      fildir = "\\srv07web\DELAD\Gemensam bärbart\" + globanv + "\".
   END.
   
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   IF OS-ERROR NE 0  THEN DO:
     
      fildir = webclienttempdir.
      mappvar = webclienttempdir.
   END.    
   ELSE mappvar = fildir.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selxml_UI C-Win 
PROCEDURE selxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   SEL_XML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   SEL_XML:ADD-LAST(txtvar + " " + STRING(TIME,"HH:MM:SS")).
   DISPLAY SEL_XML WITH FRAME {&FRAME-NAME}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utinxml_UI C-Win 
PROCEDURE utinxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynxml AS HANDLE NO-UNDO.
   DEFINE VARIABLE sparfilnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE okand AS LOGICAL NO-UNDO.
   IF typdator = 1 THEN DO:
      okand = TRUE.
   END.
   
   mvarhj = REPLACE(mvar,"*","").
   RUN selfill_UI.
   filnamnuppxml = fildir + "kalk.xml".
   {UTXKMLKOLL.I}
   RUN VALUE(vxmlpro) PERSISTENT SET dynxml (INPUT okand).
   EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   IF vad = 1 THEN DO:
      /*KALKÅR*/
      IF TOG_KALK = TRUE THEN DO:
         RUN uttab_UI (INPUT 1).
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         IF typdator = 1 THEN filnamn = fildir + STRING(extravaldfasttemp.KALKNR) + "!" + extravaldfasttemp.OMRADE + "!" + extravaldfasttemp.BENAMNING + mvarhj .
         filnamn = REPLACE(filnamn,",","").
         /*IF typdator = 2 THEN IF typdator = 1 THEN filnamn = fildir + valdfasttemp.KALKNR + "!" + valdfasttemp.OMRADE + "!" + extravaldfasttemp.BENAMNING + mvarhj.*/
         sparfilnamn = filnamn.
         RUN selxml_UI (INPUT "Kalkylfilen skapas").
         FOR EACH tempnamn:
            IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
            ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "", INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
            sparfilnamn = "".
         END.
         RUN xmlutslut_UI IN dynxml.
         RUN xmlslut_UI IN dynxml.
         /*IF typdator = 2 THEN DO:
            RUN selxml_UI (INPUT "Beredningen kopieras till handdator").
            RUN ut_UI (INPUT "berToPDA").         
         END.*/
      END.
      EMPTY TEMP-TABLE tempnamn NO-ERROR.    
      sparfilnamn = filnamnuppxml.
      IF TOG_KALK = TRUE THEN RUN creatnod_UI IN dynxml.
      IF TOG_UPP = TRUE THEN RUN uttab_UI (INPUT 2).      
      IF TOG_UPP = TRUE  THEN DO:
         RUN selxml_UI (INPUT "Kalkyluppläggets filen skapas").         
         FOR EACH tempnamn:
            IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
            ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
            sparfilnamn = "".
         END.
         RUN xmlutslut_UI IN dynxml.
         RUN xmlslut_UI IN dynxml.
         EMPTY TEMP-TABLE tempnamn NO-ERROR. 
         /*IF typdator = 2 THEN DO:
            RUN selxml_UI (INPUT "Materielkatalog och/eller Konstruktionsuppläggets kopieras till handdator").         
            filnamn = filnamnuppxml.
            RUN ut_UI (INPUT "ulToPDA").         
            filnamnuppxml = REPLACE(filnamnuppxml,"\\","\").
         END.*/
         
      END.
      RUN selxml_UI (INPUT "Kopieringen färdig").               
   END.
   IF vad = 2 THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.
      ELSE DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar.      
      END.
      IF TOG_KALK = TRUE THEN DO:
         /*KALKÅR*/
         RUN uttab_UI (INPUT 1).      
         RUN selxml_UI (INPUT "Kalkyl läses in i Guru").
         FIND FIRST berfil WHERE berfil.ROWBER = ROWID(extravaldfasttemp) NO-LOCK NO-ERROR.
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT berfil.INDIR + berfil.FILNAMNS).
         FIND FIRST temp_kalk NO-LOCK NO-ERROR.
         FIND FIRST valtemp NO-LOCK NO-ERROR.
         IF skapanyttnummer = TRUE THEN DO:
            RUN skapanykalk_UI IN eiproch (INPUT valtemp.OMRADE,OUTPUT nyttnr). 
            IF nyttnr = ? THEN DO:
               MESSAGE "Det går inte att lägga upp kalkyler på detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller är fylld." VIEW-AS ALERT-BOX.
            END.
            valtemp.NYTTNR = nyttnr.
         END.
         FIND FIRST eevaldfasttemp NO-LOCK NO-ERROR.
         IF AVAILABLE eevaldfasttemp THEN DO:
            FOR EACH temp_kalk NO-LOCK:
               BUFFER-COPY eevaldfasttemp TO temp_kalk.
            END.
         END.
         ELSE DO:
            ASSIGN
            temp_kalk.AKTIV = TRUE
            temp_kalk.ANVANDARE = Guru.Konstanter:globanv
            temp_kalk.AONR = ?
            temp_kalk.BENAMNING = "Import handdator".
            /*temp_kalk.KALKYL = FALSE */
               /*
            temp_kalk.KATAR = ?.
            */
            IF typdator = 1 THEN DO:
               temp_kalk.BENAMNING = "Import via xml-fil " + extravaldfasttemp.BENAMNING.
            END.
         END.
         FOR EACH temp_kalk:
            ASSIGN
            temp_kalk.OMRADE = valtemp.OMRADE
            temp_kalk.KALKNR  = valtemp.NYTTNR.
            
            IF temp_kalk.AONR = "" THEN temp_kalk.AONR = ?.
         END.
         RUN selxml_UI (INPUT "Kalkyl sparas i Guru").
         
         FIND FIRST tempnamn WHERE tempnamn.NODTAB = "FASTSPEC" NO-LOCK NO-ERROR.
         RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
         DELETE tempnamn.
         IF skapanyttnummer = TRUE THEN DO:
            MESSAGE "Ditt nya kalkylnummer är: " + gomrk + " " + valtemp.OMRADE + " " + STRING(nyttnr)
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
            MESSAGE "OBS! Om du importerar en kalkyl från annat företag KOPIERA ALLTID KALKYLEN TILL RÄTT ÅRTAL! Oavsätt vad som står på kalkylen."
            VIEW-AS ALERT-BOX.        
         END.
         {muswait.i}      
       
         FOR EACH temp_kalk:
            ASSIGN
            temp_kalk.OMRADE = valtemp.OMRADE 
            temp_kalk.KALKNR =  valtemp.NYTTNR.             
         END.
         FOR EACH tempKALKBEF:
            ASSIGN
            tempKALKBEF.OMRADE = valtemp.OMRADE 
            tempKALKBEF.KALKNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempFAKTOR:
            ASSIGN
            tempFAKTOR.OMRADE = valtemp.OMRADE 
            tempFAKTOR.KALKNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempFASTKALK:
            ASSIGN
            tempFASTKALK.OMRADE = valtemp.OMRADE 
            tempFASTKALK.KALKNR =  valtemp.NYTTNR.            
         END.
         FOR EACH tempmtrl:
            ASSIGN            
            tempmtrl.KALKNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempKALKFOR:
            ASSIGN
            /*tempKALKFOR.OMRADE = valtemp.OMRADE */
            tempKALKFOR.KALKNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempKALKUPP:
            ASSIGN
            /*tempKALKUPP.OMRADE = valtemp.OMRADE */
            tempKALKUPP.KALKNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempKALKKUND:
            ASSIGN
            /*tempKALKKUND.OMRADE = valtemp.OMRADE */
            tempKALKKUND.KALKNR = valtemp.NYTTNR. 
         END.
         FOR EACH tempKALKAONR:
            ASSIGN
            tempKALKAONR.OMRADE = valtemp.OMRADE 
            tempKALKAONR.KALKNR =  valtemp.NYTTNR. 
         END.
         
         ASSIGN
         valimpomrade = valtemp.OMRADE
         valkalknr =  valtemp.NYTTNR.
         /*DUMMY*/
         FOR EACH tempnamn:
            RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
         END.
         RUN xmlslut_UI IN dynxml.
         RUN selxml_UI (INPUT "Inläsning av kalkyl klar").
         EMPTY TEMP-TABLE tempnamn NO-ERROR.    
      END.
      IF typdator = 1 THEN DO:
         /*KALKÅR*/
         IF TOG_KALK = TRUE THEN RUN creatnod_UI IN dynxml.
         IF TOG_UPP = TRUE THEN DO:
            RUN uttab_UI (INPUT 2).
            RUN delupplagg_UI IN eiproch.
         END.         
         IF TOG_UPP = TRUE /*OR TOG_MTRL = TRUE*/ THEN DO:
            RUN selxml_UI (INPUT "Kalkylupplägget läses in i Guru").         
            RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
            RUN xmlslut_UI IN dynxml.
            
            RUN selxml_UI (INPUT "Kopieringen färdig").               
         END.
      END.
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   END.
   IF VALID-HANDLE(laddaprochspar) THEN DELETE PROCEDURE laddaprochspar NO-ERROR.   
   IF VALID-HANDLE(dynxml) THEN DELETE PROCEDURE dynxml.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uttab_UI C-Win 
PROCEDURE uttab_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
          
            
             
             
   IF vad = 1 THEN DO:
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER temp_kalk:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE temp_kalk:HANDLE.
      tempnamn.NODTAB = "FASTSPEC".
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempKALKBEF:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempKALKBEF:HANDLE.
      tempnamn.NODTAB = "KALKBEF".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempFAKTOR:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempFAKTOR:HANDLE.
      tempnamn.NODTAB = "FAKTOR".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempFASTKALK:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempFASTKALK:HANDLE.
      tempnamn.NODTAB = "FASTKALK".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrl:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempmtrl:HANDLE.
      tempnamn.NODTAB = "MTRL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempKALKFOR:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempKALKFOR:HANDLE.
      tempnamn.NODTAB = "KALKFOR".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempKALKUPP:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempKALKUPP:HANDLE.
      tempnamn.NODTAB = "KALKUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempKALKKUND:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempKALKKUND:HANDLE.
      tempnamn.NODTAB = "KALKKUND".
      
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempKALKAONR:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempKALKAONR:HANDLE.
      tempnamn.NODTAB = "KALKAONR".
      
   END.
   IF vad = 2 THEN DO:
         
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER p1temp:HANDLE.      
      tempnamn.TORGTH = TEMP-TABLE p1temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER p2temp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE p2temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER p3temp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE p3temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER p5temp:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE p5temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER lop1temp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE lop1temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER lop2temp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE lop1temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER lop3temp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE lop1temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER lop5temp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE lop1temp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER kalkbeftemp:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE kalkbeftemp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER frekvenstemp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE frekvenstemp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER ebrpristemp:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE ebrpristemp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER sebrpristemp:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE sebrpristemp:HANDLE.
      tempnamn.NODTAB = tempnamn.NAMNH:NAME.
      
   END.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI C-Win 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      korfil = SEARCH("wguru.exe").
      korfil = REPLACE(korfil,guruvar,"..\"). 
   END.
   ELSE DO:
      /*
      korfil = SEARCH("pget.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SEARCH("pput.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      */
      korfil = SEARCH("wguru.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SESSION:TEMP-DIRECTORY + "wguru.exe". 
   END.      
   filnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
   filnamn = REPLACE(filnamn,"\","\\").

   dbhand = REPLACE(SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv,"\","\\").
   korfil = korfil + ' "' + txtvar + '"' + ' "' + filnamn + '"' + ' "' + dbhand + '"'.
   OS-COMMAND SILENT VALUE(korfil).      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

