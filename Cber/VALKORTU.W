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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}


{BEREDNINGTEMP.I}
&Scoped-define SHARED SHARED
{BBENAMNTEMP.I}

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{KONVALTEMP.I}
{KONID.I}   
{LISTMTRL.I}   
{KOPPLINA.I}    
{FRITEMP.I}        
{BERSKAP.I}
{MARKGRUPP.I}
{KORTTMP.I}
DEFINE NEW SHARED VARIABLE lin_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE liggande AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sida AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE kon_rowid AS ROWID NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.   
DEFINE VARIABLE datvar2 AS DATE NO-UNDO.
DEFINE VARIABLE valkortapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE frikortapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numhj AS INTEGER NO-UNDO.
DEFINE VARIABLE mall AS INTEGER NO-UNDO.
DEFINE VARIABLE visadkm AS CHARACTER NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kon_val

/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val.ID2 kon_val.EXTRA1 ~
kon_val.F1 kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 kon_val.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_SORT CMB_KORTVAL BRW_VAL BTN_VISA ~
BTN_EXMARKN BTN_MEXC BTN_AVB FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR 
&Scoped-Define DISPLAYED-OBJECTS TOG_SORT CMB_KORTVAL FILL-IN-BERNR ~
FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXKSKO 
     LABEL "Kort till exel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXMARK 
     LABEL "Markstationskort LSP" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXMARKN 
     LABEL "Märkning exel" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXSTOLP 
     LABEL "Stolpstationskort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MEXC 
     LABEL "Skyltmall Exel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_KORTVAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj korttyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SKYLT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Enbart Elnätsskyltar", 1,
"Enbart övriga skyltar", 2,
"Alla skyltar", 3
     SIZE 37.5 BY .96 NO-UNDO.

DEFINE VARIABLE TOG_SORT AS LOGICAL INITIAL no 
     LABEL "För excel kort sortera efter" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VAL FOR 
      kon_val SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val.ID2 FORMAT "X(10)":U
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U
      kon_val.F2 FORMAT "X(10)":U
      kon_val.F3 FORMAT "X(10)":U
      kon_val.F4 FORMAT "X(10)":U
      kon_val.F5 FORMAT "X(10)":U
      kon_val.F6 FORMAT "X(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 92 BY 22.75 TOOLTIP "Obs! Det går att markera flera kabelskåpskort och få ut dem som flikar i excel. (gäller ej word))".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_SKYLT AT ROW 2.92 COL 71.63 NO-LABEL
     TOG_SORT AT ROW 3.92 COL 76.88
     CMB_KORTVAL AT ROW 5.04 COL 74.63 COLON-ALIGNED
     BRW_VAL AT ROW 6.25 COL 1.5
     BTN_VISA AT ROW 6.25 COL 94.5
     BTN_EXKSKO AT ROW 7.33 COL 94.5
     BTN_EXMARKN AT ROW 8.42 COL 94.5
     BTN_EXSTOLP AT ROW 8.46 COL 94.5
     BTN_MEXC AT ROW 9.5 COL 94.5
     BTN_EXMARK AT ROW 9.54 COL 94.5
     BTN_AVB AT ROW 28 COL 94.5
     FILL-IN-BERNR AT ROW 2.83 COL 14.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.96 COL 14.25 COLON-ALIGNED
     FILL-IN-AONR AT ROW 5.04 COL 14.25 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 5.04 COL 36.25 COLON-ALIGNED
     "Obs! Det går att markera flera kabelskåpskort och få ut dem som flikar i excel.  (gäller ej word)" VIEW-AS TEXT
          SIZE 100 BY 1 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.75 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: list_mtrl2 T "?" NO-UNDO temp-db list_mtrl2
      TABLE: spec_mtrl T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kabelskåpskort/Markstationskort"
         HEIGHT             = 28.42
         WIDTH              = 108.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 108.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 108.88
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
/* BROWSE-TAB BRW_VAL CMB_KORTVAL DEFAULT-FRAME */
ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

/* SETTINGS FOR BUTTON BTN_EXKSKO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXKSKO:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXMARK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXMARK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_EXMARKN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXSTOLP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXSTOLP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_SKYLT IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_SKYLT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.kon_val.F2
     _FldNameList[5]   = Temp-Tables.kon_val.F3
     _FldNameList[6]   = Temp-Tables.kon_val.F4
     _FldNameList[7]   = Temp-Tables.kon_val.F5
     _FldNameList[8]   = Temp-Tables.kon_val.F6
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kabelskåpskort/Markstationskort */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kabelskåpskort/Markstationskort */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON MOUSE-MENU-CLICK OF BRW_VAL IN FRAME DEFAULT-FRAME
DO:                                                                
/*    {muswait.i}  */
/*    RUN info_UI. */
/*    {musarrow.i} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RUN vcbrwval_UI.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:    
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXKSKO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXKSKO C-Win
ON CHOOSE OF BTN_EXKSKO IN FRAME DEFAULT-FRAME /* Kort till exel */
DO:   
   CMB_KORTVAL = INPUT CMB_KORTVAL.
   IF CMB_KORTVAL = "Kabelskåpskort Allmän" THEN mall = 6.   /*FLIKAR*/
      
   IF CMB_KORTVAL = "Magnifix Vattenfall" THEN mall = 15.  /*ny*/
   IF CMB_KORTVAL = "Xiria Vattenfall" THEN mall = 17.  /*ny*/ 
   IF CMB_KORTVAL = "Stolpstationskort Vattenfall" THEN mall = 18.
   
   IF CMB_KORTVAL = "ABB Nstn Vattenfall" THEN mall = 12.
   IF CMB_KORTVAL = "Rörlask Vattenfall" THEN mall = 13.  
   IF CMB_KORTVAL = "KL Rörlask Vattenfall" THEN mall = 14.
      

   IF CMB_KORTVAL = "Kabelskåpskort Fortum EON" THEN mall = 23.  /*FLIKAR*/
   IF CMB_KORTVAL = "Kabelskåpskort Vattenfall" THEN mall = 25.  /*FLIKAR*/
   IF CMB_KORTVAL = "Markstationskort Allmän" THEN mall = 27.
   IF CMB_KORTVAL = "Nätstationskort EON Vatt" THEN mall = 28.   
   IF CMB_KORTVAL = "Kabelskåpskort One" THEN mall = 31.   /*FLIKAR*/
   IF CMB_KORTVAL = "Kabelskåpskort Linjemontage" THEN mall = 31.   /*FLIKAR*/
    IF CMB_KORTVAL = "unerRing Vattenfall" THEN mall = 32.   /*FLIKAR*/
    IF CMB_KORTVAL = "Kabelskåpskort Kraftringen2" THEN mall = 35.
   IF CMB_KORTVAL = "Kabelskåpskort Kraftringen" THEN mall = 36.
   IF CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Ellevio " THEN mall = 43.
   IF CMB_KORTVAL:SCREEN-VALUE = "Markstationskort Ellevio " THEN mall = 42.
   IF CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort C4Elnät " THEN mall = 50.
   
   
   
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.   

   IF CMB_KORTVAL = "Stolpstationskort Vattenfall"   
   OR CMB_KORTVAL = "ABB Nstn Vattenfall" OR CMB_KORTVAL = "Magnifix Vattenfall"
    OR CMB_KORTVAL = "Xiria Vattenfall" OR CMB_KORTVAL = "Markstationskort Allmän" OR CMB_KORTVAL = "unerRing Vattenfall"   THEN DO:   
      antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      IF antal_valda > 1 THEN DO:
         MESSAGE "För stolpstation och markstation kan man bara ta ut en i taget till excel."  VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      numhj = kon_val.NUM.
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                     
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.
         IF TOG_SORT:HIDDEN = TRUE THEN TOG_SORT = FALSE.
         IF mall = 19 OR mall = 20 OR mall = 21 OR mall = 28 THEN DO:
            RUN WOKSKKORTU.P (INPUT mall,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                              INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
         ELSE DO:   
            RUN EXKSKKORTU.P (INPUT TOG_SORT,INPUT mall,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                           INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
         /*GGG*/
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val WHERE kon_val.NUM = numhj AND kon_val.KSKAP = FALSE NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
   END.   
   
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXMARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXMARK C-Win
ON CHOOSE OF BTN_EXMARK IN FRAME DEFAULT-FRAME /* Markstationskort LSP */
DO:
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                     
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.    
         IF TOG_SORT:HIDDEN = TRUE THEN TOG_SORT = FALSE.  
         IF mall = 19 OR mall = 20 OR mall = 21 OR mall = 28 THEN DO:
            RUN WOKSKKORTU.P (INPUT mall,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                              INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
         ELSE DO:   
            RUN EXKSKKORTU.P (INPUT TOG_SORT,INPUT 2,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                           INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
      /*GGG*/
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
   END.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXMARKN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXMARKN C-Win
ON CHOOSE OF BTN_EXMARKN IN FRAME DEFAULT-FRAME /* Märkning exel */
DO:
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                     
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.      
         
         RUN VFRIMARK.W.
         
      /*GGG*/
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
   END.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXSTOLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXSTOLP C-Win
ON CHOOSE OF BTN_EXSTOLP IN FRAME DEFAULT-FRAME /* Stolpstationskort */
DO:
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                     
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.    
         IF TOG_SORT:HIDDEN = TRUE THEN TOG_SORT = FALSE.  
         IF mall = 19 OR mall = 20 OR mall = 21 OR mall = 28 THEN DO:
            RUN WOKSKKORTU.P (INPUT mall,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                              INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
         ELSE DO:   
            RUN EXKSKKORTU.P (INPUT TOG_SORT,INPUT 3,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                           INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
      /*GGG*/
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
   END.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MEXC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MEXC C-Win
ON CHOOSE OF BTN_MEXC IN FRAME DEFAULT-FRAME /* Skyltmall Exel */
DO: 
   {muswait.i}
    
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                     
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.      
         IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "gran"  OR Guru.Konstanter:globforetag = "FORS"  THEN DO:
             RUN EXKSKMAES.P (INPUT 1, INPUT RAD_SKYLT).
         END.
         ELSE IF Guru.Konstanter:globforetag = "SKOK"  OR Guru.Konstanter:globforetag = "KEKR" THEN DO:
             /*tänkt som allmän skyltmall*/
             RUN EXKSKMAESAL.P (INPUT 1, INPUT 3).
         END.     
         ELSE DO:
            RUN ESSKYLTMK.P (INPUT 1,INPUT TABLE list_mtrl, INPUT TABLE kon_id,INPUT TABLE fri_temp,
                           INPUT TABLE kon_val,INPUT TABLE konstvaltemp ,INPUT TABLE berskaptemp).
         END.
            
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
   END.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "gran" THEN DO:
          RUN EXKSKMAES.P (INPUT 1, INPUT RAD_SKYLT).
      END.     
   END.
   {AVBFRAM.I} 
   
   /*RUN ESSKYLTMALL.P (INPUT TABLE mtrl_temp, INPUT prisvar, 
   INPUT valaonr, INPUT valomrade, INPUT valort, INPUT edmedd).*/


   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA C-Win
ON CHOOSE OF BTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   
   {muswait.i}               
   FIND FIRST kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      numhj = kon_val.NUM.
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      lin_rowid = ROWID(kon_id).                  
      IF musz = TRUE THEN musz = FALSE.   
      ASSIGN    
      skrivut = FALSE.      
      {AVBGOM.I}
      FIND FIRST fri_temp WHERE fri_temp.NUM = kon_val.NUM NO-LOCK NO-ERROR.
      IF AVAILABLE fri_temp THEN DO:
         /*GGG*/
         DEFINE VARIABLE numspar AS INTEGER NO-UNDO.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR.
         ASSIGN
         numspar = 0
         antal_valda = BRW_VAL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1
         musz = TRUE.
         /*Spara undana markerade*/
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val THEN DO:
               numspar = kon_val.NUM.
               FOR EACH kon_val WHERE kon_val.NUM = numspar:
                  CREATE bortkon_val.
                  BUFFER-COPY kon_val TO bortkon_val.            
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         /*Ta bort de som inte är markerade, men spara undan*/
         FOR EACH kon_val:
            FIND FIRST bortkon_val WHERE bortkon_val.NUM = kon_val.NUM AND
            bortkon_val.KSKAP = kon_val.KSKAP AND 
            bortkon_val.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bortkon_val THEN DO:
               CREATE sparakon_val.
               BUFFER-COPY kon_val TO sparakon_val.
               DELETE kon_val.
            END.            
         END.
         IF antal_valda > 1 THEN DO:
            MESSAGE "Vill du ha ett skåp/sida?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Ett skåp/sida" UPDATE sida. 
         END.
         ELSE sida = FALSE.
         /*GGG*/
         MESSAGE "Vill du ha liggande format?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Liggande/Stående?" UPDATE liggande.   
         RUN VFRIKORTU.W.
         /*GGG*/
         /*Lägg tillbaka de borttagna*/
         FOR EACH sparakon_val:
            FIND FIRST kon_val WHERE kon_val.NUM = sparakon_val.NUM AND
            kon_val.KSKAP = sparakon_val.KSKAP AND 
            kon_val.SKAPNUM = sparakon_val.SKAPNUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:
               CREATE kon_val.
               BUFFER-COPY sparakon_val TO kon_val.
            END.
         END.
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST kon_val WHERE kon_val.NUM = numhj AND kon_val.KSKAP = FALSE NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.
         EMPTY TEMP-TABLE bortkon_val NO-ERROR. 
         /*GGG*/
      END.
      ELSE DO:
         RUN VISAKORTU.W.   
         FIND FIRST kon_val WHERE kon_val.NUM = numhj AND kon_val.KSKAP = FALSE NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kon_val)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SKYLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SKYLT C-Win
ON VALUE-CHANGED OF RAD_SKYLT IN FRAME DEFAULT-FRAME
DO:
  RAD_SKYLT = INPUT RAD_SKYLT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SORT C-Win
ON VALUE-CHANGED OF TOG_SORT IN FRAME DEFAULT-FRAME /* För excel kort sortera efter */
DO:
   TOG_SORT = INPUT TOG_SORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(valkortapph) THEN DELETE PROCEDURE valkortapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(frikortapph) THEN DELETE PROCEDURE frikortapph.
   {BORTBRWPROC.I}
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
   BRW_VAL:MULTIPLE = TRUE.
   {ALLSTARTDYN.I} 
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}     
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   RUN berhmt_UI IN valkortapph (INPUT valaonr,INPUT valomrade,
                                 OUTPUT datvar2,OUTPUT TABLE beredningtemp).
   FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr) AND
   beredningtemp.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
   IF AVAILABLE beredningtemp THEN DO:
      ASSIGN
      FILL-IN-BERNR = beredningtemp.BERNR
      FILL-IN-AONR = beredningtemp.AONR
      FILL-IN-DELNR = beredningtemp.DELNR
      FILL-IN-BEN = beredningtemp.BENAMNING   
      valomrade = beredningtemp.OMRADE.
   END.
   EMPTY TEMP-TABLE kon_val NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl NO-ERROR. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VALKORT2U.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, INPUT datvar2, OUTPUT TABLE kon_val,
      OUTPUT TABLE kon_id, OUTPUT TABLE list_mtrl, OUTPUT TABLE kopp_lina,
      OUTPUT TABLE fri_temp).
   END.
   ELSE DO:
      RUN VALKORT2U.P
      (INPUT valaonr, INPUT valomrade, INPUT datvar2, OUTPUT TABLE kon_val,
      OUTPUT TABLE kon_id, OUTPUT TABLE list_mtrl, OUTPUT TABLE kopp_lina,
      OUTPUT TABLE fri_temp).
   END.       
   EMPTY TEMP-TABLE berskaptemp NO-ERROR.    
   RUN berskaphmt_UI IN frikortapph (OUTPUT TABLE berskaptemp).
   RUN markstngrhmt_UI  IN frikortapph (INPUT valaonr, INPUT valomrade, OUTPUT TABLE markgrupp).
   FOR EACH fri_temp,
   EACH markgrupp WHERE markgrupp.NUM = fri_temp.NUM AND markgrupp.KABNR = fri_temp.KABNR:
      fri_temp.GRUPPNR = markgrupp.GRUPP. 
   END.
   
   /*IF Guru.Konstanter:globforetag = "vast" OR Guru.Konstanter:globforetag = "elpa" THEN DO:*/                  
      /*HSP VAST*/
      FOR EACH fri_temp WHERE /*fri_temp.NUM = kon_val.NUM AND*/ fri_temp.TYP = "hsp":
         fri_temp.LSP = 2.               
      END.       
   /*END.*/
   


   OPEN QUERY BRW_VAL FOR EACH kon_val WHERE kon_val.KSKAP = FALSE
   USE-INDEX ORD NO-LOCK.
   GET FIRST BRW_VAL NO-LOCK.
   IF NOT AVAILABLE kon_val THEN DO:
      /*MESSAGE "Det finns inga kabelskåpskort att visa." VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.*/
   END.
   ASSIGN 
   sida = ?.
     
     RUN enable_UI.  
   
   /*{&WINDOW-NAME}:HIDDEN = FALSE.
   */
   IF Guru.Konstanter:globforetag = "cFORS" OR Guru.Konstanter:globforetag = "cELPA" THEN BTN_EXKSKO:LABEL = "Kort till Word/Excel".  
   ELSE BTN_EXKSKO:LABEL = "Kort till Excel".
   BTN_MEXC:LABEL = "Skyltmall Excel".  
   BTN_EXKSKO:HIDDEN = FALSE.            
   ENABLE BTN_EXKSKO WITH FRAME {&FRAME-NAME}.
   BTN_EXMARKN:LABEL = "Märkning".
   BTN_EXMARKN:HIDDEN = FALSE.

   IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "skok" OR Guru.Konstanter:globforetag = "KEKR"THEN DO:
      BTN_MEXC:HIDDEN = FALSE.
      IF Guru.Konstanter:globforetag = "gran" THEN DO:
         RAD_SKYLT = 3.
         /*kovändning av Mats och Kent alla skyltar ska visas*/
         /*ENABLE RAD_SKYLT WITH FRAME {&FRAME-NAME}.
         DISPLAY RAD_SKYLT WITH FRAME {&FRAME-NAME}.*/
      END.
   END. 
   ELSE BTN_MEXC:HIDDEN = TRUE.
   IF AVAILABLE kon_val THEN DO:
      EMPTY TEMP-TABLE korttmp NO-ERROR.    
      RUN korthmt_UI IN frikortapph (INPUT 2,INPUT Guru.Konstanter:globforetag,INPUT kon_val.GRUPP, OUTPUT visadkm, OUTPUT TABLE korttmp ).
      
      FOR EACH korttmp:
         CMB_KORTVAL:ADD-LAST(korttmp.KORT).
      END.  
      CMB_KORTVAL:SCREEN-VALUE = visadkm.
      CMB_KORTVAL = CMB_KORTVAL:SCREEN-VALUE.
      DISPLAY CMB_KORTVAL WITH FRAME {&FRAME-NAME}.
   END.
   
   {FRMSIZE.I}     
   {musarrow.i}
   {WIN_M_SLUT.I}
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
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
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
    RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORD"). 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "kon_val.KSKAP = FALSE").
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALKORTAPP.P PERSISTENT SET valkortapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALKORTAPP.P PERSISTENT SET valkortapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph.
   END. 
   /*
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.       
   tthandle = TEMP-TABLE konsttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").
   */
   tthandle = TEMP-TABLE konsttemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").
   END.       
   
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
  DISPLAY TOG_SORT CMB_KORTVAL FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR 
          FILL-IN-DELNR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOG_SORT CMB_KORTVAL BRW_VAL BTN_VISA BTN_EXMARKN BTN_MEXC BTN_AVB 
         FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE togsort_UI C-Win 
PROCEDURE togsort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   TOG_SORT:LABEL IN FRAME {&FRAME-NAME} = "För excel kort sortera efter " + "Grupp-nr".
   IF Guru.Konstanter:varforetypval[34] = 1 AND kon_val.GRUPP = 0 THEN DO:
      /*kabelskåpskort med grupp*/
      TOG_SORT:HIDDEN = FALSE.        
   END.
   ELSE IF Guru.Konstanter:globforetag = "celpa" THEN DO:     
      IF kon_val.GRUPP = 12 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                        
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:      
      IF kon_val.GRUPP = 12 OR kon_val.GRUPP = 30 OR kon_val.GRUPP = 33 OR kon_val.GRUPP = 38 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                 
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELKB" OR Guru.Konstanter:globforetag = "Celpa"   THEN DO:            
      IF kon_val.GRUPP = 35 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                 
      END.
      IF kon_val.GRUPP = 35 OR kon_val.GRUPP = 12 THEN DO:
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "HANA"  THEN DO:      
      IF kon_val.GRUPP = 33  THEN DO:
         TOG_SORT:HIDDEN = FALSE. 
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.      
   END.
   ELSE IF Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "SWEO"   THEN DO:      
      IF kon_val.GRUPP = 33 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                 
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.
   END.
   ELSE IF  Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC"  THEN DO:      
      IF kon_val.GRUPP = 33 OR kon_val.GRUPP = 34 OR kon_val.GRUPP = 36 OR kon_val.GRUPP = 37 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                 
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "ELTE" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "JAMT"  OR Guru.Konstanter:globforetag = "Celpa"   THEN DO:            
      IF kon_val.GRUPP = 32 THEN DO:
         TOG_SORT:HIDDEN = FALSE.                 
         TOG_SORT:LABEL = "För excel kort sortera efter " + "Fack-nr".
      END.      
   END.
   ELSE DO:
      TOG_SORT:HIDDEN = TRUE.     
   END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI C-Win 
PROCEDURE vcbrwval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST kon_val NO-LOCK NO-ERROR.   
   IF AVAILABLE kon_val THEN DO:
      RUN togsort_UI.
      ASSIGN
      status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      kon_rowid = ROWID(kon_val).
          
      RUN korthmt_UI IN frikortapph (INPUT 3,INPUT Guru.Konstanter:globforetag,INPUT kon_val.GRUPP, OUTPUT visadkm, OUTPUT TABLE korttmp ).           
      CMB_KORTVAL:SCREEN-VALUE = visadkm.
      
      /*IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:            
         IF mall NE 0 THEN DO:            
            IF mall = 6 OR mall = 4 OR mall = 3 OR mall = 8 THEN DO:               
               IF kon_val.GRUPP = 0 THEN DO:
                 CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Vattenfall".
               END.
               ELSE DO:
                  CMB_KORTVAL:SCREEN-VALUE = "Rörlask Vattenfall".         
               END.               
            END.
            ELSE DO:
               IF kon_val.GRUPP = 0 THEN DO:
                 CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Vattenfall".
               END.
            END.
         END.
         ELSE DO:
            IF kon_val.GRUPP = 0 THEN CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Vattenfall".
            ELSE CMB_KORTVAL:SCREEN-VALUE = "Rörlask Vattenfall".         
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
         IF kon_val.GRUPP = 0 THEN DO:        
            CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Fortum EON".
         END.
         ELSE DO:
            CMB_KORTVAL:SCREEN-VALUE = "Markstationskort Allmän".
         END.                     
      END.         
      ELSE IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         IF kon_val.GRUPP = 0 THEN DO:        
            CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Eon-ES".
         END.         
         ELSE CMB_KORTVAL:SCREEN-VALUE = "Markstationskort Allmän".         
      END.                     
      ELSE IF Guru.Konstanter:globforetag = "ELTE" OR Guru.Konstanter:globforetag = "ETSA" OR Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "HANA" THEN DO:         
         IF kon_val.GRUPP = 0 THEN DO:        
            CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Allmän".
         END.         
         ELSE CMB_KORTVAL:SCREEN-VALUE = "Markstationskort Allmän".         
      END.               
      ELSE IF Guru.Konstanter:globforetag = "ELKB"  THEN DO:         
         IF kon_val.GRUPP = 0 THEN DO:        
            CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Allmän".
         END.         
         ELSE CMB_KORTVAL:SCREEN-VALUE = "Markstationskort Allmän".         
      END.               
      ELSE CMB_KORTVAL:SCREEN-VALUE = "Kabelskåpskort Allmän".      
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         ASSIGN
         kon_val.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,10) 
         kon_val.F2:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B2,1,10)  
         kon_val.F3:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B3,1,10)
         kon_val.F4:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B4,1,10)      
         kon_val.F5:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B5,1,10)      
         kon_val.F6:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B6,1,10).      
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}        
      /*IF kon_val.GRUPP = 0 THEN DO:
         BTN_EXKSKO:HIDDEN = FALSE.      
         BTN_EXSTOLP:HIDDEN = FALSE.      
         BTN_EXMARK:HIDDEN = TRUE.         
      END.
      ELSE IF Guru.Konstanter:globforetag = "elpa" AND kon_val.GRUPP = 12 THEN DO:
         BTN_EXKSKO:HIDDEN = TRUE.      
         BTN_EXSTOLP:HIDDEN = TRUE.      
         BTN_EXMARK:HIDDEN = FALSE.      
      END.
      ELSE IF Guru.Konstanter:globforetag = "VAST" AND Guru.Konstanter:globanv = "CLHA" AND kon_val.GRUPP = 12 THEN DO:      
         BTN_EXKSKO:HIDDEN = TRUE.      
         BTN_EXSTOLP:HIDDEN = TRUE.      
         BTN_EXMARK:HIDDEN = FALSE.      
      END.
      ELSE DO:
         BTN_EXKSKO:HIDDEN = TRUE.      
         BTN_EXSTOLP:HIDDEN = TRUE.      
         BTN_EXMARK:HIDDEN = TRUE.      
      END.*/
   END.
   ELSE DO:
      ASSIGN      
      kon_val.F1:LABEL IN BROWSE BRW_VAL = "" 
      kon_val.F2:LABEL IN BROWSE BRW_VAL = ""  
      kon_val.F3:LABEL IN BROWSE BRW_VAL = ""
      kon_val.F4:LABEL IN BROWSE BRW_VAL = ""      
      kon_val.F5:LABEL IN BROWSE BRW_VAL = ""      
      kon_val.F6:LABEL IN BROWSE BRW_VAL = "".
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

