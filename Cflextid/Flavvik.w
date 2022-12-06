&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: Flavvik.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/08/97 -  8:27 am

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
&Scoped-define SHARED 
{FLEXTAB.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE flrec AS RECID NO-UNDO.    
DEFINE SHARED VARIABLE flrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fldrec AS RECID NO-UNDO.    
DEFINE SHARED VARIABLE fldrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tomdat AS DATE NO-UNDO.
DEFINE SHARED VARIABLE manad AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE radvar AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE flexavikapph AS HANDLE NO-UNDO.
DEFINE VARIABLE flexrec AS RECID NO-UNDO.
DEFINE TEMP-TABLE namntemp
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER  
   FIELD NAMN AS CHARACTER  
   FIELD NTEXT AS CHARACTER
   FIELD REGIS AS CHARACTER  
   FIELD DATUM AS DATE
   FIELD ALLVAL AS INTEGER
   FIELD MANADVAR AS INTEGER.     
{TIDUTTTNEW.I}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AVVIK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES flexdagtemp

/* Definitions for BROWSE BRW_AVVIK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVVIK flexdagtemp.PERSONALKOD ~
flexdagtemp.DATUM flexdagtemp.FELMED flexdagtemp.FELOK flexdagtemp.KONTROLL ~
flexdagtemp.FORNAMN flexdagtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVVIK flexdagtemp.PERSONALKOD ~
flexdagtemp.FELOK 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AVVIK flexdagtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AVVIK flexdagtemp
&Scoped-define QUERY-STRING-BRW_AVVIK FOR EACH flexdagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVVIK OPEN QUERY BRW_AVVIK FOR EACH flexdagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVVIK flexdagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVVIK flexdagtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AVVIK FBTN_SKRIV BTN_UPP BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TEXT FILL-IN-DATUM FILL-IN-REGIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Rätta":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 13.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 39.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 63.5 BY 1
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVVIK FOR 
      flexdagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVVIK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVVIK WINDOW-1 _STRUCTURED
  QUERY BRW_AVVIK NO-LOCK DISPLAY
      flexdagtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      flexdagtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      flexdagtemp.FELMED COLUMN-LABEL "Felmed" FORMAT "X(250)":U
            WIDTH 33.5
      flexdagtemp.FELOK COLUMN-LABEL "Ok" FORMAT "Ja/Nej":U
      flexdagtemp.KONTROLL COLUMN-LABEL "Kontroll" FORMAT "X(10)":U
      flexdagtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(250)":U
            WIDTH 19
      flexdagtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(250)":U
            WIDTH 25
  ENABLE
      flexdagtemp.PERSONALKOD
      flexdagtemp.FELOK
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 109 BY 21.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-TEXT AT ROW 1.75 COL 1.5 NO-LABEL
     FILL-IN-DATUM AT ROW 1.75 COL 52.5 COLON-ALIGNED NO-LABEL
     FILL-IN-REGIS AT ROW 3.25 COL 1.5 NO-LABEL
     FILL-IN_NAMN AT ROW 3.25 COL 22.88 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_FORNAMN AT ROW 3.25 COL 22.88 COLON-ALIGNED NO-LABEL
     BRW_AVVIK AT ROW 4.75 COL 1.5
     FBTN_SKRIV AT ROW 8 COL 111.5
     RAD_ALLVAL AT ROW 16.75 COL 2.25 NO-LABEL
     BTN_UPP AT ROW 26.46 COL 48.38
     BTN_AVB AT ROW 26.46 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 26.96.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: flexdagtemp T "?" NO-UNDO temp-db flexdagtemp
      TABLE: ? T "?" NO-UNDO temp-db tidpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra eller registrera flextid"
         HEIGHT             = 27.08
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
/* BROWSE-TAB BRW_AVVIK FILL-IN_FORNAMN FRAME-A */
/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVVIK
/* Query rebuild information for BROWSE BRW_AVVIK
     _TblList          = "temp-db.flexdagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.flexdagtemp.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.flexdagtemp.DATUM
"DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.flexdagtemp.FELMED
"FELMED" "Felmed" "X(250)" "character" ? ? ? ? ? ? no ? no no "33.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.flexdagtemp.FELOK
"FELOK" "Ok" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.flexdagtemp.KONTROLL
"KONTROLL" "Kontroll" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.flexdagtemp.FORNAMN
"FORNAMN" "Förnamn" "X(250)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.flexdagtemp.EFTERNAMN
"EFTERNAMN" "Efternamn" "X(250)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVVIK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_AVVIK
&Scoped-define SELF-NAME BRW_AVVIK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVVIK WINDOW-1
ON ROW-DISPLAY OF BRW_AVVIK IN FRAME FRAME-A
DO:
   IF AVAILABLE flexdagtemp THEN DO:
      IF flexdagtemp.KONTROLL = "kontroll" THEN DO:
         flexdagtemp.FELOK:READ-ONLY IN BROWSE BRW_AVVIK = FALSE.
      END.
      ELSE flexdagtemp.FELOK:READ-ONLY IN BROWSE BRW_AVVIK = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVVIK WINDOW-1
ON VALUE-CHANGED OF BRW_AVVIK IN FRAME FRAME-A
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME flexdagtemp.FELOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flexdagtemp.FELOK BRW_AVVIK _BROWSE-COLUMN WINDOW-1
ON ENTRY OF flexdagtemp.FELOK IN BROWSE BRW_AVVIK /* Ok */
DO:
   IF AVAILABLE flexdagtemp THEN DO:
     DISPLAY flexdagtemp.FELOK WITH BROWSE BRW_AVVIK.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flexdagtemp.FELOK BRW_AVVIK _BROWSE-COLUMN WINDOW-1
ON LEAVE OF flexdagtemp.FELOK IN BROWSE BRW_AVVIK /* Ok */
DO:
   IF AVAILABLE flexdagtemp THEN DO:
      IF flexdagtemp.KONTROLL = "kontroll" THEN DO:
         IF flexdagtemp.FELOK NE INPUT BROWSE BRW_AVVIK flexdagtemp.FELOK THEN DO:
            flexdagtemp.FELOK = INPUT BROWSE BRW_AVVIK flexdagtemp.FELOK.
            EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
            CREATE eflexdagtemp.
            BUFFER-COPY flexdagtemp TO eflexdagtemp.
            RUN flexdagok_IU IN flexavikapph (INPUT TABLE eflexdagtemp).
            EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.
         END.
      END.
      ELSE DO: 
         flexdagtemp.FELOK = flexdagtemp.FELOK.
      END.
   END.
   DISPLAY flexdagtemp.FELOK WITH BROWSE BRW_AVVIK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flexdagtemp.FELOK BRW_AVVIK _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF flexdagtemp.FELOK IN BROWSE BRW_AVVIK /* Ok */
DO:
   IF flexdagtemp.KONTROLL = "kontroll" THEN DO:
      IF flexdagtemp.FELOK = TRUE THEN flexdagtemp.FELOK = FALSE.
      ELSE IF flexdagtemp.FELOK = FALSE THEN flexdagtemp.FELOK = TRUE.
      EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
      CREATE eflexdagtemp.
      BUFFER-COPY flexdagtemp TO eflexdagtemp.
      RUN flexdagok_IU IN flexavikapph (INPUT TABLE eflexdagtemp).
      EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
   END.
   DISPLAY flexdagtemp.FELOK WITH BROWSE BRW_AVVIK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   ASSIGN
   flrec = ?
   flrec2 = ?   
   fldrec = ?
   fldrec2 = ?.    
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Rätta */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = flexdagtemp.PERSONALKOD NO-LOCK NO-ERROR.
   persrec = tidpers.TIDPERSREC.
   {muswait.i}
   ASSIGN
   vart = "Korr"
   regdatum = flexdagtemp.DATUM.
   
   IF vartgamla = "XXX" THEN DO:  
      vartgamla = "".
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   ELSE DO:
      ASSIGN
      flexrec = flexdagtemp.FREC
      vartgamla = "XXX".                       
      FIND tidpers WHERE tidpers.TIDPERSREC = persrec  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidpers THEN DO:         
           status-mus2 = SESSION:SET-WAIT-STATE("").
         MESSAGE 
         "Du är inte behörig att ändra eller visa denna persons tidregistreringar."
         VIEW-AS ALERT-BOX.
      END.      
      ELSE DO:        
         {AVBGOM.I}
         RUN FLEXAND.W (INPUT tidpers.PERSONALKOD).
         {AVBFRAM.I}     
         IF Guru.Konstanter:appcon THEN DO:
            RUN FAVRAAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT-OUTPUT TABLE flexdagtemp).
         END.
         ELSE DO:
            RUN FAVRAAPP.P 
            (INPUT-OUTPUT TABLE flexdagtemp).
         END.
         FIND FIRST flexdagtemp WHERE flexdagtemp.FREC = flexrec NO-LOCK NO-ERROR.
         IF AVAILABLE flexdagtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(flexdagtemp)).
         END.
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].
      END.   
   END.
   {musarrow.i}      
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   EMPTY TEMP-TABLE tidut  NO-ERROR.    
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,50) = STRING(TODAY)
   SUBSTRING(tidut.UT,65) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.    
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,5) = FILL-IN-TEXT
   SUBSTRING(tidut.UT,57) = STRING(tomdat).
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,5) = 
   "============================================================".
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,5) = FILL-IN-REGIS.
   IF RAD_ALLVAL = 1 THEN ASSIGN SUBSTRING(tidut.UT,25) = FILL-IN_FORNAMN.
   IF RAD_ALLVAL = 2 THEN ASSIGN SUBSTRING(tidut.UT,25) = FILL-IN_NAMN.
   CREATE tidut. 
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Enhet"
   SUBSTRING(tidut.UT,7) = "Datum"
   SUBSTRING(tidut.UT,16) = "Felmeddelande"
   SUBSTRING(tidut.UT,37) = "Ok"
   SUBSTRING(tidut.UT,41) = "Kontroll"
   SUBSTRING(tidut.UT,54) = "Orsak "
   SUBSTRING(tidut.UT,74) = "Förnamn"   
   SUBSTRING(tidut.UT,90) = "Efternamn".          
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,1) = 
"=========================================================================================================".
   CREATE tidut.   
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   DO WHILE AVAILABLE(flexdagtemp):
      CREATE tidut.    
      ASSIGN
      SUBSTRING(tidut.UT,1) = flexdagtemp.PERSONALKOD
      SUBSTRING(tidut.UT,7) = STRING(flexdagtemp.DATUM)
      SUBSTRING(tidut.UT,16) = SUBSTRING(flexdagtemp.FELMED,1,20)
      SUBSTRING(tidut.UT,37) = STRING(flexdagtemp.FELOK,"Ja/Nej") 
      SUBSTRING(tidut.UT,41) = SUBSTRING(flexdagtemp.KONTROLL,1,12)
      SUBSTRING(tidut.UT,74) = flexdagtemp.FORNAMN
      SUBSTRING(tidut.UT,90) = flexdagtemp.EFTERNAMN. 
      ASSIGN
      sok1 = flexdagtemp.PERSONALKOD
      sok4 = STRING(flexdagtemp.DATUM).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 14,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 14,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      IF sok1 NE "" THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,54) = SUBSTRING(sok1,1,20).
      END.   
      GET NEXT {&BROWSE-NAME} NO-LOCK. 
   END.   
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGL.P.                               
   {musarrow.i}    
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  {BORTBRWPROC.I}
  IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.      
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
   CREATE namntemp.
   ASSIGN
   namntemp.ALLVAL = RAD_ALLVAL
   namntemp.MANADVAR = manad
   namntemp.DATUM = tomdat.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLAVVAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT TABLE tidpers,OUTPUT TABLE flexdagtemp,INPUT-OUTPUT TABLE namntemp).
   END.
   ELSE DO:
      RUN FLAVVAPP.P 
      (INPUT TABLE tidpers,OUTPUT TABLE flexdagtemp,INPUT-OUTPUT TABLE namntemp).
   END.
   vart = "AND".
   FIND FIRST namntemp NO-ERROR.
   IF namntemp.ALLVAL = 1 THEN DO:
      ASSIGN
      FILL-IN-REGIS = namntemp.REGIS
      FILL-IN_FORNAMN = namntemp.FORNAMN + " " + namntemp.EFTERNAMN      
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      ASSIGN
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":"
      FILL-IN_NAMN:HIDDEN = FALSE
      FILL-IN_NAMN = namntemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = namntemp.REGIS.
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = namntemp.REGIS.
   END.
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = namntemp.REGIS.  
   IF manad = 0 THEN DO:
      ASSIGN
      FILL-IN-TEXT = namntemp.NTEXT
      FILL-IN-DATUM = namntemp.DATUM.
   END.   
   IF manad = 1 THEN DO:
      ASSIGN 
      FILL-IN-TEXT = namntemp.NTEXT 
      FILL-IN-DATUM = namntemp.DATUM.
   END.   
   RUN enable_UI.   
   {FRMSIZE.I}   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   ENABLE {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.
   DISPLAY {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.
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
   flexdagtemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_AVVIK = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AVVIK:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
   END.
   ELSE DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph.
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
  DISPLAY FILL-IN-TEXT FILL-IN-DATUM FILL-IN-REGIS 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_AVVIK FBTN_SKRIV BTN_UPP BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

