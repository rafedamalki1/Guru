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

  Created: 10/15/96 -  9:58 am

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
{MTRLTAB.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBVAR.I}*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLMALLTEMP.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
/*{DEPATEMP.I}*/
DEFINE NEW SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mall AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE flerfil AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtrladmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjwebbvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE allakat AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE lev_temp
   FIELD LEVNAMN AS CHARACTER LABEL "Leverantör"
   FIELD LEVKOD AS CHARACTER LABEL "Lev-id"
   FIELD ORDNING AS INTEGER
   INDEX ORDNING ORDNING.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
 
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_LEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lev_temp

/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV lev_temp.LEVKOD lev_temp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV 
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH lev_temp NO-LOCK ~
    BY lev_temp.LEVKOD INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH lev_temp NO-LOCK ~
    BY lev_temp.LEVKOD INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LEV lev_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV lev_temp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_LEV ED_WWW CMB_VALLEV TOG_E BTN_KOR ~
BTN_NY BTN_UPP FBTN_INLDAT BTN_AVB FILL-IN-ELPOOL 
&Scoped-Define DISPLAYED-OBJECTS ED_WWW CMB_VALLEV TOG_E FILL-IN-ELPOOL ~
FILL-IN-LEVTEXT FILL-IN-INFO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny katalog" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Uppdat. katalog" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_ELPOOL 
     LABEL "Special program" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_INLDAT 
     LABEL "Katalog status" 
     SIZE 12 BY 1 TOOLTIP "Inläst datum, jmf pris mm".

DEFINE VARIABLE CMB_VALLEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE ED_WWW AS CHARACTER INITIAL "Tryck här för info om uppdateringsdatum materielkataloger på www.elpool.se" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 46 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ELPOOL AS CHARACTER FORMAT "X(256)":U INITIAL "Tryck här för info om uppdateringsdatum materielkataloger på www.elpool.se" 
      VIEW-AS TEXT 
     SIZE 46 BY .75
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FILL-IN-INFO AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 86 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVTEXT AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UPP AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_E AS LOGICAL INITIAL no 
     LABEL "Med 'E'" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEV FOR 
      lev_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      lev_temp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      lev_temp.LEVNAMN FORMAT "x(256)":U WIDTH 31
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 40.38 BY 17.42
         TITLE "Leverantörer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_LEV AT ROW 1.5 COL 4
     ED_WWW AT ROW 2.04 COL 45.5 NO-LABEL
     CMB_VALLEV AT ROW 4.67 COL 60 COLON-ALIGNED
     TOG_E AT ROW 6 COL 78.5
     BTN_KOR AT ROW 7.33 COL 76.63
     BTN_NY AT ROW 19.25 COL 4
     BTN_UPP AT ROW 19.25 COL 4.38
     FBTN_ELPOOL AT ROW 19.25 COL 17.38
     FBTN_INLDAT AT ROW 19.25 COL 30.88
     BTN_AVB AT ROW 19.25 COL 76.63
     FILL-IN-ELPOOL AT ROW 2.04 COL 45.5 NO-LABEL
     FILL-IN-LEVTEXT AT ROW 3.5 COL 49.5 NO-LABEL
     FILL-IN-UPP AT ROW 11.25 COL 50.5 COLON-ALIGNED NO-LABEL
     FILL-IN-INFO AT ROW 20.58 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.63 BY 21.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db lev_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Materieladministration"
         HEIGHT             = 21.54
         WIDTH              = 91.13
         MAX-HEIGHT         = 24.38
         MAX-WIDTH          = 122.5
         VIRTUAL-HEIGHT     = 24.38
         VIRTUAL-WIDTH      = 122.5
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_LEV 1 FRAME-A */
ASSIGN 
       BRW_LEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       ED_WWW:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR BUTTON FBTN_ELPOOL IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ELPOOL IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-INFO IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-INFO:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LEVTEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-UPP IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.lev_temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.lev_temp.LEVKOD|yes"
     _FldNameList[1]   > Temp-Tables.lev_temp.LEVKOD
"lev_temp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.lev_temp.LEVNAMN
"lev_temp.LEVNAMN" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV WINDOW-1
ON VALUE-CHANGED OF BRW_LEV IN FRAME FRAME-A /* Leverantörer */
DO:    
   leverant = lev_temp.LEVKOD.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WINDOW-1
ON CHOOSE OF BTN_KOR IN FRAME FRAME-A /* Kör funktion */
DO:
   /*används ej
  IF Guru.Konstanter:varforetypval[27] = 1 THEN.
  ELSE DO: 
     CMB_VALLEV = INPUT CMB_VALLEV.
     TOG_E = INPUT TOG_E.
     IF CMB_VALLEV = ""  THEN DO:
        MESSAGE "Leverantör kan inte vara blank" VIEW-AS ALERT-BOX.     
        RETURN.
     END.    
     MESSAGE "Denna inläsning tar lång tid! Vill du starta den nu?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL. 
     IF svar THEN DO: 
            
        IF CMB_VALLEV = "Onninen" THEN DO:
           hjwebbvar = "http://www2.guruonweb.se/files/onninen.exe".     
        END.
        IF CMB_VALLEV = "Elektroskandia" THEN DO:
           hjwebbvar = "http://www2.guruonweb.se/files/elektroskandia.exe".
        END.
        IF CMB_VALLEV = "Ahlsell" THEN DO:
           hjwebbvar = "http://www2.guruonweb.se/files/ahlsell.exe".
        END.
        IF CMB_VALLEV = allakat THEN DO:
           hjwebbvar = "http://www2.guruonweb.se/files/onelah.exe".
        END.
        RUN OPENDOC.P (hjwebbvar,"","",NO).
        MESSAGE "Nu startar inläsning. Är du beredd?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar. 
        IF svar = FALSE THEN DO:
           RETURN NO-APPLY.
        END.
        {muswait.i}  
        FILL-IN-UPP = "Nu börjar borttag av materiel " + STRING(TIME,"HH:MM").  
        DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
      
        IF CMB_VALLEV = "Onninen" OR CMB_VALLEV = allakat THEN DO:
           RUN bortmtrl_UI IN mtrladmapph (INPUT "5").                   
           FILL-IN-UPP = "Borttag av Onninen klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
        END.
        IF CMB_VALLEV = "Elektroskandia" OR CMB_VALLEV = allakat THEN DO:
           RUN bortmtrl_UI IN mtrladmapph (INPUT "1").           
           FILL-IN-UPP = "Borttag av Elektroskandia klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
        END.
        IF CMB_VALLEV = "Ahlsell" OR CMB_VALLEV = allakat THEN DO:
           RUN bortmtrl_UI IN mtrladmapph (INPUT "2").           
           FILL-IN-UPP = "Borttag av Ahlsell klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
        END.  
        FILL-IN-UPP = "Nu börjar inläsning av materiel " + STRING(TIME,"HH:MM").  
        DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
        IF CMB_VALLEV = "Onninen" OR CMB_VALLEV = allakat THEN DO:
           IF Guru.Konstanter:appcon = FALSE THEN DO:
              RUN nykatalog_UI IN mtrladmapph (INPUT "C:\tempguru\onninen.d",INPUT TOG_E).           
           END.
           ELSE DO:
              RUN appnykatalog_UI (INPUT "C:\tempguru\onninen.d",INPUT TOG_E).
           END.
           FILL-IN-UPP = "Inläsning av Onninen klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
           RUN inlastkat_UI (INPUT "5").
        END.
        IF CMB_VALLEV = "Elektroskandia" OR CMB_VALLEV = allakat THEN DO:
           IF Guru.Konstanter:appcon = FALSE THEN DO:
              RUN nykatalog_UI IN mtrladmapph (INPUT "C:\tempguru\elektroskandia.d",INPUT TOG_E).           
           END.
           ELSE DO:
              RUN appnykatalog_UI (INPUT "C:\tempguru\elektroskandia.d",INPUT TOG_E).
           END.
           FILL-IN-UPP = "Inläsning av Elektroskandia klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
           RUN inlastkat_UI (INPUT "1").
        END.
        IF CMB_VALLEV = "Ahlsell" OR CMB_VALLEV = allakat THEN DO:
           IF Guru.Konstanter:appcon = FALSE THEN DO:
              RUN nykatalog_UI IN mtrladmapph (INPUT "C:\tempguru\ahlsell.d",INPUT TOG_E).           
           END.
           ELSE DO:
              RUN appnykatalog_UI (INPUT "C:\tempguru\ahlsell.d",INPUT TOG_E).
           END.
           FILL-IN-UPP = "Inläsning av Ahlsell klart " + STRING(TIME,"HH:MM").  
           DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.
           RUN inlastkat_UI (INPUT "2").
        END.
        FILL-IN-UPP = "Klart med inläsning av materiel " + STRING(TIME,"HH:MM").  
        DISPLAY FILL-IN-UPP VIEW-AS TEXT WITH FRAME {&FRAME-NAME} NO-LABELS.  
        {musarrow.i}       
     END.
   END.*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny katalog */
DO:      
   /* användds ej 
   NY1:
   REPEAT:
      IF leverant = "" THEN DO:
         MESSAGE "Markera den leverantör ni vill lägga in ny katalog för." VIEW-AS ALERT-BOX.
         LEAVE NY1.
      END.
      ELSE DO:   
         {muswait.i}      
         FIND FIRST mtrlmalltemp WHERE mtrlmalltemp.LEVKOD = leverant NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlmalltemp THEN DO:
            mall = TRUE.
         END.
         ELSE DO:       
            mall = FALSE.
         END.       
         SYSTEM-DIALOG GET-FILE filnamn
            TITLE          "Välj den fil som motsvarar leverantörens katalog"
            FILTERS        "All Files (*.*)"  "*.*"
            INITIAL-DIR    "a:"
            MUST-EXIST         
            USE-FILENAME
            UPDATE OKvald.
            
         IF OKvald = TRUE THEN DO:            
            {AVBGOM.I}
            RUN NYKATALOV.W.
            {AVBFRAM.I}            
            FIND FIRST levtemp WHERE levtemp.LEVKOD = leverant NO-LOCK NO-ERROR.
            MESSAGE "Vill du läsa in någon mer fil för - " + levtemp.LEVNAMN VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL. 
            IF svar THEN DO: 
               ASSIGN        
               flerfil = TRUE.                              
            END. 
            ELSE DO:
               flerfil = FALSE.
            END.                                         
         END.   
         ELSE DO:
            LEAVE NY1.
         END.                   
         IF flerfil = FALSE THEN LEAVE NY1.
      END.               
   END.  
   {musarrow.i}  */   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Uppdat. katalog */
DO:
   IF leverant = "" THEN DO:
      MESSAGE "Markera den leverantör ni vill lägga in ny katalog för." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:   
      {muswait.i}  
      {AVBGOM.I}  
     {AMERICANEUROPEAN.I}
            
      RUN UPPMTRLV.W.
     
      {EUROPEANAMERICAN.I}
     
      {AVBFRAM.I}     
      {musarrow.i}   
   END.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_WWW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_WWW WINDOW-1
ON MOUSE-SELECT-CLICK OF ED_WWW IN FRAME FRAME-A
DO:
  FILL-IN-ELPOOL:FGCOLOR = 12.
  ED_WWW:FGCOLOR = 12.  
  RUN OPENDOC.P ("http://www.elpool.se/materielinformation.aspx","","",NO).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ELPOOL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ELPOOL WINDOW-1
ON CHOOSE OF FBTN_ELPOOL IN FRAME FRAME-A /* Special program */
DO:
   MESSAGE "OBS! Kör inte något av dessa program utan att kontakta Elpool först. Tele 090 - 18 45 40.
   Vill Ni fortsätta?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
   IF svar THEN DO:   
      IF leverant = "" THEN DO:
         MESSAGE "Markera leverantör." VIEW-AS ALERT-BOX.
      END.
      ELSE DO:   
         {muswait.i}   
         {AVBGOM.I}  
         RUN MTRLELPOV.W.
         {AVBFRAM.I}     
         {musarrow.i}   
      END.   
   END.
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_INLDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_INLDAT WINDOW-1
ON CHOOSE OF FBTN_INLDAT IN FRAME FRAME-A /* Katalog status */
DO:
   IF leverant = "" THEN DO:
      MESSAGE "Markera den leverantör ni vill lägga in inläst katalog datum för." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:   
      {muswait.i}  
      
      RUN INLKATDATA.W.
      
      {musarrow.i}   
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_E
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_E WINDOW-1
ON VALUE-CHANGED OF TOG_E IN FRAME FRAME-A /* Med 'E' */
DO:
  TOG_E = INPUT TOG_E.
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrladmapph) THEN DELETE PROCEDURE mtrladmapph.
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
   {ALLSTARTDYN.I}
   ASSIGN
   leverant = " "
   flerfil = FALSE
   ED_WWW:FGCOLOR = 1
   ED_WWW:FONT = 6.
  
   status-ok = ED_WWW:LOAD-MOUSE-POINTER("GLOVE":U) IN FRAME {&FRAME-NAME}.
   FILL-IN-LEVTEXT = "Välj leverantör för inläsning av senaste materielkatalog utan priser:".   
   status-ok = CMB_VALLEV:ADD-LAST("Onninen").   
   status-ok = CMB_VALLEV:ADD-LAST("Elektroskandia").      
   status-ok = CMB_VALLEV:ADD-LAST("Ahlsell").     
   allakat = "Onninen och Elektroskandia och Ahlsell".
   status-ok = CMB_VALLEV:ADD-LAST(allakat).     
   CMB_VALLEV:SCREEN-VALUE = "Onninen".

   IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN mtrlmallhmt_UI IN mtrladmapph (OUTPUT TABLE mtrlmalltemp).         

   /*lägg till personlig specmtrllista*/            
   CREATE levtemp.
   ASSIGN
   levtemp.LEVKOD = "99" + Guru.Konstanter:globanv  
   levtemp.LEVNAMN = Guru.Konstanter:globanv + " " + "Spec. mtrl".    
   levtemp.ORDNING = 999.
   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE NO-LOCK:
      CREATE lev_temp.
      ASSIGN
      lev_temp.LEVKOD = levtemp.LEVKOD
      lev_temp.LEVNAMN = levtemp.LEVNAMN.
      lev_temp.ORDNING = levtemp.ORDNING.
   END.   
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").  
   RUN openbdyn_UI IN brwproc[1] (INPUT "").         
   FILL-IN-INFO = "OBS! Vid inläsning av kataloger uppdateras " + LC(Guru.Konstanter:gaok) + " vid nattkörningen. Tidigast 19.30.".
   RUN enable_UI.   
   IF Guru.Konstanter:globforetag NE "GRAN" THEN FILL-IN-INFO:HIDDEN = TRUE.
   {FRMSIZE.I}
   ENABLE FBTN_ELPOOL WITH FRAME {&FRAME-NAME}. 
   FILL-IN-ELPOOL:SELECTABLE = FALSE.
         
   CMB_VALLEV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   BTN_KOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
   TOG_E:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
   FILL-IN-ELPOOL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
   FILL-IN-LEVTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
   ED_WWW:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.            
   BTN_NY:HIDDEN = TRUE. 
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN MTRKADMAPP.P PERSISTENT SET mtrladmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN MTRKADMAPP.P PERSISTENT SET mtrladmapph.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appnykatalog_UI WINDOW-1 
PROCEDURE appnykatalog_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   DEFINE INPUT PARAMETER infil AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER satte AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   {AMERICANEUROPEAN.I}
   INPUT FROM VALUE(infil) convert target "iso8859-1" source "iso8859-1".       
   REPEAT:
      CREATE mtrltemp.
      ASSIGN.
      IMPORT mtrltemp.
      IF satte = TRUE THEN DO:
         IF mtrltemp.ENR NE "" THEN DO:         
            mtrltemp.enr = "E" + mtrltemp.enr.
         END.
      END.
      mtrltemp.ENHET = LC(mtrltemp.ENHET).
   END.  
   FOR EACH mtrltemp WHERE mtrltemp.ENR = "":
      DELETE mtrltemp.
   END.
   IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN nykatalogapp_UI IN mtrladmapph (INPUT TABLE mtrltemp). 
   {EUROPEANAMERICAN.I}          
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
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
  DISPLAY ED_WWW CMB_VALLEV TOG_E FILL-IN-ELPOOL FILL-IN-LEVTEXT FILL-IN-INFO 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_LEV ED_WWW CMB_VALLEV TOG_E BTN_KOR BTN_NY BTN_UPP FBTN_INLDAT 
         BTN_AVB FILL-IN-ELPOOL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inlastkat_UI WINDOW-1 
PROCEDURE inlastkat_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   DEFINE INPUT PARAMETER lkod AS CHARACTER NO-UNDO.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"                   
   inextradatatemp.HUVUDCH = lkod.                    
                       
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extradatatemp THEN DO:
      CREATE extradatatemp.
      ASSIGN
      extradatatemp.PROGRAM = "INLKAT"                   
      extradatatemp.HUVUDCH = lkod.  
   END.
   ASSIGN
   extradatatemp.SOKDATE[1] = TODAY 
   extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
   extradatatemp.SOKCHAR[2] =  Guru.Konstanter:globanv.
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

