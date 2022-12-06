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
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mmtrlbapph AS HANDLE NO-UNDO.
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
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kon_rowid AS ROWID NO-UNDO. 
DEFINE SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
/*{EGENBEN.I}*/
{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{SOKDEF.I}
{LISTMTRL.I}
{HOPPSEK2W.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{SOKMTRL.I}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES list_mtrl mtrltemp

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL list_mtrl.Enr list_mtrl.Benamning ~
list_mtrl.Enhet list_mtrl.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL list_mtrl


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define QUERY-STRING-BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_VLEV OPEN QUERY BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VLEV mtrltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-6 RECT_STOR BRW_VLEV BRW_MTRL btn_over ~
btn_back BTN_NY BTN_BORT BTN_ALLA FILL-IN-ENR FILL-IN-BEN FBTN_OK FBTN_AVB ~
RAD_SOK FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENR FILL-IN-BEN RAD_SOK ~
FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLA 
     LABEL "Hela Listan" 
     SIZE 12 BY 1 TOOLTIP "Återgår och visar hela listan med spec.mtrl".

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 32 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT_STOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      mtrltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      list_mtrl.Enr FORMAT "X(11)":U
      list_mtrl.Benamning FORMAT "x(256)":U WIDTH 30
      list_mtrl.Enhet FORMAT "x(5)":U
      list_mtrl.PRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 59.5 BY 22
         TITLE "Vald materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 40
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.BPRIS FORMAT ">>>>99.99":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
  ENABLE
      mtrltemp.Enr
      mtrltemp.Benamning
      mtrltemp.Enhet
      mtrltemp.BPRIS
      mtrltemp.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 22
         TITLE "Materiel för vald leverantör".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_VLEV AT ROW 1.5 COL 1.5
     BRW_MTRL AT ROW 1.5 COL 56.88
     btn_over AT ROW 8.21 COL 52
     btn_back AT ROW 11.71 COL 52
     BTN_NY AT ROW 24 COL 7.13
     BTN_BORT AT ROW 24 COL 20.38
     BTN_ALLA AT ROW 24 COL 33.63
     FILL-IN-ENR AT ROW 25.42 COL 20.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.58 COL 20.5 COLON-ALIGNED
     FBTN_OK AT ROW 27.83 COL 87.38
     FBTN_AVB AT ROW 27.83 COL 102.38
     RAD_SOK AT ROW 27.92 COL 17 NO-LABEL
     FILL-IN-SOKALT AT ROW 27.92 COL 2 NO-LABEL
     IMAGE-6 AT ROW 25.42 COL 1.75
     RECT_STOR AT ROW 25.21 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.88 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: list_mtrl T "?" NO-UNDO temp-db list_mtrl
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
         WIDTH              = 117.13
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.63
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.63
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
/* BROWSE-TAB BRW_VLEV RECT_STOR FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_VLEV FRAME-A */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       RECT_STOR:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.list_mtrl.Benamning
"list_mtrl.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.list_mtrl.Enhet
     _FldNameList[4]   = Temp-Tables.list_mtrl.PRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VLEV
/* Query rebuild information for BROWSE BRW_VLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = leverant
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VLEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_MTRL IN FRAME FRAME-A /* Vald materiel */
DO:
   RUN infoES_UI (INPUT 1).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VLEV
&Scoped-define SELF-NAME BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ENTRY OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
   /*
   APPLY "VALUE-CHANGED" TO BRW_VLEV.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
  /*IF  THEN*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ROW-LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
  IF AVAILABLE mtrltemp THEN DO:
     RUN visa_UI.    
     ASSIGN     
     mtrltemp.Enr = INPUT BROWSE BRW_VLEV mtrltemp.Enr
     mtrltemp.Benamning = INPUT BROWSE BRW_VLEV mtrltemp.Benamning
     mtrltemp.Enhet = LC(INPUT BROWSE BRW_VLEV mtrltemp.Enhet)
     mtrltemp.Bpris = INPUT BROWSE BRW_VLEV mtrltemp.Bpris
     mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS.
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.
     CREATE emtrltemp.     
     IF mtrltemp.NPRIS = 0 THEN DO:
        mtrltemp.NPRIS = mtrltemp.BPRIS.           
     END.
     BUFFER-COPY mtrltemp TO emtrltemp. 
     RUN admsparmtrl_UI IN mmtrlbapph ( INPUT-OUTPUT TABLE emtrltemp ). 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON VALUE-CHANGED OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
   
   /*status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() NO-ERROR.  
  IF rowvar NE ? THEN DO:      
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.
     CREATE emtrltemp.
     FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = rowvar NO-LOCK NO-ERROR.
     IF AVAILABLE mtrltemp THEN DO:     
        IF mtrltemp.NPRIS = 0 THEN DO:
           mtrltemp.NPRIS = mtrltemp.BPRIS.           
        END.
        BUFFER-COPY mtrltemp TO emtrltemp. 
        RUN admsparmtrl_UI IN mmtrlbapph ( INPUT TABLE emtrltemp ). 
     END.     
  END.
  rowvar = ?.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enr IN BROWSE BRW_VLEV /* Enr */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enr IN BROWSE BRW_VLEV /* Enr */
DO:   
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_VLEV mtrltemp.Enr = "" THEN DO:     
         MESSAGE  Guru.Konstanter:genk + " får inte vara blankt." VIEW-AS ALERT-BOX.
         DISPLAY mtrltemp.Enr WITH BROWSE BRW_VLEV.         
         RETURN NO-APPLY.      
      END.    
      IF mtrltemp.Enr NE INPUT BROWSE BRW_VLEV mtrltemp.Enr THEN DO:        
         kollenr = INPUT BROWSE BRW_VLEV mtrltemp.Enr.      
         RUN kollenr_UI IN mmtrlbapph (INPUT mtrltemp.MTRLROW, INPUT kollenr,INPUT leverant, OUTPUT TABLE felmeddtemp ).    
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            DISPLAY mtrltemp.Enr WITH BROWSE BRW_VLEV.    
            APPLY "ENTRY" TO mtrltemp.Enr IN BROWSE BRW_VLEV.
            RETURN NO-APPLY.      
         END.            
         /*IF mtrltemp.Enr NE INPUT BROWSE BRW_VLEV mtrltemp.Enr THEN rowvar = ROWID(mtrltemp).
         ELSE rowvar = ?.*/
         mtrltemp.Enr = INPUT BROWSE BRW_VLEV mtrltemp.Enr.
         DISPLAY mtrltemp.Enr WITH BROWSE BRW_VLEV.          
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Benamning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Benamning IN BROWSE BRW_VLEV /* Benämning */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Benamning IN BROWSE BRW_VLEV /* Benämning */
DO:   
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING = "" THEN DO:     
         MESSAGE  "Benämning får inte vara blankt." VIEW-AS ALERT-BOX.
         DISPLAY mtrltemp.BENAMNING WITH BROWSE BRW_VLEV.    
         RETURN NO-APPLY.      
      END.          
      IF mtrltemp.BENAMNING NE INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING THEN DO: 
         /*rowvar = ROWID(mtrltemp).*/
         mtrltemp.BENAMNING = INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING.           
         DISPLAY mtrltemp.BENAMNING  WITH BROWSE BRW_VLEV.    
      END. 
   END.
   /*ELSE rowvar = ?.*/
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enhet IN BROWSE BRW_VLEV /* Enhet */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enhet IN BROWSE BRW_VLEV /* Enhet */
DO:   
   /*IF mtrltemp.ENHET NE INPUT BROWSE BRW_VLEV mtrltemp.ENHET THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/
   IF AVAILABLE mtrltemp THEN DO:
      IF mtrltemp.ENHET NE INPUT BROWSE BRW_VLEV mtrltemp.ENHET THEN DO:
         mtrltemp.ENHET = LC(INPUT BROWSE BRW_VLEV mtrltemp.ENHET).           
         DISPLAY mtrltemp.ENHET WITH BROWSE BRW_VLEV.   
      END.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.BPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.BPRIS IN BROWSE BRW_VLEV /* Brutto pris */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.BPRIS IN BROWSE BRW_VLEV /* Brutto pris */
DO:
   /*IF mtrltemp.BPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.BPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      IF mtrltemp.BPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.BPRIS THEN DO:
         mtrltemp.BPRIS = INPUT BROWSE BRW_VLEV mtrltemp.BPRIS.           
         DISPLAY mtrltemp.BPRIS WITH BROWSE BRW_VLEV.        
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.NPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.NPRIS IN BROWSE BRW_VLEV /* Netto pris */
DO:
  IF AVAILABLE mtrltemp THEN RUN visa_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.NPRIS IN BROWSE BRW_VLEV /* Netto pris */
DO:
   /*IF mtrltemp.NPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.NPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      IF mtrltemp.NPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.NPRIS THEN DO:
         mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS.           
         DISPLAY mtrltemp.NPRIS WITH BROWSE BRW_VLEV.             
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLA WINDOW-1
ON CHOOSE OF BTN_ALLA IN FRAME FRAME-A /* Hela Listan */
DO:                                        
   {muswait.i}   
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   RUN hamta_UI IN mmtrlbapph (INPUT Guru.Konstanter:globanv, OUTPUT TABLE mtrltemp).    
   RUN openbdynspec_UI IN brwproc[1].
   GET FIRST BRW_VLEV.
   FIND FIRST mtrltemp  NO-LOCK NO-ERROR.  
   IF AVAILABLE mtrltemp THEN RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).            
   RUN lastselectdyn_UI IN brwproc[1].   
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE list_mtrl.
      antal_raknare = antal_raknare + 1.   
   END.   
   RUN openbdynspec_UI IN brwproc[4].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:                   
   ASSIGN
   antal_valda = BRW_VLEV:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN                                   
      status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare)
      mtrl_rowid = ROWID(mtrltemp).                           
      MESSAGE "Vill du ta bort mtrl - " + mtrltemp.ENR + " " + mtrltemp.BENAMNING VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
      IF svar THEN DO TRANSACTION:
         EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
         EMPTY TEMP-TABLE emtrltemp.
         CREATE emtrltemp.
         BUFFER-COPY mtrltemp TO emtrltemp.
         sprow = ROWID(mtrltemp).
         RUN admbortmtrl_UI IN mmtrlbapph (INPUT-OUTPUT TABLE emtrltemp,OUTPUT TABLE felmeddtemp ). 
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.                        
         END.            
         ELSE DO:
            EMPTY TEMP-TABLE emtrltemp.
            FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = sprow NO-ERROR.                                                          
            DELETE mtrltemp.          
         END.         
      END.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].    
      antal_raknare = antal_raknare + 1.   
   END. 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(leverant) + "' USE-INDEX LEV"). 
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].            

   
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
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   APPLY "ENTRY" TO mtrltemp.ENR IN BROWSE BRW_VLEV.   
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO:   
   
   antal_valda = BRW_VLEV:NUM-SELECTED-ROWS.         
   antal_raknare = 1.   
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare).
         RUN over_UI.
      END.       
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
      RUN openbdynspec_UI IN brwproc[4].
      FIND FIRST list_mtrl WHERE ROWID(list_mtrl) = spec_rowid NO-LOCK.
      IF AVAILABLE list_mtrl THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(list_mtrl)).
         RUN lastselectdyn_UI IN brwproc[4].        
      END.      
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW().     
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVB WINDOW-1
ON CHOOSE OF FBTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO: 
           
   {muswait.i}      
      
   FOR EACH list_mtrl:    
      FIND FIRST bmtrl_mtrl WHERE bmtrl_mtrl.ENR = list_mtrl.ENR AND bmtrl_mtrl.LEVKOD = "99" 
      NO-LOCK NO-ERROR.      
      IF AVAILABLE bmtrl_mtrl THEN DO: 
         mtrl_rowid = ROWID(bmtrl_mtrl).
      END.
      ELSE DO:
         CREATE bmtrl_mtrl.
         ASSIGN 
         bmtrl_mtrl.KALKNR = valkalknr
         bmtrl_mtrl.ENR = list_mtrl.ENR
         bmtrl_mtrl.BENAMNING = list_mtrl.BENAMNING
         bmtrl_mtrl.ENHET = list_mtrl.ENHET
         bmtrl_mtrl.NPRIS = list_mtrl.PRIS
         bmtrl_mtrl.BERKVANT = 1
         bmtrl_mtrl.LEVKOD = "99"
         mtrl_rowid = ROWID(bmtrl_mtrl). 
      END.      
   END.   
   APPLY "GO" TO FBTN_OK IN FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON GO OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:  
  musz = FALSE.  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
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
   {muswait.i}
   {BENHMT2.I}   
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}
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
   {muswait.i}   
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mmtrlbapph) THEN DELETE PROCEDURE mmtrlbapph.
   {BORTBRWPROC.I}
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
   ASSIGN WINDOW-1:TITLE = "Uppdatering av specialmateriel".   
   /*BRW_VLEV:TITLE = "Materiellista : " + levtemp.LEVNAMN.                      */
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista specialmateriel").  
   RUN hamta_UI IN mmtrlbapph (INPUT Guru.Konstanter:globanv, OUTPUT TABLE mtrltemp).
   RUN enable_UI.   
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_VLEV = FALSE
      mtrltemp.BPRIS:VISIBLE IN BROWSE BRW_VLEV = FALSE
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.
   END.
   ELSE DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_VLEV = TRUE
      mtrltemp.BPRIS:VISIBLE IN BROWSE BRW_VLEV = TRUE
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_MTRL = TRUE.
   END.
   {FRMSIZE.I}        
   leverant = "99" + Guru.Konstanter:globanv.    
   FIND FIRST mtrltemp  NO-LOCK NO-ERROR.
   IF AVAILABLE mtrltemp THEN RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).         
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   ASSIGN
   mtrltemp.ENR:LABEL IN BROWSE BRW_VLEV = Guru.Konstanter:genk
   list_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
         (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:
      RUN MSPECAPP.P PERSISTENT SET mmtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MSPECAPP.P PERSISTENT SET mmtrlbapph.
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
  DISPLAY FILL-IN-ENR FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE IMAGE-6 RECT_STOR BRW_VLEV BRW_MTRL btn_over btn_back BTN_NY BTN_BORT 
         BTN_ALLA FILL-IN-ENR FILL-IN-BEN FBTN_OK FBTN_AVB RAD_SOK 
         FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
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
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
      FIND FIRST list_mtrl WHERE list_mtrl.ENR = mtrltemp.ENR AND list_mtrl.LEVKOD = mtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
      IF AVAILABLE list_mtrl THEN DO:
         spec_rowid = ROWID(list_mtrl).
      END.      
      ELSE DO:
         CREATE list_mtrl. 
         ASSIGN
         list_mtrl.ENR = mtrltemp.ENR
         list_mtrl.BENAMNING = mtrltemp.BENAMNING
         list_mtrl.ENHET = LC(mtrltemp.ENHET)
         list_mtrl.ANTAL = 1
         list_mtrl.PRIS = mtrltemp.NPRIS          
         list_mtrl.LEVKOD = mtrltemp.LEVKOD
         spec_rowid = ROWID(list_mtrl).
      END. 
      antal_raknare = antal_raknare + 1.              
       
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
      status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.          
      DISPLAY
      mtrltemp.ENR       
      mtrltemp.BENAMNING
      mtrltemp.ENHET mtrltemp.BPRIS mtrltemp.NPRIS WITH BROWSE BRW_VLEV.         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

