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
DEFINE INPUT PARAMETER kskap AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE bmtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE kon_rowid AS ROWID NO-UNDO. 
DEFINE SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE muszkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{SOKDEF.I}
{SMTRL.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{KONVALTEMP.I}
{LISTMTRL.I}
{HOPPSEK2W.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES spec_mtrl mtrltemp

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.Enr spec_mtrl.Benamning ~
spec_mtrl.Enhet spec_mtrl.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS mtrltemp.KUND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS ~
mtrltemp.KUND 
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
btn_back BTN_NY BTN_BORT BTN_ALLA FILL-IN-ENR FILL-IN-BEN RAD_SOK FBTN_OK ~
FBTN_AVB FILL-IN-SOKALT 
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
     SIZE 20.25 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 33.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT_STOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.38 BY 4.33
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      mtrltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      spec_mtrl.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 30
      spec_mtrl.Enhet COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      spec_mtrl.NPRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58.38 BY 20.63
         TITLE "Vald materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 30
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.BPRIS FORMAT ">>>>99.99":U
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
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 60 BY 20.63
         TITLE "Materiel för vald leverantör".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_VLEV AT ROW 2 COL 1.5
     BRW_MTRL AT ROW 2 COL 66.63
     btn_over AT ROW 9.75 COL 61.88
     btn_back AT ROW 13.25 COL 61.88
     BTN_NY AT ROW 23.25 COL 11.5
     BTN_BORT AT ROW 23.25 COL 25
     BTN_ALLA AT ROW 23.25 COL 38.5
     FILL-IN-ENR AT ROW 25.04 COL 20.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.21 COL 20.5 COLON-ALIGNED
     RAD_SOK AT ROW 27.54 COL 23.88 NO-LABEL
     FBTN_OK AT ROW 28.04 COL 96
     FBTN_AVB AT ROW 28.04 COL 111
     FILL-IN-SOKALT AT ROW 27.54 COL 7.5 NO-LABEL
     IMAGE-6 AT ROW 25.04 COL 3.38
     RECT_STOR AT ROW 24.71 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: spec_mtrl T "?" NO-UNDO temp-db spec_mtrl
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
         WIDTH              = 124.63
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
   FRAME-NAME                                                           */
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
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.spec_mtrl.Enr
"spec_mtrl.Enr" "Enr" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.Enhet
"spec_mtrl.Enhet" "Enhet" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.NPRIS
"spec_mtrl.NPRIS" "Pris" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrltemp.KUND
"mtrltemp.KUND" "Märkt" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
    
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
      mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS
      mtrltemp.KUND = INPUT BROWSE BRW_VLEV mtrltemp.KUND.
      RUN visa_UI. 
      EMPTY TEMP-TABLE emtrltemp NO-ERROR.
      IF mtrltemp.Enhet = "" THEN DO:
        MESSAGE "Enhet får inte vara blankt!"  VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
      END.
      CREATE emtrltemp.     
      IF mtrltemp.NPRIS = 0 THEN DO:
        mtrltemp.NPRIS = mtrltemp.BPRIS.           
      END.
      BUFFER-COPY mtrltemp TO emtrltemp. 
      RUN admsparmtrl_UI IN bmtrlbapph ( INPUT-OUTPUT TABLE emtrltemp ). 
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
      FOR EACH spec_mtrl :
         IF spec_mtrl.ENR = " " THEN DELETE spec_mtrl.
         ELSE DO:
            FIND FIRST mtrltemp  WHERE mtrltemp.ENR = spec_mtrl.ENR NO-LOCK NO-ERROR.
            IF AVAILABLE mtrltemp THEN DO:
               ASSIGN 
               spec_mtrl.BENAMNING = mtrltemp.BENAMNING
               spec_mtrl.ENHET = LC(mtrltemp.ENHET)
               spec_mtrl.NPRIS = mtrltemp.NPRIS 
               spec_mtrl.BPRIS = mtrltemp.BPRIS.
            END.   
            IF NOT AVAILABLE mtrltemp THEN DO:            
               DELETE spec_mtrl.
            END.
         END.   
      END.      
   END.
   RELEASE mtrltemp NO-ERROR.
   RELEASE spec_mtrl NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON VALUE-CHANGED OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
   status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.             
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
         RUN AONRNAMNKOLL.P (INPUT 3, INPUT kollenr,OUTPUT musz).  
         IF musz = TRUE THEN DO:
            musz = FALSE.
            DISPLAY mtrltemp.Enr WITH BROWSE BRW_VLEV.         
            RETURN NO-APPLY.  
         END.   
         RUN kollenr_UI IN bmtrlbapph (INPUT mtrltemp.MTRLROW, INPUT kollenr,INPUT leverant, OUTPUT TABLE felmeddtemp ).    
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            DISPLAY mtrltemp.Enr WITH BROWSE BRW_VLEV.    
            APPLY "ENTRY" TO mtrltemp.Enr IN BROWSE BRW_VLEV.
            RETURN NO-APPLY.      
         END.                     
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
         mtrltemp.BENAMNING = INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING.
         mtrltemp.BENAMNING = TRIM(mtrltemp.BENAMNING,CHR(9)). 
         mtrltemp.BENAMNING = TRIM(mtrltemp.BENAMNING,CHR(10)). 
         mtrltemp.BENAMNING = TRIM(mtrltemp.BENAMNING,CHR(11)). 
         mtrltemp.BENAMNING = TRIM(mtrltemp.BENAMNING,CHR(12)). 
         mtrltemp.BENAMNING = TRIM(mtrltemp.BENAMNING,CHR(13)). 
         DISPLAY mtrltemp.BENAMNING  WITH BROWSE BRW_VLEV.    
      END. 
   END.      
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
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_VLEV mtrltemp.ENHET = "" THEN DO:     
         MESSAGE  "Enhet får inte vara blankt." VIEW-AS ALERT-BOX.
         DISPLAY mtrltemp.ENHET WITH BROWSE BRW_VLEV.    
         RETURN NO-APPLY.      
      END. 
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
   IF AVAILABLE mtrltemp THEN DO:
      IF mtrltemp.NPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.NPRIS THEN DO:
         mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS.           
         DISPLAY mtrltemp.NPRIS WITH BROWSE BRW_VLEV.             
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.KUND BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.KUND IN BROWSE BRW_VLEV /* Märkt */
DO:
  IF AVAILABLE mtrltemp THEN RUN visa_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.KUND BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.KUND IN BROWSE BRW_VLEV /* Märkt */
DO:
  IF AVAILABLE mtrltemp THEN DO:
      IF mtrltemp.KUND NE INPUT BROWSE BRW_VLEV mtrltemp.KUND THEN DO:
         mtrltemp.KUND = INPUT BROWSE BRW_VLEV mtrltemp.KUND.                    
         DISPLAY mtrltemp.KUND WITH BROWSE BRW_VLEV.                           
      END.
      APPLY "ROW-DISPLAY" TO BRW_VLEV IN FRAME {&FRAME-NAME} .     
       
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.KUND BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF mtrltemp.KUND IN BROWSE BRW_VLEV /* Märkt */
DO:
  IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_VLEV mtrltemp.KUND = TRUE THEN mtrltemp.KUND = FALSE.
      IF INPUT BROWSE BRW_VLEV mtrltemp.KUND = FALSE THEN mtrltemp.KUND = TRUE.        
      DISPLAY mtrltemp.KUND WITH BROWSE BRW_VLEV.          
      APPLY "ROW-DISPLAY" TO BRW_VLEV IN FRAME {&FRAME-NAME} .            
   END.
   {AMERICANEUROPEAN.I}
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLA WINDOW-1
ON CHOOSE OF BTN_ALLA IN FRAME FRAME-A /* Hela Listan */
DO:                                        
   {muswait.i}   
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   RUN hamta_UI IN bmtrlbapph (INPUT Guru.Konstanter:globanv, OUTPUT TABLE mtrltemp).    
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
      IF AVAILABLE spec_mtrl THEN DELETE spec_mtrl.
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
      status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE mtrltemp THEN DO:
         mtrl_rowid = ROWID(mtrltemp).                           
         MESSAGE "Vill du ta bort mtrl - " + mtrltemp.ENR + " " + mtrltemp.BENAMNING VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
         IF svar THEN DO TRANSACTION:
            EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
            EMPTY TEMP-TABLE emtrltemp.
            CREATE emtrltemp.
            BUFFER-COPY mtrltemp TO emtrltemp.
            sprow = ROWID(mtrltemp).
            RUN admbortmtrl_UI IN bmtrlbapph (INPUT-OUTPUT TABLE emtrltemp,OUTPUT TABLE felmeddtemp ). 
            FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
               DELETE felmeddtemp.                        
            END.            
            ELSE DO:
               EMPTY TEMP-TABLE emtrltemp.
               FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = sprow NO-ERROR.                                    
               DELETE mtrltemp.
               IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].                
            END.         
         END.
      END.
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
   RETURN NO-APPLY.                                     
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
         IF AVAILABLE mtrltemp THEN DO:            
            RUN over_UI.
         END.
         antal_raknare = antal_raknare + 1.              
      END.       
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
      RUN openbdynspec_UI IN brwproc[4].
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = spec_rowid NO-LOCK.
      IF AVAILABLE spec_mtrl THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
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
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      RUN linettomark2_UI IN nettoh (INPUT TABLE spec_mtrl, OUTPUT TABLE kmtrltemp APPEND).            
   END. 
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.   
   FOR EACH spec_mtrl:
      spec_mtrl.LEVKOD = "99".
      EMPTY TEMP-TABLE espec_mtrl NO-ERROR.      
      CREATE espec_mtrl.           
      BUFFER-COPY spec_mtrl TO espec_mtrl.       
      RUN kollvlevenr_UI IN bmtrlbapph (INPUT-OUTPUT TABLE  espec_mtrl, INPUT valomrade,INPUT valaonr, OUTPUT finns ).
      IF finns = TRUE THEN DO:
         FIND FIRST espec_mtrl NO-ERROR.
         ASSIGN 
         spec_mtrl.BENAMNING = espec_mtrl.BENAMNING
         spec_mtrl.ENHET = espec_mtrl.ENHET
         spec_mtrl.NPRIS = espec_mtrl.NPRIS
         spec_mtrl.BPRIS = espec_mtrl.BPRIS
         spec_mtrl.LEVKOD = espec_mtrl.LEVKOD.
      END.
      IF kskap = 1 THEN DO:         
         FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND 
         list_mtrl.ENR = spec_mtrl.ENR AND list_mtrl.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND 
         list_mtrl.SKAPNUM = kon_val.SKAPNUM AND 
         list_mtrl.ENR = spec_mtrl.ENR AND list_mtrl.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE list_mtrl THEN DO: 
         mtrl_rowid = ROWID(list_mtrl).
      END.
      ELSE DO:                         
         CREATE list_mtrl.
         ASSIGN
         list_mtrl.NUM = kon_val.NUM  
         list_mtrl.SKAPNUM = kon_val.SKAPNUM
         list_mtrl.ENR = spec_mtrl.ENR
         list_mtrl.BENAMNING = spec_mtrl.BENAMNING
         list_mtrl.ENHET = LC(spec_mtrl.ENHET)
         list_mtrl.PRIS = spec_mtrl.NPRIS
         list_mtrl.ANTAL = 1
         list_mtrl.LEVKOD = spec_mtrl.LEVKOD
         mtrl_rowid = ROWID(list_mtrl). 
      END.      
   END.   
    {musarrow.i}
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
   IF VALID-HANDLE(bmtrlbapph) THEN DELETE PROCEDURE bmtrlbapph.
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
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista specialmateriel").  
   RUN hamta_UI IN bmtrlbapph (INPUT Guru.Konstanter:globanv, OUTPUT TABLE mtrltemp).
   RUN enable_UI.   
   BTN_ALLA:HIDDEN = TRUE.
   {FRMSIZE.I} 
   leverant = "99" + Guru.Konstanter:globanv.    
   FIND FIRST mtrltemp  NO-LOCK NO-ERROR.
   IF AVAILABLE mtrltemp THEN RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).         
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   ASSIGN
   mtrltemp.ENR:LABEL IN BROWSE BRW_VLEV = Guru.Konstanter:genk
   spec_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.     
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN
      mtrltemp.BPRIS:VISIBLE IN BROWSE BRW_VLEV = FALSE
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_VLEV = FALSE
      spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.     
   END.   
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
         
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[4] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[4] (INPUT TRUE).      
   IF Guru.Konstanter:appcon THEN DO:
      RUN BSPECANVAPP.P PERSISTENT SET bmtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BSPECANVAPP.P PERSISTENT SET bmtrlbapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
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
         BTN_ALLA FILL-IN-ENR FILL-IN-BEN RAD_SOK FBTN_OK FBTN_AVB 
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
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = mtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
      IF AVAILABLE spec_mtrl THEN DO:
         spec_rowid = ROWID(spec_mtrl).
      END.      
      ELSE DO:
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = mtrltemp.ENR
         spec_mtrl.BENAMNING = mtrltemp.BENAMNING
         spec_mtrl.ENHET = LC(mtrltemp.ENHET)
         spec_mtrl.BERKVANT = 1
         spec_mtrl.NPRIS = mtrltemp.NPRIS 
         spec_mtrl.BPRIS = mtrltemp.BPRIS
         spec_mtrl.LEVKOD = mtrltemp.LEVKOD
         spec_mtrl.KUND = mtrltemp.KUND
         spec_rowid = ROWID(spec_mtrl).
      END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_VLEV" THEN RUN rowdispmtrl_UI (INPUT 1). 
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 2).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-1 
PROCEDURE rowdispmtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
          IF mtrltemp.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_VLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_VLEV = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_VLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_VLEV = Guru.Konstanter:varforetypval[38].          
         END.
         ELSE IF mtrltemp.KUND = FALSE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_VLEV = ?
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_VLEV = ?.          
         END.         
      END.
   END.
   IF vad = 2 THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:
          IF spec_mtrl.KUND = TRUE THEN DO: 
            ASSIGN
            spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28]
            spec_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF spec_mtrl.KUND = ? THEN DO: 
            ASSIGN
            spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38]
            spec_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].          
         END.         
      END.
   END.   

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
      mtrltemp.ENHET mtrltemp.BPRIS mtrltemp.NPRIS mtrltemp.KUND WITH BROWSE BRW_VLEV.         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

