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

  Created: 95/05/02 - 12:41 pm

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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER aodelnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
{MARKVAL.I} 

DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vardrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE skogrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skogrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE volrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE volrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE akerrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE akerrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE kabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE kabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastvrec AS RECID NO-UNDO. 

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE ordnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE frannr AS INTEGER NO-UNDO. 
DEFINE VARIABLE till_recid AS RECID NO-UNDO. 
DEFINE VARIABLE ponr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mall AS INTEGER NO-UNDO.
DEFINE VARIABLE sokhitta AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokenamn AS INTEGER NO-UNDO.
DEFINE VARIABLE markurvalapph AS HANDLE NO-UNDO.
{MAGA.I}
/*DEFINE TEMP-TABLE maga                       
   FIELD VARDNR AS INTEGER                   
   FIELD MARKNR AS INTEGER                   
   FIELD PERSONNUMMER AS CHARACTER           
   FIELD PNR2 AS CHARACTER                   
   FIELD MARKAGARE AS CHARACTER              
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER             
   FIELD BETECKNING AS CHARACTER             
   FIELD PROCENT AS INTEGER                  
   FIELD ANAMN AS CHARACTER                  
   FIELD AADRESS AS CHARACTER                
   FIELD APONR AS CHARACTER                  
   FIELD APADRESS AS CHARACTER               
   FIELD APERNR AS CHARACTER 
   FIELD ORDNING AS INTEGER
   FIELD MARKREC AS RECID                    
   INDEX MARKNR IS PRIMARY MARKNR ASCENDING
   INDEX ORDN ORDNING ASCENDING.*/


/* DEFINE TEMP-TABLE aovardtemp NO-UNDO                             */
/*    FIELD AONR        AS CHARACTER  FORMAT "x(6)" INITIAL ?       */
/*    FIELD DELNR       AS INTEGER    FORMAT 999    INITIAL ?       */
/*    FIELD VARDNR      AS INTEGER    FORMAT ">>>9" INITIAL ?      */
/*    FIELD OFFNR       AS INTEGER    FORMAT "->>>>>>9" INITIAL 0 */
/*    INDEX AONR IS PRIMARY AONR DELNR                              */
/*    INDEX OFFNR OFFNR                                             */
/*    INDEX VARDNR VARDNR.                                          */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MARK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES maga

/* Definitions for BROWSE BRW_MARK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MARK maga.MARKNR maga.PERSONNUMMER ~
maga.MARKAGARE maga.BETECKNING maga.GATUADRESS maga.POSTNUMMER ~
maga.POSTADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MARK maga.MARKNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MARK maga
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MARK maga
&Scoped-define QUERY-STRING-BRW_MARK FOR EACH maga NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MARK OPEN QUERY BRW_MARK FOR EACH maga NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MARK maga
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MARK maga


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_ALLAMA CMB_VARDVAL BRW_MARK BTN_ORDUPP ~
BTN_ORDNER BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VARDNR FILL-IN_BENAMNING ~
FILL-IN_AONR FILL-IN_DELNR TOG_ALLAMA CMB_VARDVAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.08.

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4.63 BY 1.83.

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4.63 BY 1.83.

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_FASTFOR 
     LABEL "mall":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_VARDVAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj word/excel-mall" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 25.63 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE TOG_ALLAMA AS LOGICAL INITIAL no 
     LABEL "Alla markägare":L 
     VIEW-AS TOGGLE-BOX
     SIZE 22.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MARK FOR 
      maga SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MARK WINDOW-1 _STRUCTURED
  QUERY BRW_MARK NO-LOCK DISPLAY
      maga.MARKNR COLUMN-LABEL "Marknr" FORMAT ">>>>9":U
      maga.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      maga.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "X(40)":U
      maga.BETECKNING COLUMN-LABEL "Beteckning" FORMAT "X(25)":U
      maga.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(25)":U
      maga.POSTNUMMER COLUMN-LABEL "Postnummer" FORMAT "XXX XX":U
      maga.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(20)":U
  ENABLE
      maga.MARKNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 104.5 BY 15.5
         TITLE "Markägare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN_VARDNR AT ROW 1.71 COL 13.75 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 1.71 COL 25.88 COLON-ALIGNED NO-LABEL
     FILL-IN_AONR AT ROW 3.08 COL 13.75 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 3.13 COL 28.75 COLON-ALIGNED
     TOG_ALLAMA AT ROW 4.58 COL 1.5
     CMB_VARDVAL AT ROW 4.67 COL 78.13 COLON-ALIGNED
     BRW_MARK AT ROW 5.88 COL 1.38
     FBTN_FASTFOR AT ROW 5.92 COL 111.5
     FBTN_VISA AT ROW 7 COL 111.5
     FBTN_SKRIV AT ROW 8.08 COL 111.5
     FBTN_EX AT ROW 9.21 COL 111.5
     BTN_ORDUPP AT ROW 9.83 COL 106.38
     BTN_ORDNER AT ROW 12.75 COL 106.38
     BTN_AVB AT ROW 21.42 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 21.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db maga
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Markvärdering"
         HEIGHT             = 22.17
         WIDTH              = 125
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 27.25
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
/* BROWSE-TAB BRW_MARK CMB_VARDVAL FRAME-A */
ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_EX IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_EX:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_FASTFOR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_FASTFOR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_SKRIV IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_SKRIV:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BENAMNING IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MARK
/* Query rebuild information for BROWSE BRW_MARK
     _TblList          = "Temp-Tables.maga"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.maga.MARKNR
"MARKNR" "Marknr" ">>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.maga.PERSONNUMMER
"PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.maga.MARKAGARE
"MARKAGARE" "Markägare" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.maga.BETECKNING
"BETECKNING" "Beteckning" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.maga.GATUADRESS
"GATUADRESS" "Gatuadress" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.maga.POSTNUMMER
"POSTNUMMER" "Postnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.maga.POSTADRESS
"POSTADRESS" "Postadress" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MARK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   musz = TRUE.
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDNER WINDOW-1
ON CHOOSE OF BTN_ORDNER IN FRAME FRAME-A /* Ner */
DO:   
   RUN btnordner_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDUPP WINDOW-1
ON CHOOSE OF BTN_ORDUPP IN FRAME FRAME-A
DO:
   RUN btnordupp_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX WINDOW-1
ON CHOOSE OF FBTN_EX IN FRAME FRAME-A /* Visa I EXCEL */
DO:   
   visvalvar = 2.
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE. 
   RUN VISAMARKV.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FASTFOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FASTFOR WINDOW-1
ON CHOOSE OF FBTN_FASTFOR IN FRAME FRAME-A /* mall */
DO:    
   {muswait.i}         
   CMB_VARDVAL = INPUT CMB_VARDVAL.
   IF CMB_VARDVAL = "Fastighetsförteckning"  THEN mall = 1.
   IF CMB_VARDVAL = "Fastighetsförteckning lsp"  THEN mall = 2.   
   IF CMB_VARDVAL = "Etikett" THEN mall = 3.
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.    
   IF mall = 1 OR mall = 2  THEN DO:   
      RUN PROJNATAG.W (OUTPUT ponr).      
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO: 
         IF mall = 1 THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 1).         
         ELSE IF mall = 2  THEN RUN EXFASTFOR.P (INPUT ponr,INPUT 2).
      END.
   END.
   IF mall = 3  THEN DO:            
      RUN EXETIKETT.P .   
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   visvalvar = 1.
   RUN valda_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.             
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).  
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
      RETURN NO-APPLY.
   END.                 
   RUN VISAMARKV.W.   
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


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:   
   visvalvar = 1.
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE. 
   RUN VISAMARKV.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLAMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLAMA WINDOW-1
ON VALUE-CHANGED OF TOG_ALLAMA IN FRAME FRAME-A /* Alla markägare */
DO:
   TOG_ALLAMA = INPUT TOG_ALLAMA.
   IF TOG_ALLAMA THEN DO:
      APPLY "CTRL-A" TO BRW_MARK.
   END.
   ELSE DO:
       status-ok = BRW_MARK:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.  
   END.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MARK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(framesizeh) THEN DELETE PROCEDURE framesizeh.
   IF VALID-HANDLE(markurvalapph) THEN DELETE PROCEDURE markurvalapph.
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
   &Scoped-define FORMATNAMN FILL-IN_AONR
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   EMPTY TEMP-TABLE maga NO-ERROR.    
   RUN ladda IN markurvalapph (INPUT valvardnr,INPUT aodelnr,
                               OUTPUT TABLE varderingtemp,
                               OUTPUT TABLE maga,
                               OUTPUT TABLE aovardtemp).                               

   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN vardrec = RECID(varderingtemp).
   FIND varderingtemp WHERE RECID(varderingtemp) = vardrec NO-LOCK NO-ERROR.
   ASSIGN FILL-IN_VARDNR = varderingtemp.VARDNR
   FILL-IN_BENAMNING = varderingtemp.BENAMNING.   
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE aovardtemp THEN DO:
      ASSIGN FILL-IN_AONR = aovardtemp.AONR
      FILL-IN_DELNR = aovardtemp.DELNR.
      IF aodelnr = 1 THEN FILL-IN_DELNR = 0.
   END.
   ELSE DO:
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.         
   ASSIGN TOG_ALLAMA = FALSE.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.   
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning").   
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning lsp").      
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").           
   END.
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK"
   OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:         
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning").   
      status-ok = CMB_VARDVAL:ADD-LAST("Fastighetsförteckning lsp").      
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").           
   END.
   ELSE DO:        
      status-ok = CMB_VARDVAL:ADD-LAST("Etikett").           
   END.
   RUN enable_UI.   
   FBTN_FASTFOR:LABEL = "Visa word/excel-mall".     
   
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" OR Guru.Konstanter:globforetag = "ELPA" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning".         
   ELSE IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK"
   OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" THEN CMB_VARDVAL:SCREEN-VALUE = "Fastighetsförteckning".         
   ELSE CMB_VARDVAL:SCREEN-VALUE = "Etikett".            
   FBTN_FASTFOR:HIDDEN = FALSE.
   ENABLE FBTN_FASTFOR WITH FRAME {&FRAME-NAME}.

   
   {FRMSIZE.I}  
   ENABLE BRW_MARK  WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_MARK:HIDDEN = FALSE.
   TOG_ALLAMA = FALSE.  
   FIND FIRST aovardtemp WHERE aovardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aovardtemp THEN DO:    
      FILL-IN_AONR:HIDDEN = TRUE.
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.         
   /*OPEN QUERY BRW_MARK FOR EACH maga USE-INDEX MARKNR NO-LOCK.*/
   OPEN QUERY BRW_MARK FOR EACH maga NO-LOCK BY maga.BETECKNING BY maga.MARKNR.
   FIND FIRST maga USE-INDEX MARKNR NO-LOCK NO-ERROR.
   ENABLE  
   FBTN_SKRIV FBTN_VISA FBTN_EX WITH FRAME {&FRAME-NAME}.       
   musz = FALSE.
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
   ASSIGN
   maga.MARKNR:READ-ONLY IN BROWSE BRW_MARK = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MARK:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_MARK:HANDLE,INPUT "Sök telefonnummer via Internet",INPUT "infotel_UI").  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MARKURVALAPP.P PERSISTENT SET markurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MARKURVALAPP.P PERSISTENT SET markurvalapph.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordner_UI WINDOW-1 
PROCEDURE btnordner_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   FIND FIRST maga NO-LOCK NO-ERROR.
   IF AVAILABLE maga THEN DO:
      ASSIGN
      status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}
      frannr = maga.ORDNING
      till_recid = RECID(maga)      
      status-ok = BRW_MARK:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = maga.ORDNING
         maga.ORDNING = frannr.   
         FIND maga WHERE RECID(maga) = till_recid.
         maga.ORDNING = tillnr.
         OPEN QUERY BRW_MARK FOR EACH maga USE-INDEX ORD NO-LOCK.
         FIND maga WHERE RECID(maga) = till_recid NO-LOCK NO-ERROR.
         IF AVAILABLE maga THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(maga)).   
            RUN lastselectdyn_UI IN brwproc[1].           
         END.
      END.   
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordupp_UI WINDOW-1 
PROCEDURE btnordupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   FIND FIRST maga NO-LOCK NO-ERROR.
   IF AVAILABLE maga THEN DO:
      ASSIGN
      status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}
      frannr = maga.ORDNING
      till_recid = RECID(maga)
      status-ok = BRW_MARK:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = maga.ORDNING
         maga.ORDNING = frannr.   
         FIND maga WHERE RECID(maga) = till_recid.
         maga.ORDNING = tillnr.
         OPEN QUERY BRW_MARK FOR EACH maga USE-INDEX ORD NO-LOCK.
         FIND maga WHERE RECID(maga) = till_recid NO-LOCK NO-ERROR.
         IF AVAILABLE maga THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(maga)).   
            RUN lastselectdyn_UI IN brwproc[1].           
         END.
      END.   
   END.   
   {musarrow.i}   
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
  DISPLAY FILL-IN_VARDNR FILL-IN_BENAMNING FILL-IN_AONR FILL-IN_DELNR TOG_ALLAMA 
          CMB_VARDVAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE TOG_ALLAMA CMB_VARDVAL BRW_MARK BTN_ORDUPP BTN_ORDNER BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infotel_UI WINDOW-1 
PROCEDURE infotel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  status-ok = BRW_MARK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok THEN DO:   
      sokenamn = INDEX(maga.MARKAGARE,",").
      IF sokenamn > 0 THEN sokhitta =  SUBSTRING(maga.MARKAGARE,1,(sokenamn - 1)) + " " +   maga.GATUADRESS + " " + maga.POSTADRESS.
      ELSE sokhitta =  maga.MARKAGARE + " " +   maga.GATUADRESS + " " + maga.POSTADRESS.       
      {HITTALANK.I} 
   END.   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI WINDOW-1 
PROCEDURE valda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE markval NO-ERROR. 
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen markägare är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   antal_raknare = 1.
   GET FIRST BRW_MARK.
   DO WHILE AVAILABLE(maga):
      maga.ORDNING = antal_raknare.
      antal_raknare = antal_raknare + 1.
      GET NEXT BRW_MARK.
   END.
   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).      
      CREATE markval.
      BUFFER-COPY maga TO markval.
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

