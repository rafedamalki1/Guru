&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/25/96 -  4:43 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
&Scoped-define NEW
&Scoped-define SHARED 
{FAKTPLANTEMP.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR faktureradtemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{FAKTAONR.I}
{ANMARKTEMP.I}
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE slut AS LOGICAL NO-UNDO.
DEFINE VARIABLE varacont AS LOGICAL NO-UNDO.
DEFINE VARIABLE anmarkapph AS HANDLE NO-UNDO.
DEFINE VARIABLE brow AS ROWID NO-UNDO.
DEFINE VARIABLE vilkavar AS CHARACTER NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktaonrtemp fastanmtemp

/* Definitions for BROWSE BRW_ANM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANM faktaonrtemp.AONR faktaonrtemp.DELNR ~
faktaonrtemp.ANM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANM faktaonrtemp.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANM faktaonrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANM faktaonrtemp
&Scoped-define QUERY-STRING-BRW_ANM FOR EACH faktaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANM OPEN QUERY BRW_ANM FOR EACH faktaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANM faktaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANM faktaonrtemp


/* Definitions for BROWSE BRW_FASTANM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_FASTANM fastanmtemp.ANVANDARE ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FASTANM fastanmtemp.ANVANDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define QUERY-STRING-BRW_FASTANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FASTANM OPEN QUERY BRW_FASTANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FASTANM fastanmtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ANM}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 EDITOR_ANM FBTN_SKR BTN_AONR ~
BTN_FAST BRW_ANM BRW_FASTANM BTN_NY BTN_UPP BTN_BORT BTN_VISAO FBTN_OK ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS EDITOR_ANM FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AONR 
     IMAGE-UP FILE "BILDER\upp-u":U
     LABEL "UPP":L 
     SIZE 7 BY 2.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FAST 
     IMAGE-UP FILE "BILDER\upp-u":U
     LABEL "UPP":L 
     SIZE 7 BY 2.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDITOR_ANM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9 NO-UNDO.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Anm." 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY .83.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58.5 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANM FOR 
      faktaonrtemp SCROLLING.

DEFINE QUERY BRW_FASTANM FOR 
      fastanmtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANM DIALOG-1 _STRUCTURED
  QUERY BRW_ANM NO-LOCK DISPLAY
      faktaonrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      faktaonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      faktaonrtemp.ANM COLUMN-LABEL "Anmärkning" FORMAT "X(35)":U
            WIDTH 31
  ENABLE
      faktaonrtemp.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47 BY 8.13
         TITLE "Anmärkningar".

DEFINE BROWSE BRW_FASTANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FASTANM DIALOG-1 _STRUCTURED
  QUERY BRW_FASTANM NO-LOCK DISPLAY
      fastanmtemp.ANVANDARE FORMAT "x(12)":U
      fastanmtemp.ANMARK FORMAT "X(50)":U WIDTH 43
  ENABLE
      fastanmtemp.ANVANDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 58.5 BY 8.13
         TITLE "Fasta anmärkningar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     EDITOR_ANM AT ROW 3.67 COL 1.5 NO-LABEL
     FBTN_SKR AT ROW 8 COL 109.5
     BTN_AONR AT ROW 12.88 COL 22.25
     BTN_FAST AT ROW 13.25 COL 74.38
     BRW_ANM AT ROW 16 COL 1.5
     BRW_FASTANM AT ROW 16 COL 50.13
     BTN_NY AT ROW 24.46 COL 52.38
     BTN_UPP AT ROW 24.46 COL 66.25
     BTN_BORT AT ROW 24.46 COL 80.13
     BTN_VISAO AT ROW 24.46 COL 93.88
     FBTN_OK AT ROW 24.5 COL 109.5
     FILL-IN_SPERSONALKOD AT ROW 26.13 COL 69.38 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 26.13 COL 89.25 COLON-ALIGNED
     BTN_AVB AT ROW 26.13 COL 109.5
     "Fakturatext" VIEW-AS TEXT
          SIZE 33 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 7.25 BY .83 AT ROW 26.13 COL 50.75
     RECT-21 AT ROW 25.92 COL 50.13
     SPACE(15.36) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Medelande på Faktura".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: faktaonrtemp T "?" NO-UNDO temp-db faktaonrtemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ANM BTN_FAST DIALOG-1 */
/* BROWSE-TAB BRW_FASTANM BRW_ANM DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.faktaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktaonrtemp.AONR
"faktaonrtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktaonrtemp.DELNR
"faktaonrtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktaonrtemp.ANM
"faktaonrtemp.ANM" "Anmärkning" "X(35)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FASTANM
/* Query rebuild information for BROWSE BRW_FASTANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ANVANDARE
"fastanmtemp.ANVANDARE" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.fastanmtemp.ANMARK
"fastanmtemp.ANMARK" ? "X(50)" "character" ? ? ? ? ? ? no ? no no "43" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FASTANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Medelande på Faktura */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Medelande på Faktura */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AONR DIALOG-1
ON CHOOSE OF BTN_AONR IN FRAME DIALOG-1 /* UPP */
DO:
   RUN anm_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FAST DIALOG-1
ON CHOOSE OF BTN_FAST IN FRAME DIALOG-1 /* UPP */
DO:
   RUN anmfast_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   RUN ny_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISAO DIALOG-1
ON CHOOSE OF BTN_VISAO IN FRAME DIALOG-1 /* Visa */
DO:
   RUN visao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_ANM DIALOG-1
ON LEAVE OF EDITOR_ANM IN FRAME DIALOG-1
DO:
   EDITOR_ANM = INPUT EDITOR_ANM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK DIALOG-1
ON CHOOSE OF FBTN_OK IN FRAME DIALOG-1 /* Ok */
DO:    
   EDITOR_ANM = INPUT EDITOR_ANM.
   RUN slutordehm_UI IN fakthmth 
   (INPUT infakplannr,
    INPUT fdelnrvar,
    INPUT EDITOR_ANM, 
    INPUT EDITOR_ANM:NUM-LINES,
    OUTPUT slut,
    OUTPUT varacont).   
   ASSIGN
   faktureradtemp.FAKTXT = EDITOR_ANM
   faktureradtemp.ANTALRADER = EDITOR_ANM:NUM-LINES. 
   {BORTBRWPROC.I}
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-1
ON CHOOSE OF FBTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).          
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Anm. */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* Anm. */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_FASTANM.
   
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SFORNAMN + '*'.   
   FIND fastanmtemp WHERE ROWID(fastanmtemp) = brow NO-LOCK NO-ERROR.
   FIND NEXT fastanmtemp WHERE fastanmtemp.PROGRAM = "FAKT" AND fastanmtemp.ANMARK MATCHES posok       
   NO-LOCK NO-ERROR.        
   IF NOT AVAILABLE fastanmtemp THEN DO:     
      FIND FIRST fastanmtemp WHERE fastanmtemp.PROGRAM = "FAKT" AND fastanmtemp.ANMARK MATCHES posok       
      NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE fastanmtemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE fastanmtemp THEN DO:      
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[2].      
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Användare */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Användare */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_FASTANM.
   brow = ROWID(fastanmtemp).
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SPERSONALKOD + '*'.   
   FIND fastanmtemp WHERE ROWID(fastanmtemp) = brow NO-LOCK NO-ERROR.
   FIND NEXT fastanmtemp WHERE fastanmtemp.PROGRAM = "FAKT" AND
   fastanmtemp.ANVANDARE MATCHES posok       
   NO-LOCK NO-ERROR.
     
   IF NOT AVAILABLE fastanmtemp THEN DO:     
      FIND FIRST fastanmtemp WHERE fastanmtemp.PROGRAM = "FAKT" AND 
      fastanmtemp.ANVANDARE MATCHES posok       
      NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE fastanmtemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE fastanmtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[2].      
      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANM
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}      
   {ALLSTARTDYN.I}
   faktaonrtemp.AONR:LABEL IN BROWSE BRW_ANM = Guru.Konstanter:gaok.
   FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND faktureradtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
   EDITOR_ANM = faktureradtemp.FAKTXT.    
   RUN startfakmed_UI IN fakthmth (INPUT infakplannr,INPUT fdelnrvar,OUTPUT TABLE faktaonrtemp).   
   RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
   RUN openbdyn_UI IN brwproc[2] (INPUT vilkavar).
   RUN enable_UI.       
   {FRMSIZED.I}  
   GET FIRST {&BROWSE-NAME} NO-LOCK.
   IF NOT AVAILABLE fastanmtemp THEN DO:
      ASSIGN        
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FAST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   vilkavar = ' WHERE fastanmtemp.PROGRAM = "FAKT" '.   
   faktaonrtemp.AONR:READ-ONLY IN BROWSE BRW_ANM = TRUE.    
   fastanmtemp.ANVANDARE:READ-ONLY IN BROWSE BRW_FASTANM = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_ANM:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_FASTANM:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv THEN DO:
      {muswait.i}
      brow = fastanmtemp.FASTANVROW.
      RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT fastanmtemp.FASTANVROW).
      RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
      RUN openbdyn_UI IN brwproc[2] (INPUT vilkavar).      
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[2].            
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ändra andras sparade anmärkningar!"
      VIEW-AS ALERT-BOX.            
   END.   
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anmfast_UI DIALOG-1 
PROCEDURE anmfast_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EDITOR_ANM = INPUT FRAME {&FRAME-NAME} EDITOR_ANM.
   status-ok = BRW_ANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   APPLY "VALUE-CHANGED" TO BRW_ANM.
   IF EDITOR_ANM = "" THEN EDITOR_ANM = fastanmtemp.ANMARK.
   ELSE EDITOR_ANM = EDITOR_ANM + CHR(10) + fastanmtemp.ANMARK.
   DISPLAY EDITOR_ANM WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anm_UI DIALOG-1 
PROCEDURE anm_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   EDITOR_ANM = INPUT FRAME {&FRAME-NAME} EDITOR_ANM.
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF status-ok = FALSE THEN RETURN.
   IF EDITOR_ANM = "" THEN EDITOR_ANM = faktaonrtemp.ANM.
   ELSE EDITOR_ANM = EDITOR_ANM + CHR(10) + faktaonrtemp.ANM.
   DISPLAY EDITOR_ANM WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv THEN DO:
      MESSAGE "Vill du verkligen ta bort denna fasta anmärkning?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE fastanmtemp.ANMARK
      UPDATE answer AS LOGICAL.
      IF answer THEN DO TRANSACTION:
         {muswait.i}
         RUN bortanm IN anmarkapph (INPUT fastanmtemp.FASTANVROW).    
         DELETE fastanmtemp.      
         RUN selnextprevrow_UI IN brwproc[2].
         
         /*RUN refreshbrw_UI IN brwproc[2].*/
         RUN refreshbrw_UI IN brwproc[2].
         RUN lastselectdyn_UI IN brwproc[2].              
         IF NOT AVAILABLE fastanmtemp THEN DO:
            ASSIGN        
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_FAST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
      END.
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ta bort andras sparade anmärkningar!"
      VIEW-AS ALERT-BOX.      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY EDITOR_ANM FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-21 EDITOR_ANM FBTN_SKR BTN_AONR BTN_FAST BRW_ANM BRW_FASTANM 
         BTN_NY BTN_UPP BTN_BORT BTN_VISAO FBTN_OK FILL-IN_SPERSONALKOD 
         FILL-IN_SFORNAMN BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}  
   RUN nyanm IN anmarkapph (INPUT "",INPUT Guru.Konstanter:globanv,
                               INPUT Guru.Konstanter:globomr,INPUT "FAKT",OUTPUT brow,
                               OUTPUT TABLE fastanmtemp APPEND).
   FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR. 
   
   RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT fastanmtemp.FASTANVROW).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      ASSIGN        
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_VISAO:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FAST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
      RUN openbdyn_UI IN brwproc[2] (INPUT vilkavar).
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[2].      
   END.    
   ELSE DO TRANSACTION:
      RUN bortanm IN anmarkapph (INPUT fastanmtemp.FASTANVROW). 
      DELETE fastanmtemp.       
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {PRINTSTAENDE.I}             
   DISPLA EDITOR_ANM AT 6 VIEW-AS EDITOR SIZE 50 BY 9 NO-LABEL.
   OUTPUT CLOSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visao_UI DIALOG-1 
PROCEDURE visao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   {muswait.i}
   RUN ANDRAMARK.W (INPUT fastanmtemp.ANMARK, INPUT ?).   
   {musarrow.i}       
   musz = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

