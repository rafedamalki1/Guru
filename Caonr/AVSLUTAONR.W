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
{DIRDEF.I}
  
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdaao.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTAONRTEMP.I}
&Scoped-define SHARED SHARED
{AONRDEF.I}
{HOPPSEK2W.I}
 

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjdelvar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE restbdatum AS DATE NO-UNDO. 
DEFINE VARIABLE avslutapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{EXTRATAB.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdaao aoresttemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR valdaao.OMRADE valdaao.AONR ~
valdaao.DELNR valdaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR valdaao.ORT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR valdaao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR valdaao
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH valdaao NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH valdaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONR valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR valdaao


/* Definitions for BROWSE BRW_REST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_REST aoresttemp.AONR aoresttemp.DELNR ~
aoresttemp.DATUM aoresttemp.PLANMONT aoresttemp.PLANMASK aoresttemp.EA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_REST aoresttemp.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_REST aoresttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_REST aoresttemp
&Scoped-define QUERY-STRING-BRW_REST FOR EACH aoresttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_REST OPEN QUERY BRW_REST FOR EACH aoresttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_REST aoresttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_REST aoresttemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_AVSAONR BTN_AVB 

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
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_AVSAONR 
     LABEL "Avsluta aonr":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_RAPP 
     LABEL "Rapporter":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_REST 
     LABEL "Restrapportera":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISAV 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE TOG_REST AS LOGICAL INITIAL no 
     LABEL "Visa restrapporterade" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.88 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      valdaao SCROLLING.

DEFINE QUERY BRW_REST FOR 
      aoresttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR WINDOW-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      valdaao.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      valdaao.ORT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 62.38 BY 14.75.

DEFINE BROWSE BRW_REST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_REST WINDOW-1 _STRUCTURED
  QUERY BRW_REST NO-LOCK DISPLAY
      aoresttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      aoresttemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      aoresttemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      aoresttemp.PLANMONT COLUMN-LABEL "Förbrukad!Montörstid" FORMAT "->>>>9":U
      aoresttemp.PLANMASK COLUMN-LABEL "Förbrukad!Maskintid" FORMAT "->>>>9":U
      aoresttemp.EA COLUMN-LABEL "Förbrukad!EA-mängd" FORMAT "->>>>9":U
  ENABLE
      aoresttemp.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 53.5 BY 14.67
         TITLE "Restrapporterade aonr".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     TOG_REST AT ROW 2 COL 1.5
     BRW_AONR AT ROW 2.96 COL 1.5
     BRW_REST AT ROW 2.96 COL 1.5
     FBTN_RAPP AT ROW 8 COL 65
     FBTN_AVSAONR AT ROW 9.1 COL 65
     FBTN_VISAV AT ROW 10.2 COL 65
     FBTN_REST AT ROW 11.3 COL 65
     BTN_AVB AT ROW 17.83 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.38 BY 18.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: aoresttemp T "?" NO-UNDO temp-db aoresttemp
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Avsluta arbetsorder"
         HEIGHT             = 18.25
         WIDTH              = 80
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
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
                                                                        */
/* BROWSE-TAB BRW_AONR TOG_REST FRAME-A */
/* BROWSE-TAB BRW_REST BRW_AONR FRAME-A */
/* SETTINGS FOR BROWSE BRW_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

/* SETTINGS FOR BROWSE BRW_REST IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON FBTN_RAPP IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_RAPP:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_REST IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_REST:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISAV IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISAV:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_REST IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_REST:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.valdaao.OMRADE
"valdaao.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaao.DELNR
"valdaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_REST
/* Query rebuild information for BROWSE BRW_REST
     _TblList          = "Temp-Tables.aoresttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aoresttemp.AONR
"aoresttemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.aoresttemp.DELNR
"aoresttemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.aoresttemp.DATUM
"aoresttemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.aoresttemp.PLANMONT
"aoresttemp.PLANMONT" "Förbrukad!Montörstid" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.aoresttemp.PLANMASK
"aoresttemp.PLANMASK" "Förbrukad!Maskintid" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.aoresttemp.EA
"aoresttemp.EA" "Förbrukad!EA-mängd" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_REST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVSAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVSAONR WINDOW-1
ON CHOOSE OF FBTN_AVSAONR IN FRAME FRAME-A /* Avsluta aonr */
DO:           
   {muswait.i}
   RUN avsao_UI.    
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_RAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_RAPP WINDOW-1
ON CHOOSE OF FBTN_RAPP IN FRAME FRAME-A /* Rapporter */
DO:
   {muswait.i}
   {AVBGOM.I}
   RUN AORAPPORT.W.
   {AVBFRAM.I}
   {musarrow.i} 
   IF vartpro NE "" THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_REST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_REST WINDOW-1
ON CHOOSE OF FBTN_REST IN FRAME FRAME-A /* Restrapportera */
DO:           
   RUN restrapp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAV WINDOW-1
ON CHOOSE OF FBTN_VISAV IN FRAME FRAME-A /* Visa */
DO:
   {muswait.i}
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_raknare = 1.
   DO TRANSACTION:
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).       
         aonrrec = valdaao.AONRREC.
         CREATE aonrtemp.
         ASSIGN 
         aonrtemp.AONR = valdaao.AONR
         aonrtemp.DELNR = valdaao.DELNR
         aonrtemp.OMRADE = valdaao.OMRADE
         aonrtemp.AONRTABREC = valdaao.AONRREC.            
         antal_raknare = antal_raknare + 1.
      END.
   END.        
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "cSUND" THEN DO:      
      RUN AORUBRIK.W.
   END.
   {AVBGOM.I}      
   {AMERICANEUROPEAN.I} 
   RUN VISAAONR.W.   
   {EUROPEANAMERICAN.I}
   EMPTY TEMP-TABLE aonrtemp NO-ERROR.    
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_REST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_REST WINDOW-1
ON VALUE-CHANGED OF TOG_REST IN FRAME FRAME-A /* Visa restrapporterade */
DO:
   TOG_REST = INPUT TOG_REST.
   IF TOG_REST = TRUE THEN DO:
      ASSIGN
      BRW_AONR:HIDDEN = TRUE
      FBTN_AVSAONR:HIDDEN = TRUE 
      FBTN_RAPP:HIDDEN = TRUE 
      FBTN_REST:HIDDEN = TRUE        
      FBTN_VISAV:HIDDEN = TRUE.            
      ENABLE BRW_REST WITH FRAME {&FRAME-NAME}. 
      RUN openbdynspec_UI IN brwproc[2].
   END.    
   ELSE DO:          
      ASSIGN      
      BRW_AONR:HIDDEN = FALSE
      FBTN_AVSAONR:HIDDEN = FALSE 
      FBTN_RAPP:HIDDEN = FALSE               
      FBTN_VISAV:HIDDEN = FALSE 
      BRW_REST:HIDDEN = TRUE.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GKAL"    THEN DO:
         IF MONTH(TODAY) = 12 THEN DO: 
            IF TODAY >= DATE(12,15,YEAR(TODAY)) AND TODAY <= DATE(12,31,YEAR(TODAY)) THEN musz = TRUE.
         END. 
         IF MONTH(TODAY) = 01 THEN DO: 
            IF TODAY >= DATE(01,01,YEAR(TODAY)) AND TODAY <= DATE(01,10,YEAR(TODAY)) THEN musz = TRUE.
         END.            IF musz = TRUE THEN DO:
            ASSIGN
            FBTN_REST:HIDDEN = FALSE.          
            ENABLE FBTN_REST WITH FRAME {&FRAME-NAME}.
         END.
         ELSE DO:
            ASSIGN
            FBTN_REST:HIDDEN = TRUE.            
         END.
      END.
      ELSE DO:
         ASSIGN
         FBTN_REST:HIDDEN = TRUE.         
      END.
      musz = FALSE.    
      RUN sek_UI.           
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(avslutapph) THEN DELETE PROCEDURE avslutapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   RUN laddabrw IN avslutapph (OUTPUT TABLE aoresttemp).
   FIND FIRST aoresttemp NO-LOCK NO-ERROR.
   ASSIGN
   WINDOW-1:TITLE = "Avsluta " + LC(Guru.Konstanter:gaol)
   FBTN_AVSAONR:LABEL = "Avsluta " + LC(Guru.Konstanter:gaok)
   FBTN_VISAV:LABEL = "Visa " + LC(Guru.Konstanter:gaok) 
   valdaao.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok
   valdaao.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   aoresttemp.AONR:LABEL IN BROWSE BRW_REST = Guru.Konstanter:gaok
   BRW_REST:TITLE = "Restrapporterade " + LC(Guru.Konstanter:gaol).
   &Scoped-define FORMATNAMN valdaao.AONR
   &Scoped-define BROWSE-NAME BRW_AONR
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN aoresttemp.AONR
   &Scoped-define BROWSE-NAME BRW_REST
   {AOFORMAT1.I}
   RUN enable_UI.   
   {FRMSIZE.I}  
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK"   OR 
   Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GKAL" 
     THEN DO:
      IF MONTH(TODAY) = 12 THEN DO: 
         IF TODAY >= DATE(12,15,YEAR(TODAY)) AND TODAY <= DATE(12,31,YEAR(TODAY)) 
         THEN musz = TRUE.         
      END. 
      IF MONTH(TODAY) = 01 THEN DO: 
         IF TODAY >= DATE(01,01,YEAR(TODAY)) AND TODAY <= DATE(01,10,YEAR(TODAY)) 
         THEN musz = TRUE.
      END.   
      IF musz = TRUE THEN ENABLE FBTN_REST WITH FRAME {&FRAME-NAME}.
      musz = FALSE.
      ENABLE TOG_REST WITH FRAME {&FRAME-NAME}.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   FIND valdaao WHERE valdaao.AONRREC  = aonrrec NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:  
      IF valdaao.AONRAVDATUM = 01/01/91 THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(valdaao)).              
         RUN lastselectdyn_UI IN brwproc[1].       
      END.
   END.
   RUN sek_UI.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI WINDOW-1 
PROCEDURE allaao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   valdaao.ORT:READ-ONLY IN BROWSE  BRW_AONR = TRUE
   aoresttemp.AONR:READ-ONLY IN BROWSE BRW_REST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_REST:HANDLE IN FRAME {&FRAME-NAME}).
    RUN setcolsortvar_UI IN brwproc[1] (INPUT "valdaao.AONRAVDATUM = 91/01/01").
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVSLUTAPP.P PERSISTENT SET avslutapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVSLUTAPP.P PERSISTENT SET avslutapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsao_UI WINDOW-1 
PROCEDURE avsao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0  THEN DO:         
      MESSAGE "Markera vad som skall avslutas !" VIEW-AS ALERT-BOX. 
      RETURN.
   END.
   antal_raknare = 1.
   regdatum = TODAY.
   
   RUN AVDATUM.W.
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.     
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).       
      IF antal_raknare = antal_valda THEN DO:
         RUN selnextprevrow_UI IN brwproc[1].          
      END.
      ASSIGN
      aonrrec = valdaao.AONRREC
      aonrvar = valdaao.AONR
      delnrvar = valdaao.DELNR.
      RUN koll IN avslutapph (INPUT aonrvar,INPUT delnrvar,INPUT Guru.Konstanter:globforetag,INPUT regdatum,OUTPUT TABLE felmeddtemp).   
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         IF felmeddtemp.VAL = 1 THEN DO:
            /*Går ej att avsluta p.g.a tidskrivning*/
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX  
            QUESTION BUTTONS YES-NO TITLE Guru.Konstanter:gaok + " " + aonrvar + " " + STRING(delnrvar) UPDATE val1 AS LOGICAL .
            DELETE felmeddtemp.
            CASE val1:
               WHEN TRUE THEN DO:
                  /*För att se övriga personer med tidskrivning*/
                  
                  RUN PERSAVSU.W.
                  
               END.
            END CASE.                           
         END.
         ELSE IF felmeddtemp.VAL = 2 THEN DO:
            /*Avsluta samtliga delnr?*/
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO TITLE Guru.Konstanter:gaok + " " + aonrvar + " " + STRING(delnrvar) UPDATE val2 AS LOGICAL.
            DELETE felmeddtemp.
            CASE val2:
               WHEN TRUE THEN DO:
                  ASSIGN
                  aonrvar = valdaao.AONR
                  delnrvar = valdaao.DELNR.
                  RUN allaao IN avslutapph (INPUT aonrvar,INPUT delnrvar,
                                            INPUT Guru.Konstanter:globforetag,INPUT regdatum, INPUT Guru.Konstanter:globanv).
                  IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
                    /*AVSLUT FBAONR*/
                     RUN fballaao IN avslutapph (INPUT aonrvar,INPUT delnrvar,INPUT Guru.Konstanter:globforetag,INPUT regdatum).                                 
                  END.
                  FOR EACH valdaao WHERE valdaao.AONR = aonrvar :
                     IF valdaao.AONRAVDATUM = 01/01/91 THEN DO:
                        ASSIGN 
                        valdaao.AONRAVDATUM = regdatum
                        valdaao.AUTOREG = TRUE.      
                     END.
                  END.  
               END.
               WHEN FALSE THEN DO:
                  MESSAGE "Inget avslutades!" VIEW-AS ALERT-BOX TITLE Guru.Konstanter:gaok + " " + aonrvar + " " + STRING(delnrvar).                                     
               END.
            END CASE. 
         END.
         ELSE DO:
            MESSAGE felmeddtemp.FELMEDD " Ring Elpool 090/184540"  VIEW-AS ALERT-BOX.            
         END.
      END.
      ELSE DO:
         ASSIGN 
         valdaao.AONRAVDATUM = regdatum
         valdaao.AUTOREG = TRUE.         
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
           /*AVSLUT FBAONR*/
            RUN fballaao IN avslutapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,INPUT Guru.Konstanter:globforetag,INPUT regdatum).                                 
         END.
      END.
      antal_raknare = antal_raknare + 1.      
   END.
   RUN relaonr IN avslutapph.
   RUN vaonrfix IN avslutapph (INPUT-OUTPUT TABLE valdaao).
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN lastselectdyn_UI IN brwproc[1]. 
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
  ENABLE FBTN_AVSAONR BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalk_UI WINDOW-1 
PROCEDURE kalk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollok_UI WINDOW-1 
PROCEDURE kollok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koll_UI WINDOW-1 
PROCEDURE koll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restrapp_UI WINDOW-1 
PROCEDURE restrapp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "ELPA" THEN musz = musz.
   ELSE DO:
      RUN sumtidkoll IN avslutapph (OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.
         RETURN.
      END.      
   END.         
   {muswait.i}   
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN
      status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).       
      aonrrec = valdaao.AONRREC.       
      FIND LAST aoresttemp WHERE aoresttemp.AONR = valdaao.AONR AND
      aoresttemp.DELNR = valdaao.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.            
      IF AVAILABLE aoresttemp THEN DO:
         IF MONTH(TODAY) = 12 AND YEAR(aoresttemp.DATUM) = YEAR(TODAY) THEN musz = TRUE.
         IF MONTH(TODAY) = 1 AND YEAR(aoresttemp.DATUM) = YEAR(TODAY) - 1 THEN musz = TRUE.
         IF musz = TRUE THEN DO:
            MESSAGE valdaao.AONR STRING(valdaao.DELNR,Guru.Konstanter:varforetypchar[1])  
            " Är redan restrapporterad." VIEW-AS ALERT-BOX.                      
         END.                
      END.
      ELSE DO:
         RUN restrapp IN avslutapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,
                                     INPUT restbdatum,OUTPUT TABLE felmeddtemp).
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddtemp.
            musz = TRUE.
         END.         
      END.
     /* RUN SUCCREST.P (INPUT AONRTAB.AONR, INPUT AONRTAB.DELNR).      */
      antal_raknare = antal_raknare + 1.
   END.     
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI WINDOW-1 
PROCEDURE sek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:aonrsekvar[4] = TRUE THEN ENABLE FBTN_VISAV WITH FRAME {&FRAME-NAME}.     
   IF Guru.Konstanter:aonrsekvar[7] = TRUE THEN ENABLE FBTN_RAPP WITH FRAME {&FRAME-NAME}.                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

