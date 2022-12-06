&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE pmpersonaltemp NO-UNDO LIKE pmpersonaltemp.
DEFINE TEMP-TABLE supportdagbok NO-UNDO LIKE supportdagbok.
DEFINE TEMP-TABLE valdbtemp NO-UNDO LIKE valdbtemp.


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
/*{EGENBEN.I}*/
{PERSONALTEMP.I}

DEFINE TEMP-TABLE pmpersonaltemp NO-UNDO LIKE personaltemp
   FIELD FORETAG AS CHARACTER
   FIELD E-POST AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   INDEX FORETAG FORETAG ANVANDARE.
DEFINE TEMP-TABLE exppmpersonaltemp NO-UNDO LIKE pmpersonaltemp.
DEFINE TEMP-TABLE supportdagbok NO-UNDO
   FIELD SUPPORTID AS INTEGER
   FIELD ANVANDARE AS CHARACTER
   FIELD STARTDATUM  AS DATE
   FIELD STARTKLOCKAN AS DECIMAL
   FIELD SLUTDATUM AS DATE
   FIELD SLUTKLOCKAN AS DECIMAL
   FIELD FRAGA AS CHARACTER
   FIELD SVAR AS CHARACTER
   FIELD FORETAG AS CHARACTER
   INDEX FORETAG FORETAG SUPPORTID.
{VALDBTEMP.I}
         
                              
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE suppapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{VALDBALL.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pmpersonaltemp supportdagbok valdbtemp

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS pmpersonaltemp.PERSONALKOD ~
pmpersonaltemp.ANVANDARE pmpersonaltemp.FORNAMN pmpersonaltemp.EFTERNAMN ~
pmpersonaltemp.TELEFON pmpersonaltemp.MOBILTEL pmpersonaltemp.E-POST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH pmpersonaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH pmpersonaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS pmpersonaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS pmpersonaltemp


/* Definitions for BROWSE BRW_SUPPORT                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_SUPPORT supportdagbok.SUPPORTID ~
supportdagbok.ANVANDARE supportdagbok.STARTDATUM supportdagbok.STARTKLOCKAN ~
supportdagbok.SLUTDATUM supportdagbok.SLUTKLOCKAN supportdagbok.FRAGA ~
supportdagbok.SVAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SUPPORT supportdagbok.ANVANDARE ~
supportdagbok.STARTDATUM supportdagbok.STARTKLOCKAN supportdagbok.SLUTDATUM ~
supportdagbok.SLUTKLOCKAN supportdagbok.FRAGA supportdagbok.SVAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define QUERY-STRING-BRW_SUPPORT FOR EACH supportdagbok NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SUPPORT OPEN QUERY BRW_SUPPORT FOR EACH supportdagbok NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SUPPORT supportdagbok


/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME FRAME-SUPP                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-SUPP ~
    ~{&OPEN-QUERY-BRW_SUPPORT}~
    ~{&OPEN-QUERY-BRW_VDB}

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_EXP 
     LABEL "Export anv" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_IMP 
     LABEL "Import anv" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_NYANV 
     LABEL "Ny användare" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_SUP 
     LABEL "Ny support" 
     SIZE 15 BY 1.13.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      pmpersonaltemp SCROLLING.

DEFINE QUERY BRW_SUPPORT FOR 
      supportdagbok SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      pmpersonaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      pmpersonaltemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      pmpersonaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      pmpersonaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 19.5
      pmpersonaltemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(11)":U
      pmpersonaltemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(11)":U
      pmpersonaltemp.E-POST COLUMN-LABEL "E-post" FORMAT "X(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 102.5 BY 6.25
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_SUPPORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SUPPORT C-Win _STRUCTURED
  QUERY BRW_SUPPORT NO-LOCK DISPLAY
      supportdagbok.SUPPORTID COLUMN-LABEL "SupportId" FORMAT ">>>>>>9":U
      supportdagbok.ANVANDARE COLUMN-LABEL "Användare" FORMAT "X(8)":U
      supportdagbok.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
      supportdagbok.STARTKLOCKAN COLUMN-LABEL "Starttid" FORMAT ">9.99":U
      supportdagbok.SLUTDATUM COLUMN-LABEL "Slutdatum" FORMAT "99/99/99":U
      supportdagbok.SLUTKLOCKAN COLUMN-LABEL "Sluttid" FORMAT ">9.99":U
      supportdagbok.FRAGA COLUMN-LABEL "Fråga" FORMAT "X(256)":U
            WIDTH 25
      supportdagbok.SVAR COLUMN-LABEL "Svar" FORMAT "X(256)":U
  ENABLE
      supportdagbok.ANVANDARE
      supportdagbok.STARTDATUM
      supportdagbok.STARTKLOCKAN
      supportdagbok.SLUTDATUM
      supportdagbok.SLUTKLOCKAN
      supportdagbok.FRAGA
      supportdagbok.SVAR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103.38 BY 19.25
         TITLE "Supportärenden" EXPANDABLE.

DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 18.5 BY 24
         FONT 4
         TITLE "Databaser" TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-NYREG
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-SUPP
     BRW_VDB AT ROW 1.5 COL 1.5
     BRW_PERS AT ROW 1.5 COL 22
     BRW_SUPPORT AT ROW 8.25 COL 21.13
     BTN_NYANV AT ROW 25.71 COL 2.75
     BTN_IMP AT ROW 26.88 COL 2.75
     BTN_SUP AT ROW 27.83 COL 59.5
     BTN_EXP AT ROW 28 COL 2.75
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: pmpersonaltemp T "?" NO-UNDO temp-db pmpersonaltemp
      TABLE: supportdagbok T "?" NO-UNDO temp-db supportdagbok
      TABLE: valdbtemp T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-NYREG:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-SUPP:FRAME = FRAME FRAME-NYREG:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-NYREG
                                                                        */
/* SETTINGS FOR FRAME FRAME-SUPP
                                                                        */
/* BROWSE-TAB BRW_VDB 1 FRAME-SUPP */
/* BROWSE-TAB BRW_PERS BRW_VDB FRAME-SUPP */
/* BROWSE-TAB BRW_SUPPORT BRW_PERS FRAME-SUPP */
ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-SUPP         = 1000
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-SUPP = TRUE
       BRW_PERS:COLUMN-RESIZABLE IN FRAME FRAME-SUPP       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.pmpersonaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.pmpersonaltemp.PERSONALKOD
"pmpersonaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.pmpersonaltemp.ANVANDARE
"pmpersonaltemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.pmpersonaltemp.FORNAMN
"pmpersonaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.pmpersonaltemp.EFTERNAMN
"pmpersonaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.pmpersonaltemp.TELEFON
"pmpersonaltemp.TELEFON" "Telefon" ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.pmpersonaltemp.MOBILTEL
"pmpersonaltemp.MOBILTEL" "Mobiltele" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.pmpersonaltemp.E-POST
"pmpersonaltemp.E-POST" "E-post" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SUPPORT
/* Query rebuild information for BROWSE BRW_SUPPORT
     _TblList          = "Temp-Tables.supportdagbok"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.supportdagbok.SUPPORTID
"supportdagbok.SUPPORTID" "SupportId" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.supportdagbok.ANVANDARE
"supportdagbok.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.supportdagbok.STARTDATUM
"supportdagbok.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.supportdagbok.STARTKLOCKAN
"supportdagbok.STARTKLOCKAN" "Starttid" ">9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.supportdagbok.SLUTDATUM
"supportdagbok.SLUTDATUM" "Slutdatum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.supportdagbok.SLUTKLOCKAN
"supportdagbok.SLUTKLOCKAN" "Sluttid" ">9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.supportdagbok.FRAGA
"supportdagbok.FRAGA" "Fråga" "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.supportdagbok.SVAR
"supportdagbok.SVAR" "Svar" "X(256)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_SUPPORT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SUPPORT
&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BRW_SUPPORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SUPPORT C-Win
ON ROW-LEAVE OF BRW_SUPPORT IN FRAME FRAME-SUPP /* Supportärenden */
DO:
   DISPLAY 
   supportdagbok.ANVANDARE 
   supportdagbok.STARTDATUM 
   supportdagbok.STARTKLOCKAN 
   supportdagbok.SLUTDATUM 
   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA 
   supportdagbok.SVAR WITH BROWSE BRW_SUPPORT NO-ERROR.

   ASSIGN 
   supportdagbok.ANVANDARE     INPUT BROWSE BRW_SUPPORT   supportdagbok.ANVANDARE   
   supportdagbok.STARTDATUM    INPUT BROWSE BRW_SUPPORT   supportdagbok.STARTDATUM  
   supportdagbok.STARTKLOCKAN  INPUT BROWSE BRW_SUPPORT   supportdagbok.STARTKLOCKAN
   supportdagbok.SLUTDATUM     INPUT BROWSE BRW_SUPPORT   supportdagbok.SLUTDATUM   
   supportdagbok.SLUTKLOCKAN   INPUT BROWSE BRW_SUPPORT   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA         INPUT BROWSE BRW_SUPPORT   supportdagbok.FRAGA       
   supportdagbok.SVAR          INPUT BROWSE BRW_SUPPORT   supportdagbok.SVAR.        
   


   DISPLAY 
   supportdagbok.ANVANDARE 
   supportdagbok.STARTDATUM 
   supportdagbok.STARTKLOCKAN 
   supportdagbok.SLUTDATUM 
   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA 
   supportdagbok.SVAR WITH BROWSE BRW_SUPPORT NO-ERROR.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SUPPORT C-Win
ON VALUE-CHANGED OF BRW_SUPPORT IN FRAME FRAME-SUPP /* Supportärenden */
DO:
    status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VDB IN FRAME FRAME-SUPP /* Databaser */
DO:
   status-ok = BRW_VDB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   valford = 'pmpersonaltemp.FORETAG = "' + valdbtemp.FORETAG + '"'.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[2]. 
   valford = 'supportdagbok.FORETAG = "' + valdbtemp.FORETAG + '"'.
   RUN setcolsortvar_UI IN brwproc[3] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[3]. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME FRAME-SUPP /* Databaser */
DO:
   status-ok = BRW_VDB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXP C-Win
ON CHOOSE OF BTN_EXP IN FRAME FRAME-SUPP /* Export anv */
DO:
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\". 
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   RUN export_UI IN suppapph (OUTPUT TABLE exppmpersonaltemp).
   fildir = fildir + "supportpers.d".
   OUTPUT TO VALUE(fildir) APPEND.
   FOR EACH exppmpersonaltemp:
      EXPORT exppmpersonaltemp.
   END.
   OUTPUT CLOSE.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMP C-Win
ON CHOOSE OF BTN_IMP IN FRAME FRAME-SUPP /* Import anv */
DO:
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\". 
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som Ni vill koppla"
   FILTERS        "All Files (*.d)"  "*.d"   
   INITIAL-DIR    fildir
   UPDATE OKvald.   
   IF OKvald = TRUE THEN DO:
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      INPUT FROM VALUE(fildir).
      REPEAT:
         CREATE exppmpersonaltemp.
         ASSIGN.
         IMPORT exppmpersonaltemp.
      END.
      RUN import_UI IN suppapph (INPUT TABLE exppmpersonaltemp).
   END.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYANV C-Win
ON CHOOSE OF BTN_NYANV IN FRAME FRAME-SUPP /* Ny användare */
DO:
   ccc
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SUP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SUP C-Win
ON CHOOSE OF BTN_SUP IN FRAME FRAME-SUPP /* Ny support */
DO:
  ccc
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_PERS
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
   IF VALID-HANDLE(suppapph) THEN DELETE PROCEDURE suppapph NO-ERROR.
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
   RUN enable_UI.
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_VDB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_SUPPORT:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SUPPORTAPP.P PERSISTENT SET suppapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SUPPORTAPP.P PERSISTENT SET suppapph.
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-NYREG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-NYREG}
  ENABLE BRW_VDB BRW_PERS BRW_SUPPORT BTN_NYANV BTN_IMP BTN_SUP BTN_EXP 
      WITH FRAME FRAME-SUPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SUPP}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

