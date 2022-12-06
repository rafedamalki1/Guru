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

  Created: 09/24/96 -  3:39 pm

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
DEFINE INPUT PARAMETER bbenamning AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{BESTKUNDALLT.I}
{DIRDEF.I}
{ANSPROJBER.I}
{ARBATE.I}
{OMRTEMPW.I}
{AVDTEMP.I}
DEFINE NEW SHARED TEMP-TABLE visa NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
DEFINE NEW SHARED VARIABLE akval AS INTEGER NO-UNDO. 

DEFINE NEW SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valmanad AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE utomr AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE omr AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valnamn AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE datvar AS DATE NO-UNDO. 
DEFINE NEW SHARED VARIABLE mellanrum AS LOGICAL NO-UNDO.

DEFINE VARIABLE vallista AS INTEGER NO-UNDO.


DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE depaapph AS HANDLE NO-UNDO.
DEFINE VARIABLE best_nr_koll AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE bnummer AS INTEGER NO-UNDO.

&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED 
{bestnrtab.I}
/*DEFINE NEW SHARED TEMP-TABLE best_nr_tab NO-UNDO
    FIELD bestnr AS INTEGER FORMAT ">>>>>9" LABEL "Bestnr"
    FIELD bestdatum AS DATE FORMAT "99/99/99" LABEL "Best-datum"
    FIELD bestallare AS CHARACTER FORMAT "x(12)" LABEL "Beställare"
    FIELD AONR AS CHARACTER
    FIELD DELNR AS INTEGER
    INDEX NR bestnr DESCENDING.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BESTNR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES best_nr_tab

/* Definitions for BROWSE BRW_BESTNR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_BESTNR best_nr_tab.Bestnr ~
best_nr_tab.Bestdatum best_nr_tab.AONR best_nr_tab.DELNR ~
best_nr_tab.Bestallare 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BESTNR 
&Scoped-define QUERY-STRING-BRW_BESTNR FOR EACH best_nr_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BESTNR OPEN QUERY BRW_BESTNR FOR EACH best_nr_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BESTNR best_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BESTNR best_nr_tab


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BRW_BESTNR BTN_NY BTN_ANDRA BTN_BORT ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lager", 1,
"Projekt", 2
     SIZE 32 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BESTNR FOR 
      best_nr_tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BESTNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BESTNR WINDOW-1 _STRUCTURED
  QUERY BRW_BESTNR NO-LOCK DISPLAY
      best_nr_tab.Bestnr FORMAT ">>>>>9":U
      best_nr_tab.Bestdatum FORMAT "99/99/99":U
      best_nr_tab.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      best_nr_tab.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      best_nr_tab.Bestallare FORMAT "x(12)":U WIDTH 11
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 37 BY 22.08 ROW-HEIGHT-CHARS .38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_VAL AT ROW 1 COL 1.5 NO-LABEL
     BRW_BESTNR AT ROW 2.5 COL 1.5
     BTN_NY AT ROW 25 COL 4
     BTN_ANDRA AT ROW 25 COL 16.75
     BTN_BORT AT ROW 25 COL 29.5
     BTN_AVB AT ROW 25 COL 46.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.63 BY 25.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db best_nr_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beställning"
         HEIGHT             = 25.58
         WIDTH              = 60.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 85.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 85.25
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
/* BROWSE-TAB BRW_BESTNR RAD_VAL FRAME-A */
ASSIGN 
       BRW_BESTNR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BESTNR
/* Query rebuild information for BROWSE BRW_BESTNR
     _TblList          = "Temp-Tables.best_nr_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.best_nr_tab.Bestnr
     _FldNameList[2]   = Temp-Tables.best_nr_tab.Bestdatum
     _FldNameList[3]   > Temp-Tables.best_nr_tab.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.best_nr_tab.DELNR
"DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.best_nr_tab.Bestallare
"Bestallare" ? ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BESTNR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA WINDOW-1
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-A /* Ändra */
DO:
   {muswait.i}
   IF AVAILABLE best_nr_tab THEN DO:
      best_nr_koll = best_nr_tab.bestnr.
      IF best_nr_tab.AONR NE "" THEN DO:
         ASSIGN
         valaonr = best_nr_tab.AONR
         valdelnr = best_nr_tab.DELNR.
      END.
      ELSE DO:
         ASSIGN
         valaonr = ""
         valdelnr = ?.
      END.
      {AVBGOM.I}
      RUN MTRLBESTV.W (INPUT-OUTPUT best_nr_koll).
      {AVBFRAM.I}
      IF musz = FALSE THEN DO:
         RUN refreshbrw_UI IN brwproc[1].
         FIND FIRST best_nr_tab WHERE best_nr_tab.bestnr = best_nr_koll NO-ERROR.
         IF AVAILABLE best_nr_tab THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_nr_tab)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         
      END.
   END.
   musz = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   IF AVAILABLE best_nr_tab THEN DO:   
      MESSAGE "Vill du ta bort beställning - " + STRING(best_nr_tab.bestnr) VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         {muswait.i}      
         bnummer = best_nr_tab.bestnr. 
         /*RUN bortbest_UI IN depaapph (INPUT vald_depa, INPUT bnummer ).*/
         EMPTY TEMP-TABLE felmeddtemp  NO-ERROR. 
         RUN bortbest2_UI IN depaapph (INPUT vald_depa, INPUT bnummer, OUTPUT TABLE felmeddtemp ).
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:                           
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.                        
            DELETE felmeddtemp.
            RETURN NO-APPLY.                           
         END.
         DELETE best_nr_tab.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
         RUN openbdynspec_UI IN brwproc[1].      
         RUN lastselectdyn_UI IN brwproc[1].      
         {musarrow.i}
      END.                               
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:            
   {muswait.i}
   {AVBGOM.I}
   ASSIGN
   best_nr_koll = 0   
   valaonr = ""
   valdelnr = ?.
   IF RAD_VAL = 2 THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE avdtemp).
      END.
      ELSE DO:
         RUN AVSKAP.P 
         (OUTPUT TABLE avdtemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ARBARTS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE aarttemp).
      END.
      ELSE DO:
         RUN ARBARTS.P 
         (OUTPUT TABLE aarttemp).                  
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ANSBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      ELSE DO:
         RUN ANSBER.P 
         (OUTPUT TABLE ansvaraotemp,OUTPUT TABLE beredartemp,OUTPUT TABLE projtemp).
      END.
      {OMRHMT.I}     
      bestvad = 2.
      {BESTHMT.I}
      
      /*lista = TRUE.*/
      akval = 1. 
      EMPTY TEMP-TABLE uppvaltemp NO-ERROR.
      EMPTY TEMP-TABLE visa NO-ERROR.   
      
      vallista = 42.
      CREATE visa.
      ASSIGN
      visa.ORDNING = 42
      visa.UPPFOLJVAL = 42
      visa.KUURVAL = TRUE
      visa.DELNRKOLL = TRUE.         
      visa.UT = Guru.Konstanter:gaok + "/Beställning".
      CREATE uppvaltemp.
      ASSIGN         
      uppvaltemp.ENDBEST = ?               
      uppvaltemp.VISPERAR = TRUE    
      uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
      uppvaltemp.SLUTDATUM  = TODAY
      uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
      uppvaltemp.AVSLUTSLUT = TODAY
      uppvaltemp.TILLFALLFAST = 1
      SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
      uppvaltemp.BEREDARE = "ALLA"
      uppvaltemp.ARBANSVARIG = "ALLA"
      uppvaltemp.BESTNAMN = "ALLA"         
      uppvaltemp.OMRNAMN = "ALLA"
      uppvaltemp.AVDNAMN = "ALLA"
      uppvaltemp.AVDNR = "ALLA"
      uppvaltemp.FAKTTYP = "ALLA"
      uppvaltemp.PAAV = 1
      uppvaltemp.MANUPPDEL = FALSE
      uppvaltemp.VALDLISTA = visa.UT.            
      RUN MDPKUURVALU.W 
         (INPUT-OUTPUT vallista ,
          OUTPUT aonummer ,
          OUTPUT delnummer ,
          INPUT-OUTPUT TABLE uppvaltemp, 
          INPUT-OUTPUT TABLE bestkundallt,
          INPUT-OUTPUT TABLE omrtemp, 
          INPUT-OUTPUT TABLE avdtemp).
         /*
          INPUT-OUTPUT TABLE valdaao,
          INPUT-OUTPUT TABLE utsokaonr).     
          */
      
      
      IF musz = TRUE THEN DO:
         musz = musz.        
      END.
      ELSE DO:
         RUN MTRLBESTV.W (INPUT-OUTPUT best_nr_koll).
      END.      
   END.
   ELSE DO:
      RUN MTRLBESTV.W (INPUT-OUTPUT best_nr_koll).
   END.   
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].      
      FIND FIRST best_nr_tab WHERE best_nr_tab.bestnr = best_nr_koll NO-ERROR.
      IF AVAILABLE best_nr_tab THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(best_nr_tab)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   musz = FALSE.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:     
      EMPTY TEMP-TABLE best_nr_tab NO-ERROR. 
      RUN bestbhmt_UI IN depaapph (INPUT vald_depa,OUTPUT TABLE best_nr_tab ). 
      RUN setdescvarcol_UI IN brwproc[1] (INPUT TRUE).
      RUN openbdynspec_UI IN brwproc[1].
      
      ASSIGN
      best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BESTNR = FALSE
      best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BESTNR = FALSE.
      
   END.
   ELSE DO:
      EMPTY TEMP-TABLE best_nr_tab NO-ERROR. 
      RUN bestbhmt2_UI IN depaapph (INPUT vald_depa,OUTPUT TABLE best_nr_tab ). 
      RUN setdescvarcol_UI IN brwproc[1] (INPUT TRUE).
      RUN openbdynspec_UI IN brwproc[1].
      
      ASSIGN
      best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BESTNR = TRUE
      best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BESTNR = TRUE.
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BESTNR
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
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
   RUN disable_UI.
END.
/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CHOOSE" TO BTN_AVB IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CHOOSE" TO BTN_AVB IN FRAME {&FRAME-NAME}.
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
   RAD_VAL = 1.
   ASSIGN WINDOW-1:TITLE = "Beställningar för depå - " + bbenamning.
   best_nr_tab.AONR:LABEL IN BROWSE BRW_BESTNR = Guru.Konstanter:gaok.
   RUN bestbhmt_UI IN depaapph (INPUT vald_depa,OUTPUT TABLE best_nr_tab ). 
   RUN setdescvarcol_UI IN brwproc[1] (INPUT TRUE).
   RUN openbdynspec_UI IN brwproc[1].
   RUN enable_UI.   
   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "Csnat" THEN musz = musz.
   ELSE DO:
      RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   {FRMSIZE.I}  
  
  {musarrow.i}
   {WIN_M_SLUT.I}
    ASSIGN
   best_nr_tab.AONR:VISIBLE IN BROWSE BRW_BESTNR = FALSE
   best_nr_tab.DELNR:VISIBLE IN BROWSE BRW_BESTNR = FALSE.
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
      (INPUT BRW_BESTNR:HANDLE IN FRAME {&FRAME-NAME}).         
 
   IF Guru.Konstanter:appcon THEN DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph.     
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
  DISPLAY RAD_VAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_VAL BRW_BESTNR BTN_NY BTN_ANDRA BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

