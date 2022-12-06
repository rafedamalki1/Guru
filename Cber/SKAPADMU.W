&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>
             
  Output Parameters:
      <none>

  Author: 

  Created: 06/12/97 -  3:16 pm

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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{BERSKAP.I}
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
/* Local Variable Definitions ---                                       */    
DEFINE NEW SHARED VARIABLE typkod AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE ny AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE skap_row AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE firstkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE skapadmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ktypkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES berskaptemp

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL berskaptemp.KOD ~
berskaptemp.BENAMNING berskaptemp.MODUL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH berskaptemp NO-LOCK ~
    BY berskaptemp.KOD
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH berskaptemp NO-LOCK ~
    BY berskaptemp.KOD.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL berskaptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL berskaptemp


/* Definitions for FRAME FRAME-B                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_TYP BRW_MTRL FBTN_VISA FBTN_SKRIV ~
FBTN_EX BTN_NY BTN_ANDRA BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_TYP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      berskaptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-2 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      berskaptemp.KOD FORMAT "X(12)":U
      berskaptemp.BENAMNING FORMAT "x(256)":U WIDTH 50
      berskaptemp.MODUL FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 74.5 BY 23.75
         TITLE "Upplagda skåp".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_TYP AT ROW 1.75 COL 1.5 NO-LABEL
     BRW_MTRL AT ROW 3.25 COL 1.5
     FBTN_VISA AT ROW 4.17 COL 77.75
     FBTN_SKRIV AT ROW 5.25 COL 77.75
     FBTN_EX AT ROW 6.38 COL 77.75
     BTN_NY AT ROW 27.96 COL 10.88
     BTN_ANDRA AT ROW 27.96 COL 25.88
     BTN_BORT AT ROW 27.96 COL 40.88
     BTN_AVB AT ROW 27.96 COL 77.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Temp-Tables and Buffers:
      TABLE: berskaptemp T "?" NO-UNDO temp-db berskaptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration kabelskåp"
         COLUMN             = 14.13
         ROW                = 1.5
         HEIGHT             = 28.42
         WIDTH              = 92
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 96.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 96.25
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BRW_MTRL CMB_TYP FRAME-B */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-B         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-B = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_TYP IN FRAME FRAME-B
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.berskaptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.berskaptemp.KOD|yes"
     _FldNameList[1]   = Temp-Tables.berskaptemp.KOD
     _FldNameList[2]   > Temp-Tables.berskaptemp.BENAMNING
"BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.berskaptemp.MODUL
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA WINDOW-2
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-B /* Ändra */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO: 
      {muswait.i}     
      ASSIGN      
      ny = FALSE
      ktypkodvar = berskaptemp.KTYPKOD
      kodvar = berskaptemp.KOD
      skap_row = berskaptemp.BERSKAPROW. 
      {AVBGOM.I}         
      RUN SKAPANDU.W (INPUT-OUTPUT ktypkodvar,INPUT-OUTPUT kodvar).                    
      {AVBFRAM.I}
      IF musz = FALSE THEN DO:
         DELETE berskaptemp.         
         FOR EACH berskaptemp WHERE berskaptemp.KTYPKOD = ktypkodvar AND
            berskaptemp.KOD = kodvar AND berskaptemp.ENR NE "" USE-INDEX KOD NO-LOCK:
            DELETE berskaptemp.
         END.         
         RUN laddaberskapen_UI IN skapadmapph (INPUT ktypkodvar,INPUT kodvar, OUTPUT TABLE berskaptemp APPEND).         
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST berskaptemp WHERE berskaptemp.KTYPKOD = ktypkodvar AND berskaptemp.KOD = kodvar AND 
         berskaptemp.ENR = ""
         NO-LOCK NO-ERROR.
         IF AVAILABLE berskaptemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(berskaptemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
      END.
      musz = FALSE.     
      {musarrow.i}                                            
   END.
   ELSE DO:
      MESSAGE "Du har inte valt något skåp som ska ändras."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:    
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-2
ON CHOOSE OF BTN_BORT IN FRAME FRAME-B /* Ta bort */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO:      
      {muswait.i} 
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
      ASSIGN
      enrvar = berskaptemp.ENR
      ktypkodvar = berskaptemp.KTYPKOD
      kodvar = berskaptemp.KOD
      skap_row = berskaptemp.BERSKAPROW. 
      MESSAGE "Vill du ta bort - " + berskaptemp.KOD berskaptemp.BENAMNING  + "."
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort" UPDATE svar AS LOGICAL.         
      IF svar THEN DO: 
         RUN bortber_UI IN skapadmapph (INPUT enrvar,INPUT ktypkodvar,
                                        INPUT kodvar,INPUT skap_row).   
         FOR EACH berskaptemp WHERE berskaptemp.KTYPKOD = ktypkodvar AND
         berskaptemp.KOD = kodvar AND berskaptemp.ENR NE "" NO-LOCK.
            DELETE berskaptemp.
         END.
         FIND FIRST berskaptemp WHERE berskaptemp.BERSKAPROW = skap_row NO-LOCK NO-ERROR.
         DELETE berskaptemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].                            
         RUN lastselectdyn_UI IN brwproc[1].
      END.        
      {musarrow.i}     
   END.
   ELSE DO:
      MESSAGE "Du har inte valt något skåp som ska tas bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-2
ON CHOOSE OF BTN_NY IN FRAME FRAME-B /* Ny */
DO: 
   {muswait.i} 
   ny = TRUE.   
   {AVBGOM.I}       
   kodvar = "".
   RUN SKAPANDU.W (INPUT-OUTPUT ktypkodvar,INPUT-OUTPUT kodvar).                    
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:
      RUN laddaberskapen_UI IN skapadmapph (INPUT ktypkodvar,INPUT kodvar, OUTPUT TABLE berskaptemp APPEND).
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST berskaptemp WHERE berskaptemp.KTYPKOD = ktypkodvar AND berskaptemp.KOD = kodvar AND 
      berskaptemp.ENR = ""
      NO-LOCK NO-ERROR.
      IF AVAILABLE berskaptemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(berskaptemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   musz = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TYP WINDOW-2
ON VALUE-CHANGED OF CMB_TYP IN FRAME FRAME-B
DO:
   {muswait.i} 
   ASSIGN
   CMB_TYP = INPUT CMB_TYP
   typkod = CMB_TYP.   
   ktypkodvar = typkod.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " berskaptemp.KTYPKOD = '" + CMB_TYP + "' AND berskaptemp.ENR = '' ").  
   RUN openbdynspec_UI IN brwproc[1].   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX WINDOW-2
ON CHOOSE OF FBTN_EX IN FRAME FRAME-B /* Visa I EXCEL */
DO:
   {muswait.i} 

   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
  RUN EXCSKAPU.P.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i}                 
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISSKAP2.W.      
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO:
   {muswait.i} 
   IF musz = TRUE THEN musz = FALSE.     
   {AVBGOM.I}
   RUN VISSKAP2.W.   
   {AVBFRAM.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(skapadmapph) THEN DELETE PROCEDURE skapadmapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
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
   status-ok = CMB_TYP:DELETE("0"). 
   RUN laddaberskap_UI IN skapadmapph (OUTPUT TABLE berskaptemp).
   FIND FIRST konsttemp WHERE konsttemp.KONSKOD = 0
   USE-INDEX KOD NO-LOCK NO-ERROR. 
   IF AVAILABLE konsttemp THEN DO:
      ASSIGN
      status-ok = CMB_TYP:ADD-LAST(konsttemp.KTYPKOD)IN FRAME {&FRAME-NAME}
      CMB_TYP:SCREEN-VALUE = konsttemp.KTYPKOD
      firstkod = konsttemp.KTYPKOD.
      FOR EACH konsttemp WHERE konsttemp.KONSKOD = 0 AND 
      konsttemp.KTYPKOD NE firstkod NO-LOCK:      
         status-ok = CMB_TYP:ADD-LAST(konsttemp.KTYPKOD)IN FRAME {&FRAME-NAME}.              
      END.      
      ASSIGN  
      CMB_TYP = INPUT CMB_TYP.              
      APPLY "VALUE-CHANGED" TO CMB_TYP IN FRAME {&FRAME-NAME}. 
      RUN enable_UI.
      {FRMSIZE.I} 
   END.
   ELSE DO:
      MESSAGE "Det finns inga konstruktioner upplagda. Funktionen ej möjlig att utföra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      SESSION:SET-WAIT-STATE("").
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY. 
   END.   
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPADMAPP.P PERSISTENT SET skapadmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPADMAPP.P PERSISTENT SET skapadmapph.
   END.  
   RUN setcolindex_UI IN brwproc[1] (INPUT "KOD").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY CMB_TYP 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_TYP BRW_MTRL FBTN_VISA FBTN_SKRIV FBTN_EX BTN_NY BTN_ANDRA 
         BTN_BORT BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

