&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-4


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-4 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/18/97 -  8:54 am

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{LEVTEMP.I}
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.       
DEFINE NEW SHARED VARIABLE for_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE gruppkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE gruppkodkopi AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.     
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE berfanmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
{ANMARKTEMP2.I}
   
DEFINE TEMP-TABLE efastanmtempkopia NO-UNDO LIKE fastanmtemp.
DEFINE BUFFER fastanmtempbuff FOR fastanmtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-D
&Scoped-define BROWSE-NAME BRW_FASTANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastanmtemp

/* Definitions for BROWSE BRW_FASTANM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_FASTANM fastanmtemp.ORDNING ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FASTANM fastanmtemp.ORDNING ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define QUERY-STRING-BRW_FASTANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FASTANM OPEN QUERY BRW_FASTANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FASTANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FASTANM fastanmtemp


/* Definitions for FRAME FRAME-D                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-D ~
    ~{&OPEN-QUERY-BRW_FASTANM}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BTN_KOPI CMB_LEVTILL CMB_VAL ~
CMB_VALKOPIERA BRW_FASTANM BTN_AVB BTN_NY BTN_AND BTN_BORT 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL CMB_LEVTILL CMB_VAL CMB_VALKOPIERA 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-4 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_LEVTILL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VALKOPIERA AS CHARACTER FORMAT "X(30)":U 
     LABEL "Kopiera till" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Beredning", 1,
"Inköp", 2,
"Egenkontroll", 3
     SIZE 58.5 BY 1.29 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FASTANM FOR 
      fastanmtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FASTANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FASTANM WINDOW-4 _STRUCTURED
  QUERY BRW_FASTANM NO-LOCK DISPLAY
      fastanmtemp.ORDNING COLUMN-LABEL "Ordning" FORMAT "->,>>>,>>9":U
      fastanmtemp.ANMARK COLUMN-LABEL "Anmärkning" FORMAT "X(75)":U
  ENABLE
      fastanmtemp.ORDNING
      fastanmtemp.ANMARK
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 112.5 BY 20.21
         TITLE "Fasta anmärkningar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-D
     RAD_VAL AT ROW 2 COL 2 NO-LABEL
     BTN_KOPI AT ROW 4.71 COL 115.38 WIDGET-ID 4
     CMB_LEVTILL AT ROW 4.75 COL 2 NO-LABEL
     CMB_VAL AT ROW 4.75 COL 3.5 NO-LABEL
     CMB_VALKOPIERA AT ROW 4.75 COL 50.75 WIDGET-ID 2
     BRW_FASTANM AT ROW 6.29 COL 2
     BTN_AVB AT ROW 27.42 COL 115.38
     BTN_NY AT ROW 27.58 COL 13
     BTN_AND AT ROW 27.58 COL 28
     BTN_BORT AT ROW 27.58 COL 43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128.88 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-4 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration av fasta anmärkningar"
         HEIGHT             = 28.46
         WIDTH              = 129.13
         MAX-HEIGHT         = 41.67
         MAX-WIDTH          = 231.38
         VIRTUAL-HEIGHT     = 41.67
         VIRTUAL-WIDTH      = 231.38
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
/* SETTINGS FOR WINDOW WINDOW-4
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-D
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FASTANM CMB_VALKOPIERA FRAME-D */
ASSIGN 
       BRW_FASTANM:HIDDEN  IN FRAME FRAME-D                = TRUE
       BRW_FASTANM:MAX-DATA-GUESS IN FRAME FRAME-D         = 1000
       BRW_FASTANM:COLUMN-RESIZABLE IN FRAME FRAME-D       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEVTILL IN FRAME FRAME-D
   ALIGN-L                                                              */
ASSIGN 
       CMB_LEVTILL:HIDDEN IN FRAME FRAME-D           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-D
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_VALKOPIERA IN FRAME FRAME-D
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
THEN WINDOW-4:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FASTANM
/* Query rebuild information for BROWSE BRW_FASTANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ORDNING
"fastanmtemp.ORDNING" "Ordning" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fastanmtemp.ANMARK
"fastanmtemp.ANMARK" "Anmärkning" "X(75)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_FASTANM */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FASTANM
&Scoped-define SELF-NAME BRW_FASTANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FASTANM WINDOW-4
ON ENTRY OF BRW_FASTANM IN FRAME FRAME-D /* Fasta anmärkningar */
DO:
   IF AVAILABLE fastanmtemp THEN DO:
      DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FASTANM WINDOW-4
ON ROW-LEAVE OF BRW_FASTANM IN FRAME FRAME-D /* Fasta anmärkningar */
DO:
   IF AVAILABLE fastanmtemp THEN DO:
      DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
      ASSIGN
      fastanmtemp.ANMARK = INPUT BROWSE BRW_FASTANM fastanmtemp.ANMARK.
      DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
   END.
   EMPTY TEMP-TABLE efastanmtemp NO-ERROR. 
   CREATE efastanmtemp.
   BUFFER-COPY fastanmtemp TO efastanmtemp.
   RUN btnspar_UI IN berfanmapph (INPUT TABLE efastanmtemp,OUTPUT for_rowid).                                 
   fastanmtemp.FASTANVROW = for_rowid.
   DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FASTANM WINDOW-4
ON VALUE-CHANGED OF BRW_FASTANM IN FRAME FRAME-D /* Fasta anmärkningar */
DO:
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE fastanmtemp THEN DO:
      DISPLAY fastanmtemp.ORDNING fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM. 
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastanmtemp.ORDNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastanmtemp.ORDNING BRW_FASTANM _BROWSE-COLUMN WINDOW-4
ON ENTRY OF fastanmtemp.ORDNING IN BROWSE BRW_FASTANM /* Ordning */
DO:
   IF AVAILABLE fastanmtemp THEN DO:
      DISPLAY fastanmtemp.ORDNING WITH BROWSE BRW_FASTANM.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastanmtemp.ORDNING BRW_FASTANM _BROWSE-COLUMN WINDOW-4
ON LEAVE OF fastanmtemp.ORDNING IN BROWSE BRW_FASTANM /* Ordning */
DO:
   IF AVAILABLE fastanmtemp THEN DO:
      ASSIGN
      fastanmtemp.ORDNING = INPUT BROWSE BRW_FASTANM fastanmtemp.ORDNING.
      fastanmtemp.PROGRAM = "EGENK" + STRING(fastanmtemp.ORDNING). 
      DISPLAY fastanmtemp.ORDNING WITH BROWSE BRW_FASTANM.
   END.
   EMPTY TEMP-TABLE efastanmtemp NO-ERROR. 
   CREATE efastanmtemp.
   BUFFER-COPY fastanmtemp TO efastanmtemp.
   RUN btnspar_UI IN berfanmapph (INPUT TABLE efastanmtemp,OUTPUT for_rowid).                                 
   fastanmtemp.FASTANVROW = for_rowid.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastanmtemp.ANMARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastanmtemp.ANMARK BRW_FASTANM _BROWSE-COLUMN WINDOW-4
ON ANY-KEY OF fastanmtemp.ANMARK IN BROWSE BRW_FASTANM /* Anmärkning */
DO:
   RUN Anykey_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastanmtemp.ANMARK BRW_FASTANM _BROWSE-COLUMN WINDOW-4
ON ENTRY OF fastanmtemp.ANMARK IN BROWSE BRW_FASTANM /* Anmärkning */
DO:
    IF AVAILABLE fastanmtemp THEN DO:
      DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastanmtemp.ANMARK BRW_FASTANM _BROWSE-COLUMN WINDOW-4
ON LEAVE OF fastanmtemp.ANMARK IN BROWSE BRW_FASTANM /* Anmärkning */
DO:
  IF AVAILABLE fastanmtemp THEN DO:
      ASSIGN
      fastanmtemp.ANMARK = INPUT BROWSE BRW_FASTANM fastanmtemp.ANMARK.
      DISPLAY fastanmtemp.ANMARK WITH BROWSE BRW_FASTANM.
   END.
   EMPTY TEMP-TABLE efastanmtemp NO-ERROR. 
   CREATE efastanmtemp.
   BUFFER-COPY fastanmtemp TO efastanmtemp.
   RUN btnspar_UI IN berfanmapph (INPUT TABLE efastanmtemp,OUTPUT for_rowid).                                 
   fastanmtemp.FASTANVROW = for_rowid.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-4
ON CHOOSE OF BTN_AND IN FRAME FRAME-D /* Ändra */
DO:
   {muswait.i}                        
   ASSIGN
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_valda = BRW_FASTANM:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   IF RAD_VAL = 1 OR RAD_VAL = 3 THEN DO:
      ASSIGN
      gruppkod = STRING(konstgrptemp.KONSKOD).
   END.
   ELSE gruppkod = levtemp.LEVKOD. 
   IF antal_valda > 0 THEN DO:
      
      ASSIGN
      for_rowid = fastanmtemp.FASTANVROW.
      andra = TRUE.
      RUN NYBFASTU.W (INPUT RAD_VAL).
      IF musz = FALSE THEN DO:
         IF RAD_VAL = 1 THEN DO:
            RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'BERE' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").  
         END.
         ELSE IF RAD_VAL = 2 THEN DO:
            RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'INKOP' AND fastanmtemp.OMRADE = '" +  levtemp.LEVKOD + "'").
         END.
         ELSE IF RAD_VAL = 3 THEN DO:
            RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM BEGINS 'EGENK' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").
         END.    
         RUN openbdynspec_UI IN brwproc[1].  
         FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = for_rowid  NO-LOCK NO-ERROR.
         IF AVAILABLE fastanmtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).
            RUN lastselectdyn_UI IN brwproc[1].         
         END.
      END.
      musz = FALSE.      
      andra = FALSE.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen fasta anmärkningar att ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-4
ON CHOOSE OF BTN_BORT IN FRAME FRAME-D /* Ta bort */
DO: 
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   IF AVAILABLE fastanmtemp THEN DO:     
      ASSIGN
      for_rowid = fastanmtemp.FASTANVROW.
      MESSAGE "Vill du ta bort " + fastanmtemp.ANMARK + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
      IF svar THEN DO:
         RUN bortanm_UI IN berfanmapph (INPUT for_rowid).
         DELETE fastanmtemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].  
         RUN lastselectdyn_UI IN brwproc[1].
      END.   
   END.
   ELSE DO:
      MESSAGE "Det finns ingen fasta anmärkningar att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPI WINDOW-4
ON CHOOSE OF BTN_KOPI IN FRAME FRAME-D /* Kopiera */
DO:   
   {muswait.i}     
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL
   NO-LOCK NO-ERROR.
   gruppkod = STRING(konstgrptemp.KONSKOD).
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VALKOPIERA 
   NO-LOCK NO-ERROR.
   gruppkodkopi = STRING(konstgrptemp.KONSKOD). 
   EMPTY TEMP-TABLE efastanmtemp NO-ERROR. 
   EMPTY TEMP-TABLE efastanmtempkopia NO-ERROR.
   IF RAD_VAL = 1 THEN DO:
      FOR EACH fastanmtemp WHERE fastanmtemp.PROGRAM = 'BERE' AND fastanmtemp.OMRADE =  gruppkod:
         CREATE efastanmtemp.
         BUFFER-COPY fastanmtemp TO efastanmtemp.
         efastanmtemp.OMRADE = gruppkodkopi.
         efastanmtemp.FASTANVROW = ?.
         CREATE efastanmtempkopia.
         BUFFER-COPY efastanmtemp TO efastanmtempkopia.
         RUN btnspar_UI IN berfanmapph (INPUT TABLE efastanmtempkopia,OUTPUT for_rowid).
         EMPTY TEMP-TABLE efastanmtempkopia NO-ERROR. 
         efastanmtemp.FASTANVROW = for_rowid.                                 
      END.
   END.
   IF RAD_VAL = 3 THEN DO:
      FOR EACH fastanmtemp WHERE fastanmtemp.PROGRAM BEGINS 'EGENK' AND fastanmtemp.OMRADE =  gruppkod:
        CREATE efastanmtemp.
         BUFFER-COPY fastanmtemp TO efastanmtemp.
         efastanmtemp.OMRADE = gruppkodkopi.
         efastanmtemp.FASTANVROW = ?.
         CREATE efastanmtempkopia.
         BUFFER-COPY efastanmtemp TO efastanmtempkopia.
         RUN btnspar_UI IN berfanmapph (INPUT TABLE efastanmtempkopia,OUTPUT for_rowid).
         EMPTY TEMP-TABLE efastanmtempkopia NO-ERROR. 
         efastanmtemp.FASTANVROW = for_rowid.     
      END.
   END.
   FOR EACH efastanmtemp  WHERE NO-LOCK:
      CREATE fastanmtempbuff.
      BUFFER-COPY efastanmtemp TO fastanmtempbuff.
   END.  
   EMPTY TEMP-TABLE efastanmtemp NO-ERROR.    
   RUN openbdynspec_UI IN brwproc[1].      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-4
ON CHOOSE OF BTN_NY IN FRAME FRAME-D /* Ny */
DO:
   {muswait.i} 
   IF RAD_VAL = 1 OR RAD_VAL = 3 THEN DO:
      ASSIGN
      gruppkod = STRING(konstgrptemp.KONSKOD).
   END.
   ELSE gruppkod = levtemp.LEVKOD.   
   andra = FALSE.
   RUN NYBFASTU.W (INPUT RAD_VAL).
   
   IF musz = FALSE THEN DO:
      IF RAD_VAL = 1 THEN DO: 
         RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'BERE' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").  
      END.
      ELSE IF RAD_VAL = 2 THEN DO: 
         RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'INKOP' AND fastanmtemp.OMRADE = '" +  levtemp.LEVKOD + "'").
      END.
      ELSE IF RAD_VAL = 3 THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM BEGINS 'EGENK' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").
      END.     
      RUN openbdynspec_UI IN brwproc[1].  
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = for_rowid  NO-LOCK NO-ERROR.
      IF AVAILABLE fastanmtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).
         RUN lastselectdyn_UI IN brwproc[1].         
      END.
   END.
   musz = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVTILL WINDOW-4
ON VALUE-CHANGED OF CMB_LEVTILL IN FRAME FRAME-D
DO:
   {muswait.i} 
   ASSIGN
   CMB_LEVTILL = INPUT CMB_LEVTILL.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEVTILL 
   NO-LOCK NO-ERROR. 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'INKOP' AND fastanmtemp.OMRADE = '" +  levtemp.LEVKOD + "'").  
   RUN openbdynspec_UI IN brwproc[1].  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL WINDOW-4
ON VALUE-CHANGED OF CMB_VAL IN FRAME FRAME-D
DO:  
   {muswait.i} 
   ASSIGN
   CMB_VAL = INPUT CMB_VAL.
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL 
   NO-LOCK NO-ERROR. 
   IF RAD_VAL = 1 THEN  RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'BERE' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").
   IF RAD_VAL = 3 THEN  RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM BEGINS 'EGENK' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").    
   RUN openbdynspec_UI IN brwproc[1].  
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALKOPIERA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALKOPIERA WINDOW-4
ON VALUE-CHANGED OF CMB_VALKOPIERA IN FRAME FRAME-D /* Kopiera till */
DO:  
   {muswait.i} 
   ASSIGN
   CMB_VALKOPIERA = INPUT CMB_VALKOPIERA.
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-4
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-D
DO:   
   RAD_VAL = INPUT RAD_VAL.
   CMB_VAL:HIDDEN = TRUE.
   CMB_VALKOPIERA:HIDDEN = TRUE.
   CMB_LEVTILL:HIDDEN = TRUE.
   BTN_KOPI:HIDDEN = TRUE.
   IF RAD_VAL = 1 OR RAD_VAL = 3 THEN DO:
      CMB_VAL:HIDDEN = FALSE. 
      CMB_VALKOPIERA:HIDDEN = FALSE.
      BTN_KOPI:HIDDEN = FALSE.
      APPLY "VALUE-CHANGED" TO CMB_VAL.
   END.
   ELSE DO:
      CMB_LEVTILL:HIDDEN = FALSE.
      APPLY "VALUE-CHANGED" TO CMB_LEVTILL.
   END.
   IF RAD_VAL = 1 OR RAD_VAL = 2 THEN DO:
      fastanmtemp.ORDNING:VISIBLE IN BROWSE BRW_FASTANM = FALSE.
   END.
   ELSE DO:
      fastanmtemp.ORDNING:VISIBLE IN BROWSE BRW_FASTANM = TRUE.
   END.
   status-ok = BRW_FASTANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-4 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(berfanmapph) THEN DELETE PROCEDURE berfanmapph.
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
   RUN ladda_UI IN berfanmapph (OUTPUT TABLE fastanmtemp,
                                OUTPUT TABLE konstgrptemp,
                                OUTPUT TABLE levtemp).
   status-ok = CMB_LEVTILL:DELETE("0"). 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND 
      levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:              
      status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.   
   FIND FIRST levtemp NO-LOCK NO-ERROR. 
   CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN.                          
   ASSIGN  
   status-ok = CMB_VALKOPIERA:DELETE("0")
   status-ok = CMB_VAL:DELETE("0")
   musz = FALSE. 
   FOR EACH konstgrptemp USE-INDEX ORD NO-LOCK:
      ASSIGN
      status-ok = CMB_VAL:ADD-LAST(konstgrptemp.BENAMNING)IN FRAME {&FRAME-NAME}.
      status-ok = CMB_VALKOPIERA:ADD-LAST(konstgrptemp.BENAMNING)IN FRAME {&FRAME-NAME}.
   END.    
   FIND FIRST konstgrptemp USE-INDEX ORD NO-LOCK NO-ERROR. 
   CMB_VAL:SCREEN-VALUE = konstgrptemp.BENAMNING.
   CMB_VAL = INPUT CMB_VAL. 
   RAD_VAL = 1.
   CMB_VALKOPIERA = CMB_VAL.               
   RUN enable_UI.  
   BRW_FASTANM:NO-EMPTY-SPACE = NO.
   APPLY "VALUE-CHANGED" TO RAD_VAL.               
   ASSIGN  
   BRW_FASTANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " fastanmtemp.PROGRAM = 'BERE' AND fastanmtemp.OMRADE = '" +  STRING(konstgrptemp.KONSKOD) + "'").  
   RUN openbdynspec_UI IN brwproc[1].  
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-4 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: 
  Parameters:  
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
   (INPUT BRW_FASTANM:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN SetBrwFont_UI IN brwproc[1] (INPUT 24).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERFANMAPP.P PERSISTENT SET berfanmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERFANMAPP.P PERSISTENT SET berfanmapph.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Anykey_UI WINDOW-4 
PROCEDURE Anykey_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    RETURN.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-4  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
  THEN DELETE WIDGET WINDOW-4.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-4  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL CMB_LEVTILL CMB_VAL CMB_VALKOPIERA 
      WITH FRAME FRAME-D IN WINDOW WINDOW-4.
  ENABLE RAD_VAL BTN_KOPI CMB_LEVTILL CMB_VAL CMB_VALKOPIERA BRW_FASTANM 
         BTN_AVB BTN_NY BTN_AND BTN_BORT 
      WITH FRAME FRAME-D IN WINDOW WINDOW-4.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

