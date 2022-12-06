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

  Created: 95/05/02 -  1:43 pm

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
&Scoped-define SHARED SHARED
&Scoped-define NEW NEW
{OMRALLT.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}

DEFINE NEW SHARED VARIABLE orgapph AS HANDLE NO-UNDO. /* ORGAPP.P */
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.    
DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE anr AS INTEGER NO-UNDO.
DEFINE VARIABLE sjid AS CHARACTER NO-UNDO.

{SOKDEF.I}

{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AVD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES juromravd

/* Definitions for BROWSE BRW_AVD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD juromravd.AVDELNINGNR juromravd.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD 
&Scoped-define QUERY-STRING-BRW_AVD FOR EACH juromravd NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AVD OPEN QUERY BRW_AVD FOR EACH juromravd NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AVD juromravd
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD juromravd


/* Definitions for BROWSE BRW_JUROMRAVD                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_JUROMRAVD juromravd.ID ~
juromravd.AVDELNINGNR juromravd.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_JUROMRAVD juromravd.ID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_JUROMRAVD juromravd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_JUROMRAVD juromravd
&Scoped-define QUERY-STRING-BRW_JUROMRAVD FOR EACH juromravd NO-LOCK
&Scoped-define OPEN-QUERY-BRW_JUROMRAVD OPEN QUERY BRW_JUROMRAVD FOR EACH juromravd NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_JUROMRAVD juromravd
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_JUROMRAVD juromravd


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_AVD}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_ORG CMB_JURP BRW_AVD FBTN_VPER FBTN_SKR ~
BRW_JUROMRAVD BTN_NY BTN_AND BTN_BORT BTN_ZON BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_ORG FILL-IN-UTSOK CMB_JURP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ZON 
     LABEL "Zoner":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Juridiskperson" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alla" 
     DROP-DOWN-LIST
     SIZE 26.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTSOK AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE RAD_ORG AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Område", 1,
"Avdelning", 2
     SIZE 49 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVD FOR 
      juromravd SCROLLING.

DEFINE QUERY BRW_JUROMRAVD FOR 
      juromravd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD WINDOW-1 _STRUCTURED
  QUERY BRW_AVD NO-LOCK DISPLAY
      juromravd.AVDELNINGNR COLUMN-LABEL "Avdelningnr" FORMAT ">>>9":U
      juromravd.NAMN COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 25.75
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 40 BY 5.75
         TITLE "Avdelning".

DEFINE BROWSE BRW_JUROMRAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_JUROMRAVD WINDOW-1 _STRUCTURED
  QUERY BRW_JUROMRAVD NO-LOCK DISPLAY
      juromravd.ID FORMAT "X(8)":U WIDTH 8.5
      juromravd.AVDELNINGNR COLUMN-LABEL "Avdelning" FORMAT ">>>9":U
      juromravd.NAMN COLUMN-LABEL "Benämning" FORMAT "X(45)":U
            WIDTH 35
  ENABLE
      juromravd.ID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 58 BY 11.75
         TITLE "Alla".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_ORG AT ROW 1.25 COL 1.5 NO-LABEL
     FILL-IN-UTSOK AT ROW 2.5 COL 2.88 NO-LABEL
     CMB_JURP AT ROW 2.5 COL 30.25 COLON-ALIGNED
     BRW_AVD AT ROW 4.25 COL 1.5
     FBTN_VPER AT ROW 8 COL 62
     FBTN_SKR AT ROW 9.1 COL 62
     BRW_JUROMRAVD AT ROW 10.5 COL 1.5
     BTN_NY AT ROW 22.5 COL 2.5
     BTN_AND AT ROW 22.5 COL 16.5
     BTN_BORT AT ROW 22.5 COL 30.5
     BTN_ZON AT ROW 22.5 COL 55.5 RIGHT-ALIGNED
     BTN_AVB AT ROW 22.5 COL 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76 BY 22.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: juromravd T "?" NO-UNDO temp-db juromravd
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Organisation"
         HEIGHT             = 23
         WIDTH              = 76.25
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
/* BROWSE-TAB BRW_AVD CMB_JURP FRAME-A */
/* BROWSE-TAB BRW_JUROMRAVD FBTN_SKR FRAME-A */
/* SETTINGS FOR BUTTON BTN_ZON IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       BTN_ZON:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UTSOK IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-UTSOK:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD
/* Query rebuild information for BROWSE BRW_AVD
     _TblList          = "Temp-Tables.juromravd"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.juromravd.AVDELNINGNR
"juromravd.AVDELNINGNR" "Avdelningnr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.juromravd.NAMN
"juromravd.NAMN" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "25.75" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_AVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_JUROMRAVD
/* Query rebuild information for BROWSE BRW_JUROMRAVD
     _TblList          = "Temp-Tables.juromravd"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.juromravd.ID
"juromravd.ID" ? ? "character" ? ? ? ? ? ? yes ? no no "8.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.juromravd.AVDELNINGNR
"juromravd.AVDELNINGNR" "Avdelning" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.juromravd.NAMN
"juromravd.NAMN" "Benämning" "X(45)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_JUROMRAVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_AVD
&Scoped-define SELF-NAME BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVD WINDOW-1
ON VALUE-CHANGED OF BRW_AVD IN FRAME FRAME-A /* Avdelning */
DO:
   status-ok = BRW_AVD:SELECT-FOCUSED-ROW() NO-ERROR.    
   IF RAD_ORG = 1 THEN DO:
      anr = juromravd.AVDELNINGNR.
      FIND FIRST juromravd WHERE juromravd.TYP = "OMR" AND juromravd.AVDELNINGNR = anr USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR. 
      brwsortvar = "juromravd.TYP = '" + "OMR" + "'  AND juromravd.AVDELNINGNR  = '" +  STRING(anr) + "'".        
      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_JUROMRAVD
&Scoped-define SELF-NAME BRW_JUROMRAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_JUROMRAVD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_JUROMRAVD IN FRAME FRAME-A /* Alla */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
  /* ASSIGN brec = RECID(OMRADETAB).*/
   RUN andra_UI. 
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
   RUN bort_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ZON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ZON WINDOW-1
ON CHOOSE OF BTN_ZON IN FRAME FRAME-A /* Zoner */
DO:
   /*
   &Scoped-define BROWSE-NAME BRW_JUROMRAVD
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE juromravd THEN DO:
      RETURN NO-APPLY.
   END.

   {muswait.i}
   {AVBGOM.I}
   RUN ZONREG.W (INPUT juromravd.ID).
   {AVBFRAM.I}  
   {musarrow.i}   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP WINDOW-1
ON VALUE-CHANGED OF CMB_JURP IN FRAME FRAME-A /* Juridiskperson */
DO:
   CMB_JURP = INPUT CMB_JURP.  
   IF CMB_JURP = "Alla" THEN DO:       
      brwsortvar = 'juromravd.TYP = "AVD"'.
      RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "AVDELNINGNR").
      RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[2].
      status-ok = BRW_AVD:DESELECT-FOCUSED-ROW() NO-ERROR. 
      IF RAD_ORG = 1 THEN DO:      
         brwsortvar = 'juromravd.TYP = "OMR"'.
         RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ID").
      END.
      ELSE IF RAD_ORG = 2 THEN DO:      
         brwsortvar = 'juromravd.TYP = "AVD"'.
         RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "AVDELNINGNR").
      END.      
   END.
   ELSE DO:        
      IF RAD_ORG = 1 THEN DO:               
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.NAMN = CMB_JURP USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.                  
         sjid = juromravd.SID .
         brwsortvar = "juromravd.TYP = '" + "AVD" + "'  AND juromravd.SID  = '" +  STRING(sjid) + "'".                 
         RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "AVDELNINGNR").
         RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
         RUN openbdynspec_UI IN brwproc[2].
         status-ok = BRW_AVD:DESELECT-FOCUSED-ROW() NO-ERROR. 

         /*FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.NAMN = CMB_JURP USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.         
         anr = juromravd.AVDELNINGNR.
         FIND FIRST juromravd WHERE juromravd.TYP = "OMR" AND juromravd.AVDELNINGNR = anr USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.         
         brwsortvar = "juromravd.TYP = '" + "OMR" + "'  AND juromravd.AVDELNINGNR  = '" +  STRING(anr) + "'".        */
         brwsortvar = "juromravd.TYP = '" + "OMR" + "'  AND juromravd.SID  = '" +  STRING(sjid) + "'".      

      END.
      ELSE IF RAD_ORG = 2  THEN DO:
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.NAMN = CMB_JURP USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.         
         sjid = juromravd.SID .
         brwsortvar = "juromravd.TYP = '" + "AVD" + "'  AND juromravd.SID  = '" +  STRING(sjid) + "'".                 
         RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "AVDELNINGNR").
         RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
         RUN openbdynspec_UI IN brwproc[2].
         status-ok = BRW_AVD:DESELECT-FOCUSED-ROW() NO-ERROR. 
      END.      
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
   RUN openbdynspec_UI IN brwproc[1]. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa */
DO:
   skrivut = FALSE.  
   RUN ut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ORG WINDOW-1
ON VALUE-CHANGED OF RAD_ORG IN FRAME FRAME-A
DO:
   RAD_ORG = INPUT RAD_ORG.
   ASSIGN
   BTN_ZON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   BRW_AVD:HIDDEN = TRUE.
   CMB_JURP:HIDDEN = TRUE.
   FILL-IN-UTSOK:HIDDEN = TRUE.
   IF RAD_ORG = 1 THEN DO:
      FILL-IN-UTSOK:HIDDEN = FALSE.
      ASSIGN      
         /*
      BRW_JUROMRAVD:HEIGHT-CHARS = 11.75
      BRW_JUROMRAVD:ROW = 12
      */
      BRW_AVD:HIDDEN = FALSE
      CMB_JURP:HIDDEN = FALSE.
      ASSIGN
      juromravd.ID:VISIBLE IN BROWSE BRW_JUROMRAVD = TRUE
      juromravd.AVDELNINGNR:VISIBLE IN BROWSE BRW_JUROMRAVD = FALSE
      juromravd.ID:LABEL IN BROWSE BRW_JUROMRAVD = Guru.Konstanter:gomrk
      BRW_JUROMRAVD:TITLE = Guru.Konstanter:gomrl.      
      
      IF Guru.Konstanter:globforetag = "cELPA" THEN BTN_ZON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
      
      IF CMB_JURP = "Alla" THEN DO:      
         brwsortvar = 'juromravd.TYP = "OMR"'.
      END.
      ELSE DO:
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.NAMN = CMB_JURP USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.         
         sjid = juromravd.SID .
         brwsortvar = "juromravd.TYP = '" + "OMR" + "'  AND juromravd.SID  = '" +  STRING(sjid) + "'".                 
      END.
      RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ID").
      RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[1].      
   END.
   ELSE IF RAD_ORG = 2 THEN DO:    
      FILL-IN-UTSOK:HIDDEN = FALSE.
      ASSIGN      
         /*
      BRW_JUROMRAVD:ROW = 6 
      BRW_JUROMRAVD:HEIGHT-CHARS = 17.75
      */
      CMB_JURP:HIDDEN = FALSE      
      juromravd.ID:VISIBLE IN BROWSE BRW_JUROMRAVD = FALSE
      juromravd.AVDELNINGNR:VISIBLE IN BROWSE BRW_JUROMRAVD = TRUE
      juromravd.AVDELNINGNR:LABEL IN BROWSE BRW_JUROMRAVD= Guru.Konstanter:gavdk
      BRW_JUROMRAVD:TITLE = Guru.Konstanter:gavdl.
      IF CMB_JURP = "Alla" THEN DO:      
         brwsortvar = 'juromravd.TYP = "AVD"'.
      END.
      ELSE DO:
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.NAMN = CMB_JURP USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.         
         sjid = juromravd.SID .
         brwsortvar = "juromravd.TYP = '" + "AVD" + "'  AND juromravd.SID  = '" +  STRING(sjid) + "'".                 
      END.
      RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "AVDELNINGNR").
      RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE IF RAD_ORG = 3 THEN DO:           
      ASSIGN      
         /*
      BRW_JUROMRAVD:ROW = 6       
      BRW_JUROMRAVD:HEIGHT-CHARS = 17.75      
      */
      juromravd.ID:VISIBLE IN BROWSE BRW_JUROMRAVD = TRUE
      juromravd.AVDELNINGNR:VISIBLE IN BROWSE BRW_JUROMRAVD = FALSE
      juromravd.ID:LABEL IN BROWSE BRW_JUROMRAVD = Guru.Konstanter:gjuk
      BRW_JUROMRAVD:TITLE = Guru.Konstanter:gjul
      
      brwsortvar = 'juromravd.TYP = "JUR"'.
      RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ID").
      RUN setcolsortvar_UI IN brwproc[1] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[1].
   END.   
   /*DISPLAY FILL-IN-ORG WITH FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVD
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
   IF VALID-HANDLE(orgapph) THEN DELETE PROCEDURE orgapph.
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
   ASSIGN
   FILL-IN-UTSOK = "Gör urval:". 
   RUN enable_UI.      
   RUN orghmt_UI IN orgapph (OUTPUT TABLE juromravd).             
   RAD_ORG = 1.
   brwsortvar = 'juromravd.TYP = "AVD"'.
   RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "AVDELNINGNR").
   RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
   RUN openbdynspec_UI IN brwproc[2].
   status-ok = BRW_AVD:DESELECT-FOCUSED-ROW() NO-ERROR. 
   juromravd.AVDELNINGNR:LABEL IN BROWSE BRW_AVD= Guru.Konstanter:gavdk.
   /*FOR EACH juromravd WHERE juromravd.TYP = "AVD" NO-LOCK:      
      stat = CMB_AVD:ADD-LAST(juromravd.NAMN).
   END.  */
   FOR EACH juromravd WHERE juromravd.TYP = "JUR" NO-LOCK:      
      stat = CMB_JURP:ADD-LAST(juromravd.NAMN).
   END.  
   
   CMB_JURP:SCREEN-VALUE = "Alla".       
   CMB_JURP = "Alla".       
   APPLY "VALUE-CHANGED" TO RAD_ORG IN FRAME {&FRAME-NAME}.
   /*FILL-IN-ORG = Guru.Konstanter:gomrk.*/
   status-ok = RAD_ORG:DELETE("Område").
   status-ok = RAD_ORG:DELETE("Avdelning").
   RAD_ORG:ADD-LAST(Guru.Konstanter:gomrl, 1).
   RAD_ORG:ADD-LAST(Guru.Konstanter:gavdl, 2).
   RAD_ORG:ADD-LAST(Guru.Konstanter:gjul, 3).
   BRW_AVD:TITLE = Guru.Konstanter:gavdl.
   CMB_JURP:LABEL = Guru.Konstanter:gjul.
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_JUROMRAVD:HANDLE,INPUT "Personer",INPUT "orgpers_UI").
   DISPLAY RAD_ORG WITH FRAME {&FRAME-NAME}.       
   /*
   IF BTN_ZON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE THEN DO:
      ASSIGN
      BTN_NY:COLUMN = BTN_NY:COLUMN + 8
      BTN_AND:COLUMN = BTN_NY:COLUMN + 14
      BTN_BORT:COLUMN = BTN_AND:COLUMN + 14.
   END.
   */
   {FRMSIZE.I}  
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
   juromravd.ID:READ-ONLY IN BROWSE BRW_JUROMRAVD = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_JUROMRAVD:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_AVD:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ORGAPP.P PERSISTENT SET orgapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      IF Guru.Konstanter:varforetypval[27] = 1 THEN RUN ORGAPP2.P PERSISTENT SET orgapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ORGAPP.P PERSISTENT SET orgapph.
      IF Guru.Konstanter:varforetypval[27] = 1 THEN RUN ORGAPP2.P PERSISTENT SET orgapph.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andny_UI WINDOW-1 
PROCEDURE andny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*{muswait.i}
   {AVBGOM.I}
   */


   /*
   IF RAD_ORG = 1 THEN DO:
      vemanvandare = juromravd.ID.
   END.
   ELSE IF RAD_ORG = 2 THEN DO:
      vemanvandare = STRING(juromravd.AVDELNINGNR).
   END.
   IF RAD_ORG = 3 THEN DO:
      vemanvandare = juromravd.SID.
     */


   IF RAD_ORG = 1 THEN DO:
      {AVBGOM.I}       
      RUN ORGNY.W (INPUT-OUTPUT vemanvandare, INPUT vemanvandare).  
      IF musz = FALSE THEN DO: 
         FIND FIRST juromravd WHERE juromravd.TYP = "OMR" AND juromravd.ID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DELETE juromravd.
         RUN hamtaen_UI IN orgapph (INPUT 1,INPUT vemanvandare,INPUT TRUE,OUTPUT TABLE juromravd APPEND).   
         FIND FIRST juromravd WHERE juromravd.TYP = "OMR" AND juromravd.ID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(juromravd)).              
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].     
         END.        
      
      END.
   END.
   ELSE IF RAD_ORG = 2 THEN DO:
      RUN AVDNY.W (INPUT-OUTPUT vemanvandare).  
      IF musz = FALSE THEN DO: 
         FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.AVDELNINGNR = INTEGER(vemanvandare)
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DELETE juromravd.
         RUN hamtaen_UI IN orgapph (INPUT 2,INPUT vemanvandare,INPUT TRUE,OUTPUT TABLE juromravd APPEND).   
         FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.AVDELNINGNR = INTEGER(vemanvandare)
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(juromravd)).              
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].     
         END.
         status-ok = BRW_AVD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(juromravd)).              
         RUN openbdynspec_UI IN brwproc[2].
         RUN lastselectdyn_UI IN brwproc[2].         
      END.
      
   END.
   ELSE IF RAD_ORG = 3 THEN DO:
      stat = FALSE.
      IF vemanvandare = ? THEN stat = TRUE.
      RUN JURNY.W (INPUT-OUTPUT vemanvandare).  
      IF musz = FALSE THEN DO: 
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.SID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DELETE juromravd.
         RUN hamtaen_UI IN orgapph (INPUT 3,INPUT vemanvandare,INPUT TRUE,OUTPUT TABLE juromravd APPEND).   
         FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.SID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE juromravd THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(juromravd)).              
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].     
         END.
         IF stat = TRUE THEN stat = CMB_JURP:ADD-LAST(juromravd.NAMN).
         stat = FALSE.
      END.      
   END.
   {AVBFRAM.I}      
   {musarrow.i} 
   musz = FALSE.    
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_JUROMRAVD
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE juromravd THEN DO:
      RETURN NO-APPLY.
   END.
   IF RAD_ORG = 1 THEN DO:
      vemanvandare = juromravd.ID.
   END.
   ELSE IF RAD_ORG = 2 THEN DO:
      vemanvandare = STRING(juromravd.AVDELNINGNR).
   END.
   IF RAD_ORG = 3 THEN DO:
      vemanvandare = juromravd.SID.
   END.
   RUN andny_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*cccc*/
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orgpers_UI WINDOW-1 
PROCEDURE orgpers_UI :
   DEFINE VARIABLE tempz AS CHARACTER NO-UNDO.
   &Scoped-define BROWSE-NAME BRW_JUROMRAVD
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   
   RUN BESTINFO.W (INPUT juromravd.ID,OUTPUT tempz).
   
END PROCEDURE.
   /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE VARIABLE utvar AS CHARACTER NO-UNDO.
   &Scoped-define BROWSE-NAME BRW_JUROMRAVD
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE juromravd THEN DO:
      RETURN NO-APPLY.
   END.
   IF RAD_ORG = 1 THEN DO:
      ASSIGN
      utvar = "detta " + LC(Guru.Konstanter:gomrl)
      vemanvandare = juromravd.ID.
   END.
   ELSE IF RAD_ORG = 2 THEN DO:
      ASSIGN
      utvar = "denna " + LC(Guru.Konstanter:gavdl)
      vemanvandare = STRING(juromravd.AVDELNINGNR).
   END.
   IF RAD_ORG = 3 THEN DO:
      ASSIGN
      utvar = "denna " + LC(Guru.Konstanter:gjul)
      vemanvandare = juromravd.ID.
   END.

   MESSAGE "Vill du verkligen ta bort " + utvar + "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE juromravd.NAMN
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      IF RAD_ORG = 3 THEN DO:
         ASSIGN
         utvar = "denna " + LC(Guru.Konstanter:gjul)
         vemanvandare = juromravd.SID.
      END.
      {muswait.i}
      RUN orgbort_UI IN orgapph (INPUT RAD_ORG,INPUT vemanvandare,OUTPUT TABLE felmeddtemp).   
      {musarrow.i}
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      ELSE DO:
          DELETE juromravd.
          RUN selnextprevrow_UI IN brwproc[1].
          RUN refreshbrw_UI IN brwproc[1].
          RUN lastselectdyn_UI IN brwproc[1].        
      END.               
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
  DISPLAY RAD_ORG FILL-IN-UTSOK CMB_JURP 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_ORG CMB_JURP BRW_AVD FBTN_VPER FBTN_SKR BRW_JUROMRAVD BTN_NY 
         BTN_AND BTN_BORT BTN_ZON BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   vemanvandare = ?.
   RUN andny_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   RUN visa_UI IN orgapph (INPUT RAD_ORG,OUTPUT TABLE tidut).   
   {musarrow.i}
   
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Organisation").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

