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
{ANMARKD.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE avtregproch AS HANDLE NO-UNDO. /* AVTREGAPP.P */
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE frannr AS INTEGER NO-UNDO.  
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
DEFINE VARIABLE avtid AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

{TIDUTTTNEW.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{AVTALTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AVTAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avtaltemp

/* Definitions for BROWSE BRW_AVTAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTAL avtaltemp.AVTAL avtaltemp.LANK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTAL 
&Scoped-define QUERY-STRING-BRW_AVTAL FOR EACH avtaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTAL OPEN QUERY BRW_AVTAL FOR EACH avtaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTAL avtaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTAL avtaltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AVTAL FBTN_VPER BTN_UPP FBTN_SKR BTN_NER ~
BTN_NY BTN_AND BTN_BORT BTN_AVB 

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

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Btn 1" 
     SIZE 5.75 BY 1.83.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "Btn 1" 
     SIZE 5.75 BY 1.83.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVTAL FOR 
      avtaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTAL WINDOW-1 _STRUCTURED
  QUERY BRW_AVTAL NO-LOCK DISPLAY
      avtaltemp.AVTAL COLUMN-LABEL "Namn" FORMAT "X(42)":U
      avtaltemp.LANK COLUMN-LABEL "Dokument" FORMAT "Ja/Nej":U
            WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 55.5 BY 9.83
         TITLE "Tillstånd/Avtal/Dokument".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_AVTAL AT ROW 1.5 COL 1.5
     FBTN_VPER AT ROW 3 COL 65
     BTN_UPP AT ROW 4.04 COL 57.63
     FBTN_SKR AT ROW 4.1 COL 65
     BTN_NER AT ROW 6.83 COL 57.63
     BTN_NY AT ROW 11.75 COL 3.5
     BTN_AND AT ROW 11.75 COL 20.5
     BTN_BORT AT ROW 11.75 COL 37.38
     BTN_AVB AT ROW 11.75 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.63 BY 12.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: avtaltemp T "?" NO-UNDO temp-db avtaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Tillstånd/Avtal/Dokument"
         HEIGHT             = 12.29
         WIDTH              = 79.63
         MAX-HEIGHT         = 21.54
         MAX-WIDTH          = 102.5
         VIRTUAL-HEIGHT     = 21.54
         VIRTUAL-WIDTH      = 102.5
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
/* BROWSE-TAB BRW_AVTAL 1 FRAME-A */
ASSIGN 
       BRW_AVTAL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTAL
/* Query rebuild information for BROWSE BRW_AVTAL
     _TblList          = "Temp-Tables.avtaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtaltemp.AVTAL
"avtaltemp.AVTAL" "Namn" "X(42)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.avtaltemp.LANK
"avtaltemp.LANK" "Dokument" ? "logical" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
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


&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER WINDOW-1
ON CHOOSE OF BTN_NER IN FRAME FRAME-A /* Btn 1 */
DO:

   GET CURRENT BRW_AVTAL NO-LOCK.
   frannr = avtaltemp.ORDNING.
   GET NEXT BRW_AVTAL NO-LOCK.
   IF AVAILABLE avtaltemp THEN DO:
      tillnr = avtaltemp.ORDNING.
      avtaltemp.ORDNING = frannr.
      GET PREV BRW_AVTAL NO-LOCK.
      avtaltemp.ORDNING = tillnr.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   ELSE GET PREV BRW_AVTAL NO-LOCK.
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


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Btn 1 */
DO:
   GET CURRENT BRW_AVTAL NO-LOCK.
   frannr = avtaltemp.ORDNING.
   GET PREV BRW_AVTAL NO-LOCK.
   IF AVAILABLE avtaltemp THEN DO:
      tillnr = avtaltemp.ORDNING.
      avtaltemp.ORDNING = frannr.
      GET NEXT BRW_AVTAL NO-LOCK.
      avtaltemp.ORDNING = tillnr.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   ELSE GET NEXT BRW_AVTAL NO-LOCK.
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


&Scoped-define BROWSE-NAME BRW_AVTAL
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
   RUN sparaavtal_UI IN avtregproch (INPUT ?, INPUT TABLE avtaltemp).
   IF VALID-HANDLE(avtregproch) THEN DELETE PROCEDURE avtregproch. 
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
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN openbdynspec_UI IN brwproc[1].
   GET FIRST BRW_AVTAL NO-LOCK.
   IF NOT AVAILABLE avtaltemp THEN DO:
      ASSIGN
      BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
      (INPUT BRW_AVTAL:HANDLE IN FRAME {&FRAME-NAME}).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVTREGAPP.P PERSISTENT SET avtregproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN AVTREGAPP.P PERSISTENT SET avtregproch.         
   END.   
   RUN laddaavt_UI IN avtregproch (OUTPUT TABLE avtaltemp).
   RUN setcolindex_UI IN brwproc[1] (INPUT " ORDNING ").
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
   {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE avtaltemp THEN DO:
      
      avtid = avtaltemp.AVTALID.
      RUN AVTALNY.W (INPUT-OUTPUT avtid). 
      
      {musarrow.i}
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST avtaltemp WHERE avtaltemp.AVTALID = avtid NO-ERROR.
         IF AVAILABLE avtaltemp THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      END.   
      ELSE musz = FALSE.    
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anmark_UI WINDOW-1 
PROCEDURE anmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 2 THEN DO:                
      ASSIGN                  
      SUBSTRING(tidut.UT,18) = tidtext.
      CREATE tidut.
   END.     
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
   {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE avtaltemp THEN DO:
      MESSAGE "Vill du ta bort denna post ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE SUBSTRING(avtaltemp.AVTAL,1,30)
      UPDATE answer AS LOGICAL.
      IF answer THEN DO:
         RUN avtcheck_UI IN avtregproch (INPUT avtaltemp.AVTALID, OUTPUT felmedd).
         IF felmedd = "" THEN DO:
            RUN tabortavtal_UI IN avtregproch (INPUT avtaltemp.AVTALID).
            DELETE avtaltemp.
            RUN selnextprevrow_UI IN brwproc[1].
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         ELSE MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      IF NOT AVAILABLE avtaltemp THEN DO:
         ASSIGN                                        
         BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
         FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
         FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
  ENABLE BRW_AVTAL FBTN_VPER BTN_UPP FBTN_SKR BTN_NER BTN_NY BTN_AND BTN_BORT 
         BTN_AVB 
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
   
   avtid = ?.
   RUN AVTALNY.W (INPUT-OUTPUT avtid). 
   
   {musarrow.i}
   IF musz = FALSE THEN DO:
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST avtaltemp WHERE avtaltemp.AVTALID = avtid NO-ERROR.
      IF AVAILABLE avtaltemp THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(avtaltemp)).
      ASSIGN                                        
      BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE  
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE  
      FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.   
   ELSE musz = FALSE.       
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
  str = "=======================================================================".
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Tillstånd/avtal"
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.                 
   CREATE tidut.
   ASSIGN tidut.UT = str.
   OPEN QUERY lageq FOR EACH avtaltemp USE-INDEX ORD NO-LOCK.
   GET FIRST lageq NO-LOCK.
   DO WHILE AVAILABLE(avtaltemp):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Tillstånd/avtal: " + SUBSTRING(avtaltemp.AVTAL,1,30).
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Filnamn        : " + avtaltemp.FILNAMN.
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "Dokument       : " + STRING(avtaltemp.LANK,"Ja/Nej").
      IF avtaltemp.ANMARK NE "" THEN DO:
         CREATE tidut.      
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Anmärkning     :"
         retvar = 1
         ednum = 1
         ednum3 = LENGTH(avtaltemp.ANMARK)
         retvar = INDEX(avtaltemp.ANMARK,CHR(10),ednum)
         edtecken = 52
         edtext = avtaltemp.ANMARK
         tidtext = "".  
         IF retvar <= edtecken THEN DO:         
            tidtext = SUBSTRING(edtext,ednum,retvar - 1).
            ednum = retvar + 1.
         END.
         {ANMARK2.I}   
      END.       
      CREATE tidut.
      ASSIGN tidut.UT = str.
      GET NEXT lageq NO-LOCK.
   END.
   CLOSE QUERY lageq.       
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Tillstånd/avtal").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.      
            
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

