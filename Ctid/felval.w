&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

  Created: 95/07/04 - 11:31 am

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
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{PHMT.I}
{TIDPERS.I}
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE andvisdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE andvisrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE radmusz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempmanad AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE markpers    
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING. 

DEFINE NEW SHARED TEMP-TABLE vomrtemp
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES personaltemp

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS personaltemp
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for FRAME FRAME-B                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_BER 
     LABEL "Rätta bered.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_LON 
     LABEL "Rätta lönet.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_TID 
     LABEL "Rätta tidr.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_TRA 
     LABEL "Rätta trakt.":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 15 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-namn" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-namn" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 1.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS WINDOW-2 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 40
  ENABLE
      personaltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 65 BY 13.54
         TITLE "Personal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_AR AT ROW 1.5 COL 5.5 NO-LABEL
     CMB_MANAD AT ROW 1.5 COL 12.63 COLON-ALIGNED NO-LABEL
     BRW_PERS AT ROW 3.21 COL 1.5
     FBTN_TID AT ROW 8 COL 67.25
     FBTN_LON AT ROW 9.1 COL 67.25
     FBTN_BER AT ROW 10.2 COL 67.25
     FBTN_TRA AT ROW 11.3 COL 67.25
     FILL-IN-SOKPA AT ROW 17 COL 2 NO-LABEL
     FILL-IN_SPERSONALKOD AT ROW 17 COL 20.5 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 17 COL 35.25 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 17 COL 52.75 COLON-ALIGNED
     BTN_AVB AT ROW 17.04 COL 67.25
     RECT-SOK AT ROW 16.79 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.88 BY 17.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Rättning av veckokörda uppgifter"
         HEIGHT             = 17.54
         WIDTH              = 81.13
         MAX-HEIGHT         = 23.63
         MAX-WIDTH          = 96.63
         VIRTUAL-HEIGHT     = 23.63
         VIRTUAL-WIDTH      = 96.63
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BRW_PERS CMB_MANAD FRAME-B */
/* SETTINGS FOR BROWSE BRW_PERS IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME FRAME-B                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-B         = 300.

/* SETTINGS FOR COMBO-BOX CMB_AR IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       CMB_AR:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_MANAD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_MANAD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR BUTTON FBTN_BER IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_BER:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR BUTTON FBTN_LON IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_LON:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR BUTTON FBTN_TID IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_TID:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR BUTTON FBTN_TRA IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_TRA:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKPA IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-SOKPA:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SEFTERNAMN IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SEFTERNAMN:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SFORNAMN IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SFORNAMN:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SPERSONALKOD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SPERSONALKOD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SOK IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       RECT-SOK:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:   
   RUN rad_UI. 
   andvisdatum = regdatum.                   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR WINDOW-2
ON LEAVE OF CMB_AR IN FRAME FRAME-B
DO:
   CMB_AR = INPUT CMB_AR.
   regar = CMB_AR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR WINDOW-2
ON VALUE-CHANGED OF CMB_AR IN FRAME FRAME-B
DO:
   CMB_AR = INPUT CMB_AR.
   regar = CMB_AR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD WINDOW-2
ON LEAVE OF CMB_MANAD IN FRAME FRAME-B
DO:
   CMB_MANAD = INPUT CMB_MANAD.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD WINDOW-2
ON VALUE-CHANGED OF CMB_MANAD IN FRAME FRAME-B
DO:
   CMB_MANAD = INPUT CMB_MANAD.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BER WINDOW-2
ON CHOOSE OF FBTN_BER IN FRAME FRAME-B /* Rätta bered. */
DO:
   RUN and_UI (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_LON WINDOW-2
ON CHOOSE OF FBTN_LON IN FRAME FRAME-B /* Rätta lönet. */
DO:
   RUN and_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TID WINDOW-2
ON CHOOSE OF FBTN_TID IN FRAME FRAME-B /* Rätta tidr. */
DO:   
   RUN and_UI (INPUT 1). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_TRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TRA WINDOW-2
ON CHOOSE OF FBTN_TRA IN FRAME FRAME-B /* Rätta trakt. */
DO:
   RUN and_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN WINDOW-2
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME FRAME-B /* E-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME FRAME-B /* E-namn */
DO:
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "EFTERNAMN", INPUT FILL-IN_SEFTERNAMN).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN WINDOW-2
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME FRAME-B /* F-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME FRAME-B /* F-namn */
DO:
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "FORNAMN", INPUT FILL-IN_SFORNAMN).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD WINDOW-2
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME FRAME-B /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME FRAME-B /* Enhet/Sign */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "PERSONALKOD", INPUT FILL-IN_SPERSONALKOD).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
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
   OPEN QUERY BRW_PERS FOR EACH personaltemp
   WHERE personaltemp.AKTIV = TRUE NO-LOCK .   
   RUN REGVEC.P.    
   RUN enable_UI.   
   {FRMSIZE.I}              
   RUN globman_UI.      
   RUN repos_UI.
   ENABLE FBTN_TID FBTN_LON FBTN_BER FBTN_TRA WITH FRAME {&FRAME-NAME}.   
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/         
   personaltemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERS = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE and_UI WINDOW-2 
PROCEDURE and_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   RUN rad_UI.
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN DO:      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      MESSAGE 
      "Du är inte behörig att ändra eller visa denna persons tidregistreringar."
      VIEW-AS ALERT-BOX.
      RETURN.
   END.     
   {AVBGOM.I}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 17
   soktemp.SOKDATE[1] = bdatum
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKINT[1] = vadgora.
   {SOKANROP.I}      
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Det finns inga veckokörda uppgifter att rätta"
      VIEW-AS ALERT-BOX TITLE "Meddelande".      
   END.
   ELSE RUN FELTID.W (INPUT tidpers.PERSONALKOD, INPUT vadgora).  
   {AVBFRAM.I}
   {musarrow.i}
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
  ENABLE BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE globman_UI WINDOW-2 
PROCEDURE globman_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ENABLE CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
   ASSIGN  /*LADDAR ÅR I CMB_AR*/
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 3,"9999")) 
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY),"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) + 1,"9999"))
   status-ok = CMB_AR:DELETE("0")
   CMB_AR:SCREEN-VALUE = STRING(YEAR(andvisdatum),"9999")
   CMB_AR = INPUT CMB_AR. 
   regar = CMB_AR.   
   tempmanad = MONTH(andvisdatum).
   CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(tempmanad,CMB_MANAD:LIST-ITEMS).
   CMB_MANAD = INPUT CMB_MANAD.    
   DISPLAY CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rad_UI WINDOW-2 
PROCEDURE rad_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   regmannamn = CMB_MANAD.
   RUN MANNR.P.
   regdatum = DATE(regmnr,01,CMB_AR).
   bdatum = regdatum.
   IF MONTH(regdatum) = 12 THEN avdatum = DATE(12,31,YEAR(regdatum)). 
   ELSE avdatum = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.           
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   EMPTY TEMP-TABLE tidpers  NO-ERROR.    
   RUN skapapers_UI.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repos_UI WINDOW-2 
PROCEDURE repos_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN 
   FILL-IN-SOKPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN_SEFTERNAMN:HIDDEN = FALSE 
   FILL-IN_SFORNAMN:HIDDEN = FALSE 
   FILL-IN_SPERSONALKOD:HIDDEN = FALSE 
   RECT-SOK:HIDDEN = FALSE
   FILL-IN-SOKPA = "Sök på:". 
   DISPLAY FILL-IN-SOKPA WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN_SEFTERNAMN FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD 
   WITH FRAME {&FRAME-NAME}.
   BRW_PERS:HIDDEN = FALSE.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.                   
   IF AVAILABLE personaltemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   ELSE DO:
      APPLY "HOME" TO BRW_PERS.
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapapers_UI WINDOW-2 
PROCEDURE skapapers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  musz = FALSE.
  EMPTY TEMP-TABLE tidpers NO-ERROR.   
  IF Guru.Konstanter:appcon THEN DO:                           
     RUN TIDMVALP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
     (INPUT personaltemp.PERSONALKOD,INPUT FALSE,INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv,INPUT 1,
     INPUT TABLE markpers,INPUT TABLE vomrtemp,OUTPUT TABLE tidpers).
  END.
  ELSE DO:
     RUN TIDMVALP.P 
      (INPUT personaltemp.PERSONALKOD,INPUT FALSE,INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv,INPUT 1,
      INPUT TABLE markpers,INPUT TABLE vomrtemp,OUTPUT TABLE tidpers).                  
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

