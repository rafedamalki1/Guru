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

  Created: 95/06/20 -  1:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
{DHMT.I}
{VHMT.I}
{SOKDEF.I}
DEFINE TEMP-TABLE extraveckoarbtemp NO-UNDO LIKE veckoarbtemp.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vecosrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE nummervar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidsummasek AS INTEGER NO-UNDO.
DEFINE BUFFER vecktidbuff FOR veckoarbtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES veckoarbtemp arbtidtemp

/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define QUERY-STRING-DIALOG-1 FOR EACH veckoarbtemp NO-LOCK, ~
      EACH arbtidtemp WHERE TRUE /* Join to veckoarbtemp incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-1 OPEN QUERY DIALOG-1 FOR EACH veckoarbtemp NO-LOCK, ~
      EACH arbtidtemp WHERE TRUE /* Join to veckoarbtemp incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-1 veckoarbtemp arbtidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-1 veckoarbtemp
&Scoped-define SECOND-TABLE-IN-QUERY-DIALOG-1 arbtidtemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-VESCH CMB_MAN CMB_TIS CMB_ONS ~
CMB_TOR CMB_FRE CMB_LOR CMB_SON BTN_OK BTN_AVB FILL-IN-TOTVARB ~
FILL-IN-DAGSCH FILL-IN-TOTARB FILL-IN-ARBBORJ FILL-IN-ARBSLUT ~
FILL-IN-FRUBORJ FILL-IN-FRUSLUT FILL-IN-LUNBORJ FILL-IN-LUNSLUT ~
FILL-IN-KAFBORJ FILL-IN-KAFSLUT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VESCH FILL-IN-TOTVARB ~
FILL-IN-DAGSCH FILL-IN-TOTARB FILL-IN-ARBBORJ FILL-IN-ARBSLUT ~
FILL-IN-FRUBORJ FILL-IN-FRUSLUT FILL-IN-LUNBORJ FILL-IN-LUNSLUT ~
FILL-IN-KAFBORJ FILL-IN-KAFSLUT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_FRE AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "fredag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_LOR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "lördag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_MAN AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "måndag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_ONS AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "onsdag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_SON AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "söndag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_TIS AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "tisdag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_TOR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "torsdag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-ARBBORJ AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets början" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-ARBSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets slut" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAGSCH AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Dagschema" 
      VIEW-AS TEXT 
     SIZE 3.75 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRUBORJ AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Frukost börjar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRUSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Frukost slutar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-KAFBORJ AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Kafferast börjar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-KAFSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Kafferast slutar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-LUNBORJ AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Lunch börjar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-LUNSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Lunch slutar" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTARB AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Tot arbetstid" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTVARB AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Tot arbetstid/vecka" 
      VIEW-AS TEXT 
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-VESCH AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Veckoschema" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-1 FOR 
      veckoarbtemp, 
      arbtidtemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-VESCH AT ROW 2.63 COL 41 COLON-ALIGNED
     CMB_MAN AT ROW 4 COL 10.5 COLON-ALIGNED
     CMB_TIS AT ROW 5.5 COL 10.5 COLON-ALIGNED
     CMB_ONS AT ROW 7 COL 10.5 COLON-ALIGNED
     CMB_TOR AT ROW 8.5 COL 10.5 COLON-ALIGNED
     CMB_FRE AT ROW 10 COL 10.5 COLON-ALIGNED
     CMB_LOR AT ROW 11.5 COL 10.5 COLON-ALIGNED
     CMB_SON AT ROW 13 COL 10.5 COLON-ALIGNED
     BTN_OK AT ROW 14.38 COL 46.75
     BTN_AVB AT ROW 14.38 COL 61.75
     FILL-IN-TOTVARB AT ROW 2.63 COL 70 COLON-ALIGNED
     FILL-IN-DAGSCH AT ROW 5.46 COL 39.25 COLON-ALIGNED
     FILL-IN-TOTARB AT ROW 5.46 COL 67.75 COLON-ALIGNED
     FILL-IN-ARBBORJ AT ROW 7.08 COL 39.25 COLON-ALIGNED
     FILL-IN-ARBSLUT AT ROW 7.08 COL 67.75 COLON-ALIGNED
     FILL-IN-FRUBORJ AT ROW 8.71 COL 39.25 COLON-ALIGNED
     FILL-IN-FRUSLUT AT ROW 8.71 COL 67.75 COLON-ALIGNED
     FILL-IN-LUNBORJ AT ROW 10.38 COL 39.25 COLON-ALIGNED
     FILL-IN-LUNSLUT AT ROW 10.38 COL 67.75 COLON-ALIGNED
     FILL-IN-KAFBORJ AT ROW 12 COL 39.25 COLON-ALIGNED
     FILL-IN-KAFSLUT AT ROW 12 COL 67.75 COLON-ALIGNED
     "Välj dagschema för :" VIEW-AS TEXT
          SIZE 23.25 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     SPACE(53.25) SKIP(12.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Skapa nytt eller ändra veckoschema":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: arbtidtemp T "?" NO-UNDO temp-db arbtidtemp
      TABLE: veckoarbtemp T "?" NO-UNDO temp-db veckoarbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_FRE IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_LOR IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_MAN IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_ONS IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_SON IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_TIS IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_TOR IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _TblList          = "Temp-Tables.veckoarbtemp,Temp-Tables.arbtidtemp WHERE Temp-Tables.veckoarbtemp ..."
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Skapa nytt eller ändra veckoschema */
DO:
   FIND FIRST veckoarbtemp WHERE RECID(veckoarbtemp) = vecosrec NO-LOCK NO-ERROR.
   IF vart = 1 THEN DO: 
      IF AVAILABLE veckoarbtemp THEN DELETE veckoarbtemp.
   END.
   ELSE IF vart = 2 THEN DO: 
      IF AVAILABLE veckoarbtemp THEN BUFFER-COPY extraveckoarbtemp TO veckoarbtemp.
   END.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Skapa nytt eller ändra veckoschema */
DO:
   APPLY "END-ERROR" TO FRAME DIALOG-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:   
   APPLY "END-ERROR" TO FRAME DIALOG-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}
   nummervar = veckoarbtemp.VECKOSCHEMA.
   IF vart = 1 THEN DO:
      RUN andra_UI (INPUT 1).
   END.
   ELSE IF vart = 2 THEN DO:
      RUN andra_UI (INPUT 2).           
   END.
   FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA = nummervar 
   NO-LOCK NO-ERROR.
   RUN vectot_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FRE DIALOG-1
ON VALUE-CHANGED OF CMB_FRE IN FRAME DIALOG-1 /* fredag */
DO:
   CMB_FRE = INPUT CMB_FRE.
   ASSIGN veckoarbtemp.ARBTIDFRE = CMB_FRE.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDFRE 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LOR DIALOG-1
ON VALUE-CHANGED OF CMB_LOR IN FRAME DIALOG-1 /* lördag */
DO:
   CMB_LOR = INPUT CMB_LOR.
   ASSIGN veckoarbtemp.ARBTIDLOR = CMB_LOR.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDLOR 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MAN DIALOG-1
ON VALUE-CHANGED OF CMB_MAN IN FRAME DIALOG-1 /* måndag */
DO:
   CMB_MAN = INPUT CMB_MAN.
   ASSIGN veckoarbtemp.ARBTIDMAN = CMB_MAN.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDMAN 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ONS DIALOG-1
ON VALUE-CHANGED OF CMB_ONS IN FRAME DIALOG-1 /* onsdag */
DO:
   CMB_ONS = INPUT CMB_ONS.
   ASSIGN veckoarbtemp.ARBTIDONS = CMB_ONS.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDONS 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_SON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_SON DIALOG-1
ON VALUE-CHANGED OF CMB_SON IN FRAME DIALOG-1 /* söndag */
DO:
   CMB_SON = INPUT CMB_SON.
   ASSIGN veckoarbtemp.ARBTIDSON = CMB_SON.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDSON 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TIS DIALOG-1
ON VALUE-CHANGED OF CMB_TIS IN FRAME DIALOG-1 /* tisdag */
DO:
   CMB_TIS = INPUT CMB_TIS.
   ASSIGN veckoarbtemp.ARBTIDTIS = CMB_TIS.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTIS 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TOR DIALOG-1
ON VALUE-CHANGED OF CMB_TOR IN FRAME DIALOG-1 /* torsdag */
DO:
   CMB_TOR = INPUT CMB_TOR.
   ASSIGN veckoarbtemp.ARBTIDTOR = CMB_TOR.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTOR 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   RUN visa_UI.
   RUN vectot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VESCH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VESCH DIALOG-1
ON LEAVE OF FILL-IN-VESCH IN FRAME DIALOG-1 /* Veckoschema */
DO:

   ASSIGN veckoarbtemp.VECKOSCHEMA = INPUT FILL-IN-VESCH.
   FIND FIRST vecktidbuff WHERE vecktidbuff.VECKOSCHEMA =  veckoarbtemp.VECKOSCHEMA AND
   RECID(vecktidbuff) NE vecosrec USE-INDEX VECKOSCHEMA NO-LOCK NO-ERROR.
   IF AVAILABLE vecktidbuff THEN DO:
      IF RECID(vecktidbuff) NE vecosrec THEN DO:
         MESSAGE "Veckoschema" veckoarbtemp.VECKOSCHEMA " finns redan upplagt!"
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ASSIGN veckoarbtemp.VECKOSCHEMA = INPUT FILL-IN-VESCH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   FIND FIRST veckoarbtemp WHERE RECID(veckoarbtemp) = vecosrec NO-LOCK NO-ERROR.
   IF AVAILABLE veckoarbtemp AND vart = 2 THEN DO: 
      CREATE extraveckoarbtemp.
      BUFFER-COPY veckoarbtemp TO extraveckoarbtemp.
   END.
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_MAN:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_MAN:DELETE("0").                                                   
   ASSIGN  CMB_MAN:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDMAN).                     
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_TIS:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_TIS:DELETE("0").                                                   
   ASSIGN  CMB_TIS:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDTIS).                     
                                                                                    
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_ONS:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_ONS:DELETE("0").                                                   
   ASSIGN  CMB_ONS:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDONS).                     
                                                                                      
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_TOR:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_TOR:DELETE("0").                                                   
   ASSIGN  CMB_TOR:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDTOR).                     
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_FRE:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_FRE:DELETE("0").                                                   
   ASSIGN  CMB_FRE:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDFRE).                     
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_LOR:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_LOR:DELETE("0").                                                   
   ASSIGN  CMB_LOR:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDLOR).                     
   FOR EACH arbtidtemp USE-INDEX ARBTIDKOD NO-LOCK:                                   
      status-ok = CMB_SON:ADD-LAST(STRING(arbtidtemp.ARBTIDKOD)).                     
   END.                                                                               
   status-ok = CMB_SON:DELETE("0").                                                   
   ASSIGN  CMB_SON:SCREEN-VALUE = STRING(veckoarbtemp.ARBTIDSON).                     
                                                                                     
   RUN vectot_UI.                                                                        
                                                                                    
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDMAN          
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
   ASSIGN
   FILL-IN-VESCH = veckoarbtemp.VECKOSCHEMA
   FILL-IN-TOTVARB = veckoarbtemp.TOTVECKTID                    
   FILL-IN-DAGSCH = arbtidtemp.ARBTIDKOD
   FILL-IN-TOTARB = arbtidtemp.TOTDAGTID
   FILL-IN-ARBBORJ = arbtidtemp.START 
   FILL-IN-ARBSLUT = arbtidtemp.SLUT 
   FILL-IN-FRUBORJ = arbtidtemp.FRUKOSTSTART  
   FILL-IN-FRUSLUT = arbtidtemp.FRUKOSTSLUT   
   FILL-IN-LUNBORJ = arbtidtemp.LUNCHSTART   
   FILL-IN-LUNSLUT = arbtidtemp.LUNCHSLUT   
   FILL-IN-KAFBORJ = arbtidtemp.KAFESTART    
   FILL-IN-KAFSLUT = arbtidtemp.KAFESLUT.
   RUN enable_UI.
   IF vart = 2 THEN DO:   /*Om ändra*/
      DISABLE FILL-IN-VESCH FILL-IN-TOTVARB WITH FRAME {&FRAME-NAME}.
   END.
   {FRMSIZED.I} 
   FIND FIRST veckoarbtemp WHERE RECID(veckoarbtemp) = vecosrec NO-LOCK NO-ERROR.
   {musarrow.i}         
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.                                                
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER vartvar AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VECKAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vartvar,INPUT arbtidtemp.ARBTIDKOD,INPUT veckoarbtemp.VECKOSCHEMA,
      INPUT-OUTPUT felvar,OUTPUT TABLE felmeddtemp,
      INPUT-OUTPUT TABLE veckoarbtemp,INPUT-OUTPUT TABLE arbtidtemp).                               
   END.
   ELSE DO:
      RUN VECKAPP.P  
      (INPUT vartvar,INPUT arbtidtemp.ARBTIDKOD,INPUT veckoarbtemp.VECKOSCHEMA,
      INPUT-OUTPUT felvar,OUTPUT TABLE felmeddtemp,
      INPUT-OUTPUT TABLE veckoarbtemp,INPUT-OUTPUT TABLE arbtidtemp).                               
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
  DISPLAY FILL-IN-VESCH FILL-IN-TOTVARB FILL-IN-DAGSCH FILL-IN-TOTARB 
          FILL-IN-ARBBORJ FILL-IN-ARBSLUT FILL-IN-FRUBORJ FILL-IN-FRUSLUT 
          FILL-IN-LUNBORJ FILL-IN-LUNSLUT FILL-IN-KAFBORJ FILL-IN-KAFSLUT 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-VESCH CMB_MAN CMB_TIS CMB_ONS CMB_TOR CMB_FRE CMB_LOR CMB_SON 
         BTN_OK BTN_AVB FILL-IN-TOTVARB FILL-IN-DAGSCH FILL-IN-TOTARB 
         FILL-IN-ARBBORJ FILL-IN-ARBSLUT FILL-IN-FRUBORJ FILL-IN-FRUSLUT 
         FILL-IN-LUNBORJ FILL-IN-LUNSLUT FILL-IN-KAFBORJ FILL-IN-KAFSLUT 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vectot_UI DIALOG-1 
PROCEDURE vectot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    tidsummasek = 0.
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDMAN    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTIS    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDONS    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTOR    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDFRE    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDLOR    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDSON    
    USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                        
    nytid = arbtidtemp.TOT.                                                      
    RUN TIMSEK.P.                                                                
    tidsummasek = tidsummasek + sekunder.                                        
    sekunder = tidsummasek.                                                      
    RUN SEKTIM.P.                                                                
    ASSIGN veckoarbtemp.TOTVECKTID = nytid.                                      
    ASSIGN
    FILL-IN-TOTVARB = veckoarbtemp.TOTVECKTID.                    
    DISPLAY FILL-IN-TOTVARB 
     WITH FRAME {&FRAME-NAME}. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI DIALOG-1 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   FILL-IN-VESCH = veckoarbtemp.VECKOSCHEMA
   FILL-IN-DAGSCH = arbtidtemp.ARBTIDKOD
   FILL-IN-TOTARB = arbtidtemp.TOTDAGTID
   FILL-IN-ARBBORJ = arbtidtemp.START 
   FILL-IN-ARBSLUT = arbtidtemp.SLUT 
   FILL-IN-FRUBORJ = arbtidtemp.FRUKOSTSTART  
   FILL-IN-FRUSLUT = arbtidtemp.FRUKOSTSLUT   
   FILL-IN-LUNBORJ = arbtidtemp.LUNCHSTART   
   FILL-IN-LUNSLUT = arbtidtemp.LUNCHSLUT   
   FILL-IN-KAFBORJ = arbtidtemp.KAFESTART    
   FILL-IN-KAFSLUT = arbtidtemp.KAFESLUT.                                                                                   
   DISPLAY FILL-IN-VESCH FILL-IN-TOTVARB FILL-IN-DAGSCH 
            FILL-IN-TOTARB FILL-IN-ARBBORJ FILL-IN-ARBSLUT 
            FILL-IN-FRUBORJ FILL-IN-FRUSLUT FILL-IN-LUNBORJ 
            FILL-IN-LUNSLUT FILL-IN-KAFBORJ FILL-IN-KAFSLUT 
   WITH FRAME {&FRAME-NAME}. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

