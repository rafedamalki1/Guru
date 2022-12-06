&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER vardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER valomradenamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vbenmark AS CHARACTER NO-UNDO.
{JURPERST.I}
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{NRSERIETEMP.I}
&Scoped-define SHARED SHARED   

{MARKVARD.I}
{OMRTEMPW.I}
{FASTIGHET.I}
{ANSVMARK.I}

&Scoped-define NEW NEW
{AONRDEF.I}
{ANVPERS.I}


DEFINE SHARED TEMP-TABLE berkalanvandartemp NO-UNDO LIKE anvandartemp.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aoplan AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE anv AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE vsist AS INTEGER NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE andravardapph AS HANDLE NO-UNDO.
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ARBAN

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ansvmarktemp natagtemp

/* Definitions for BROWSE BRW_ARBAN                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBAN ansvmarktemp.PERSONALKOD ~
ansvmarktemp.FORNAMN ansvmarktemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBAN ansvmarktemp.PERSONALKOD ~
ansvmarktemp.EFTERNAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ARBAN ansvmarktemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ARBAN ansvmarktemp
&Scoped-define QUERY-STRING-BRW_ARBAN FOR EACH ansvmarktemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_ARBAN OPEN QUERY BRW_ARBAN FOR EACH ansvmarktemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_ARBAN ansvmarktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBAN ansvmarktemp


/* Definitions for BROWSE BRW_NATAG                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_NATAG natagtemp.natagare 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NATAG 
&Scoped-define QUERY-STRING-BRW_NATAG FOR EACH natagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_NATAG OPEN QUERY BRW_NATAG FOR EACH natagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_NATAG natagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NATAG natagtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ARBAN}~
    ~{&OPEN-QUERY-BRW_NATAG}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_OMR FILL-IN_BENAMNING BRW_ARBAN ~
FILL-IN_VARDANV FILL-IN-VARTAL BRW_NATAG BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS CMB_OMR FILL-IN_VARDNR FILL-IN_BENAMNING ~
FILL-IN_ANVANDARE FILL-IN_VARDANV FILL-IN-VARTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_NATAGARE AS CHARACTER FORMAT "X(30)":U 
     LABEL "Ledningsägare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(16)":U 
     LABEL "Utförande område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VARDTIDPKT AS DATE FORMAT "9999/99/99":U 
     LABEL "Värderingstidpunkt" 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Värderingsårtal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Utfärdare" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "x(80)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 58.63 BY 1.

DEFINE VARIABLE FILL-IN_NATAGARE AS CHARACTER FORMAT "X(60)" 
     LABEL "Nätägare" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE FILL-IN_VARDANV AS CHARACTER FORMAT "X(8)" 
     LABEL "Värderingsman" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Markvärdering nr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ARBAN FOR 
      ansvmarktemp SCROLLING.

DEFINE QUERY BRW_NATAG FOR 
      natagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ARBAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBAN DIALOG-1 _STRUCTURED
  QUERY BRW_ARBAN NO-LOCK DISPLAY
      ansvmarktemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(5)":U
            WIDTH 7
      ansvmarktemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 14
      ansvmarktemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 17
  ENABLE
      ansvmarktemp.PERSONALKOD
      ansvmarktemp.EFTERNAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 43 BY 7.88
         TITLE "Värderingsman".

DEFINE BROWSE BRW_NATAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NATAG DIALOG-1 _STRUCTURED
  QUERY BRW_NATAG NO-LOCK DISPLAY
      natagtemp.natagare COLUMN-LABEL "Nätägare" FORMAT "x(256)":U
            WIDTH 50
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 43 BY 8
         TITLE "Dubbelklicka för val av nätägare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     CMB_OMR AT ROW 1.5 COL 20 COLON-ALIGNED
     FILL-IN_VARDNR AT ROW 2.88 COL 20 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 4.29 COL 20 COLON-ALIGNED
     BRW_ARBAN AT ROW 5.38 COL 52
     FILL-IN_ANVANDARE AT ROW 5.71 COL 20 COLON-ALIGNED
     FILL-IN_VARDANV AT ROW 7.13 COL 20 COLON-ALIGNED
     FILL-IN_NATAGARE AT ROW 8.54 COL 20 COLON-ALIGNED
     FILL-IN-VARTAL AT ROW 9.96 COL 20 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-VARDTIDPKT AT ROW 11.25 COL 20 COLON-ALIGNED
     BRW_NATAG AT ROW 13.58 COL 52 WIDGET-ID 100
     CMB_NATAGARE AT ROW 14.5 COL 20 COLON-ALIGNED
     BTN_REG AT ROW 21.96 COL 65.75
     BTN_AVS AT ROW 21.96 COL 80.75
     SPACE(1.62) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Markvärdering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: ansvmarktemp T "?" NO-UNDO temp-db ansvmarktemp
      TABLE: natagtemp T "?" NO-UNDO temp-db natagtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_ARBAN FILL-IN_BENAMNING DIALOG-1 */
/* BROWSE-TAB BRW_NATAG FILL-IN-VARDTIDPKT DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_NATAGARE IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_NATAGARE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VARDTIDPKT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VARDTIDPKT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ANVANDARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NATAGARE IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NATAGARE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBAN
/* Query rebuild information for BROWSE BRW_ARBAN
     _TblList          = "Temp-Tables.ansvmarktemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ansvmarktemp.PERSONALKOD
"ansvmarktemp.PERSONALKOD" "Enhet/!Sign" "X(5)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ansvmarktemp.FORNAMN
"ansvmarktemp.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ansvmarktemp.EFTERNAMN
"ansvmarktemp.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_ARBAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NATAG
/* Query rebuild information for BROWSE BRW_NATAG
     _TblList          = "Temp-Tables.natagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.natagtemp.natagare
"natagtemp.natagare" "Nätägare" "x(256)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_NATAG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Markvärdering */
DO:
   IF vart = "NYA" THEN DO TRANSACTION:
      FIND varderingtemp WHERE varderingtemp.VARDNR = vardnr NO-LOCK NO-ERROR.        
      IF AVAILABLE varderingtemp THEN DELETE varderingtemp.
   END.  
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(andravardapph) THEN DELETE PROCEDURE andravardapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   ASSIGN 
   avbryt = TRUE
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Markvärdering */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ARBAN
&Scoped-define SELF-NAME BRW_ARBAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBAN DIALOG-1
ON LEAVE OF BRW_ARBAN IN FRAME DIALOG-1 /* Värderingsman */
DO:
   FILL-IN_VARDANV = INPUT FILL-IN_VARDANV.
   FIND FIRST ansvmarktemp WHERE ansvmarktemp.PERSONALKOD = FILL-IN_VARDANV 
   USE-INDEX PKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ansvmarktemp THEN DO:
      MESSAGE 
      "Ingen ansvarig med enhet " FILL-IN_VARDANV " finns i registret!" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_VARDANV IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBAN DIALOG-1
ON VALUE-CHANGED OF BRW_ARBAN IN FRAME DIALOG-1 /* Värderingsman */
DO:
   IF AVAILABLE ansvmarktemp THEN DO:
      ASSIGN FILL-IN_VARDANV = ansvmarktemp.PERSONALKOD.
   END.
   DISPLAY FILL-IN_VARDANV WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NATAG
&Scoped-define SELF-NAME BRW_NATAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NATAG DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_NATAG IN FRAME DIALOG-1 /* Dubbelklicka för val av nätägare */
DO:
   status-ok = BRW_NATAG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   FILL-IN_NATAGARE = natagtemp.NATAGARE.
   DISPLAY FILL-IN_NATAGARE WITH FRAME {&FRAME-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO: 
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}  
   ASSIGN
   musz = FALSE  
   FILL-IN_VARDANV = INPUT FILL-IN_VARDANV 
   FILL-IN_VARDNR = INPUT FILL-IN_VARDNR 
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING
   CMB_NATAGARE = INPUT CMB_NATAGARE
   FILL-IN-VARDTIDPKT = INPUT FILL-IN-VARDTIDPKT
   CMB_OMR = INPUT CMB_OMR
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING
   FILL-IN-VARTAL = INPUT FILL-IN-VARTAL. 
   IF FILL-IN_BENAMNING = "" THEN DO:
      MESSAGE "Benämningen får ej vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:varforetypval[5] = 1 THEN FILL-IN_NATAGARE = INPUT FILL-IN_NATAGARE.
   IF FILL-IN_VARDANV = "" THEN DO:
      MESSAGE "Värderingsman kan inte vara blank!" VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_VARDANV IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END. 
   FIND FIRST ansvmarktemp WHERE ansvmarktemp.PERSONALKOD = FILL-IN_VARDANV 
   USE-INDEX PKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ansvmarktemp  THEN DO:
      MESSAGE "Värderingsman finns inte!" VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_VARDANV IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.  
   IF vart = "NYA" THEN DO:
      CMB_OMR = INPUT CMB_OMR.   
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         FIND FIRST vardinttemp WHERE vardinttemp.OMRADE = omrtemp.OMRADE USE-INDEX OMRADE NO-LOCK NO-ERROR.                
         IF NOT AVAILABLE vardinttemp THEN DO:
            MESSAGE "Det går inte att lägga upp värderingar på detta " + LC(Guru.Konstanter:gomrk) + ". Kontakta administratör!" VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN NO-APPLY.               
         END.     
         ELSE IF vardinttemp.VARDINT2 < vardinttemp.VARDSIST THEN DO:
            MESSAGE "Det går inte att lägga upp värderingar på detta " + LC(Guru.Konstanter:gomrk) + ". Kontakta administratör!" VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN NO-APPLY.            
         END.
         ELSE DO:
            ASSIGN
            FILL-IN_VARDNR = vardinttemp.VARDSIST.
            vardinttemp.VARDSIST = vardinttemp.VARDSIST + 1.
            DISPLAY FILL-IN_VARDNR WITH FRAME {&FRAME-NAME}.
         END.    
      END. 
   END.
   RUN kontroll IN andravardapph (INPUT aoplan,INPUT aonrrec,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".                           
      DELETE felmeddtemp.
      RETURN.
   END.
   vardnr = FILL-IN_VARDNR.
   IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      IF FILL-IN-VARDTIDPKT = ? THEN CMB_NATAGARE = ?.
      CMB_NATAGARE = STRING(FILL-IN-VARDTIDPKT,"9999-99-99").
   END.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      RUN btnok IN andravardapph (INPUT vart,INPUT aoplan,INPUT vardnr,INPUT omrtemp.OMRADE,
                            INPUT aonrrec,INPUT anv,INPUT FILL-IN_VARDANV,
                            INPUT FILL-IN_VARDNR,INPUT FILL-IN_BENAMNING,
                            INPUT FILL-IN_NATAGARE,INPUT CMB_NATAGARE).
   END.
   DEBUGGER:SET-BREAK().
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "VARDNRARTAL"                   
   inextradatatemp.HUVUDINT  = FILL-IN_VARDNR
   inextradatatemp.SOKINT[1] = FILL-IN-VARTAL.
   

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(andravardapph) THEN DELETE PROCEDURE andravardapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_NATAGARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_NATAGARE DIALOG-1
ON LEAVE OF CMB_NATAGARE IN FRAME DIALOG-1 /* Ledningsägare */
DO:
   CMB_NATAGARE = INPUT CMB_NATAGARE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_NATAGARE DIALOG-1
ON VALUE-CHANGED OF CMB_NATAGARE IN FRAME DIALOG-1 /* Ledningsägare */
DO:
   CMB_OMR = INPUT CMB_OMR.
 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON LEAVE OF CMB_OMR IN FRAME DIALOG-1 /* Utförande område */
DO:
   CMB_OMR = INPUT CMB_OMR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1 /* Utförande område */
DO:
   CMB_OMR = INPUT CMB_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      FIND FIRST vardinttemp WHERE vardinttemp.OMRADE = omrtemp.OMRADE USE-INDEX OMRADE NO-LOCK NO-ERROR.                
      IF NOT AVAILABLE vardinttemp THEN DO:
         MESSAGE "Det går inte att lägga upp värderingar på detta " + LC(Guru.Konstanter:gomrk) + ". Kontakta administratör!" VIEW-AS ALERT-BOX.
         ASSIGN FILL-IN_VARDNR = 0.
         DISPLAY FILL-IN_VARDNR WITH FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                                  
 
      END.     
      ELSE IF vardinttemp.VARDINT2 < vardinttemp.VARDSIST THEN DO:
         MESSAGE "Det går inte att lägga upp värderingar på detta " + LC(Guru.Konstanter:gomrk) + ". Kontakta administratör!" VIEW-AS ALERT-BOX.
         ASSIGN FILL-IN_VARDNR = 0.
         DISPLAY FILL-IN_VARDNR WITH FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                                  
         /*APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}.*/ 
      END.
      ELSE DO:
         ASSIGN        
         FILL-IN_VARDNR = vardinttemp.VARDSIST.         
      END.    
   END. 
   ELSE DO:
      ASSIGN FILL-IN_VARDNR = 0.
   END.
   DISPLAY FILL-IN_VARDNR  WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VARDTIDPKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VARDTIDPKT DIALOG-1
ON LEAVE OF FILL-IN-VARDTIDPKT IN FRAME DIALOG-1 /* Värderingstidpunkt */
DO:
  FILL-IN-VARDTIDPKT = INPUT FILL-IN-VARDTIDPKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BENAMNING DIALOG-1
ON LEAVE OF FILL-IN_BENAMNING IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING. 
   IF FILL-IN_BENAMNING = "" THEN DO:
      MESSAGE "Får ej vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   FILL-IN_BENAMNING = TRIM(FILL-IN_BENAMNING).
   DISPLAY FILL-IN_BENAMNING WITH FRAME {&FRAME-NAME}.
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN_BENAMNING,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NATAGARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NATAGARE DIALOG-1
ON ENTRY OF FILL-IN_NATAGARE IN FRAME DIALOG-1 /* Nätägare */
DO:                          
   ASSIGN
   BRW_ARBAN:HIDDEN = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VARDANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARDANV DIALOG-1
ON ENTRY OF FILL-IN_VARDANV IN FRAME DIALOG-1 /* Värderingsman */
DO:                          
   ASSIGN
   BRW_ARBAN:HIDDEN = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ARBAN
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
   RUN laddavard IN andravardapph (INPUT vart,INPUT vardnr,INPUT aonrrec,
                                      OUTPUT TABLE varderingtemp,
                                      OUTPUT TABLE uppaonrtemp,
                                      OUTPUT TABLE nrprefistemp,
                                      OUTPUT TABLE ansvmarktemp,
                                      OUTPUT TABLE anvandartemp,
                                      OUTPUT TABLE vardinttemp,
                                      INPUT-OUTPUT TABLE varderingtemp).
                                      
   RUN natag_UI IN andravardapph (INPUT Guru.Konstanter:globforetag, OUTPUT TABLE natagtemp).
   IF vart = "NYA" THEN DO:
      FIND FIRST varderingtemp NO-LOCK NO-ERROR.
      vardrec = RECID(varderingtemp).
   END.
   ELSE DO:
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = vardnr NO-LOCK NO-ERROR.
      vardrec = RECID(varderingtemp).
   END.


   {OMRHMT.I}

   CMB_OMR:LIST-ITEMS = "". 
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN). 
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   END.      
             
   RUN grundtid_UI.
   DEBUGGER:SET-BREAK().
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "VARDNRARTAL"                   
   inextradatatemp.HUVUDINT = FILL-IN_VARDNR.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      FILL-IN-VARTAL = extradatatemp.SOKINT[1].      
   END.
   ELSE FILL-IN-VARTAL = YEAR(TODAY).   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   RUN enable_UI.       
   {FRMSIZED.I}
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   IF vart = "NYA" THEN DO:      
      IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "VELD" THEN DO:
          FILL-IN_NATAGARE = "Vattenfall Eldistribution AB".
          DISPLAY FILL-IN_NATAGARE  WITH FRAME {&FRAME-NAME}.
      END.
   END.    
   IF Guru.Konstanter:varforetypval[5] = 1 THEN DO:      
      DISPLAY FILL-IN_NATAGARE  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_NATAGARE  WITH FRAME {&FRAME-NAME}.
      FILL-IN_NATAGARE:HIDDEN = FALSE.
      BRW_NATAG:HIDDEN = FALSE.
   END.   
   ELSE DO:
      DISABLE FILL-IN_NATAGARE WITH FRAME {&FRAME-NAME}.
      FILL-IN_NATAGARE:HIDDEN = TRUE.
      BRW_NATAG:HIDDEN = TRUE.
   END.   
   
   DISABLE CMB_NATAGARE WITH FRAME {&FRAME-NAME}.
   CMB_NATAGARE:HIDDEN = TRUE.
   
   IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:      
      DISPLAY FILL-IN-VARDTIDPKT  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-VARDTIDPKT  WITH FRAME {&FRAME-NAME}.
      FILL-IN-VARDTIDPKT:HIDDEN = FALSE.
   END.   
   ELSE DO:
      DISABLE FILL-IN-VARDTIDPKT WITH FRAME {&FRAME-NAME}.
      FILL-IN-VARDTIDPKT:HIDDEN = TRUE.
   END.
   {DIA_M_SLUT.I}
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = vardnr NO-LOCK NO-ERROR.
   FIND FIRST ansvmarktemp WHERE ansvmarktemp.PERSONALKOD = varderingtemp.VARDANV 
   USE-INDEX PKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ansvmarktemp THEN DO:
      IF vart = "NYA" THEN DO:
         FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
         FIND FIRST ansvmarktemp WHERE ansvmarktemp.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ansvmarktemp THEN DO:         
            APPLY "HOME" TO BRW_ARBAN.
            status-ok = BRW_ARBAN:SELECT-FOCUSED-ROW().
         END.
         ELSE DO:
            FILL-IN_VARDANV = ansvmarktemp.PERSONALKOD.
            DISPLAY FILL-IN_VARDANV WITH FRAME {&FRAME-NAME}.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ansvmarktemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.   
      END.
      ELSE DO:   
         APPLY "HOME" TO BRW_ARBAN.
         status-ok = BRW_ARBAN:SELECT-FOCUSED-ROW().
      END.   
   END.
   ELSE DO:        
      IF AVAILABLE ansvmarktemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ansvmarktemp)).
         RUN lastselectdyn_UI IN brwproc[1].    
      END.
   END.          
   IF vart = "AND" THEN DO:  
      DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.
   IF aoplan = TRUE THEN DO: 
      DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.      
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONRREC = aonrrec NO-LOCK NO-ERROR.
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE NO-LOCK NO-ERROR.      
      
      FIND FIRST vardinttemp WHERE vardinttemp.OMRADE = omrtemp.OMRADE USE-INDEX OMRADE NO-LOCK NO-ERROR.                
      
      IF AVAILABLE vardinttemp THEN DO:
         ASSIGN
         FILL-IN_VARDNR = vardinttemp.VARDSIST.
         DISPLAY FILL-IN_VARDNR  WITH FRAME {&FRAME-NAME}. 
      END.

   END.   
   CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk).
   {musarrow.i}  
   my1hand = FILL-IN_VARDANV:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_ARBAN:MOVE-AFTER-TAB-ITEM(my1hand).
   IF FILL-IN_BENAMNING = "" THEN FILL-IN_BENAMNING = vbenmark.
   DISPLAY FILL-IN_BENAMNING WITH FRAME {&FRAME-NAME}. 
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
   ansvmarktemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_ARBAN = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ARBAN:HANDLE IN FRAME {&FRAME-NAME}).
      
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_NATAG:HANDLE IN FRAME {&FRAME-NAME}).     
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      RUN ANDRAVARDAPP.P PERSISTENT SET andravardapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
      RUN ANDRAVARDAPP.P PERSISTENT SET andravardapph.
   END.
    IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
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
  DISPLAY CMB_OMR FILL-IN_VARDNR FILL-IN_BENAMNING FILL-IN_ANVANDARE 
          FILL-IN_VARDANV FILL-IN-VARTAL 
      WITH FRAME DIALOG-1.
  ENABLE CMB_OMR FILL-IN_BENAMNING BRW_ARBAN FILL-IN_VARDANV FILL-IN-VARTAL 
         BRW_NATAG BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
   IF vart = "NYA" THEN DO:
      FRAME {&FRAME-NAME}:TITLE = "Ny markvärdering".
      IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "CELPA" THEN DO: 
         FILL-IN-VARDTIDPKT = TODAY.
      END.      
      IF aoplan = FALSE THEN DO:
         
         FIND FIRST omrtemp WHERE omrtemp.NAMN = valomradenamn
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF AVAILABLE omrtemp THEN DO:
            ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.   
            APPLY "VALUE-CHANGED" TO CMB_OMR IN FRAME {&FRAME-NAME}.                       
         END.
         IF NOT AVAILABLE omrtemp THEN DO:
            FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               ASSIGN CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.   
               APPLY "VALUE-CHANGED" TO CMB_OMR IN FRAME {&FRAME-NAME}.                       
            END.
         END.
         FILL-IN_ANVANDARE = Guru.Konstanter:globanv.          
         FIND FIRST varderingtemp NO-LOCK NO-ERROR.
         ASSIGN
         varderingtemp.ANVANDARE = Guru.Konstanter:globanv.         
      END.
      ELSE DO:
         FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONRREC = aonrrec NO-LOCK NO-ERROR.
         IF AVAILABLE uppaonrtemp THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               ASSIGN  CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.             
               FILL-IN_BENAMNING = uppaonrtemp.ORT.            
            END.
            FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = vardnr NO-LOCK NO-ERROR. 
            IF AVAILABLE varderingtemp THEN DO:
               ASSIGN 
               FILL-IN_ANVANDARE = Guru.Konstanter:globanv
               varderingtemp.ANVANDARE = Guru.Konstanter:globanv.
            END.
         END.           
      END.      
      
      IF FILL-IN_ANVANDARE = "" THEN FILL-IN_ANVANDARE = Guru.Konstanter:globanv.
      anv = FILL-IN_ANVANDARE.
   END.   
   ELSE DO:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = varderingtemp.OMRADE 
      USE-INDEX OMR NO-LOCK NO-ERROR. 
      IF AVAILABLE omrtemp THEN DO:
         ASSIGN  CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = omrtemp.NAMN.
      END.
      
      ASSIGN
      FILL-IN_ANVANDARE = SUBSTRING(varderingtemp.ANVANDARE,1,39)     
      FILL-IN_VARDANV = varderingtemp.VARDANV
      FILL-IN_VARDNR = varderingtemp.VARDNR
      FILL-IN_BENAMNING = varderingtemp.BENAMNING.
      IF FILL-IN_ANVANDARE = "" THEN FILL-IN_ANVANDARE = Guru.Konstanter:globanv.
      anv = FILL-IN_ANVANDARE.
      FIND FIRST berkalanvandartemp WHERE berkalanvandartemp.ANVANDARE =  FILL-IN_ANVANDARE NO-LOCK NO-ERROR.
      IF AVAILABLE berkalanvandartemp THEN FILL-IN_ANVANDARE = TRIM(FILL-IN_ANVANDARE) + " -- " + berkalanvandartemp.AV-NAMN.
      IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "CELPA" THEN DO: 
         FILL-IN-VARDTIDPKT = DATE(SUBSTRING(varderingtemp.ANVANDARE,40,20)).     
      END.
      IF Guru.Konstanter:varforetypval[5] = 1 THEN FILL-IN_NATAGARE = varderingtemp.NATAGARE.      
      
   END.   
   IF FILL-IN_ANVANDARE = "" THEN FILL-IN_ANVANDARE = Guru.Konstanter:globanv.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

