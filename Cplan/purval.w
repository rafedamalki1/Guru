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

  Created: 95/10/26 -  6:24 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{PRIARBANLTEMP.I}
&Scoped-define SHARED SHARED
{OMRTEMPW.I}
{REGVAR.I}
{ANSVPLANTEMP.I} 
{AUTOMREGTEMP.I}

DEFINE SHARED VARIABLE listnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE muszval AS INTEGER NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE urvalapph AS HANDLE NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED  
{BESTKUNDALLT.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_ARBAN

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ansvplantemp arbarttemp

/* Definitions for BROWSE BRW_ARBAN                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBAN ansvplantemp.PERSONALKOD ~
ansvplantemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBAN 
&Scoped-define QUERY-STRING-BRW_ARBAN FOR EACH ansvplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBAN OPEN QUERY BRW_ARBAN FOR EACH ansvplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBAN ansvplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBAN ansvplantemp


/* Definitions for BROWSE BRW_ARBART                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBART arbarttemp.ARBARTKOD ~
arbarttemp.ARBBENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBART arbarttemp.ARBARTKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ARBART arbarttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ARBART arbarttemp
&Scoped-define QUERY-STRING-BRW_ARBART FOR EACH arbarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBART OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBART arbarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBART arbarttemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_ARBAN}~
    ~{&OPEN-QUERY-BRW_ARBART}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_KLAR BTN_AVB 

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

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-HELP AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HELP-2 AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     LABEL "Debitering" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)" INITIAL ? 
     LABEL "Anl.nr." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)" INITIAL ? 
     LABEL "Arbetsansvarig" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL ? 
     LABEL "Arbetsart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(5)" INITIAL ? 
     LABEL "Beredare/Projektör" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" INITIAL ? 
     LABEL "Område" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL ? 
     LABEL "Prioritet" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_SLUTVNR AS INTEGER FORMAT "999" INITIAL ? 
     LABEL "Slut  vnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN_STARTVNR AS INTEGER FORMAT "999" INITIAL ? 
     LABEL "Start vnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE SEL_BESORG AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 27.5 BY 11.67 NO-UNDO.

DEFINE VARIABLE SEL_DEBPR AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 18 BY 7.29 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANL AS LOGICAL INITIAL no 
     LABEL "Alla anläggningar" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_ARBINA AS LOGICAL INITIAL no 
     LABEL "Även inaktiva arbetsarter" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ARBAN FOR 
      ansvplantemp SCROLLING.

DEFINE QUERY BRW_ARBART FOR 
      arbarttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ARBAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBAN DIALOG-1 _STRUCTURED
  QUERY BRW_ARBAN NO-LOCK DISPLAY
      ansvplantemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      ansvplantemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 42 BY 14
         TITLE "Välj arbetsansvarig".

DEFINE BROWSE BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBART DIALOG-1 _STRUCTURED
  QUERY BRW_ARBART NO-LOCK DISPLAY
      arbarttemp.ARBARTKOD COLUMN-LABEL "Arbetsart" FORMAT ">>>":U
      arbarttemp.ARBBENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
  ENABLE
      arbarttemp.ARBARTKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 42 BY 14
         TITLE "Välj arbetsart".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_ANLNR AT ROW 1.79 COL 11 COLON-ALIGNED
     FILL-IN_OMRADE AT ROW 1.79 COL 33.5 COLON-ALIGNED
     FILL-IN-PRISTYP AT ROW 1.79 COL 48.5 COLON-ALIGNED
     FILL-IN_PKOD AT ROW 2.04 COL 10 COLON-ALIGNED
     SEL_BESORG AT ROW 2.63 COL 14.5 NO-LABEL
     SEL_DEBPR AT ROW 2.75 COL 14.5 NO-LABEL
     TOG_ARBINA AT ROW 3.04 COL 24.5
     FILL-IN_BEREDARE AT ROW 3.04 COL 40 COLON-ALIGNED
     FILL-IN_ARBARTKOD AT ROW 3.04 COL 54.5 COLON-ALIGNED
     FILL-IN_ARBANSVARIG AT ROW 3.54 COL 15 COLON-ALIGNED
     TOG_ANL AT ROW 3.54 COL 24.5
     BRW_ARBART AT ROW 4.29 COL 1.5
     BRW_ARBAN AT ROW 4.29 COL 1.5
     BTN_KLAR AT ROW 5.04 COL 62
     BTN_NVE AT ROW 5.58 COL 27.63
     BTN_NVE-2 AT ROW 5.58 COL 46.88
     FILL-IN_STARTVNR AT ROW 6.04 COL 21 COLON-ALIGNED
     FILL-IN_SLUTVNR AT ROW 6.04 COL 40 COLON-ALIGNED
     BTN_FVE AT ROW 6.67 COL 27.63
     BTN_FVE-2 AT ROW 6.67 COL 46.88
     FILL-IN-HELP AT ROW 7.54 COL 9.5 COLON-ALIGNED NO-LABEL
     FILL-IN-HELP-2 AT ROW 8.54 COL 9.5 COLON-ALIGNED NO-LABEL
     BTN_AVB AT ROW 18.58 COL 62
     "Välj:" VIEW-AS TEXT
          SIZE 8.5 BY 1.5 AT ROW 1.63 COL 1.5
          FONT 17
     SPACE(66.74) SKIP(16.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: ansvplantemp T "?" NO-UNDO temp-db ansvplantemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ARBART TOG_ANL DIALOG-1 */
/* BROWSE-TAB BRW_ARBAN BRW_ARBART DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_ARBAN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_ARBAN:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_ARBAN:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300
       BRW_ARBAN:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_ARBAN:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR BROWSE BRW_ARBART IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_ARBART:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-HELP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-HELP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-HELP-2 IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-HELP-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ANLNR IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_ANLNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ARBANSVARIG IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_ARBANSVARIG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ARBARTKOD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_ARBARTKOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BEREDARE IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_BEREDARE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_OMRADE IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_OMRADE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PKOD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_PKOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SLUTVNR IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_SLUTVNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_STARTVNR IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN_STARTVNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_BESORG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       SEL_BESORG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_DEBPR IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       SEL_DEBPR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ANL IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_ANL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ARBINA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_ARBINA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBAN
/* Query rebuild information for BROWSE BRW_ARBAN
     _TblList          = "Temp-Tables.ansvplantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ansvplantemp.PERSONALKOD
"ansvplantemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ansvplantemp.EFTERNAMN
"ansvplantemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ARBAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBART
/* Query rebuild information for BROWSE BRW_ARBART
     _TblList          = "Temp-Tables.arbarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbarttemp.ARBARTKOD
"arbarttemp.ARBARTKOD" "Arbetsart" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.arbarttemp.ARBBENAMNING
"arbarttemp.ARBBENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ARBART */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK  KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Välj */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.
   IF VALID-HANDLE(urvalapph) THEN DELETE PROCEDURE urvalapph.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Välj */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(urvalapph) THEN DELETE PROCEDURE urvalapph.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON WINDOW-CLOSE OF FRAME DIALOG-1 /* Välj */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(urvalapph) THEN DELETE PROCEDURE urvalapph.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ARBAN
&Scoped-define SELF-NAME BRW_ARBAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBAN DIALOG-1
ON VALUE-CHANGED OF BRW_ARBAN IN FRAME DIALOG-1 /* Välj arbetsansvarig */
DO:   
   FILL-IN_ARBANSVARIG = ansvplantemp.PERSONALKOD.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ARBART
&Scoped-define SELF-NAME BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBART DIALOG-1
ON VALUE-CHANGED OF BRW_ARBART IN FRAME DIALOG-1 /* Välj arbetsart */
DO:
   FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:     
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   DEFINE VARIABLE regdd AS CHARACTER NO-UNDO.
   ASSIGN
   FILL-IN_STARTVNR = INPUT FILL-IN_STARTVNR      
   regdd = regdagnamn
   regdagnamn = "ONS"
   regvnr = FILL-IN_STARTVNR.
   RUN VECODAT.P.
   ASSIGN
   regdagnamn = regdd
   regdatum = regdatum - 7.   
   RUN REGVEC.P.
   FILL-IN_STARTVNR = regvnr.
   DISPLAY FILL-IN_STARTVNR WITH FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   DEFINE VARIABLE regdd AS CHARACTER NO-UNDO.
   ASSIGN
   FILL-IN_SLUTVNR = INPUT FILL-IN_SLUTVNR       
   regdd = regdagnamn
   regdagnamn = "ONS"
   regvnr = FILL-IN_SLUTVNR.
   RUN VECODAT.P.
   ASSIGN
   regdagnamn = regdd
   regdatum = regdatum - 7.   
   RUN REGVEC.P.
   FILL-IN_SLUTVNR = regvnr.
   DISPLAY FILL-IN_SLUTVNR WITH FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR DIALOG-1
ON CHOOSE OF BTN_KLAR IN FRAME DIALOG-1 /* Visa */
DO:
   musz = FALSE.
   IF listnr = 2 AND SEL_DEBPR = "" THEN DO:
      MESSAGE "Välj en debiteringstyp!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF listnr = 6 AND (SEL_BESORG = "" OR SEL_BESORG = ?) THEN DO:
      MESSAGE "Välj en " Guru.Konstanter:gbestk "!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   IF listnr = 5 AND (FILL-IN_SLUTVNR < FILL-IN_STARTVNR) THEN DO:            
      ASSIGN
      regvnr = FILL-IN_STARTVNR
      regdagnamn = 'MÅN'.
      RUN VECODAT.P.         
      ASSIGN
      bdatum = regdatum
      regvnr = FILL-IN_SLUTVNR
      regdagnamn = 'MÅN'.
      RUN VECODAT.P.          
      ASSIGN
      avdatum = regdatum.              
      IF avdatum < bdatum THEN DO:        
         IF FILL-IN_SLUTVNR = 0 THEN FILL-IN_SLUTVNR = FILL-IN_SLUTVNR.      
         ELSE DO:
            MESSAGE 
            "Periodens startvnr kan inte var större än slutvnr." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_STARTVNR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_KLAR IN FRAME {&FRAME-NAME}. 
         END.
      END.
   END.    
   {BORTBRWPROC.I}
   IF VALID-HANDLE(urvalapph) THEN DELETE PROCEDURE urvalapph.   
   {muswait.i}    
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:
   DEFINE VARIABLE regdd AS CHARACTER NO-UNDO.
   ASSIGN
   FILL-IN_STARTVNR = INPUT FILL-IN_STARTVNR      
   regdd = regdagnamn
   regdagnamn = "ONS"
   regvnr = FILL-IN_STARTVNR.
   RUN VECODAT.P.
   ASSIGN
   regdagnamn = regdd
   regdatum = regdatum + 7.   
   RUN REGVEC.P.
   FILL-IN_STARTVNR = regvnr.
   DISPLAY FILL-IN_STARTVNR WITH FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:
   DEFINE VARIABLE regdd AS CHARACTER NO-UNDO.
   ASSIGN
   FILL-IN_SLUTVNR = INPUT FILL-IN_SLUTVNR       
   regdd = regdagnamn
   regdagnamn = "ONS"
   regvnr = FILL-IN_SLUTVNR.
   RUN VECODAT.P.
   ASSIGN
   regdagnamn = regdd
   regdatum = regdatum + 7.   
   RUN REGVEC.P.
   FILL-IN_SLUTVNR = regvnr.
   DISPLAY FILL-IN_SLUTVNR WITH FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUTVNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTVNR DIALOG-1
ON LEAVE OF FILL-IN_SLUTVNR IN FRAME DIALOG-1 /* Slut  vnr */
DO:
   FILL-IN_SLUTVNR = INPUT FILL-IN_SLUTVNR.
   IF FILL-IN_SLUTVNR < FILL-IN_STARTVNR THEN DO:
      IF FILL-IN_SLUTVNR = 0 THEN FILL-IN_SLUTVNR = FILL-IN_SLUTVNR.
      ELSE DO:
         MESSAGE 
         "Periodens startvnr kan inte var större än slutvnr."
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.  
   IF FILL-IN_SLUTVNR = 0 THEN FILL-IN_SLUTVNR = FILL-IN_SLUTVNR.
   ELSE DO:    
      regvnr = FILL-IN_SLUTVNR.
      RUN VECOKOLL.P. 
      regvnr = FILL-IN_STARTVNR.  
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTVNR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_SLUTVNR IN FRAME DIALOG-1 /* Slut  vnr */
DO:
   ASSIGN
   FILL-IN_SLUTVNR = INPUT FILL-IN_SLUTVNR
   regvnr = INPUT FILL-IN_SLUTVNR
   regdagnamn = "MÅN".
   RUN VECOKOLL.P.
   regvnr = FILL-IN_SLUTVNR.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   RUN KALENDER.W.
   FILL-IN_SLUTVNR = regvnr.
   DISPLAY FILL-IN_SLUTVNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_STARTVNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STARTVNR DIALOG-1
ON LEAVE OF FILL-IN_STARTVNR IN FRAME DIALOG-1 /* Start vnr */
DO:
   FILL-IN_STARTVNR = INPUT FILL-IN_STARTVNR.
   IF FILL-IN_SLUTVNR < FILL-IN_STARTVNR THEN DO:
      IF FILL-IN_STARTVNR = 0 THEN FILL-IN_SLUTVNR = FILL-IN_SLUTVNR.
      ELSE FILL-IN_SLUTVNR = FILL-IN_STARTVNR.
   END.
   IF FILL-IN_STARTVNR = 0 THEN FILL-IN_SLUTVNR = FILL-IN_SLUTVNR.
   ELSE DO:
      regvnr = FILL-IN_STARTVNR.
      RUN VECOKOLL.P. 
      regvnr = FILL-IN_STARTVNR.  
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
      DISPLAY FILL-IN_SLUTVNR WITH FRAME {&FRAME-NAME}. 
   END.
   APPLY "ENTRY" TO FILL-IN_SLUTVNR IN FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STARTVNR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_STARTVNR IN FRAME DIALOG-1 /* Start vnr */
DO:
   ASSIGN
   FILL-IN_STARTVNR = INPUT FILL-IN_STARTVNR
   regvnr = INPUT FILL-IN_STARTVNR
   regdagnamn = "MÅN".
   RUN VECOKOLL.P.
   regvnr = FILL-IN_STARTVNR.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   RUN KALENDER.W.
   FILL-IN_STARTVNR = regvnr.
   DISPLAY FILL-IN_STARTVNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_BESORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_BESORG DIALOG-1
ON VALUE-CHANGED OF SEL_BESORG IN FRAME DIALOG-1
DO:
   SEL_BESORG = INPUT SEL_BESORG.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = SEL_BESORG USE-INDEX BESTID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:
      MESSAGE "Allvarligt fel kontakta ansvarig." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = bestkundallt.BESTID
      USE-INDEX OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         FILL-IN_OMRADE = bestkundallt.BESTID.
      END.
      ELSE FILL-IN_OMRADE = omrtemp.OMRADE.     
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_DEBPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_DEBPR DIALOG-1
ON VALUE-CHANGED OF SEL_DEBPR IN FRAME DIALOG-1
DO:         
   ASSIGN
   SEL_DEBPR = INPUT SEL_DEBPR
   FILL-IN-PRISTYP = SEL_DEBPR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ANL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ANL DIALOG-1
ON VALUE-CHANGED OF TOG_ANL IN FRAME DIALOG-1 /* Alla anläggningar */
DO:
   TOG_ANL = INPUT TOG_ANL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ARBINA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ARBINA DIALOG-1
ON VALUE-CHANGED OF TOG_ARBINA IN FRAME DIALOG-1 /* Även inaktiva arbetsarter */
DO:
  TOG_ARBINA = INPUT TOG_ARBINA.
  IF TOG_ARBINA = TRUE THEN DO:
     RUN laddaarballa IN urvalapph (OUTPUT TABLE arbarttemp).
     OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
     GET FIRST BRW_ARBART NO-LOCK.
     IF AVAILABLE arbarttemp THEN DO: 
        APPLY "HOME" TO BRW_ARBART.
        status-ok = BRW_ARBART:SELECT-FOCUSED-ROW() NO-ERROR.
        ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
     END.     
  END.
  ELSE DO:
     RUN laddaarbaktiv IN urvalapph (OUTPUT TABLE arbarttemp).
     OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
     GET FIRST BRW_ARBART NO-LOCK.
     IF AVAILABLE arbarttemp THEN DO: 
        APPLY "HOME" TO BRW_ARBART.
        status-ok = BRW_ARBART:SELECT-FOCUSED-ROW() NO-ERROR.
        ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
     END.     
  END.
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

   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 2.
      {BESTHMT.I}      
   END.
   
   IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:

   END.
   ELSE DO:
      FOR EACH omrtemp:
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE NO-ERROR.
         IF NOT AVAILABLE bestkundallt THEN DO:
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
         END.      
      END.  
   END.
   FOR EACH bestkundallt USE-INDEX BESTID NO-LOCK:      
      status-ok = SEL_BESORG:ADD-LAST(bestkundallt.BESTNAMN).
   END. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PURVALAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE ansvplantemp,OUTPUT TABLE automregtemp). 
   END.
   ELSE DO:
      RUN PURVALAPP.P 
      (OUTPUT TABLE ansvplantemp,OUTPUT TABLE automregtemp).                  
   END.

   FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK:
      status-ok = SEL_DEBPR:ADD-LAST(automregtemp.PRISTYP).
   END.  
   IF listnr = 1 OR listnr = 19 THEN DO:      
      LEAVE MAIN-BLOCK.
   END.
/*    IF listnr = 9 THEN DO: */
/*       LEAVE MAIN-BLOCK.   */
/*    END.                   */
   ASSIGN
   BRW_ARBART:TITLE = "Välj" + Guru.Konstanter:gartk
   arbarttemp.ARBARTKOD:LABEL IN BROWSE BRW_ARBART = Guru.Konstanter:gartk 
   FILL-IN_ARBARTKOD:LABEL = Guru.Konstanter:gartk
   TOG_ARBINA:LABEL = "Även inaktiva " + LC(Guru.Konstanter:gartk)
   FILL-IN_PKOD:LABEL = Guru.Konstanter:gpriok
   FILL-IN-PRISTYP:LABEL = Guru.Konstanter:gdebk
   BRW_ARBAN:TITLE = "Välj " + LC(Guru.Konstanter:garbak)
/*    BRW_BERED:TITLE = "Välj " + LC(Guru.Konstanter:gberek) */
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbal 
   FILL-IN_BEREDARE:LABEL = Guru.Konstanter:gberel.
   RUN enable_UI.       
   {FRMSIZED.I}   
   IF listnr = 2 THEN DO:
      ENABLE SEL_DEBPR WITH FRAME {&FRAME-NAME}.
      SEL_DEBPR:HIDDEN = FALSE.
   END.
   ELSE IF listnr = 3 THEN DO:
      TOG_ARBINA = FALSE.
      TOG_ARBINA:HIDDEN = FALSE.
      ENABLE BRW_ARBART TOG_ARBINA WITH FRAME {&FRAME-NAME}.
      BRW_ARBART:HIDDEN = FALSE.
      APPLY "VALUE-CHANGED" TO TOG_ARBINA.
      RUN openbdynspec_UI IN brwproc[3]. 
      GET FIRST BRW_ARBART NO-LOCK.
      IF AVAILABLE arbarttemp THEN DO: 
         APPLY "HOME" TO BRW_ARBART.
         status-ok = BRW_ARBART:SELECT-FOCUSED-ROW() NO-ERROR.
         ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
      END.
      ELSE DO:
         MESSAGE "Det finns inget att göra något uraval på. Kontakta ansvarig!"
         VIEW-AS ALERT-BOX.
         musz = TRUE.
         LEAVE MAIN-BLOCK.
      END.  
   END.  
/*    IF listnr = 3 THEN DO:                             */
/*       ENABLE BRW_ARBART WITH FRAME {&FRAME-NAME}.     */
/*       BRW_ARBART:HIDDEN = FALSE.                      */
/*       APPLY "HOME" TO BRW_ARBART.                     */
/*       status-ok = BRW_ARBART:SELECT-FOCUSED-ROW().    */
/*       ASSIGN FILL-IN_ARBARTKOD = ARBETSART.ARBARTKOD. */
/*    END.                                               */
/*    IF listnr = 4 THEN DO:                        */
/*       ENABLE BRW_PRIO WITH FRAME {&FRAME-NAME}.  */
/*       BRW_PRIO:HIDDEN = FALSE.                   */
/*       APPLY "HOME" TO BRW_PRIO.                  */
/*       status-ok = BRW_PRIO:SELECT-FOCUSED-ROW(). */
/*       ASSIGN FILL-IN_PKOD = PRIORITET.PKOD.      */
/*    END.                                          */
/*    IF listnr = 5 THEN DO:                                                            */
/*       regdatum = DATE(01,01,YEAR(TODAY)).                                            */
/*       RUN REGVEC.P.                                                                  */
/*       IF SUBSTRING(STRING(regvnr),3,1) = "0" THEN regvnr = regvnr + 1.               */
/*       FILL-IN_STARTVNR = regvnr.                                                     */
/*       regdatum = TODAY.                                                              */
/*       RUN REGVEC.P.                                                                  */
/*       FILL-IN_SLUTVNR = regvnr.                                                      */
/*       ENABLE FILL-IN_SLUTVNR                                                         */
/*       FILL-IN_STARTVNR BTN_FVE BTN_FVE-2 BTN_NVE BTN_NVE-2 WITH FRAME {&FRAME-NAME}. */
/*       ASSIGN                                                                         */
/*       FILL-IN-HELP = "LÄMNA ETT FÄLT = 000 OM LISTAN SKALL"                          */
/*       FILL-IN-HELP-2 ="GÄLLA " + CAPS(Guru.Konstanter:gplk) + " FR.O.M RESP T.O.M VECKA".            */
/*       DISPLAY FILL-IN_SLUTVNR FILL-IN_STARTVNR FILL-IN-HELP FILL-IN-HELP-2           */
/*       WITH FRAME {&FRAME-NAME}.                                                      */
/*       ASSIGN                                                                         */
/*       FILL-IN_SLUTVNR:HIDDEN = FALSE                                                 */
/*       FILL-IN_STARTVNR:HIDDEN = FALSE.                                               */
/*    END.                                                                              */
   IF listnr = 6 THEN DO:
      ENABLE SEL_BESORG WITH FRAME {&FRAME-NAME}.
      SEL_BESORG:HIDDEN = FALSE.
   END.
/*    IF listnr = 7 THEN DO:                         */
/*       ENABLE BRW_BERED WITH FRAME {&FRAME-NAME}.  */
/*       BRW_BERED:HIDDEN = FALSE.                   */
/*       APPLY "HOME" TO BRW_BERED.                  */
/*       status-ok = BRW_BERED:SELECT-FOCUSED-ROW(). */
/*       FILL-IN_BEREDARE = PERSONALTAB.PERSONALKOD. */
/*    END.                                           */
   IF listnr = 8 THEN DO:
      ENABLE BRW_ARBAN WITH FRAME {&FRAME-NAME}.
      BRW_ARBAN:HIDDEN = FALSE.
      APPLY "HOME" TO BRW_ARBAN.
      status-ok = BRW_ARBAN:SELECT-FOCUSED-ROW().
      FILL-IN_ARBANSVARIG = ansvplantemp.PERSONALKOD.
   END. 
/*    IF listnr = 10 THEN DO:                             */
/*       TOG_ANL = FALSE.                                 */
/*       ENABLE BRW_ANL TOG_ANL WITH FRAME {&FRAME-NAME}. */
/*       ASSIGN                                           */
/*       BRW_ANL:HIDDEN = FALSE                           */
/*       TOG_ANL:HIDDEN = FALSE.                          */
/*       APPLY "HOME" TO BRW_ANL.                         */
/*       status-ok = BRW_ANL:SELECT-FOCUSED-ROW().        */
/*       FILL-IN_ANLNR = ANLAGGNING.ANLNR.                */
/*    END.                                                */
   FILL-IN_OMRADE:LABEL = Guru.Konstanter:gomrk. 
   {musarrow.i}
   {DIA_M_SLUT.I}
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
   arbarttemp.ARBARTKOD:READ-ONLY IN BROWSE BRW_ARBART = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ARBAN:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_ARBART:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN URVALAPP.P PERSISTENT SET urvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN URVALAPP.P PERSISTENT SET urvalapph.
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
  ENABLE BTN_KLAR BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

