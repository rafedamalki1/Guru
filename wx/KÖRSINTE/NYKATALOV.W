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

  Created: 95/09/15 -  2:57 pm

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW                    
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE NEW SHARED VARIABLE decivar AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE dew AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE valhjalp AS INTEGER NO-UNDO.





DEFINE SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE mall AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE flerfil AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO. 
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE pos2 AS INTEGER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{MTRLMALLTEMP.I}
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".
   
{TIDUTTTNEW.I}
DEFINE NEW SHARED TEMP-TABLE test_tab
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD NPRIS AS DECIMAL
   FIELD BPRIS AS DECIMAL. 
   
/*DEFINE QUERY mtrlq FOR MTRL.     */
/*{EGENBEN.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-37 RECT-38 RECT-39 RECT-40 RECT-41 ~
RECT-42 BTN_SKRIV BTN_TEST FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 ~
FILL-IN-BEN2 FILL-IN-NPRIS1 FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 ~
FILL-IN-BPRIS1 FILL-IN-BPRIS2 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 ~
FILL-IN-BEN2 FILL-IN-NPRIS1 FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 ~
FILL-IN-BPRIS1 FILL-IN-BPRIS2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TEST 
     LABEL "Test":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Ben. från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BPRIS1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Bruttopris från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BPRIS2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Enhet från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Enr från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Nettopris från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.13 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.13 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.13 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.88 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.88 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.88 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 108 BY 10.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.75 COL 1.5
     BTN_SKRIV AT ROW 3.08 COL 111.5
     BTN_TEST AT ROW 4.17 COL 111.5
     FILL-IN-ENR1 AT ROW 13.46 COL 23 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 13.46 COL 36.13 COLON-ALIGNED
     FILL-IN-BEN1 AT ROW 13.46 COL 56.5 COLON-ALIGNED
     FILL-IN-BEN2 AT ROW 13.46 COL 70.13 COLON-ALIGNED
     FILL-IN-NPRIS1 AT ROW 14.63 COL 23 COLON-ALIGNED
     FILL-IN-NPRIS2 AT ROW 14.63 COL 36.13 COLON-ALIGNED
     FILL-IN-ENHET1 AT ROW 14.63 COL 56.5 COLON-ALIGNED
     FILL-IN-ENHET2 AT ROW 14.63 COL 70.13 COLON-ALIGNED
     FILL-IN-BPRIS1 AT ROW 15.83 COL 23 COLON-ALIGNED
     FILL-IN-BPRIS2 AT ROW 15.83 COL 36.13 COLON-ALIGNED
     BTN_OK AT ROW 15.92 COL 96.5
     BTN_AVB AT ROW 15.92 COL 111.5
     RECT-37 AT ROW 13.25 COL 6.5
     RECT-38 AT ROW 14.46 COL 6.5
     RECT-39 AT ROW 15.67 COL 6.5
     RECT-40 AT ROW 13.25 COL 44.75
     RECT-41 AT ROW 14.46 COL 44.75
     RECT-42 AT ROW 15.67 COL 44.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 16.58.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 16.63
         WIDTH              = 125
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 125
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
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT RECT-42 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-VINST /* Avbryt */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-2
ON CHOOSE OF BTN_OK IN FRAME FRAME-VINST /* Ok */
DO:    
   {BORTBRWPROC.I}
   ASSIGN
   FILL-IN-ENR1 = INPUT FILL-IN-ENR1  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2 
   FILL-IN-BEN1 = INPUT FILL-IN-BEN1 
   FILL-IN-BEN2 = INPUT FILL-IN-BEN2
   FILL-IN-ENHET1 = INPUT FILL-IN-ENHET1 
   FILL-IN-ENHET2 = INPUT FILL-IN-ENHET2
   FILL-IN-BPRIS1 = INPUT FILL-IN-BPRIS1 
   FILL-IN-BPRIS2 = INPUT FILL-IN-BPRIS2
   FILL-IN-NPRIS1 = INPUT FILL-IN-NPRIS1 
   FILL-IN-NPRIS2 = INPUT FILL-IN-NPRIS2.
   MESSAGE "Klicka på Ja om det finns korrekta uppgifter att "
   "läsa in katalogen för " + levtemp.LEVNAMN VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO UPDATE svar.         
   IF svar THEN DO:
      IF INPUT FILL-IN-ENR1 = 0 OR INPUT FILL-IN-ENR2 = 0 OR
      INPUT FILL-IN-BEN1 = 0 OR INPUT FILL-IN-BEN2 = 0 OR
      INPUT FILL-IN-ENHET1 = 0 OR INPUT FILL-IN-ENHET2 = 0 THEN DO:
         MESSAGE "Det finns ett värde 0 för " + LC(Guru.Konstanter:genl) + ", benämning eller enhet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENR1.
      END.   
      ELSE IF (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1 > 11 THEN DO:
         MESSAGE Guru.Konstanter:genl + " får inte vara längre än 11 tecken." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENR2. 
      END.
      ELSE IF (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1 > 40 THEN DO: 
         MESSAGE "Benämning får inte vara längre än 40 tecken." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN2.
      END. 
      ELSE IF (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1 > 3 THEN DO:
         MESSAGE "Enheten får inte vara längre än 3 tecken." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENHET2.
      END.
      ELSE IF (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1 > 9 THEN DO: 
         MESSAGE "Nettopris får inte vara längre än 9 tecken." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-NPRIS1.
      END.  
      ELSE IF (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1 > 9 THEN DO: 
         MESSAGE "Bruttopris får inte vara längre än 9 tecken." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BPRIS1.      
      END. 
      ELSE IF (INPUT FILL-IN-BPRIS1 + INPUT FILL-IN-BPRIS2) = 0 AND 
      (INPUT FILL-IN-NPRIS1 + INPUT FILL-IN-NPRIS2) = 0 THEN DO: 
         MESSAGE "I måste ange antingen netto- eller bruttopris." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-NPRIS1.  
      END. 
      ELSE IF
         INPUT FILL-IN-ENR1 > INPUT FILL-IN-ENR2 OR
         INPUT FILL-IN-BEN1 > INPUT FILL-IN-BEN2 OR
         INPUT FILL-IN-ENHET1 > INPUT FILL-IN-ENHET2 OR
         INPUT FILL-IN-BPRIS1 > INPUT FILL-IN-BPRIS2 OR
         INPUT FILL-IN-NPRIS1 > INPUT FILL-IN-NPRIS2 THEN DO:
         MESSAGE "Det finns fält där värdet från: är större än värdet till:." VIEW-AS ALERT-BOX. 
         APPLY "ENTRY" TO FILL-IN-ENR1. 
      END.                                
      ELSE DO:
         IF dew = FALSE THEN DO:
            
            RUN DECIMAL.W.
            
         END.
         IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
            ASSIGN
            pos1 = INPUT FILL-IN-BPRIS1 
            pos2 = INPUT FILL-IN-BPRIS2.
         END.
         ELSE DO:          
            ASSIGN
            pos1 = INPUT FILL-IN-NPRIS1 
            pos2 = INPUT FILL-IN-NPRIS2.
         END.                         
         MESSAGE "Detta kan ta en stund." VIEW-AS ALERT-BOX.
         {muswait.i} 
         IF flerfil = FALSE THEN DO: 
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN DELMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT leverant).
            END.
            ELSE DO:
               RUN DELMTRL.P
               (INPUT leverant).
            END.                                    
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN CREMTRLV.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT decivar, INPUT pos2, INPUT leverant, INPUT FILL-IN-ENR1, 
            INPUT FILL-IN-ENR2, INPUT FILL-IN-BEN1, 
            INPUT FILL-IN-BEN2, INPUT FILL-IN-ENHET1, INPUT FILL-IN-ENHET2, 
            INPUT FILL-IN-BPRIS1, INPUT FILL-IN-BPRIS2, INPUT FILL-IN-NPRIS1, 
            INPUT FILL-IN-NPRIS2, INPUT TABLE tidin).
         END.
         ELSE DO:
            RUN CREMTRLV.P
            (INPUT decivar, INPUT pos2, INPUT leverant, INPUT FILL-IN-ENR1, 
            INPUT FILL-IN-ENR2, INPUT FILL-IN-BEN1, 
            INPUT FILL-IN-BEN2, INPUT FILL-IN-ENHET1, INPUT FILL-IN-ENHET2, 
            INPUT FILL-IN-BPRIS1, INPUT FILL-IN-BPRIS2, INPUT FILL-IN-NPRIS1, 
            INPUT FILL-IN-NPRIS2, INPUT TABLE tidin).
         END.                                                                         
         /* Flyttat till  CREMTRLV.P
         DO TRANSACTION:
            FIND FIRST MTRLMALL WHERE MTRLMALL.LEVKOD = leverant EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE MTRLMALL THEN DELETE MTRLMALL.
         END.
         DO TRANSACTION:
            CREATE MTRLMALL.
            ASSIGN
            MTRLMALL.LEVKOD = leverant
            MTRLMALL.ENR1 = INPUT FILL-IN-ENR1
            MTRLMALL.ENR2 = INPUT FILL-IN-ENR2 
            MTRLMALL.BEN1 = INPUT FILL-IN-BEN1
            MTRLMALL.BEN2 = INPUT FILL-IN-BEN2
            MTRLMALL.ENH1 = INPUT FILL-IN-ENHET1
            MTRLMALL.ENH2 = INPUT FILL-IN-ENHET2
            MTRLMALL.NPRIS1 = INPUT FILL-IN-NPRIS1
            MTRLMALL.NPRIS2 = INPUT FILL-IN-NPRIS2
            MTRLMALL.BPRIS1 = INPUT FILL-IN-BPRIS1
            MTRLMALL.BPRIS2 = INPUT FILL-IN-BPRIS2.
         END.*/
/*          MESSAGE "Vill Ni kopiera in extra mtrl från annan leverantör. T.ex. ELEF extra?" */
/*          VIEW-AS ALERT-BOX                                                                */
/*          QUESTION BUTTONS YES-NO UPDATE svar.                                             */
/*          IF svar THEN DO:                                                                 */
/*             {AVBGOM.I}                                                                    */
/*             RUN KOPIKATA.W.                                                               */
/*             {AVBFRAM.I}                                                                   */
/*          END.                                                                             */
         /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" 
 *          OR Guru.Konstanter:globforetag = "SOLE"  OR Guru.Konstanter:globforetag = "VATT"
 *          THEN DO: 
 *             ASSIGN
 *             skrivut = FALSE.
 *             MESSAGE "En lista kommer nu att skapas. Den visar artiklar som finns upplaggda i depå, men ej längre i aktuell leverantörs katalog."
 *             VIEW-AS ALERT-BOX.                                 
 *             {AVBGOM.I}
 *             RUN KATKOLL.W.
 *             {AVBFRAM.I}
 *          END.*/  
                      
         {musarrow.i}
         APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
     RUN ut_UI.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TEST WINDOW-2
ON CHOOSE OF BTN_TEST IN FRAME FRAME-VINST /* Test */
DO: 
   EMPTY TEMP-TABLE test_tab NO-ERROR.    
   FIND FIRST tidin NO-LOCK NO-ERROR.
   IF INPUT FILL-IN-ENR1 = 0 OR INPUT FILL-IN-ENR2 = 0 OR
      INPUT FILL-IN-BEN1 = 0 OR INPUT FILL-IN-BEN2 = 0 OR
      INPUT FILL-IN-ENHET1 = 0 OR INPUT FILL-IN-ENHET2 = 0 THEN DO:
      MESSAGE "Det finns ett värde 0 för " + LC(Guru.Konstanter:genl) + ", benämning eller enhet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR1.
   END.   
   ELSE IF (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1 > 11 THEN DO:
      MESSAGE Guru.Konstanter:genl + " får inte vara längre än 11 tecken." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR2. 
   END.
   ELSE IF (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1 > 40 THEN DO: 
      MESSAGE "Benämning får inte vara längre än 40 tecken." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN2.
   END. 
   ELSE IF (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1 > 3 THEN DO:
      MESSAGE "Enheten får inte vara längre än 3 tecken." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENHET2.
   END.
   ELSE IF (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1 > 9 THEN DO: 
      MESSAGE "Nettopris får inte vara längre än 9 tecken." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-NPRIS1.
   END.  
   ELSE IF (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1 > 9 THEN DO: 
      MESSAGE "Bruttopris får inte vara längre än 9 tecken." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BPRIS1.      
   END. 
   ELSE IF (INPUT FILL-IN-BPRIS1 + INPUT FILL-IN-BPRIS2) = 0 AND 
   (INPUT FILL-IN-NPRIS1 + INPUT FILL-IN-NPRIS2) = 0 THEN DO: 
      MESSAGE "Ni måste ange antingen netto- eller bruttopris." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-NPRIS1.  
   END. 
   ELSE IF
      INPUT FILL-IN-ENR1 > INPUT FILL-IN-ENR2 OR
      INPUT FILL-IN-BEN1 > INPUT FILL-IN-BEN2 OR
      INPUT FILL-IN-ENHET1 > INPUT FILL-IN-ENHET2 OR
      INPUT FILL-IN-BPRIS1 > INPUT FILL-IN-BPRIS2 OR
      INPUT FILL-IN-NPRIS1 > INPUT FILL-IN-NPRIS2 THEN DO:
      MESSAGE "Det finns fält där värdet från: är större än värdet till:." VIEW-AS ALERT-BOX. 
      APPLY "ENTRY" TO FILL-IN-ENR1. 
   END.                                
   ELSE DO: 
      
      RUN DECIMAL.W.
      
      IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
         ASSIGN
         pos1 = INPUT FILL-IN-BPRIS1 
         pos2 = INPUT FILL-IN-BPRIS2.
      END.
      ELSE DO:          
         ASSIGN
         pos1 = INPUT FILL-IN-NPRIS1 
         pos2 = INPUT FILL-IN-NPRIS2.
      END.                      
      CREATE test_tab.  
      ASSIGN
      test_tab.ENR = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENR1,
      (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1)
      test_tab.BENAMNING = SUBSTRING(tidin.TIN,INPUT FILL-IN-BEN1,
      (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1)
      test_tab.ENHET = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENHET1,
      (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1).       
      IF SUBSTRING(tidin.TIN,(pos2 - decivar),1) = "." THEN DO:
         IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
            test_tab.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)).
         END.
         ELSE DO:          
            test_tab.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
         END.
         IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
         END.
         ELSE DO:      
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)). 
         END.      
      END. 
      ELSE DO:  
         IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
            test_tab.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,decivar).
         END.
         ELSE DO:          
            test_tab.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,decivar).
         END.
         IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,decivar).
         END.
         ELSE DO:      
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,decivar). 
         END.
      END.      
      
      RUN TESTKATV.W.
      
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN1 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-BEN1 IN FRAME FRAME-VINST /* Ben. från */
DO:
   valhjalp = 2.
   RUN HJALP.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-BEN2 IN FRAME FRAME-VINST /* till */
DO:
   valhjalp = 2.
   
   RUN HJALP.W.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BPRIS1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BPRIS1 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-BPRIS1 IN FRAME FRAME-VINST /* Bruttopris från */
DO:
   valhjalp = 4.
   
   RUN HJALP.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BPRIS2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BPRIS2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-BPRIS2 IN FRAME FRAME-VINST /* till */
DO:
   valhjalp = 4.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENHET1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENHET1 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-ENHET1 IN FRAME FRAME-VINST /* Enhet från */
DO:
   valhjalp = 3.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENHET2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENHET2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-ENHET2 IN FRAME FRAME-VINST /* till */
DO:
   valhjalp = 3.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR1 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-ENR1 IN FRAME FRAME-VINST /* Enr från */
DO: 
   valhjalp = 1.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-ENR2 IN FRAME FRAME-VINST /* till */
DO:
   valhjalp = 1.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NPRIS1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NPRIS1 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-NPRIS1 IN FRAME FRAME-VINST /* Nettopris från */
DO:
   valhjalp = 4.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NPRIS2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NPRIS2 WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-NPRIS2 IN FRAME FRAME-VINST /* till */
DO:
   valhjalp = 4.

   RUN HJALP.W.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   dew = FALSE. 
   decivar = 0.  
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   
   /*wtidvar och dlcvar  sätts i  */
   kommando = guruvar + "priskat.q".  
   ASSIGN
   kommando2 = SEARCH("quoter.exe").
   /*kommando2 = dlcvar + "QUOTER.EXE".*/
   OS-COMMAND SILENT VALUE(kommando2)
   VALUE(filnamn) > VALUE(kommando).   
   {AMERICANEUROPEAN.I}
   INPUT FROM VALUE(kommando) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.
      CREATE tidin.
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE. 
   {EUROPEANAMERICAN.I}
   /*OS-DELETE VALUE(kommando).*/
   FIND FIRST levtemp WHERE levtemp.LEVKOD = leverant NO-LOCK NO-ERROR.
   ASSIGN WINDOW-2:TITLE = "Inläggning av ny katalog för - " + levtemp.LEVNAMN.
   IF mall = TRUE THEN DO:
      FIND FIRST mtrlmalltemp WHERE mtrlmalltemp.LEVKOD = leverant NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-ENR1:LABEL = Guru.Konstanter:genk + " från"
      FILL-IN-ENR1 = mtrlmalltemp.ENR1 
      FILL-IN-ENR2 = mtrlmalltemp.ENR2 
      FILL-IN-BEN1 = mtrlmalltemp.BEN1 
      FILL-IN-BEN2 = mtrlmalltemp.BEN2 
      FILL-IN-ENHET1 = mtrlmalltemp.ENH1 
      FILL-IN-ENHET2 = mtrlmalltemp.ENH2 
      FILL-IN-NPRIS1 = mtrlmalltemp.NPRIS1 
      FILL-IN-NPRIS2 = mtrlmalltemp.NPRIS2 
      FILL-IN-BPRIS1 = mtrlmalltemp.BPRIS1 
      FILL-IN-BPRIS2 = mtrlmalltemp.BPRIS2.
   END.
   ELSE DO:
      mall = mall.
   END.          
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   str=
"=======================================================================================".      
   RUN huvud_UI.   
   IF musz = FALSE THEN RUN rubrik_UI.  
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      ELSE DO:   
         RUN ut_UI.
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}          
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
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
  DISPLAY FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 FILL-IN-BEN2 FILL-IN-NPRIS1 
          FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 FILL-IN-BPRIS1 
          FILL-IN-BPRIS2 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE RECT-37 RECT-38 RECT-39 RECT-40 RECT-41 RECT-42 BTN_SKRIV BTN_TEST 
         FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 FILL-IN-BEN2 FILL-IN-NPRIS1 
         FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 FILL-IN-BPRIS1 
         FILL-IN-BPRIS2 BTN_OK BTN_AVB 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/   
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                            
         str = "1        10        20        30        40        50        60        70".
         CREATE tidut. 
         ASSIGN        
         SUBSTRING(tidut.UT,1) = str. 
   str = ".========.=========.=========.=========.=========.=========.=========.==========".      
         CREATE tidut. 
         SUBSTRING(tidut.UT,1) = str.                                                                 
      END.
   END.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-2 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   FIND FIRST tidin NO-LOCK NO-ERROR.   
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = SUBSTRING(tidin.TIN,1,80). 
      CREATE tidut.       
      str = "81       90        100       110       120       130       140       150".           
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = str. 
      CREATE tidut.
      str = ".========.=========.=========.=========.=========.=========.=========.==========".      
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = SUBSTRING(tidin.TIN,81,160).
      CREATE tidut.       
      str = "161      170       180       190       200       210       220       230".           
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = str. 
      CREATE tidut.
      str = ".========.=========.=========.=========.=========.=========.=========.==========".      
      SUBSTRING(tidut.UT,1) = str.
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = SUBSTRING(tidin.TIN,161,240).
                                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

