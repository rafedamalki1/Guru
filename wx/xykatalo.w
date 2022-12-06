&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-2
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
&Scoped-define NEW                    
DEFINE NEW SHARED VARIABLE deci AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE dew AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.                               
DEFINE NEW SHARED VARIABLE valhjalp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE SHARED VARIABLE mall AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE flerfil AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO. 
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE pos2 AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".
   
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".

DEFINE NEW SHARED TEMP-TABLE test_tab
   FIELD ENR LIKE MTRL.ENR
   FIELD BENAMNING LIKE MTRL.BENAMNING
   FIELD ENHET LIKE MTRL.ENHET
   FIELD NPRIS LIKE MTRL.NPRIS
   FIELD BPRIS LIKE MTRL.BPRIS. 
   
DEFINE QUERY mtrlq FOR MTRL.     

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut 

/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_TEST 
     LABEL "Test":L 
     SIZE 12 BY 1.5.

DEFINE VARIABLE FILL-IN-BEN1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Ben. från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-BPRIS1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Bruttopris från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-BPRIS2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Enhet från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Enr från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Nettopris från" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE VARIABLE FILL-IN-NPRIS2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 4.38 BY .86 NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 83.13 BY 2.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 83.13 BY 10.91
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 38.13 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 38.13 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 38.13 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44.88 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44.88 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44.88 BY 2.14
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BRW_UT FOR tidut SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT QUERY BRW_UT NO-LOCK DISPLAY 
      tidut.ut
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 8.59.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BTN_OK AT ROW 1.45 COL 10.5
     BTN_TEST AT ROW 1.45 COL 28
     BTN_AVS AT ROW 1.45 COL 45.5
     BTN_SKRIV AT ROW 1.45 COL 63
     BRW_UT AT ROW 4.5 COL 2
     FILL-IN-ENR1 AT ROW 15 COL 17.5 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 15 COL 30.63 COLON-ALIGNED
     FILL-IN-BEN1 AT ROW 15 COL 51 COLON-ALIGNED
     FILL-IN-BEN2 AT ROW 15 COL 64.63 COLON-ALIGNED
     FILL-IN-NPRIS1 AT ROW 17.09 COL 17.5 COLON-ALIGNED
     FILL-IN-NPRIS2 AT ROW 17.09 COL 30.63 COLON-ALIGNED
     FILL-IN-ENHET1 AT ROW 17.09 COL 51 COLON-ALIGNED
     FILL-IN-ENHET2 AT ROW 17.09 COL 64.63 COLON-ALIGNED
     FILL-IN-BPRIS1 AT ROW 19.14 COL 17.5 COLON-ALIGNED
     FILL-IN-BPRIS2 AT ROW 19.14 COL 30.63 COLON-ALIGNED
     RECT-35 AT ROW 1 COL 1
     RECT-36 AT ROW 3.5 COL 1
     RECT-37 AT ROW 14.41 COL 1
     RECT-40 AT ROW 14.41 COL 39.25
     RECT-38 AT ROW 16.55 COL 1
     RECT-41 AT ROW 16.55 COL 39.25
     RECT-39 AT ROW 18.68 COL 1
     RECT-42 AT ROW 18.68 COL 39.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.41
         SIZE 83.5 BY 19.95
         BGCOLOR 8 .

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         COLUMN             = 9.13
         ROW                = 2.77
         HEIGHT             = 20.41
         WIDTH              = 83.5
         MAX-HEIGHT         = 25.14
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.14
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST            = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "rt.tidut"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = rt.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _TblList          = ""
     _Options          = "NO-LOCK KEEP-EMPTY"
     _OrdList          = ""
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-2
ON CHOOSE OF BTN_OK IN FRAME FRAME-VINST /* Ok */
DO:    
   MESSAGE "Klicka på Ja om det finns korrekta uppgifter att "
   "läsa in katalogen för " + LEVERANTOR.LEVNAMN VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      IF INPUT FILL-IN-ENR1 = 0 OR INPUT FILL-IN-ENR2 = 0 OR
      INPUT FILL-IN-BEN1 = 0 OR INPUT FILL-IN-BEN2 = 0 OR
      INPUT FILL-IN-ENHET1 = 0 OR INPUT FILL-IN-ENHET2 = 0 THEN DO:
         MESSAGE "DET FINNS ETT VÄRDE 0 FÖR ENR, BENÄMNING ELLER ENHET." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENR1.
      END.   
      ELSE IF (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1 > 11 THEN DO:
         MESSAGE "ENR FÅR INTE VARA LÄNGRE ÄN 11 TECKEN" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENR2. 
      END.
      ELSE IF (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1 > 40 THEN DO: 
         MESSAGE "BENÄMNING FÅR INTE VARA LÄNGRE ÄN 40 TECKEN" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN2.
      END. 
      ELSE IF (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1 > 3 THEN DO: 
         MESSAGE "ENHETEN FÅR INTE VARA LÄNGRE ÄN 3 TECKEN" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-ENHET2.
      END. 
      ELSE IF (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1 > 9 THEN DO: 
         MESSAGE "NETTOPRIS FÅR INTE VARA LÄNGRE ÄN 9 TECKEN" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-NPRIS1.
      END.  
      ELSE IF (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1 > 9 THEN DO: 
         MESSAGE "BRUTTOPRIS FÅR INTE VARA LÄNGRE ÄN 9 TECKEN" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BPRIS1.      
      END. 
      ELSE IF (INPUT FILL-IN-BPRIS1 + INPUT FILL-IN-BPRIS2) = 0 AND 
      (INPUT FILL-IN-NPRIS1 + INPUT FILL-IN-NPRIS2) = 0 THEN DO: 
         MESSAGE "NI MÅSTE ANGE ANTINGEN NETTO- ELLER BRUTTOPRIS." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-NPRIS1.  
      END. 
      ELSE IF
         INPUT FILL-IN-ENR1 > INPUT FILL-IN-ENR2 OR
         INPUT FILL-IN-BEN1 > INPUT FILL-IN-BEN2 OR
         INPUT FILL-IN-ENHET1 > INPUT FILL-IN-ENHET2 OR
         INPUT FILL-IN-BPRIS1 > INPUT FILL-IN-BPRIS2 OR
         INPUT FILL-IN-NPRIS1 > INPUT FILL-IN-NPRIS2 THEN DO:
         MESSAGE "DET FINNS FÄLT DÄR VÄRDET FRÅN: ÄR STÖRRE ÄN VÄRDET TILL:." VIEW-AS ALERT-BOX. 
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
         MESSAGE "DETTA KAN TA EN STUND." VIEW-AS ALERT-BOX.
         {muswait.i} 
         IF flerfil = FALSE THEN DO:                         
            OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
            MTRL.LEVKOD = leverant USE-INDEX LEV NO-LOCK.
            DO TRANSACTION:
               GET FIRST mtrlq EXCLUSIVE-LOCK.
               IF AVAILABLE MTRL THEN DO:
                  DELETE MTRL.
                  GET NEXT mtrlq EXCLUSIVE-LOCK.
               END.  
            END. 
            DO TRANSACTION:  
               DO WHILE AVAILABLE(MTRL):
                  DELETE MTRL.
                  GET NEXT mtrlq EXCLUSIVE-LOCK.
               END.
            END.                  
            CLOSE QUERY mtrlq.
         END.
         FIND FIRST tidin NO-LOCK NO-ERROR.
         IF AVAILABLE tidin THEN DO TRANSACTION: 
            CREATE MTRL.  
            ASSIGN 
            MTRL.LEVKOD = leverant
            MTRL.ENR = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENR1,
            (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1)
            MTRL.BENAMNING = SUBSTRING(tidin.TIN,INPUT FILL-IN-BEN1,
            (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1)
            MTRL.ENHET = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENHET1,
            (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1).                                                                         
            IF SUBSTRING(tidin.TIN,(pos2 - deci),1) = "." THEN DO:
               IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
                  MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                 (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)).
               END.
               ELSE DO:          
                  MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                 (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
               END.
               IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
                  MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                  (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
               END.
               ELSE DO:      
                  MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                  (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)). 
               END.      
            END. 
            ELSE DO:  
               IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
                  MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                  (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci).
               END.
               ELSE DO:          
                  MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                  (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
               END.
               IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
                  MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                  (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
               END.
               ELSE DO:      
                  MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                  (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci). 
               END.
            END.                                                   
         END.
         REPEAT: 
            FIND NEXT tidin NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidin THEN LEAVE.
            ELSE DO TRANSACTION:
               CREATE MTRL.  
               ASSIGN 
               MTRL.LEVKOD = leverant
               MTRL.ENR = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENR1,
               (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1)
               MTRL.BENAMNING = SUBSTRING(tidin.TIN,INPUT FILL-IN-BEN1,
               (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1)
               MTRL.ENHET = SUBSTRING(tidin.TIN,INPUT FILL-IN-ENHET1,
               (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1).                                                                         
               IF SUBSTRING(tidin.TIN,(pos2 - deci),1) = "." THEN DO:
                  IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
                     MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                     (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)).
                  END.
                  ELSE DO:          
                     MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                     (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
                  END.
                  IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
                     MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                     (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)).
                  END.
                  ELSE DO:      
                     MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                     (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)). 
                  END.      
               END. 
               ELSE DO:  
                  IF INPUT FILL-IN-NPRIS1 = 0 THEN DO: 
                     MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                     (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci).
                  END.
                  ELSE DO:          
                     MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                     (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
                  END.
                  IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
                     MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
                     (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
                  END.
                  ELSE DO:      
                     MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
                     (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci). 
                  END.
               END.                                                   
            END. 
         END.                                                                        
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
         END.
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO: 
            ASSIGN
            skrivut = FALSE.
            MESSAGE "En lista kommer nu att skapas. Den visar artiklar som finns upplaggda i depå, men ej längre i aktuell leverantörs katalog."
            VIEW-AS ALERT-BOX.                                 
            RUN KATKOLL.W.
         END.                        
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
   RUN SKRIVVAL.W.       
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
   FOR EACH test_tab.
      DELETE test_tab.
   END.   
   FIND FIRST tidin NO-LOCK NO-ERROR.
   IF INPUT FILL-IN-ENR1 = 0 OR INPUT FILL-IN-ENR2 = 0 OR
      INPUT FILL-IN-BEN1 = 0 OR INPUT FILL-IN-BEN2 = 0 OR
      INPUT FILL-IN-ENHET1 = 0 OR INPUT FILL-IN-ENHET2 = 0 THEN DO:
      MESSAGE "DET FINNS ETT VÄRDE 0 FÖR ENR, BENÄMNING ELLER ENHET." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR1.
   END.   
   ELSE IF (INPUT FILL-IN-ENR2 - INPUT FILL-IN-ENR1) + 1 > 11 THEN DO:
      MESSAGE "ENR FÅR INTE VARA LÄNGRE ÄN 11 TECKEN" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR2. 
   END.
   ELSE IF (INPUT FILL-IN-BEN2 - INPUT FILL-IN-BEN1) + 1 > 40 THEN DO: 
      MESSAGE "BENÄMNING FÅR INTE VARA LÄNGRE ÄN 40 TECKEN" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN2.
   END. 
   ELSE IF (INPUT FILL-IN-ENHET2 - INPUT FILL-IN-ENHET1) + 1 > 3 THEN DO: 
      MESSAGE "ENHETEN FÅR INTE VARA LÄNGRE ÄN 3 TECKEN" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENHET2.
   END. 
   ELSE IF (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1 > 9 THEN DO: 
      MESSAGE "NETTOPRIS FÅR INTE VARA LÄNGRE ÄN 9 TECKEN" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-NPRIS1.
   END.  
   ELSE IF (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1 > 9 THEN DO: 
      MESSAGE "BRUTTOPRIS FÅR INTE VARA LÄNGRE ÄN 9 TECKEN" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BPRIS1.      
   END. 
   ELSE IF (INPUT FILL-IN-BPRIS1 + INPUT FILL-IN-BPRIS2) = 0 AND 
   (INPUT FILL-IN-NPRIS1 + INPUT FILL-IN-NPRIS2) = 0 THEN DO: 
      MESSAGE "NI MÅSTE ANGE ANTINGEN NETTO- ELLER BRUTTOPRIS." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-NPRIS1.  
   END. 
   ELSE IF
      INPUT FILL-IN-ENR1 > INPUT FILL-IN-ENR2 OR
      INPUT FILL-IN-BEN1 > INPUT FILL-IN-BEN2 OR
      INPUT FILL-IN-ENHET1 > INPUT FILL-IN-ENHET2 OR
      INPUT FILL-IN-BPRIS1 > INPUT FILL-IN-BPRIS2 OR
      INPUT FILL-IN-NPRIS1 > INPUT FILL-IN-NPRIS2 THEN DO:
      MESSAGE "DET FINNS FÄLT DÄR VÄRDET FRÅN: ÄR STÖRRE ÄN VÄRDET TILL:." VIEW-AS ALERT-BOX. 
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
      IF SUBSTRING(tidin.TIN,(pos2 - deci),1) = "." THEN DO:
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
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci).
         END.
         ELSE DO:          
            test_tab.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
         END.
         IF INPUT FILL-IN-BPRIS1 = 0 THEN DO:
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-NPRIS1,
            (INPUT FILL-IN-NPRIS2 - INPUT FILL-IN-NPRIS1) + 1)) / EXP(10,deci).
         END.
         ELSE DO:      
            test_tab.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,INPUT FILL-IN-BPRIS1,
            (INPUT FILL-IN-BPRIS2 - INPUT FILL-IN-BPRIS1) + 1)) / EXP(10,deci). 
         END.
      END.      
      RUN TESTKAT.W.   
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
   {muswait.i} 
   ASSIGN
   dew = FALSE. 
   deci = 0.
   FOR EACH tidin:
      DELETE tidin.
   END.     
   DOS SILENT quoter VALUE(filnamn) > PRISKAT.Q.                    
   INPUT FROM PRISKAT.Q NO-ECHO
   CONVERT TARGET "iso8859-1".
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.
      CREATE tidin.
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE. 
   FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = leverant NO-LOCK NO-ERROR.
   ASSIGN WINDOW-2:TITLE = "Inläggning av ny katalog för - " + LEVERANTOR.LEVNAMN.
   IF mall = TRUE THEN DO:
      FIND FIRST MTRLMALL WHERE MTRLMALL.LEVKOD = leverant NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-ENR1 = MTRLMALL.ENR1 
      FILL-IN-ENR2 = MTRLMALL.ENR2 
      FILL-IN-BEN1 = MTRLMALL.BEN1 
      FILL-IN-BEN2 = MTRLMALL.BEN2 
      FILL-IN-ENHET1 = MTRLMALL.ENH1 
      FILL-IN-ENHET2 = MTRLMALL.ENH2 
      FILL-IN-NPRIS1 = MTRLMALL.NPRIS1 
      FILL-IN-NPRIS2 = MTRLMALL.NPRIS2 
      FILL-IN-BPRIS1 = MTRLMALL.BPRIS1 
      FILL-IN-BPRIS2 = MTRLMALL.BPRIS2.
   END.
   ELSE DO:
      mall = mall.
   END.               
   FOR EACH tidut:
      DELETE tidut.
   END.  
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
   {musarrow.i}                                                
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 FILL-IN-BEN2 FILL-IN-NPRIS1 
          FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 FILL-IN-BPRIS1 
          FILL-IN-BPRIS2 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE RECT-35 BTN_OK BTN_TEST BTN_AVS BTN_SKRIV RECT-36 RECT-37 RECT-40 
         FILL-IN-ENR1 FILL-IN-ENR2 FILL-IN-BEN1 FILL-IN-BEN2 RECT-38 RECT-41 
         FILL-IN-NPRIS1 FILL-IN-NPRIS2 FILL-IN-ENHET1 FILL-IN-ENHET2 RECT-39 
         RECT-42 FILL-IN-BPRIS1 FILL-IN-BPRIS2 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
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


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
