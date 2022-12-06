&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WWSTART 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/06 - 10:23 am

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
DEFINE INPUT-OUTPUT PARAMETER nyprog AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
nyprog = FALSE.
&Scoped-define NEW NEW
{SPECMTRLG1.I}       
{FORETEMP.I}
{GLOBVAR2DEL1.I}
/*{EGENBVAR.I}*/
{OMRTEMPW.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{LOPTEMP.I}
{BESTKUNDALLT.I}
{SEKVARN.I}
{FAKTTYPDEF.I}
{SOKMTRL.I}
{LEVTEMP.I}
{SOKDEF.I}



                              

{PROVAG.I}
DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkregvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_FAST AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE TOG_AONR AS LOGICAL INITIAL no NO-UNDO.
DEFINE NEW SHARED VARIABLE finnskoff AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_kundlev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE NEW SHARED VARIABLE vald_lev AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE NEW SHARED VARIABLE best AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE huvudlev AS CHARACTER FORMAT "X(25)" NO-UNDO.  


DEFINE NEW SHARED VARIABLE alltidmax AS LOGICAL NO-UNDO.
DEFINE VARIABLE gastsek AS LOGICAL EXTENT 20 NO-UNDO.
DEFINE VARIABLE storkollbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE storkollhojd AS INTEGER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE ph_frame AS HANDLE NO-UNDO.
   ASSIGN
   globstorh = 682
   globstorb = 1000.
DEFINE VARIABLE raknarsek AS INTEGER NO-UNDO.
DEFINE VARIABLE anvdator AS CHARACTER NO-UNDO.  
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.            
DEFINE VARIABLE p-handle AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE globanvnt AS CHARACTER NO-UNDO. 
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".




DEFINE TEMP-TABLE tesxttemp
   FIELD EGENTEXT     AS CHARACTER 
   FIELD EGENTEXTFULL AS CHARACTER
   FIELD GURUTEXT     AS CHARACTER
   FIELD PROGRAM      AS CHARACTER
   INDEX PROGRAM IS PRIMARY PROGRAM.

IF Guru.Konstanter:appcon THEN DO:
   RUN EGENBENA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (OUTPUT TABLE tesxttemp, OUTPUT TABLE foretemp).
END.
ELSE DO:
   RUN EGENBENA.P (OUTPUT TABLE tesxttemp,OUTPUT TABLE foretemp).
END.
FIND FIRST foretemp NO-ERROR.
ASSIGN
globforetag = foretemp.FORETAG
plusdval = foretemp.PLUSD.
{FORESTYR.I}

RUN EGENSTART.P (INPUT TABLE tesxttemp).
{FAKTTYPSKAP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 EDD_FUNK SEL_UPP BTN_KOR ~
BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS EDD_FUNK SEL_UPP FILL-IN-GURU FILL-IN-FUNK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WWSTART AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt användare" 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_BYTW 
     LABEL "Byt fönsterstorlek" 
     SIZE 22 BY 2 TOOLTIP "Anpassa Guru till din skärmupplösning.".

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 22 BY 2
     BGCOLOR 8 .

DEFINE BUTTON BTN_MEDD 
     LABEL "Skapa meddelande till Guruanvändare" 
     SIZE 22 BY 2.

DEFINE BUTTON BTN_UPPDAT 
     LABEL "Uppdatera program" 
     SIZE 22 BY 2 TOOLTIP "Uppdatera din Guru applikation med den senaste versionen.".

DEFINE VARIABLE EDD_FUNK AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 64.5 BY 7
     BGCOLOR 15 FGCOLOR 9 FONT 5 NO-UNDO.

DEFINE VARIABLE FILL-IN-FUNK AS CHARACTER FORMAT "X(256)":U INITIAL "Funktioner:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Välkommen till" 
      VIEW-AS TEXT 
     SIZE 50.5 BY 1
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "adeicon/blank":U CONVERT-3D-COLORS
     SIZE 8 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 42.5 BY 19.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 24.5 BY 19.25
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 38 BY 16.5
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     EDD_FUNK AT ROW 2.5 COL 2 NO-LABEL
     SEL_UPP AT ROW 3.25 COL 3.5 NO-LABEL
     BTN_KOR AT ROW 4.58 COL 44.88
     BTN_MEDD AT ROW 7.21 COL 44.88
     BTN_BYT AT ROW 9.83 COL 44.88
     BTN_BYTW AT ROW 12.46 COL 44.88
     BTN_UPPDAT AT ROW 15.08 COL 44.88
     BTN_AVB AT ROW 17.75 COL 44.88
     FILL-IN-GURU AT ROW 1 COL 3 NO-LABEL
     FILL-IN-FUNK AT ROW 2.5 COL 45.5 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2 COL 1
     RECT-2 AT ROW 2 COL 43.5
     IMAGE-3 AT ROW 1 COL 56.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.25 BY 20.38
         BGCOLOR 8 
         DEFAULT-BUTTON BTN_KOR.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WWSTART ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU"
         COLUMN             = 20.25
         ROW                = 6.63
         HEIGHT             = 20.46
         WIDTH              = 67.38
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 16
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WWSTART
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
ASSIGN 
       BTN_UPPDAT:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       EDD_FUNK:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FUNK IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-FUNK:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       IMAGE-3:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
THEN WWSTART:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-B
/* Query rebuild information for FRAME FRAME-B
     _Query            is NOT OPENED
*/  /* FRAME FRAME-B */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WWSTART WWSTART
ON WINDOW-CLOSE OF WWSTART /* GURU */
DO:   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WWSTART
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:       
   DEFINE VARIABLE franfil  AS CHARACTER NO-UNDO.
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF Guru.Konstanter:globanv = "ELPAO" AND Guru.Konstanter:apphand NE ? THEN DO:
      Guru.Konstanter:appcon = TRUE.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT FALSE).
   END. 
   IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
      RUN feldum_UI NO-ERROR.
   END.
   IF PROGRESS = "FULL" THEN DEFAULT-WINDOW:HIDDEN = FALSE.
   franfil = "DEL " + SESSION:TEMP-DIR + "*.TXT". 
   /*OS-DELETE VALUE(franfil).*/
   OS-COMMAND SILENT VALUE(franfil).
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WWSTART
ON ENDKEY OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:  
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF Guru.Konstanter:globanv = "ELPAO" AND Guru.Konstanter:apphand NE ? THEN DO:
      Guru.Konstanter:appcon = TRUE.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT FALSE).
   END.
   IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
      RUN feldum_UI NO-ERROR.
   END.
   IF PROGRESS = "FULL" THEN DEFAULT-WINDOW:HIDDEN = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WWSTART
ON CHOOSE OF BTN_BYT IN FRAME FRAME-B /* Byt användare */
DO:
   IF varforetypval[17] = 1 THEN DO:
      RUN gastmedd_UI.      
      RETURN NO-APPLY.      
   END.
   {AVBGOM.I}
   musz = FALSE.
   RUN GURU.W.
   {AVBFRAM.I}
   IF Guru.Konstanter:globanv = "ALLMÄN" THEN globallm = TRUE.
   ELSE IF Guru.Konstanter:globanv = "ALLMÄN2" THEN globallm = TRUE.
   ELSE globallm = FALSE.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
     (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
      OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
      OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
   END.
   ELSE DO:
      RUN LOSENKOLL.P 
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
      OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
      OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
   END.
   IF storkollbredd > globstorb THEN globstorb = storkollbredd.
   IF storkollhojd  > globstorh THEN globstorh = storkollhojd.
   IF musz = TRUE THEN DO:
      APPLY "CLOSE" TO THIS-PROCEDURE.              
   END.  
   ELSE DO:
      IF Guru.Konstanter:globanv = "ELPAO" THEN DO:
         foretemp.GRAFTECK = TRUE.
      END.
      ELSE foretemp.GRAFTECK = FALSE.
      RUN insatt_UI.                  
      RUN sek_UI.
      {AVBGOM.I}   
      RUN VISMEDD.W.
      {AVBFRAM.I}      
      {musarrow.i}
      IF Guru.Konstanter:globanv = "FLEX" THEN DO:
         vartpro = "FLX".
         APPLY "CHOOSE" TO BTN_KOR. 
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYTW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTW WWSTART
ON CHOOSE OF BTN_BYTW IN FRAME FRAME-B /* Byt fönsterstorlek */
DO:
   IF varforetypval[17] >= 1 THEN DO:
      RUN gastmedd_UI.      
      RETURN NO-APPLY.          
   END.
  {AVBGOM.I}
  RUN GURUFONSTER.W.
  {AVBFRAM.I}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WWSTART
ON CHOOSE OF BTN_KOR IN FRAME FRAME-B /* Kör funktion */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MEDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MEDD WWSTART
ON CHOOSE OF BTN_MEDD IN FRAME FRAME-B /* Skapa meddelande till Guruanvändare */
DO:  
   IF varforetypval[17] >= 1 THEN DO:
      RUN gastmedd_UI.      
      RETURN NO-APPLY.      
   END.
   vartpro = "MED".
   {AVBGOM.I}
   RUN MEDDREG.W.
   {AVBFRAM.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPDAT WWSTART
ON CHOOSE OF BTN_UPPDAT IN FRAME FRAME-B /* Uppdatera program */
DO:
  IF varforetypval[17] >= 1 THEN DO:
     RUN gastmedd_UI.      
      RETURN NO-APPLY.      
  END.
  nyprog = TRUE.
  {AVBGOM.I}
  RUN BLOBUPP.W.
  {AVBFRAM.I}
  APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON MOUSE-MENU-CLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   /*
   IF SEL_UPP = "Kalkylering" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA"   OR 
      Guru.Konstanter:globforetag = "GRIT"  OR
      Guru.Konstanter:globforetag = "GADM" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         {AVBGOM.I}
         RUN EARES.W.
         {AVBFRAM.I}
      END.
   END.   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.   
   {WSTART2.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WWSTART 


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
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF Guru.Konstanter:globanv = "ELPAO" AND Guru.Konstanter:apphand NE ? THEN DO:
      Guru.Konstanter:appcon = TRUE.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT FALSE).
   END.
   IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
      RUN feldum_UI NO-ERROR.
   END.
   IF PROGRESS = "FULL" THEN DEFAULT-WINDOW:HIDDEN = FALSE.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.  
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  
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
   {WSTART1.I}
   {STARTWIN.I}
   Guru.GlobalaVariabler:retvalkoll =  TRUE.    
   vartpro = "".
   IF foretemp.VERSION = " " THEN DO TRANSACTION:
      FIND CURRENT foretemp EXCLUSIVE-LOCK NO-ERROR.
      foretemp.VERSION = "GURU !".
   END.
   IF Guru.Konstanter:appfel = FALSE THEN DO: 
      RUN INLOAPI.P (OUTPUT anvdator).
      ASSIGN
      globanvnt = TRIM(SUBSTRING(anvdator,1,20))
      Guru.Konstanter:globanv = TRIM(SUBSTRING(anvdator,1,20)) 
      datornamn = TRIM(SUBSTRING(anvdator,25,20)).
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         Guru.Konstanter:globanv = "ELPAO".
         globlos = "KAGGEN".
      END.
      ELSE DO:
         /*AUTOLOGIN*/         
         IF Guru.Konstanter:appcon THEN DO:
            RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
             OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
             OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
         END.
         ELSE DO:
            RUN LOSENKOLL.P 
            (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
             OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
             OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
         END.
         IF storkollbredd > globstorb THEN globstorb = storkollbredd.
         IF storkollhojd  > globstorh THEN globstorh = storkollhojd.
         /*EJ AUTOINLOGG*/
         IF Guru.Konstanter:globforetag = "BIRK" OR Guru.Konstanter:globforetag = "elpa" THEN musz = TRUE.
         IF musz = TRUE THEN DO:
            musz = FALSE.
           /* {&WINDOW-NAME}:HIDDEN = FALSE.                       */
            RUN GURU.W.  
           /* {&WINDOW-NAME}:HIDDEN = TRUE.       */                
            IF musz = TRUE THEN musz = musz.
            ELSE DO:
               IF Guru.Konstanter:appcon THEN DO:
                  IF Guru.Konstanter:globanv = "elpao" THEN Guru.Konstanter:globanv = Guru.Konstanter:globanv.
                  ELSE DO:
                     /*VISAR NT-LOGG OCH GURULOGG*/
                     RUN INLFEL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                     (INPUT Guru.Konstanter:globanvnt,INPUT Guru.Konstanter:globanv).
                  END.                  
               END.
            END.            
         END.         
         IF musz = TRUE THEN DO:
            APPLY "CLOSE" TO THIS-PROCEDURE. 
            LEAVE MAIN-BLOCK.    
         END.       
      END.      
   END.
   ELSE DO:
      Guru.Konstanter:appfel = FALSE.
      Guru.Konstanter:globanv = "FLEX".
      globlos = "FLEX".
   END.        
   IF Guru.Konstanter:globanv = "ALLMÄN" THEN globallm = TRUE.
   ELSE IF Guru.Konstanter:globanv = "ALLMÄN2" THEN globallm = TRUE.
   ELSE globallm = FALSE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,
       OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).    
   END.
   ELSE DO:
      RUN LOSENKOLL.P 
      (INPUT 1,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz).
   END.
   IF storkollbredd > globstorb THEN globstorb = storkollbredd.
   IF storkollhojd  > globstorh THEN globstorh = storkollhojd.
   IF musz = TRUE THEN DO:
      APPLY "CLOSE" TO THIS-PROCEDURE. 
      LEAVE MAIN-BLOCK.    
   END.       
   /*RÄKNAR ANTAL ANVÄNDARE*/   
   IF Guru.Konstanter:appcon THEN DO:
      anvdator = "".
      ASSIGN
      SUBSTRING(anvdator,1,20) = Guru.Konstanter:globanv
      SUBSTRING(anvdator,25,20) = datornamn
      SUBSTRING(anvdator,50,20) = Guru.Konstanter:globanvnt.
      RUN INLOGRAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT anvdator,INPUT TRUE).
   END.        
   
   RUN insatt_UI.
   
   IF PROGRESS = "FULL" AND Guru.Konstanter:globanv = "ELPAO" THEN DO:
      SESSION:DEBUG-ALERT = YES.
   END.
   IF musz = FALSE THEN RUN enable_UI.         
   RUN sek_UI.
   IF varforetypval[17] >= 1 THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 88.
      {SOKANROP.I}
      IF soktemp.SOKLOG[1] = TRUE THEN varforetypval[17] = 0.
      ELSE DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 89.
         soktemp.SOKCHAR[1] = datornamn.
         {SOKANROP.I}
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "1-5" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[1] = soktemp.SOKLOG[1]
            gastsek[2] = soktemp.SOKLOG[2]
            gastsek[3] = soktemp.SOKLOG[3]
            gastsek[4] = soktemp.SOKLOG[4]
            gastsek[5] = soktemp.SOKLOG[5].
         END.
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "6-10" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[6] = soktemp.SOKLOG[1]
            gastsek[7] = soktemp.SOKLOG[2]
            gastsek[8] = soktemp.SOKLOG[3]
            gastsek[9] = soktemp.SOKLOG[4]
            gastsek[10] = soktemp.SOKLOG[5].
         END.
         FIND FIRST soktemp WHERE soktemp.SOKCHAR[2] = "11-15" NO-LOCK NO-ERROR.
         IF AVAILABLE soktemp THEN DO:
            ASSIGN
            gastsek[11] = soktemp.SOKLOG[1]
            gastsek[12] = soktemp.SOKLOG[2]
            gastsek[13] = soktemp.SOKLOG[3]
            gastsek[14] = soktemp.SOKLOG[4]
            gastsek[15] = soktemp.SOKLOG[5].
         END.
      END.
   END.      
   IF varforetypval[17] = 0 THEN EDD_FUNK:HIDDEN = TRUE.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN RUN eddstart_UI.
   ELSE RUN eddstart_UI.
   
   /*
   /*{center.i}*/
   RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
  /* {FRMSIZE.I}    */
  */
   ASSIGN
   {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS 
   {&WINDOW-NAME}:MIN-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
   {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = globstorh
   {&WINDOW-NAME}:MAX-WIDTH-PIXELS = globstorb
   ph_frame = FRAME {&FRAME-NAME}:HANDLE.
   ON WINDOW-RESIZED OF {&WINDOW-NAME} 
   DO:     
      RUN FRM-SIZE.P (INPUT ph_frame,INPUT {&WINDOW-NAME}:HEIGHT-PIXELS,INPUT {&WINDOW-NAME}:WIDTH-PIXELS ).         
      RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
      {APPATFRAGA.I}
      alltidmax = NOT alltidmax.
   END.
   ON MOUSE-MENU-DBLCLICK OF CURRENT-WINDOW PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   ON MOUSE-MENU-DBLCLICK OF ph_frame PERSISTENT RUN fileinfo_UI IN THIS-PROCEDURE.
   /*
   RUN SetWindowLongB IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},{&GWL_STYLE},i).
   RUN SendMessageB IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},{&BM_SETSTYLE},i,1).
   */
   IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
   ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   /*
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:      
      FILE-INFO:FILE-NAME = "prox.dll".
      kommando = FILE-INFO:FULL-PATHNAME.  
      kommando = "regsvr32 /s " + '"' + kommando + '"'.
      OS-COMMAND SILENT VALUE(kommando).      
      
   END.
   */
   /*
   IF Guru.Konstanter:globforetag = "sund" THEN  DO:
      FILE-INFO:FILE-NAME = "prox.dll". 
      kommando = FILE-INFO:FULL-PATHNAME.  
      kommando = "regsvr32 /s " + '"' + kommando + '"'.
      OS-COMMAND SILENT VALUE(kommando). 
   END.
   */

   /*updateringsknapp*/ 
   IF bloblog AND (varforetypval[14] = 1 OR 
      SESSION:CLIENT-TYPE = "WEBCLIENT" OR 
      Guru.Konstanter:globforetag = "ELPA") THEN DO:
      BTN_UPPDAT:HIDDEN = FALSE.
      /*vattenfalls citrix*/
      IF datornamn = "thnmf" THEN BTN_UPPDAT:HIDDEN = TRUE.
      IF datornamn = "thnblade01" THEN BTN_UPPDAT:HIDDEN = TRUE.      
   END.
   ELSE BTN_UPPDAT:HIDDEN = TRUE.
   BTN_MEDD:LABEL = "Skapa meddelande~ntill Guruanvändare". 
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_MEDD:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   IF musz = TRUE THEN LEAVE MAIN-BLOCK.
   IF musz = TRUE THEN RETURN.    
   WWSTART:TITLE = foretemp.ATRHOME + " - " + "Systemsupport 090/184540".   
   IF Guru.Konstanter:globanv = "FLEX"  THEN DO:
     vartpro = "FLX".
     APPLY "CHOOSE" TO BTN_KOR. 
   END.
   IF Guru.Konstanter:globanv = "ELPAO" THEN DO:
      foretemp.GRAFTECK = TRUE.
   END.
   ELSE foretemp.GRAFTECK = FALSE.   
   IF Guru.GlobalaVariabler:retvalkoll = TRUE THEN DO:
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
   END.

   IF Guru.Konstanter:globanv = "CLHA" AND Guru.Konstanter:globforetag = "VAST" THEN varforetypval[16] = 1.
   IF Guru.Konstanter:globanv = "ELPAO" AND Guru.Konstanter:globforetag = "VAST" THEN varforetypval[16] = 1.
   IF Guru.Konstanter:globanv = "FLEX" THEN musz = musz.
   ELSE DO:
      RUN VISMEDD.W.      
   END.
    RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
   {AVBFRAM.I}
   {musarrow.i}      
     
   {&WINDOW-NAME}:HIDDEN = FALSE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WWSTART  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
  THEN DELETE WIDGET WWSTART.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eddstart_UI WWSTART 
PROCEDURE eddstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN      
   {&WINDOW-NAME}:WIDTH-CHARS = 128.
   {&WINDOW-NAME}:HEIGHT-CHARS = {&WINDOW-NAME}:HEIGHT-CHARS + 3.
   FRAME FRAME-B:HEIGHT-CHARS = FRAME FRAME-B:HEIGHT-CHARS + 3.
   FRAME FRAME-B:WIDTH-CHARS = {&WINDOW-NAME}:WIDTH-CHARS - 3.
   /*FRAME FRAME-B:HEIGHT-CHARS = {&WINDOW-NAME}:HEIGHT-CHARS - 1.*/
   
   ASSIGN
   EDD_FUNK:WIDTH-CHARS = EDD_FUNK:WIDTH-CHARS - 9
   EDD_FUNK:HEIGHT-CHARS = EDD_FUNK:HEIGHT-CHARS + 11.5
   EDD_FUNK:COLUMN = 69 
   EDD_FUNK:ROW = 2. 
   ASSIGN
   IMAGE-3:WIDTH-CHARS = 15
   IMAGE-3:HEIGHT-CHARS = 2.75
   IMAGE-3:COLUMN = 50 
   IMAGE-3:ROW = 2. 
   IMAGE-3:LOAD-IMAGE("BILDER\elpumea.jpg").
   IMAGE-3:HIDDEN = FALSE.
   ASSIGN
   FILL-IN-FUNK:ROW = FILL-IN-FUNK:ROW + 3
   BTN_AVB:ROW =     BTN_AVB:ROW + 3   
   BTN_BYT:ROW =     BTN_BYT:ROW + 3
   BTN_BYTW:ROW =    BTN_BYTW:ROW + 3  
   BTN_KOR:ROW =     BTN_KOR:ROW + 3   
   BTN_MEDD:ROW =    BTN_MEDD:ROW + 3  
   BTN_UPPDAT:ROW =  BTN_UPPDAT:ROW + 3
   EDD_FUNK:ROW =    EDD_FUNK:ROW + 3  
   RECT-1:ROW =      RECT-1:ROW + 3    
   RECT-2:ROW =      RECT-2:ROW + 3    
   SEL_UPP:ROW =     SEL_UPP:ROW + 3.         
   SEL_UPP = "Kalkylering".
   DISPLAY SEL_UPP WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO SEL_UPP.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WWSTART  _DEFAULT-ENABLE
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
  DISPLAY EDD_FUNK SEL_UPP FILL-IN-GURU FILL-IN-FUNK 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  ENABLE RECT-1 RECT-2 EDD_FUNK SEL_UPP BTN_KOR BTN_MEDD BTN_BYT BTN_BYTW 
         BTN_UPPDAT BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE feldum_UI WWSTART 
PROCEDURE feldum_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fileinfo_UI WWSTART 
PROCEDURE fileinfo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   {filinfo.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gastmedd_UI WWSTART 
PROCEDURE gastmedd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill du prova flera moduler i GURU tag kontakt med Elpool 090-184540"
   VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insatt_UI WWSTART 
PROCEDURE insatt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF varforetypval[17] = 0 THEN 
   FILL-IN-GURU = "Välkommen till" + " " + SUBSTRING(foretemp.VERSION,1,10) + " " + globpersnamn.
   ELSE FILL-IN-GURU = "Välkommen till" + " " + SUBSTRING(foretemp.VERSION,1,10).
   DISPLAY FILL-IN-GURU WITH FRAME {&FRAME-NAME}.
   ASSIGN
   aonrlogvar = FALSE
   bulalogvar = FALSE
   berelogvar = FALSE
   faktlogvar = FALSE
   kalklogvar = FALSE
   kalk2logvar = FALSE
   mtrllogvar = FALSE
   perlogvar = FALSE
   planlogvar = FALSE
   reglogvar = FALSE
   tadmlogvar = FALSE
   tidalogvar = FALSE
   tidblogvar = FALSE
   tidologvar = FALSE
   tidrlogvar = FALSE
   tidlogvar = FALSE
   tidslogvar = FALSE
   tidtlogvar = FALSE
   regar = YEAR(TODAY)
   regmnr = MONTH(TODAY).  
   IF Guru.Konstanter:globniv = 0 THEN DO:
      ASSIGN
      globallpers = TRUE
      Guru.Konstanter:globallao = TRUE.
   END. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = 1.
   DO WHILE i <= 20:
     status-ok = SEL_UPP:DELETE(1) IN FRAME {&FRAME-NAME}.      
     i = i + 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextguru_UI WWSTART 
PROCEDURE nextguru_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST xsektemp WHERE xsektemp.MENYVART = xhop AND
   xsektemp.AV-LEVEL = Guru.Konstanter:globniv NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI WWSTART 
PROCEDURE sek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   xhop = "GURU".
   vart = "FMED".
   vart = "".
   xhop = "GURU".    
   IF Guru.Konstanter:appcon THEN DO:
      RUN SEKSTART.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT xhop,INPUT globniv,OUTPUT TABLE xsektemp).
   END.
   ELSE DO: 
      RUN SEKSTART.P 
      (INPUT xhop,INPUT globniv,OUTPUT TABLE xsektemp).
   END.
   RUN SEKALLA.P (INPUT "GURU").
   IF varforetypval[17] = 0 THEN DO:
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO: 
         raknarsek = 1.
         DO WHILE raknarsek <= 20:
          /*hoppsekvar[1] = (gaol) 
            Guru.Konstanter:hoppsekvar[2] = "Materielhantering"
            Guru.Konstanter:hoppsekvar[3] = "Kalkylering"
            Guru.Konstanter:hoppsekvar[4] = "Tidredovisning
            Guru.Konstanter:hoppsekvar[5] = "Flextid"
            Guru.Konstanter:hoppsekvar[6] = "Uppföljning"
            Guru.Konstanter:hoppsekvar[7] = "Personaladministration"
            Guru.Konstanter:hoppsekvar[8] = "Sekretess"
            Guru.Konstanter:hoppsekvar[9] = "Register"
            Guru.Konstanter:hoppsekvar[10] ="Faktureringsrutin"
            Guru.Konstanter:hoppsekvar[11] = Guru.Konstanter:gpll   
            Guru.Konstanter:hoppsekvar[12] = "Markvärdering"
            Guru.Konstanter:hoppsekvar[13] = "Projekteringskalender"
            Guru.Konstanter:hoppsekvar[14] = "Avbrott/Störning"
            Guru.Konstanter:hoppsekvar[15] = "SMS-administration").
      */
            IF raknarsek = 1 OR 
               raknarsek = 3 OR   
               raknarsek = 4 OR 
               raknarsek = 5 OR 
               raknarsek = 6 OR 
               raknarsek = 7 OR 
               raknarsek = 8 OR 
               raknarsek = 9 OR 
               raknarsek = 10 OR
               raknarsek = 11 OR
               raknarsek = 12 OR 
               raknarsek = 14 OR
               raknarsek = 15 
               THEN raknarsek = raknarsek.
            ELSE Guru.Konstanter:hoppsekvar[raknarsek] = FALSE.
            raknarsek = raknarsek + 1.
         END.
         /*stopp undermeny
         xhop = "BERE".
         RUN nextguru_UI.
         RUN stoppweb_UI.
         xhop = "MTRL". 
         RUN nextguru_UI.
         RUN stoppweb_UI.
         xhop = "BULA". 
         RUN nextguru_UI.
         RUN stoppweb_UI. 
         */              
      END.   
   END.
   IF Guru.Konstanter:hoppsekvar[1] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gaol) IN FRAME {&FRAME-NAME}.
   END.                     
   IF Guru.Konstanter:hoppsekvar[2] = TRUE THEN DO:
      
      IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
         status-ok = SEL_UPP:ADD-LAST("Beredning").
         status-ok = SEL_UPP:ADD-LAST("Materielhantering").
         /*status-ok = SEL_UPP:INSERT("Beredning","Materielhantering").*/
      END.
      ELSE status-ok = SEL_UPP:ADD-LAST("Materielhantering").
           
   END.        
   IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST("Kalkylering").
   END.    
   IF Guru.Konstanter:hoppsekvar[4] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST("Tidredovisning").
   END.        
   IF Guru.Konstanter:hoppsekvar[5] = TRUE THEN DO:
      status-ok = SEL_UPP:ADD-LAST("Flextid").
   END.    
   IF Guru.Konstanter:hoppsekvar[6] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST("Uppföljning").
   END.    
   IF Guru.Konstanter:hoppsekvar[7] = TRUE THEN DO: 
      status-ok = SEL_UPP:ADD-LAST("Personaladministration").
   END.    
   IF Guru.Konstanter:hoppsekvar[8] = TRUE THEN DO:        
      status-ok = SEL_UPP:ADD-LAST("Sekretess").       
   END.    
   IF Guru.Konstanter:hoppsekvar[9] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST("Register").
   END. 
   IF Guru.Konstanter:hoppsekvar[10] = TRUE THEN DO: 
      status-ok = SEL_UPP:ADD-LAST("Faktureringsrutin").
   END.          
   IF Guru.Konstanter:hoppsekvar[11] = TRUE THEN DO:  
      status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gpll).
   END.                       
   IF Guru.Konstanter:hoppsekvar[12] = TRUE THEN DO:
      status-ok = SEL_UPP:ADD-LAST("Markvärdering").
   END.
   /*
   IF Guru.Konstanter:hoppsekvar[13] = TRUE THEN DO:
      status-ok = SEL_UPP:ADD-LAST("Projekteringskalender").
   END.
   */
   IF Guru.Konstanter:hoppsekvar[14] = TRUE THEN DO:
      status-ok = SEL_UPP:ADD-LAST("Avbrott/Störning").
      IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "MALU" OR 
         Guru.Konstanter:globforetag = "BIRK" THEN DO:         
         RUN SEKALLA.P (INPUT "STOR").
         IF Guru.Konstanter:storsekvar[5] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Rapporter").
         IF Guru.Konstanter:storsekvar[6] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Administration").
         IF Guru.Konstanter:storsekvar[7] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Import/Export").         
      END.        
   END.   
   IF Guru.Konstanter:hoppsekvar[15] = TRUE THEN DO:
      status-ok = SEL_UPP:ADD-LAST("SMS-administration").
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stoppweb_UI WWSTART 
PROCEDURE stoppweb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = 1.
   IF AVAILABLE xsektemp THEN DO:
      DO WHILE i <= 20:
         xsektemp.SEK[i] = FALSE. 
         i = i + 1.
      END. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI WWSTART 
PROCEDURE vart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {PERSISTENTDELETE.I}
   DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
   IF varforetypval[17] = 1 THEN DO:
      IF SEL_UPP = Guru.Konstanter:gaol AND gastsek[1] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Beredning" AND gastsek[2] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Materielhantering" AND gastsek[2] = TRUE THEN SEL_UPP = SEL_UPP.                       
      ELSE IF SEL_UPP = "Kalkylering" AND gastsek[3] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Tidredovisning" AND gastsek[4] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Flextid" AND gastsek[5] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Uppföljning" AND gastsek[6] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Personaladministration" AND gastsek[7] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Sekretess" AND gastsek[8] = TRUE THEN SEL_UPP = SEL_UPP.       
      ELSE IF SEL_UPP = "Register" AND gastsek[9] = TRUE THEN SEL_UPP = SEL_UPP.   
      ELSE IF SEL_UPP = "Faktureringsrutin" AND gastsek[10] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = Guru.Konstanter:gpll AND gastsek[11] = TRUE THEN SEL_UPP = SEL_UPP.   
      ELSE IF SEL_UPP = "Markvärdering" AND gastsek[12] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Projekteringskalender" AND gastsek[13] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE IF SEL_UPP = "Avbrott/Störning" AND gastsek[14] = TRUE THEN SEL_UPP = SEL_UPP.        
      ELSE IF SEL_UPP = "SMS-administration" AND gastsek[15] = TRUE THEN SEL_UPP = SEL_UPP.
      ELSE DO:
         RUN gastmedd_UI.
         RETURN NO-APPLY.
      END.
   END.
   IF SEL_UPP = Guru.Konstanter:gaol THEN vartpro = "AOR".     
   ELSE IF SEL_UPP = "Materielhantering" THEN vartpro = "DEP".
   ELSE IF SEL_UPP = "Kalkylering" THEN DO:
      ASSIGN
      vart = "KAL"         
      vartpro = "KAL".
   END.
   ELSE IF SEL_UPP = "Tidredovisning" THEN vartpro = "TID".
   ELSE IF SEL_UPP = "Flextid" THEN vartpro = "FLX".
   ELSE IF SEL_UPP = "Uppföljning" THEN vartpro = "UPP".
   ELSE IF SEL_UPP = "Personaladministration" THEN vartpro = "PER".
   ELSE IF SEL_UPP = "Sekretess" THEN vartpro = "SEK".       
   ELSE IF SEL_UPP = "Register" THEN vartpro = "REG".
   ELSE IF SEL_UPP = "Faktureringsrutin" THEN vartpro = "FAF".      
   ELSE IF SEL_UPP = Guru.Konstanter:gpll THEN vartpro = "PLA".   
   ELSE IF SEL_UPP = "Markvärdering" THEN vartpro = "MVA".
   ELSE IF SEL_UPP = "Projekteringskalender" THEN vartpro = "END".
   ELSE IF SEL_UPP = "Avbrott/Störning" THEN vartpro = "STR".
   ELSE IF SEL_UPP = "Rapporter" THEN vartpro = "STR1".
   ELSE IF SEL_UPP = "Administration" THEN vartpro = "STR2".
   ELSE IF SEL_UPP = "Import/Export" THEN vartpro = "STR3". 
   ELSE IF SEL_UPP = "SMS-administration" THEN vartpro = "SMS".
   ELSE IF SEL_UPP = "Beredning" THEN vartpro = "BER".     
   {AVBGOM.I}
   RUN XVARTPRO.P.
   {AVBFRAM.I} 
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

