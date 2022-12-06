&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{AVDELNINGTEMP.I}
 
&Scoped-define NEW 
{OMRALLT.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE orgapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avdelningextratemp

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH avdelningextratemp NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH avdelningextratemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 avdelningextratemp
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 avdelningextratemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN avdelningtemp.ELVOMRKOD avdelningtemp.GATUADR ~
avdelningtemp.POSTNR avdelningtemp.POSTADR avdelningtemp.TELVXL ~
avdelningtemp.TELJOUR avdelningextratemp.BANKGIRO ~
avdelningextratemp.POSTGIRO avdelningextratemp.ORGNR ~
avdelningextratemp.VATNR avdelningextratemp.F-SKATT ~
avdelningextratemp.DROJRANT avdelningtemp.ANTALAB avdelningtemp.ANTALANST ~
avdelningtemp.NUAK avdelningtemp.BUDGRAMINV avdelningtemp.BUDGRAMUH ~
avdelningtemp.KOSTPERS avdelningtemp.KOSTMASK avdelningtemp.KOSTNTYPINV ~
avdelningtemp.KOSTNTYPUH avdelningtemp.KONTOA avdelningtemp.KONTOB ~
avdelningtemp.KONTOC avdelningtemp.KONTOD 
&Scoped-define ENABLED-TABLES avdelningtemp avdelningextratemp
&Scoped-define FIRST-ENABLED-TABLE avdelningtemp
&Scoped-define SECOND-ENABLED-TABLE avdelningextratemp
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-12 SEL_JUR FILL-IN_NAMN BTN_Ok ~
BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN avdelningtemp.ELVOMRKOD avdelningtemp.GATUADR ~
avdelningtemp.POSTNR avdelningtemp.POSTADR avdelningtemp.TELVXL ~
avdelningtemp.TELJOUR avdelningextratemp.BANKGIRO ~
avdelningextratemp.POSTGIRO avdelningextratemp.ORGNR ~
avdelningextratemp.VATNR avdelningextratemp.F-SKATT ~
avdelningextratemp.DROJRANT avdelningtemp.ANTALAB avdelningtemp.ANTALANST ~
avdelningtemp.NUAK avdelningtemp.BUDGRAMINV avdelningtemp.BUDGRAMUH ~
avdelningtemp.KOSTPERS avdelningtemp.KOSTMASK avdelningtemp.KOSTNTYPINV ~
avdelningtemp.KOSTNTYPUH avdelningtemp.KONTOA avdelningtemp.KONTOB ~
avdelningtemp.KONTOC avdelningtemp.KONTOD 
&Scoped-define DISPLAYED-TABLES avdelningtemp avdelningextratemp
&Scoped-define FIRST-DISPLAYED-TABLE avdelningtemp
&Scoped-define SECOND-DISPLAYED-TABLE avdelningextratemp
&Scoped-Define DISPLAYED-OBJECTS SEL_JUR FILL-IN_NAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_Ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(38)" 
     LABEL "NAMN" 
     VIEW-AS FILL-IN 
     SIZE 39.88 BY 1.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.88 BY 13.46
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.88 BY 8.54
     BGCOLOR 8 .

DEFINE VARIABLE SEL_JUR AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 20.5 BY 4.42 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      avdelningextratemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     avdelningtemp.AVDELNINGNR AT ROW 1.46 COL 25 COLON-ALIGNED
          LABEL "Avdelningnr" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     avdelningtemp.AVDELNINGNAMN AT ROW 1.46 COL 43.25 COLON-ALIGNED
          LABEL "Namn" FORMAT "x(35)"
          VIEW-AS FILL-IN 
          SIZE 36.5 BY 1
          BGCOLOR 15 
     avdelningtemp.ELVOMRKOD AT ROW 2.54 COL 25 COLON-ALIGNED
          LABEL "Nummeriskkod"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.GATUADR AT ROW 3.63 COL 25 COLON-ALIGNED
          LABEL "Gatuadress"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     avdelningtemp.POSTNR AT ROW 4.75 COL 25 COLON-ALIGNED
          LABEL "Postnummer"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     avdelningtemp.POSTADR AT ROW 4.75 COL 57.63 COLON-ALIGNED
          LABEL "Postadress"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     avdelningtemp.TELVXL AT ROW 5.88 COL 25 COLON-ALIGNED
          LABEL "Telefonnr"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     avdelningtemp.TELJOUR AT ROW 5.88 COL 57.63 COLON-ALIGNED
          LABEL "Journummer"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     avdelningextratemp.BANKGIRO AT ROW 6.96 COL 25 COLON-ALIGNED
          LABEL "Bankgiro" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningextratemp.POSTGIRO AT ROW 6.96 COL 57.63 COLON-ALIGNED
          LABEL "Postgiro" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     avdelningextratemp.ORGNR AT ROW 8.04 COL 25 COLON-ALIGNED
          LABEL "Organisationsnr" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     avdelningextratemp.VATNR AT ROW 9.13 COL 25 COLON-ALIGNED
          LABEL "Vatnr" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     SEL_JUR AT ROW 9.42 COL 68.63 NO-LABEL
     avdelningextratemp.F-SKATT AT ROW 10.25 COL 25 COLON-ALIGNED
          LABEL "Text för F-SKATT" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     avdelningextratemp.DROJRANT AT ROW 11.38 COL 25 COLON-ALIGNED
          LABEL "Text för dröjsmålränta" FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     FILL-IN_NAMN AT ROW 12.58 COL 24.88 COLON-ALIGNED
     avdelningtemp.ANTALAB AT ROW 14.75 COL 25 COLON-ALIGNED
          LABEL "Abbonnent ant."
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.ANTALANST AT ROW 14.75 COL 57.63 COLON-ALIGNED
          LABEL "Heltidsanst."
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.NUAK AT ROW 15.83 COL 25 COLON-ALIGNED
          LABEL "Nuak i Kkr"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.BUDGRAMINV AT ROW 16.92 COL 25 COLON-ALIGNED
          LABEL "Budeget ram inv. i Kkr."
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.BUDGRAMUH AT ROW 16.92 COL 57.63 COLON-ALIGNED
          LABEL "Budget ram uh i Kkr"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.KOSTPERS AT ROW 18.04 COL 25 COLON-ALIGNED
          LABEL "Person kr/h"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     avdelningtemp.KOSTMASK AT ROW 18.04 COL 57.63 COLON-ALIGNED
          LABEL "Maskin kr/h"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DIALOG-3
     avdelningtemp.KOSTNTYPINV AT ROW 19.13 COL 25 COLON-ALIGNED
          LABEL "Kostnadstypsgrupp inv"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     avdelningtemp.KOSTNTYPUH AT ROW 19.13 COL 57.63 COLON-ALIGNED
          LABEL "Kostnadstypgrupp uh"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     avdelningtemp.KONTOA AT ROW 20.25 COL 25 COLON-ALIGNED
          LABEL "Kontodel A"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     avdelningtemp.KONTOB AT ROW 20.25 COL 57.63 COLON-ALIGNED
          LABEL "Kontodel B"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     avdelningtemp.KONTOC AT ROW 21.33 COL 25 COLON-ALIGNED
          LABEL "Kontodel C"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     avdelningtemp.KONTOD AT ROW 21.33 COL 57.63 COLON-ALIGNED
          LABEL "Kontodel D"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     BTN_Ok AT ROW 23.42 COL 60.88
     BTN_AVB AT ROW 23.42 COL 75.88
     RECT-11 AT ROW 1 COL 1
     RECT-12 AT ROW 14.46 COL 1
     SPACE(0.00) SKIP(1.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Avdelning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: avdelningextratemp T "?" NO-UNDO temp-db avdelningextratemp
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN avdelningtemp.ANTALAB IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.ANTALANST IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.AVDELNINGNAMN IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.AVDELNINGNR IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningextratemp.BANKGIRO IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.BUDGRAMINV IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.BUDGRAMUH IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningextratemp.DROJRANT IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.ELVOMRKOD IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningextratemp.F-SKATT IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.GATUADR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KONTOA IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KONTOB IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KONTOC IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KONTOD IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KOSTMASK IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KOSTNTYPINV IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KOSTNTYPUH IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.KOSTPERS IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.NUAK IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningextratemp.ORGNR IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.POSTADR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningextratemp.POSTGIRO IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN avdelningtemp.POSTNR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.TELJOUR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningtemp.TELVXL IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN avdelningextratemp.VATNR IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT9.avdelningextratemp"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Avdelning */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Avdelning */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME avdelningtemp.AVDELNINGNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL avdelningtemp.AVDELNINGNR DIALOG-3
ON LEAVE OF avdelningtemp.AVDELNINGNR IN FRAME DIALOG-3 /* Avdelningnr */
DO:
   avdelningtemp.AVDELNINGNR = INPUT avdelningtemp.AVDELNINGNR.
   avdelningextratemp.AVDELNINGNR = INPUT avdelningtemp.AVDELNINGNR.
   IF avdelningtemp.AVDELNINGNR = 0 THEN DO:
      MESSAGE Guru.Konstanter:gavdk " kan inte vara noll!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.AVDELNINGNR = INPUT avdelningtemp.AVDELNINGNR 
   NO-LOCK NO-ERROR.
   IF AVAILABLE juromravd THEN DO:
      MESSAGE Guru.Konstanter:gavdk "finns redan upplagt!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-3
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_Ok DIALOG-3
ON CHOOSE OF BTN_Ok IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}   
   ASSIGN
   avdelningtemp.AVDELNINGNR      = INPUT avdelningtemp.AVDELNINGNR  
   avdelningtemp.AVDELNINGNAMN    = INPUT avdelningtemp.AVDELNINGNAMN
   avdelningtemp.ELVOMRKOD        = INPUT avdelningtemp.ELVOMRKOD    
   avdelningtemp.GATUADR          = INPUT avdelningtemp.GATUADR      
   avdelningtemp.POSTNR           = INPUT avdelningtemp.POSTNR       
   avdelningtemp.POSTADR          = INPUT avdelningtemp.POSTADR      
   avdelningtemp.TELVXL           = INPUT avdelningtemp.TELVXL       
   avdelningtemp.TELJOUR          = INPUT avdelningtemp.TELJOUR      
   avdelningextratemp.BANKGIRO        = INPUT avdelningextratemp.BANKGIRO    
   avdelningextratemp.POSTGIRO        = INPUT avdelningextratemp.POSTGIRO    
   avdelningextratemp.ORGNR           = INPUT avdelningextratemp.ORGNR       
   avdelningextratemp.VATNR           = INPUT avdelningextratemp.VATNR       
   avdelningextratemp.F-SKATT         = INPUT avdelningextratemp.F-SKATT     
   avdelningextratemp.DROJRANT        = INPUT avdelningextratemp.DROJRANT    
   avdelningtemp.ANTALAB          = INPUT avdelningtemp.ANTALAB      
   avdelningtemp.ANTALANST        = INPUT avdelningtemp.ANTALANST    
   avdelningtemp.NUAK             = INPUT avdelningtemp.NUAK         
   avdelningtemp.BUDGRAMINV       = INPUT avdelningtemp.BUDGRAMINV   
   avdelningtemp.BUDGRAMUH        = INPUT avdelningtemp.BUDGRAMUH    
   avdelningtemp.KOSTPERS         = INPUT avdelningtemp.KOSTPERS     
   avdelningtemp.KOSTMASK         = INPUT avdelningtemp.KOSTMASK     
   avdelningtemp.KOSTNTYPINV      = INPUT avdelningtemp.KOSTNTYPINV  
   avdelningtemp.KOSTNTYPUH       = INPUT avdelningtemp.KOSTNTYPUH   
   avdelningtemp.KONTOA           = INPUT avdelningtemp.KONTOA       
   avdelningtemp.KONTOB           = INPUT avdelningtemp.KONTOB       
   avdelningtemp.KONTOC           = INPUT avdelningtemp.KONTOC       
   avdelningtemp.KONTOD           = INPUT avdelningtemp.KONTOD
   FILL-IN_NAMN = INPUT FILL-IN_NAMN.
   IF avdelningtemp.AVDELNINGNR = 0 THEN DO:
      MESSAGE Guru.Konstanter:gavdk " kan inte vara noll!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF FILL-IN_NAMN NE "" THEN DO:   
      FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.NAMN = FILL-IN_NAMN NO-LOCK NO-ERROR.
      IF NOT AVAILABLE juromravd THEN DO:
         MESSAGE Guru.Konstanter:gjul " finns inte!" VIEW-AS ALERT-BOX.      
         RETURN NO-APPLY. 
      END.
      ELSE avdelningtemp.POSTANST = juromravd.SID.
   END.   
   ELSE DO:
      avdelningtemp.POSTANST = "".
   END.      
   RUN sparavd_UI IN orgapph (INPUT TABLE avdelningtemp,INPUT TABLE avdelningextratemp,OUTPUT TABLE felmeddtemp).
   vem = STRING(avdelningtemp.AVDELNINGNR).
   APPLY "GO" TO FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_JUR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_JUR DIALOG-3
ON VALUE-CHANGED OF SEL_JUR IN FRAME DIALOG-3
DO:
   SEL_JUR = INPUT SEL_JUR.
   FILL-IN_NAMN = SEL_JUR.
   DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   RUN avdhmten_UI IN orgapph (INPUT vem,OUTPUT TABLE avdelningtemp,OUTPUT TABLE avdelningextratemp).
   FIND FIRST avdelningtemp NO-ERROR.
   FIND FIRST avdelningextratemp NO-ERROR.   
   avdelningtemp.AVDELNINGNR:LABEL = Guru.Konstanter:gavdl.
   IF vem = ? THEN DO TRANSACTION:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny" + LC(Guru.Konstanter:gavdl).       
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + avdelningtemp.AVDELNINGNAMN.           
   END.
   FOR EACH juromravd WHERE juromravd.TYP = "JUR" NO-LOCK:      
      stat = SEL_JUR:ADD-LAST(juromravd.NAMN).
   END.    
   IF avdelningtemp.POSTANST NE "" THEN DO:
      FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.SID = avdelningtemp.POSTANST NO-ERROR.      
      IF AVAILABLE juromravd THEN DO:
         FILL-IN_NAMN = juromravd.NAMN. 
         SEL_JUR:SCREEN-VALUE = juromravd.NAMN.
      END.
   END.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      avdelningtemp.ELVOMRKOD:LABEL = "Faktura företag".     
   END.
   FILL-IN_NAMN:LABEL = Guru.Konstanter:gjuk.
   RUN enable_UI.       
   {FRMSIZED.I}
   FIND FIRST juromravd WHERE juromravd.TYP = "JUR" NO-ERROR.
   IF NOT AVAILABLE juromravd THEN SEL_JUR:HIDDEN = TRUE.   
   IF vem NE ? THEN DO:     
      DISABLE avdelningtemp.AVDELNINGNR WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY SEL_JUR FILL-IN_NAMN 
      WITH FRAME DIALOG-3.
  IF AVAILABLE avdelningextratemp THEN 
    DISPLAY avdelningextratemp.BANKGIRO avdelningextratemp.POSTGIRO 
          avdelningextratemp.ORGNR avdelningextratemp.VATNR 
          avdelningextratemp.F-SKATT avdelningextratemp.DROJRANT 
      WITH FRAME DIALOG-3.
  IF AVAILABLE avdelningtemp THEN 
    DISPLAY avdelningtemp.AVDELNINGNR avdelningtemp.AVDELNINGNAMN 
          avdelningtemp.ELVOMRKOD avdelningtemp.GATUADR avdelningtemp.POSTNR 
          avdelningtemp.POSTADR avdelningtemp.TELVXL avdelningtemp.TELJOUR 
          avdelningtemp.ANTALAB avdelningtemp.ANTALANST avdelningtemp.NUAK 
          avdelningtemp.BUDGRAMINV avdelningtemp.BUDGRAMUH 
          avdelningtemp.KOSTPERS avdelningtemp.KOSTMASK 
          avdelningtemp.KOSTNTYPINV avdelningtemp.KOSTNTYPUH 
          avdelningtemp.KONTOA avdelningtemp.KONTOB avdelningtemp.KONTOC 
          avdelningtemp.KONTOD 
      WITH FRAME DIALOG-3.
  ENABLE RECT-11 RECT-12 avdelningtemp.AVDELNINGNR avdelningtemp.AVDELNINGNAMN 
         avdelningtemp.ELVOMRKOD avdelningtemp.GATUADR avdelningtemp.POSTNR 
         avdelningtemp.POSTADR avdelningtemp.TELVXL avdelningtemp.TELJOUR 
         avdelningextratemp.BANKGIRO avdelningextratemp.POSTGIRO 
         avdelningextratemp.ORGNR avdelningextratemp.VATNR SEL_JUR 
         avdelningextratemp.F-SKATT avdelningextratemp.DROJRANT FILL-IN_NAMN 
         avdelningtemp.ANTALAB avdelningtemp.ANTALANST avdelningtemp.NUAK 
         avdelningtemp.BUDGRAMINV avdelningtemp.BUDGRAMUH 
         avdelningtemp.KOSTPERS avdelningtemp.KOSTMASK 
         avdelningtemp.KOSTNTYPINV avdelningtemp.KOSTNTYPUH 
         avdelningtemp.KONTOA avdelningtemp.KONTOB avdelningtemp.KONTOC 
         avdelningtemp.KONTOD BTN_Ok BTN_AVB 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

