&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER faonr AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER fdelnr AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER vama AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER mall AS INTEGER  NO-UNDO.
DEFINE OUTPUT PARAMETER ponr AS CHARACTER FORMAT "X(12)" NO-UNDO.
DEFINE VARIABLE kontaktvar AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
&Scoped-define NEW  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-PROJEKT FILL-IN-LEDLITT ~
FILL-IN-PNAMN FILL-IN-PLED FILL-IN-NAT FILL-IN-KONC FILL-IN-DIARIENR ~
FILL-IN-PLATS FILL-IN-TIDPUNKT FILL-IN-ARTAL BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PROJEKT FILL-IN-LEDLITT ~
FILL-IN-PNAMN FILL-IN-PLED FILL-IN-NAT FILL-IN-KONC FILL-IN-DIARIENR ~
FILL-IN-PLATS FILL-IN-TIDPUNKT FILL-IN-ARTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ARTAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Årtal" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DIARIENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Diarienummer" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KONC AS CHARACTER FORMAT "X(256)":U 
     LABEL "Konc" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDLITT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ledningslittera" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nätstation" 
     VIEW-AS FILL-IN 
     SIZE 27.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Plats" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLED AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektledare" 
     VIEW-AS FILL-IN 
     SIZE 27.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektnamn" 
     VIEW-AS FILL-IN 
     SIZE 48.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKT AS CHARACTER FORMAT "X(18)":U 
     LABEL "ProjektID" 
     VIEW-AS FILL-IN 
     SIZE 21.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIDPUNKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tidpunkt" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-PROJEKT AT ROW 1.58 COL 33 COLON-ALIGNED
     FILL-IN-LEDLITT AT ROW 2.79 COL 33 COLON-ALIGNED
     FILL-IN-PNAMN AT ROW 4 COL 33 COLON-ALIGNED
     FILL-IN-PLED AT ROW 5.21 COL 33 COLON-ALIGNED
     FILL-IN-NAT AT ROW 6.42 COL 33 COLON-ALIGNED
     FILL-IN-KONC AT ROW 7.63 COL 33 COLON-ALIGNED
     FILL-IN-DIARIENR AT ROW 8.83 COL 33 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-PLATS AT ROW 10.04 COL 33 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-TIDPUNKT AT ROW 11.25 COL 33 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-ARTAL AT ROW 12.5 COL 33 COLON-ALIGNED WIDGET-ID 6
     BTN_OK AT ROW 18.46 COL 61
     BTN_AVS AT ROW 18.46 COL 76
     SPACE(1.12) SKIP(0.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nätägarens projektnummer".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Nätägarens projektnummer */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Nätägarens projektnummer */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON CHOOSE OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
   
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON GO OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
  IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   musz = FALSE.
                   
   ASSIGN
   FILL-IN-PROJEKT = INPUT FILL-IN-PROJEKT
   FILL-IN-LEDLITT = INPUT FILL-IN-LEDLITT
   FILL-IN-PNAMN = INPUT FILL-IN-PNAMN
   FILL-IN-PLED = INPUT FILL-IN-PLED
   FILL-IN-NAT = INPUT FILL-IN-NAT
   FILL-IN-KONC = INPUT FILL-IN-KONC
   FILL-IN-DIARIENR = INPUT FILL-IN-DIARIENR      
   FILL-IN-PLATS = INPUT FILL-IN-PLATS
   FILL-IN-TIDPUNKT = INPUT FILL-IN-TIDPUNKT
   FILL-IN-ARTAL = INPUT FILL-IN-ARTAL.
   ponr = FILL-IN-PROJEKT.
   IF ponr NE "" THEN DO:
      IF vama = "vard" THEN DO:
         IF  mall = 27 OR mall = 28 OR mall = 29 OR mall = 97 OR mall = 98 OR mall = 104 THEN DO:
            IF FILL-IN-KONC = "5" OR FILL-IN-KONC = "10" OR FILL-IN-KONC = "12" OR FILL-IN-KONC = "13" OR FILL-IN-KONC = "14" OR FILL-IN-KONC = "16" OR FILL-IN-KONC = "19" OR FILL-IN-KONC = "20" OR FILL-IN-KONC = "21" OR
            FILL-IN-KONC = "30" OR FILL-IN-KONC = "31" OR FILL-IN-KONC = "32" OR FILL-IN-KONC = "33" OR FILL-IN-KONC = "34" OR FILL-IN-KONC = "35" OR FILL-IN-KONC = "36" OR FILL-IN-KONC = "38" OR FILL-IN-KONC = "39" OR
            FILL-IN-KONC = "40" OR FILL-IN-KONC = "41" OR FILL-IN-KONC = "42" OR FILL-IN-KONC = "43" OR FILL-IN-KONC = "44" OR FILL-IN-KONC = "45" OR FILL-IN-KONC = "47" OR FILL-IN-KONC = "48" OR FILL-IN-KONC = "50" OR
            FILL-IN-KONC = "51" OR FILL-IN-KONC = "52" OR FILL-IN-KONC = "53" OR FILL-IN-KONC = "54" OR FILL-IN-KONC = "60" OR FILL-IN-KONC = "61" OR FILL-IN-KONC = "62" OR FILL-IN-KONC = "63" OR FILL-IN-KONC = "64"
            THEN. 
            ELSE DO:      
               MESSAGE "Koncessionsområde måste vara  5,10,12,13,14,16,19,20,21,30,31,32,33,34,35,36,38,39,40,41,42,44,45,47,48,50,51,52,53,54,60,61,62,63,64" VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-KONC IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
         END.
      END.
   END.         
      
   IF FILL-IN-LEDLITT = "" AND FILL-IN-PNAMN = "" AND FILL-IN-PLED = "" AND FILL-IN-KONC = "" AND FILL-IN-DIARIENR = ""   THEN .
   ELSE IF FILL-IN-PROJEKT = "" THEN.
   ELSE DO:   

      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT
      inextradatatemp.SOKCHAR[1] = FILL-IN-LEDLITT
      inextradatatemp.SOKCHAR[2] = FILL-IN-PNAMN
      inextradatatemp.SOKCHAR[3] = FILL-IN-PLED
      inextradatatemp.SOKCHAR[4] = FILL-IN-KONC
      inextradatatemp.SOKCHAR[5] = FILL-IN-DIARIENR   
      /*inextradatatemp.SOKCHAR[5] = FILL-IN-DIARIENR tidigare FILL-IN-DOMSAGA  */
      /*inextradatatemp.SOKDATE[1] = FILL-IN-UTFARD LEDIGA      
      inextradatatemp.SOKCHAR[6] = FILL-IN-UNAMN 
      inextradatatemp.SOKCHAR[7] = FILL-IN-ATTEST 
      inextradatatemp.SOKCHAR[8] = FILL-IN-PN*/
      inextradatatemp.SOKCHAR[9] = FILL-IN-NAT.

       
      
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   END.
   IF FILL-IN-PLATS = "" AND FILL-IN-TIDPUNKT = "" AND FILL-IN-ARTAL = ""  THEN .
   ELSE IF FILL-IN-PROJEKT = "" THEN.
   ELSE DO:      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ2"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT
      inextradatatemp.SOKCHAR[1] = FILL-IN-PLATS
      inextradatatemp.SOKCHAR[2] = FILL-IN-TIDPUNKT
      inextradatatemp.SOKCHAR[3] = FILL-IN-ARTAL.          
      
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   END.
         
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON GO OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARTAL Dialog-Frame
ON LEAVE OF FILL-IN-ARTAL IN FRAME Dialog-Frame /* Årtal */
DO:
   ASSIGN
   FILL-IN-KONC = INPUT FILL-IN-KONC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DIARIENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DIARIENR Dialog-Frame
ON LEAVE OF FILL-IN-DIARIENR IN FRAME Dialog-Frame /* Diarienummer */
DO:
   ASSIGN
   FILL-IN-DIARIENR = INPUT FILL-IN-DIARIENR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KONC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KONC Dialog-Frame
ON LEAVE OF FILL-IN-KONC IN FRAME Dialog-Frame /* Konc */
DO:
   ASSIGN
   FILL-IN-KONC = INPUT FILL-IN-KONC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LEDLITT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LEDLITT Dialog-Frame
ON LEAVE OF FILL-IN-LEDLITT IN FRAME Dialog-Frame /* Ledningslittera */
DO:
   ASSIGN
   FILL-IN-LEDLITT = INPUT FILL-IN-LEDLITT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NAT Dialog-Frame
ON LEAVE OF FILL-IN-NAT IN FRAME Dialog-Frame /* Nätstation */
DO:
  ASSIGN
   FILL-IN-NAT = INPUT FILL-IN-NAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLATS Dialog-Frame
ON LEAVE OF FILL-IN-PLATS IN FRAME Dialog-Frame /* Plats */
DO:
   ASSIGN
   FILL-IN-KONC = INPUT FILL-IN-KONC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PLED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLED Dialog-Frame
ON LEAVE OF FILL-IN-PLED IN FRAME Dialog-Frame /* Projektledare */
DO:
  ASSIGN
   FILL-IN-PLED = INPUT FILL-IN-PLED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PNAMN Dialog-Frame
ON LEAVE OF FILL-IN-PNAMN IN FRAME Dialog-Frame /* Projektnamn */
DO:
  ASSIGN
   FILL-IN-PNAMN = INPUT FILL-IN-PNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PROJEKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PROJEKT Dialog-Frame
ON LEAVE OF FILL-IN-PROJEKT IN FRAME Dialog-Frame /* ProjektID */
DO:
   ASSIGN
   FILL-IN-PROJEKT = INPUT FILL-IN-PROJEKT.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF FILL-IN-PROJEKT NE "" THEN DO: 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   END.    
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN      
      FILL-IN-LEDLITT = extradatatemp.SOKCHAR[1] 
      FILL-IN-PNAMN = extradatatemp.SOKCHAR[2] 
      FILL-IN-PLED = extradatatemp.SOKCHAR[3]
      FILL-IN-KONC = extradatatemp.SOKCHAR[4]
      FILL-IN-DIARIENR = extradatatemp.SOKCHAR[5] 
      /*inextradatatemp.SOKCHAR[5] = FILL-IN-DIARIENR tidigare FILL-IN-DOMSAGA  */
      /*FILL-IN-UTFARD  = extradatatemp.SOKDATE[1] LEDIGA 
      FILL-IN-UNAMN = extradatatemp.SOKCHAR[6] 
      FILL-IN-ATTEST = extradatatemp.SOKCHAR[7] 
      FILL-IN-PN = extradatatemp.SOKCHAR[8] */
      FILL-IN-NAT = extradatatemp.SOKCHAR[9] .
   END.
   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF FILL-IN-PROJEKT NE "" THEN DO: 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ2"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   END.    
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN      
      FILL-IN-PLATS = extradatatemp.SOKCHAR[1] 
      FILL-IN-TIDPUNKT = extradatatemp.SOKCHAR[2] 
      FILL-IN-ARTAL = extradatatemp.SOKCHAR[3].      
   END.
   /*används ej längre*/
   /*EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   IF FILL-IN-PROJEKT NE "" THEN DO: 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "PROJHSAND"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   END.    
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN      
      FILL-IN-GAMLED = extradatatemp.SOKCHAR[1] 
      FILL-IN-DODAS = extradatatemp.SOKCHAR[2] 
      FILL-IN-KRHA = extradatatemp.SOKINT[1]             
      FILL-IN-SKADEERS = extradatatemp.SOKINT[2]
      FILL-IN-TILLDAG = extradatatemp.SOKDATE[1]
      FILL-IN-TKMETER = extradatatemp.SOKCHAR[3]     
      FILL-IN-NSTN = extradatatemp.SOKINT[4].                 
   END.*/
      
   
   DISPLAY FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED FILL-IN-KONC  FILL-IN-NAT FILL-IN-DIARIENR WITH  FRAME {&FRAME-NAME}.
   ASSIGN 
   FILL-IN-KONC:HIDDEN = TRUE
   FILL-IN-PLATS:HIDDEN = TRUE 
   FILL-IN-TIDPUNKT:HIDDEN = TRUE 
   FILL-IN-ARTAL:HIDDEN = TRUE
   FILL-IN-DIARIENR:HIDDEN = TRUE.   
     
   IF vama = "vard" THEN DO:
      IF mall = 6  OR mall = 27 OR mall = 28 OR mall = 29 OR mall = 97 OR mall = 98 OR mall = 104 THEN DO:      
         FILL-IN-KONC:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE .
      END.
      ELSE IF mall = 88 OR  mall = 24 OR  mall = 25 THEN DO:
               
         FILL-IN-DIARIENR:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE .
      END.
      ELSE IF mall = 52 THEN DO:
         /*Informationsbrev Vattenfall*/
         ASSIGN
         FILL-IN-PLATS:HIDDEN = FALSE 
         FILL-IN-TIDPUNKT:HIDDEN = FALSE 
         FILL-IN-ARTAL:HIDDEN = FALSE.
      END.           
   END.
      
   
   /*IF vama = "utan" AND mall = 6 THEN DO:
      ASSIGN
      FILL-IN-UTFARD:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-UNAMN:HIDDEN = FALSE.
      FILL-IN-ATTEST:HIDDEN = FALSE.
      FILL-IN-PN:HIDDEN = FALSE.
   END.
   
   DO:
      ASSIGN
      FILL-IN-UTFARD:HIDDEN  IN FRAME {&FRAME-NAME} = TRUE 
      FILL-IN-UNAMN:HIDDEN = TRUE.
      FILL-IN-ATTEST:HIDDEN = TRUE.
      FILL-IN-PN:HIDDEN = TRUE.
   END.*/

         
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TIDPUNKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TIDPUNKT Dialog-Frame
ON LEAVE OF FILL-IN-TIDPUNKT IN FRAME Dialog-Frame /* Tidpunkt */
DO:
   ASSIGN
   FILL-IN-KONC = INPUT FILL-IN-KONC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
     
   
   FILL-IN-PROJEKT:LABEL = "ProjektID".
   
   IF Guru.Konstanter:globforetag = "kewa" OR Guru.Konstanter:globforetag = "gull" OR Guru.Konstanter:globforetag = "svkk" THEN  FILL-IN-PNAMN:LABEL = "Projektnamn/Ledningsnamn".

   IF faonr NE "" THEN DO:   
      RUN projextra3 IN nyttaoapph (INPUT faonr, INPUT fdelnr, OUTPUT ponr,OUTPUT kontaktvar).   
      IF ponr NE "" THEN DO:
         FILL-IN-PROJEKT = ponr.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "NATPROJ"                   
         inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN      
            FILL-IN-LEDLITT = extradatatemp.SOKCHAR[1] 
            FILL-IN-PNAMN = extradatatemp.SOKCHAR[2] 
            FILL-IN-PLED = extradatatemp.SOKCHAR[3]             
            FILL-IN-KONC = extradatatemp.SOKCHAR[4]
            FILL-IN-DIARIENR = extradatatemp.SOKCHAR[5]
            /*FILL-IN-DOMSAGA = extradatatemp.SOKCHAR[5] tidigare
            FILL-IN-UTFARD = extradatatemp.SOKDATE[1]    LEDIGA 
            FILL-IN-UNAMN = extradatatemp.SOKCHAR[6]     
            FILL-IN-ATTEST = extradatatemp.SOKCHAR[7]
            FILL-IN-PN = extradatatemp.SOKCHAR[8]*/
            FILL-IN-NAT = extradatatemp.SOKCHAR[9].
           
         END.
         
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "NATPROJ2"                   
         inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN      
            FILL-IN-PLATS = extradatatemp.SOKCHAR[1] 
            FILL-IN-TIDPUNKT = extradatatemp.SOKCHAR[2] 
            FILL-IN-ARTAL = extradatatemp.SOKCHAR[3].                          
         END.
         
         /*FILL-IN-PROJEKT = ponr.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "PROJHSAND"                   
         inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN      
            FILL-IN-GAMLED = extradatatemp.SOKCHAR[1] 
            FILL-IN-DODAS = extradatatemp.SOKCHAR[2] 
            FILL-IN-KRHA = extradatatemp.SOKINT[1]             
            FILL-IN-SKADEERS = extradatatemp.SOKINT[2]
            FILL-IN-TILLDAG = extradatatemp.SOKDATE[1]
            FILL-IN-TKMETER = extradatatemp.SOKCHAR[3]     
            FILL-IN-NSTN = extradatatemp.SOKINT[4].                 

         END.*/   
      END.
   END.
      
   {musarrow.i}
   RUN enable_UI.
      
   /*används ej  härnösand gamla mallar      
   ASSIGN 
   FILL-IN-GAMLED:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-DODAS:HIDDEN = TRUE 
   FILL-IN-KRHA:HIDDEN = TRUE             
   FILL-IN-SKADEERS:HIDDEN = TRUE
   FILL-IN-TILLDAG:HIDDEN = TRUE
   FILL-IN-TKMETER:HIDDEN = TRUE     
   FILL-IN-NSTN:HIDDEN = TRUE.*/
   ASSIGN  
   FILL-IN-KONC:HIDDEN = TRUE 
   FILL-IN-PLATS:HIDDEN = TRUE 
   FILL-IN-TIDPUNKT:HIDDEN = TRUE 
   FILL-IN-ARTAL:HIDDEN = TRUE
   FILL-IN-DIARIENR:HIDDEN = TRUE.   
           
   IF vama = "vard" THEN DO: 
     IF mall = 6 OR mall = 27 OR mall = 28 OR mall = 29 OR mall = 97 OR mall = 98 OR mall = 104 THEN DO:
        FILL-IN-KONC:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE.
     END.
     IF mall = 88 THEN DO:
        FILL-IN-DIARIENR:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE.
     END.
     IF mall = 52 THEN DO:
         /*Informationsbrev Vattenfall*/
         ASSIGN
         FILL-IN-PLATS:HIDDEN = FALSE 
         FILL-IN-TIDPUNKT:HIDDEN = FALSE 
         FILL-IN-ARTAL:HIDDEN = FALSE.
     END.
                                 
   END.   
   
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED FILL-IN-NAT 
          FILL-IN-KONC FILL-IN-DIARIENR FILL-IN-PLATS FILL-IN-TIDPUNKT 
          FILL-IN-ARTAL 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED FILL-IN-NAT 
         FILL-IN-KONC FILL-IN-DIARIENR FILL-IN-PLATS FILL-IN-TIDPUNKT 
         FILL-IN-ARTAL BTN_OK BTN_AVS 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

