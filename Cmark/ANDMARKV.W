&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETE marknrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER felnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER  hjpersonummer AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

{REGVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE markrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE andmarkvapph AS HANDLE NO-UNDO.
DEFINE VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollpnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE marknamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokhitta AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokenamn AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_PERSONNUMMER FILL-IN_MARKAGARE ~
FILL-IN_PNR2 FILL-IN_MAKA FILL-IN_GATUADRESS FILL-IN_POSTNUMMER ~
FILL-IN_POSTADRESS FILL-IN_BOXEN FILL-IN_TELEFON FILL-IN_TELEFON2 ~
BTN_SOKHITTA FILL-IN_TELEFONM FILL-IN_MAIL FILL-IN-BANKKONTO ~
FILL-IN-BANKNAMN BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_MARKNR FILL-IN_PERSONNUMMER ~
FILL-IN_MARKAGARE FILL-IN_PNR2 FILL-IN_MAKA FILL-IN_GATUADRESS ~
FILL-IN_POSTNUMMER FILL-IN_POSTADRESS FILL-IN_BOXEN FILL-IN_TELEFON ~
FILL-IN_TELEFON2 FILL-IN_TELEFONM FILL-IN_MAIL FILL-IN-BANKKONTO ~
FILL-IN-BANKNAMN 

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

DEFINE BUTTON BTN_SOKHITTA 
     LABEL "Sök telefonnummer" 
     SIZE 12 BY 1 TOOLTIP "Sök telefonnummer via Internet med markägare och adress".

DEFINE VARIABLE FILL-IN-BANKKONTO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bankkonto/Bankgiro" 
     VIEW-AS FILL-IN 
     SIZE 16.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BANKNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Banknamn" 
     VIEW-AS FILL-IN 
     SIZE 17.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BOXEN AS CHARACTER FORMAT "x(8)" 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_GATUADRESS AS CHARACTER FORMAT "X(256)" 
     LABEL "Gatuadress" 
     VIEW-AS FILL-IN 
     SIZE 40.25 BY 1.

DEFINE VARIABLE FILL-IN_MAIL AS CHARACTER FORMAT "x(50)" 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.

DEFINE VARIABLE FILL-IN_MAKA AS CHARACTER FORMAT "X(50)" 
     LABEL "Markägare maka/make" 
     VIEW-AS FILL-IN 
     SIZE 40.25 BY 1.

DEFINE VARIABLE FILL-IN_MARKAGARE AS CHARACTER FORMAT "X(50)" 
     LABEL "Markägare" 
     VIEW-AS FILL-IN 
     SIZE 40.25 BY 1.

DEFINE VARIABLE FILL-IN_MARKNR AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Markägarnummer" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_PERSONNUMMER AS CHARACTER FORMAT "XXXXXX-XXXX" INITIAL "0000000000" 
     LABEL "Personnummer/Org.nr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_PNR2 AS CHARACTER FORMAT "XXXXXX-XXXX" INITIAL "0000000000" 
     LABEL "Personnummer maka/make" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_POSTADRESS AS CHARACTER FORMAT "X(256)" 
     LABEL "Postadress" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN_POSTNUMMER AS CHARACTER FORMAT "XXX XX" 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_TELEFON AS CHARACTER FORMAT "x(11)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_TELEFON2 AS CHARACTER FORMAT "x(30)" 
     LABEL "Telefon arb" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.

DEFINE VARIABLE FILL-IN_TELEFONM AS CHARACTER FORMAT "x(30)" 
     LABEL "Telefon mobil" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_MARKNR AT ROW 1.75 COL 24.38 COLON-ALIGNED
     FILL-IN_PERSONNUMMER AT ROW 2.88 COL 24.38 COLON-ALIGNED
     FILL-IN_MARKAGARE AT ROW 4 COL 24.38 COLON-ALIGNED
     FILL-IN_PNR2 AT ROW 5.13 COL 24.38 COLON-ALIGNED
     FILL-IN_MAKA AT ROW 6.25 COL 24.38 COLON-ALIGNED
     FILL-IN_GATUADRESS AT ROW 7.38 COL 24.38 COLON-ALIGNED
     FILL-IN_POSTNUMMER AT ROW 8.5 COL 24.38 COLON-ALIGNED
     FILL-IN_POSTADRESS AT ROW 9.63 COL 24.38 COLON-ALIGNED
     FILL-IN_BOXEN AT ROW 10.75 COL 24.38 COLON-ALIGNED
     FILL-IN_TELEFON AT ROW 11.88 COL 24.38 COLON-ALIGNED HELP
          "HEMTELE"
     FILL-IN_TELEFON2 AT ROW 13 COL 24.38 COLON-ALIGNED HELP
          "ARB.TELE"
     BTN_SOKHITTA AT ROW 14 COL 58.5 WIDGET-ID 6
     FILL-IN_TELEFONM AT ROW 14.13 COL 24.38 COLON-ALIGNED HELP
          "ARB.TELE"
     FILL-IN_MAIL AT ROW 15.25 COL 24.38 COLON-ALIGNED HELP
          "ARB.TELE"
     FILL-IN-BANKKONTO AT ROW 16.38 COL 24.38 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-BANKNAMN AT ROW 17.5 COL 24.38 COLON-ALIGNED WIDGET-ID 4
     BTN_REG AT ROW 20.54 COL 41.25
     BTN_AVS AT ROW 20.54 COL 56.25
     SPACE(0.62) SKIP(0.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny/Ändra markägare":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MARKNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ny/Ändra markägare */
DO:
   IF vart = "NYA" THEN RETURN NO-APPLY.
   APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ny/Ändra markägare */
DO:
   IF vart = "NYA" THEN RETURN NO-APPLY.
   APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:

   IF vart = "NYA" THEN DO:
      RUN bortmarknr IN andmarkvapph (INPUT  FILL-IN_MARKNR).     
   END.   
   APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON GO OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   IF VALID-HANDLE(andmarkvapph) THEN DELETE PROCEDURE andmarkvapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = TRUE.
   RETURN.
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
   FILL-IN_MARKNR = INPUT FILL-IN_MARKNR 
   FILL-IN_MARKAGARE = INPUT FILL-IN_MARKAGARE 
   FILL-IN_PERSONNUMMER = INPUT FILL-IN_PERSONNUMMER  
   FILL-IN_MAKA = INPUT FILL-IN_MAKA 
   FILL-IN_PNR2 = INPUT FILL-IN_PNR2
   FILL-IN_GATUADRESS = INPUT FILL-IN_GATUADRESS 
   FILL-IN_POSTADRESS = INPUT FILL-IN_POSTADRESS 
   FILL-IN_POSTNUMMER = INPUT FILL-IN_POSTNUMMER 
   FILL-IN_BOXEN = INPUT FILL-IN_BOXEN 
   FILL-IN_TELEFON = INPUT FILL-IN_TELEFON 
   FILL-IN_TELEFON2 = INPUT FILL-IN_TELEFON2
   FILL-IN_TELEFONM = INPUT FILL-IN_TELEFONM
   FILL-IN_MAIL = INPUT FILL-IN_MAIL
   FILL-IN-BANKKONTO = INPUT FILL-IN-BANKKONTO
   FILL-IN-BANKNAMN = INPUT FILL-IN-BANKNAMN.
   IF FILL-IN_MARKNR = 0 THEN DO:
      MESSAGE "Marknumret kan inte vara blank!" VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_MARKNR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   
   IF Guru.Konstanter:globforetag = "VAST"  THEN DO:
      IF FILL-IN_PERSONNUMMER = " " OR FILL-IN_PERSONNUMMER = "0000000000 " THEN DO:
         MESSAGE "Personnummer kan inte vara blankt" VIEW-AS ALERT-BOX.   
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   /*
   IF vart = "NYA" THEN DO:
      /*HÄMTAR FELMEDTEMP*/
      RUN felkoll_UI.      
      /*GER DIG VAL*/
      RUN felkollnr_UI.            
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.      
   END.
   */
   IF FILL-IN_MAIL NE "" THEN DO:
      RUN epostkoll_UI (INPUT FILL-IN_MAIL,OUTPUT musz).
      IF musz = TRUE  THEN DO:         
         RETURN NO-APPLY.      
      END.
      musz = FALSE.
   END.
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markagaretemp THEN DO:
      CREATE markagaretemp.
   END.
   ASSIGN 
   markagaretemp.MARKNR = FILL-IN_MARKNR
   markagaretemp.MARKAGARE = FILL-IN_MARKAGARE
   markagaretemp.PERSONNUMMER = FILL-IN_PERSONNUMMER  
   markagaretemp.PNR2 = FILL-IN_PNR2
   markagaretemp.GATUADRESS = FILL-IN_GATUADRESS
   markagaretemp.POSTADRESS = FILL-IN_POSTADRESS     
   markagaretemp.POSTNUMMER = FILL-IN_POSTNUMMER
   markagaretemp.BOXEN = FILL-IN_BOXEN
   markagaretemp.TELEFON = FILL-IN_TELEFON  
   markagaretemp.TELEFON2 = FILL-IN_TELEFON2.     
   ASSIGN marknrvar = markagaretemp.MARKNR.
   RUN btnok IN andmarkvapph (INPUT markagaretemp.MARKNR,
                              INPUT TABLE markagaretemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "MARKAG"                   
   inextradatatemp.HUVUDINT = markagaretemp.MARKNR
   inextradatatemp.SOKCHAR[1] = FILL-IN_TELEFONM
   inextradatatemp.SOKCHAR[2] = FILL-IN_MAIL
   inextradatatemp.SOKCHAR[3] = FILL-IN_MAKA
   inextradatatemp.SOKCHAR[4] = FILL-IN-BANKKONTO
   inextradatatemp.SOKCHAR[5] = FILL-IN-BANKNAMN.
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(andmarkvapph) THEN DELETE PROCEDURE andmarkvapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SOKHITTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SOKHITTA DIALOG-1
ON CHOOSE OF BTN_SOKHITTA IN FRAME DIALOG-1 /* Sök telefonnummer */
DO:
   ASSIGN         
   FILL-IN_MARKAGARE = INPUT FILL-IN_MARKAGARE    
   FILL-IN_GATUADRESS = INPUT FILL-IN_GATUADRESS 
   FILL-IN_POSTADRESS = INPUT FILL-IN_POSTADRESS
   sokenamn = INDEX(FILL-IN_MARKAGARE,",").
   IF sokenamn > 0 THEN sokhitta =  SUBSTRING(FILL-IN_MARKAGARE,1,(sokenamn - 1)) + " " +   FILL-IN_GATUADRESS + " " + FILL-IN_POSTADRESS.
   ELSE sokhitta =  FILL-IN_MARKAGARE + " " +   FILL-IN_GATUADRESS + " " + FILL-IN_POSTADRESS.       
   {HITTALANK.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_MAIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MAIL DIALOG-1
ON LEAVE OF FILL-IN_MAIL IN FRAME DIALOG-1 /* E-post */
DO:
   FILL-IN_MAIL = INPUT FILL-IN_MAIL.                                                             
   IF FILL-IN_MAIL NE "" THEN DO:                                                                  .
      musz = FALSE.                                                                                 
      RUN EPOSTKOLL.P (INPUT FILL-IN_MAIL,OUTPUT musz).                                            
      IF musz = FALSE THEN DO:                                                                      
         APPLY "ENTRY" TO FILL-IN_MAIL IN FRAME {&FRAME-NAME}.                                     
         RETURN NO-APPLY.                                                                           
      END.
      musz = FALSE.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PERSONNUMMER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PERSONNUMMER DIALOG-1
ON LEAVE OF FILL-IN_PERSONNUMMER IN FRAME DIALOG-1 /* Personnummer/Org.nr */
DO:
   
   kollpnr = INPUT FILL-IN_PERSONNUMMER.
   IF SUBSTRING(kollpnr,1,6) = "" THEN DO:
      ASSIGN SUBSTRING(kollpnr,1,6) = "000000".         
   END.
   IF SUBSTRING(kollpnr,7,4) = "" THEN DO:
      ASSIGN SUBSTRING(kollpnr,7,4) = "0000".      
   END.
   IF SUBSTRING(kollpnr,1,6) > "999999" OR SUBSTRING(kollpnr,1,6) < "000000" THEN DO:
      MESSAGE "Ickenumeriska tecken får ej förekomma i personnumret" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF SUBSTRING(kollpnr,7,4) > "9999" OR SUBSTRING(kollpnr,7,4) < "0000" THEN DO:
      MESSAGE "Ickenumeriska tecken får ej förekomma i personnumret" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   FILL-IN_PERSONNUMMER = kollpnr.
   DISPLAY FILL-IN_PERSONNUMMER WITH FRAME {&FRAME-NAME}.
   IF vart = "NYA" THEN DO:
      RUN felkoll_UI.            
      RUN felkollnr_UI.            
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PNR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PNR2 DIALOG-1
ON LEAVE OF FILL-IN_PNR2 IN FRAME DIALOG-1 /* Personnummer maka/make */
DO:
   kollpnr = INPUT FILL-IN_PNR2.
   IF SUBSTRING(kollpnr,1,6) = "" THEN DO:
      ASSIGN SUBSTRING(kollpnr,1,6) = "000000".         
   END.
   IF SUBSTRING(kollpnr,7,4) = "" THEN DO:
      ASSIGN SUBSTRING(kollpnr,7,4) = "0000".      
   END.
   IF SUBSTRING(kollpnr,1,6) > "999999" OR SUBSTRING(kollpnr,1,6) < "000000" THEN DO:
      MESSAGE "Ickenumeriska tecken får ej förekomma i personnumret" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF SUBSTRING(kollpnr,7,4) > "9999" OR SUBSTRING(kollpnr,7,4) < "0000" THEN DO:
      MESSAGE "Ickenumeriska tecken får ej förekomma i personnumret" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   FILL-IN_PNR2 = kollpnr.
   DISPLAY FILL-IN_PNR2 WITH FRAME {&FRAME-NAME}.   
   IF vart = "NYA" THEN DO:
      RUN felkoll_UI.     
      RUN felkollnr_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
   END.
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
   {ALLSTARTDYN.I}
/*   FIND VARDERING WHERE RECID(VARDERING) = kalkrec NO-LOCK NO-ERROR.*/   
   RUN grundtid_UI. 
   RUN enable_UI.       
   {FRMSIZED.I}   
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANDMARKVAPP.P PERSISTENT SET andmarkvapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANDMARKVAPP.P PERSISTENT SET andmarkvapph.
   END.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
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
  DISPLAY FILL-IN_MARKNR FILL-IN_PERSONNUMMER FILL-IN_MARKAGARE FILL-IN_PNR2 
          FILL-IN_MAKA FILL-IN_GATUADRESS FILL-IN_POSTNUMMER FILL-IN_POSTADRESS 
          FILL-IN_BOXEN FILL-IN_TELEFON FILL-IN_TELEFON2 FILL-IN_TELEFONM 
          FILL-IN_MAIL FILL-IN-BANKKONTO FILL-IN-BANKNAMN 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_PERSONNUMMER FILL-IN_MARKAGARE FILL-IN_PNR2 FILL-IN_MAKA 
         FILL-IN_GATUADRESS FILL-IN_POSTNUMMER FILL-IN_POSTADRESS FILL-IN_BOXEN 
         FILL-IN_TELEFON FILL-IN_TELEFON2 BTN_SOKHITTA FILL-IN_TELEFONM 
         FILL-IN_MAIL FILL-IN-BANKKONTO FILL-IN-BANKNAMN BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epostkoll_UI DIALOG-1 
PROCEDURE epostkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ep AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER fel AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE okepost AS LOGICAL    NO-UNDO.
   fel = TRUE.
   ep = TRIM(ep).
   RUN EPOSTKOLL.P (INPUT ep,OUTPUT okepost).
   IF okepost = TRUE THEN fel = FALSE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felkollnr_UI DIALOG-1 
PROCEDURE felkollnr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {musarrow.i}           
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 OR felmeddtemp.VAL = 2 OR felmeddtemp.VAL = 3 OR felmeddtemp.VAL = 4 THEN DO:           
         RUN MARKAGAREFEL.W (OUTPUT felnr).
         IF felnr = 1 THEN DO:
            musz = FALSE.
            DELETE felmeddtemp.
         END.
         IF felnr = 2 THEN DO:
            musz = TRUE. 
            DELETE felmeddtemp.
            APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME {&FRAME-NAME}.
            RETURN.
         END.      
         IF felnr = 3 THEN DO:
            
            IF felmeddtemp.VAL = 1 THEN DO:
               ASSIGN
               felnr = 3
               hjpersonummer = FILL-IN_PERSONNUMMER.
            END.
            IF felmeddtemp.VAL = 2 THEN DO:
               ASSIGN
               felnr = 3
               hjpersonummer = FILL-IN_PNR2.
            END.
            IF felmeddtemp.VAL = 3 THEN DO:
               ASSIGN
               felnr = 33
               hjpersonummer = FILL-IN_PERSONNUMMER.
            END.
            IF felmeddtemp.VAL = 4 THEN DO:
               ASSIGN
               felnr = 33
               hjpersonummer = FILL-IN_PNR2.
            END.
            DELETE felmeddtemp.
            RUN bortmarknr IN andmarkvapph (INPUT  FILL-IN_MARKNR).     
            APPLY "GO" TO BTN_AVS. 
            RETURN.
         END.
         IF felnr = 4 THEN DO:
            musz = TRUE.
            DELETE felmeddtemp.
            RUN bortmarknr IN andmarkvapph (INPUT  FILL-IN_MARKNR). 
            APPLY "GO" TO BTN_AVS.
            RETURN.
         END.
      END.
      /*ELSE IF felmeddtemp.VAL = 2 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME {&FRAME-NAME}. 
         musz = TRUE.
         RETURN.
      END.      */
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felkoll_UI DIALOG-1 
PROCEDURE felkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN kontroll IN andmarkvapph (INPUT marknrvar,INPUT FILL-IN_PERSONNUMMER,
                                    INPUT FILL-IN_PNR2,OUTPUT TABLE felmeddtemp).

   
   
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
     RUN hmtmarknr IN andmarkvapph (OUTPUT marknrvar).     
     ASSIGN  
     FILL-IN_MARKNR = marknrvar.       
   END.   
   ELSE DO:
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar USE-INDEX MARKNR NO-LOCK NO-ERROR.    
      IF AVAILABLE markagaretemp THEN DO:
         
         RUN hamtmarknamn IN andmarkvapph (INPUT  marknrvar,OUTPUT marknamn).   
         ASSIGN
         FILL-IN_MARKNR = markagaretemp.MARKNR
         FILL-IN_MARKAGARE =  marknamn
         FILL-IN_PERSONNUMMER = markagaretemp.PERSONNUMMER
         FILL-IN_PNR2 = markagaretemp.PNR2
         FILL-IN_GATUADRESS = markagaretemp.GATUADRESS
         FILL-IN_POSTNUMMER = markagaretemp.POSTNUMMER 
         FILL-IN_POSTADRESS = markagaretemp.POSTADRESS
         FILL-IN_BOXEN = markagaretemp.BOXEN
         FILL-IN_TELEFON = markagaretemp.TELEFON 
         FILL-IN_TELEFON2 = markagaretemp.TELEFON2.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "MARKAG"                   
         inextradatatemp.HUVUDINT = markagaretemp.MARKNR.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN
            FILL-IN_TELEFONM = extradatatemp.SOKCHAR[1]
            FILL-IN_MAIL = extradatatemp.SOKCHAR[2]
            FILL-IN_MAKA = extradatatemp.SOKCHAR[3]
            FILL-IN-BANKKONTO = extradatatemp.SOKCHAR[4]
            FILL-IN-BANKNAMN = extradatatemp.SOKCHAR[5].
         END. 
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

