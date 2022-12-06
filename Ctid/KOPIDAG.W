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
{TIDALLT.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sdatum AS DATE NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{TIDAPPDEF.I}
&Scoped-define NEW
{PERBEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{UPPGHMT.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
{OMRTEMPW.I}
{SOKDEF.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.


DEFINE VARIABLE frandat AS DATE NO-UNDO.
DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .91 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .91 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-DATUM FILL-IN-DATUM-2 BTN_REG ~
BTN_AVB BTN_NVE BTN_FVE BTN_NVE-2 BTN_FVE-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-PKOD ~
FILL-IN-MANAD FILL-IN-DAG FILL-IN-DAG-2 FILL-IN-FRDATUM FILL-IN-DAG-3 

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

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG-2 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG-3 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM-2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRDATUM AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Kopiera från datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-1 AT ROW 7.5 COL 20.5 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 8.83 COL 20.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DATUM-2 AT ROW 10.5 COL 20.5 COLON-ALIGNED AUTO-RETURN 
     BTN_REG AT ROW 12.5 COL 14.25
     BTN_AVB AT ROW 12.5 COL 29.25
     FILL-IN-PKOD AT ROW 2 COL 20.5 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 3.42 COL 20.5 COLON-ALIGNED NO-LABEL
     BTN_NVE AT ROW 8.5 COL 32
     BTN_FVE AT ROW 9.33 COL 32
     BTN_NVE-2 AT ROW 10.33 COL 32
     BTN_FVE-2 AT ROW 11.21 COL 32
     FILL-IN-DAG AT ROW 8.83 COL 33.25 COLON-ALIGNED NO-LABEL
     FILL-IN-DAG-2 AT ROW 10.5 COL 33.25 COLON-ALIGNED NO-LABEL
     FILL-IN-FRDATUM AT ROW 5.58 COL 20.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DAG-3 AT ROW 5.58 COL 24.5 COLON-ALIGNED NO-LABEL
     SPACE(9.99) SKIP(7.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopiera alla tidregistreringar från en dag till en eller flera andra dagar":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FRDATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kopiera alla tidregistreringar från en dag till en eller flera andra dagar */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kopiera alla tidregistreringar från en dag till en eller flera andra dagar */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM - 1.   
   IF FILL-IN-DATUM = 0 THEN FILL-IN-DATUM = 1.
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         FILL-IN-DATUM = DAY(tillochmeddatum + 1).
      END.
   END.   
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 - 1.   
   IF FILL-IN-DATUM-2 = 0 THEN FILL-IN-DATUM-2 = 1.
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
   END.      
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}. 
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM + 1.
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.         
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}. 
   IF FILL-IN-DATUM-2 < FILL-IN-DATUM THEN DO:
      ASSIGN
      FILL-IN-DATUM-2 = FILL-IN-DATUM
      FILL-IN-DAG-2 = FILL-IN-DAG.
      DISPLAY FILL-IN-DATUM-2 FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 + 1.
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM-2 = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* OK */
DO:
   {muswait.i} 
   ASSIGN
   FILL-IN-FRDATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-FRDATUM
   FILL-IN-DATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM
   FILL-IN-DATUM-2 = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM-2.   
   
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 < FILL-IN-DATUM THEN DO:
      MESSAGE "Felaktigt angivet datum."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara " DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara " DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kann ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END. 
   IF FILL-IN-DATUM-2 <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kann ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.

   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.
   RUN anst_UI.   
   frandat = DATE(regmnr,FILL-IN-FRDATUM,regar).
   FILL-IN-STARTDAT = DATE(regmnr,FILL-IN-DATUM,regar).
   FILL-IN-STOPPDAT = DATE(regmnr,FILL-IN-DATUM-2,regar).
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KOPDAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:globanv,INPUT personaltemp.PERSONALKOD,INPUT frandat,INPUT FILL-IN-STARTDAT,
      INPUT FILL-IN-STOPPDAT,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN KOPDAPP.P  
      (INPUT Guru.Konstanter:globanv,INPUT personaltemp.PERSONALKOD,INPUT frandat,INPUT FILL-IN-STARTDAT,
      INPUT FILL-IN-STOPPDAT,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      FOR EACH felmeddtemp:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
      END.
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* OK */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Start datum */
DO:  
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.            
   IF MONTH(regdatum) = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE((regmnr + 1),01,regar) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med "
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                              
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM-2 DIALOG-1
ON LEAVE OF FILL-IN-DATUM-2 IN FRAME DIALOG-1 /* Slut datum */
DO:  
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.            
   IF MONTH(regdatum) = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE((regmnr + 1),01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM-2 <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med "
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                                                            
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).      
   RUN REGDAG.P.   
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FRDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRDATUM DIALOG-1
ON LEAVE OF FILL-IN-FRDATUM IN FRAME DIALOG-1 /* Kopiera från datum */
DO:  
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.            
   IF MONTH(regdatum) = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE((regmnr + 1),01,regar) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med "
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                              
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
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
   
   
   RUN REGVEC.P.
   RUN REGDAG.P.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.   
   RUN anst_UI. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.   
   RUN grundtid_UI.   
   FILL-IN-MANAD = regmannamn.
   SESSION:DATA-ENTRY-RETURN = TRUE.
   FILL-IN-MANAD = regmannamn.

   RUN enable_UI.          
   {FRMSIZED.I}    
   {musarrow.i}        
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI DIALOG-1 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.       
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
  DISPLAY FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-PKOD FILL-IN-MANAD FILL-IN-DAG 
          FILL-IN-DAG-2 FILL-IN-FRDATUM FILL-IN-DAG-3 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-DATUM FILL-IN-DATUM-2 BTN_REG BTN_AVB BTN_NVE BTN_FVE 
         BTN_NVE-2 BTN_FVE-2 
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
   ASSIGN FILL-IN-PKOD = personaltemp.PERSONALKOD.
   IF sdatum NE ? THEN DO:   
      ASSIGN
      FILL-IN-FRDATUM = DAY(sdatum)
      FILL-IN-DATUM = DAY(sdatum + 1)
      FILL-IN-DATUM-2 = DAY(sdatum + 1).
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-FRDATUM = 1
      FILL-IN-DATUM = 1
      FILL-IN-DATUM-2 = 1.
   END.   
   IF tillochmeddatum NE ? THEN DO:
      IF sdatum LE tillochmeddatum  THEN DO:      
         ASSIGN
         FILL-IN-FRDATUM = DAY(sdatum)
         FILL-IN-DATUM = DAY(tillochmeddatum + 1)
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
      IF sdatum = ? THEN DO:      
         ASSIGN
         FILL-IN-FRDATUM = DAY(tillochmeddatum + 1)
         FILL-IN-DATUM = DAY(tillochmeddatum + 1)
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
   END.
   regdatum = DATE((regmnr),FILL-IN-FRDATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".   
   FILL-IN-DAG-3 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-3 WITH FRAME {&FRAME-NAME}.      

   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

