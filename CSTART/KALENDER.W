&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

  Created: 06/17/96 - 10:49 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE bdatum AS DATE NO-UNDO.
DEFINE VARIABLE avdatum AS DATE NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE ye AS INTEGER FORMAT "9999" NO-UNDO.
DEFINE VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE day AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE forstaregvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE andraregvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE regvnr1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE regvnr2 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE vnr1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE vnr2 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE antal AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.
DEFINE VARIABLE nyladdning AS LOGICAL NO-UNDO.
DEFINE VARIABLE vecknumm AS INTEGER FORMAT "999" NO-UNDO. 
DEFINE VARIABLE vnr4 AS INTEGER NO-UNDO.  
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.          
DEFINE VARIABLE regvnrspar AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE skillnad AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE tal AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE dar AS INTEGER FORMAT "9999" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_AR CMB_MANAD CMB_VNR BTN_KLAR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_AR CMB_MANAD CMB_VNR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL "januari" 
     LABEL "Månad" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VNR AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Aktuella veckonr" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     CMB_AR AT ROW 1.67 COL 6.88
     CMB_MANAD AT ROW 3.67 COL 11.88 COLON-ALIGNED
     CMB_VNR AT ROW 5.67 COL 1.38
     BTN_KLAR AT ROW 7.04 COL 2.25
     BTN_AVB AT ROW 7.04 COL 17.25
     SPACE(0.49) SKIP(0.33)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kalender".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AR IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_VNR IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kalender */
DO:
   regvnr = forstaregvnr.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kalender */
DO:
   regvnr = forstaregvnr.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   regvnr = forstaregvnr.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR DIALOG-1
ON CHOOSE OF BTN_KLAR IN FRAME DIALOG-1 /* Ok */
DO:
   regvnr = INPUT CMB_VNR. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR DIALOG-1
ON LEAVE OF CMB_AR IN FRAME DIALOG-1 /* Årtal */
DO:
   CMB_AR = INPUT CMB_AR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR DIALOG-1
ON VALUE-CHANGED OF CMB_AR IN FRAME DIALOG-1 /* Årtal */
DO:
   CMB_AR = INPUT CMB_AR.
   RUN laddavnr_UI. /*LADDAR VECKONR. I CMB_VNR*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-1
ON LEAVE OF CMB_MANAD IN FRAME DIALOG-1 /* Månad */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-1
ON VALUE-CHANGED OF CMB_MANAD IN FRAME DIALOG-1 /* Månad */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
   RUN laddavnr_UI. /*LADDAR VECKONR. I CMB_VNR*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VNR DIALOG-1
ON LEAVE OF CMB_VNR IN FRAME DIALOG-1 /* Aktuella veckonr */
DO:
   ASSIGN
   CMB_VNR = INPUT CMB_VNR
   regvnr = CMB_VNR.
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
   RUN VECODAT.P.
   forstaregvnr = regvnr.  
   ASSIGN  /*LADDAR ÅR I CMB_AR*/
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) - 3,"9999"))  
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) - 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) - 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum),"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) + 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) + 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(regdatum) + 3,"9999")).
   ASSIGN
   status-ok = CMB_AR:DELETE("0")
   CMB_AR:SCREEN-VALUE = STRING(YEAR(regdatum),"9999")
   CMB_AR = INPUT CMB_AR.
   RUN manad_UI. /*LADDAR MÅNADER I CMB_MANAD*/
   RUN laddavnr_UI. /*LADDAR VECKONR. I CMB_VNR*/
   RUN enable_UI.       
   {FRMSIZED.I} 
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY CMB_AR CMB_MANAD CMB_VNR 
      WITH FRAME DIALOG-1.
  ENABLE CMB_AR CMB_MANAD CMB_VNR BTN_KLAR BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddavnr_UI DIALOG-1 
PROCEDURE laddavnr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   nyladdning = TRUE.
   antal = 6.
   REPEAT WHILE antal > 0: 
      ASSIGN     
      status-ok = CMB_VNR:DELETE(antal)IN FRAME {&FRAME-NAME}.     
      antal = antal - 1.
   END.
   ye = CMB_AR.
   IF CMB_MANAD = STRING("januari") THEN DO:
      man = 01.
   END.
   ELSE IF CMB_MANAD = STRING("februari")THEN DO:
      man = 02.
   END.
   ELSE IF CMB_MANAD = STRING("mars")THEN DO:
      man = 03.
   END.
   ELSE IF CMB_MANAD = STRING("april")THEN DO:
      man = 04.
   END.
   ELSE IF CMB_MANAD = STRING("maj")THEN DO:
      man = 05.
   END.
   ELSE IF CMB_MANAD = STRING("juni")THEN DO:
      man = 06.
   END.
   ELSE IF CMB_MANAD = STRING("juli")THEN DO:
      man = 07.
   END.
   ELSE IF CMB_MANAD = STRING("augusti")THEN DO:
      man = 08.
   END.
   ELSE IF CMB_MANAD = STRING("september")THEN DO:
      man = 09.
   END.
   ELSE IF CMB_MANAD = STRING("oktober")THEN DO:
      man = 10.
   END.
   ELSE IF CMB_MANAD = STRING("november")THEN DO:
      man = 11.
   END.
   ELSE IF CMB_MANAD = STRING("december")THEN DO:
      man = 12.
   END.
   regdatum = DATE(man,01,ye).
   RUN REGVEC.P. 
   ASSIGN             
   vnr1 = regvnr     
   bdatum = regdatum.
   IF CMB_MANAD = STRING("januari") OR CMB_MANAD = STRING("mars") OR 
      CMB_MANAD = STRING("maj") OR CMB_MANAD = STRING("juli") OR 
      CMB_MANAD = STRING("augusti") OR CMB_MANAD = STRING("oktober") OR 
      CMB_MANAD = STRING("december") THEN DO:
      day = 31. 
   END.   
   ELSE IF CMB_MANAD = STRING("februari") THEN DO:
      regdatum2 = DATE(man,01,ye) + 28.
      IF (SUBSTRING(STRING(regdatum2,"99999999"),5,2)) = "02" THEN DO:
         day = 29.
      END.   
      ELSE DO:
         day = 28.
      END.   
   END.           
   ELSE DO:
      day = 30.
   END.
   regdatum = DATE(man,day,ye).
   RUN REGVEC.P.     
   vnr2 = regvnr.
   ASSIGN  
   avdatum = regdatum
   regdatum = bdatum.
   IF WEEKDAY(regdatum) = 1 THEN regdatum = regdatum - 6.          /*sön*/
   ELSE IF WEEKDAY(regdatum) = 2 THEN regdatum = regdatum - 0.          /*mån*/
   ELSE IF WEEKDAY(regdatum) = 3 THEN regdatum = regdatum - 1.        /*tis*/
   ELSE IF WEEKDAY(regdatum) = 4 THEN regdatum = regdatum - 2.         /*ons*/
   ELSE IF WEEKDAY(regdatum) = 5 THEN regdatum = regdatum - 3.         /*tor*/
   ELSE IF WEEKDAY(regdatum) = 6 THEN regdatum = regdatum - 4.          /*fre*/
   ELSE IF WEEKDAY(regdatum) = 7 THEN regdatum = regdatum - 5.      /*lör*/    
   REPEAT WHILE regdatum <= avdatum:
      RUN REGVEC.P.
      /*IF regvnr = vnr2 THEN LEAVE.*/
      ASSIGN
      status-ok = CMB_VNR:ADD-LAST(STRING(regvnr),"999")IN FRAME {&FRAME-NAME} NO-ERROR. 
      regdatum = regdatum + 7.
   END.                            
   ASSIGN
   status-ok = CMB_VNR:DELETE("0")
   CMB_VNR:SCREEN-VALUE = STRING(vnr1)
   CMB_VNR = INPUT CMB_VNR.
                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manad_UI DIALOG-1 
PROCEDURE manad_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF MONTH(regdatum) = 01 THEN DO:
      CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("januari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.   
   ELSE IF MONTH(regdatum) = 02 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("februari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(regdatum) = 03 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("mars").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(regdatum) = 04 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("april").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(regdatum) = 05 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("maj").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(regdatum) = 06 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juni").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(regdatum) = 07 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juli").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(regdatum) = 08 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("augusti").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(regdatum) = 09 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("september").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(regdatum) = 10 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("oktober").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(regdatum) = 11 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("november").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(regdatum) = 12 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("december").
      CMB_MANAD = INPUT CMB_MANAD.
   END.       
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE veckocheck_UI DIALOG-1 
PROCEDURE veckocheck_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*DENNA PROCEDUR ANVÄNS EJ LÄNGRE DÅ DO WHILE REGDATUM <= AVDATUM*/
/*KOLL SA ATT VECKONUMMER AR RIMLIGT */ 
/*FÖR ATT FÅ RÄTT START DECAD*/
/*
   REPEAT WHILE vnr1 <= vnr2:
      RUN veckocheck_UI.
      IF musz = TRUE THEN DO:
         skillnad = vnr2 - vnr1.
         IF man = 01 THEN DO:            
            ASSIGN            
            tal = INTEGER(SUBSTRING(STRING(vnr2,"999"),3 ,1))
            tal = tal - 1.
            IF INTEGER(SUBSTRING(STRING(vnr2,"999"),3 ,1)) > 1 THEN DO:   
               vnr1 = vnr1 + skillnad - TAL.
            END. 
         END.     
         IF man = 12 THEN DO:
            vnr1 = vnr1 + skillnad.
         END.      
      END.
      ELSE DO:   
         ASSIGN
         status-ok = CMB_VNR:ADD-LAST(STRING(vnr1),"999")IN FRAME {&FRAME-NAME}.      
         IF vnr1 = forstaregvnr THEN DO:
            ASSIGN
            status-ok = CMB_VNR:DELETE("0")
            CMB_VNR:SCREEN-VALUE = STRING(vnr1)
            CMB_VNR = INPUT CMB_VNR
            nyladdning = FALSE.
         END.
         IF nyladdning = TRUE THEN DO:
            ASSIGN
            status-ok = CMB_VNR:DELETE("0")
            CMB_VNR:SCREEN-VALUE = STRING(andraregvnr)
            CMB_VNR = INPUT CMB_VNR.
         END.
         vnr1 = vnr1 + 1.        
      END.
   END. */  
IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 9 THEN DO:
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 0 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) + 10.      
   END.
   ELSE DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END. 
ELSE IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 0 THEN DO:
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 9 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) - 10.      
   END.
   ELSE DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END.   
ELSE DO:
   dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
         INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
END. 
musz = FALSE.
IF SUBSTRING(STRING(vnr1,"999"),2 ,2) = "00" THEN DO:    
   musz = TRUE.
END.
IF SUBSTRING(STRING(vnr1,"999"),2 ,2) > "53" THEN DO:    
   musz = TRUE.
END.
ASSIGN
regvnr = vnr1
regvnrspar = regvnr
regdatumspar = regdatum
vecknumm = regvnr
vnr4 = INTEGER(SUBSTRING(STRING(vecknumm,"999"),1 ,1))
vnr4 = vnr4 + dar
regdatum = DATE(12,31,vnr4).
RUN REGVEC.P. /*GÖR OM REGDATUM TILL REGVNR*/
IF SUBSTRING(STRING(vecknumm,"999"),2 ,2) = "53" THEN DO:
   IF SUBSTRING(STRING(vecknumm,"999"),2 ,2) NE
   SUBSTRING(STRING(regvnr,"999"),2 ,2) THEN DO:
      musz = TRUE.
      RETURN.
   END.
END.      
IF regvnr < vecknumm THEN DO:
   ASSIGN 
   regvnr = regvnrspar
   regdatum = regdatumspar
   musz = TRUE.
   RETURN.   
END.
regdatum = DATE(01,01,vnr4).
RUN REGVEC.P. /*GÖR OM REGDATUM TILL REGVNR*/
IF regvnr > vecknumm THEN DO:
   ASSIGN
   regvnr = regvnrspar
   regdatum = regdatumspar 
   musz = TRUE.
END.
ASSIGN                       
regvnr = regvnrspar
regdatum = regdatumspar
vnr1 = regvnr.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

