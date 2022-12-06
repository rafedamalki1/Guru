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

  Created: 04/15/97 -  9:24 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED   
{MARKVARD.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE markvh AS HANDLE NO-UNDO.
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_basbelopp FILL-IN_FAKTORFS ~
FILL-IN_MOMS FILL-IN_IPVARDE FILL-IN_PRISTIDPUNKT FILL-IN-FBELOPP ~
FILL-IN-ZON1 FILL-IN-ZON2 FILL-IN-MOM2P1 FILL-IN-NOINLAND ~
FILL-IN-NOKUSTLANDEJG FILL-IN-TILLV3 FILL-IN-TILLV4A FILL-IN-TILLV4B BTN_OK ~
BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_basbelopp FILL-IN_FAKTORFS ~
FILL-IN_MOMS FILL-IN_IPVARDE FILL-IN_PRISTIDPUNKT FILL-IN-FBELOPP ~
FILL-IN-ZON1 FILL-IN-ZON2 FILL-IN-MOM2P1 FILL-IN-NOINLAND ~
FILL-IN-NOKUSTLANDEJG FILL-IN-TILLV3 FILL-IN-TILLV4A FILL-IN-TILLV4B 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-FBELOPP AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Rev fast belopp" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-GOTALAND AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Götaland" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOM2P1 AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Moment 2 punkt 1" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN-NOINLAND AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Norrlands Inland" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NOKUSTLAND AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Norrlands Kustland" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NOKUSTLANDEJG AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Norrlands Kustland ej Gävleborgs län" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SVEALAND AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Svealand" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLV3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Tillväxtområde 3 (2009 års skogsnorm)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLV4A AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Tillväxtområde 4A (2009 års skogsnorm)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLV4B AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Tillväxtområde 4B (2009 års skogsnorm)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ZON1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Rev Zon 1 kr/m" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ZON2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Rev Zon 2 kr/m" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_basbelopp AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "1 % av basbelopp" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_FAKTORFS AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Faktor förenklad skogsnorm Svealand" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_IPVARDE AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Index Ip" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_ITVARDE AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Index It" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_MOMS AS DECIMAL FORMAT "->>>>9.999" INITIAL 0 
     LABEL "Moms" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FILL-IN_PRISTIDPUNKT AS CHARACTER FORMAT "X(8)" 
     LABEL "Pristidpunkt" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_basbelopp AT ROW 1.25 COL 38.88 COLON-ALIGNED
     FILL-IN_FAKTORFS AT ROW 2.29 COL 38.88 COLON-ALIGNED
     FILL-IN_MOMS AT ROW 3.29 COL 38.88 COLON-ALIGNED
     FILL-IN_IPVARDE AT ROW 4.71 COL 38.88 COLON-ALIGNED
     FILL-IN_ITVARDE AT ROW 5.71 COL 38.88 COLON-ALIGNED
     FILL-IN_PRISTIDPUNKT AT ROW 6.75 COL 38.88 COLON-ALIGNED
     FILL-IN-FBELOPP AT ROW 8.38 COL 38.88 COLON-ALIGNED
     FILL-IN-ZON1 AT ROW 9.42 COL 38.88 COLON-ALIGNED
     FILL-IN-ZON2 AT ROW 10.46 COL 38.88 COLON-ALIGNED
     FILL-IN-MOM2P1 AT ROW 12.71 COL 38.88 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-NOINLAND AT ROW 13.83 COL 22.88 WIDGET-ID 2
     FILL-IN-NOKUSTLANDEJG AT ROW 14.96 COL 2.88 WIDGET-ID 14
     FILL-IN-TILLV3 AT ROW 16.08 COL 1.88 WIDGET-ID 16
     FILL-IN-TILLV4A AT ROW 17.21 COL 1 WIDGET-ID 12
     FILL-IN-TILLV4B AT ROW 18.33 COL 1 WIDGET-ID 10
     FILL-IN-NOKUSTLAND AT ROW 21.29 COL 20.75 WIDGET-ID 4
     FILL-IN-SVEALAND AT ROW 22.33 COL 30.75 WIDGET-ID 6
     FILL-IN-GOTALAND AT ROW 23.38 COL 30.75 WIDGET-ID 8
     BTN_OK AT ROW 25.38 COL 32.13
     BTN_AVSL AT ROW 25.38 COL 47.13
     SPACE(2.86) SKIP(0.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra markvärderingsdata".


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

/* SETTINGS FOR FILL-IN FILL-IN-GOTALAND IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-GOTALAND:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NOINLAND IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-NOKUSTLAND IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-NOKUSTLAND:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NOKUSTLANDEJG IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SVEALAND IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-SVEALAND:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TILLV3 IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TILLV4A IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TILLV4B IN FRAME DIALOG-1
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_ITVARDE IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ITVARDE:HIDDEN IN FRAME DIALOG-1           = TRUE.

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
ON END-ERROR OF FRAME DIALOG-1 /* Ändra markvärderingsdata */
DO:
   IF VALID-HANDLE(markvh) THEN DELETE PROCEDURE markvh.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra markvärderingsdata */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:                                
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   {muswait.i} 
   ASSIGN
   FILL-IN_basbelopp = INPUT FRAME {&FRAME-NAME} FILL-IN_basbelopp
   FILL-IN_FAKTORFS = INPUT FILL-IN_FAKTORFS
   FILL-IN_MOMS = INPUT FILL-IN_MOMS
   FILL-IN_IPVARDE = INPUT FILL-IN_IPVARDE
   FILL-IN_ITVARDE = INPUT FILL-IN_ITVARDE 
   FILL-IN_PRISTIDPUNKT = INPUT FILL-IN_PRISTIDPUNKT
   FILL-IN-FBELOPP = INPUT FILL-IN-FBELOPP
   FILL-IN-ZON1 = INPUT FILL-IN-ZON1
   FILL-IN-ZON2 = INPUT FILL-IN-ZON2
   FILL-IN-NOINLAND = INPUT FILL-IN-NOINLAND
   /*FILL-IN-NOKUSTLAND = INPUT FILL-IN-NOKUSTLAND
   FILL-IN-SVEALAND = INPUT FILL-IN-SVEALAND
   FILL-IN-GOTALAND = INPUT FILL-IN-GOTALAND.*/
   
   FILL-IN-MOM2P1 = INPUT FILL-IN-MOM2P1
   FILL-IN-NOKUSTLANDEJG = INPUT FILL-IN-NOKUSTLANDEJG
   FILL-IN-TILLV3 = INPUT FILL-IN-TILLV3
   FILL-IN-TILLV4A = INPUT FILL-IN-TILLV4A
   FILL-IN-TILLV4B = INPUT FILL-IN-TILLV4B.
   

   RUN klar IN markvh (INPUT markdivtemp.ITVARDE,INPUT FILL-IN_basbelopp,
                       INPUT FILL-IN_FAKTORFS,INPUT FILL-IN_MOMS,
                       INPUT FILL-IN_IPVARDE,INPUT FILL-IN_ITVARDE,
                       INPUT FILL-IN_PRISTIDPUNKT).
   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "REVADM"                   
   inextradatatemp.HUVUDINT = 1                   
   inextradatatemp.HUVUDCH = "1"   
   inextradatatemp.SOKDEC[1] = FILL-IN-FBELOPP
   inextradatatemp.SOKDEC[2] = FILL-IN-ZON1
   inextradatatemp.SOKDEC[3] = FILL-IN-ZON2.   
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "STORSKOG"                   
   inextradatatemp.HUVUDINT = 1                   
   inextradatatemp.HUVUDCH = "1"   
   inextradatatemp.SOKINT[1] = FILL-IN-MOM2P1
   inextradatatemp.SOKDEC[1] = FILL-IN-NOINLAND
   /*inextradatatemp.SOKDEC[2] = FILL-IN-NOKUSTLAND
   inextradatatemp.SOKDEC[3] = FILL-IN-SVEALAND
   inextradatatemp.SOKDEC[4] = FILL-IN-GOTALAND.*/
   /*nystorskog 20140224*/ 
   inextradatatemp.SOKDEC[5] = FILL-IN-NOKUSTLANDEJG
   inextradatatemp.SOKDEC[6] = FILL-IN-TILLV3
   inextradatatemp.SOKDEC[7] = FILL-IN-TILLV4A
   inextradatatemp.SOKDEC[8] = FILL-IN-TILLV4B.
      
       
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   
   
   musz = FALSE.
   {musarrow.i}  
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(markvh) THEN DELETE PROCEDURE markvh.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FBELOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FBELOPP DIALOG-1
ON LEAVE OF FILL-IN-FBELOPP IN FRAME DIALOG-1 /* Rev fast belopp */
DO:
  FILL-IN-FBELOPP = INPUT FILL-IN-FBELOPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ZON1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ZON1 DIALOG-1
ON LEAVE OF FILL-IN-ZON1 IN FRAME DIALOG-1 /* Rev Zon 1 kr/m */
DO:
  FILL-IN-ZON1 = INPUT FILL-IN-ZON1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ZON2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ZON2 DIALOG-1
ON LEAVE OF FILL-IN-ZON2 IN FRAME DIALOG-1 /* Rev Zon 2 kr/m */
DO:
  FILL-IN-ZON2 = INPUT FILL-IN-ZON2.
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
   {muswait.i}  
   {ALLSTARTDYN.I} 
   RUN markdivhmt IN markvh (OUTPUT TABLE markdivtemp).

   FIND FIRST markdivtemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markdivtemp THEN DO:
      CREATE markdivtemp.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_basbelopp    = markdivtemp.basbelopp
      FILL-IN_FAKTORFS     = markdivtemp.FAKTORFS
      FILL-IN_MOMS         = markdivtemp.MOMS
      FILL-IN_IPVARDE      = markdivtemp.IPVARDE
      FILL-IN_ITVARDE      = markdivtemp.ITVARDE
      FILL-IN_PRISTIDPUNKT = markdivtemp.PRISTIDPUNKT.       
   END.
   /*revavtal uppdateras årligen finns i revadmhmt_UI i ANDVARDSPECAPP.P och SKAPPROTOU7.P*/
   
   /*EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "REVADM"                   
   inextradatatemp.HUVUDINT = 1                    
   inextradatatemp.HUVUDCH = "1".                          
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp  NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO: 
      ASSIGN
      FILL-IN-FBELOPP = extradatatemp.SOKDEC[1]
      FILL-IN-ZON1 = extradatatemp.SOKDEC[2]
      FILL-IN-ZON2 = extradatatemp.SOKDEC[3].
   END.*/
   RUN enable_UI.
   /*IF  Guru.Konstanter:varforetypval[42] = 1 THEN DO: 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "STORSKOG"                   
      inextradatatemp.HUVUDINT = 1                    
      inextradatatemp.HUVUDCH = "1".                          
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp  NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO: 
         ASSIGN
         FILL-IN-MOM2P1 = extradatatemp.SOKINT[1] 
         FILL-IN-NOINLAND = extradatatemp.SOKDEC[1]
         /*FILL-IN-NOKUSTLAND = extradatatemp.SOKDEC[2]
         FILL-IN-SVEALAND = extradatatemp.SOKDEC[3]
         FILL-IN-GOTALAND = extradatatemp.SOKDEC[4].*/
                          
         /*nystorskog 20140224 */
         FILL-IN-NOKUSTLANDEJG = extradatatemp.SOKDEC[5]  
         FILL-IN-TILLV3 = extradatatemp.SOKDEC[6]
         FILL-IN-TILLV4A = extradatatemp.SOKDEC[7]
         FILL-IN-TILLV4B = extradatatemp.SOKDEC[8].
         
         DISPLAY FILL-IN-MOM2P1 FILL-IN-NOINLAND FILL-IN-NOKUSTLANDEJG FILL-IN-TILLV3 FILL-IN-TILLV4A FILL-IN-TILLV4B  /*FILL-IN-NOKUSTLAND FILL-IN-SVEALAND FILL-IN-GOTALAND*/ WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:*/
   /*storskog uppdateras årligen i hmtkrhaN_UI andvardspecapp.p förutom för UMEA och UMBR som har index*/
   
   FILL-IN-MOM2P1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-NOINLAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-NOKUSTLAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-SVEALAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-GOTALAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-NOKUSTLANDEJG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   FILL-IN-TILLV3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-TILLV4A:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-TILLV4B:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*revavtal uppdateras årligen finns i revadmhmt_UI i ANDVARDSPECAPP.P och SKAPPROTOU7.P*/
   FILL-IN-FBELOPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-ZON1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN-ZON2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*END.*/      
   FILL-IN_FAKTORFS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FILL-IN_BASBELOPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
          

   {FRMSIZED.I}    
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN MARKVAPP.P PERSISTENT SET markvh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MARKVAPP.P PERSISTENT SET markvh.
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
  DISPLAY FILL-IN_basbelopp FILL-IN_FAKTORFS FILL-IN_MOMS FILL-IN_IPVARDE 
          FILL-IN_PRISTIDPUNKT FILL-IN-FBELOPP FILL-IN-ZON1 FILL-IN-ZON2 
          FILL-IN-MOM2P1 FILL-IN-NOINLAND FILL-IN-NOKUSTLANDEJG FILL-IN-TILLV3 
          FILL-IN-TILLV4A FILL-IN-TILLV4B 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_basbelopp FILL-IN_FAKTORFS FILL-IN_MOMS FILL-IN_IPVARDE 
         FILL-IN_PRISTIDPUNKT FILL-IN-FBELOPP FILL-IN-ZON1 FILL-IN-ZON2 
         FILL-IN-MOM2P1 FILL-IN-NOINLAND FILL-IN-NOKUSTLANDEJG FILL-IN-TILLV3 
         FILL-IN-TILLV4A FILL-IN-TILLV4B BTN_OK BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nykind_UI DIALOG-1 
PROCEDURE nykind_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

