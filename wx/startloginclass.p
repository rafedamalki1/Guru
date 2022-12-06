/* KÖRS EJ startloginclass.p*/
DEFINE INPUT PARAMETER typkoll AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
/*Vid  typkoll = 1 och typkoll = 3 skall Guru.Konstanter:globanv 
   Guru.Konstanter:globlos           = globlos
   sättas före detta program
     Vid  typkoll = 3 skall Guru.Konstanter:globanv sättas före detta program
     BYT LÖSEN/ANGE LÖSEN = 3.
   INLOGGNING = 2
   BYTANV = 1
   */
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.   
DEFINE VARIABLE globlos AS CHARACTER NO-UNDO.
DEFINE VARIABLE globallao AS LOGICAL NO-UNDO.
DEFINE VARIABLE globallpers AS LOGICAL NO-UNDO.
DEFINE VARIABLE globavd AS INTEGER NO-UNDO. 
DEFINE VARIABLE globomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE alltidmax AS LOGICAL NO-UNDO. 
DEFINE VARIABLE globstorb AS INTEGER NO-UNDO.
DEFINE VARIABLE globstorh AS INTEGER NO-UNDO.
DEFINE VARIABLE globDefaultstorh AS INTEGER NO-UNDO.
DEFINE VARIABLE globDefaultstorb AS INTEGER NO-UNDO.
DEFINE VARIABLE globniv AS INTEGER NO-UNDO. 
DEFINE VARIABLE globpersnamn AS CHARACTER NO-UNDO. 
DEFINE VARIABLE globanvpkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE globanvavdnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE globjid AS CHARACTER NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE storkollbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE storkollhojd AS INTEGER NO-UNDO.
DEFINE VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
DEFINE TEMP-TABLE xsektemp
  FIELD AV-LEVEL AS INTEGER
  FIELD MENYVART AS CHARACTER
  FIELD SEK AS LOGICAL EXTENT 20
  INDEX XSEK IS PRIMARY MENYVART AV-LEVEL.
  /*
DEFINE TEMP-TABLE omrtemp NO-UNDO
   FIELD ANVANDARE AS CHARACTER
   FIELD PANDRA AS LOGICAL
   FIELD AVDELNINGNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.
DEFINE TEMP-TABLE omrtempsek NO-UNDO LIKE omrtemp.
DEFINE VARIABLE statusomr AS LOGICAL NO-UNDO.
*/

{EXTRADATA.I}

RUN datornamn_UI.
RUN losen_UI.
IF musz = TRUE THEN RETURN. 
RUN sek_UI.
RUN start_UI.

PROCEDURE datornamn_UI :
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
   IF typkoll = 2 THEN DO:      
      ASSIGN 
      Guru.Konstanter:globanv = TRIM(outanvanv).   
   END.
   
   IF typkoll = 23 THEN DO:      
      ASSIGN 
      Guru.Konstanter:globanv = "ALLMÄN".
      Guru.Konstanter:globlos = "ALLMÄN".
      ASSIGN
      globlos = Guru.Konstanter:globlos.   
   END.
   IF typkoll = 24 THEN DO:      
      ASSIGN 
      Guru.Konstanter:globanv = "ALLMÄN2".
      Guru.Konstanter:globlos = "ALLMÄN2".
      
      ASSIGN
      globlos = Guru.Konstanter:globlos.   
   END.
   IF typkoll = 25 THEN DO:      
      ASSIGN 
      Guru.Konstanter:globanv = "FLEX".
      Guru.Konstanter:globlos = "FLEX".
      IF varforetypval[56] = 6 THEN Guru.Konstanter:globlos = "FLEXAR".  
      ASSIGN
      globlos = Guru.Konstanter:globlos.   
   END.
   IF typkoll = 1 OR typkoll = 3 THEN DO:      
      ASSIGN 
      globlos = Guru.Konstanter:globlos. 
        
   END.
   ASSIGN  
   Guru.Konstanter:datornamn = TRIM(outdatornamn)
   Guru.Konstanter:globanvnt = TRIM(outanvanv).
   IF Guru.Konstanter:globanvbyt NE "" THEN  Guru.Konstanter:globanv =  Guru.Konstanter:globanvbyt.
  
   /*Vid  typkoll = 1 och typkoll = 3 skall Guru.Konstanter:globanv 
   Guru.Konstanter:globlos           = globlos
   sättas före detta program
     Vid  typkoll = 3 skall Guru.Konstanter:globanv sättas före detta program
   */  
END PROCEDURE.
PROCEDURE losen_UI :
   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
   IF typkoll >= 22 THEN  typkoll = 2.
   IF Guru.Konstanter:appcon THEN DO:
      RUN LOSENKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT typkoll,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz,OUTPUT globjid).
   END.
   ELSE DO:   
       RUN LOSENKOLL.P 
       (INPUT typkoll,INPUT Guru.Konstanter:globanv,INPUT-OUTPUT globlos,OUTPUT globallao,
       OUTPUT globallpers,OUTPUT globavd,OUTPUT globomr,OUTPUT storkollbredd,
       OUTPUT storkollhojd,OUTPUT globniv,OUTPUT globpersnamn,OUTPUT globanvpkod,OUTPUT globanvavdnr,OUTPUT musz,OUTPUT globjid).
   END.
    
   IF musz = TRUE THEN DO:
      RUN sprakut_UI.
      /*
      rrr = System.Windows.Forms.MessageBox:Show(Root:LanguageManager:GetStringAsMessage(58),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
      */
      RETURN.
   END.   
  
   IF storkollbredd > globstorb THEN globstorb = storkollbredd.
   IF storkollhojd  > globstorh THEN globstorh = storkollhojd.
     IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.                  
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      alltidmax = extradatatemp.SOKLOG[10].         
      IF extradatatemp.SOKCHAR[1] = "" THEN extradatatemp.SOKCHAR[1] = "AMERICAN".
      SESSION:NUMERIC-FORMAT = extradatatemp.SOKCHAR[1].
      ASSIGN
      Guru.Konstanter:globsprak         = extradatatemp.SOKINT[3]
      Guru.Konstanter:globnetprissortvar = extradatatemp.SOKINT[4].
      {GLOBFONSTER.I}
      
      ASSIGN 
      Guru.Konstanter:globDefaultstorb = globDefaultstorb
      Guru.Konstanter:globDefaultstorh = globDefaultstorh.
      ASSIGN 
      Guru.Konstanter:globstorb = globDefaultstorb
      Guru.Konstanter:globstorh = globstorh. 
           
      
   END.
   ASSIGN
 
   Guru.Konstanter:globlos           = globlos      
   Guru.Konstanter:globallao         = globallao   
   Guru.Konstanter:globallpers       = globallpers 
   Guru.Konstanter:globavd           = globavd     
   Guru.Konstanter:globomr           = globomr
   Guru.Konstanter:alltidmax          = alltidmax  
   Guru.Konstanter:globstorb         = globstorb   
   Guru.Konstanter:globstorh         = globstorh.
   ASSIGN   
   Guru.Konstanter:globniv           = Guru.Konstanter:globniv     
   Guru.Konstanter:globpersnamn      = globpersnamn
   Guru.Konstanter:globanvpkod       = globanvpkod 
   Guru.Konstanter:globanvavdnr      = globanvavdnr
   Guru.Konstanter:globjid           = globjid.
   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
   
END PROCEDURE.
     
PROCEDURE sek_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE xsektemp NO-ERROR. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN SEKSTART.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT "",INPUT Guru.Konstanter:globniv,OUTPUT TABLE xsektemp).
   END.
   ELSE DO: 
      RUN SEKSTART.P 
      (INPUT "",INPUT Guru.Konstanter:globniv,OUTPUT TABLE xsektemp).
   END.   
   
   FOR EACH xsektemp BY xsektemp.MENYVART:
      IF xsektemp.MENYVART = "GURU" OR xsektemp.MENYVART = "ALLA" THEN DO:        
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:hoppsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.          
      IF xsektemp.MENYVART = "STOR" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:storsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "REG"  THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:regsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END. 
      END.
      IF xsektemp.MENYVART = "PLAN"  THEN DO:    
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:plansekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "PERS" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:persekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "FAKT" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:faktsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END. 
         END.
      END.
      IF xsektemp.MENYVART = "BULA" THEN DO:               
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:bulasekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TID" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDB" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidbsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDT" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidtsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "TIDA" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidasekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TIDS" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidssekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TADM" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tadmsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.  
      IF xsektemp.MENYVART = "TIDO" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidosekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END. 
      IF xsektemp.MENYVART = "TIDR" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:tidrsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "AONR" THEN DO:
         IF AVAILABLE xsektemp THEN DO:
         i = 1.
            DO WHILE i <= 20:
               Guru.Konstanter:aonrsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "KALK" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:kalk2sekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "MTRL" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:mtrlsekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
      IF xsektemp.MENYVART = "BERE" THEN DO:
         i = 1.
         IF AVAILABLE xsektemp THEN DO:
            DO WHILE i <= 20:
               Guru.Konstanter:beresekvar[i] = xsektemp.SEK[i]. 
               i = i + 1.
            END.
         END.
      END.
   END.   
END PROCEDURE.

PROCEDURE sprakut_UI :
   /*Skapar en fakeroot som enbart laddar konstanter OCH RENSAR TT*/
   DEFINE VARIABLE startvar AS Guru.FakeRoot NO-UNDO.
   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
   startvar = NEW Guru.FakeRoot().
   /*
   rrr = System.Windows.Forms.MessageBox:Show(startvar:LanguageManager:GetStringAsMessage(58),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Warning).
   */
   DELETE OBJECT startvar NO-ERROR.
END PROCEDURE.
PROCEDURE start_UI :
   /*Skapar en fakeroot som enbart laddar konstanter OCH RENSAR TT*/
   DEFINE VARIABLE startvar AS Guru.FakeRoot NO-UNDO.
   startvar = NEW Guru.FakeRoot().
   DELETE OBJECT startvar NO-ERROR.
END PROCEDURE.
