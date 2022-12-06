/*FAKMEDF.P*/   

    
/*körs från nattkörningen*/
/*STARTDATUM SÄTTS FÖRSTA GÅNGEN DÅ TIDSKRIVS PÅ AONR OCH TAS BORT DÅ INGEN TID FINNS*/
{NAMNDB.I}

 
 

DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE EDITOR_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 9
       NO-UNDO.
DEFINE VARIABLE aonrslut AS LOGICAL NO-UNDO.
DEFINE VARIABLE startmed AS CHARACTER NO-UNDO.  
DEFINE TEMP-TABLE medplan
   FIELD ANVANDARE LIKE AONRTAB.ANVANDARE  
   FIELD FANVANDARE LIKE FAKTURADM.ANVANDARE  
   FIELD MED1 AS CHARACTER    
   FIELD KOLL AS CHARACTER 
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD PERSONALKOD AS CHARACTER
   FIELD DATUM AS DATE 
   FIELD VECKONUMMER AS INTEGER 
   FIELD FAKTNR LIKE FAKTPLAN.FAKTNR 
   FIELD NAMN LIKE FAKTPLAN.NAMN 
   FIELD BESTID LIKE FAKTPLAN.BESTID
   FIELD VIBESTID LIKE FAKTPLAN.BESTID
   INDEX MED IS PRIMARY FANVANDARE ANVANDARE BESTID FAKTNR
   INDEX MED2 ANVANDARE KOLL BESTID FAKTNR.    
DEFINE TEMP-TABLE medlop                
   FIELD ANVANDARE LIKE AONRTAB.ANVANDARE  
   FIELD FANVANDARE LIKE FAKTURADM.ANVANDARE  
   FIELD MED1 AS CHARACTER    
   FIELD KOLL AS CHARACTER 
   FIELD AONR LIKE AONRTAB.AONR
   FIELD DELNR LIKE AONRTAB.DELNR
   FIELD PERSONALKOD AS CHARACTER
   FIELD DATUM AS DATE    
   FIELD VECKONUMMER AS INTEGER 
   FIELD FAKTNR LIKE FAKTPLAN.FAKTNR 
   FIELD NAMN LIKE FAKTPLAN.NAMN 
   FIELD BESTID LIKE FAKTPLAN.BESTID
   FIELD VIBESTID LIKE FAKTPLAN.BESTID
   INDEX MED IS PRIMARY FANVANDARE ANVANDARE BESTID FAKTNR
   INDEX MED2 ANVANDARE KOLL BESTID FAKTNR.       
DEFINE BUFFER medbuff FOR MEDDELANDE.

{SMTPDEF3.I}
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE tillvar   AS CHARACTER LABEL "To" NO-UNDO.

DEFINE QUERY aonrq FOR AONRTAB.  
/*VARFÖR*/
FIND FIRST medplan NO-LOCK NO-ERROR.  
FIND FIRST medlop NO-LOCK NO-ERROR.  
FIND FIRST medbuff NO-LOCK NO-ERROR.
FIND FIRST FAKTURADM NO-LOCK NO-ERROR.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
DEFINE VARIABLE fuppapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fnr AS INTEGER NO-UNDO.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
   RUN FAKUPPARPA.P PERSISTENT SET fuppapph.
   RUN start2_UI IN fuppapph.
   RUN getfirst_UI IN fuppapph (OUTPUT fnr).
   IF fnr NE 0 THEN DO:
      REPEAT:
        RUN kost_UI IN fuppapph (INPUT fnr).
         RUN getnext_UI IN fuppapph (OUTPUT fnr).
         IF fnr = 0 THEN LEAVE.
      END.
   END.
   RUN start4_UI IN fuppapph.
   RUN getfirst_UI IN fuppapph (OUTPUT fnr).
   IF fnr NE 0 THEN DO:
      REPEAT:
         RUN kost_UI IN fuppapph (INPUT fnr).
         RUN getnext_UI IN fuppapph (OUTPUT fnr).
         IF fnr = 0 THEN LEAVE.
      END.
   END.
END.

/*BETALNINGSPLAN*/
   /*START DATUM FÖR ALLA AONR*/
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
   */
IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR 
Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
   OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 
   USE-INDEX AONR NO-LOCK.   
    GET FIRST aonrq NO-LOCK.    
   DO WHILE AVAILABLE(AONRTAB):            
      DO TRANSACTION:
         GET CURRENT aonrq EXCLUSIVE-LOCK NO-WAIT.    
         IF LOCKED(AONRTAB) = FALSE THEN RUN startdat_UI.               
      END.
      GET NEXT aonrq NO-LOCK.
   END.
   GET FIRST aonrq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):  
      IF AONRTAB.FAKTTYP = "Fastpris" OR AONRTAB.FAKTTYP = "A-contofakt." OR AONRTAB.FAKTTYP = "Avtal" THEN DO:      
         RUN fast_UI.
      END. 
      ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         IF AONRTAB.FAKTTYP = "Löpande räkning" OR AONRTAB.FAKTTYP = "Takprisfakt." THEN
         RUN lop_UI.
      END.
      GET NEXT aonrq NO-LOCK.
   END.          
END.
FOR EACH MEDDELANDE WHERE MEDDELANDE.SDATUM = TODAY AND MEDDELANDE.EMOTAGET = FALSE AND MEDDELANDE.SANDARE = "FAKT.ADM.":    
   RUN epostadd_UI.
END.
PROCEDURE epostadd_UI :
   tillvar = "".
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = MEDDELANDE.MOTTAGARE NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         tillvar = SUBSTRING(PERSONALTAB.PERSONSOK,20).
         RUN mailstart_UI (INPUT "Dags att godkänna fakturor i Guru.",INPUT MEDDELANDE.MEDD).
      END.
   END.           
END PROCEDURE.
PROCEDURE mailstart_UI :
   DEFINE INPUT PARAMETER amnevar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.                             
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      ASSIGN
         /*
      servervar = "172.16.79.249".    
      servervar = "130.1.27.253".  
      */
      servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   END.
   ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      {SMTPFRANELPOOL.I}
   END.
   IF servervar = "" OR tillvar = "" THEN RETURN.
   IF namndb() = "utbi" THEN RETURN.
   ASSIGN 
   mailhub             = servervar     
   EmailTo             = tillvar 
   EmailFrom           = franvar
   EmailCC             = ""
   Attachmentstyp      = ""
   LocalFiles          = ""
   Subject             = amnevar
   Bodysmtp            = meddvar
   MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
   BodyType            = "".
   
   IF Guru.Konstanter:globforetag = "sund"  THEN EmailFrom = "webguru@sundsvallenergi.se".
   IF Guru.Konstanter:globforetag = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".
   IF Guru.Konstanter:globforetag = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
   RUN smtpmail_UI (INPUT FALSE).
   IF oSuccessful = TRUE THEN DO:
      oSuccessful = FALSE.                      
   END.            
END PROCEDURE.
PROCEDURE fast_UI:
   OPEN QUERY betpovrq FOR EACH FAKTSTART WHERE FAKTSTART.PLANDATUM = TODAY + 1 AND
   FAKTSTART.FAKTURERAD = FALSE
   NO-LOCK.
   GET FIRST betpovrq NO-LOCK.   
   DO WHILE AVAILABLE(FAKTSTART):  
      RUN medovr_UI.
      GET NEXT betpovrq NO-LOCK.                   
   END.                            
   
   OPEN QUERY betplanq FOR EACH FAKTPLAN WHERE FAKTPLAN.SLUTFAKT = FALSE 
   NO-LOCK.  
   GET FIRST betplanq NO-LOCK.
   DO WHILE AVAILABLE(FAKTPLAN):  
      aonrslut = FALSE.           
      OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.FAKTNR = FAKTPLAN.FAKTNR 
      NO-LOCK.
      GET FIRST aonrq NO-LOCK.   
      IF AVAILABLE AONRTAB THEN DO:
         aonrslut = TRUE. 
         REPEAT:      
            IF NOT AVAILABLE AONRTAB THEN LEAVE.
            IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
               aonrslut = FALSE.
               LEAVE.           
            END. 
            GET NEXT aonrq NO-LOCK.
         END.     
      END.   
      CLOSE QUERY aonrq. 
      IF aonrslut = TRUE THEN DO:
         RUN medslut_UI.
      END. 
      GET NEXT betplanq NO-LOCK.
   END.               
   CLOSE QUERY betplanq.
   RUN plantidkoll_UI.     
   IF AVAILABLE MEDDELANDE THEN DO:
      RELEASE MEDDELANDE.   
   END.    
   IF AVAILABLE medbuff THEN DO:
      RELEASE medbuff.   
   END.
   FOR EACH medplan USE-INDEX MED2 NO-LOCK:
      /*TID UTAN BETPLAN*/
      IF medplan.KOLL = "START" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:          
            IF MEDDELANDE.MOTTAGARE NE medplan.ANVANDARE THEN DO:
               CREATE MEDDELANDE.  
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) + " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medplan.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) +               
         "Person: " + medplan.PERSONALKOD + " har skrivit på " + LC(Guru.Konstanter:gaok) + " " + 
         medplan.AONR + " " + STRING(medplan.DELNR,"999") + " den " + 
         STRING(medplan.DATUM) + CHR(10).              
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medplan.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medplan.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.".         
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) +          
         "Person : " + medplan.PERSONALKOD + " har skrivit på " + LC(Guru.Konstanter:gaok) + " " + 
         medplan.AONR + " " + STRING(medplan.DELNR,"999") + " den " + 
         STRING(medplan.DATUM) + CHR(10).  
      END.   
      /*ALLA MELLAN DAGAR*/
      IF medplan.KOLL = "ÖVRIG" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:
            IF MEDDELANDE.MOTTAGARE NE medplan.ANVANDARE THEN DO:
               CREATE MEDDELANDE.
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medplan.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.".               
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +            
         "Planerat faktureringsdatum:" + STRING(medplan.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medplan.VIBESTID + CHR(10).          
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medplan.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medplan.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.". 
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +            
         "Planerat faktureringsdatum:" + STRING(medplan.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " :" + medplan.VIBESTID + " " + medplan.ANVANDARE + CHR(10).         
      END.
      /*ALLA AONR AVSLUTADE*/        
      IF medplan.KOLL = "SLUT" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:
            IF MEDDELANDE.MOTTAGARE NE medplan.ANVANDARE THEN DO:
               CREATE MEDDELANDE. 
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medplan.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +                   
         Guru.Konstanter:gbestk + " : " + medplan.VIBESTID + CHR(10).      
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medplan.FANVANDARE THEN DO:
               CREATE medbuff.
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) + " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medplan.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.". 
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +                  
         Guru.Konstanter:gbestk + " : " + medplan.VIBESTID + " " + medplan.ANVANDARE + CHR(10).  
      END.
      /*TID EFTER AVSLUT*/
      IF medplan.KOLL = "AVSLUT" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:          
            IF MEDDELANDE.MOTTAGARE NE medplan.ANVANDARE THEN DO:
               CREATE MEDDELANDE.  
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) + " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medplan.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +     
         "Person: " + medplan.PERSONALKOD + " har skrivit på " + LC(Guru.Konstanter:gaok) + " " + 
         medplan.AONR + " " + STRING(medplan.DELNR,"999") + " den " + 
         STRING(medplan.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medplan.VIBESTID + CHR(10).     
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medplan.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM FASTPRIS ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medplan.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.".         
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medplan.MED1 + CHR(10) + 
         "Betplan: " + STRING(medplan.FAKTNR) + " " + medplan.NAMN + CHR(10) +     
         "Person : " + medplan.PERSONALKOD + " har skrivit på " + LC(Guru.Konstanter:gaok) + " " + 
         medplan.AONR + " " + STRING(medplan.DELNR,"999") + " den " + 
         STRING(medplan.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medplan.VIBESTID + " " + medplan.ANVANDARE + CHR(10).  
      END.                                   
   END.     
END PROCEDURE.
PROCEDURE lop_UI:
   /*Löpande räkning*/  
   OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 AND
   (AONRTAB.FAKTTYP = "Löpande räkning" OR AONRTAB.FAKTTYP = "Takprisfakt.") AND 
   AONRTAB.FAKTNR = 0 
   USE-INDEX AONR NO-LOCK.   
   /*FINNS EKO-LÖNE-SAMMANST. UPPGIFTER*/     
   GET FIRST aonrq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):              
      FIND FIRST SUMTIDDAG WHERE 
      SUMTIDDAG.AONR = AONRTAB.AONR AND 
      SUMTIDDAG.DELNR = AONRTAB.DELNR AND SUMTIDDAG.VECKOKORD NE "" 
      USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE SUMTIDDAG THEN DO:  
         startmed = "Löpande räkning saknas trots att tidskrivning finns och är ekonomi-och lönesammanställd".               
         RUN medlopstart_UI.
      END.
      GET NEXT aonrq NO-LOCK.         
   END. 
   CLOSE QUERY aonrq.              
   OPEN QUERY kundrglq FOR EACH FAKTREGLER USE-INDEX FAKTREGLER NO-LOCK.
   GET FIRST kundrglq NO-LOCK.
   DO WHILE AVAILABLE(FAKTREGLER):
      OPEN QUERY faktplanq FOR EACH FAKTPLAN WHERE 
      FAKTPLAN.FAKTNR = FAKTREGLER.FAKTNR AND
      FAKTPLAN.SLUTFAKT = FALSE USE-INDEX FAKTNR NO-LOCK.
      GET FIRST faktplanq NO-LOCK.
      DO WHILE AVAILABLE(FAKTPLAN):      
         IF FAKTPLAN.SENASTFAK = ? THEN DO:  
            OPEN QUERY faktkollq FOR EACH FAKTKOLL WHERE 
            FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR
            USE-INDEX FAKTNR NO-LOCK.
            GET FIRST faktkollq NO-LOCK.
            DO WHILE AVAILABLE(FAKTKOLL): 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = FAKTKOLL.AONR AND 
               SUMTIDDAG.DELNR = FAKTKOLL.DELNR AND               
               SUMTIDDAG.VECKOKORD NE "" 
               USE-INDEX AONR NO-LOCK NO-ERROR.
               IF AVAILABLE SUMTIDDAG THEN DO:
                  startmed = 
                  "Det finns uppgifter att fakturera för första gången på denna faktura".
                  RUN nylopmed_UI.
               END.
               GET NEXT faktkollq NO-LOCK.
            END.                       
         END.     
         ELSE IF FAKTPLAN.SENASTFAK + FAKTREGLER.FAKINT <= TODAY + 1 THEN DO:
            OPEN QUERY faktkollq FOR EACH FAKTKOLL WHERE 
            FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR  
            USE-INDEX FAKTNR NO-LOCK.
            GET FIRST faktkollq NO-LOCK.
            DO WHILE AVAILABLE(FAKTKOLL): 
               FIND FIRST SUMTIDDAG WHERE SUMTIDDAG.AONR = FAKTKOLL.AONR AND 
               SUMTIDDAG.DELNR = FAKTKOLL.DELNR AND 
               SUMTIDDAG.DATUM > FAKTKOLL.SENASTTID AND SUMTIDDAG.VECKOKORD NE "" 
               USE-INDEX AONR NO-LOCK NO-ERROR.
               IF AVAILABLE SUMTIDDAG THEN DO:
                  startmed = "Det finns uppgifter att fakturera".
                  RUN nylopmed_UI.
               END.
               GET NEXT faktkollq NO-LOCK.
            END.                       
         END.                       
         GET NEXT faktplanq NO-LOCK.
      END.
      GET NEXT kundrglq NO-LOCK.
   END. 
   CLOSE QUERY faktkollq.
   CLOSE QUERY kundrglq.
   CLOSE QUERY faktplanq.
   /*KOLL OM ALLA AONR ÄR AVSLUTADE*/
   OPEN QUERY faktplanq FOR EACH FAKTPLAN WHERE FAKTPLAN.SLUTFAKT = FALSE 
   USE-INDEX FAKTNR NO-LOCK.  
   GET FIRST faktplanq NO-LOCK.
   DO WHILE AVAILABLE(FAKTPLAN):  
      aonrslut = FALSE.           
      OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.FAKTNR = FAKTPLAN.FAKTNR 
      USE-INDEX FAKTNR NO-LOCK.
      GET FIRST aonrq NO-LOCK.   
      IF AVAILABLE AONRTAB THEN DO:
         aonrslut = TRUE. 
         REPEAT:      
            IF NOT AVAILABLE AONRTAB THEN LEAVE.
            IF AONRTAB.AONRAVDATUM = 01/01/91 THEN DO:
               /*DET FINNS AONR SOM EJ ÄR AVSLUTADE*/
               aonrslut = FALSE.
               LEAVE.           
            END. 
            GET NEXT aonrq NO-LOCK.
         END.     
      END.   
      CLOSE QUERY aonrq. 
      IF aonrslut = TRUE THEN DO:
         RUN medlopslut_UI.
      END. 
      GET NEXT faktplanq NO-LOCK.
   END.
   RUN loptidkoll_UI.
   IF AVAILABLE MEDDELANDE THEN DO:
      RELEASE MEDDELANDE.   
   END.    
   IF AVAILABLE medbuff THEN DO:
      RELEASE medbuff.   
   END.                   
   FOR EACH medlop USE-INDEX MED2 NO-LOCK:
      /*TID INGET LÖPNR*/
      IF medlop.KOLL = "START" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:          
            IF MEDDELANDE.MOTTAGARE NE medlop.ANVANDARE THEN DO:
               CREATE MEDDELANDE.  
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
               STRING(TODAY) + " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
            STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medlop.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +           
         "Person: " + medlop.PERSONALKOD + " har skrivit på aonr " + 
         medlop.AONR + " " + STRING(medlop.DELNR,"999") + " den " + 
         STRING(medlop.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + CHR(10).     
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medlop.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medlop.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.".         
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +         
         "Person: " + medlop.PERSONALKOD + " har skrivit på aonr " + 
         medlop.AONR + " " + STRING(medlop.DELNR,"999") + " den " + 
         STRING(medlop.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + " " + medlop.ANVANDARE + CHR(10).                
      END.   
      /*NY TID*/
      IF medlop.KOLL = "NYA" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:
            IF MEDDELANDE.MOTTAGARE NE medlop.ANVANDARE THEN DO:
               CREATE MEDDELANDE.
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medlop.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.".               
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) + 
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +                  
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + CHR(10).          
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medlop.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medlop.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.". 
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) + 
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +                  
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + " " + medlop.ANVANDARE + CHR(10).                  
      END.
      /*ALLA AONR AVSLUTADE*/
      IF medlop.KOLL = "SLUT" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:          
            IF MEDDELANDE.MOTTAGARE NE medlop.ANVANDARE THEN DO:
               CREATE MEDDELANDE.  
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
               STRING(TODAY) + " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
            STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medlop.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +           
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +         
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + CHR(10).     
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medlop.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medlop.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.".         
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +                  
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + " " + medlop.ANVANDARE + CHR(10).                
      END.
      IF medlop.KOLL = "AVSLUT" THEN DO TRANSACTION:  
         IF AVAILABLE MEDDELANDE THEN DO:          
            IF MEDDELANDE.MOTTAGARE NE medlop.ANVANDARE THEN DO:
               CREATE MEDDELANDE.  
               ASSIGN
               MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
               STRING(TODAY) + " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE MEDDELANDE.
            ASSIGN
            MEDDELANDE.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + 
            STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         MEDDELANDE.SDATUM = TODAY
         MEDDELANDE.EMOTAGET = FALSE      
         MEDDELANDE.MOTTAGARE = medlop.ANVANDARE.                              
         MEDDELANDE.SANDARE = "FAKT.ADM.". 
         ASSIGN
         MEDDELANDE.MEDD = MEDDELANDE.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +           
         "Person: " + medlop.PERSONALKOD + " har skrivit på aonr " + 
         medlop.AONR + " " + STRING(medlop.DELNR,"999") + " den " + 
         STRING(medlop.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + CHR(10).     
         /*ADMINISTRATTÖR*/       
         IF AVAILABLE medbuff THEN DO:
            IF medbuff.MOTTAGARE NE medlop.FANVANDARE THEN DO:
               CREATE medbuff.  
               ASSIGN
               medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
            END.
         END.  
         ELSE DO:
            CREATE medbuff.       
            ASSIGN
            medbuff.MEDD = "!! FAKTURA INFORMATION OM Löpande räkning ARBETEN " + STRING(TODAY) +  " !!" + CHR(10).
         END.
         ASSIGN 
         medbuff.SDATUM = TODAY
         medbuff.EMOTAGET = FALSE      
         medbuff.MOTTAGARE = medlop.FANVANDARE.                              
         medbuff.SANDARE = "FAKT.ADM.".         
         ASSIGN
         medbuff.MEDD = medbuff.MEDD + CHR(10) +               
         medlop.MED1 + CHR(10) +         
         "Löpande räkning: " + STRING(medlop.FAKTNR) + " " + medlop.NAMN + CHR(10) +
         "Person: " + medlop.PERSONALKOD + " har skrivit på aonr " + 
         medlop.AONR + " " + STRING(medlop.DELNR,"999") + " den " + 
         STRING(medlop.DATUM) + CHR(10) +
         Guru.Konstanter:gbestk + " : " + medlop.VIBESTID + " " + medlop.ANVANDARE + CHR(10).                
      END.                                          
   END.
END PROCEDURE.    

/*SUB RUNS*/
/*BETPLAN*/
PROCEDURE medovr_UI:         
   /*ALLA MELLAN DAGAR*/
   FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTSTART.FAKTNR
   NO-LOCK NO-ERROR.
   IF AVAILABLE FAKTPLAN THEN DO:
      FIND FIRST medplan WHERE medplan.FAKTNR = FAKTPLAN.FAKTNR AND
      medplan.KOLL = "ÖVRIG" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE medplan THEN DO: 
         CREATE medplan.    
         ASSIGN
         medplan.ANVANDARE = FAKTPLAN.ANVANDARE  
         medplan.FANVANDARE = FAKTURADM.ANVANDARE  
         medplan.MED1 = "Denna betalningsplan bör delfaktureras"    
         medplan.KOLL = "ÖVRIG"    
         medplan.DATUM = FAKTSTART.PLANDATUM
         medplan.FAKTNR = FAKTPLAN.FAKTNR 
         medplan.NAMN = FAKTPLAN.NAMN
         medplan.BESTID = FAKTPLAN.BESTID.
         FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
         IF AVAILABLE BESTTAB THEN medplan.VIBESTID = BESTTAB.VIBESTID.
         ELSE DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
            IF AVAILABLE OMRADETAB THEN medplan.VIBESTID = OMRADETAB.OMRADE.
         END.                   
      END.
   END.  
END PROCEDURE.   

PROCEDURE medslut_UI:
   /*DAX FÖR SLUT FAKT ALLA AONR ÄR AVSLUTADE*/
   FIND FIRST FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTPLAN.FAKTNR AND
   FAKTSTART.START = "SLUT" NO-LOCK NO-ERROR.
   IF AVAILABLE FAKTSTART THEN DO:
      IF FAKTSTART.PLAN% NE 0 THEN DO:
         IF FAKTSTART.FAKTURERAD = FALSE THEN DO:              
            FIND FIRST medplan WHERE medplan.FAKTNR = FAKTPLAN.FAKTNR AND
            medplan.KOLL = "SLUT" NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE medplan THEN DO: 
               CREATE medplan.
               ASSIGN
               medplan.ANVANDARE = FAKTPLAN.ANVANDARE  
               medplan.FANVANDARE = FAKTURADM.ANVANDARE  
               medplan.MED1 = 
               "Denna betalningsplan bör slutfaktureras då alla aonr är avslutade"
               medplan.KOLL = "SLUT"
               medplan.FAKTNR = FAKTPLAN.FAKTNR 
               medplan.NAMN = FAKTPLAN.NAMN
               medplan.BESTID = FAKTPLAN.BESTID.
               FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
               IF AVAILABLE BESTTAB THEN medplan.VIBESTID = BESTTAB.VIBESTID.
               ELSE DO:
                  FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE OMRADETAB THEN medplan.VIBESTID = OMRADETAB.OMRADE.
               END.                          
            END.   
         END.
      END.            
   END.       
END PROCEDURE. 

PROCEDURE medstart_UI:      
   /*TID UTAN BETPLAN*/          
   CREATE medplan.
   ASSIGN
   medplan.ANVANDARE = AONRTAB.ANVANDARE  
   medplan.FANVANDARE = FAKTURADM.ANVANDARE  
   medplan.MED1 = startmed    
   medplan.KOLL = "START"
   medplan.AONR = AONRTAB.AONR
   medplan.DELNR = AONRTAB.DELNR
   medplan.PERSONALKOD = TIDREGITAB.PERSONALKOD 
   medplan.DATUM = TIDREGITAB.DATUM
   medplan.VECKONUMMER = TIDREGITAB.VECKONUMMER.   
END PROCEDURE.

PROCEDURE nybetstart_UI:      
   /*FÖRSTA TID PÅ BETPLAN*/       
   FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = AONRTAB.FAKTNR 
   NO-LOCK NO-ERROR.   
   FIND FIRST medplan WHERE medplan.FAKTNR = FAKTPLAN.FAKTNR  AND
   medplan.KOLL = "START" NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE medplan THEN DO:
      CREATE medplan.
      ASSIGN
      medplan.ANVANDARE = AONRTAB.ANVANDARE  
      medplan.FANVANDARE = FAKTURADM.ANVANDARE  
      medplan.MED1 = startmed    
      medplan.KOLL = "NY"
      medplan.AONR = AONRTAB.AONR
      medplan.DELNR = AONRTAB.DELNR
      medplan.PERSONALKOD = TIDREGITAB.PERSONALKOD 
      medplan.DATUM = TIDREGITAB.DATUM
      medplan.VECKONUMMER = TIDREGITAB.VECKONUMMER
      medplan.FAKTNR = FAKTPLAN.FAKTNR 
      medplan.NAMN = FAKTPLAN.NAMN
      medplan.BESTID = FAKTPLAN.BESTID.
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE BESTTAB THEN medplan.VIBESTID = BESTTAB.VIBESTID.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN medplan.VIBESTID = OMRADETAB.OMRADE.
      END.                 
   END.
END PROCEDURE.

PROCEDURE plantidkoll_UI:
   /*TID EFTER AVSLUT*/
   startmed = 
   "Det finns tidskrivning på denna betalningsplan efter att den är avslutad".
   OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 AND
   AONRTAB.FAKTNR NE 0 NO-LOCK.
   GET FIRST aonrq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):
      FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = AONRTAB.FAKTNR AND
      FAKTPLAN.SLUTFAKT = TRUE 
      NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTPLAN THEN DO:
         FIND LAST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND
         TIDREGITAB.DELNR = AONRTAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM > FAKTPLAN.SENASTFAK THEN DO:
               FIND FIRST medplan WHERE medplan.FAKTNR = AONRTAB.FAKTNR AND
               medplan.KOLL = "AVSLUT" NO-LOCK NO-ERROR.
               IF NOT AVAILABLE medplan THEN DO:
                  CREATE medplan.
                  ASSIGN
                  medplan.ANVANDARE = AONRTAB.ANVANDARE  
                  medplan.FANVANDARE = FAKTURADM.ANVANDARE  
                  medplan.MED1 = startmed    
                  medplan.KOLL = "AVSLUT"
                  medplan.AONR = AONRTAB.AONR
                  medplan.DELNR = AONRTAB.DELNR
                  medplan.PERSONALKOD = TIDREGITAB.PERSONALKOD 
                  medplan.DATUM = TIDREGITAB.DATUM
                  medplan.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  medplan.FAKTNR = FAKTPLAN.FAKTNR 
                  medplan.NAMN = FAKTPLAN.NAMN
                  medplan.BESTID = FAKTPLAN.BESTID.
                  FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE BESTTAB THEN medplan.VIBESTID = BESTTAB.VIBESTID.
                  ELSE DO:
                     FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
                     IF AVAILABLE OMRADETAB THEN medplan.VIBESTID = OMRADETAB.OMRADE.
                  END.           
               END.
            END.
         END.
      END.   
      GET NEXT aonrq NO-LOCK.       
   END.
END PROCEDURE. 
     
PROCEDURE startdat_UI:      
   /**/
    /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt. "Avtal"
   */

   FIND FIRST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND 
   TIDREGITAB.DELNR = AONRTAB.DELNR
   USE-INDEX AONR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TIDREGITAB THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
   ELSE DO: 
      IF AONRTAB.STARTDATUM = ? THEN DO:      
         IF AONRTAB.FAKTTYP = "Fastpris" OR AONRTAB.FAKTTYP = "A-contofakt" OR 
         AONRTAB.FAKTTYP = "Avtal" THEN DO: 
            IF AONRTAB.FAKTNR = 0 THEN DO:
               startmed = "Faktura saknas trots att tidskrivning har startat".               
               RUN medstart_UI.   
            END.                                                                     
            ELSE DO:
               FIND FIRST FAKTSTART WHERE FAKTSTART.FAKTNR = AONRTAB.FAKTNR AND
               FAKTSTART.START = "START"
                NO-LOCK NO-ERROR.
               IF AVAILABLE FAKTSTART THEN DO:
                  IF FAKTSTART.PLAN% NE 0 THEN DO:
                     IF FAKTSTART.FAKTURERAD = FALSE THEN DO:                     
                        startmed = "Tidskrivning har startat". 
                        RUN nybetstart_UI.
                     END.
                  END.   
               END.
            END.                                 
         END.         
      END.    
      ELSE IF AONRTAB.STARTDATUM NE TIDREGITAB.DATUM THEN DO:  
         IF AONRTAB.FAKTTYP = "Fastpris" OR AONRTAB.FAKTTYP = "A-contofakt" OR 
         AONRTAB.FAKTTYP = "Avtal" THEN DO:  
            IF AONRTAB.FAKTNR = 0 THEN DO:
               startmed = 
               "Faktura saknas trots att tidskrivning har startat".               
               RUN medstart_UI.
            END.                                                                     
            ELSE DO: 
               FIND FIRST FAKTSTART WHERE FAKTSTART.FAKTNR = AONRTAB.FAKTNR AND
               FAKTSTART.START = "START"
               NO-LOCK NO-ERROR.
               IF AVAILABLE FAKTSTART THEN DO:
                  IF FAKTSTART.PLAN% NE 0 THEN DO:
                     IF FAKTSTART.FAKTURERAD = FALSE THEN DO: 
                        startmed = "Tidskrivning har startat. Startdatumet är ändrat".               
                        RUN nybetstart_UI.             
                     END.   
                  END.
               END.
            END.
         END.   
      END.            
   END. 
END PROCEDURE.        

/*LÖP*/
PROCEDURE medlopstart_UI:             
   /*FINNS TID INGEN LÖPNR*/
   CREATE medlop.
   ASSIGN
   medlop.ANVANDARE = AONRTAB.ANVANDARE  
   medlop.FANVANDARE = FAKTURADM.ANVANDARE  
   medlop.BESTID = AONRTAB.BESTID       
   medlop.MED1 = startmed    
   medlop.KOLL = "START"
   medlop.AONR = AONRTAB.AONR
   medlop.DELNR = AONRTAB.DELNR
   medlop.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
   medlop.DATUM = SUMTIDDAG.DATUM.       
   FIND FIRST BESTTAB WHERE BESTTAB.BESTID = AONRTAB.BESTID NO-LOCK NO-ERROR.
   IF AVAILABLE BESTTAB THEN medlop.VIBESTID = BESTTAB.VIBESTID.
   ELSE DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN medlop.VIBESTID = OMRADETAB.OMRADE.
   END.
END PROCEDURE.     

PROCEDURE medlopslut_UI:
   /*ALLA AONR KLARA*/
   FIND FIRST medlop WHERE medlop.FAKTNR = FAKTPLAN.FAKTNR AND
   medlop.KOLL = "SLUT" NO-LOCK NO-ERROR.            
   IF NOT AVAILABLE medlop THEN DO: 
      CREATE medlop.
      ASSIGN
      medlop.ANVANDARE = FAKTPLAN.ANVANDARE  
      medlop.FANVANDARE = FAKTURADM.ANVANDARE  
      medlop.MED1 = 
      "Denna Löpande räkning bör slutfaktureras då alla aonr är avslutade"
      medlop.KOLL = "SLUT"
      medlop.FAKTNR = FAKTPLAN.FAKTNR 
      medlop.NAMN = FAKTPLAN.NAMN
      medlop.BESTID = FAKTPLAN.BESTID.  
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE BESTTAB THEN medlop.VIBESTID = BESTTAB.VIBESTID.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN medlop.VIBESTID = OMRADETAB.OMRADE.
      END.                                  
   END.       
END PROCEDURE. 

PROCEDURE nylopmed_UI:             
   /*NY TID*/
   CREATE medlop.
   ASSIGN
   medlop.ANVANDARE = FAKTPLAN.ANVANDARE  
   medlop.FANVANDARE = FAKTURADM.ANVANDARE
   medlop.BESTID = FAKTPLAN.BESTID   
   medlop.FAKTNR = FAKTPLAN.FAKTNR
   medlop.NAMN = FAKTPLAN.NAMN   
   medlop.MED1 = startmed    
   medlop.KOLL = "NYA"
   medlop.AONR = SUMTIDDAG.AONR
   medlop.DELNR = SUMTIDDAG.DELNR
   medlop.PERSONALKOD = SUMTIDDAG.PERSONALKOD 
   medlop.DATUM = SUMTIDDAG.DATUM.    
   FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
   IF AVAILABLE BESTTAB THEN medlop.VIBESTID = BESTTAB.VIBESTID.
   ELSE DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN medlop.VIBESTID = OMRADETAB.OMRADE.
   END.
END PROCEDURE. 

PROCEDURE loptidkoll_UI:
   /*TID EFTER AVSLUT*/
   startmed = 
   "Det finns tidskrivning på denna Löpande räkning efter att den är avslutad".
   OPEN QUERY aonrq FOR EACH AONRTAB WHERE AONRTAB.AONRAVDATUM = 01/01/91 AND
   AONRTAB.FAKTNR NE 0 NO-LOCK.
   GET FIRST aonrq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):
      FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = AONRTAB.FAKTNR AND
      FAKTPLAN.SLUTFAKT = TRUE 
      USE-INDEX FAKTNR NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTPLAN THEN DO:
         FIND LAST TIDREGITAB WHERE TIDREGITAB.AONR = AONRTAB.AONR AND
         TIDREGITAB.DELNR = AONRTAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.DATUM > FAKTPLAN.SENASTTID THEN DO:
               FIND FIRST medlop WHERE medlop.FAKTNR = AONRTAB.FAKTNR AND
               medlop.KOLL = "AVSLUT" NO-LOCK NO-ERROR.
               IF NOT AVAILABLE medlop THEN DO:
                  CREATE medlop.
                  ASSIGN
                  medlop.ANVANDARE = AONRTAB.ANVANDARE  
                  medlop.FANVANDARE = FAKTURADM.ANVANDARE  
                  medlop.MED1 = startmed    
                  medlop.KOLL = "AVSLUT"
                  medlop.AONR = AONRTAB.AONR
                  medlop.DELNR = AONRTAB.DELNR
                  medlop.PERSONALKOD = TIDREGITAB.PERSONALKOD 
                  medlop.DATUM = TIDREGITAB.DATUM
                  medlop.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  medlop.FAKTNR = FAKTPLAN.FAKTNR 
                  medlop.NAMN = FAKTPLAN.NAMN
                  medlop.BESTID = FAKTPLAN.BESTID.           
                  FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE BESTTAB THEN medlop.VIBESTID = BESTTAB.VIBESTID.
                  ELSE DO:
                     FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.BESTID NO-LOCK NO-ERROR.
                     IF AVAILABLE OMRADETAB THEN medlop.VIBESTID = OMRADETAB.OMRADE.
                  END.
               END.
            END.
         END.
      END.   
      GET NEXT aonrq NO-LOCK.       
   END.
END PROCEDURE. 
