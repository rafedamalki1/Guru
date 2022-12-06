/*APLIST.P*/

&Scoped-define NEW NEW
{TIDPERS.I}
DEFINE TEMP-TABLE mankoll    
   FIELD PERSONALKOD LIKE  PERSONALTAB.PERSONALKOD 
   FIELD AR AS INTEGER
   FIELD MANADNR AS INTEGER
   INDEX PKOD IS PRIMARY PERSONALKOD AR MANADNR.
{TIDUTTT.I}
DEFINE BUFFER kollut FOR tidut.
DEFINE BUFFER tidbuf FOR TIDREGITAB.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
DEFINE VARIABLE bakmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE gamman AS INTEGER NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.


DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER val AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER inregvnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER listar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE INPUT PARAMETER listmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.

{EXTRADATA.I}
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.

ASSIGN
str=                                                                                  
"================================================================================"
regar = listar
regmnr = listmnr
regvnr = inregvnr.

IF listmnr = 12 THEN DO:
   hjdat = DATE(01,01,(listar + 1)) - 1.
END.
ELSE hjdat = DATE((listmnr + 1) ,01,listar) - 1.

IF val = TRUE THEN RUN huvud_UI. 
ELSE RUN huvud2_UI.

{GDPRLOGGCLIENT.I} 
PROCEDURE huvud2_UI :
     /*HUVUD2*/          
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,4) = "LISTNING AV ALLA EJ GODKÄNDA TIDSEDLAR"   
   SUBSTRING(tidut.UT,60) = STRING(TODAY)
   SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.                         
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "ENHET/".
   
   IF gvisatidpermanad = TRUE THEN DO:
      SUBSTRING(tidut.UT,68) = "GODKÄND T.O.M".
   END.
   CREATE tidut.                         
   ASSIGN 
   SUBSTRING(tidut.UT,1) = "SIGN"
   SUBSTRING(tidut.UT,9) = "FÖRNAMN"
   SUBSTRING(tidut.UT,25) = "EFTERNAMN"   
   SUBSTRING(tidut.UT,51) = CAPS(Guru.Konstanter:gomrk)
   SUBSTRING(tidut.UT,58) = "VNR".   
   IF gvisatidpermanad = TRUE THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,58) = "MÅNAD"         
      SUBSTRING(tidut.UT,68) = "DATUM".
   END.
   CREATE tidut.  
   ASSIGN tidut.UT = str.      
   IF gvisatidpermanad = TRUE THEN DO:
      FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.
         ftro = FALSE.
         RUN ftro_UI.                        
         /*Färdigrapportering*/
          /*forfärdig*/
         IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "GKAL" OR globforetag = "LULE"    THEN DO:
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = "" AND TIDREGITAB.DATUM < hjdat AND
            ( TIDREGITAB.GODKAND = ""   OR  TIDREGITAB.GODKAND BEGINS "F")
            USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.   
         ELSE DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.GODKAND = ""   
            USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.           
         IF AVAILABLE TIDREGITAB THEN DO:            
            /* OM DET FINNS EJ FÄRDIGA MÅNADER UTAN TIDSKRIVNING*/
            FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = tidpers.PERSONALKOD AND
            GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND GODKOLL.DATMAN < MONTH(TIDREGITAB.DATUM)
            AND GODKOLL.KLAR = FALSE NO-LOCK NO-ERROR.
            IF AVAILABLE GODKOLL THEN DO:               
               ASSIGN
               regar = GODKOLL.DATAR
               regmnr = GODKOLL.DATMAN.                              
            END.
            ELSE DO:  
               ASSIGN
               regar = YEAR(TIDREGITAB.DATUM)
               regmnr = MONTH(TIDREGITAB.DATUM).                              
            END.   
            RUN MANNAMN.P.                     
            IF listar < regar THEN regar = regar.
            ELSE IF listar = regar AND listmnr < regmnr THEN regar = regar.
            ELSE DO:
               CREATE tidut.           
               ASSIGN    
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN            
               SUBSTRING(tidut.UT,51) = tidpers.OMRADE
               SUBSTRING(tidut.UT,58) = regmannamn.
               IF NOT AVAILABLE GODKOLL THEN DO:
                   /*forfärdig*/
                  IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV"  OR globforetag = "GKAL" OR globforetag = "LULE"  THEN DO:
                     FIND FIRST tidbuff WHERE 
                     tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                     tidbuff.GODKAND BEGINS "F" AND YEAR(tidbuff.DATUM) = regar
                     AND MONTH(tidbuff.DATUM) = regmnr
                     USE-INDEX PSTART NO-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff  THEN 
                     SUBSTRING(tidut.UT,76) = SUBSTRING(tidbuff.GODKAND,1,1).
                     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                     IF AVAILABLE PERSONALTAB THEN
                     SUBSTRING(tidut.UT,78) = SUBSTRING(PERSONALTAB.TIDSGODK,1,5).
                     
                  END.
               END.   
               FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = tidpers.PERSONALKOD AND
               GODKOLL.DATAR = regar AND 
               GODKOLL.DATMAN = regmnr 
               USE-INDEX PKODAR NO-LOCK NO-ERROR.
               IF AVAILABLE GODKOLL THEN DO:
                  SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
               END.                                                       
            END.   
            REPEAT:                            
               regmnr = regmnr + 1.
               IF regmnr = 13 THEN DO:
                  ASSIGN
                  regmnr = 1
                  regar = regar + 1.
               END.               
               IF listar < regar THEN LEAVE.
               IF listar = regar AND listmnr < regmnr THEN LEAVE.
                /*forfärdig*/
               IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:                
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(TIDREGITAB.DATUM) = regar AND  
                  MONTH(TIDREGITAB.DATUM) = regmnr AND 
                  TIDREGITAB.VECKOKORD = "" AND
                  (TIDREGITAB.GODKAND = "" OR  
                   TIDREGITAB.GODKAND BEGINS "F")   
                  USE-INDEX PSTART NO-LOCK NO-ERROR.                              
               END.
               ELSE DO:
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(TIDREGITAB.DATUM) = regar AND  
                  MONTH(TIDREGITAB.DATUM) = regmnr AND 
                  TIDREGITAB.GODKAND = ""  USE-INDEX PSTART NO-LOCK NO-ERROR.
               END.      
               IF AVAILABLE TIDREGITAB THEN DO:                  
                  RUN MANNAMN.P. 
                  CREATE tidut.           
                  ASSIGN    
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN                  
                  SUBSTRING(tidut.UT,51) = tidpers.OMRADE
                  SUBSTRING(tidut.UT,58) = regmannamn.
                  FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                  GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND 
                  GODKOLL.DATMAN = MONTH(TIDREGITAB.DATUM) 
                  USE-INDEX PKODAR NO-LOCK NO-ERROR.
                  IF AVAILABLE GODKOLL THEN DO:
                     SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                  END.                        
                   /*forfärdig*/
                  IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:                   
                     SUBSTRING(tidut.UT,76) = SUBSTRING(TIDREGITAB.GODKAND,1,1).
                     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                     IF AVAILABLE PERSONALTAB THEN
                     SUBSTRING(tidut.UT,78) = SUBSTRING(PERSONALTAB.TIDSGODK,1,5).
                  END.
               END.
               
            END.                          
            IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV"  OR globforetag = "lule"  THEN DO:
                              
               IF listmnr GE 2 THEN DO:
                  bakmnr = listmnr.
                  REPEAT:
                     bakmnr = bakmnr - 1.
                     IF bakmnr < 1 THEN LEAVE.
                     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                     IF AVAILABLE PERSONALTAB THEN DO:
                        IF PERSONALTAB.ANSTALLNING = "Ej tidskrivande personal" THEN .
                        ELSE DO:                     
                           FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = tidpers.PERSONALKOD AND
                           GODKOLL.DATAR = listar AND 
                           GODKOLL.DATMAN = bakmnr 
                           USE-INDEX PKODAR NO-LOCK NO-ERROR.
                           IF NOT AVAILABLE GODKOLL THEN DO:
                              /*om en person har saknar tid flera månader bakåt. Görs bara om det finns någon gammaL tid registrerad, 
                              alltså borde det inte drabba nyanställd */
                              IF ftro = TRUE THEN .
                              ELSE DO:  
                                 FIND FIRST TIDREGITAB WHERE 
                                 TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                                 YEAR(TIDREGITAB.DATUM) = listar AND MONTH(TIDREGITAB.DATUM) < bakmnr
                                 AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.VECKOKORD = ""  USE-INDEX PSTART NO-LOCK NO-ERROR.
                                 IF AVAILABLE TIDREGITAB THEN DO:
                                    regmnr = bakmnr.                                 
                                    RUN MANNAMN.P.                        
                                    FIND FIRST TIDREGITAB WHERE 
                                    TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                                    YEAR(TIDREGITAB.DATUM) = listar AND MONTH(TIDREGITAB.DATUM) = bakmnr
                                    AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.VECKOKORD = ""  USE-INDEX PSTART NO-LOCK NO-ERROR.
                                    IF AVAILABLE TIDREGITAB THEN.
                                    ELSE DO:                                 
                                       FIND FIRST tidut WHERE SUBSTRING(tidut.UT,1,6) = tidpers.PERSONALKOD AND
                                       SUBSTRING(tidut.UT,58) = regmannamn  NO-LOCK NO-ERROR.
                                       IF NOT AVAILABLE tidut THEN DO:
                                          CREATE tidut.           
                                          ASSIGN    
                                          SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                                          SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                                          SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN            
                                          SUBSTRING(tidut.UT,51) = tidpers.OMRADE
                                          SUBSTRING(tidut.UT,58) = regmannamn
                                          SUBSTRING(tidut.UT,68) = "ingen tid".                  
                                       END.
                                    END.
                                 END.
                              END.                            
                           END. 
                        END.
                     END.
                  END.
               END.
            END.
            
         END.
         ELSE IF NOT AVAILABLE TIDREGITAB THEN DO:        
             /*forfärdig*/             
            IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV"  OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               YEAR(TIDREGITAB.DATUM) = listar AND MONTH(TIDREGITAB.DATUM) = listmnr
               AND TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:
                  IF ftro = TRUE THEN.
                  ELSE DO:
                     regmnr = listmnr.
                     RUN MANNAMN.P.
                     FIND FIRST tidut WHERE SUBSTRING(tidut.UT,1,6) = tidpers.PERSONALKOD AND
                     SUBSTRING(tidut.UT,58) = regmannamn  NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE tidut THEN DO:                     
                        CREATE tidut.           
                        ASSIGN    
                        SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                        SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                        SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN            
                        SUBSTRING(tidut.UT,51) = tidpers.OMRADE
                        SUBSTRING(tidut.UT,58) = regmannamn
                        SUBSTRING(tidut.UT,68) = "ingen tid".                  
                        FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                        IF AVAILABLE PERSONALTAB THEN
                        SUBSTRING(tidut.UT,78) = SUBSTRING(PERSONALTAB.TIDSGODK,1,5).
                     END.
                  END.   
               END.
               ELSE DO:
                  FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                  GODKOLL.DATAR = listar AND 
                  GODKOLL.DATMAN = listmnr 
                  USE-INDEX PKODAR NO-LOCK NO-ERROR.
                  IF AVAILABLE GODKOLL THEN DO:
                     IF GODKOLL.KLAR = FALSE THEN DO:
                        musz = FALSE.               
                        IF MONTH(TODAY) > listmnr THEN musz = TRUE.
                        ELSE IF GODKOLL.DATUM < (TODAY - 7) THEN musz = TRUE.                         
                        IF musz = TRUE THEN DO:
                           musz = FALSE.
                           regmnr = listmnr.
                           RUN MANNAMN.P.
                           CREATE tidut.           
                           ASSIGN    
                           SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                           SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                           SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN            
                           SUBSTRING(tidut.UT,51) = tidpers.OMRADE
                           SUBSTRING(tidut.UT,58) = regmannamn
                           SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                           FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                           IF AVAILABLE PERSONALTAB THEN
                           SUBSTRING(tidut.UT,78) = SUBSTRING(PERSONALTAB.TIDSGODK,1,5).
                        END.                            
                     END.
                  END.
               END.               
               IF ftro = TRUE THEN .
               ELSE DO:                  
                  /*om en person har saknar tid flera månader bakåt. Görs bara om det finns någon gammaL tid registrerad, 
                  alltså borde det inte drabba nyanställd */
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(TIDREGITAB.DATUM) = listar AND MONTH(TIDREGITAB.DATUM) < listmnr
                  AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF AVAILABLE TIDREGITAB THEN DO:
                     FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                     FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
                     gamman = listmnr.
                     IF AVAILABLE PERSONALTAB THEN DO:
                        IF AVAILABLE FLEXAVT THEN DO:                                       
                           REPEAT:                  
                              gamman = gamman - 1.
                              IF gamman < 1 THEN LEAVE.
                              IF MONTH(TIDREGITAB.DATUM) > gamman  THEN LEAVE.
                              IF PERSONALTAB.ANSTALLNING = "Ej tidskrivande personal" THEN LEAVE.
                              IF FLEXAVT.FLEXTID = TRUE THEN LEAVE.
                              
                              FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                              GODKOLL.DATAR = listar AND 
                              GODKOLL.DATMAN = gamman USE-INDEX PKODAR NO-LOCK NO-ERROR.
                              IF NOT AVAILABLE GODKOLL THEN DO:
                                 regmnr = gamman.
                                 RUN MANNAMN.P.
                                 FIND FIRST kollut WHERE SUBSTRING(kollut.UT,1,6) = tidpers.PERSONALKOD AND
                                 SUBSTRING(kollut.UT,58) = regmannamn  NO-LOCK NO-ERROR.
                                 IF NOT AVAILABLE kollut THEN DO:                     
                                    CREATE kollut.           
                                    ASSIGN    
                                    SUBSTRING(kollut.UT,1) = tidpers.PERSONALKOD
                                    SUBSTRING(kollut.UT,9) = tidpers.FORNAMN
                                    SUBSTRING(kollut.UT,25) = tidpers.EFTERNAMN            
                                    SUBSTRING(kollut.UT,51) = tidpers.OMRADE
                                    SUBSTRING(kollut.UT,58) = regmannamn
                                    SUBSTRING(kollut.UT,68) = "ingen tid".                                                
                                    IF AVAILABLE PERSONALTAB THEN
                                    SUBSTRING(kollut.UT,78) = SUBSTRING(PERSONALTAB.TIDSGODK,1,5).                                                     
                                 END.
                              END.
                           END.
                        END.
                     END.
                  END.
               END.   
            END.                              
         END.
      END.               
   END.
   ELSE DO:
      FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:  
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.        
         FIND FIRST TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
         TIDREGITAB.VECKONUMMER <= regvnr AND 
         TIDREGITAB.GODKAND = ""   
         USE-INDEX GODGAMLA NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE TIDREGITAB THEN NEXT. 
         ELSE DO:                        
            tidtabrec = RECID(TIDREGITAB).
            CREATE tidut.           
            ASSIGN    
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
            SUBSTRING(tidut.UT,51) = tidpers.OMRADE
            SUBSTRING(tidut.UT,58) = STRING(TIDREGITAB.VECKONUMMER).             
         END.      
         FIND tidbuf WHERE RECID(tidbuf) = tidtabrec NO-LOCK NO-ERROR.
         REPEAT:
            FIND NEXT TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.VECKONUMMER <= regvnr AND  
            TIDREGITAB.VECKONUMMER > tidbuf.VECKONUMMER AND 
            TIDREGITAB.GODKAND = ""   
            USE-INDEX GODGAMLA NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB THEN LEAVE. 
            ELSE DO:               
               tidtabrec = RECID(TIDREGITAB).               
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,51) = tidpers.OMRADE
               SUBSTRING(tidut.UT,58) = STRING(TIDREGITAB.VECKONUMMER).                                  
               FIND tidbuf WHERE RECID(tidbuf) = tidtabrec NO-LOCK NO-ERROR.
            END.
         END. 
      END.       
   END. 
   FIND LAST tidut NO-LOCK NO-ERROR.  
  
END PROCEDURE.

PROCEDURE huvud_UI :
     /*HUVUD*/        
   musz = FALSE.     
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,60) = STRING(TODAY)
   SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.
   IF globforetag = "LULE" THEN DO: 
      ASSIGN 
      SUBSTRING(tidut.UT,2) = "LISTNING AV ALLA GODKÄNDA EJ  LÖNESAMMANSTÄLLDA TIDSEDLAR".
   END.
   ELSE DO:
      ASSIGN 
      SUBSTRING(tidut.UT,2) = "LISTNING AV ALLA GODKÄNDA EJ EKONOMI- OCH LÖNESAMMANSTÄLLDA TIDSEDLAR".
   END.
   CREATE tidut.                         
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "ENHET/" 
   SUBSTRING(tidut.UT,61) = "GODKÄND". 
   IF gvisatidpermanad = TRUE THEN DO:
      SUBSTRING(tidut.UT,61,7) = "".      
      SUBSTRING(tidut.UT,68) = "GODKÄND T.O.M".
   END.   
   CREATE tidut.                         
   ASSIGN
   SUBSTRING(tidut.UT,1) = "SIGN"
   SUBSTRING(tidut.UT,8) = "FÖRNAMN"
   SUBSTRING(tidut.UT,24) = "EFTERNAMN"   
   SUBSTRING(tidut.UT,57) = "VNR"   
   SUBSTRING(tidut.UT,61) = "VNR".
   IF gvisatidpermanad = TRUE THEN DO:
      SUBSTRING(tidut.UT,57,7) = "".
      ASSIGN
      SUBSTRING(tidut.UT,57) = "MÅNAD"
      SUBSTRING(tidut.UT,68) = "DATUM".         
   END.  
   CREATE tidut.  
   ASSIGN tidut.UT = str.
   IF gvisatidpermanad = TRUE THEN DO:
      FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:  
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.          
         IF globforetag = "LULE" THEN DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.GODKAND BEGINS "G" AND
            TIDREGITAB.DATUM > 03/31/2005 AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "    
            USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.                             
         ELSE DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = ""   
            USE-INDEX PSTART NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE TIDREGITAB THEN DO:
            ASSIGN
            regar = YEAR(TIDREGITAB.DATUM)
            regmnr = MONTH(TIDREGITAB.DATUM).
            RUN MANNAMN.P. 
            IF listar < regar THEN regar = regar.
            ELSE IF listar = regar AND listmnr < regmnr THEN regar = regar.
            ELSE DO:
               CREATE tidut.           
               ASSIGN    
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN            
               SUBSTRING(tidut.UT,51) = tidpers.OMRADE
               SUBSTRING(tidut.UT,58) = regmannamn.               
                /*forfärdig*/                
               IF globforetag = "gkal" OR globforetag = "sund" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "LULE" THEN DO:                                    
                  FIND FIRST tidbuff WHERE 
                  tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(tidbuff.DATUM) = YEAR(TIDREGITAB.DATUM)  AND  
                  MONTH(tidbuff.DATUM) = MONTH(TIDREGITAB.DATUM)  AND 
                  tidbuff.GODKAND = "F" AND tidbuff.VECKOKORD = ""   
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:                                  
                     IF globforetag = "LULE" THEN DO: 
                        FIND LAST tidbuff WHERE 
                        tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                        YEAR(tidbuff.DATUM) = YEAR(TIDREGITAB.DATUM)  AND  
                        MONTH(tidbuff.DATUM) = MONTH(TIDREGITAB.DATUM)  AND 
                        tidbuff.GODKAND BEGINS "G" AND
                        tidbuff.DATUM > 03/31/2005 AND SUBSTRING(tidbuff.VECKOKORD,10,9) = ""   
                        USE-INDEX PSTART NO-LOCK NO-ERROR.                       
                     END.                             
                     ELSE DO: 
                        FIND LAST tidbuff WHERE 
                        tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                        YEAR(tidbuff.DATUM) = YEAR(TIDREGITAB.DATUM)  AND  
                        MONTH(tidbuff.DATUM) = MONTH(TIDREGITAB.DATUM)  AND 
                        tidbuff.GODKAND BEGINS "G" AND tidbuff.VECKOKORD = ""   
                        USE-INDEX PSTART NO-LOCK NO-ERROR.
                     END.
                     IF AVAILABLE tidbuff THEN DO:                        
                        SUBSTRING(tidut.UT,68) = STRING(tidbuff.DATUM).
                     END.                     
                  END.
                  ELSE DO:                     
                     FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                     GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND 
                     GODKOLL.DATMAN = MONTH(TIDREGITAB.DATUM) 
                     USE-INDEX PKODAR NO-LOCK NO-ERROR.
                     IF AVAILABLE GODKOLL THEN DO:                        
                        SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                     END.                       
                  END.
               END.
               ELSE DO:               
                  FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                  GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND 
                  GODKOLL.DATMAN = MONTH(TIDREGITAB.DATUM) 
                  USE-INDEX PKODAR NO-LOCK NO-ERROR.
                  IF AVAILABLE GODKOLL THEN DO:
                     SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                  END.                       
               END.
            END.   
            REPEAT:
               regmnr = regmnr + 1.
               IF regmnr = 13 THEN DO:
                  ASSIGN
                  regmnr = 1
                  regar = regar + 1.
               END.                 
               IF listar < regar THEN LEAVE.
               IF listar = regar AND listmnr < regmnr THEN LEAVE.
               IF globforetag = "LULE" THEN DO: 
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(TIDREGITAB.DATUM) = regar AND  
                  MONTH(TIDREGITAB.DATUM) = regmnr AND 
                  TIDREGITAB.GODKAND BEGINS "G" AND
                  TIDREGITAB.DATUM > 03/31/2005 AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "    
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
               END.                             
               ELSE DO: 
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  YEAR(TIDREGITAB.DATUM) = regar AND  
                  MONTH(TIDREGITAB.DATUM) = regmnr AND 
                  TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = ""   
                  USE-INDEX PSTART NO-LOCK NO-ERROR.                            
               END.
               IF AVAILABLE TIDREGITAB THEN DO:
                  RUN MANNAMN.P. 
                  CREATE tidut.           
                  ASSIGN    
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,9) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,25) = tidpers.EFTERNAMN                  
                  SUBSTRING(tidut.UT,51) = tidpers.OMRADE
                  SUBSTRING(tidut.UT,58) = regmannamn.                  
                   /*forfärdig*/
                  IF globforetag = "gkal" OR globforetag = "sund" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "LULE"  THEN DO:                                        
                     FIND FIRST tidbuff WHERE 
                     tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                     YEAR(tidbuff.DATUM) = regar AND  
                     MONTH(tidbuff.DATUM) = regmnr AND 
                     tidbuff.GODKAND = "F" AND tidbuff.VECKOKORD = ""   
                     USE-INDEX PSTART NO-LOCK NO-ERROR.
                     IF AVAILABLE tidbuff THEN DO:                                           
                        IF globforetag = "LULE" THEN DO: 
                           FIND LAST tidbuff WHERE 
                           tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                           YEAR(tidbuff.DATUM) = regar AND  
                           MONTH(tidbuff.DATUM) = regmnr AND 
                           tidbuff.GODKAND BEGINS "G" AND
                           tidbuff.DATUM > 03/31/2005 AND SUBSTRING(tidbuff.VECKOKORD,10,9) = ""   
                           USE-INDEX PSTART NO-LOCK NO-ERROR.                       
                        END.                             
                        ELSE DO: 
                           FIND LAST tidbuff WHERE 
                           tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND 
                           YEAR(tidbuff.DATUM) = regar AND  
                           MONTH(tidbuff.DATUM) = regmnr AND 
                           tidbuff.GODKAND BEGINS "G" AND tidbuff.VECKOKORD = ""   
                           USE-INDEX PSTART NO-LOCK NO-ERROR.
                        END.
                        IF AVAILABLE tidbuff THEN DO:                           
                           SUBSTRING(tidut.UT,68) = STRING(tidbuff.DATUM).
                        END.                     
                     END.
                     ELSE DO:
                        FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                        GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND 
                        GODKOLL.DATMAN = MONTH(TIDREGITAB.DATUM) 
                        USE-INDEX PKODAR NO-LOCK NO-ERROR.
                        IF AVAILABLE GODKOLL THEN DO:
                           SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                        END.                       
                     END.
                  END.
                  ELSE DO:               
                     FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
                     GODKOLL.DATAR = YEAR(TIDREGITAB.DATUM) AND 
                     GODKOLL.DATMAN = MONTH(TIDREGITAB.DATUM) 
                     USE-INDEX PKODAR NO-LOCK NO-ERROR.
                     IF AVAILABLE GODKOLL THEN DO:
                        SUBSTRING(tidut.UT,68) = STRING(GODKOLL.DATUM).
                     END.
                  END.
               END.
            END.
         END.
      END.      
   END.
   ELSE DO:      
      FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK: 
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.
         IF globforetag = "LULE" THEN DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.VECKONUMMER <= regvnr AND 
            TIDREGITAB.GODKAND BEGINS "G" AND
            TIDREGITAB.DATUM > 03/31/2005 AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "    
            USE-INDEX PVNR NO-LOCK NO-ERROR.
         END.                             
         ELSE DO: 
            FIND FIRST TIDREGITAB WHERE 
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.VECKONUMMER <= regvnr AND 
            TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = ""  
            USE-INDEX PVNR NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE TIDREGITAB THEN NEXT. 
         ELSE DO:                        
            tidtabrec = RECID(TIDREGITAB).
            CREATE tidut.               
            ASSIGN SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,8) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,24) = tidpers.EFTERNAMN    
            SUBSTRING(tidut.UT,57) = STRING(TIDREGITAB.VECKONUMMER) 
            SUBSTRING(tidut.UT,61) = SUBSTRING(TIDREGITAB.GODKAND,1,4).            
         END.   
         FIND tidbuf WHERE RECID(tidbuf) = tidtabrec NO-LOCK NO-ERROR.
         REPEAT:  
            IF globforetag = "LULE" THEN DO: 
               FIND NEXT TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.VECKONUMMER <= regvnr AND  
               TIDREGITAB.VECKONUMMER > tidbuf.VECKONUMMER AND 
               TIDREGITAB.GODKAND BEGINS "G" AND
               TIDREGITAB.DATUM > 03/31/2005 AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "    
               USE-INDEX PVNR NO-LOCK NO-ERROR.
            END.                             
            ELSE DO: 
               FIND NEXT TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.VECKONUMMER <= regvnr AND  
               TIDREGITAB.VECKONUMMER > tidbuf.VECKONUMMER AND 
               TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = ""  
               USE-INDEX PVNR NO-LOCK NO-ERROR.
            END.
            IF NOT AVAILABLE TIDREGITAB THEN LEAVE. 
            ELSE DO:                          
               tidtabrec = RECID(TIDREGITAB).
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,8) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,24) = tidpers.EFTERNAMN    
               SUBSTRING(tidut.UT,57) = STRING(TIDREGITAB.VECKONUMMER) 
               SUBSTRING(tidut.UT,61) = SUBSTRING(TIDREGITAB.GODKAND,1,4).               
               FIND tidbuf WHERE RECID(tidbuf) = tidtabrec NO-LOCK NO-ERROR.
            END. 
         END.   
      END.       
   END.      
   FIND LAST tidut NO-LOCK NO-ERROR.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
    edataapph = ?.
END PROCEDURE.

PROCEDURE ftro_UI :                                      
   /*förtroendetid skriver endast frånvaro*/      
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   ftro = FALSE.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FORTRO"                   
   inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
     ftro = extradatatemp.SOKLOG[1].         
   END.   
   ELSE ftro = FALSE. 
                                              
END PROCEDURE.
