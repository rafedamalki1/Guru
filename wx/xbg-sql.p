/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: BG-SQL.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2004.04.27 13:54 ELPAO   
     Modified: 2004.10.28 08:47 ELPAO    
     Modified: 2005.02.14 14:59 ELPAO    
     Modified: 2005.10.04 13:56 ELPAO    
     Modified: 
*/

/*ansluter och hämtar tabell DEF*/


RUN hamtab_UI IN odbch (OUTPUT TABLE sqltab).

/*hämtar  kunder från web används ej*/
IF vadgora.VAD[1] = TRUE THEN DO:  
   uttext = "INKUNDER ".
   RUN ut_UI.   
   RUN startodbc_UI.
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_user" NO-LOCK NO-ERROR.
   kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[13] + " = 'bort'".
   RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat).
   FOR EACH sqldat:
      OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.E-POST = sqldat.DATAFALT[4] NO-LOCK.
      GET FIRST pq NO-LOCK.
      DO WHILE AVAILABLE(PERSONALTAB):
         FIND FIRST EXTRAPERS WHERE EXTRAPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE EXTRAPERS THEN DO:
            IF EXTRAPERS.SOKINT[2] = INTEGER(sqldat.DATAFALT[1]) THEN DO TRANSACTION:
               FIND CURRENT EXTRAPERS EXCLUSIVE-LOCK NO-ERROR.
               EXTRAPERS.SOKLOG[3] = TRUE.
            END.
         END.         
         GET NEXT pq NO-LOCK.
      END.
      
   END.
   EMPTY TEMP-TABLE sqldat NO-ERROR. 
   kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[13] + " = 'bort'".
   RUN bort_UI IN odbch (INPUT kommando).
   kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[13] + " = 'and'".
   RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat).
   FOR EACH sqldat:
      pkod = "".
      EMPTY TEMP-TABLE pkodtemp NO-ERROR. 
      OPEN QUERY pq FOR EACH PERSONALTAB WHERE PERSONALTAB.E-POST = sqldat.DATAFALT[4] NO-LOCK.
      GET FIRST pq NO-LOCK.
      DO WHILE AVAILABLE(PERSONALTAB):
         FIND FIRST EXTRAPERS WHERE EXTRAPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE EXTRAPERS THEN DO:
            IF EXTRAPERS.SOKINT[2] = INTEGER(sqldat.DATAFALT[1]) THEN DO:
               CREATE pkodtemp.
               pkodtemp.PERSONALKOD = EXTRAPERS.PERSONALKOD.
            END.             
         END.         
         GET NEXT pq NO-LOCK.
      END.
      FOR EACH pkodtemp:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkodtemp.PERSONALKOD NO-LOCK NO-ERROR.
         CREATE ktemp.           
         IF AVAILABLE PERSONALTAB THEN DO:
            /*
            ASSIGN
            ktemp.ANM         = PERSONALTAB.ANM
            ktemp.BOX         = PERSONALTAB.BOX 
            ktemp.RINGT       = PERSONALTAB.RINGT    
            ktemp.RINGN       = PERSONALTAB.RINGN    
            ktemp.SENASTFAX   = PERSONALTAB.SENASTFAX
            ktemp.FORETAG     = PERSONALTAB.FORETAG.  
            */
         END.      
         ASSIGN
         ktemp.KUNDNR      =    pkodtemp.PERSONALKOD
         ktemp.FORNAMN     = sqldat.DATAFALT[2]
         ktemp.EFTERNAMN   = sqldat.DATAFALT[3]
         ktemp.GATUADRESS  = sqldat.DATAFALT[6]
         ktemp.POSTNUMMER  = sqldat.DATAFALT[8]
         ktemp.POSTADRESS  = sqldat.DATAFALT[7]
         ktemp.TELEFON     = sqldat.DATAFALT[10]
         ktemp.PERSONSOK   = sqldat.DATAFALT[9]
         ktemp.MOBILTEL    = sqldat.DATAFALT[11]
         ktemp.E-POST      = sqldat.DATAFALT[4]
         ktemp.AKTIV       = "JA"
         ktemp.ANVANDARE   = "sqlserver"
         ktemp.PART        = "JA"
         ktemp.SQLID       =  INTEGER(sqldat.DATAFALT[1]).
      END.
   END.
   EMPTY TEMP-TABLE sqldat NO-ERROR. 
   FOR EACH ktemp:
      FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_soker" NO-LOCK NO-ERROR.
      kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[1] + " = " + STRING(ktemp.SQLID).
      RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat). 
      FOR EACH sqldat:                             
         ASSIGN
         ktemp.RUM       = sqldat.DATAFALT[3]
         ktemp.RUM2      = sqldat.DATAFALT[4]
         ktemp.MINBOBO   = sqldat.DATAFALT[5]
         ktemp.MAXBOBO   = sqldat.DATAFALT[6]
         ktemp.MINPRISBO = sqldat.DATAFALT[7]          
         ktemp.BPRIS     = sqldat.DATAFALT[8] 
         ktemp.BOAREA    = sqldat.DATAFALT[9]
         ktemp.MAXBOVI   = sqldat.DATAFALT[10]
         ktemp.MINPRISVI = sqldat.DATAFALT[11]
         ktemp.VPRIS     = sqldat.DATAFALT[12].                
      END.
      EMPTY TEMP-TABLE sqldat NO-ERROR. 
      FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_finns" NO-LOCK NO-ERROR.
      kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[1] + " = " + STRING(ktemp.SQLID).
      RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat). 
      FOR EACH sqldat:   
         /*
         FIND FIRST NO-LOCK NO-ERROR.
         CREATE ptemp.

      Botyp
               Omrid
               TotAntalrum
               sovrum
               Boarea
               Biarea
               Totarea
               Tomtarea
               Fpris
               Byggar
                        FIELD PERSONALKOD AS CHARACTER  
                  FIELD POOLEN AS CHARACTER
                  FIELD AKTIV AS CHARACTER
                  FIELD ANDATUM    AS CHARACTER
                  FIELD GATADR     AS CHARACTER
                  FIELD POOLPRIS   AS CHARACTER
                  FIELD MANAVG     AS CHARACTER
                  FIELD RUM        AS CHARACTER
                  FIELD SOVRUM     AS CHARACTER
                  FIELD BOAREA     AS CHARACTER
                  FIELD BIYTA      AS CHARACTER
                  FIELD TOTYTA     AS CHARACTER
                  FIELD TOMTYTA    AS CHARACTER
                  FIELD BOTYP      AS CHARACTER
                  FIELD STADSDEL   AS CHARACTER
                  FIELD OMRADE     AS CHARACTER
                  FIELD KRAV       AS CHARACTER
                  FIELD ONSKAR     AS CHARACTER
                  FIELD FINNS      AS CHARACTER
                  FIELD BYGGAR     AS CHARACTER
                  FIELD SQLID       AS INTEGER
                  INDEX PKOD IS PRIMARY PERSONALKOD. 
                        ASSIGN
                        ktemp.RUM       = sqldat.DATAFALT[3]
                        ktemp.RUM2      = sqldat.DATAFALT[4]
                        ktemp.MINBOBO   = sqldat.DATAFALT[5]
                        ktemp.MAXBOBO   = sqldat.DATAFALT[6]
                        ktemp.MINPRISBO = sqldat.DATAFALT[7]          
                        ktemp.BPRIS     = sqldat.DATAFALT[8] 
                        ktemp.BOAREA    = sqldat.DATAFALT[9]
                        ktemp.MAXBOVI   = sqldat.DATAFALT[10]
                        ktemp.MINPRISVI = sqldat.DATAFALT[11]
                        ktemp.VPRIS     = sqldat.DATAFALT[12].                
                    
               */
       END.
      EMPTY TEMP-TABLE sqldat NO-ERROR. 
   END.
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_user" NO-LOCK NO-ERROR.
   kommando = sqltab.FALT[13] + " WHERE " + sqltab.FALT[13] + " = 'and'". 
   RUN andra_UI IN odbch (INPUT sqltab.TABNAMN,INPUT kommando).
   
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_user" NO-LOCK NO-ERROR.
   kommando = sqltab.TABNAMN + " WHERE " + sqltab.FALT[13] + " = 'ny'".
   RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat).
   FOR EACH sqldat:
      /*skapa temp-filer*/
      
   END.
   EMPTY TEMP-TABLE sqldat NO-ERROR. 
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "eo_user" NO-LOCK NO-ERROR.
   kommando = sqltab.FALT[13] + " WHERE " + sqltab.FALT[13] + " = 'ny'". 
   RUN andra_UI IN odbch (INPUT sqltab.TABNAMN,INPUT kommando).
   RUN avslut_UI IN odbch.
   DELETE PROCEDURE odbch.                 
   
   uttext = "INKUNDER OK ".
   RUN ut_UI.

END.


/*TABELLER
Eo_finns
Eo_omr_user
Userid 
Email
Omrid
botyp
*/

/*lev Bt-objekt SKICKAR BT OBJEKT*/
IF vadgora.VAD[2] = TRUE THEN DO:  
   
   uttext = "BT-OBJEKT ".
   /*LOGG*/
   RUN ut_UI.
   EMPTY TEMP-TABLE resultatobj NO-ERROR. 
   /*SKAPA TEMP TABELL FÖR VARJE AKTUELLT OBJEKT*/ 
   FOR EACH HUSOBJ WHERE HUSOBJ.ANDATUM >= objdat NO-LOCK:
      CREATE husobjtemp.
      BUFFER-COPY HUSOBJ TO husobjtemp.      
   END.
   /*SUMMERA ALLA OBJEKT PÅ PRIS OMRÅDE RUM*/
   FOR EACH husobjtemp:
      RUN prisint_UI (INPUT husobjtemp.PRIS).
      FIND FIRST resultatobj WHERE resultatobj.OMRADE = husobjtemp.STADSDEL AND 
      resultatobj.BOTYP = husobjtemp.BOTYP AND
      resultatobj.RUM = husobjtemp.RUM AND resultatobj.MINPRIS = prislag * 1000 AND 
      resultatobj.MAXPRIS = prishog * 1000 
      NO-ERROR.     
      IF NOT AVAILABLE resultatobj THEN CREATE resultatobj.
      ASSIGN
      resultatobj.OMRADE = husobjtemp.STADSDEL   
      resultatobj.BOTYP  = husobjtemp.BOTYP 
      resultatobj.RUM     = husobjtemp.RUM
      resultatobj.MANAVG = husobjtemp.MANAVG
      resultatobj.ANTAL   = resultatobj.ANTAL + 1  
      resultatobj.MINPRIS = prislag * 1000
      resultatobj.MAXPRIS = prishog * 1000.
   END.
   /*TA BORT FEL REGISTRERINGAR*/
   FOR EACH resultatobj WHERE resultatobj.OMRADE = "":
      DELETE resultatobj.
   END.
   FOR EACH resultatobj WHERE resultatobj.BOTYP = "":
      DELETE resultatobj.
   END.
   /*SKICKA DATA TILL RÄTT TABELL OCH PÅ RÄTT FORMAT*/
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "Eo_resultbt" NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE sqldat  NO-ERROR.
   FOR EACH resultatobj:
      CREATE sqldat.
      ASSIGN
      sqldat.TABNAMN = sqltab.TABNAMN
      sqldat.DATAFALT[1] = "0"
      sqldat.DATAFALT[1 + 1] = "'" + resultatobj.OMRADE + "'" 
      sqldat.DATAFALT[2 + 1] = "'" + resultatobj.BOTYP + "'" 
      sqldat.DATAFALT[3 + 1] = STRING(resultatobj.RUM)    
      sqldat.DATAFALT[4 + 1] = STRING(resultatobj.ANTAL)  
      sqldat.DATAFALT[5 + 1] = STRING(resultatobj.MINPRIS)
      sqldat.DATAFALT[6 + 1] = STRING(resultatobj.MAXPRIS)
      sqldat.DATAFALT[7 + 1] = STRING(resultatobj.MANAVG).
   END.
   /*ANSLUT*/
   RUN startodbc_UI.
   /*TA BORT TIDIGARE POSTER*/
   RUN bort_UI IN odbch (INPUT sqltab.TABNAMN).
   /*SKICKA ÖVER NY DATA*/
   RUN addp_UI IN odbch (INPUT sqltab.TABNAMN,INPUT TABLE sqltab,INPUT TABLE sqldat).
   /*STÄNG*/
   RUN slutodbc_UI.
   
   uttext = "BT-OBJEKT OK ".
   /*LOGG*/
   RUN ut_UI.
END.


/*lev SÖK-objekt SKICKAR UT SÖK OBJEKT*/
IF vadgora.VAD[3] = TRUE THEN DO:  
   
   uttext = "SÖK-OBJEKT ".
   /*LOGG*/
   RUN ut_UI.
   EMPTY TEMP-TABLE resultatobj NO-ERROR. 
   /*VILKA OBJEKT */
   FOR EACH POLOBJ WHERE POLOBJ.POOLEN = TRUE AND POLOBJ.ANDATUM >= objdat NO-LOCK, 
   EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = POLOBJ.PERSONALKOD  NO-LOCK,
   EACH EXTRAPERS WHERE EXTRAPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   EXTRAPERS.SOKLOG[3] = FALSE NO-LOCK:
      /*SUMMERA ALLA OBJEKT PÅ PRIS OMRÅDE RUM*/
      RUN prisint_UI (INPUT POLOBJ.POOLPRIS).      
      FIND FIRST resultatobj WHERE resultatobj.OMRADE = POLOBJ.STADSDEL AND 
      resultatobj.BOTYP = POLOBJ.BOTYP AND
      resultatobj.RUM = POLOBJ.RUM AND resultatobj.MINPRIS = prislag * 1000 AND 
      resultatobj.MAXPRIS = prishog * 1000 NO-ERROR.     
      IF NOT AVAILABLE resultatobj THEN CREATE resultatobj.
      ASSIGN
      resultatobj.OMRADE = POLOBJ.STADSDEL   
      resultatobj.BOTYP  = POLOBJ.BOTYP 
      resultatobj.RUM   = POLOBJ.RUM
      resultatobj.MANAVG = POLOBJ.MANAVG
      resultatobj.ANTAL = resultatobj.ANTAL + 1  
      resultatobj.MINPRIS = prislag * 1000
      resultatobj.MAXPRIS = prishog * 1000.
   END.
   /*TA BORT FEL REGISTRERINGAR*/
   FOR EACH resultatobj WHERE resultatobj.OMRADE = "":
      DELETE resultatobj.
   END.
   FOR EACH resultatobj WHERE resultatobj.BOTYP = "":
      DELETE resultatobj.
   END.
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "Eo_resultbb" NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE sqldat  NO-ERROR.
   /*SKICKA DATA TILL RÄTT TABELL OCH PÅ RÄTT FORMAT*/
   FOR EACH resultatobj:
      CREATE sqldat.
      ASSIGN
      sqldat.TABNAMN = sqltab.TABNAMN
      sqldat.DATAFALT[1] = "0"
      sqldat.DATAFALT[1 + 1] = "'" + resultatobj.OMRADE + "'"
      sqldat.DATAFALT[2 + 1] = "'" + resultatobj.BOTYP + "'"  
      sqldat.DATAFALT[3 + 1] = STRING(resultatobj.RUM)    
      sqldat.DATAFALT[4 + 1] = STRING(resultatobj.ANTAL)  
      sqldat.DATAFALT[5 + 1] = STRING(resultatobj.MINPRIS)
      sqldat.DATAFALT[6 + 1] = STRING(resultatobj.MAXPRIS)
      sqldat.DATAFALT[7 + 1] = STRING(resultatobj.MANAVG).
   END.
   /*ANSLUT*/
   RUN startodbc_UI.
   /*TA BORT GAMMLA*/
   RUN bort_UI IN odbch (INPUT sqltab.TABNAMN).
/*SKICKA ÖVER NY DATA*/
   RUN addp_UI IN odbch (INPUT sqltab.TABNAMN,INPUT TABLE sqltab,INPUT TABLE sqldat).
   /*STÄNG*/
   RUN slutodbc_UI.

   
   uttext = "SÖK-OBJEKT OK".
   /*LOGG*/
   RUN ut_UI.
END.
 /*FUNGERA PÅ SAMMA SÄTT SOM OVAN*/
/*lev sök-uppdrag*/
IF vadgora.VAD[4] = TRUE THEN DO:  
   
   uttext = "SÖK-KUND ".
   RUN ut_UI.
   FOR EACH EXTRAPERS WHERE 
   EXTRAPERS.SOKLOG[2] = TRUE AND EXTRAPERS.SOKLOG[3] = FALSE   
   /*AND EXTRAPERS.SOKDAT[1] >= objdat*/ NO-LOCK,
   EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = EXTRAPERS.PERSONALKOD NO-LOCK:
      FOR EACH OMRPERS WHERE OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK:
         
         IF OMRPERS.BOTYP = "BOSTADSRÄTTER" OR OMRPERS.BOTYP = "BRF-Radhus" OR OMRPERS.BOTYP = "Nyproduktion" THEN DO:
            RUN prisint_UI (INPUT PERSONALTAB.MINPRISBO).                             
            prishj1 = prislag.            
            RUN prisint_UI (INPUT PERSONALTAB.BPRIS).                             
            prishj2 = prishog.            
         END.
         ELSE DO:
            RUN prisint_UI (INPUT PERSONALTAB.MINPRISVI).                             
            prishj1 = prislag.            
            RUN prisint_UI (INPUT PERSONALTAB.VPRIS).                             
            prishj2 = prishog.            
         END.
         prislag = prishj1.                                                           
         prishog = prishj2.               
         IF prislag = 0 AND prishog = 0 THEN DO:
            ASSIGN
            prislag = 0
            prishog = 1000000.
         END.         
         FIND FIRST resultatobj WHERE resultatobj.OMRADE = OMRPERS.OMRADE AND 
         resultatobj.BOTYP = OMRPERS.BOTYP AND resultatobj.RUM = PERSONALTAB.RUM AND 
         resultatobj.MINPRIS = prislag * 1000 AND resultatobj.MAXPRIS = prishog * 1000  NO-ERROR.     
         IF NOT AVAILABLE resultatobj THEN CREATE resultatobj.
         ASSIGN
         resultatobj.OMRADE   = OMRPERS.OMRADE           
         resultatobj.BOTYP    = OMRPERS.BOTYP              
         resultatobj.RUM      = PERSONALTAB.RUM      
         resultatobj.ANTAL    = resultatobj.ANTAL + 1
         resultatobj.MINPRIS  = prislag * 1000
         resultatobj.MAXPRIS  = prishog * 1000.                     
      END.
   END.
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "Eo_resultso" NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE sqldat  NO-ERROR.
   FOR EACH resultatobj:
      CREATE sqldat.
      ASSIGN
      sqldat.TABNAMN = sqltab.TABNAMN
      sqldat.DATAFALT[1] = "0"
      sqldat.DATAFALT[1 + 1] = "'" + resultatobj.OMRADE + "'" 
      sqldat.DATAFALT[2 + 1] = "'" + resultatobj.BOTYP + "'"  
      sqldat.DATAFALT[3 + 1] = STRING(resultatobj.RUM)    
      sqldat.DATAFALT[4 + 1] = STRING(resultatobj.ANTAL)  
      sqldat.DATAFALT[5 + 1] = STRING(resultatobj.MINPRIS)
      sqldat.DATAFALT[6 + 1] = STRING(resultatobj.MAXPRIS).
   END.
   RUN startodbc_UI.
   RUN bort_UI IN odbch (INPUT sqltab.TABNAMN).
   RUN addp_UI IN odbch (INPUT sqltab.TABNAMN,INPUT TABLE sqltab,INPUT TABLE sqldat).
   RUN slutodbc_UI.
   
   uttext = "SÖK-KUND OK ".
   RUN ut_UI.
END.


/*bt-kunder*/
IF vadgora.VAD[5] = TRUE THEN DO:  
   
   uttext = "BT-KUND ".
   RUN ut_UI.
   FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE NO-LOCK,
   EACH EXTRAPERS WHERE EXTRAPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND EXTRAPERS.SOKLOG[3] = FALSE NO-LOCK:
      FOR EACH OMRPERS WHERE OMRPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK:
         IF OMRPERS.BOTYP = "BOSTADSRÄTTER" OR OMRPERS.BOTYP = "BRF-Radhus" OR OMRPERS.BOTYP = "Nyproduktion" THEN DO:
            RUN prisint_UI (INPUT PERSONALTAB.MINPRISBO).                             
            prishj1 = prislag.            
            RUN prisint_UI (INPUT PERSONALTAB.BPRIS).                             
            prishj2 = prishog.            
         END.
         ELSE DO:
            RUN prisint_UI (INPUT PERSONALTAB.MINPRISVI).                             
            prishj1 = prislag.            
            RUN prisint_UI (INPUT PERSONALTAB.VPRIS).                             
            prishj2 = prishog.            
         END.
         prislag = prishj1.                                                           
         prishog = prishj2.               
         IF prislag = 0 AND prishog = 0 THEN DO:
            ASSIGN
            prislag = 0                         
            prishog = 1000000.
         END.             

         FIND FIRST resultatobj WHERE resultatobj.OMRADE = OMRPERS.OMRADE AND 
         resultatobj.BOTYP = OMRPERS.BOTYP AND resultatobj.RUM = PERSONALTAB.RUM AND 
         resultatobj.MINPRIS = prislag * 1000 AND resultatobj.MAXPRIS = prishog * 1000  
         
         NO-ERROR.     
         IF NOT AVAILABLE resultatobj THEN CREATE resultatobj.
         ASSIGN
         resultatobj.OMRADE   = OMRPERS.OMRADE           
         resultatobj.BOTYP    = OMRPERS.BOTYP              
         resultatobj.RUM      = PERSONALTAB.RUM      
         resultatobj.ANTAL    = resultatobj.ANTAL + 1        
         resultatobj.MINPRIS  = prislag * 1000
         resultatobj.MAXPRIS  = prishog * 1000.                                       
      END.
   END.     
   FIND FIRST sqltab WHERE sqltab.TABNAMN = "Eo_resultbtk" NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE sqldat  NO-ERROR.
   FOR EACH resultatobj:
      IF resultatobj.OMRADE = "" OR resultatobj.BOTYP = "" THEN DELETE resultatobj.
      ELSE DO:
         CREATE sqldat.
         ASSIGN
         sqldat.TABNAMN = sqltab.TABNAMN
         sqldat.DATAFALT[1] = "0"
         sqldat.DATAFALT[1 + 1] = "'" + resultatobj.OMRADE + "'" 
         sqldat.DATAFALT[2 + 1] = "'" + resultatobj.BOTYP + "'"  
         sqldat.DATAFALT[3 + 1] = STRING(resultatobj.RUM)    
         sqldat.DATAFALT[4 + 1] = STRING(resultatobj.ANTAL)  
         sqldat.DATAFALT[5 + 1] = STRING(resultatobj.MINPRIS)
         sqldat.DATAFALT[6 + 1] = STRING(resultatobj.MAXPRIS).
      END.
   END.
   RUN startodbc_UI.
   RUN bort_UI IN odbch (INPUT sqltab.TABNAMN).
   RUN slutodbc_UI.
   DEFINE VARIABLE rr AS INTEGER    NO-UNDO.
   DEFINE VARIABLE rr1000 AS INTEGER    NO-UNDO.
   FOR EACH sqldat:
      CREATE esqldat. 
      BUFFER-COPY sqldat TO esqldat.
      DELETE sqldat.
      rr = rr + 1.
      IF rr > 1000 THEN DO:
         rr1000 = rr1000 + 1.                    
         uttext = "BT-KUND 1000 * ".
         RUN ut_UI.
         RUN startodbc_UI.
         RUN addp_UI IN odbch (INPUT sqltab.TABNAMN,INPUT TABLE sqltab,INPUT TABLE esqldat).   
         RUN slutodbc_UI.
         rr = 0.
         EMPTY TEMP-TABLE esqldat NO-ERROR. 
      END.
   END.
   FIND FIRST esqldat NO-LOCK NO-ERROR.
   IF AVAILABLE esqldat THEN DO:
      RUN startodbc_UI.
      RUN addp_UI IN odbch (INPUT sqltab.TABNAMN,INPUT TABLE sqltab,INPUT TABLE esqldat).   
      RUN slutodbc_UI.
   END.
   
   uttext = "BT-KUND OK ".
   RUN ut_UI.   
END.
PROCEDURE ut_UI:
   OUTPUT TO d:\delad\pro9S\autotid.txt  APPEND.
   PUT uttext " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE prisint_UI:
   DEFINE INPUT  PARAMETER prisvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE prisok AS LOGICAL    NO-UNDO.
   IF prisvar = 0 THEN DO:
      ASSIGN
      prislag = 0
      prishog = 0.
      RETURN.
   END.
   prislag = 0.
   prishog = 750.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 1000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 1250.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 1500.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 1750.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 2000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 2250.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 2500.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 2750.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 3000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 2250.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 3500.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 3750.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 4000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 6000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 10000.
   RUN priskoll_UI (INPUT prisvar,OUTPUT prisok).
   IF prisok = TRUE THEN RETURN.
   prislag = prishog.
   prishog = 1000000.   
END PROCEDURE.
PROCEDURE priskoll_UI:
   DEFINE INPUT  PARAMETER prisvar AS INTEGER    NO-UNDO.
   DEFINE OUTPUT PARAMETER prisok AS LOGICAL    NO-UNDO.       
   IF prisvar > prislag AND prisvar <= prishog THEN DO:
      prisok = TRUE.
      RETURN.
   END.
END PROCEDURE.


PROCEDURE startodbc_UI:
   /*
   RUN ODBC.P PERSISTENT SET odbch (INPUT "captst07.capitex.se", 
                                 INPUT "195.67.71.103",
                                 INPUT "erikolsson",
                                 INPUT "1VtQLw",
                                 OUTPUT TABLE feltemp).
                                 
   */    
   /*
   RUN ODBC.P PERSISTENT SET odbch (INPUT "yorick.capitex.se",
                                 INPUT "195.67.71.107",       
                                 INPUT "eo_result",           
                                 INPUT "FRiED6",              
                                 OUTPUT TABLE feltemp).      
*/                                 
RUN ODBC.P PERSISTENT SET odbch (INPUT "julia.capitex.se",
                                 INPUT "195.67.71.108",       
                                 INPUT "eo_result",           
                                 INPUT "FRiED6",              
                                 OUTPUT TABLE feltemp).      

   
END PROCEDURE.
PROCEDURE slutodbc_UI:
   RUN avslut_UI IN odbch.
   DELETE PROCEDURE odbch.                 
END PROCEDURE.

