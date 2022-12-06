
/*------------------------------------------------------------------------
    File        : AOMBAPPI.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 27 14:17:28 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE huvud_UI :  
   CREATE tidut. 
   SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
   CREATE tidut.
   CREATE tidut.
   tidut.UT = uppvaltemp.VALDLISTA. 
   IF uppvaltemp.VISPERAR = TRUE THEN DO: 
      SUBSTRING(tidut.UT,64) = "ÅR " + STRING(YEAR(uppvaltemp.STARTDATUM),"9999").
   END.                        
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      SUBSTRING(tidut.UT,64) = "PERIOD " +  STRING(uppvaltemp.STARTDATUM) + 
      " - " + STRING(uppvaltemp.SLUTDATUM).     
   END.
   ELSE DO:
      SUBSTRING(tidut.UT,64) = "VISNING AV ALLT".
   END.
   CREATE tidut.
   {KUURV.I}
   CREATE tidut.
   ASSIGN 
   tidut.UT = "UF=KOSTNADER FRÅN TIDREDOVISNING OCH KOSTNADSREGISTRERING, KA=KALKYLERING".      
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   nrcol[12] = 13
   nrcol[13] = 14
   nrcol[14] = 15 
   nrcol[15] = 12    /*ny kolumn*/

   nrcol[16] = 16 
   nrcol[17] = 17 
   nrcol[18] = 18 
   nrcol[19] = 19
   nrcol[20] = 20 

   breddantal = 20   /*antal kolumner*/
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 8
   bredd[8] = 7
   bredd[9] = 7
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 9
   bredd[15] = 9.
   /*
   bredd[16] = 8
   bredd[17] = 8
   bredd[18] = 8.
   */
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   CREATE tidut.  
   CREATE tidut.   
   CREATE tidut.             
   ASSIGN                                                                                                       
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "MONTÖR"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "BERED."         
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "ÖVRIGA"  
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "ARBETS"                                          
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "FRÄMMANDE TJ."
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "MATERIEL"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "ÖVRIG"
   SUBSTRING(tidut.UT,utnr[nrcol[15]]) = "SUMMA"
   SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "INTÄKT".
   IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "      ".
   END.
   str2 = tidut.UT.                 
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN                                                                                                       
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "UTRUST"             
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "UTLÄGG"     
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "INLEJDA  TJ."
      SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "TELEFON".      
   END.
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "TIMMAR"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "TIMMAR"         
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "TIMMAR"  
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "GRÄVARE"  
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "ÖVRIGA" 
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[15]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "INTÄKT"   
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "RESULTAT"
   SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "PLAN"
   SUBSTRING(tidut.UT,200) = CAPS(Guru.Konstanter:garbal)
   SUBSTRING(tidut.UT,220) = CAPS(Guru.Konstanter:gberel)
   SUBSTRING(tidut.UT,240) = CAPS(Guru.Konstanter:gprojl).
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:   
      SUBSTRING(tidut.UT,260) = "LÖNETILLÄGG".
      IF Guru.Konstanter:varforetypval[47] = 1 THEN  SUBSTRING(tidut.UT,280) = "ANNAT BOLAG".
   END.
   IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "INTE.INT"   
      SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "EX.INTÄK"
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "RESULTAT" .  
   END.

/*    IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN"  */
/*     OR Guru.Konstanter:globforetag = "GKAL" THEN DO:                */
/*       ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[14]]) = "OFFERTPRIS".           */
/*    END.                                                                    */
   IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "KOSTN."             
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "KOSTN."
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "KONSULT"                                            
      SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "ÖVRIGT". 
   END.
   str3 = tidut.UT.                
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KA".   
   FOR EACH valdaao: 
      FIND FIRST slutsum WHERE slutsum.AONR = valdaao.AONR AND
      slutsum.DELNR = valdaao.DELNR NO-ERROR.
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST kosttemp WHERE kosttemp.AONR = valdaao.AONR AND
         kosttemp.DELNR = valdaao.DELNR USE-INDEX AONR NO-ERROR.
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND
         kalksumsum.DELNR = kalksumsum.DELNR USE-INDEX AONR NO-ERROR.
         IF AVAILABLE kosttemp THEN musz = musz.                  
         ELSE IF AVAILABLE kalksumsum THEN musz = musz.
         ELSE NEXT.
         CREATE slutsum.
         ASSIGN
         slutsum.AONR  = valdaao.AONR 
         slutsum.DELNR = valdaao.DELNR
         slutsum.ORT   = valdaao.ORT.  
      END.   
      ELSE slutsum.ORT   = valdaao.ORT.
   END.
   FOR EACH slutsum USE-INDEX AONR:  
      DEBUGGER:SET-BREAK().    
      ASSIGN
      varin = 0
      varkalk = 0.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kalksumsum THEN DO:
         varkalk = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
         kalksumsum.MTRL + kalksumsum.OVRKR +  
         kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.         
      END.
      FIND FIRST kosttemp WHERE kosttemp.AONR = slutsum.AONR AND
      kosttemp.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kosttemp THEN DO:
         varin = kosttemp.INKOMST.
      END.
      IF slutsum.ANNATBOL = "" THEN DO:
         vartot = vartot + (varin - varkalk).
      END.   
      FIND FIRST valdaao WHERE valdaao.AONR = slutsum.AONR AND valdaao.DELNR = slutsum.DELNR NO-ERROR.
      CREATE tidut.                 
      IF NOT AVAILABLE kosttemp THEN DO:
         ASSIGN 
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2]).              
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF".                  
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.TIMMAR,">>>>>9").                                                          
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.BTIMMAR,">>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.OTIMMAR,">>>>>9").  
         SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP,">>>>>>>9").  
         SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(slutsum.BELOPP,">>>>>>>9").
         SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(0 - slutsum.BELOPP,"->>>>>>9").                  
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END. 
         IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:    
              
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slutsum.MBELOPP,">>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(slutsum.IBELOPP,">>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING((slutsum.BELOPP + slutsum.MBELOPP + slutsum.IBELOPP + slutsum.LONKOST),">>>>>>>9")
            SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(0 - (slutsum.BELOPP + slutsum.MBELOPP + slutsum.IBELOPP),"->>>>>>9").
            SUBSTRING(tidut.UT,260) = STRING(slutsum.LONKOST,">>>>>>>9").            
            IF slutsum.ANNATBOL NE "" THEN   SUBSTRING(tidut.UT,280) = slutsum.ANNATBOL.  
                   
         END.
         IF slutsum.ANNATBOL = "" THEN DO:
            /*CCC*/
            /*ANNARS UPPDELNING PÅ BOLAG*/       
            IF varin - varkalk > 0 THEN                                   
            SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
         END.   
         IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
            FIND FIRST intakttemp WHERE intakttemp.AONR = slutsum.AONR AND 
            intakttemp.DELNR = slutsum.DELNR
            NO-ERROR.
            IF NOT AVAILABLE intakttemp THEN CREATE intakttemp.
            ASSIGN
            intakttemp.AONR = slutsum.AONR  
            intakttemp.DELNR = slutsum.DELNR.
            IF intakttemp.INTINTAKT > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intakttemp.INTINTAKT,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intakttemp.INTINTAKT,"->>>>>>9").
            IF intakttemp.EXTERNINTAKT > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(intakttemp.EXTERNINTAKT,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(intakttemp.EXTERNINTAKT,"->>>>>>9").
            intakttemp.RESULTAT = (intakttemp.INTINTAKT + intakttemp.EXTERNINTAKT) - slutsum.BELOPP.                      
            IF intakttemp.RESULTAT > 0 THEN                                   
            SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(intakttemp.RESULTAT,"->>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(intakttemp.RESULTAT,"->>>>>>>9").           
         END.
         SUBSTRING(tidut.UT,150) = slutsum.ORT.
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.                   
         ASSIGN  
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BTIMMAR = sumsum.BTIMMAR + slutsum.BTIMMAR
         sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR
         sumsum.BELOPP =  sumsum.BELOPP + slutsum.BELOPP 
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - slutsum.BELOPP).         
         IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      
            ASSIGN
            sumsum.GRAVARE =  sumsum.GRAVARE + slutsum.MBELOPP 
            sumsum.MASKOVRIG =  sumsum.MASKOVRIG + slutsum.IBELOPP
            sumsum.LONKOST = sumsum.LONKOST + slutsum.LONKOST.
         END.
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])      
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"
         SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.OTIMMAR,">>>>>9").
         SUBSTRING(tidut.UT,150) = slutsum.ORT. 
         IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kosttemp.BELOPP1P,">>>>>9")                                                          
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kosttemp.BELOPP2P,">>>>>9").                        
         END.
         ELSE DO:            
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.TIMMAR,">>>>>9")                                                          
            SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.BTIMMAR,">>>>>9").            
         END.
         MESSAGE "1"
         VIEW-AS ALERT-BOX.
         IF kosttemp.BELOPP < 0                                                             
         THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP + kosttemp.BELOPP,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(slutsum.BELOPP + kosttemp.BELOPP,">>>>>>>9").
         IF kosttemp.GRAVARE < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.GRAVARE,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.GRAVARE,">>>>>>9").
         IF kosttemp.MASKOVRIG < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.MASKOVRIG,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.MASKOVRIG,">>>>>>9").
         IF kosttemp.MTRL < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kosttemp.MTRL,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kosttemp.MTRL,">>>>>>>9").
         summakostvar = slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
         kosttemp.MASKOVRIG + kosttemp.OVRKR + kosttemp.BELOPP.
         IF summakostvar > 0 THEN DO:
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,">>>>>>>9").                       
         END.
         ELSE DO:           
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,"->>>>>>9").            
         END.
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kosttemp.OVRKR,"->>>>>9").
         IF slutsum.ANNATBOL = "" THEN DO:            
            /*ANNARS UPPDELNING PÅ BOLAG*/
            IF kosttemp.INKOMST > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kosttemp.INKOMST,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kosttemp.INKOMST,"->>>>>>9").           
            hjalpvar = kosttemp.INKOMST - (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + 
            kosttemp.MASKOVRIG + kosttemp.OVRKR + kosttemp.BELOPP).
            IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,"->>>>>>9").
         END.
         ELSE DO:
            hjalpvar = 0 - slutsum.BELOPP.
            IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,"->>>>>>9").
         END.      
         IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:    
            
            /* 65922 KOSTREG istället/också för MASKINENTREP förslag Wenche 20181108
            byt till 65923 i övergång för att inte få dubbelt  Wenche 20181127*/        
            IF kosttemp.GRAVARE > 0 THEN 
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.GRAVARE + slutsum.MBELOPP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(slutsum.MBELOPP,">>>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(slutsum.IBELOPP,">>>>>>9").
            summakostvar = summakostvar + slutsum.MBELOPP + slutsum.IBELOPP + slutsum.LONKOST.
            IF summakostvar > 0 THEN DO:
               SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,">>>>>>>9").                       
            END.
            ELSE DO:           
               SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(summakostvar,"->>>>>>9").            
            END.
            hjalpvar = hjalpvar - ( slutsum.MBELOPP + slutsum.IBELOPP).
            IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(hjalpvar,"->>>>>>9").
            SUBSTRING(tidut.UT,260) = STRING(slutsum.LONKOST,">>>>>>>9").                       
            IF slutsum.ANNATBOL NE "" THEN   SUBSTRING(tidut.UT,280) = slutsum.ANNATBOL.         
         END.
         IF slutsum.ANNATBOL = "" THEN DO:
            /*ANNARS UPPDELNING PÅ BOLAG*/
            IF varin - varkalk > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(varin - varkalk,"->>>>>>>9").
         END.            
         IF AVAILABLE valdaao THEN DO:
            RUN pnamn_UI.                       
         END.
         IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
            FIND FIRST intakttemp WHERE intakttemp.AONR = slutsum.AONR AND 
            intakttemp.DELNR = slutsum.DELNR
            NO-ERROR.
            IF NOT AVAILABLE intakttemp THEN CREATE intakttemp.
            ASSIGN
            intakttemp.AONR = slutsum.AONR  
            intakttemp.DELNR = slutsum.DELNR.
            IF intakttemp.INTINTAKT > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intakttemp.INTINTAKT,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intakttemp.INTINTAKT,"->>>>>>9").

            IF intakttemp.EXTERNINTAKT > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(intakttemp.EXTERNINTAKT,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(intakttemp.EXTERNINTAKT,"->>>>>>9").
            
            intakttemp.RESULTAT = (intakttemp.INTINTAKT + intakttemp.EXTERNINTAKT) - summakostvar.            
            IF intakttemp.RESULTAT > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(intakttemp.RESULTAT,"->>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(intakttemp.RESULTAT,"->>>>>>>9").           
         END.
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
         ASSIGN
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BTIMMAR = sumsum.BTIMMAR + slutsum.BTIMMAR
         sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR.
         IF slutsum.ANNATBOL = "" THEN DO:
            ASSIGN 
            sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST - 
            (slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
            kosttemp.OVRKR + kosttemp.BELOPP))
            sumsum.BELOPP = sumsum.BELOPP + slutsum.BELOPP + kosttemp.BELOPP
            sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP + kosttemp.MTRL + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
            kosttemp.OVRKR + kosttemp.BELOPP
            sumsum.MTRL = sumsum.MTRL + kosttemp.MTRL
            sumsum.GRAVARE = sumsum.GRAVARE + kosttemp.GRAVARE
            sumsum.MASKOVRIG = sumsum.MASKOVRIG + kosttemp.MASKOVRIG
            sumsum.OVRKR = sumsum.OVRKR + kosttemp.OVRKR                                                     
            sumsum.INKOMST = sumsum.INKOMST + kosttemp.INKOMST.
         END.
         ELSE DO:
            ASSIGN 
            sumsum.TINKOMST = sumsum.TINKOMST - slutsum.BELOPP
            sumsum.BELOPP = sumsum.BELOPP + slutsum.BELOPP 
            sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP.                                      
         END.      
         IF Guru.Konstanter:globforetag = "gkal" THEN DO:               
            ASSIGN
            sumsum.GRAVARE =  sumsum.GRAVARE + slutsum.MBELOPP 
            sumsum.MASKOVRIG =  sumsum.MASKOVRIG + slutsum.IBELOPP. 
         END.
      END.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum.
         ASSIGN
         kalksumsum.AONR = slutsum.AONR 
         kalksumsum.DELNR = slutsum.DELNR.
      END.
      IF AVAILABLE kalksumsum THEN DO:
         IF slutsum.ANNATBOL = "" THEN DO:
            /*ANNARS UPPDELNING PÅ BOLAG*/
            /*ANTAL MINUS*/
            CREATE tidut.                        
            kalktotvar = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
            kalksumsum.MTRL + kalksumsum.OVRKR + kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.
            ASSIGN                                              
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".                    
            IF kalksumsum.MONTTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR,">>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR,"->>>>9").
            IF kalksumsum.BTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BTIMMAR,">>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BTIMMAR,"->>>>9").
            IF kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,"->>>>>>9").
            IF kalksumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kalksumsum.MASKGBELOPP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kalksumsum.MASKGBELOPP,"->>>>>9").
            IF kalksumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kalksumsum.MBELOPP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kalksumsum.MBELOPP,"->>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kalksumsum.MTRL,"->>>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kalksumsum.OVRKR,"->>>>>9").
            SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(kalktotvar,"->>>>>>9").         
            IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN"  OR 
               Guru.Konstanter:globforetag = "GADM" OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:                      
               IF uppvaltemp.DELNRKOLL = TRUE  THEN DO:
                  FIND FIRST AONRTAB WHERE AONRTAB.AONR = slutsum.AONR AND AONRTAB.DELNR = slutsum.DELNR NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTAB THEN DO:      
                     ASSIGN kalksumsum.INKOMST = AONRTAB.BETNR.                  
                     IF AONRTAB.BETNR > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRTAB.BETNR,">>>>>>>9") .  
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRTAB.BETNR,"->>>>>>9") .  
                  END.
                  IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
                     IF (AONRTAB.BETNR /*- AONRTAB.STARTVNR*/) > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRTAB.BETNR /*- AONRTAB.STARTVNR*/,">>>>>>>9").
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(AONRTAB.BETNR /*- AONRTAB.STARTVNR*/,"->>>>>>9").
                     IF AONRTAB.STARTVNR > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRTAB.STARTVNR,">>>>>>>9").           
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(AONRTAB.STARTVNR,"->>>>>>9").            
                     IF AONRTAB.BETNR - kalktotvar > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(AONRTAB.BETNR - kalktotvar,">>>>>>>>9").
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(AONRTAB.BETNR - kalktotvar,"->>>>>>>9").
                     ASSIGN
                     intkalktotvar = intkalktotvar +  (AONRTAB.BETNR /*- AONRTAB.STARTVNR*/)
                     exkalktotvar  = exkalktotvar  + AONRTAB.STARTVNR.
                  END.
               END.
               ELSE DO:
                  DEFINE VARIABLE startvnrvar AS INTEGER NO-UNDO.
                  startvnrvar = 0.
                  FOR EACH AONRTAB WHERE AONRTAB.AONR = slutsum.AONR NO-LOCK:
                     ASSIGN 
                     startvnrvar = startvnrvar + AONRTAB.STARTVNR.
                     kalksumsum.INKOMST = kalksumsum.INKOMST + AONRTAB.BETNR.                  
                  END.
                  IF kalksumsum.INKOMST > 0 THEN
                  SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalksumsum.INKOMST,">>>>>>>9") .  
                  ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalksumsum.INKOMST,"->>>>>>9") .                    
                  IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
                     IF (kalksumsum.INKOMST /*- AONRTAB.STARTVNR*/) > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalksumsum.INKOMST /*- AONRTAB.STARTVNR*/,">>>>>>>9").
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(kalksumsum.INKOMST /*- AONRTAB.STARTVNR*/,"->>>>>>9").
                     IF startvnrvar > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(startvnrvar,">>>>>>>9").           
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(startvnrvar,"->>>>>>9").            
                     IF kalksumsum.INKOMST - kalktotvar > 0 THEN
                     SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(kalksumsum.INKOMST - kalktotvar,"->>>>>>>9").
                     ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(kalksumsum.INKOMST - kalktotvar,"->>>>>>>9").
                     ASSIGN
                     intkalktotvar = intkalktotvar +  (kalksumsum.INKOMST /*- AONRTAB.STARTVNR*/)
                     exkalktotvar  = exkalktotvar  + startvnrvar.
                  END.               
               END.
            END.
            /*ANTAL MINUS*/
            IF kalksumsum.TYP = 6 THEN DO:
               IF kalksumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.TIMMAR,"->>>>9").
            
            END.
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(0,">>>>>9").
               IF kalksumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,"->>>>9").
               kalksumsum.MONTTIMMAR = kalksumsum.TIMMAR.
               kalksumsum.TIMMAR = 0.
            END.
            
            FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
            ASSIGN
            sumsum.MONTTIMMAR = sumsum.MONTTIMMAR + kalksumsum.MONTTIMMAR
            sumsum.TIMMAR = sumsum.TIMMAR + kalksumsum.TIMMAR
            sumsum.BTIMMAR = sumsum.BTIMMAR + kalksumsum.BTIMMAR
            sumsum.BELOPP = sumsum.BELOPP + kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP
            sumsum.MASKGBELOPP = sumsum.MASKGBELOPP + kalksumsum.MASKGBELOPP
            sumsum.MBELOPP = sumsum.MBELOPP + kalksumsum.MBELOPP
            sumsum.MTRL = sumsum.MTRL + kalksumsum.MTRL
            sumsum.OVRKR = sumsum.OVRKR + kalksumsum.OVRKR
            sumsum.SKOSTNAD = sumsum.SKOSTNAD + 
            (kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP + 
            kalksumsum.MBELOPP + kalksumsum.MASKGBELOPP +
            kalksumsum.MTRL + kalksumsum.OVRKR).
            IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" 
             OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:
               ASSIGN sumsum.INKOMST = sumsum.INKOMST + kalksumsum.INKOMST.      
            END.
         END.            
      END.
      CREATE tidut.            
   END.
   /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" 
    OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = "Planerat offertpris:".
      IF AVAILABLE AONRTAB THEN DO:      
         SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(AONRTAB.BETNR,">>>>>999") .  
      END.
   END.*/
   IF ingakostver = FALSE THEN  DO:     
      CREATE tidut. 
      CREATE tidut.      
      CREATE tidut. 
      CREATE tidut.
      ASSIGN tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING".              
      CREATE tidut.         
      CREATE tidut.
      ASSIGN              
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok)         
      SUBSTRING(tidut.UT,12) = "KONTO"                           
      SUBSTRING(tidut.UT,18) = "VER-NR"         
      SUBSTRING(tidut.UT,34) = "DATUM"  
      SUBSTRING(tidut.UT,43) = "BENÄMNING"
      SUBSTRING(tidut.UT,86) = "BELOPP".  
      CREATE tidut.             
      ASSIGN  
      tidut.UT =                                    
"==========.=====.===============.========.==========================================.==========".        
      FOR EACH kosttemp2 NO-LOCK:         
         CREATE tidut.
         ASSIGN                   
         SUBSTRING(tidut.UT,1) = kosttemp2.AONR + STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12,5) = SUBSTRING(kosttemp2.BOKKONTO,1,5) 
         SUBSTRING(tidut.UT,18) = kosttemp2.FAKTNR         
         SUBSTRING(tidut.UT,34) = STRING(kosttemp2.REGDATUM)
         SUBSTRING(tidut.UT,43) = kosttemp2.BENAMNING
         SUBSTRING(tidut.UT,86) = STRING((kosttemp2.MTRL + kosttemp2.OVRKR + kosttemp2.GRAVARE +
         kosttemp2.MASKOVRIG + 
         kosttemp2.PERSKOST + kosttemp2.PERSKOST2 + kosttemp2.TRAKTKOST) - kosttemp2.INKOMST,"->>>>>>>>9").                  
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
             ASSIGN
             SUBSTRING(tidut.UT,7,3) = STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
             SUBSTRING(tidut.UT,10,1) = " "
             SUBSTRING(tidut.UT,11,6) = SUBSTRING(kosttemp2.BOKKONTO,1,6). 
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         ASSIGN
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + sumsum.MASKOVRIG + sumsum.GRAVARE + sumsum.LONKOST   
         sumsum.TINKOMST = sumsum.TINKOMST + (0 - (sumsum.MASKOVRIG + sumsum.GRAVARE)).
      END.
      ASSIGN       
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                     
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9")    
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,">>>>>9") 
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.OTIMMAR,">>>>>9")   
      SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(sumsum.TINKOMST,"->>>>>>9").
      IF vartot > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,"->>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING(vartot,"->>>>>>>9").
      IF sumsum.BELOPP < 0                                            
      THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,">>>>>>>9").
      IF sumsum.GRAVARE < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.GRAVARE,">>>>>>9").
      IF sumsum.MASKOVRIG < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MASKOVRIG,">>>>>>9").
      IF sumsum.MTRL < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,">>>>>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                  
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
      IF sumsum.INKOMST > 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,"->>>>>>9").      
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         SUBSTRING(tidut.UT,260) = STRING(sumsum.LONKOST,">>>>>>>9").
      END.
      IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
         FOR EACH intakttemp:
            ACCUMULATE intakttemp.INTINTAKT (TOTAL).
            ACCUMULATE intakttemp.EXTERNINTAKT (TOTAL).
            ACCUMULATE intakttemp.RESULTAT (TOTAL).
         END.
         IF (ACCUM TOTAL intakttemp.INTINTAKT) > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING((ACCUM TOTAL intakttemp.INTINTAKT),">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING((ACCUM TOTAL intakttemp.INTINTAKT),"->>>>>>9").
         IF (ACCUM TOTAL intakttemp.EXTERNINTAKT) > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING((ACCUM TOTAL intakttemp.EXTERNINTAKT),">>>>>>>9").      
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING((ACCUM TOTAL intakttemp.EXTERNINTAKT),"->>>>>>9").
         IF (ACCUM TOTAL intakttemp.RESULTAT) > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING((ACCUM TOTAL intakttemp.RESULTAT),"->>>>>>>9").      
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING((ACCUM TOTAL intakttemp.RESULTAT),"->>>>>>>9").      
      END.
      /*ANTAL MINUS*/
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
      CREATE tidut.
      ASSIGN                 
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".            
      
      IF sumsum.MONTTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.MONTTIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.MONTTIMMAR,"->>>>9").
      IF sumsum.BTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BTIMMAR,"->>>>9").
      IF sumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.TIMMAR,">>>>>9").                                                                 
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.TIMMAR,"->>>>9").                                                                 
      IF sumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,">>>>>>>9").                                                                       
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.BELOPP,"->>>>>>9").                                                                       
      IF sumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MASKGBELOPP,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MASKGBELOPP,"->>>>>9").
      IF sumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MBELOPP,">>>>>>9").             
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.MBELOPP,"->>>>>9").             
      
      SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.OVRKR,"->>>>>9").
      SUBSTRING(tidut.UT,utnr[nrcol[15]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").

      IF sumsum.MTRL > 0 THEN                                        
      SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.MTRL,"->>>>>>9").

      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" 
       OR Guru.Konstanter:globforetag = "GKAL"  THEN DO:      
         IF sumsum.INKOMST > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,">>>>>>>9").         
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.INKOMST,"->>>>>>9").         
         IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
            
            IF intkalktotvar > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intkalktotvar,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(intkalktotvar,"->>>>>>9").
            IF exkalktotvar > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(exkalktotvar,">>>>>>>9").           
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(exkalktotvar,"->>>>>>9").           
            IF (intkalktotvar + exkalktotvar) - sumsum.SKOSTNAD > 0 THEN
            SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING((intkalktotvar + exkalktotvar) - sumsum.SKOSTNAD,"->>>>>>>9").            
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[14]]) = STRING((intkalktotvar + exkalktotvar) - sumsum.SKOSTNAD,"->>>>>>>9").            
         END.
      END.         
   END.
END PROCEDURE.

PROCEDURE kostreg_UI :   
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KOSTREG):     
      CREATE kosttemp2.
      ASSIGN            
      kosttemp2.ANVANDARE = SUBSTRING(KOSTREG.ANVANDARE,1,12) 
      kosttemp2.AONR = KOSTREG.AONR 
      kosttemp2.DELNR = valdaao.DELNR
      kosttemp2.BENAMNING = KOSTREG.BENAMNING 
      kosttemp2.BETDATUM = KOSTREG.BETDATUM 
      kosttemp2.BOKKONTO = KOSTREG.BOKKONTO 
      
      kosttemp2.FAKBES = KOSTREG.FAKBES 
      kosttemp2.FAKTNR = KOSTREG.FAKTNR 
     /* kosttemp2.FAKTURERAD = KOSTREG.FAKTURERAD */
      kosttemp2.INKOMST = KOSTREG.INKOMST  
     /* kosttemp2.KOSTAUTO = KOSTREG.KOSTAUTO */
      kosttemp2.LEVKOD = KOSTREG.LEVKOD  
      kosttemp2.MOMS = KOSTREG.MOMS 
      kosttemp2.RADNR = KOSTREG.RADNR 
      kosttemp2.REGDATUM = KOSTREG.REGDATUM 
      kosttemp2.TRAKTKOST = KOSTREG.TRAKTKOST.
      
      /*IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "6" THEN DO:      
            IF KOSTREG.BOKKONTO = "65920" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.
            ELSE IF KOSTREG.BOKKONTO > "65920" THEN kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.      
            ELSE kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.
            ASSIGN
            kosttemp2.MTRL = KOSTREG.MTRL 
            kosttemp2.OVRKR = KOSTREG.OVRKR 
            kosttemp2.PERSKOST = KOSTREG.PERSKOST 
            kosttemp2.PERSKOST2 = 0.
         END.
         ELSE DO:
            ASSIGN
            kosttemp2.MTRL = KOSTREG.MTRL 
            kosttemp2.OVRKR = KOSTREG.OVRKR 
            kosttemp2.PERSKOST = KOSTREG.PERSKOST 
            kosttemp2.PERSKOST2 = 0.
         END.
      END.*/
      IF Guru.Konstanter:globforetag = "gkal" THEN DO:       
         /* 65922 KOSTREG istället/också för MASKINENTREP förslag Wenche 20181108
         byt till 65923 i övergång för att inte få dubbelt  Wenche 20181127*/       
         IF KOSTREG.BOKKONTO = "65923" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.                     
         ASSIGN
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.         
      END.
      ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         ASSIGN
         kosttemp2.MASKOVRIG = KOSTREG.MASKKOST
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.
      END.
      ELSE IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
         vartvar = "".
         RUN kontokoll_UI (INPUT KOSTREG.BOKKONTO,OUTPUT vartvar).
         IF vartvar = "OVRIGT" THEN kosttemp2.OVRKR = KOSTREG.OVRKR.
         IF vartvar = "MASK1" THEN kosttemp2.GRAVARE = KOSTREG.MASKKOST.
         IF vartvar = "MASK2" THEN kosttemp2.MASKOVRIG = KOSTREG.MASKKOST.
         IF vartvar = "MASK3" THEN kosttemp2.MTRL = KOSTREG.MASKKOST. 
         IF vartvar = "MTRL2" THEN kosttemp2.PERSKOST = KOSTREG.MTRL.
         IF vartvar = "PERS2" THEN kosttemp2.PERSKOST2 = KOSTREG.PERSKOST.            
      END.
      ELSE DO:
         ASSIGN
         kosttemp2.MASKOVRIG = KOSTREG.MASKKOST
         kosttemp2.MTRL = KOSTREG.MTRL 
         kosttemp2.OVRKR = KOSTREG.OVRKR 
         kosttemp2.PERSKOST = KOSTREG.PERSKOST 
         kosttemp2.PERSKOST2 = 0.
      END.
      GET NEXT kq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE kontokoll_UI:
   DEFINE INPUT PARAMETER varkont LIKE KOSTREG.BOKKONTO NO-UNDO.
   DEFINE OUTPUT PARAMETER varvart AS CHARACTER NO-UNDO.
   RUN SUPER (INPUT varkont,OUTPUT varvart).
END PROCEDURE.

PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):  
      IF Guru.Konstanter:globforetag = "GKAL" AND SUMTIDDAG.PERSMASK = FALSE THEN DO:
         /*Kalmar vill ha med tiskrivning maskiner -ej kostnadsregistreringar*/         
         CREATE dagtemp.
         ASSIGN          
         dagtemp.OMRADE = SUMTIDDAG.OMRADE
         dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.BEFATTNING = SUBSTRING(SUMTIDDAG.BEFATTNING,1,20)
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.                  
      END.
      ELSE IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
      ELSE IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
         /*
         FIND FIRST restid WHERE restid.AONR = SUMTIDDAG.AONR AND
         restid.DELNR = SUMTIDDAG.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE restid THEN CREATE restid.
         ASSIGN          
         restid.AONR = SUMTIDDAG.AONR
         restid.DELNR = SUMTIDDAG.DELNR 
         restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.           
         CREATE dagtemp.
         ASSIGN          
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = SUMTIDDAG.DELNR 
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK             
         dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
         dagtemp.BELOPP = SUMTIDDAG.BELOPP 
         dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
         dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
         */
      END.
      ELSE DO:
         CREATE dagtemp.
         ASSIGN
         dagtemp.OMRADE = SUMTIDDAG.OMRADE
         dagtemp.GEOMRADE = SUMTIDDAG.GEOMRADE          
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST. 
                 
         IF SUMTIDDAG.PRIS = 0 THEN DO:
            dagtemp.OTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
         END.
         ELSE DO:
            /*"EJ DEBI.."*/
            IF SUMTIDDAG.PRISTYP = "BEREDARE." THEN DO: 
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.    
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "BEREDARE" THEN DO:
               dagtemp.BTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "MONTÖR" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "ELMONTÖR" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
            ELSE IF SUMTIDDAG.BEFATTNING BEGINS "AVLÄSARE" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.                                 
            ELSE IF Guru.Konstanter:globforetag = "gkal" and SUMTIDDAG.BEFATTNING BEGINS  "TEKNIKER(+)" THEN DO:
               dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.                       
            ELSE DO:
               dagtemp.OTIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.
            END.
         END.
      END.
      GET NEXT stq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   IF Guru.Konstanter:varforetypval[47] = 1 THEN DO:
      /*TEST BOLAGSUPPDELNING GKAL*/
      DEFINE BUFFER oradebuff FOR OMRADETAB.
      DEFINE BUFFER avdbuff FOR AVDELNING.
      DEFINE BUFFER jurbuff FOR JURPERS.
      FOR EACH dagtemp WHERE dagtemp.OMRADE NE dagtemp.GEOMR:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = dagtemp.OMRADE NO-LOCK NO-ERROR.
         FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
         FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
         FIND FIRST oradebuff WHERE oradebuff.OMRADE = dagtemp.GEOMRADE NO-LOCK NO-ERROR.
         FIND FIRST avdbuff WHERE avdbuff.AVDELNINGNR = oradebuff.AVDELNINGNR NO-LOCK NO-ERROR.
         FIND FIRST jurbuff WHERE jurbuff.JUDID = avdbuff.POSTANST NO-LOCK NO-ERROR.
         IF JURPERS.JUDID NE jurbuff.JUDID AND oradebuff.OMRADE NE ""  THEN DO:
            dagtemp.ANNATBOL = dagtemp.OMRADE.
         END.
         ELSE dagtemp.ANNATBOL = "".                    
      END.        
   END.      
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = TRUE NO-LOCK 
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.BTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK).        
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.ANNATBOL BY dagtemp.PERSMASK).        
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT
         slutsum.ANNATBOL = dagtemp.ANNATBOL
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.OTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OTIMMAR)
         slutsum.BTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BTIMMAR).             
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).             
      END.     
   END.   
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*MASKINER*/      
      FOR EACH dagtemp WHERE dagtemp.PERSMASK = FALSE NO-LOCK 
      BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING: 
         ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK BY dagtemp.BEFATTNING).           
         IF LAST-OF(dagtemp.BEFATTNING) THEN DO:
            FIND FIRST slutsum WHERE slutsum.AONR = dagtemp.AONR AND
            slutsum.DELNR = dagtemp.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE slutsum THEN CREATE slutsum.
            IF dagtemp.BEFATTNING = "INHYRD PERSONAL" THEN DO:           
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.IBELOPP = slutsum.IBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).               
            END.
            ELSE DO:
               ASSIGN 
               slutsum.AONR = dagtemp.AONR
               slutsum.DELNR = dagtemp.DELNR  
               slutsum.MBELOPP = slutsum.MBELOPP + 
               (ACCUM TOTAL BY dagtemp.BEFATTNING dagtemp.BELOPP).               
            END.
         END.
      END.

   END.
   FOR EACH kosttemp2 BREAK BY kosttemp2.AONR BY kosttemp2.DELNR:     
      ACCUMULATE kosttemp2.PERSKOST2 (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.PERSKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.GRAVARE (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MASKOVRIG (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.TRAKTKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MTRL (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.OVRKR (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      IF LAST-OF(kosttemp2.DELNR) THEN DO:
         CREATE kosttemp.
         ASSIGN 
         kosttemp.AONR = kosttemp2.AONR
         kosttemp.DELNR = kosttemp2.DELNR 
         kosttemp.MASKOVRIG = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MASKOVRIG) 
         kosttemp.GRAVARE = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.GRAVARE) 
         kosttemp.TBELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.TRAKTKOST)
         kosttemp.MTRL = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MTRL)    
         kosttemp.OVRKR = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.OVRKR)
         kosttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.INKOMST).         
         IF Guru.Konstanter:globforetag = "GRIT" THEN DO:
            ASSIGN
            kosttemp.BELOPP1P = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST)
            kosttemp.BELOPP2P = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST2).
         END.
         ELSE DO:
            kosttemp.BELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST). 
         END.
         IF Guru.Konstanter:varforetypval[10] = 1 THEN DO:
            kosttemp.INKOMST = 0.
         END.
      END.       
   END. 
   FOR EACH kosttemp2 WHERE kosttemp2.INKOMST NE 0 
      BREAK BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO:     
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR BY kosttemp2.BOKKONTO). 
      IF LAST-OF(kosttemp2.BOKKONTO) THEN DO:
         CREATE inkomsttemp.
         ASSIGN 
         inkomsttemp.AONR = kosttemp2.AONR
         inkomsttemp.DELNR = kosttemp2.DELNR 
         inkomsttemp.BOKKONTO = kosttemp2.BOKKONTO
         inkomsttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.BOKKONTO kosttemp2.INKOMST).                  
      END.       
   END.
   FOR EACH inkomsttemp:
      FIND FIRST intakttemp WHERE intakttemp.AONR = inkomsttemp.AONR AND 
      intakttemp.DELNR = inkomsttemp.DELNR NO-ERROR.
      IF NOT AVAILABLE intakttemp THEN CREATE intakttemp.
      ASSIGN
      intakttemp.AONR  = inkomsttemp.AONR 
      intakttemp.DELNR = inkomsttemp.DELNR.             
      IF inkomsttemp.BOKKONTO = "36680" THEN intakttemp.INTINTAKT = intakttemp.INTINTAKT + inkomsttemp.INKOMST.   
      ELSE intakttemp.EXTERNINTAKT = intakttemp.EXTERNINTAKT + inkomsttemp.INKOMST.
   END.
END PROCEDURE.
PROCEDURE pnamn_UI:
   ASSIGN
   SUBSTRING(tidut.UT,200) = valdaao.ARBANSVARIG
   SUBSTRING(tidut.UT,220) = valdaao.BEREDARE   
   SUBSTRING(tidut.UT,240) = valdaao.PROJEKTOR.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.ARBANSVARIG NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,200) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.   
   ELSE SUBSTRING(tidut.UT,200) = valdaao.ARBANSVARIG.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.BEREDARE NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      SUBSTRING(tidut.UT,220) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   ELSE SUBSTRING(tidut.UT,220) = valdaao.BEREDARE.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = valdaao.PROJEKTOR NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO: 
      SUBSTRING(tidut.UT,240) = CAPS(SUBSTRING(PERSONALTAB.FORNAMN,1,1)) + " " + CAPS(PERSONALTAB.EFTERNAMN).
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   ELSE SUBSTRING(tidut.UT,240) = valdaao.PROJEKTOR.
  
END PROCEDURE.

PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
