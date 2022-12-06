
/*------------------------------------------------------------------------
    File        : AOKAPPP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 27 14:06:49 CEST 2014
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
   tidut.UT = "TI=TIDREDOVISNING KO=KOSTNADSREGISTRERING KA=KALKYLERING". 
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 15
   utnr[4] = 22
   utnr[5] = 30
   utnr[6] = 37
   utnr[7] = 45
   utnr[8] = 52
   utnr[9] = 60
   utnr[10] = 68
   utnr[11] = 76
   utnr[12] = 85
   utnr[13] = 93
   utnr[14] = 102
   str = "".      
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DO WHILE i <= 109:
      i = i + 1.
      str = str + "=".      
   END.   
   i = 2.      
   DO WHILE i <= 14:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.          
   CREATE tidut.  
   CREATE tidut.   
   CREATE tidut.             
   ASSIGN                                                                                                       
   SUBSTRING(tidut.UT,utnr[3]) = "ARB."             
   SUBSTRING(tidut.UT,utnr[4]) = "ARBETS"         
   SUBSTRING(tidut.UT,utnr[5]) = "ÖVER."  
   SUBSTRING(tidut.UT,utnr[6]) = "ÖVERTID"
   SUBSTRING(tidut.UT,utnr[7]) = "RES."
   SUBSTRING(tidut.UT,utnr[8]) = "TRAKT."
   SUBSTRING(tidut.UT,utnr[9]) = "L-TILL"    
   SUBSTRING(tidut.UT,utnr[10]) = "FR. TJ."   
   SUBSTRING(tidut.UT,utnr[11]) = "MATERIEL"
   SUBSTRING(tidut.UT,utnr[12]) = "ÖVRIG.".    
   str2 = tidut.UT.                 
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[1]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[3]) = "TIMMAR"             
   SUBSTRING(tidut.UT,utnr[4]) = "KOSTNAD"         
   SUBSTRING(tidut.UT,utnr[5]) = "TIMMAR"  
   SUBSTRING(tidut.UT,utnr[6]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[7]) = "TIMMAR"  
   SUBSTRING(tidut.UT,utnr[8]) = "KOSTNAD" 
   SUBSTRING(tidut.UT,utnr[9]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[10]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[11]) = "KOSTNAD" 
   SUBSTRING(tidut.UT,utnr[12]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[13]) = "INTÄKT"
   SUBSTRING(tidut.UT,utnr[14]) = "RESULTAT".
   str3 = tidut.UT.                
   CREATE tidut.      
   tidut.UT = str.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "TI".
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KA".
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KO".
   FOR EACH slutsum USE-INDEX AONR:
      FIND FIRST restid WHERE restid.AONR = slutsum.AONR AND
      restid.DELNR = slutsum.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.                    
      CREATE tidut.                     
      ASSIGN 
      SUBSTRING(tidut.UT,utnr[1]) = slutsum.AONR + " " + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1]).
      SUBSTRING(tidut.UT,utnr[2]) = "TI".        
      SUBSTRING(tidut.UT,utnr[3]) = STRING(slutsum.TIMMAR,">>>>99").                                                          
      SUBSTRING(tidut.UT,utnr[4]) = STRING(slutsum.BELOPP,">>>>>99").  
      SUBSTRING(tidut.UT,utnr[5]) = STRING(slutsum.OTIMMAR,">>>>99").  
      SUBSTRING(tidut.UT,utnr[6]) = STRING(slutsum.OBELOPP,">>>>>99").
      SUBSTRING(tidut.UT,utnr[8]) = STRING(slutsum.TBELOPP,">>>>>99").      
      SUBSTRING(tidut.UT,utnr[9]) = STRING(slutsum.LONKOST,"->>>>99").
      SUBSTRING(tidut.UT,utnr[10]) = STRING(slutsum.MBELOPP,">>>>>99").
      IF AVAILABLE restid THEN SUBSTRING(tidut.UT,utnr[7]) = STRING(restid.TIMMAR,">>>>99").
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "TI" NO-ERROR.                   
      ASSIGN  
      sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
      sumsum.BELOPP =  sumsum.BELOPP  + slutsum.BELOPP
      sumsum.OTIMMAR = sumsum.OTIMMAR + slutsum.OTIMMAR
      sumsum.OBELOPP = sumsum.OBELOPP + slutsum.OBELOPP
      sumsum.TBELOPP = sumsum.TBELOPP + slutsum.TBELOPP   
      sumsum.LONKOST = sumsum.LONKOST + slutsum.LONKOST
      sumsum.MBELOPP = sumsum.MBELOPP + slutsum.MBELOPP.
      IF AVAILABLE restid THEN DO:
         sumsum.RESTIM = sumsum.RESTIM + restid.TIMMAR.
      END.
      FIND FIRST kosttemp WHERE kosttemp.AONR = slutsum.AONR AND
      kosttemp.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kosttemp THEN DO:
         SUBSTRING(tidut.UT,utnr[14]) = STRING(kosttemp.INKOMST -                                  
         (slutsum.BELOPP + slutsum.OBELOPP + slutsum.TBELOPP + 
          slutsum.LONKOST + slutsum.MBELOPP + kosttemp.MTRL + kosttemp.OVRKR + kosttemp.BELOPP + kosttemp.MBELOPP),"->>>>>99").
         IF Guru.Konstanter:globforetag = "GRAN"  THEN DO:
            SUBSTRING(tidut.UT,utnr[14]) = STRING(kosttemp.INKOMST -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + kosttemp.MTRL + kosttemp.MBELOPP + kosttemp.OVRKR),"->>>>>99").            
         END.
         IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:
            SUBSTRING(tidut.UT,utnr[14]) = STRING(kosttemp.INKOMST -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + kosttemp.MTRL + + slutsum.MBELOPP + kosttemp.OVRKR),"->>>>>99").            
         END.
         CREATE tidut.
         IF kosttemp.BELOPP < 0 
         THEN SUBSTRING(tidut.UT,utnr[4]) = STRING(kosttemp.BELOPP,"->>>>99").
         ELSE SUBSTRING(tidut.UT,utnr[4]) = STRING(kosttemp.BELOPP,">>>>>99").
         IF kosttemp.MTRL < 0 
         THEN SUBSTRING(tidut.UT,utnr[11]) = STRING(kosttemp.MTRL,"->>>>>99").
         ELSE SUBSTRING(tidut.UT,utnr[11]) = STRING(kosttemp.MTRL,">>>>>>99").
         ASSIGN
         SUBSTRING(tidut.UT,utnr[2]) = "KO"
         SUBSTRING(tidut.UT,utnr[8]) = STRING(kosttemp.TBELOPP,">>>>>99").
         IF kosttemp.MBELOPP < 0 
         THEN SUBSTRING(tidut.UT,utnr[10]) = STRING(kosttemp.MBELOPP,"->>>>99").
         ELSE SUBSTRING(tidut.UT,utnr[10]) = STRING(kosttemp.MBELOPP,">>>>>99").
         ASSIGN
         SUBSTRING(tidut.UT,utnr[12]) = STRING(kosttemp.OVRKR,"->>>>99")                                                     
         SUBSTRING(tidut.UT,utnr[13]) = STRING(kosttemp.INKOMST,"->>>>>99").                                 
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KO" NO-ERROR.
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + kosttemp.MTRL + kosttemp.MBELOPP + kosttemp.OVRKR)).
         END.
         ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + kosttemp.MTRL + slutsum.MBELOPP + kosttemp.OVRKR)).
         END.
         ELSE DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + slutsum.TBELOPP + slutsum.LONKOST + 
            slutsum.MBELOPP + kosttemp.MTRL + kosttemp.OVRKR + kosttemp.BELOPP)).
         END.
         ASSIGN
         sumsum.BELOPP = sumsum.BELOPP + kosttemp.BELOPP
         sumsum.MTRL = sumsum.MTRL + kosttemp.MTRL
         sumsum.TBELOPP = sumsum.TBELOPP + kosttemp.TBELOPP
         sumsum.MBLOPP = sumsum.MBLOPP + kosttemp.MBELOPP
         sumsum.OVRKR = sumsum.OVRKR + kosttemp.OVRKR                                                     
         sumsum.INKOMST = sumsum.INKOMST + kosttemp.INKOMST.         
      END.
      ELSE DO:
         SUBSTRING(tidut.UT,utnr[14]) = STRING(0 - 
         (slutsum.BELOPP + slutsum.OBELOPP + slutsum.TBELOPP + slutsum.LONKOST + 
          slutsum.MBELOPP),"->>>>>99").
         IF Guru.Konstanter:globforetag = "GRAN"  THEN DO:
            SUBSTRING(tidut.UT,utnr[14]) = STRING(0 - (slutsum.BELOPP + slutsum.OBELOPP),"->>>>>99").
         END.
         IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
            SUBSTRING(tidut.UT,utnr[14]) = STRING(0 - (slutsum.BELOPP + slutsum.OBELOPP +
            slutsum.MBELOPP ),"->>>>>99").
         END.
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KO" NO-ERROR.
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (0 - (slutsum.BELOPP + slutsum.OBELOPP)).
         END.         
         ELSE IF  Guru.Konstanter:globforetag = "GKAL" THEN DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (0 - (slutsum.BELOPP + slutsum.OBELOPP + slutsum.MBELOPP)).
         END.         
         ELSE DO:
            sumsum.TINKOMST = sumsum.TINKOMST + (0 -                                  
            (slutsum.BELOPP + slutsum.OBELOPP + slutsum.TBELOPP + slutsum.LONKOST + 
            slutsum.MBELOPP)).
         END.
      END.
      FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
      kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
      IF AVAILABLE kalksumsum THEN DO:
         /*ANTAL MINUS*/
         CREATE tidut.                        
         ASSIGN                                        
         SUBSTRING(tidut.UT,utnr[2]) = "KA".            
         IF kalksumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[3]) = STRING(kalksumsum.TIMMAR,">>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[3]) = STRING(kalksumsum.TIMMAR,"->>>>9").
         IF kalksumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[4]) = STRING(kalksumsum.BELOPP,">>>>>>9"). 
         ELSE SUBSTRING(tidut.UT,utnr[4]) = STRING(kalksumsum.BELOPP,"->>>>>9"). 
         IF kalksumsum.OTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[5]) = STRING(kalksumsum.OTIMMAR,">>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[5]) = STRING(kalksumsum.OTIMMAR,"->>>>9").
         IF kalksumsum.OBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[6]) = STRING(kalksumsum.OBELOPP,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[6]) = STRING(kalksumsum.OBELOPP,"->>>>>9").
         IF kalksumsum.TBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[8]) = STRING(kalksumsum.TBELOPP,">>>>>99").
         ELSE SUBSTRING(tidut.UT,utnr[8]) = STRING(kalksumsum.TBELOPP,"->>>>99").
         IF kalksumsum.LONKOST >= 0 THEN SUBSTRING(tidut.UT,utnr[9]) = STRING(kalksumsum.LONKOST,">>>>>99").
         ELSE SUBSTRING(tidut.UT,utnr[9]) = STRING(kalksumsum.LONKOST,"->>>>99").
         IF kalksumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[10]) = STRING(kalksumsum.MBELOPP,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[10]) = STRING(kalksumsum.MBELOPP,"->>>>>9").
         
         IF kalksumsum.MTRL < 0 THEN SUBSTRING(tidut.UT,utnr[11]) = STRING(kalksumsum.MTRL,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[11]) = STRING(kalksumsum.MTRL,">>>>>>>9").
         SUBSTRING(tidut.UT,utnr[12]) = STRING(kalksumsum.OVRKR,"->>>>>9").
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
         ASSIGN
         sumsum.TIMMAR = sumsum.TIMMAR + kalksumsum.TIMMAR
         sumsum.BELOPP = sumsum.BELOPP + kalksumsum.BELOPP
         sumsum.OTIMMAR = sumsum.OTIMMAR + kalksumsum.OTIMMAR
         sumsum.OBELOPP = sumsum.OBELOPP + kalksumsum.OBELOPP
         sumsum.TBELOPP = sumsum.TBELOPP + kalksumsum.TBELOPP
         sumsum.LONKOST = sumsum.LONKOST + kalksumsum.LONKOST
         sumsum.MBELOPP = sumsum.MBELOPP + kalksumsum.MBELOPP
         sumsum.MTRL = sumsum.MTRL + kalksumsum.MTRL
         sumsum.OVRKR = sumsum.OVRKR + kalksumsum.OVRKR.
      END.
      CREATE tidut.
   END.

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
         SUBSTRING(tidut.UT,1) = kosttemp2.AONR
         SUBSTRING(tidut.UT,8,3) = STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12,5) = SUBSTRING(kosttemp2.BOKKONTO,1,5) 
         SUBSTRING(tidut.UT,18) = kosttemp2.FAKTNR         
         SUBSTRING(tidut.UT,34) = STRING(kosttemp2.REGDATUM)
         SUBSTRING(tidut.UT,43) = kosttemp2.BENAMNING
         SUBSTRING(tidut.UT,86) = STRING((kosttemp2.MTRL + kosttemp2.OVRKR + kosttemp2.MASKKOST + 
         kosttemp2.PERSKOST + kosttemp2.TRAKTKOST) - kosttemp2.INKOMST,"->>>>>>>99").                  
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN            
            SUBSTRING(tidut.UT,7,3) = STRING(kosttemp2.DELNR,Guru.Konstanter:varforetypchar[1])
            SUBSTRING(tidut.UT,10,1) = " "
            SUBSTRING(tidut.UT,11,6) = SUBSTRING(kosttemp2.BOKKONTO,1,6). 
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "TI" NO-ERROR.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[1]) = "SUMMA".
      ASSIGN       
      SUBSTRING(tidut.UT,utnr[2]) = "TI"                     
      SUBSTRING(tidut.UT,utnr[3]) = STRING(sumsum.TIMMAR,">>>>99")    
      SUBSTRING(tidut.UT,utnr[4]) = STRING(sumsum.BELOPP,">>>>>99")   
      SUBSTRING(tidut.UT,utnr[5]) = STRING(sumsum.OTIMMAR,">>>>99")   
      SUBSTRING(tidut.UT,utnr[6]) = STRING(sumsum.OBELOPP,">>>>>99")  
      SUBSTRING(tidut.UT,utnr[8]) = STRING(sumsum.TBELOPP,">>>>>99")  
      SUBSTRING(tidut.UT,utnr[9]) = STRING(sumsum.LONKOST,"->>>>99")  
      SUBSTRING(tidut.UT,utnr[10]) = STRING(sumsum.MBELOPP,">>>>>99")
      SUBSTRING(tidut.UT,utnr[7]) = STRING(sumsum.RESTIM,">>>>99").
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KO" NO-ERROR.
      ASSIGN       
      SUBSTRING(tidut.UT,utnr[14]) = STRING(sumsum.TINKOMST,"->>>>>99").
      CREATE tidut.
      IF sumsum.BELOPP < 0 
      THEN SUBSTRING(tidut.UT,utnr[4]) = STRING(sumsum.BELOPP,"->>>>99").
      ELSE SUBSTRING(tidut.UT,utnr[4]) = STRING(sumsum.BELOPP,">>>>>99").
      IF sumsum.MTRL < 0 
      THEN SUBSTRING(tidut.UT,utnr[11]) = STRING(sumsum.MTRL,"->>>>>99").
      ELSE SUBSTRING(tidut.UT,utnr[11]) = STRING(sumsum.MTRL,">>>>>>99").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[2]) = "KO"
      SUBSTRING(tidut.UT,utnr[8]) = STRING(sumsum.TBELOPP,">>>>>99").
      IF sumsum.MBELOPP < 0 
      THEN SUBSTRING(tidut.UT,utnr[10]) = STRING(sumsum.MBLOPP,"->>>>99").
      ELSE SUBSTRING(tidut.UT,utnr[10]) = STRING(sumsum.MBLOPP,">>>>>99").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[12]) = STRING(sumsum.OVRKR,"->>>>99")                                                     
      SUBSTRING(tidut.UT,utnr[13]) = STRING(sumsum.INKOMST,"->>>>>99").                                 
      FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
      /*ANTAL MINUS*/
      CREATE tidut.
      ASSIGN       
      SUBSTRING(tidut.UT,utnr[2]) = "KA".            
      IF sumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[3]) = STRING(sumsum.TIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[3]) = STRING(sumsum.TIMMAR,"->>>>9").
      IF sumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[4]) = STRING(sumsum.BELOPP,">>>>>>9"). 
      ELSE SUBSTRING(tidut.UT,utnr[4]) = STRING(sumsum.BELOPP,"->>>>>9"). 
      IF sumsum.OTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[5]) = STRING(sumsum.OTIMMAR,">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[5]) = STRING(sumsum.OTIMMAR,"->>>>9").
      IF sumsum.OBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[6]) = STRING(sumsum.OBELOPP,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[6]) = STRING(sumsum.OBELOPP,"->>>>>9").
      IF sumsum.TBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[8]) = STRING(sumsum.TBELOPP,">>>>>99").
      ELSE SUBSTRING(tidut.UT,utnr[8]) = STRING(sumsum.TBELOPP,"->>>>99").
      IF sumsum.LONKOST >= 0 THEN SUBSTRING(tidut.UT,utnr[9]) = STRING(sumsum.LONKOST,">>>>>99").
      ELSE SUBSTRING(tidut.UT,utnr[9]) = STRING(sumsum.LONKOST,"->>>>99").
      IF sumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[10]) = STRING(sumsum.MBELOPP,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[10]) = STRING(sumsum.MBELOPP,"->>>>>9").
      
      IF sumsum.MTRL < 0 THEN SUBSTRING(tidut.UT,utnr[11]) = STRING(sumsum.MTRL,"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[11]) = STRING(sumsum.MTRL,">>>>>>>9").
      SUBSTRING(tidut.UT,utnr[12]) = STRING(sumsum.OVRKR,"->>>>>9").
   END.
END PROCEDURE.



PROCEDURE skapadagdag_UI :
   GET FIRST stq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):     
      IF SUMTIDDAG.PRISTYP = "RESTID..." THEN DO: 
         FIND FIRST restid WHERE restid.AONR = valdaao.AONR AND
         restid.DELNR = valdaao.DELNR USE-INDEX AONR EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE restid THEN CREATE restid.
         ASSIGN          
         restid.AONR = valdaao.AONR
         restid.DELNR = valdaao.DELNR 
         restid.TIMMAR = restid.TIMMAR + SUMTIDDAG.TIMMAR.           
         CREATE dagtemp.
         ASSIGN   
         dagtemp.OMRADE = SUMTIDDAG.OMRADE       
         dagtemp.AONR = valdaao.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK             
         dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
         dagtemp.BELOPP = SUMTIDDAG.BELOPP 
         dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
         dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
      END.
      ELSE DO:
         CREATE dagtemp.
         ASSIGN          
         dagtemp.OMRADE = SUMTIDDAG.OMRADE
         dagtemp.AONR = valdaao.AONR
         dagtemp.DELNR = valdaao.DELNR
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK 
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR
         dagtemp.OTIMMAR = SUMTIDDAG.OTIMMAR 
         dagtemp.BELOPP = SUMTIDDAG.BELOPP 
         dagtemp.OBELOPP = SUMTIDDAG.OBELOPP 
         dagtemp.TBELOPP = SUMTIDDAG.TBELOPP
         dagtemp.LONKOST = SUMTIDDAG.LONKOST.
      END.
      GET NEXT stq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE summa_UI.
   /*PERSONER*/
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = TRUE NO-LOCK 
   BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).  
      ACCUMULATE dagtemp.TBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR  
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.OTIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OTIMMAR)
         slutsum.OBELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OBELOPP)    
         slutsum.TBELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TBELOPP) 
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).                    
      END.     
   END.
   /*MASKINER*/
   FOR EACH dagtemp WHERE dagtemp.PERSMASK = FALSE NO-LOCK 
      BREAK BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK: 
      ACCUMULATE dagtemp.BELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.TIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OTIMMAR (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.OBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).  
      ACCUMULATE dagtemp.TBELOPP (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK).
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         FIND FIRST slutsum WHERE slutsum.AONR = dagtemp.AONR AND
         slutsum.DELNR = dagtemp.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE slutsum THEN CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR  
         slutsum.MBELOPP = slutsum.MBELOPP + 
         (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.OBELOPP) + 
         (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TBELOPP) + 
         (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP).                 
          /*kalmar vill ha med gurua maskiner i uppföljning- ej kostnadsregistreringar */
         IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "xGKAL" THEN slutsum.MBELOPP = 0.
      END.
   END.
   FOR EACH kosttemp2 BREAK BY kosttemp2.AONR BY kosttemp2.DELNR:         
      ACCUMULATE kosttemp2.PERSKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MASKKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.TRAKTKOST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      ACCUMULATE kosttemp2.MTRL (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.OVRKR (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR).  
      ACCUMULATE kosttemp2.INKOMST (TOTAL BY kosttemp2.AONR BY kosttemp2.DELNR). 
      IF LAST-OF(kosttemp2.DELNR) THEN DO:
         CREATE kosttemp.
         ASSIGN 
         kosttemp.AONR = kosttemp2.AONR
         kosttemp.DELNR = kosttemp2.DELNR 
         kosttemp.BELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.PERSKOST)                       
         kosttemp.MBELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MASKKOST) 
         kosttemp.TBELOPP = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.TRAKTKOST)
         kosttemp.MTRL = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.MTRL)    
         kosttemp.OVRKR = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.OVRKR)
         kosttemp.INKOMST = (ACCUM TOTAL BY kosttemp2.DELNR kosttemp2.INKOMST).         
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
      kosttemp2.DELNR = KOSTREG.DELNR
      kosttemp2.BENAMNING = KOSTREG.BENAMNING 
      kosttemp2.BETDATUM = KOSTREG.BETDATUM 
      kosttemp2.BOKKONTO = KOSTREG.BOKKONTO 
      kosttemp2.DELNR = KOSTREG.DELNR 
      kosttemp2.FAKBES = KOSTREG.FAKBES 
      kosttemp2.FAKTNR = KOSTREG.FAKTNR 
     /* kosttemp2.FAKTURERAD = KOSTREG.FAKTURERAD */
      kosttemp2.INKOMST = KOSTREG.INKOMST  
     /* kosttemp2.KOSTAUTO = KOSTREG.KOSTAUTO */
      kosttemp2.LEVKOD = KOSTREG.LEVKOD  
      kosttemp2.MASKKOST = KOSTREG.MASKKOST  
      kosttemp2.MOMS = KOSTREG.MOMS 
      kosttemp2.MTRL = KOSTREG.MTRL 
      kosttemp2.OVRKR = KOSTREG.OVRKR 
      kosttemp2.PERSKOST = KOSTREG.PERSKOST 
      kosttemp2.RADNR = KOSTREG.RADNR 
      kosttemp2.REGDATUM = KOSTREG.REGDATUM 
      kosttemp2.TRAKTKOST = KOSTREG.TRAKTKOST.
      GET NEXT kq NO-LOCK.      
   END.
END PROCEDURE.


PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
