
/*------------------------------------------------------------------------
    File        : AOMBAPPSUNDI.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Jun 27 15:03:14 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE fakt_UI :   
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONT):     
      RUN hitta_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONT.FAKTNR = valdaao.FAKTNR THEN 
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONT.FAKTNR NO-LOCK NO-ERROR.
         RUN hitta_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST + FAKTINTAKTKONT.BELOPP.
      END.   
      
      GET NEXT fq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE faktk_UI :   
   GET FIRST fkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):     
      RUN hittak_UI (INPUT valdaao.BESTID, INPUT valdaao.FAKTTYP).   
      IF FAKTINTAKTKONTKRED.FAKTNR = valdaao.FAKTNR THEN sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      ELSE DO:
         FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = FAKTINTAKTKONTKRED.FAKTNR NO-LOCK NO-ERROR.
         RUN hittak_UI (INPUT FAKTPLAN.BESTID, INPUT FAKTPLAN.FAKTTYP).         
         sumaotemp.FAKTKOST = sumaotemp.FAKTKOST - FAKTINTAKTKONTKRED.BELOPP.
      END.             
      GET NEXT fkq NO-LOCK.      
   END.
END PROCEDURE.
PROCEDURE hitta_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR 
   NO-ERROR.
   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.      
   END.
   ASSIGN      
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR.            
END PROCEDURE.
PROCEDURE hittak_UI :
   DEFINE INPUT PARAMETER varbes LIKE BESTTAB.BESTID NO-UNDO.
   DEFINE INPUT PARAMETER varftyp LIKE FAKTPLAN.FAKTTYP NO-UNDO.   
   FIND FIRST sumaotemp WHERE sumaotemp.AONR = valdaao.AONR AND sumaotemp.DELNR = valdaao.DELNR 
   NO-ERROR.
   IF NOT AVAILABLE sumaotemp THEN DO:
      CREATE sumaotemp.
   END.
   ASSIGN      
   sumaotemp.AONR = valdaao.AONR
   sumaotemp.DELNR = valdaao.DELNR.            
END PROCEDURE.
PROCEDURE delnrsumma_UI:   
   IF delnrsumma = TRUE THEN DO:
      CREATE tidut.                 
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Summa för " + aonrvar
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(delnrsummalager[4],">>>>>9")                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(delnrsummalager[5],">>>>>>>9").  
      IF delnrsummalager[6] < 0                                          
      THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalager[6],"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalager[6],">>>>>>9").
      IF delnrsummalager[7] < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[7],"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalager[7],">>>>>>9").
      IF delnrsummalager[8] < 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(delnrsummalager[8],"->>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(delnrsummalager[8],">>>>>>>9").
      IF delnrsummalager[9] < 0                                             
      THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(delnrsummalager[9],"->>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(delnrsummalager[9],">>>>>>9").
      IF delnrsummalager[10] > 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(delnrsummalager[10],">>>>>>>9").                       
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(delnrsummalager[10],"->>>>>>9").            
      IF delnrsummalager[11] > 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(delnrsummalager[11],">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(delnrsummalager[11],"->>>>>>9").
      IF delnrsummalager[12] > 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(delnrsummalager[12],">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(delnrsummalager[12],"->>>>>>9").                
      IF delnrsummalager[13] > 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(delnrsummalager[13],"->>>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(delnrsummalager[13],"->>>>>>>9").
      CREATE tidut.                        
      /*ANTAL MINUS*/
      ASSIGN                                           
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".
      IF delnrsummalagerK[4] >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(delnrsummalagerK[4],">>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(delnrsummalagerK[4],"->>>>9").
      IF delnrsummalagerK[5] >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(delnrsummalagerK[5],">>>>>>>9").                                                                       
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(delnrsummalagerK[5],"->>>>>>9").                                                                       
      IF delnrsummalagerK[6] >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalagerK[6],">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(delnrsummalagerK[6],"->>>>>9").
      IF delnrsummalagerK[7] >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalagerK[7],">>>>>>9").
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(delnrsummalagerK[7],"->>>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(delnrsummalagerK[8],"->>>>>>9")                                                                          
      SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(delnrsummalagerK[9],"->>>>>9")                                                                 
      SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(delnrsummalagerK[10],"->>>>>>9").
      IF delnrsummalagerK[11] > 0 
      THEN SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(delnrsummalagerK[11],">>>>>>>9") .  
      ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(delnrsummalagerK[11],"->>>>>>9") .                  
      CREATE tidut.                 
   END.
   
   i = 1.
   DO WHILE i <= breddantal:             
      ASSIGN
      delnrsummalagerK[i] = 0
      delnrsummalager[i] = 0.            
      i = i + 1.
   END.  

END PROCEDURE.
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
   nrcol[12] = 12
   nrcol[13] = 13
   breddantal = 13   /*antal kolumner*/
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 7
   bredd[7] = 7
   bredd[8] =  8
   bredd[9] =  7
   bredd[10] = 8
   bredd[11] = 8
   bredd[12] = 8
   bredd[13] = 9.   
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
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ARBETS"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "ARBETS."         
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "MASKIN"                                          
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "ÖVR.ENT"
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "MATRIEL"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "ÖVRIG"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "SUMMA"
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "INTÄKT".
   str2 = tidut.UT.                 
   CREATE tidut.      
   ASSIGN                               
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = CAPS(Guru.Konstanter:gaok)      
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = CAPS(Guru.Konstanter:gaonamnk)
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "TIMMAR"             
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "KOSTNAD"  
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = "KOSTNAD"
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = "INTÄKT"
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = "RESULTAT"
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = "PLAN" .
   
   str3 = tidut.UT.                
   CREATE tidut.      
   tidut.UT = str.
   IF summaforst = TRUE THEN DO:
      CREATE tidut.      
      tidut.UT = "UF".
      CREATE tidut.      
      tidut.UT = "KA".   
      CREATE tidut.      
   END.
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "UF".
   CREATE sumsum.
   ASSIGN sumsum.PERSONALKOD = "KA".   
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR.       
   CREATE inextrakopptemp.      
   FOR EACH valdaao: 
      FIND FIRST slutsum WHERE slutsum.AONR = valdaao.AONR AND
      slutsum.DELNR = valdaao.DELNR NO-ERROR.
      ASSIGN
      inextrakopptemp.PROGRAM = "FBDEB"
         /*
      inextrakopptemp.KOPPLACHAR1 = valdaao.AONR  
      inextrakopptemp.KOPPLAINT1 =   valdaao.DELNR      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      */
      inextrakopptemp.KOPPLACHAR1 = ?  
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR           
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR.
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp APPEND).
      ASSIGN
      inextrakopptemp.PROGRAM = "FBKRED"
      inextrakopptemp.KOPPLACHAR1 = ?  
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR           
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR.
         /*
      inextrakopptemp.KOPPLACHAR1 = valdaao.AONR  
      inextrakopptemp.KOPPLAINT1 =   valdaao.DELNR      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.   
      */
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp APPEND).
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST kosttemp WHERE kosttemp.AONR = valdaao.AONR AND kosttemp.DELNR = valdaao.DELNR USE-INDEX AONR NO-ERROR.
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = valdaao.AONR AND kalksumsum.DELNR = kalksumsum.DELNR USE-INDEX AONR NO-ERROR.
         IF AVAILABLE kosttemp THEN musz = musz.                  
         ELSE IF AVAILABLE kalksumsum THEN musz = musz.

         ELSE NEXT.
         CREATE slutsum.
         ASSIGN
         slutsum.AONR  = valdaao.AONR 
         slutsum.DELNR = valdaao.DELNR
         slutsum.ORT   = valdaao.ORT.  
      END.         
      ELSE slutsum.ORT = valdaao.ORT.
   END.
   IF uppvaltemp.VISPERAR = TRUE THEN DO:                  
      OPEN QUERY exq FOR EACH extrakopptemp WHERE 
      YEAR(extrakopptemp.SOKDATE[1]) = YEAR(uppvaltemp.STARTDATUM).
   END.
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      OPEN QUERY exq FOR EACH extrakopptemp WHERE 
      extrakopptemp.SOKDATE[1] >= uppvaltemp.STARTDATUM AND 
      extrakopptemp.SOKDATE[1] <= uppvaltemp.SLUTDATUM. 
   END.
   ELSE DO:                  
      OPEN QUERY exq FOR EACH extrakopptemp.
   END.
   GET FIRST exq.
   DO WHILE AVAILABLE(extrakopptemp):     
      FIND FIRST slutsum WHERE slutsum.AONR = extrakopptemp.KOPPLACHAR2 AND
      slutsum.DELNR = extrakopptemp.KOPPLAINT2 NO-ERROR.
      IF NOT AVAILABLE slutsum THEN DO:
         FIND FIRST valdaao WHERE valdaao.AONR = extrakopptemp.KOPPLACHAR2 AND
         valdaao.DELNR = extrakopptemp.KOPPLAINT2 NO-ERROR.
         IF AVAILABLE valdaao THEN DO:
            CREATE slutsum.
            ASSIGN
            slutsum.AONR  = valdaao.AONR 
            slutsum.DELNR = valdaao.DELNR
            slutsum.ORT   = valdaao.ORT.  
         END.        
      END. 
               
      IF AVAILABLE slutsum THEN DO:
         IF extrakopptemp.PROGRAM = "FBDEB" THEN slutsum.MBELOPP = slutsum.MBELOPP + extrakopptemp.SOKDEC[1].
         IF extrakopptemp.PROGRAM = "FBKRED" THEN slutsum.MBELOPP = slutsum.MBELOPP - extrakopptemp.SOKDEC[1].
      END.
      GET NEXT exq.
   END.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.      
   edataapph = ?.
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   fbestapph = ?.   
   FIND FIRST slutsum NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      aonrvar = slutsum.AONR.   
      FOR EACH slutsum USE-INDEX AONR:      
         IF slutsum.AONR NE aonrvar THEN DO:
            RUN delnrsumma_UI.
            aonrvar = slutsum.AONR.        
         END.
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
         IF NOT AVAILABLE kosttemp THEN DO:
            CREATE kosttemp.
            ASSIGN
            kosttemp.AONR = slutsum.AONR 
            kosttemp.DELNR = slutsum.DELNR. 
         END.      
         FIND FIRST sumaotemp WHERE sumaotemp.AONR = slutsum.AONR AND sumaotemp.DELNR = slutsum.DELNR NO-ERROR.
         IF AVAILABLE sumaotemp THEN kosttemp.INKOMST = kosttemp.INKOMST + sumaotemp.FAKTKOST. 
         varin = kosttemp.INKOMST.
         vartot = vartot + (varin - varkalk).
         CREATE tidut.                 
         ASSIGN
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = slutsum.AONR + STRING(slutsum.DELNR,Guru.Konstanter:varforetypchar[1])     
         SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(slutsum.ORT,1,bredd[2])      
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"      
         SUBSTRING(tidut.UT,150) = slutsum.ORT
         SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(slutsum.TIMMAR,">>>>>9")                                                          
         delnrsummalager[4] = delnrsummalager[4] + slutsum.TIMMAR
         SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(slutsum.BELOPP,">>>>>>>9")
         delnrsummalager[5] = delnrsummalager[5] + slutsum.BELOPP.
         IF slutsum.LONKOST + kosttemp.GRAVARE < 0                                          
         THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.LONKOST + kosttemp.GRAVARE,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(slutsum.LONKOST + kosttemp.GRAVARE,">>>>>>9").
         delnrsummalager[6] = delnrsummalager[6] + slutsum.LONKOST + kosttemp.GRAVARE.
         IF kosttemp.MASKOVRIG + slutsum.MBELOPP < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kosttemp.MASKOVRIG + slutsum.MBELOPP,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kosttemp.MASKOVRIG + slutsum.MBELOPP,">>>>>>9").
         delnrsummalager[7] = delnrsummalager[7] + slutsum.MBELOPP + kosttemp.MASKOVRIG.
         IF kosttemp.MTRL < 0 
         THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.MTRL,"->>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kosttemp.MTRL,">>>>>>>9").
         delnrsummalager[8] = delnrsummalager[8] + kosttemp.MTRL.
         IF kosttemp.OVRKR < 0                                             
         THEN SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.OVRKR,"->>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kosttemp.OVRKR,">>>>>>9").
         delnrsummalager[9] = delnrsummalager[9] + kosttemp.OVRKR.
         summakostvar = slutsum.BELOPP + kosttemp.MTRL + slutsum.LONKOST + kosttemp.GRAVARE + 
         kosttemp.MASKOVRIG + slutsum.MBELOPP + kosttemp.OVRKR.
         IF summakostvar > 0 THEN DO:                                  
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(summakostvar,">>>>>>>9").                       
         END.
         ELSE DO:           
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(summakostvar,"->>>>>>9").            
         END.
         delnrsummalager[10] = delnrsummalager[10] + summakostvar.
         IF kosttemp.INKOMST > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kosttemp.INKOMST,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(kosttemp.INKOMST,"->>>>>>9").
         delnrsummalager[11] = delnrsummalager[11] + kosttemp.INKOMST.
         hjalpvar = kosttemp.INKOMST - (slutsum.BELOPP + kosttemp.MTRL + slutsum.LONKOST + kosttemp.GRAVARE + 
         kosttemp.MASKOVRIG + slutsum.MBELOPP + kosttemp.OVRKR).
         IF hjalpvar > 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(hjalpvar,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(hjalpvar,"->>>>>>9").                
         delnrsummalager[12] = delnrsummalager[12] + hjalpvar.
         IF varin - varkalk > 0 THEN
         SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(varin - varkalk,"->>>>>>>9").
         ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(varin - varkalk,"->>>>>>>9").
         delnrsummalager[13] = delnrsummalager[13] + (varin - varkalk).
         FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
         ASSIGN
         sumsum.TIMMAR =  sumsum.TIMMAR  + slutsum.TIMMAR                                                      
         sumsum.BELOPP = sumsum.BELOPP + slutsum.BELOPP + kosttemp.BELOPP
         sumsum.SKOSTNAD = sumsum.SKOSTNAD + slutsum.BELOPP + kosttemp.MTRL + 
         slutsum.LONKOST + kosttemp.GRAVARE + kosttemp.MASKOVRIG + slutsum.MBELOPP +
         kosttemp.OVRKR
         sumsum.SLONKOST = sumsum.SLONKOST + slutsum.LONKOST + kosttemp.GRAVARE 
         sumsum.TINKOMST = sumsum.TINKOMST + (kosttemp.INKOMST - 
         (slutsum.BELOPP + kosttemp.MTRL + slutsum.LONKOST + kosttemp.GRAVARE + kosttemp.MASKOVRIG + 
          slutsum.MBELOPP + kosttemp.OVRKR))
         sumsum.GRAVARE = sumsum.GRAVARE + kosttemp.GRAVARE + slutsum.LONKOST
         sumsum.MASKOVRIG = sumsum.MASKOVRIG + kosttemp.MASKOVRIG + slutsum.MBELOPP         
         sumsum.MTRL = sumsum.MTRL + kosttemp.MTRL
         sumsum.OVRKR = sumsum.OVRKR + kosttemp.OVRKR                                                     
         sumsum.INKOMST = sumsum.INKOMST + kosttemp.INKOMST.         
      
         FIND FIRST kalksumsum WHERE kalksumsum.AONR = slutsum.AONR AND
         kalksumsum.DELNR = slutsum.DELNR USE-INDEX AONR NO-ERROR.
         IF NOT AVAILABLE kalksumsum THEN DO:
            CREATE kalksumsum.
            ASSIGN
            kalksumsum.AONR = slutsum.AONR 
            kalksumsum.DELNR = slutsum.DELNR.
         END.
         IF AVAILABLE kalksumsum THEN DO:
            CREATE tidut.                        
            kalktotvar = kalksumsum.BELOPP + kalksumsum.MBELOPP + 
            kalksumsum.MTRL + kalksumsum.OVRKR + kalksumsum.MASKGBELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.
            ASSIGN                                   
            /*ANTAL MINUS*/
            SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".
            
            IF kalksumsum.MONTTIMMAR + kalksumsum.TIMMAR + kalksumsum.BTIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR + kalksumsum.TIMMAR + kalksumsum.BTIMMAR,">>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.MONTTIMMAR + kalksumsum.TIMMAR + kalksumsum.BTIMMAR,"->>>>9").            
            delnrsummalagerK[4] = delnrsummalagerK[4] + kalksumsum.MONTTIMMAR + kalksumsum.TIMMAR + kalksumsum.BTIMMAR.
            IF kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP,"->>>>>>9").
            delnrsummalagerK[5] = delnrsummalagerK[5] + kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP.
            IF kalksumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.MASKGBELOPP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(kalksumsum.MASKGBELOPP,"->>>>>9").
            delnrsummalagerK[6] = delnrsummalagerK[6] + kalksumsum.MASKGBELOPP.
            IF kalksumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.MBELOPP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(kalksumsum.MBELOPP,"->>>>>9").
            delnrsummalagerK[7] = delnrsummalagerK[7] + kalksumsum.MBELOPP.
            ASSIGN
            SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(kalksumsum.MTRL,"->>>>>>9")
            delnrsummalagerK[8] = delnrsummalagerK[8] + kalksumsum.MTRL
            SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(kalksumsum.OVRKR,"->>>>>9")                                                                 
            delnrsummalagerK[9] = delnrsummalagerK[9] + kalksumsum.OVRKR
            SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(kalktotvar,"->>>>>>9")
            delnrsummalagerK[10] = delnrsummalagerK[10] + kalktotvar.
            
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = slutsum.AONR AND AONRTAB.DELNR = slutsum.DELNR 
            NO-LOCK NO-ERROR.
            IF AVAILABLE AONRTAB THEN DO:      
               ASSIGN kalksumsum.INKOMST = AONRTAB.BETNR.                  
               IF AONRTAB.BETNR > 0 THEN
               SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(AONRTAB.BETNR,">>>>>>>9") .  
               ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(AONRTAB.BETNR,"->>>>>>9") .                  
               delnrsummalagerK[11] = delnrsummalagerK[11] + AONRTAB.BETNR.
            END.        
            /*
            IF kalksumsum.TYP = 6 THEN DO:
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
            END.
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(kalksumsum.TIMMAR,">>>>>9").
               kalksumsum.MONTTIMMAR = kalksumsum.TIMMAR.
               kalksumsum.TIMMAR = 0.
            END.
            */
            FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
            ASSIGN
            sumsum.TIMMAR = sumsum.TIMMAR + kalksumsum.TIMMAR + kalksumsum.MONTTIMMAR + kalksumsum.BTIMMAR 
            sumsum.BELOPP = sumsum.BELOPP + kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP
            sumsum.MASKGBELOPP = sumsum.MASKGBELOPP + kalksumsum.MASKGBELOPP
            sumsum.MBELOPP = sumsum.MBELOPP + kalksumsum.MBELOPP
            sumsum.MTRL = sumsum.MTRL + kalksumsum.MTRL
            sumsum.OVRKR = sumsum.OVRKR + kalksumsum.OVRKR
            sumsum.SKOSTNAD = sumsum.SKOSTNAD + 
            (kalksumsum.BELOPP + kalksumsum.MONTBELOPP + kalksumsum.BERBELOPP + 
            kalksumsum.MBELOPP + kalksumsum.MASKGBELOPP +
            kalksumsum.MTRL + kalksumsum.OVRKR)
            sumsum.INKOMST = sumsum.INKOMST + kalksumsum.INKOMST.               
         END.
         
         CREATE tidut.
      END.
   END.
   FIND LAST slutsum NO-LOCK NO-ERROR.
   IF AVAILABLE slutsum THEN DO:
      RUN delnrsumma_UI.       
   END.
   FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "UF" NO-ERROR.
   IF summaforst = TRUE THEN DO:
      FIND FIRST tidut WHERE tidut.UT = "UF" NO-LOCK NO-ERROR.
      tidut.UT = "".
   END.
   ELSE CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SUMMA".
   ASSIGN       
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "UF"                     
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9")    
   SUBSTRING(tidut.UT,utnr[nrcol[12]]) = STRING(sumsum.TINKOMST,"->>>>>>9").
   IF vartot > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(vartot,"->>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[13]]) = STRING(vartot,"->>>>>>>9").
   IF sumsum.BELOPP < 0                                            
   THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,"->>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,">>>>>>>9").
   IF sumsum.GRAVARE < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.GRAVARE,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.GRAVARE,">>>>>>9").
   IF sumsum.MASKOVRIG < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MASKOVRIG,"->>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MASKOVRIG,">>>>>>9").
   IF sumsum.MTRL < 0 
   THEN SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,"->>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,">>>>>>>9").
   ASSIGN                                                
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                    
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
   IF sumsum.INKOMST > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,"->>>>>>9").      
   FIND FIRST sumsum WHERE sumsum.PERSONALKOD = "KA" NO-ERROR.
   IF summaforst = TRUE THEN DO:
      FIND FIRST tidut WHERE tidut.UT = "KA" NO-LOCK NO-ERROR.
      tidut.UT = "".
   END.
   ELSE CREATE tidut.
   /*ANTAL MINUS*/
   ASSIGN                 
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "KA".
   
   IF sumsum.TIMMAR >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,">>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(sumsum.TIMMAR,"->>>>9").
   IF sumsum.BELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,">>>>>>>9").                                                                       
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(sumsum.BELOPP,"->>>>>>9").                                                                       
   IF sumsum.MASKGBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.MASKGBELOPP,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumsum.MASKGBELOPP,"->>>>>9").
   IF sumsum.MBELOPP >= 0 THEN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MBELOPP,">>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[7]]) = STRING(sumsum.MBELOPP,"->>>>>9").
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[9]]) = STRING(sumsum.OVRKR,"->>>>>9")                                                                     
   SUBSTRING(tidut.UT,utnr[nrcol[10]]) = STRING(sumsum.SKOSTNAD,"->>>>>>9").
   IF sumsum.MTRL > 0 THEN                                  
   SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,">>>>>>>9").
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[8]]) = STRING(sumsum.MTRL,"->>>>>>9").
   IF sumsum.INKOMST > 0 THEN
   SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,">>>>>>>9").         
   ELSE SUBSTRING(tidut.UT,utnr[nrcol[11]]) = STRING(sumsum.INKOMST,"->>>>>>9").                     
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

      IF KOSTREG.BOKKONTO = "471600" THEN DO:      
         kosttemp2.GRAVARE = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST. 
      END.
      ELSE IF KOSTREG.BOKKONTO = "471500" THEN DO:      
         kosttemp2.GRAVARE = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST. 
      END.
      ELSE IF KOSTREG.BOKKONTO BEGINS "47" THEN DO:
         kosttemp2.MASKOVRIG = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.
      ELSE IF KOSTREG.BOKKONTO >= "450000" AND KOSTREG.BOKKONTO <= "459999" THEN DO:
         kosttemp2.MTRL = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.
      ELSE IF KOSTREG.BOKKONTO >= "500000" AND KOSTREG.BOKKONTO <= "999999" THEN DO:
         kosttemp2.OVRKR = KOSTREG.MASKKOST + KOSTREG.MTRL + KOSTREG.OVRKR +  KOSTREG.PERSKOST.          
      END.
      IF KOSTREG.BOKKONTO = "681000" THEN DO:      
         DELETE kosttemp2.
      END.
      ELSE IF KOSTREG.BOKKONTO = "682000" THEN DO:      
         DELETE kosttemp2.
      END.
      ELSE IF KOSTREG.BOKKONTO = "683000" THEN DO:      
         DELETE kosttemp2.
      END.
      ELSE IF KOSTREG.BOKKONTO = "369999" THEN DO:      
         DELETE kosttemp2.
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
      IF SUMTIDDAG.PERSMASK = FALSE THEN musz = musz.
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
         dagtemp.AONR = SUMTIDDAG.AONR
         dagtemp.DELNR = valdaao.DELNR 
         dagtemp.ORT = SUMTIDDAG.ORT
         dagtemp.PERSMASK = SUMTIDDAG.PERSMASK
         dagtemp.LONKOST = SUMTIDDAG.LONKOST
         dagtemp.BELOPP = SUMTIDDAG.BELOPP + SUMTIDDAG.OBELOPP.
         dagtemp.TIMMAR = SUMTIDDAG.TIMMAR + SUMTIDDAG.OTIMMAR.         
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
      ACCUMULATE dagtemp.LONKOST (TOTAL BY dagtemp.AONR BY dagtemp.DELNR BY dagtemp.PERSMASK). 
      IF LAST-OF(dagtemp.PERSMASK) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.AONR = dagtemp.AONR
         slutsum.DELNR = dagtemp.DELNR
         slutsum.ORT = dagtemp.ORT
         slutsum.BELOPP = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.BELOPP)                      
         slutsum.TIMMAR = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.TIMMAR) 
         slutsum.LONKOST = (ACCUM TOTAL BY dagtemp.PERSMASK dagtemp.LONKOST).         
      END.     
   END.
   FOR EACH kosttemp2 BREAK BY kosttemp2.AONR BY kosttemp2.DELNR:     
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
   
END PROCEDURE.   

PROCEDURE procset_UI:  
  IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .


