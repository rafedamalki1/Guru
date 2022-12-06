
/*ESFAKONTO.I*/
PROCEDURE utesab_UI:
   IF OPSYS = "UNIX" THEN DO: 
      ASSIGN
      prognamndat = "/u10/guru/export/gurukund".
      prognamnold = "/u12/guru/export/gurukund." + STRING(TODAY,"99999999").               
   END.    
   ELSE DO: 
      ASSIGN
      prognamndat = "d:\delad\PRO9\guru\export\gurukund.txt"
      prognamnold = "d:\delad\PRO9\guru\export\gurukund." + STRING(TODAY,"99999999").   
   END. 
   kommando = SEARCH(prognamndat).
   OUTPUT TO VALUE(prognamndat) APPEND NO-ECHO CONVERT TARGET "swedish-7-bit" SOURCE "iso8859-1".
   IF kommando = ? THEN DO:
      str = "".
      FIND FIRST huvudtemp NO-ERROR. 
      SUBSTRING(str,1,2) = huvudtemp.TRANSKOD.
      SUBSTRING(str,3,6) = huvudtemp.KUND.
      PUT UNFORMATTED str AT 1 SKIP.
   END.
   FOR EACH strokund WHERE strokund.TRANSKOD = "25":
      str = "".
      SUBSTRING(str,1,2) = strokund.TRANSKOD.
      SUBSTRING(str,10,3) = STRING(strokund.FORETAG,"999").          
      SUBSTRING(str,15,7) = STRING(INTEGER(strokund.KUNDNR),"9999999").
      SUBSTRING(str,22,7) = STRING(INTEGER(strokund.FAKTNR),"9999999").
      /*SUBSTRING(str,22,1) = SUBSTRING(strokund.KUNDTYPFAKT,1,1).*/      
      SUBSTRING(str,30,1) = STRING(strokund.DELKUND).
      IF strokund.DELKUND = 1 THEN DO:
         SUBSTRING(str,31,30) = strokund.NAMNKUND.
      END.
      IF strokund.DELKUND = 2 THEN DO:
         SUBSTRING(str,31,25) = strokund.ADRESS.
         SUBSTRING(str,56,6) = STRING(strokund.POSTNR,"999 99").
         SUBSTRING(str,63,18) = strokund.POSTADRESS.         
      END.
      IF strokund.DELKUND = 3 THEN DO:
         SUBSTRING(str,31,55) = strokund.ADRESS.
         
      END.     
      PUT UNFORMATTED str AT 1 SKIP.
   END.  
   FOR EACH strokund WHERE strokund.TRANSKOD = "20":
      str = "".
      SUBSTRING(str,1,2) = strokund.TRANSKOD.
      SUBSTRING(str,10,3) = STRING(strokund.FORETAG,"999").
      SUBSTRING(str,17,6) = STRING(strokund.DATUMFAKT,"999999").
      SUBSTRING(str,23,7) = STRING(INTEGER(strokund.KUNDNR),"9999999").
      SUBSTRING(str,30,7) = STRING(INTEGER(strokund.FAKTNR),"9999999").
      /*IF strovar = TRUE THEN SUBSTRING(str,30,1) = SUBSTRING(strokund.KUNDTYPFAKT,1,1).*/
      SUBSTRING(str,39,6) = STRING(strokund.DATUMFF,"999999").
      SUBSTRING(str,46,9) = STRING(strokund.MBELOPP * 100,"999999999"). 
      SUBSTRING(str,65,10) = STRING(strokund.BELOPP * 100,"9999999999").
      SUBSTRING(str,75,1) = strokund.DEBKRED.
      IF strovar = TRUE THEN SUBSTRING(str,80,1) = strokund.KUNDTYPFAKT.    
      PUT UNFORMATTED str AT 1 SKIP.
   END.
   FOR EACH strokund WHERE strokund.TRANSKOD = "05" AND strokund.ORDNINGNUM = 5:
      str = "".
      SUBSTRING(str,1,2) = strokund.TRANSKOD.
      SUBSTRING(str,10,3) = STRING(strokund.FORETAG,"999").
      SUBSTRING(str,19,6) = STRING(strokund.BOKDATUM,"999999").
      SUBSTRING(str,27,6) = STRING(INTEGER(strokund.FAKTNR),"999999").
      SUBSTRING(str,33,5) = STRING(INTEGER(strokund.KUNDKONTO),"99999").
      /*SUBSTRING(str,39,8) = STRING(INTEGER(strokund.AONR),"999999") + STRING(strokund.DELNR,"99").*/
      SUBSTRING(str,70,10) = STRING(strokund.BELOPP * 100,"9999999999").
      SUBSTRING(str,80,1) = strokund.DEBKRED.
      PUT UNFORMATTED str AT 1 SKIP.
   END.
   FOR EACH strokund WHERE strokund.TRANSKOD = "05" AND strokund.ORDNINGNUM = 6:
      str = "".
      SUBSTRING(str,1,2) = strokund.TRANSKOD.
      SUBSTRING(str,10,3) = STRING(strokund.FORETAG,"999").
      SUBSTRING(str,19,6) = STRING(strokund.BOKDATUM,"999999").
      SUBSTRING(str,27,6) = STRING(INTEGER(strokund.FAKTNR),"999999").
      SUBSTRING(str,33,5) = STRING(INTEGER(strokund.KUNDKONTO),"99999").      
      SUBSTRING(str,70,10) = STRING(strokund.BELOPP * 100,"9999999999").
      SUBSTRING(str,80,1) = strokund.DEBKRED.
      PUT UNFORMATTED str AT 1 SKIP.
   END.
   FOR EACH strokund WHERE strokund.TRANSKOD = "05" AND strokund.ORDNINGNUM = 7:
      str = "".
      SUBSTRING(str,1,2) = strokund.TRANSKOD.
      SUBSTRING(str,10,3) = STRING(strokund.FORETAG,"999").
      SUBSTRING(str,19,6) = STRING(strokund.BOKDATUM,"999999").
      SUBSTRING(str,27,6) = STRING(INTEGER(strokund.FAKTNR),"999999").
      SUBSTRING(str,33,5) = STRING(INTEGER(strokund.KUNDKONTO),"99999").
      SUBSTRING(str,38,1) = strokund.REF2.
      SUBSTRING(str,39,8) = STRING(INTEGER(strokund.AONR),"999999") + STRING(strokund.DELNR,"99").
      SUBSTRING(str,70,10) = STRING(strokund.BELOPP * 100,"9999999999").
      SUBSTRING(str,80,1) = strokund.DEBKRED.
      PUT UNFORMATTED str AT 1 SKIP.     
   END.   
   OUTPUT CLOSE.
   OS-APPEND VALUE(prognamndat) VALUE(prognamnold).   
END PROCEDURE.
PROCEDURE strokud_UI:
   CREATE strokund.
   ASSIGN
   strokund.FORETAG = AVDELNING.AVDELNINGNR
   strokund.ORDNING = skarpvar
   strokund.ORDNINGNUM = 1
   strokund.TRANSKOD = "25"
   strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
   strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
   strokund.FAKTNR = skarpvar
   strokund.DELKUND = 1.
   IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
   IF debkred = FALSE THEN strokund.NAMNKUND = FAKTNAMN.BESTNAMN.
   ELSE strokund.NAMNKUND = FAKTNAMNKRE.BESTNAMN.
   CREATE strokund.
   ASSIGN
   strokund.FORETAG = AVDELNING.AVDELNINGNR
   strokund.ORDNING = skarpvar
   strokund.ORDNINGNUM = 2
   strokund.TRANSKOD = "25"
   strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
   strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
   strokund.FAKTNR = skarpvar
   strokund.DELKUND = 2.
   IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
   IF debkred = FALSE THEN DO:
      ASSIGN
      strokund.POSTNR = FAKTNAMN.FAKPNR
      strokund.POSTADRESS = FAKTNAMN.FAKORT.
      IF SUBSTRING(FAKTNAMN.FAKADRESS,27,25) NE "" THEN DO:              
         strokund.ADRESS = SUBSTRING(FAKTNAMN.FAKADRESS,27,25).               
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 3
         strokund.TRANSKOD = "25"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
         strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
         strokund.FAKTNR = skarpvar
         strokund.DELKUND = 3
         strokund.ADRESS = SUBSTRING(FAKTNAMN.FAKADRESS,1,25).                          
         IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
      END.
      ELSE strokund.ADRESS = SUBSTRING(FAKTNAMN.FAKADRESS,1,25).
   END.
   ELSE DO:
      ASSIGN
      strokund.POSTNR = FAKTNAMNKRE.FAKPNR
      strokund.POSTADRESS = FAKTNAMNKRE.FAKORT. 
      IF SUBSTRING(FAKTNAMNKRE.FAKADRESS,27,25) NE "" THEN DO:              
         strokund.ADRESS = SUBSTRING(FAKTNAMNKRE.FAKADRESS,27,25).               
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 3
         strokund.TRANSKOD = "25"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
         strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
         strokund.FAKTNR = skarpvar
         strokund.DELKUND = 3
         strokund.ADRESS = SUBSTRING(FAKTNAMNKRE.FAKADRESS,1,25).                          
         IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
      END.
      ELSE strokund.ADRESS = SUBSTRING(FAKTNAMNKRE.FAKADRESS,1,25).
   END.          
END PROCEDURE.
PROCEDURE eskundres_UI:
   FIND FIRST FAKTKUNDKONTO WHERE 
   FAKTKUNDKONTO.FAKTNR = fplanr AND  
   FAKTKUNDKONTO.FDELNR = fpdelnr AND 
   FAKTKUNDKONTO.VFAKTNR = skarpvar NO-LOCK NO-ERROR.
   ASSIGN
   varfakturd = FAKTKUNDKONTO.FAKTDATUM 
   varforfalld = FAKTKUNDKONTO.FDATUM.
   OPEN QUERY kq FOR EACH FAKTKUNDKONTO WHERE 
   FAKTKUNDKONTO.FAKTNR = fplanr AND 
   FAKTKUNDKONTO.VFAKTNR = skarpvar NO-LOCK. 
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(FAKTKUNDKONTO):   
      varbelopp = varbelopp + FAKTKUNDKONTO.BELOPP. 
      GET NEXT kq NO-LOCK.
   END.       
   OPEN QUERY mq FOR EACH FAKTMOMS WHERE 
   FAKTMOMS.FAKTNR = fplanr AND 
   FAKTMOMS.VFAKTNR = skarpvar NO-LOCK.  
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(FAKTMOMS):   
      varmbelopp = varmbelopp + FAKTMOMS.MOMSBELOPP. 
      GET NEXT mq NO-LOCK.
   END.
   varmbelopp = varmbelopp + FAKTURERAD.ORESUTJ.
   CREATE strokund.
   ASSIGN
   strokund.FORETAG = AVDELNING.AVDELNINGNR
   strokund.ORDNING = skarpvar
   strokund.ORDNINGNUM = 4
   strokund.TRANSKOD = "20"
   strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
   strokund.DATUMFAKT = varfakturd
   strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
   strokund.FAKTNR = skarpvar                  
   strokund.DATUMFF = varforfalld
   strokund.MBELOPP = varmbelopp
   strokund.BELOPP = varbelopp + varmbelopp
   strokund.DEBKRED = "+".
   IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9". 
   /*KUND ÄR MOMSAD OCH ÖRESUTJ*/
END PROCEDURE.
PROCEDURE esbokfor_UI:
   varbelopp = 0.
   orevar = FALSE.
   FOR EACH FAKTKUNDKONTO WHERE 
   FAKTKUNDKONTO.FAKTNR = fplanr AND 
   FAKTKUNDKONTO.VFAKTNR = skarpvar NO-LOCK BREAK BY 
   FAKTKUNDKONTO.AONR BY FAKTKUNDKONTO.DELNR BY 
   FAKTKUNDKONTO.KUNDKONTOID BY FAKTKUNDKONTO.MOTPARTID:
      ACCUMULATE FAKTKUNDKONTO.BELOPP (TOTAL BY 
      FAKTKUNDKONTO.AONR BY FAKTKUNDKONTO.DELNR BY 
      FAKTKUNDKONTO.KUNDKONTOID BY FAKTKUNDKONTO.MOTPARTID).
      IF LAST-OF(FAKTKUNDKONTO.MOTPARTID) THEN DO:         
         FIND FIRST KUNDFODRAN WHERE KUNDFODRAN.KUNDKONTOID = FAKTKUNDKONTO.KUNDKONTOID NO-LOCK NO-ERROR.
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 5
         strokund.TRANSKOD = "05"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR    
         strokund.BOKDATUM = varfakturd              
         strokund.FAKTNR = skarpvar
         strokund.KUNDKONTO = KUNDFODRAN.KUNDKONTO
         strokund.REF2 = "-"                         
         strokund.AONR = FAKTKUNDKONTO.AONR
         strokund.DELNR = FAKTKUNDKONTO.DELNR
         strokund.BELOPP = (ACCUM TOTAL BY FAKTKUNDKONTO.MOTPARTID FAKTKUNDKONTO.BELOPP) 
         strokund.DEBKRED = "+".  
         IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9". 
         /*ÖRESUTJÄMNAS VIA MOMSEN
         IF orevar = FALSE THEN DO:
            orevar = TRUE.
            strokund.BELOPP = strokund.BELOPP + FAKTURERAD.ORESUTJ.
         END.
         */
      END.
   END.  
   /*KUNDBOK ÄR ÖRESUTJ*/
   orevar = FALSE.
   FOR EACH FAKTMOMS WHERE 
   FAKTMOMS.FAKTNR = fplanr AND 
   FAKTMOMS.VFAKTNR = skarpvar NO-LOCK BREAK BY 
   FAKTMOMS.MOMSID:   
      ACCUMULATE FAKTMOMS.MOMSBELOPP (TOTAL BY 
      FAKTMOMS.MOMSID).
      IF LAST-OF(FAKTMOMS.MOMSID) THEN DO:         
         FIND FIRST MOMSTAB WHERE MOMSTAB.MOMSID = FAKTMOMS.MOMSID NO-LOCK NO-ERROR.
         IF MOMSTAB.MOMSKOD = "" THEN DO:
         END.
         ELSE DO:
            CREATE strokund.
            ASSIGN
            strokund.FORETAG = AVDELNING.AVDELNINGNR
            strokund.ORDNING = skarpvar
            strokund.ORDNINGNUM = 6
            strokund.TRANSKOD = "05"
            strokund.KLIENTNR = AVDELNING.AVDELNINGNR    
            strokund.BOKDATUM = varfakturd
            strokund.FAKTNR = skarpvar              
            strokund.KUNDKONTO = MOMSTAB.MOMSKOD
            strokund.REF2 = "-"                                  
            strokund.BELOPP = (ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP) 
            strokund.DEBKRED = "-".   
            IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
            IF orevar = FALSE THEN DO:
               orevar = TRUE.
               strokund.BELOPP = strokund.BELOPP + FAKTURERAD.ORESUTJ.
            END.         
            FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMS.AONR AND
            vismomstemp.DELNR = FAKTMOMS.DELNR NO-ERROR.
            IF NOT AVAILABLE vismomstemp THEN CREATE vismomstemp.
            ASSIGN
            vismomstemp.AONR = FAKTMOMS.AONR 
            vismomstemp.DELNR = FAKTMOMS.DELNR
            vismomstemp.MOMS = vismomstemp.MOMS + strokund.BELOPP /*FAKTMOMS.MOMSBELOPP*/.
         END.
      END.
   END.
   /*MOMS ÄR ÖRESUTJ*/
   OPEN QUERY vmq FOR EACH vismomstemp,
   EACH strokund WHERE strokund.ORDNING = skarpvar AND
   strokund.ORDNINGNUM = 5 AND
   strokund.AONR = vismomstemp.AONR AND 
   strokund.DELNR = vismomstemp.DELNR.
   GET FIRST vmq.   
   DO WHILE AVAILABLE(vismomstemp):           
      strokund.BELOPP = strokund.BELOPP + vismomstemp.MOMS.      
      GET NEXT vmq.
   END.    
   CLOSE QUERY vmq.
   /*MOMS PÅ KUNDBOK*/

   FOR EACH FAKTINTAKTKONT WHERE 
   FAKTINTAKTKONT.FAKTNR = fplanr AND 
   FAKTINTAKTKONT.VFAKTNR = skarpvar NO-LOCK BREAK BY 
   FAKTINTAKTKONT.AONR BY FAKTINTAKTKONT.DELNR BY 
   FAKTINTAKTKONT.INTAKTID BY FAKTINTAKTKONT.MOTPARTID:
      ACCUMULATE FAKTINTAKTKONT.BELOPP (TOTAL BY 
      FAKTINTAKTKONT.AONR BY FAKTINTAKTKONT.DELNR BY 
      FAKTINTAKTKONT.INTAKTID BY FAKTINTAKTKONT.MOTPARTID).
      IF LAST-OF(FAKTINTAKTKONT.MOTPARTID) THEN DO:         
         FIND FIRST INTAKTTAB WHERE INTAKTTAB.INTAKTID = FAKTINTAKTKONT.INTAKTID NO-LOCK NO-ERROR.
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 7
         strokund.TRANSKOD = "05"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR   
         strokund.BOKDATUM = varfakturd                       
         strokund.FAKTNR = skarpvar
         strokund.KUNDKONTO = INTAKTTAB.INTAKTKONTO
         strokund.REF2 = "-"                  
         strokund.AONR = FAKTINTAKTKONT.AONR
         strokund.DELNR = FAKTINTAKTKONT.DELNR                         
         strokund.BELOPP = (ACCUM TOTAL BY FAKTINTAKTKONT.MOTPARTID FAKTINTAKTKONT.BELOPP) 
         strokund.DEBKRED = "-".   
         IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
      END.
   END.
   /*TIDIGARE UTAN MOMS */
   FIND FIRST FAKTURERINGSTYP WHERE 
   FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
   IF FAKTURERINGSTYP.TIDIGAREFAKT = TRUE AND FAKTURERINGSTYP.SLUT = TRUE THEN DO:
      OPEN QUERY fakintq FOR EACH FAKTINTAKTKONT WHERE 
      FAKTINTAKTKONT.FAKTNR = fplanr AND 
      FAKTINTAKTKONT.VFAKTNR NE skarpvar NO-LOCK, 
      EACH INTAKTTAB WHERE INTAKTTAB.INTAKTID = FAKTINTAKTKONT.INTAKTID NO-LOCK, 
      EACH MOTPART WHERE MOTPART.MOTPARTID = FAKTINTAKTKONT.MOTPARTID NO-LOCK.
      GET FIRST fakintq NO-LOCK.
      DO WHILE AVAILABLE(FAKTINTAKTKONT): 
         IF FAKTINTAKTKONT.VFAKTNR = 0 THEN DO:
         END.
         ELSE DO:
            FIND FIRST strokund WHERE 
            strokund.FORETAG = AVDELNING.AVDELNINGNR AND
            strokund.ORDNING = skarpvar AND 
            strokund.ORDNINGNUM = 7 AND 
            strokund.TRANSKOD = "05" AND
            strokund.KLIENTNR = AVDELNING.AVDELNINGNR  AND 
            strokund.BOKDATUM = varfakturd AND                      
            strokund.FAKTNR = skarpvar AND
            strokund.KUNDKONTO = INTAKTTAB.INTAKTKONTO AND
            strokund.REF2 = "-" AND                 
            strokund.AONR = FAKTINTAKTKONT.AONR AND
            strokund.DELNR = FAKTINTAKTKONT.DELNR AND
            strokund.DEBKRED = "-" NO-ERROR.
            IF AVAILABLE strokund THEN DO:                        
               strokund.BELOPP = strokund.BELOPP + FAKTINTAKTKONT.BELOPP.          
            END.
            ELSE DO:                   
               CREATE strokund. 
               ASSIGN
               strokund.FORETAG = AVDELNING.AVDELNINGNR 
               strokund.ORDNING = skarpvar  
               strokund.ORDNINGNUM = 7 
               strokund.TRANSKOD = "05" 
               strokund.KLIENTNR = AVDELNING.AVDELNINGNR  
               strokund.BOKDATUM = varfakturd                    
               strokund.FAKTNR = skarpvar 
               strokund.KUNDKONTO = INTAKTTAB.INTAKTKONTO
               strokund.REF2 = "-"              
               strokund.AONR = FAKTINTAKTKONT.AONR 
               strokund.DELNR = FAKTINTAKTKONT.DELNR 
               strokund.DEBKRED = "+".
               strokund.BELOPP = FAKTINTAKTKONT.BELOPP.
               IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
               FIND FIRST strokund WHERE 
               strokund.FORETAG = AVDELNING.AVDELNINGNR AND
               strokund.ORDNING = skarpvar AND 
               strokund.ORDNINGNUM = 7 AND 
               strokund.TRANSKOD = "05" AND
               strokund.KLIENTNR = AVDELNING.AVDELNINGNR  AND 
               strokund.BOKDATUM = varfakturd AND                      
               strokund.FAKTNR = skarpvar AND               
               strokund.REF2 = "-" AND                 
               strokund.AONR = FAKTINTAKTKONT.AONR AND
               strokund.DELNR = FAKTINTAKTKONT.DELNR AND
               strokund.DEBKRED = "-" NO-ERROR.
               IF AVAILABLE strokund THEN DO:                        
                  strokund.BELOPP = strokund.BELOPP + FAKTINTAKTKONT.BELOPP.          
               END.
            END.
         END.   
         GET NEXT fakintq NO-LOCK.
      END.
   END.
END PROCEDURE.

PROCEDURE faktimesab_UI:
   RUN ESFAKTIM.P 
   (INPUT debkred, INPUT fplanr, INPUT fpdelnr, INPUT skarpvar).
END PROCEDURE.
PROCEDURE eskundreskre_UI:
   FIND FIRST FAKTKUNDKONTOKRED WHERE 
   FAKTKUNDKONTOKRED.FAKTNR = fplanr AND  
   FAKTKUNDKONTOKRED.FDELNR = fpdelnr AND 
   FAKTKUNDKONTOKRED.VKREDIT = skarpvar NO-LOCK NO-ERROR.
   ASSIGN
   varfakturd = FAKTKUNDKONTOKRED.FAKTDATUM 
   varforfalld = FAKTKUNDKONTOKRED.FDATUM.
   OPEN QUERY kkq FOR EACH FAKTKUNDKONTOKRED WHERE 
   FAKTKUNDKONTOKRED.FAKTNR = fplanr AND 
   FAKTKUNDKONTOKRED.VKREDIT = skarpvar NO-LOCK. 
   GET FIRST kkq NO-LOCK.
   DO WHILE AVAILABLE(FAKTKUNDKONTOKRED):   
      varbelopp = varbelopp + FAKTKUNDKONTOKRED.BELOPP. 
      GET NEXT kkq NO-LOCK.
   END.       
   OPEN QUERY kmq FOR EACH FAKTMOMSKRED WHERE 
   FAKTMOMSKRED.FAKTNR = fplanr AND 
   FAKTMOMSKRED.VKREDIT = skarpvar NO-LOCK.  
   GET FIRST kmq NO-LOCK.
   DO WHILE AVAILABLE(FAKTMOMSKRED):   
      varmbelopp = varmbelopp + FAKTMOMSKRED.MOMSBELOPP. 
      GET NEXT kmq NO-LOCK.
   END.
   varmbelopp = varmbelopp + FAKTKRED.ORESUTJ.
   CREATE strokund.
   ASSIGN
   strokund.FORETAG = AVDELNING.AVDELNINGNR
   strokund.ORDNING = skarpvar
   strokund.ORDNINGNUM = 4
   strokund.TRANSKOD = "20"
   strokund.KLIENTNR = AVDELNING.AVDELNINGNR 
   strokund.DATUMFAKT = varfakturd
   strokund.KUNDNR = SUBSTRING(BESTTAB.VIBESTID,1,6)        
   strokund.FAKTNR = skarpvar                  
   strokund.DATUMFF = varforfalld
   strokund.MBELOPP = varmbelopp
   strokund.BELOPP = varbelopp + varmbelopp
   strokund.DEBKRED = "-".
   IF strovar = TRUE THEN strokund.KUNDTYPFAKT = "9".
END PROCEDURE.
PROCEDURE esbokforkre_UI:
   varbelopp = 0.
   orevar = FALSE.
   FOR EACH FAKTKUNDKONTOKRED WHERE 
   FAKTKUNDKONTOKRED.FAKTNR = fplanr AND 
   FAKTKUNDKONTOKRED.VKREDIT = skarpvar NO-LOCK BREAK BY 
   FAKTKUNDKONTOKRED.AONR BY FAKTKUNDKONTOKRED.DELNR BY 
   FAKTKUNDKONTOKRED.KUNDKONTOID BY FAKTKUNDKONTOKRED.MOTPARTID:
      ACCUMULATE FAKTKUNDKONTOKRED.BELOPP (TOTAL BY 
      FAKTKUNDKONTOKRED.AONR BY FAKTKUNDKONTOKRED.DELNR BY 
      FAKTKUNDKONTOKRED.KUNDKONTOID BY FAKTKUNDKONTOKRED.MOTPARTID).
      IF LAST-OF(FAKTKUNDKONTOKRED.MOTPARTID) THEN DO:         
         FIND FIRST KUNDFODRAN WHERE KUNDFODRAN.KUNDKONTOID = FAKTKUNDKONTOKRED.KUNDKONTOID NO-LOCK NO-ERROR.
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 5
         strokund.TRANSKOD = "05"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR    
         strokund.BOKDATUM = varfakturd              
         strokund.FAKTNR = skarpvar
         strokund.KUNDKONTO = KUNDFODRAN.KUNDKONTO
         strokund.REF2 = "-"                         
         strokund.AONR = FAKTKUNDKONTOKRED.AONR
         strokund.DELNR = FAKTKUNDKONTOKRED.DELNR
         strokund.BELOPP = (ACCUM TOTAL BY FAKTKUNDKONTOKRED.MOTPARTID FAKTKUNDKONTOKRED.BELOPP) 
         strokund.DEBKRED = "-".
         /*VIA MOMSEN
         IF orevar = FALSE THEN DO:
            orevar = TRUE.
            strokund.BELOPP = strokund.BELOPP + FAKTKRED.ORESUTJ.
         END.
         */
      END.
   END.  
   /*KUNDBOK ÄR ÖRESUTJ*/
   orevar = FALSE.
   FOR EACH FAKTMOMSKRED WHERE 
   FAKTMOMSKRED.FAKTNR = fplanr AND 
   FAKTMOMSKRED.VKREDIT = skarpvar NO-LOCK BREAK BY 
   FAKTMOMSKRED.MOMSID:   
      ACCUMULATE FAKTMOMSKRED.MOMSBELOPP (TOTAL BY 
      FAKTMOMSKRED.MOMSID).
      IF LAST-OF(FAKTMOMSKRED.MOMSID) THEN DO:         
         FIND FIRST MOMSTAB WHERE MOMSTAB.MOMSID = FAKTMOMSKRED.MOMSID NO-LOCK NO-ERROR.
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 6
         strokund.TRANSKOD = "05"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR    
         strokund.BOKDATUM = varfakturd
         strokund.FAKTNR = skarpvar              
         strokund.KUNDKONTO = MOMSTAB.MOMSKOD
         strokund.REF2 = "-"                                  
         strokund.BELOPP = (ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP) 
         strokund.DEBKRED = "+".   
         IF orevar = FALSE THEN DO:
            orevar = TRUE.
            strokund.BELOPP = strokund.BELOPP + FAKTKRED.ORESUTJ.
         END.         
         FIND FIRST vismomstemp WHERE vismomstemp.AONR = FAKTMOMSKRED.AONR AND
         vismomstemp.DELNR = FAKTMOMSKRED.DELNR NO-ERROR.
         IF NOT AVAILABLE vismomstemp THEN CREATE vismomstemp.
         ASSIGN
         vismomstemp.AONR = FAKTMOMSKRED.AONR 
         vismomstemp.DELNR = FAKTMOMSKRED.DELNR
         vismomstemp.MOMS = vismomstemp.MOMS + strokund.BELOPP  /*FAKTMOMSKRED.MOMSBELOPP*/.
      END.
   END.
   /*MOMS ÄR ÖRESUTJ*/

   OPEN QUERY vmq FOR EACH vismomstemp,
   EACH strokund WHERE strokund.ORDNING = skarpvar AND
   strokund.ORDNINGNUM = 5 AND
   strokund.AONR = vismomstemp.AONR AND strokund.DELNR = vismomstemp.DELNR.
   GET FIRST vmq.
   DO WHILE AVAILABLE(strokund):           
      strokund.BELOPP = strokund.BELOPP + vismomstemp.MOMS.      
      GET NEXT vmq.
   END.    
   CLOSE QUERY vmq.
   /*MOMS PÅ KUNDBOK*/

   FOR EACH FAKTINTAKTKONTKRED WHERE 
   FAKTINTAKTKONTKRED.FAKTNR = fplanr AND 
   FAKTINTAKTKONTKRED.VKREDIT = skarpvar NO-LOCK BREAK BY 
   FAKTINTAKTKONTKRED.AONR BY FAKTINTAKTKONTKRED.DELNR BY 
   FAKTINTAKTKONTKRED.INTAKTID BY FAKTINTAKTKONTKRED.MOTPARTID:
      ACCUMULATE FAKTINTAKTKONTKRED.BELOPP (TOTAL BY 
      FAKTINTAKTKONTKRED.AONR BY FAKTINTAKTKONTKRED.DELNR BY 
      FAKTINTAKTKONTKRED.INTAKTID BY FAKTINTAKTKONTKRED.MOTPARTID).
      IF LAST-OF(FAKTINTAKTKONTKRED.MOTPARTID) THEN DO:         
         FIND FIRST INTAKTTAB WHERE INTAKTTAB.INTAKTID = FAKTINTAKTKONTKRED.INTAKTID NO-LOCK NO-ERROR.
         CREATE strokund.
         ASSIGN
         strokund.FORETAG = AVDELNING.AVDELNINGNR
         strokund.ORDNING = skarpvar
         strokund.ORDNINGNUM = 7
         strokund.TRANSKOD = "05"
         strokund.KLIENTNR = AVDELNING.AVDELNINGNR   
         strokund.BOKDATUM = varfakturd                       
         strokund.FAKTNR = skarpvar
         strokund.KUNDKONTO = INTAKTTAB.INTAKTKONTO
         strokund.REF2 = "-"                  
         strokund.AONR = FAKTINTAKTKONTKRED.AONR
         strokund.DELNR = FAKTINTAKTKONTKRED.DELNR                         
         strokund.BELOPP = (ACCUM TOTAL BY FAKTINTAKTKONTKRED.MOTPARTID FAKTINTAKTKONTKRED.BELOPP) 
         strokund.DEBKRED = "+".   
      END.
   END.
END PROCEDURE.

PROCEDURE faktimkreesab_UI:
   RUN ESFAKTIM.P 
   (INPUT debkred, INPUT fplanr, INPUT fpdelnr, INPUT skarpvar).
END PROCEDURE.

