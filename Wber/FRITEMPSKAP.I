
/*------------------------------------------------------------------------
    File        : FRITEMPSKAP.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 29 12:48:00 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/
     DEFINE VARIABLE valtskaptyp AS CHARACTER NO-UNDO.
     DEFINE VARIABLE kabut AS LOGICAL NO-UNDO.
     OPEN QUERY kq FOR EACH konbuff WHERE konbuff.NUM = kon_val.NUM NO-LOCK BY konbuff.SKAPNUM.
      GET FIRST kq NO-LOCK.
      DO WHILE AVAILABLE(konbuff):
         IF konbuff.SKAPNUM = -98 THEN musz = musz.
         ELSE DO: 
             
            FIND FIRST fri_temp WHERE fri_temp.NUM = konbuff.NUM AND
            fri_temp.KABNR = konbuff.SKAPNUM NO-LOCK NO-ERROR.
            IF AVAILABLE fri_temp THEN DO:
               IF fri_temp.SKAP NE "" THEN valtskaptyp = fri_temp.SKAP.  
            END. 
            IF NOT AVAILABLE fri_temp THEN DO:
               CREATE fri_temp.
               ASSIGN
               fri_temp.SKAP = valtskaptyp
               fri_temp.VARSKAPAD = TRUE 
               fri_temp.NUM = konbuff.num                  
               fri_temp.KABNR = konbuff.SKAPNUM 
               fri_temp.ARTAL = YEAR(TODAY).
            END.              
            IF kon_val.GRUP = 0 THEN DO:
               IF konbuff.F2 = "CONNECTIT" THEN . 
               ELSE IF konbuff.F2 NE "" THEN DO: 
                  FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP 
                  NO-LOCK NO-ERROR.                   
                  FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konbuff.GRUPP AND
                  konstvaltemp.KTYPKOD = konbuff.F1 AND konstvaltemp.KVALKOD = konbuff.F2 AND
                  konstvaltemp.BB = bbenamntemp.B2 AND konstvaltemp.KOPP = TRUE
                  USE-INDEX KOPIA NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE konstvaltemp THEN DO:
                     DEBUGGER:SET-BREAK().
                     RUN laddatempen_UI IN bervalapph (INPUT konbuff.GRUPP, OUTPUT TABLE konstvaltemp APPEND). 
                     FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konbuff.GRUPP AND
                     konstvaltemp.KTYPKOD = konbuff.F1 AND konstvaltemp.KVALKOD = konbuff.F2 AND
                     konstvaltemp.BB = bbenamntemp.B2 AND konstvaltemp.KOPP = TRUE
                     USE-INDEX KOPIA NO-LOCK NO-ERROR.
                  END. 
                  IF AVAILABLE konstvaltemp THEN fri_temp.BENAMNING = konstvaltemp.BENAMNING.
                  ELSE fri_temp.BENAMNING =  konbuff.F2.                            
                 
               END.   
            END.
            /*markstation */           
            ELSE DO:
               /*IF Guru.Konstanter:globforetag = "vast" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "elpa" THEN DO:*/
               /*onevatt*/
               IF Guru.Konstanter:varforetypchar[5] = "12,30,33,37,38,45" OR Guru.Konstanter:varforetypchar[5] = "33,34,36,37"   THEN DO:
                  /*HSP VAST*/
                  IF konbuff.F2 NE "" THEN DO:                     
                     FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konbuff.GRUPP AND
                     konstvaltemp.KTYPKOD = konbuff.F1 AND konstvaltemp.KVALKOD = konbuff.F2 AND
                     konstvaltemp.BB = bbenamntemp.B2 AND konstvaltemp.KOPP = TRUE
                     USE-INDEX KOPIA NO-LOCK NO-ERROR. 
                     IF AVAILABLE konstvaltemp THEN DO:
                        /* tillagt LSP = 2 TYP = hsp för att säkerställa att den alltid blir klassad som hsp Lena 20160119*/                        
                        ASSIGN   
                        fri_temp.LSP = 2            
                        fri_temp.TYP = "hsp"
                        fri_temp.BENAMNING = konstvaltemp.BENAMNING.
                     END.      
                    
                  END.
               END.
               IF konbuff.F5 NE "" THEN DO:
                  IF konbuff.F5 = "CONNECTIT" THEN DO:
                     musz = musz.
                  END.
                  ELSE DO:       
                     FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konbuff.GRUPP AND
                     konstvaltemp.KTYPKOD = konbuff.F1 AND konstvaltemp.KVALKOD = konbuff.F5 AND
                     konstvaltemp.BB = bbenamntemp.B5 AND konstvaltemp.KOPP = TRUE
                     USE-INDEX KOPIA NO-LOCK NO-ERROR. 
                     /*
                     FIND FIRST fribuff WHERE fribuff.NUM = konbuff.NUM NO-LOCK NO-ERROR.
                     */
                     kabut = TRUE.
                     /*onevatt*/
                     /*IF Guru.Konstanter:globforetag = "vast" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "elpa" THEN DO:*/
                     IF Guru.Konstanter:varforetypchar[5] = "12,30,33,37,38,45" OR Guru.Konstanter:varforetypchar[5] = "33,34,36,37"   THEN DO:
                        /* om  kabeln finns med bland lågspänningskablar KONSKOD = 14. Högspänningskablar ska ej med i kortet*/
                        FIND FIRST konsttemp WHERE konsttemp.KONSKOD = 14 AND
                        konsttemp.KTYPKOD =  LEFT-TRIM(konstvaltemp.KVALKOD) NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE konsttemp THEN kabut = FALSE.
                     END.
                     IF kabut = TRUE THEN DO:                     
                        IF AVAILABLE konstvaltemp THEN DO:
                           fri_temp.BENAMNING = konstvaltemp.BENAMNING.
                        END.
                     END.  
                     /*/* om  kabeln finns med bland lågspänningskablar KONSKOD = 14. Högspänningskablar ska ej med i kortet*/
                     FIND FIRST konsttemp WHERE konsttemp.KONSKOD = 14 AND
                     konsttemp.KTYPKOD =  LEFT-TRIM(konstvaltemp.KVALKOD) NO-LOCK NO-ERROR.
                     IF AVAILABLE konsttemp THEN DO: */                       
                     /*
                     IF kabut = TRUE THEN DO:                     
                        CREATE fri_temp.
                        ASSIGN
                        fri_temp.KABADR = kabelandben
                        fri_temp.NUM = konbuff.num                           
                        fri_temp.KABNR = konbuff.SKAPNUM
                        fri_temp.ARTAL = YEAR(TODAY).                       
                        IF AVAILABLE konstvaltemp THEN DO:
                           fri_temp.BENAMNING = konstvaltemp.BENAMNING.
                           FIND FIRST list_mtrl WHERE list_mtrl.NUM = konbuff.num AND list_mtrl.SKAPNUM = konbuff.SKAPNUM AND list_mtrl.LINKAB = TRUE NO-LOCK NO-ERROR.
                           IF AVAILABLE list_mtrl THEN DO:                              
                              fri_temp.Meter = list_mtrl.ANTAL. 
                           END.
                        END.   
                        IF AVAILABLE fribuff THEN DO:
                           ASSIGN
                           fri_temp.SKAP = fribuff.SKAP
                           fri_temp.SKAPADR = fribuff.SKAPADR.
                        END.                        
                        fri_temp.SAKR = sakrstor.
                        IF Guru.Konstanter:varforetypval[44] = 1   THEN DO:                                       
                           CREATE markgrupp.
                           ASSIGN                           
                           markgrupp.NUM = fri_temp.NUM
                           markgrupp.KABNR = fri_temp.KABNR.                           
                        END.
                        
                         THEN DO:                           
                           RUN kollahsplsp_UI.
                        END.
                        ELSE DO:
                           FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konbuff.GRUPP AND
                           konstvaltemp.KTYPKOD = konbuff.F1 AND konstvaltemp.KVALKOD = konbuff.F3 AND
                           konstvaltemp.BB = bbenamntemp.B3 AND konstvaltemp.KOPP = TRUE
                           USE-INDEX KOPIA NO-LOCK NO-ERROR.
                           IF AVAILABLE konstvaltemp THEN DO:
                              DEBUGGER:SET-BREAK().                              
                              fri_temp.APPARAT = konstvaltemp.BENAMNING.
                              IF Guru.Konstanter:globforetag = "LULE" THEN DO:                           
                                  FIND FIRST list_mtrl WHERE list_mtrl.NUM = konbuff.num AND list_mtrl.SKAPNUM = konbuff.SKAPNUM AND list_mtrl.TYPBER = TRUE NO-LOCK NO-ERROR.
                                  IF AVAILABLE list_mtrl THEN DO:
                                     IF SUBSTRING(fri_temp.APPARAT,(LENGTH(fri_temp.APPARAT) - 1),2) = "K" + STRING(konbuff.TYP) THEN.
                                     ELSE fri_temp.APPARAT = fri_temp.APPARAT +  " K" + STRING(konbuff.TYP).
                                 END.
                              END.                            
                           END.
                        END.
                        
                     END.
                     */
                  END.
               END.
            END.
            
              
         END.
         GET NEXT kq NO-LOCK.
      END.
