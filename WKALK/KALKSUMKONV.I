
/*------------------------------------------------------------------------
    File        : KALKSUMKONV.I
    Purpose     : 
   -------KVID TIMTYP--- KOSTTYP---EABER- -TTRECID ORDNINGTIMMAR ORDNINGKOSTNAD

         1  Beredare  Arbete    no    1597700             1              1
         2  Montör    Arbete    yes   1597701             2              1
         3  Maskin    Maskin    yes   1597702             3              3
         4            Övrigt    no    1597696             1              4
         5            Materiel  no    1597697             1              2
         6  Utrustni  Utrustni  yes   1597698             1              5
         7            Övrigt    yes   1597699             1              4
obs!    UKOST ska inte tas med i totlakostnaden den ligger i  OVRKR
  ----------------------------------------------------------------------*/

   FOR EACH kalkantal NO-LOCK:
      IF kalkantal.KVID = 1 THEN DO:
          kalksumsum.BTIMMAR = kalksumsum.BTIMMAR + kalkantal.SUMMA.  
       END.
       ELSE IF kalkantal.KVID = 2 THEN DO:
          kalksumsum.TIMMAR = kalksumsum.TIMMAR + kalkantal.SUMMA.  
       END.
       ELSE IF kalkantal.KVID = 3 THEN DO:
          kalksumsum.MASKTIMMAR = kalksumsum.MASKTIMMAR + kalkantal.SUMMA.  
       END.
       ELSE IF kalkantal.KVID = 6 THEN DO:
          kalksumsum.UTIMMAR = kalksumsum.UTIMMAR + kalkantal.SUMMA.  
       END.
    END.
    
    FOR EACH kalkkostnad NO-LOCK:
       IF frikalkvar = TRUE THEN kalksumsum.EAMANGD = kalksumsum.EAMANGD + kalkkostnad.FRIEAMANGD.
       ELSE kalksumsum.EAMANGD = kalksumsum.EAMANGD + kalkkostnad.EAMANGD.
       IF kalkkostnad.KVID = 1 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.BELOPP = kalksumsum.BELOPP + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.BELOPP = kalksumsum.BELOPP + kalkkostnad.TOTKOST.  
       END.
       ELSE IF kalkkostnad.KVID = 2 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.BELOPP = kalksumsum.BELOPP + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.BELOPP = kalksumsum.BELOPP + kalkkostnad.TOTKOST.  
       END.
       ELSE IF kalkkostnad.KVID = 3 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.MASKGBELOPP = kalksumsum.MASKGBELOPP + kalkkostnad.TOTKOST.  
       END.
       ELSE IF kalkkostnad.KVID = 4 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.TOTKOST.  
       END.
       ELSE IF kalkkostnad.KVID = 5 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.MTRL = kalksumsum.MTRL + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.MTRL = kalksumsum.MTRL + kalkkostnad.TOTKOST.  
       END.
       ELSE IF kalkkostnad.KVID = 6 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.TOTKOST. 
          IF frikalkvar = TRUE THEN kalksumsum.UKOST = kalksumsum.UKOST + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.UKOST = kalksumsum.UKOST + kalkkostnad.TOTKOST. 
       END.
       ELSE IF kalkkostnad.KVID = 7 THEN DO:
          IF frikalkvar = TRUE THEN kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.FRITOTKOST.
          ELSE kalksumsum.OVRKR = kalksumsum.OVRKR + kalkkostnad.TOTKOST.  
       END.
    END.
    
   
   
    