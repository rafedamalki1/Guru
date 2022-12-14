/*XFLYTFA.P*/
{AMERICANEUROPEAN.I}
FIND FIRST faktplan WHERE faktplan.faktnr = 19 NO-LOCK.
OUTPUT TO c:\protemp9\faktplan.d.
EXPORT faktplan.
OUTPUT TO c:\protemp9\fAKTURERAD.d.
FOR EACH FAKTURERAD WHERE faktplan.faktnr = 19 NO-LOCK:
   EXPORT FAKTURERAD.
END.
OUTPUT TO c:\protemp9\fAKTSTART.d.
FOR EACH FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.        
   EXPORT faktstart.
END.   
OUTPUT TO c:\protemp9\fAKTKOST.d.
FOR EACH FAKTKOST WHERE fAKTKOST.FAKTNR = FAKTPLAN.FAKTNR: 
   EXPORT faktkost.
END. 
OUTPUT TO c:\protemp9\fakttid.d.
FOR EACH FAKTTID WHERE FAKTTID.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.
   EXPORT fakttid.    
END.
OUTPUT TO c:\protemp9\faktfria.d.
FOR EACH FAKTFRIA WHERE FAKTFRIA.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.
   EXPORT faktfria.
END.
OUTPUT TO c:\protemp9\faktmtrl.d.
FOR EACH FAKTMTRL WHERE FAKTMTRL.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.  
   EXPORT faktmtrl.    
END.
OUTPUT TO c:\protemp9\fAKTINTAKTKONT.d.
FOR EACH FAKTINTAKTKONT WHERE 
FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.
    EXPORT    FAKTINTAKTKONT.
 
END.
OUTPUT TO c:\protemp9\fAKTAONRKONTO.d.
FOR EACH FAKTAONRKONTO WHERE 
FAKTAONRKONTO.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.
EXPORT    FAKTAONRKONTO.
 
END.
OUTPUT TO c:\protemp9\fAKTKUNDKONTO.d.
FOR EACH FAKTKUNDKONTO WHERE 
FAKTKUNDKONTO.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.
    EXPORT    FAKTKUNDKONTO.
END.
OUTPUT TO c:\protemp9\fAKTMOMS.d.
FOR EACH FAKTMOMS WHERE 
    FAKTMOMS.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.
EXPORT    FAKTMOMS.
END.
OUTPUT TO c:\protemp9\fAKTKOLL.d.                                   
FOR EACH FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR 
NO-LOCK.
   EXPORT    FAKTKOLL.
END.
OUTPUT TO c:\protemp9\fAKTUPPARB.d. 
FOR EACH FAKTUPPARB WHERE 
FAKTUPPARB.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.
   EXPORT FAKTUPPARB.
END.
OUTPUT TO c:\protemp9\faktnamn.d. 
FOR EACH faktnamn WHERE 
FAKTNAMN.FAKTURNR = FAKTPLAN.FAKTNR  NO-LOCK.
   EXPORT faktnamn.
END.
OUTPUT TO c:\protemp9\faktaonr.d. 
FOR EACH faktaonr WHERE 
faktaonr.FAKTNR = FAKTPLAN.FAKTNR  NO-LOCK.
   EXPORT faktaonr.
END.
OUTPUT TO c:\protemp9\fAKTBEF.d. 
FOR EACH FAKTBEF WHERE FAKTBEF.FAKTNR = FAKTPLAN.FAKTNR 
NO-LOCK.
   EXPORT faktbef.
END.
OUTPUT TO c:\protemp9\fAKTREGLER.d. 
FOR EACH FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
 NO-LOCK.         
   EXPORT faktregler.
END.
OUTPUT TO c:\protemp9\fAKTOVER.d.   
FOR EACH FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR 
 USE-INDEX FAKTOVER NO-LOCK.                 
 EXPORT FAKTOVER.
   
   END.
OUTPUT TO c:\protemp9\fAKTPRISLISTA.d.   
FOR EACH FAKTPRISLISTA WHERE FAKTPRISLISTA.FAKTNR = FAKTPLAN.FAKTNR:
      EXPORT FAKTPRISLISTA.
   END.
OUTPUT TO c:\protemp9\aonrtab.d.   
FOR EACH AONRTAB WHERE AONRTAB.FAKTNR = FAKTPLAN.FAKTNR:
   EXPORT aonrtab.
END.
OUTPUT TO c:\protemp9\kostreg.d.   
FOR EACH AONRTAB WHERE AONRTAB.FAKTNR = FAKTPLAN.FAKTNR:
   FOR EACH kostreg WHERE kostreg.aonr = aonrtab.aonr AND kostreg.delnr = aonrtab.delnr NO-LOCK.
      EXPORT kostreg.
   END.
END.
{EUROPEANAMERICAN.I}
