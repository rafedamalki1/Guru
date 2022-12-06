
/*------------------------------------------------------------------------
    File        : klogsubidkalk.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 22 10:35:15 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

FOR EACH KALKHUV WHERE NO-LOCK:
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.


   FOR EACH KALKFAKTORER WHERE KALKFAKTORER.KALKNR = KALKHUV.KALKNR AND KALKFAKTORER.OMRADE = KALKHUV.OMRADE:
      KALKFAKTORER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID.
   END. 
   FOR EACH KALKEGNAPRISER WHERE KALKEGNAPRISER.KALKNR = KALKHUV.KALKNR AND KALKEGNAPRISER.OMRADE = KALKHUV.OMRADE:
      KALKEGNAPRISER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID.
   END.
  
   
   
END.