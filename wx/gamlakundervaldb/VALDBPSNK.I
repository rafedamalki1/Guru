
/*------------------------------------------------------------------------
    File        : VALDBPSNK.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Aug 24 11:18:45 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "PSNK"
         valdbtemp.GFORETAG = "PSNK".
         ASSIGN 
         valdbtemp.DBNAMN = valdbtemp.GFORETAG
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db " + valdbtemp.GFORETAG + " -H " + {www2db.I} + " -S 2899 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "PSN Kraftkonsult Eldistribution AB".  
         valdbtemp.WWWFTP = TRUE. 