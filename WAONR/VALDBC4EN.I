
/*------------------------------------------------------------------------
    File        : VALDBC4EN.I
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
         valdbtemp.FORETAG = "C4EN"
         valdbtemp.GFORETAG = "C4EN".
         ASSIGN 
         valdbtemp.DBNAMN = valdbtemp.GFORETAG
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db " + valdbtemp.GFORETAG + " -H " + {www2db.I} + " -S 3847 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "C4 Energi AB".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
        