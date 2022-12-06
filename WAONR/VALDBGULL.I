
/*------------------------------------------------------------------------
    File        : VALDBGULL.I
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
         valdbtemp.FORETAG = "GULL"
         valdbtemp.GFORETAG = "GULL".
         ASSIGN 
         valdbtemp.DBNAMN = valdbtemp.GFORETAG
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db " + valdbtemp.GFORETAG + " -H " + {www2db.I} + " -S 3849 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Gullströms Kraft AB".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
         