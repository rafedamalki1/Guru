
/*------------------------------------------------------------------------
    File        : VALDBANDERS.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Feb 18 09:58:46 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/



         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGonWeb116/"
         valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/EKGonWeb10/"
         valdbtemp.FORETAG = "SEKG"
         valdbtemp.GFORETAG = "SEKG"
         valdbtemp.DBNAMN = "SEKG"
         valdbtemp.VALDB = "EKG  ANDERS"
         valdbtemp.ORDNING = 1.      
         
           
         ASSIGN 
         valdbtemp.GFORETAG = "Anders"    
         valdbtemp.DBCON = "-db D:\pro11\GURU\DB\" +  valdbtemp.DBNAMN + " -1"
         valdbtemp.DBPLATS = "D:\pro11\GURU\DB\"
         valdbtemp.DBCACHE = ""      
          
         valdbtemp.WWWFTP = TRUE.

             
        


/*
     
     /*
     CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "SNAT"
      valdbtemp.GFORETAG = "Anders"
      valdbtemp.DBNAMN = "SUNDNAT"
      valdbtemp.ORDNING = 5      
      valdbtemp.DBCON = "-db D:\pro11\GURU\DB\SUNDNAT -1"
      valdbtemp.DBPLATS = "D:\pro11\GURU\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Guru Anders".
      valdbtemp.WWWFTP = FALSE.
      valdbtemp.APPCON = "-AppService App116 -H " + {pc122db.I} + "-S 2955". 
      CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GRAN"
         valdbtemp.VALDB = "One Nordic On Web ANDERS" 
         valdbtemp.DBNAMN = "ONEWWW"
         valdbtemp.VALDB = "One Nordic On Web ANDERS"
         valdbtemp.ORDNING = 2.      
         ASSIGN 
         valdbtemp.GFORETAG = "Anders"    
         valdbtemp.DBCON = "-db D:\pro11\GURU\DB\ONEWWW -1"
         valdbtemp.DBPLATS = "D:\pro11\GURU\DB\"
         valdbtemp.DBCACHE = ""      
          
         valdbtemp.WWWFTP = TRUE.    
          valdbtemp.APPCON = "-AppService App116 -H " + {pc122db.I} + "-S 2955". 
      */    
     
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "Anders"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 5   
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService App116 -H PC122 -S 2955"
       valdbtemp.VALDB = "Guru Anders".
      valdbtemp.WWWFTP = FALSE.  
      valdbtemp.DBCON  = "-db RT9 -S 3327 -H " + {pc122db.I} + "  -N TCP".  

CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "Anders"
      valdbtemp.DBNAMN = "RT9kalk"
      valdbtemp.ORDNING = 5   
      valdbtemp.DBCON = "-db RT9kalk -1"
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
       valdbtemp.VALDB = "Guru Anders".
      valdbtemp.WWWFTP = FALSE.
      valdbtemp.APPCON = "-AppService App116 -H " + {pc122db.I} + "-S 2955". 
*/