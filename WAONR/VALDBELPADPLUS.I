
/*------------------------------------------------------------------------
    File        : VALDBELPADPLUS.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Dec 07 11:17:58 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGonWeb116/"
      valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/EKGonWeb10/"
      valdbtemp.FORETAG = "DARPLU"
      valdbtemp.GFORETAG = "ELPADARPLU"
      valdbtemp.DBNAMN = "DARPLU"
      valdbtemp.ORDNING = 2      
      valdbtemp.DBPLATS = "d:\PRO11\GURU\DB\"
      valdbtemp.DBCACHE = ""     
      valdbtemp.VALDB = "DarwinPlus På pc112".
      valdbtemp.WWWFTP = TRUE. 
      valdbtemp.APPCON = {App11elpool.i}.  
      valdbtemp.DBCON = "-db DARPLU -H " + {pc122db.I} + " -S 2803 -N TCP".
CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGoNweb11/"
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "SEKG"
      valdbtemp.GFORETAG = "ELPSEKG"
      valdbtemp.DBNAMN = "SEKG"
      valdbtemp.ORDNING = 6      
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Elpools EKG utveckling OE11".
      valdbtemp.WWWFTP = TRUE.    
      valdbtemp.APPCON = {App11elpool.i}.  
      valdbtemp.DBCON = "-db SEKG -S 3540 -H " + {pc122db.I} +  " -N TCP".     