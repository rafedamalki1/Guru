
/*------------------------------------------------------------------------
    File        : VALDBLENA.I
    Purpose     : 

    Syntax      :

    Description : 12345678901234567890
  Jaggillarskidor&5419

    Author(s)   : 
    Created     : Mon Feb 18 09:59:47 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/
  
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "SNAT"
      valdbtemp.GFORETAG = "Lena"
      valdbtemp.DBNAMN = "sundnatmon"
      valdbtemp.ORDNING = 5      
      valdbtemp.DBCON = "-db D:\PRO11\GURU\DB\sundnatmon -1"
      valdbtemp.DBPLATS = "D:\PRO11\GURU\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService App116 -H PC208 -S 2955"
      valdbtemp.VALDB = "Guru Lena".
      valdbtemp.WWWFTP = FALSE.  
