
/*------------------------------------------------------------------------
    File        : PROJPACK.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Mar 02 17:50:21 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

ASSIGN 
   radprojladd[1] =  "Projektpackning"
   radprojladd[2] =  "Snabbleverans"
   radprojladd[3] =  "Ej tagit ställning".
   
  
   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ASSIGN 
      radprojladd[1] =  "Konstruktionsorder"
      radprojladd[2] =  "Standardorder"
      radprojladd[4] =  "Upplagsorder".
   END.   
