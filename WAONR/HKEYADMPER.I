
/*------------------------------------------------------------------------
    File        : HKEYADMPER.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Jan 19 10:32:54 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/
   /*
   
   */
   IF PROVERSION BEGINS "10" THEN  RUN HKEYADMPER10.p (OUTPUT hkeyvar).
   IF PROVERSION BEGINS "11" THEN RUN HKEYADMPER11.p (OUTPUT hkeyvar).