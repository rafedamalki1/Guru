
/*------------------------------------------------------------------------
    File        : DBDISCONN.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Tue Oct 04 11:15:14 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER debdis AS CHARACTER NO-UNDO.


   {DELALIAS.I}
   DISCONNECT rt9 NO-ERROR.
   /*
   DISCONNECT VALUE(debdis) NO-ERROR.
   
   DISCONNECT rt9 .
   */
     

