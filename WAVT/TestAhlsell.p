
/*------------------------------------------------------------------------
    File        : TestAhlsell.p
    Purpose     : 

    Syntax      : 

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE Tahl   AS TESTARGRID NO-UNDO.
RUN PuschEnter_UI.
PROCEDURE PuschEnter_UI :
   Tahl =  NEW TESTARGRID().
   WAIT-FOR Tahl:ShowDialog().
END PROCEDURE.