
/*------------------------------------------------------------------------
    File        : DYNQUIT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Sep 22 11:51:23 CEST 2021
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE Quit_UI :
   MESSAGE "Du avslutar Guru nu! Har du sparat allt du behöver?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO UPDATE quitfraga AS LOGICAL.       
   CASE quitfraga:
      WHEN TRUE THEN DO:
         QUIT.            
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.   
   END CASE.
  
END PROCEDURE.
