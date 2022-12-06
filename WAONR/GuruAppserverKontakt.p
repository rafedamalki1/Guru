 
/*------------------------------------------------------------------------
    File        : GuruAppserverKontakt.p
    Purpose     : s≈ ATT MAN INTE TAPPAR KONTAKTEN VID INAKTIVITET

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Dec 05 15:20:31 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE kontakt AS LOGICAL NO-UNDO.

RUN Kontakt.


RUN GuruAppserverKontaktM.p.

PROCEDURE Kontakt:
  
   kontakt = TRUE.
END PROCEDURE.