/*DOKHANTI.I*/
PROCEDURE mappvar:
   DEFINE INPUT PARAMETER varmapp AS CHARACTER NO-UNDO.
   IF varmapp NE "" THEN DO:       
      FILE-INFO:FILE-NAME = varmapp.
      IF FILE-INFO:FULL-PATHNAME = ? THEN OS-CREATE-DIR VALUE(varmapp).      
   END.
   /*   net use n: \\stugan.energi-miljo.se\guru /u:energi-miljo\gurusrvc guru2000*/
END PROCEDURE.
PROCEDURE delmappvar:
   DEFINE INPUT PARAMETER varmapp AS CHARACTER NO-UNDO.
   OS-DELETE VALUE(varmapp) RECURSIVE.
END PROCEDURE.

PROCEDURE bortfil.
   DEFINE INPUT PARAMETER filnamnvar AS CHARACTER NO-UNDO.
   IF  filnamnvar = "" OR filnamnvar = ? THEN DO:
      RETURN.
   END.
   OS-DELETE VALUE(filnamnvar).
   
END PROCEDURE.

PROCEDURE osmappvar :
   DEFINE INPUT PARAMETER fil AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER mappvar AS CHARACTER NO-UNDO.
   OS-COPY VALUE(fil) VALUE(mappvar).
END PROCEDURE.
