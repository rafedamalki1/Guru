
/*------------------------------------------------------------------------
    File        : kalkradd2.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 21 10:36:46 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE BUFFER KALKNUMSUBB FOR KALKNUMSUB.
FOR EACH KALKHUV WHERE KALKHUV.KLOGID = 10 NO-LOCK:
   felvar = FALSE.
   num:
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND KALKNUM.KLOGSUBID = 18 NO-LOCK:
      RUN SUB_UI (OUTPUT felvar).
      IF felvar = TRUE THEN LEAVE num. 
   END.
END.

PROCEDURE SUB_UI :
   DEFINE OUTPUT PARAMETER fel AS LOGICAL NO-UNDO.
   FOR EACH  KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUB.KPID = 4 AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
      FIND FIRST KALKNUMSUBB WHERE KALKNUMSUBB.KALKNR = KALKNUM.KALKNR AND KALKNUMSUBB.KPID = 4 AND KALKNUMSUBB.NUM = KALKNUM.NUM AND
      /*ROWID(KALKNUMSUBB) NE ROWID(KALKNUMSUB)*/
      
      KALKNUMSUBB.NUMSUBID NE KALKNUMSUB.NUMSUBID
             
      NO-LOCK NO-ERROR.
      IF AVAILABLE KALKNUMSUBB THEN DO:
         OUTPUT TO C:\TEMP\kalkradd.TXT APPEND.
         PUT KALKHUV.KALKNR KALKNUM.ARBKOD KALKNUMSUB.NUM KALKNUMSUB.NUMSUBID SKIP.
         OUTPUT CLOSE.
         fel = TRUE.
         RETURN. 
      END.      
   END.

END PROCEDURE.