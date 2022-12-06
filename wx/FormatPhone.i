
/******************************************************************************
   Include File for use in Prograss And WebSpeed applications.
   
          NAME: FormatPhone.i
        AUTHOR: Speedy Mercer
   DESCRIPTION: Formats phone numbers in the format:
                  (nnn)nnn-nnnn for 10 digits
                  (nnnn)nnn-nnnn for 11 digits
                Returns original format otherwise.
        SYNTAX: x = FormatPhone(MyPhone#).
       UPDATED:
            BY:
******************************************************************************/
FUNCTION FormatPhone RETURNS CHARACTER (INPUT pcPhone AS CHARACTER).
   DEF VAR vcOldPhone AS CHAR NO-UNDO.
   DEF VAR i          AS INTEGER NO-UNDO.
   
   ASSIGN vcOldPhone = pcPhone.
   
   /* Strip non-numeric characters */
   pcPhone = REPLACE(REPLACE(REPLACE(REPLACE(pcPhone,"/",""),"?",""),",",""),".","").
   pcPhone = REPLACE(REPLACE(REPLACE(REPLACE(pcPhone,")",""),"(",""),"\","")," ","").
   pcPhone = REPLACE(pcPhone,"-","").
   
   /* Check for non-numeric characters */
   ASSIGN i = INTEGER(pcPhone) NO-ERROR.
   
   /* If there are non-numerics, return unformatted */
   IF (error-status:num-messages > 0 )
   THEN RETURN vcOldPhone. 
   ELSE 
   CASE LENGTH(pcPhone):
      /* Standard (nnn)nnn-nnnn format */
      WHEN 10 
         THEN pcPhone = "(" + SUBSTRING(pcPhone,1,3) + ")" 
                      + SUBSTRING(pcPhone,4,3) + "-" 
                      + SUBSTRING(pcPhone,7,4).
      /* Non-Standard (nnnn)nnn-nnnn format */
      WHEN 11 
         THEN pcPhone = "(" + SUBSTRING(pcPhone,1,4) + ")" 
                      + SUBSTRING(pcPhone,5,3) + "-" 
                      + SUBSTRING(pcPhone,8,4).
      /* Otherwise, do not format */                
      OTHERWISE pcPhone = vcOldPhone.
   END.
   
   RETURN pcPhone. 
END FUNCTION.


