/* r-factrl.p */

PROCEDURE Factorial:
   DEFINE INPUT PARAMETER  PTerm AS INTEGER.
   DEFINE OUTPUT PARAMETER FactorialResult AS INTEGER.

   DEFINE VARIABLE WorkingResult AS INTEGER.
   
   IF  PTerm <= 1 THEN DO:
      FactorialResult = 1.
      RETURN.
   END.
   ELSE DO:
      RUN Factorial (INPUT  PTerm - 1, OUTPUT WorkingResult).
      FactorialResult =  PTerm * WorkingResult.
   END.
END PROCEDURE.

DEFINE VARIABLE FactorialResult AS INTEGER.
DEFINE VARIABLE FactorialInput AS INTEGER.

REPEAT:
    SET FactorialInput.
    RUN Factorial (INPUT FactorialInput, OUTPUT FactorialResult).
    DISPLAY FactorialResult.   
END.   
