/*start.p*/
                
def NEW SHARED TEMP-TABLE ttTest  NO-UNDO
   FIELD iValue AS INTEGER.

DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.

DO iCount = 1 TO 10:
    CREATE ttTest.
    ASSIGN iValue = iCount.
END.
                  
run brwstart2.w.





