/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE h-pp2 AS HANDLE.
DEFINE VARIABLE h-pp3 AS HANDLE.
DEFINE VARIABLE h-proc AS HANDLE.   
DEFINE VARIABLE pp2-cnxt AS LOGICAL LABEL "Proc. 2 Context Available?".
DEFINE VARIABLE pp3-cnxt AS LOGICAL LABEL "Proc. 3 Context Available?". 
DEFINE VARIABLE File-list AS CHARACTER FORMAT "x(24)" LABEL "Contexts" 
    VIEW-AS TEXT.
DEFINE BUTTON b-pp2-on LABEL "Run Procedure 2".  
DEFINE BUTTON b-pp2-off LABEL "Delete Context 2".  
DEFINE BUTTON b-pp3-on LABEL "Run Procedure 3".
DEFINE BUTTON b-pp3-off LABEL "Delete Context 3". 
DEFINE BUTTON b-num-cnxt LABEL "Check Contexts".  
DEFINE BUTTON b-exit LABEL "Exit".
/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1) 
    SPACE pp2-cnxt SPACE(3) b-pp2-on SPACE(3) b-pp2-off SPACE SKIP(1)
    SPACE pp3-cnxt SPACE(3) b-pp3-on SPACE(3) b-pp3-off SPACE SKIP(1)    
    SPACE File-list SPACE(3) b-num-cnxt SPACE(3) SKIP(2)
    b-exit TO 40 SKIP(1)
        WITH SIDE-LABELS ROW 2 CENTERED THREE-D
            TITLE "Creating and Deleting Procedure Contexts".        
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF b-pp2-on
DO:
    IF NOT pp2-cnxt THEN DO:  
        RUN lt-12-02.p PERSISTENT SET h-pp2. 
        pp2-cnxt = VALID-HANDLE(h-pp2).
        DISPLAY pp2-cnxt WITH FRAME Frame1.   
    END.
    RUN p-message IN h-pp2.
END.
ON CHOOSE OF b-pp2-off
DO:
    DELETE PROCEDURE h-pp2 NO-ERROR. 
    pp2-cnxt = VALID-HANDLE(h-pp2).
    DISPLAY pp2-cnxt WITH FRAME Frame1.      
END.
ON CHOOSE OF b-pp3-on
DO:        
    IF NOT pp3-cnxt THEN DO:  
        RUN lt-12-03.p PERSISTENT SET h-pp3. 
        pp3-cnxt = VALID-HANDLE(h-pp3).
        DISPLAY pp3-cnxt WITH FRAME Frame1.   
    END.
    RUN p-message IN h-pp3.  
END.
ON CHOOSE OF b-pp3-off
DO:
    DELETE PROCEDURE h-pp3 NO-ERROR.  
    pp3-cnxt = VALID-HANDLE(h-pp3).   
    DISPLAY pp3-cnxt WITH FRAME Frame1.    
END.
ON CHOOSE OF b-num-cnxt
DO:   
    File-list = "".
    h-proc = SESSION:FIRST-PROCEDURE.
    REPEAT WHILE VALID-HANDLE(h-proc).
        File-list = File-list + " " + h-proc:FILE-NAME.
        h-proc = h-proc:NEXT-SIBLING. 
    END.  
    DISPLAY File-list WITH FRAME Frame1.
END.   
ON CHOOSE OF b-exit
DO:      
    IF pp2-cnxt THEN DELETE PROCEDURE h-pp2.     
    IF pp3-cnxt THEN DELETE PROCEDURE h-pp3. 
    APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
END. 
/**********  MAIN LOGIC  **********/ 
DISPLAY pp2-cnxt pp3-cnxt WITH FRAME Frame1.    
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.






