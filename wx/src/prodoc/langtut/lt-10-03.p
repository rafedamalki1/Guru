DEFINE VARIABLE Bal-due AS LOGICAL LABEL "Balance Due?" 
    VIEW-AS TOGGLE-BOX.
    
FOR EACH Customer WITH STREAM-IO THREE-D:
    IF Balance > 0 THEN Bal-due = YES. 
        ELSE Bal-due = NO.
        DISPLAY Name Bal-due.
END.



    
