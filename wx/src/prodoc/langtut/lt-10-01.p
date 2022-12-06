DEFINE VARIABLE Bal-due AS LOGICAL LABEL "Balance Due?" 
    VIEW-AS TOGGLE-BOX.
    
DEFINE FRAME Frame1
    sports.Customer.Name 
    Bal-due VIEW-AS TEXT
        WITH DOWN USE-TEXT CENTERED THREE-D.
              
FOR EACH Customer FIELDS (Name Balance) WITH FRAME Frame1:
    IF Balance > 0 THEN Bal-due = YES. 
        ELSE Bal-due = NO.
    DISPLAY Name Bal-due.
END.



        
