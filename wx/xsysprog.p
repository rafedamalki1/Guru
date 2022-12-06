    /*xsysprog.p*/
    
    FIND FIRST _User WHERE _User._Userid = "sysprogress" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE _User THEN DO TRANSACTION:
       MESSAGE "User sysprogress finns ej"   VIEW-AS ALERT-BOX.
       CREATE _User.
        _User._Userid = "sysprogress".
        _User._Password = "ilabebPjpeCektqC".
        FIND FIRST _User WHERE _User._Userid = "sysprogress" NO-LOCK NO-ERROR.
        IF AVAILABLE _User THEN DO:
           MESSAGE "User sysprogress skapad"  VIEW-AS ALERT-BOX.
        END.
        ELSE DO:
           MESSAGE "User sysprogress kunde ej skapas"  VIEW-AS ALERT-BOX.
        END.
    END.
    ELSE DO:
       MESSAGE "Användare sysprogress finns"   VIEW-AS ALERT-BOX.
    END.
    RELEASE _User NO-ERROR.

 
