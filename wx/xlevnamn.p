FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = "11" NO-LOCK NO-ERROR.
FOR EACH INKADRESS WHERE INKADRESS.LEVKODINK = "11" EXCLUSIVE-LOCK:
    /*DISP INKADRESS.LEVKODINK INKADRESS.LEVNAMN FORMAT "X(30)" LEVERANTOR.LEVNAMN FORMAT "X(30)"
    INKADRESS.BERNR INKADRESS.OMRADE.*/
    ASSIGN INKADRESS.LEVNAMN = LEVERANTOR.LEVNAMN.
END.
FOR EACH INKMTRL WHERE INKMTRL.LEVKODINK = "11" EXCLUSIVE-LOCK:
    /*DISP INKMTRL.LEVKODINK INKMTRL.LEVNAMN FORMAT "X(30)" LEVERANTOR.LEVNAMN FORMAT "X(30)".*/
    ASSIGN INKMTRL.LEVNAMN = LEVERANTOR.LEVNAMN.

END.