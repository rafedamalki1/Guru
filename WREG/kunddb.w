 
 /*kunddb.w*/
 IF PROGRESS = "FULL" THEN DO:
   SESSION:DEBUG-ALERT = YES.
END.
RUN wc-start.w.
