DEFINE VARIABLE skick  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE efel AS CHARACTER.      /* Status txt  */
RUN SMTPPOST.P (
INPUT "m01.85.nuinternet.com", 
INPUT "elpool.ume@elpool.se",
INPUT "Nya från Guru", 
INPUT "TEST", 
INPUT "",
INPUT "",
INPUT "xxxxx",
INPUT "elpa", 
OUTPUT skick, 
OUTPUT efel).
DISPLAY efel FORMAT "x(68)".
