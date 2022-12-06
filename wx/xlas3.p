DEFINE VARIABLE vem AS CHARACTER NO-UNDO.
DEFINE VARIABLE xl1 AS HANDLE NO-UNDO.
DEFINE VARIABLE xl2 AS HANDLE NO-UNDO.
vem = "eb". 

run C:\DELAD\pro9\guru\wx\xlas1.p persistent set xl1 . 
run C:\DELAD\pro9\guru\wx\xlas1.p persistent set xl2 . 

run l1_UI in xl1 .
run l3_UI in xl2 .
run l4_UI in xl1 .
run l5_UI in xl2 .

