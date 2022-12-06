/*xP2.p.*/


CREATE SERVER Guru.Konstanter:apphand.
appcon = Guru.Konstanter:apphand:CONNECT("-S appeseta -H elpaso.sydkraft.se -N TCP" ,CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),"KAGGEN","").
RUN XP22.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT .
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand.
appcon = FALSE.
