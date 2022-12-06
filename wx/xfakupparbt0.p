
CONNECT -db grannord -S 2501 -H granguru -N tcp -U CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) -P "KAGGEN".

CONNECT -db utbi -S 2908 -H granguru -N tcp -U CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) -P "KAGGEN".
 CREATE ALIAS rt9 FOR DATABASE grannord.
RUN xfakupparbt.p.
