/* NORDFTO.P Beraknar den totala arbetstiden minus lunchrasten. */
/* för frånvaro */
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.

DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE totaltiden AS INTEGER NO-UNDO.
DEFINE VARIABLE sluttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE starttiden AS INTEGER NO-UNDO.
DEFINE VARIABLE frustarten2 AS INTEGER NO-UNDO.
DEFINE VARIABLE fruslutet2 AS INTEGER NO-UNDO.
DEFINE VARIABLE frukost AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffestart2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffeslut2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffe AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchstarten2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchslutet2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchen AS INTEGER NO-UNDO.
DEFINE VARIABLE sluttiden2 AS INTEGER NO-UNDO.
RUN SLUTARB.P.
nytid = regstart.
RUN TIMSEK.P.
starttiden = sekunder.
nytid = regslut.
RUN TIMSEK.P.
sluttiden = sekunder.
nytid = lunchslutet.
RUN TIMSEK.P.
lunchslutet2 = sekunder.
nytid = lunchstarten.
RUN TIMSEK.P.
lunchstarten2 = sekunder.
IF sluttiden < lunchslutet2 AND sluttiden > lunchstarten2 THEN DO:
   lunchen = sluttiden - lunchstarten2.
END.
ELSE IF sluttiden <= lunchstarten2 THEN lunchen = 0.
ELSE IF starttiden < lunchslutet2 AND starttiden > lunchstarten2
THEN lunchen = lunchslutet2 - starttiden.
ELSE IF starttiden >= lunchslutet2 THEN lunchen = 0.
ELSE IF starttiden <= lunchstarten2 AND sluttiden >= lunchslutet2
THEN lunchen = lunchslutet2 - lunchstarten2.

nytid = frustarten.
RUN TIMSEK.P.
frustarten2 = sekunder.
nytid = fruslutet.
RUN TIMSEK.P.
fruslutet2 = sekunder.
IF sluttiden < fruslutet2 AND sluttiden > frustarten2
THEN frukost = sluttiden - frustarten2.
ELSE IF sluttiden <= frustarten2 THEN frukost = 0.
ELSE IF starttiden < fruslutet2 AND starttiden > frustarten2
THEN frukost = fruslutet2 - starttiden.
ELSE IF starttiden >= fruslutet2 THEN frukost = 0.
ELSE IF starttiden <= frustarten2 AND sluttiden >= fruslutet2
THEN frukost = fruslutet2 - frustarten2.

nytid = kaffestart.
RUN TIMSEK.P.
kaffestart2 = sekunder.
nytid = kaffeslut.
RUN TIMSEK.P.
kaffeslut2 = sekunder.
IF sluttiden < kaffeslut2 AND sluttiden > kaffestart2
THEN kaffe = sluttiden - kaffestart2.
ELSE IF sluttiden <= kaffestart2 THEN kaffe = 0.
ELSE IF starttiden < kaffeslut2 AND starttiden > kaffestart2
THEN kaffe = kaffestart2 - starttiden.
ELSE IF starttiden >= kaffeslut2 THEN kaffe = 0.
ELSE IF starttiden <= kaffestart2 AND sluttiden >= kaffeslut2
THEN kaffe = kaffeslut2 - kaffestart2.

IF sluttiden < starttiden THEN DO:
   totaltiden = 86400 - starttiden + sluttiden.
END.
ELSE DO:
   totaltiden = sluttiden - starttiden.
END.

totaltiden = totaltiden - lunchen.

totaltiden = totaltiden - frukost.

totaltiden = totaltiden - kaffe.

sekunder = totaltiden.

RUN SEKTIM.P.
