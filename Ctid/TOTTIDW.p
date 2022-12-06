 /* TOTTIDw.P Beraknar den totala arbetstiden minus lunchrasten. */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE VARIABLE frustarten2 AS INTEGER NO-UNDO.
DEFINE VARIABLE fruslutet2 AS INTEGER NO-UNDO.
DEFINE VARIABLE frukost AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffestart2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffeslut2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kaffe AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchstarten2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchslutet2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchen AS INTEGER NO-UNDO.
DEFINE VARIABLE totaltiden AS INTEGER NO-UNDO.  
{SLUTARBW.I}
nytid = lunchslutet.
RUN TIMSEK.P.
lunchslutet2 = sekunder.
nytid = lunchstarten.
RUN TIMSEK.P.
lunchstarten2 = sekunder.
IF regslutsek < lunchslutet2 AND regslutsek > lunchstarten2 THEN DO:
   lunchen = regslutsek - lunchstarten2.
END.
ELSE IF regslutsek <= lunchstarten2 THEN lunchen = 0.
ELSE IF regstartsek < lunchslutet2 AND regstartsek > lunchstarten2
THEN lunchen = lunchslutet2 - regstartsek.
ELSE IF regstartsek >= lunchslutet2 THEN lunchen = 0.
ELSE IF regstartsek <= lunchstarten2 AND regslutsek >= lunchslutet2
THEN lunchen = lunchslutet2 - lunchstarten2.
 
nytid = frustarten.
RUN TIMSEK.P.
ASSIGN 
frustarten2 = sekunder
nytid = fruslutet.
RUN TIMSEK.P.
fruslutet2 = sekunder.
IF regslutsek < fruslutet2 AND regslutsek > frustarten2
THEN frukost = regslutsek - frustarten2.
ELSE IF regslutsek <= frustarten2 THEN frukost = 0.
ELSE IF regstartsek < fruslutet2 AND regstartsek > frustarten2
THEN frukost = fruslutet2 - regstartsek.
ELSE IF regstartsek >= fruslutet2 THEN frukost = 0.
ELSE IF regstartsek <= frustarten2 AND regslutsek >= fruslutet2
THEN frukost = fruslutet2 - frustarten2.

nytid = kaffestart.
RUN TIMSEK.P.         
ASSIGN 
kaffestart2 = sekunder
nytid = kaffeslut.
RUN TIMSEK.P.
kaffeslut2 = sekunder.
IF regslutsek < kaffeslut2 AND regslutsek > kaffestart2
THEN kaffe = regslutsek - kaffestart2.
ELSE IF regslutsek <= kaffestart2 THEN kaffe = 0.
ELSE IF regstartsek < kaffeslut2 AND regstartsek > kaffestart2
THEN kaffe = kaffestart2 - regstartsek.
ELSE IF regstartsek >= kaffeslut2 THEN kaffe = 0.
ELSE IF regstartsek <= kaffestart2 AND regslutsek >= kaffeslut2
THEN kaffe = kaffeslut2 - kaffestart2.
 
IF regslutsek < regstartsek THEN DO:
   totaltiden = 86400 - regstartsek + regslutsek.
END.
ELSE DO:
   totaltiden = regslutsek - regstartsek.
END.
ASSIGN
totaltiden = totaltiden - lunchen
totaltiden = totaltiden - frukost
totaltiden = totaltiden - kaffe
sekunder = totaltiden.
IF sekunder < 0 THEN sekunder = 0.

 /* OVANSTÅENDE SKA FINNAS MED VID HOPKOPPLING TILL PLUSD. OM PLUSD SKA
    VARA INKOPPLAT ELLER EJ KAN STYRAS FRÅN AVTALSMENYN "PLUSD ELLER EJ"*/
RUN SEKTIM.P.
