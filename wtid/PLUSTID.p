 /* PLUSTID.P Beraknar SLUTTID NAR ARBETADE TIMMAR AR ANGIVET. */
&Scoped-define NEW
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
totaltiden = sekunder.
RUN SLUTARB.P.
IF frustarten NE fruslutet THEN DO:
   nytid = frustarten.
   RUN TIMSEK.P.
   ASSIGN 
   frustarten2 = sekunder
   nytid = fruslutet.
   RUN TIMSEK.P.
   fruslutet2 = sekunder.
   IF ( regstartsek + totaltiden ) < fruslutet2 AND ( regstartsek + totaltiden ) > frustarten2
   THEN frukost = fruslutet2 - frustarten2.
   ELSE IF ( regstartsek + totaltiden ) <= frustarten2 THEN frukost = 0.
   ELSE IF regstartsek < fruslutet2 AND regstartsek > frustarten2
   THEN frukost = fruslutet2 - regstartsek.
   ELSE IF regstartsek >= fruslutet2 THEN frukost = 0.
   ELSE IF regstartsek <= frustarten2 AND ( regstartsek + totaltiden )  >= fruslutet2
   THEN frukost = fruslutet2 - frustarten2.
  
   totaltiden = totaltiden + frukost.   
END.       
IF lunchstarten NE lunchslutet THEN DO:
   nytid = lunchstarten.
   RUN TIMSEK.P.
   ASSIGN 
   lunchstarten2 = sekunder
   nytid = lunchslutet.
   RUN TIMSEK.P.
   lunchslutet2 = sekunder.  
   IF ( regstartsek + totaltiden) < lunchslutet2 AND ( regstartsek + totaltiden) > lunchstarten2 
   THEN DO:
      lunchen = lunchslutet2 - lunchstarten2.
   END.
   ELSE IF ( regstartsek + totaltiden) <= lunchstarten2 THEN lunchen = 0.
   ELSE IF regstartsek < lunchslutet2 AND regstartsek > lunchstarten2
   THEN lunchen = lunchslutet2 - regstartsek.
   ELSE IF regstartsek >= lunchslutet2 THEN lunchen = 0.
   ELSE IF regstartsek <= lunchstarten2 AND ( regstartsek + totaltiden) >= lunchslutet2
   THEN lunchen = lunchslutet2 - lunchstarten2.   
   totaltiden = totaltiden + lunchen.   
END.       
IF kaffestart NE kaffeslut THEN DO:
   nytid = kaffestart.
   RUN TIMSEK.P.
   ASSIGN 
   kaffestart2 = sekunder
   nytid = kaffeslut.
   RUN TIMSEK.P.
   kaffeslut2 = sekunder.  
   IF ( regstartsek + totaltiden) < kaffeslut2 AND ( regstartsek + totaltiden) > kaffestart2
   THEN kaffe = kaffeslut2 - kaffestart2.
   ELSE IF ( regstartsek + totaltiden) <= kaffestart2 THEN kaffe = 0.
   ELSE IF regstartsek < kaffeslut2 AND regstartsek > kaffestart2
   THEN kaffe = kaffestart2 - regstartsek.
   ELSE IF regstartsek >= kaffeslut2 THEN kaffe = 0.
   ELSE IF regstartsek <= kaffestart2 AND ( regstartsek + totaltiden) >= kaffeslut2
   THEN kaffe = kaffeslut2 - kaffestart2.
   totaltiden = totaltiden + kaffe.
END.       
regslutsek = regstartsek + totaltiden.
sekunder = regslutsek.
IF sekunder < 0 THEN sekunder = 0.
RUN SEKTIM.P.

    
