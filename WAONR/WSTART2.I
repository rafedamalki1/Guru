   /*WSTART2.I*/
   IF SEL_UPP = Guru.Konstanter:gaol THEN DO:
      SEL_UPP:TOOLTIP = "Skapa och ?ndra " + LC(Guru.Konstanter:gaok) + ", samt listor".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Projektnumret anv?nds f?r att koppla samman alla moduler i systemet.
 Koppling till konto och f?rdelning p? olika konton kan g?ras. 
Dokument, avtal, bilder etc. som ber?r projektet kopplas eller kopieras in till projektet f?r
att alltid ge en sammanh?llen dokumentflora."
+ CHR(10) + "     " +
"Redovisning av tid sker mot projektnummer och koppling till ekonomi med automatisk kontof?rdelning blir m?jlig. 
Utv?xling av data kan normalt skapas p? projektnummer som tv?v?gskommunikation mellan GURU och ekonomisystemet.
Projektnummer serier kan skapas med utg?ngspunkt i lokala krav och underindelning 
av projekt kan g?ras.".
      END.
   END.
   ELSE IF SEL_UPP = "Beredning" THEN DO:
      SEL_UPP:TOOLTIP = "Beredning, projektbest?llning".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Standardkonstruktioner l?ggs in i systemet med materielspecifikationer som kopplas till 
valda leverant?rskataloger. Registrering av anl?ggningsidentifikation kan g?ras liksom registrering 
av adresser f?r leveranser till upplagsplatser."
+ CHR(10) + "     " + 
"F?r eldistributionsanl?ggningar finns funktioner f?r 
att sammankoppla olika anl?ggningsdelar, ber?kna kabelsk?psstorlek, 
skapa kabelsk?pskort och ber?kna schaktvolymer." 
+ CHR(10) + "     " +
"Rapporter i form av summerade materiellistor totalt,
per upplag, per anl?ggningsdel eller per konstruktion med eller utan priser kan tas ut. 
Ink?psprotokoll kan tas ut per leverant?r." 
+ CHR(10) + "     " + 
"Materielbest?llningsrutiner via E-post eller EDI har 
utarbetats tillsammans med leverant?rerna som direkt kan handha de elektroniskt ?verf?rda 
best?llningarna.". 
      END.
   END.
   ELSE IF SEL_UPP = "Materielhantering" THEN DO:
      SEL_UPP:TOOLTIP = "Dep?, plocklistor samt adminstration".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + "-F?rr?d" + CHR(10) + "     " +
"All lagerf?rd materiel l?ggs upp i f?rr?dssystemet. Kataloger fr?n leverant?rer registreras 
och kan l?sas in."  
+ CHR(10) + "     " +
"Best?llningar, leveranser, restnotering. Leveranser och returer ger sammantaget 
m?jligheter att alltid ha aktuella lagersaldon. M?jlighet att s?lja ?ver disk och skriva ut 
f?ljesedlar finns. Koppling finns i systemet s? att best?llning kan g?ras fr?n beredningen och att 
returer till f?rr?det reducerar projektets materielkostnader." 
+ CHR(10) + "     " +
"Materiel kan best?llas via E-post eller EDI.".
      END.
   END.
   ELSE IF SEL_UPP = "Kalkylering" THEN DO: 
      SEL_UPP:TOOLTIP = "Kalkylera. Fria eller fasta kalkyler".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Kalkyler kan utf?ras med hj?lp av branschkataloger fr?n EBR som utg?ngspunkt. Branschkatalogen finns i tre 
niv?er och konverteringstabeller finns f?r att transformera en kalkyl fr?n en niv? till en annan." 
+ CHR(10) + "     " +
"Katalogernas priser kan justeras f?r att passa f?retagets kostnadsniv? generellt. Varje kalkyl kan individuellt
 anpassas dels vad g?ller timkostnader, men ocks? justeras med faktorer f?r upp- resp. nedr?kning av tim?tg?ng f?r 
samtliga resurser."
+ CHR(10) + "     " +
"Materielkostnader kan h?mtas fr?n branschkatalogen och anpassas alternativt kan grossistkatalog anv?ndas direkt 
eller materiel fr?n genomf?rd beredning kopplas."
+ CHR(10) + "     " +
"Generella kompletteringar med nya arbetsmoment kan g?ras. Vidare kan kompletteringar med nya arbetsmoment i
varje enskild kalkyl."
+ CHR(10) + "     " +     
"Kalkyler kan ocks? utf?ras 'fritt' med antal ber?knade timmar totalt, per person eller personalkategori 
beroende p? hur priss?ttningen g?rs i f?retaget. Externa tj?nster kalkyleras p? motsvarande s?tt."
+ CHR(10) + "     " +
"Materielkostnader kan registreras som en summakostnad alternativt specificeras per artikel med hj?lp av 
leverant?rskataloger som l?sts in i systemet. Materielpriser netto resp. brutto kan ber?knas." 
+ CHR(10) + "     " + 
"Offerter fr?n leverant?rer kan registreras och offert till kund skapas."
+ CHR(10) + "     " + 
"Till ett projekt kan en eller flera kalkyler med alternativa l?sningar kopplas f?r att sedan en skall slutligt fastst?llas.". 
      END.
   END.
   ELSE IF SEL_UPP = "Tidredovisning" THEN DO:
      SEL_UPP:TOOLTIP = "Ny registrering och ?ndring av tid,beredskap,tj?nsteresor och l?netill?gg samt ?verl?snig till ekonomi och l?nesystem".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Registrering av tid sker med anst?llningsnummer och projektnummer. Registreringen kan ske med olika 
tekniska l?sningar, vid terminal, via talsvarssystem eller 
meddelandes?ndning -SMS- via GSM-telefoni." 
+ CHR(10) + "     " +
"Varje person har avtal och arbetstidsschema inlagt i systemet mot vilket registreringarna tolkas. 
Registrering sker vid arbetsdagens b?rjan och vid byte av projekt medan dagens slut registreras 
automatiskt via inlagt arbetstidsschema. I och med registrering i systemet finns allt underlag f?r 
l?neber?kning och kostnadsredovisning klart." 
+ CHR(10) + "     " +
"Tidrapporter kan tas ut vecko- eller m?nadsvis. Automatik finns i systemet som registrerar 
g?rdagens projektnummer om ingen registrering g?rs idag. 
Avtal kopplas till respektive person s? att det enbart kr?vs start- och slutregistrering f?r att
systemet skall kunna  ber?kna och registrera koder f?r olika ers?ttningar. 
All registrering sammanst?lls i den tidrapport som kan tas ut vecko- eller m?nadsvis." 
+ CHR(10) + "     " +
"Avvikelsekalender l?ggs in i systemet  som tar hand om alla speciella helgdagar, 'kl?mdagar' och de 
avtalstolkningar som h?r till dessa. 
Regelsystem f?r endags- och flerdygnf?rr?ttningar inrikes och utrikes l?ggs in f?r olika 
avtalsomr?den. Registrering sker med start- och sluttidpunkter och med svar p? fr?gor om 
m?ltidsreducering etc." 
+ CHR(10) + "     " +
"Alla kostnader kan kopplas till projekt f?r en total uppf?ljning och 
eventuell ?verf?ring till ekonomisystem. Traktamentskoder, restid, m?ltidsavdrag och f?rm?nsv?rden 
presenteras p? vecko- alternativt m?nadsrapport.".  
      END.
   END.
   ELSE IF SEL_UPP = "Flextid" THEN DO:
      SEL_UPP:TOOLTIP = "Registrering av flextid".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"F?r de delar av verksamheten som anv?nder flextidssystem finns allt integrerat i systemet."
+ CHR(10) + "     " +
"Registrering kan ske vid inpassering och mot ett f?r varje f?retag anpassat regelsystem. 
All arbetad tid priss?tts och kan ?verf?ras till ekonomi- respektive l?nesystem."
+ CHR(10) + "     " +
"'Normala' dataterminaler kan anv?ndas varf?r ingen speciell utrustning kr?vs f?r registrering.".  
      END.
   END.
   ELSE IF SEL_UPP = "Uppf?ljning" THEN DO:
      SEL_UPP:TOOLTIP = "Olika f?retagsanpassade uppf?ljnigslistor".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Ett stort antal standardlistor finns och kan ut?kas efter kundens behov. Systemet kan utrustas med 
rapportgenerator f?r framtagning av egna listor."
+ CHR(10) + "     " +
"All uppf?ljning ?r uppdaterad t.o.m. dagen f?re.".
      END.
   END.
   ELSE IF SEL_UPP = "Personaladministration" THEN DO:
      SEL_UPP:TOOLTIP = "Ny och ?ndring av personalregister samt uppl?ggning av priser och arbetstider".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
         "Registrering av personal och deras arbetstider och avtal.".
      END.
   END.
   ELSE IF SEL_UPP = "Sekretess" THEN DO:
      SEL_UPP:TOOLTIP = "Uppl?ggning av beh?righetsniv?er och anv?ndarregistrering".      
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Sekretessystemet ger stora m?jligheter att avgr?nsa ?tkomsten till olika funktioner. 
Att inte vara beh?rig inneb?r att man helt enkelt inte f?r upp funktionen i bild.". 
      END.
   END.
   ELSE IF SEL_UPP = "Register" THEN DO:
      SEL_UPP:TOOLTIP = "Uppl?ggning av bl.a. organisation, konton och faktureringsregler".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Registerdelen inneh?ller m?jligheter att anpassa namns?ttning f?r vissa funktioner till vad som 
anv?nds inom f?retaget. Kontotabeller m.m. l?ggs in s? att alltid korrekta data kan n?s och 
kontroller fungera vid registrering."
+ CHR(10) + "     " + 
"Rutiner f?r att l?sa in kataloger fr?n exempelvis 
materielleverant?rer via elektroniskt media finns.".
      END.
   END.
   ELSE IF SEL_UPP = "Faktureringsrutin" THEN DO:
      SEL_UPP:TOOLTIP = "Fakturering och registrering av l?pander?kning och fastpris fakturering".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Redovisningen i systemet inneb?r att underlag f?r fakturor kan skapas. 
Projekten l?ggs upp kopplade till kund med de villkor som avtalats." 
+ CHR(10) + "     " +
"Fast pris, takpris, l?pande r?kning, avtalsfakturering, fri fakturering anges och intervall eller 
datum f?r fakturering registreras. Faktura med ?verenskommet fast pris kan kompletteras med 
till?ggsarbeten som ej finns med i det ursprungliga avtalet. 
Det fasta priset kan uppdelas i delfaktureringar som anges med datum. F?r faktura d?r s? kr?vs kan 
all tid och alla till?gg av olika slag redovisas specificerad."
+ CHR(10) + "     " +
"Meddelande s?nds fr?n systemet till ansvariga f?r projektet n?r faktureringsdatum infaller.".
      END.
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gpll THEN DO:
      SEL_UPP:TOOLTIP = "Planerade arbeten - Budgetram".   
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Planer - budget"
+ CHR(10) + "     " +
"Planer f?r de kommande ?ren l?ggs in och byggs upp med plannummer f?r olika fasta verksamheter, 
investeringar, best?llningar etc. Varje plannummer kopplas till kontotabell och utf?rande?r.  
Plannumret f?r en kalkyl som omfattar egna insatser, externa tj?nster,materiel och ?vriga kostnader."
+ CHR(10) + "     " +
"Budgetramar f?r olika konton kan l?ggas in och planerna st?llas mot dessa. 
Omplanering mellan olika ?r ?r enkel att g?ra." 
+ CHR(10) + "     " +
"I och med att ett arbete skall startasupp tas ett projektnummer ut som kopplas till den 
ursprungliga planen.". 
      END.
   END.
   ELSE IF SEL_UPP = "Markv?rdering" THEN DO:
      SEL_UPP:TOOLTIP = "Skogs och ?kerv?rdering samt volymber?kning". 
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Ers?ttning till mark?gare f?r anl?ggning av distributionsledningar enligt f?renklad skogsnorm, 
intr?ng i ?ker samt kabel i ?ker kan ber?knas. Fastigheter och mark?gare registreras, 
st?mplingsl?ngd inf?res, l?ngder etc. registreras och utmynnar i ers?ttningar f?rdelade efter 
?garf?rh?llanden."
+ CHR(10) + "     " +
"Markv?rderingen kopplas till projektet och kostnaden redovisas i uppf?ljningen. 
Kontrakt med bilagor skrivs ut f?r resp. mark?gare.".   
      END.
   END.
   ELSE IF SEL_UPP = "Projekteringskalender" THEN DO:
      SEL_UPP:TOOLTIP = "Projekteringskalender".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = SEL_UPP + CHR(10).
      END.
   END.
   ELSE IF SEL_UPP = "SMS-administration" THEN DO:
      SEL_UPP:TOOLTIP = "Uppl?gg av GSM-telefoner, rapporter mm.".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
         "Uppl?gg av GSM-telefoner, rapporter mm.".
      END.
   END.
   ELSE IF SEL_UPP = "Avbrott/St?rning" THEN DO:
      SEL_UPP:TOOLTIP = "Registrering, ?ndring och borttag av avbrott/st?rning".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Rapportering av driftst?rningar och avbrott registreras och alla typer av rapporter 
till n?tmyndighet kan tas fram."
+ CHR(10) + "     " +
"Modulen utvecklad tillsammans med Svensk Energi AB helt enligt branschorganisationens krav och 
?nskem?l. ?verf?ring fr?n Guru till Svensk Energi f?r uppdatering av ?rlig branschstatistik 
fungerar helt problemfritt." 
+ CHR(10) + "     " +
"Alla de olika uppf?ljningslistor som tas fram i branschen kan tas fram 
f?r det egna f?retaget.".
      END.
   END.
   /*
   ELSE IF SEL_UPP = "Rapporter" THEN DO:
      SEL_UPP:TOOLTIP = "Rapporter av olika slag. SAFI, SAIDI m.fl.".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
      END.
   END.
   ELSE IF SEL_UPP = "Administration" THEN DO:
      SEL_UPP:TOOLTIP = "Uppl?gg av f?retag och distriktuppgifter".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
      END.
   END.
   ELSE IF SEL_UPP = "Import/Export" THEN DO:
      SEL_UPP:TOOLTIP = "Exportering och importering av uppgifter".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
      END.
   END.
   */
   IF Guru.Konstanter:varforetypval[17] > 0 THEN DISPLAY EDD_FUNK WITH FRAME {&FRAME-NAME}. 

