   /*WSTART2.I*/
   IF SEL_UPP = Guru.Konstanter:gaol THEN DO:
      SEL_UPP:TOOLTIP = "Skapa och ändra " + LC(Guru.Konstanter:gaok) + ", samt listor".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Projektnumret används för att koppla samman alla moduler i systemet.
 Koppling till konto och fördelning på olika konton kan göras. 
Dokument, avtal, bilder etc. som berör projektet kopplas eller kopieras in till projektet för
att alltid ge en sammanhållen dokumentflora."
+ CHR(10) + "     " +
"Redovisning av tid sker mot projektnummer och koppling till ekonomi med automatisk kontofördelning blir möjlig. 
Utväxling av data kan normalt skapas på projektnummer som tvåvägskommunikation mellan GURU och ekonomisystemet.
Projektnummer serier kan skapas med utgångspunkt i lokala krav och underindelning 
av projekt kan göras.".
      END.
   END.
   ELSE IF SEL_UPP = "Beredning" THEN DO:
      SEL_UPP:TOOLTIP = "Beredning, projektbeställning".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Standardkonstruktioner läggs in i systemet med materielspecifikationer som kopplas till 
valda leverantörskataloger. Registrering av anläggningsidentifikation kan göras liksom registrering 
av adresser för leveranser till upplagsplatser."
+ CHR(10) + "     " + 
"För eldistributionsanläggningar finns funktioner för 
att sammankoppla olika anläggningsdelar, beräkna kabelskåpsstorlek, 
skapa kabelskåpskort och beräkna schaktvolymer." 
+ CHR(10) + "     " +
"Rapporter i form av summerade materiellistor totalt,
per upplag, per anläggningsdel eller per konstruktion med eller utan priser kan tas ut. 
Inköpsprotokoll kan tas ut per leverantör." 
+ CHR(10) + "     " + 
"Materielbeställningsrutiner via E-post eller EDI har 
utarbetats tillsammans med leverantörerna som direkt kan handha de elektroniskt överförda 
beställningarna.". 
      END.
   END.
   ELSE IF SEL_UPP = "Materielhantering" THEN DO:
      SEL_UPP:TOOLTIP = "Depå, plocklistor samt adminstration".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + "-Förråd" + CHR(10) + "     " +
"All lagerförd materiel läggs upp i förrådssystemet. Kataloger från leverantörer registreras 
och kan läsas in."  
+ CHR(10) + "     " +
"Beställningar, leveranser, restnotering. Leveranser och returer ger sammantaget 
möjligheter att alltid ha aktuella lagersaldon. Möjlighet att sälja över disk och skriva ut 
följesedlar finns. Koppling finns i systemet så att beställning kan göras från beredningen och att 
returer till förrådet reducerar projektets materielkostnader." 
+ CHR(10) + "     " +
"Materiel kan beställas via E-post eller EDI.".
      END.
   END.
   ELSE IF SEL_UPP = "Kalkylering" THEN DO: 
      SEL_UPP:TOOLTIP = "Kalkylera. Fria eller fasta kalkyler".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Kalkyler kan utföras med hjälp av branschkataloger från EBR som utgångspunkt. Branschkatalogen finns i tre 
nivåer och konverteringstabeller finns för att transformera en kalkyl från en nivå till en annan." 
+ CHR(10) + "     " +
"Katalogernas priser kan justeras för att passa företagets kostnadsnivå generellt. Varje kalkyl kan individuellt
 anpassas dels vad gäller timkostnader, men också justeras med faktorer för upp- resp. nedräkning av timåtgång för 
samtliga resurser."
+ CHR(10) + "     " +
"Materielkostnader kan hämtas från branschkatalogen och anpassas alternativt kan grossistkatalog användas direkt 
eller materiel från genomförd beredning kopplas."
+ CHR(10) + "     " +
"Generella kompletteringar med nya arbetsmoment kan göras. Vidare kan kompletteringar med nya arbetsmoment i
varje enskild kalkyl."
+ CHR(10) + "     " +     
"Kalkyler kan också utföras 'fritt' med antal beräknade timmar totalt, per person eller personalkategori 
beroende på hur prissättningen görs i företaget. Externa tjänster kalkyleras på motsvarande sätt."
+ CHR(10) + "     " +
"Materielkostnader kan registreras som en summakostnad alternativt specificeras per artikel med hjälp av 
leverantörskataloger som lästs in i systemet. Materielpriser netto resp. brutto kan beräknas." 
+ CHR(10) + "     " + 
"Offerter från leverantörer kan registreras och offert till kund skapas."
+ CHR(10) + "     " + 
"Till ett projekt kan en eller flera kalkyler med alternativa lösningar kopplas för att sedan en skall slutligt fastställas.". 
      END.
   END.
   ELSE IF SEL_UPP = "Tidredovisning" THEN DO:
      SEL_UPP:TOOLTIP = "Ny registrering och ändring av tid,beredskap,tjänsteresor och lönetillägg samt överläsnig till ekonomi och lönesystem".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Registrering av tid sker med anställningsnummer och projektnummer. Registreringen kan ske med olika 
tekniska lösningar, vid terminal, via talsvarssystem eller 
meddelandesändning -SMS- via GSM-telefoni." 
+ CHR(10) + "     " +
"Varje person har avtal och arbetstidsschema inlagt i systemet mot vilket registreringarna tolkas. 
Registrering sker vid arbetsdagens början och vid byte av projekt medan dagens slut registreras 
automatiskt via inlagt arbetstidsschema. I och med registrering i systemet finns allt underlag för 
löneberäkning och kostnadsredovisning klart." 
+ CHR(10) + "     " +
"Tidrapporter kan tas ut vecko- eller månadsvis. Automatik finns i systemet som registrerar 
gårdagens projektnummer om ingen registrering görs idag. 
Avtal kopplas till respektive person så att det enbart krävs start- och slutregistrering för att
systemet skall kunna  beräkna och registrera koder för olika ersättningar. 
All registrering sammanställs i den tidrapport som kan tas ut vecko- eller månadsvis." 
+ CHR(10) + "     " +
"Avvikelsekalender läggs in i systemet  som tar hand om alla speciella helgdagar, 'klämdagar' och de 
avtalstolkningar som hör till dessa. 
Regelsystem för endags- och flerdygnförrättningar inrikes och utrikes läggs in för olika 
avtalsområden. Registrering sker med start- och sluttidpunkter och med svar på frågor om 
måltidsreducering etc." 
+ CHR(10) + "     " +
"Alla kostnader kan kopplas till projekt för en total uppföljning och 
eventuell överföring till ekonomisystem. Traktamentskoder, restid, måltidsavdrag och förmånsvärden 
presenteras på vecko- alternativt månadsrapport.".  
      END.
   END.
   ELSE IF SEL_UPP = "Flextid" THEN DO:
      SEL_UPP:TOOLTIP = "Registrering av flextid".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"För de delar av verksamheten som använder flextidssystem finns allt integrerat i systemet."
+ CHR(10) + "     " +
"Registrering kan ske vid inpassering och mot ett för varje företag anpassat regelsystem. 
All arbetad tid prissätts och kan överföras till ekonomi- respektive lönesystem."
+ CHR(10) + "     " +
"'Normala' dataterminaler kan användas varför ingen speciell utrustning krävs för registrering.".  
      END.
   END.
   ELSE IF SEL_UPP = "Uppföljning" THEN DO:
      SEL_UPP:TOOLTIP = "Olika företagsanpassade uppföljnigslistor".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Ett stort antal standardlistor finns och kan utökas efter kundens behov. Systemet kan utrustas med 
rapportgenerator för framtagning av egna listor."
+ CHR(10) + "     " +
"All uppföljning är uppdaterad t.o.m. dagen före.".
      END.
   END.
   ELSE IF SEL_UPP = "Personaladministration" THEN DO:
      SEL_UPP:TOOLTIP = "Ny och ändring av personalregister samt uppläggning av priser och arbetstider".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
         "Registrering av personal och deras arbetstider och avtal.".
      END.
   END.
   ELSE IF SEL_UPP = "Sekretess" THEN DO:
      SEL_UPP:TOOLTIP = "Uppläggning av behörighetsnivåer och användarregistrering".      
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Sekretessystemet ger stora möjligheter att avgränsa åtkomsten till olika funktioner. 
Att inte vara behörig innebär att man helt enkelt inte får upp funktionen i bild.". 
      END.
   END.
   ELSE IF SEL_UPP = "Register" THEN DO:
      SEL_UPP:TOOLTIP = "Uppläggning av bl.a. organisation, konton och faktureringsregler".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Registerdelen innehåller möjligheter att anpassa namnsättning för vissa funktioner till vad som 
används inom företaget. Kontotabeller m.m. läggs in så att alltid korrekta data kan nås och 
kontroller fungera vid registrering."
+ CHR(10) + "     " + 
"Rutiner för att läsa in kataloger från exempelvis 
materielleverantörer via elektroniskt media finns.".
      END.
   END.
   ELSE IF SEL_UPP = "Faktureringsrutin" THEN DO:
      SEL_UPP:TOOLTIP = "Fakturering och registrering av löpanderäkning och fastpris fakturering".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Redovisningen i systemet innebär att underlag för fakturor kan skapas. 
Projekten läggs upp kopplade till kund med de villkor som avtalats." 
+ CHR(10) + "     " +
"Fast pris, takpris, löpande räkning, avtalsfakturering, fri fakturering anges och intervall eller 
datum för fakturering registreras. Faktura med överenskommet fast pris kan kompletteras med 
tilläggsarbeten som ej finns med i det ursprungliga avtalet. 
Det fasta priset kan uppdelas i delfaktureringar som anges med datum. För faktura där så krävs kan 
all tid och alla tillägg av olika slag redovisas specificerad."
+ CHR(10) + "     " +
"Meddelande sänds från systemet till ansvariga för projektet när faktureringsdatum infaller.".
      END.
   END.
   ELSE IF SEL_UPP = Guru.Konstanter:gpll THEN DO:
      SEL_UPP:TOOLTIP = "Planerade arbeten - Budgetram".   
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Planer - budget"
+ CHR(10) + "     " +
"Planer för de kommande åren läggs in och byggs upp med plannummer för olika fasta verksamheter, 
investeringar, beställningar etc. Varje plannummer kopplas till kontotabell och utförandeår.  
Plannumret får en kalkyl som omfattar egna insatser, externa tjänster,materiel och övriga kostnader."
+ CHR(10) + "     " +
"Budgetramar för olika konton kan läggas in och planerna ställas mot dessa. 
Omplanering mellan olika år är enkel att göra." 
+ CHR(10) + "     " +
"I och med att ett arbete skall startasupp tas ett projektnummer ut som kopplas till den 
ursprungliga planen.". 
      END.
   END.
   ELSE IF SEL_UPP = "Markvärdering" THEN DO:
      SEL_UPP:TOOLTIP = "Skogs och åkervärdering samt volymberäkning". 
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Ersättning till markägare för anläggning av distributionsledningar enligt förenklad skogsnorm, 
intrång i åker samt kabel i åker kan beräknas. Fastigheter och markägare registreras, 
stämplingslängd införes, längder etc. registreras och utmynnar i ersättningar fördelade efter 
ägarförhållanden."
+ CHR(10) + "     " +
"Markvärderingen kopplas till projektet och kostnaden redovisas i uppföljningen. 
Kontrakt med bilagor skrivs ut för resp. markägare.".   
      END.
   END.
   ELSE IF SEL_UPP = "Projekteringskalender" THEN DO:
      SEL_UPP:TOOLTIP = "Projekteringskalender".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = SEL_UPP + CHR(10).
      END.
   END.
   ELSE IF SEL_UPP = "SMS-administration" THEN DO:
      SEL_UPP:TOOLTIP = "Upplägg av GSM-telefoner, rapporter mm.".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
         "Upplägg av GSM-telefoner, rapporter mm.".
      END.
   END.
   ELSE IF SEL_UPP = "Avbrott/Störning" THEN DO:
      SEL_UPP:TOOLTIP = "Registrering, ändring och borttag av avbrott/störning".
      IF Guru.Konstanter:varforetypval[17] > 0 THEN DO:
         EDD_FUNK = CHR(9) + SEL_UPP + CHR(10) + "     " +
"Rapportering av driftstörningar och avbrott registreras och alla typer av rapporter 
till nätmyndighet kan tas fram."
+ CHR(10) + "     " +
"Modulen utvecklad tillsammans med Svensk Energi AB helt enligt branschorganisationens krav och 
önskemål. Överföring från Guru till Svensk Energi för uppdatering av årlig branschstatistik 
fungerar helt problemfritt." 
+ CHR(10) + "     " +
"Alla de olika uppföljningslistor som tas fram i branschen kan tas fram 
för det egna företaget.".
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
      SEL_UPP:TOOLTIP = "Upplägg av företag och distriktuppgifter".
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

