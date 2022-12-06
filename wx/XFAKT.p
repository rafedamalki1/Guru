UNDER REGISTER.
   TEXTER TILL ÖVERTID.
   - Vid övertid skall typ av övertid skrivas ut, t ex montör 50 %, montör 100 %. 
   (PUNKT UNDER LÖPANDE FAKTURERING)

   PRISLISTOR
   - Det skall finnas mallprislistor som kan kopplas till kunderna. 
     Prislistorna skall vara märkta med giltighetsdatum - 
     för att nya prislistor skall kunna läggas upp parallellt med de gamla och 
     automatiskt börja gälla när fakturering kommer till ett visst datum.
     (PUNKT UNDER LÖPANDE FAKTURERING)
     
   KONTON FÖR FAKTURERING
   - Varje faktura innehåller en rad med kundfordranskontering, 
     en rad med momskontering (ej vid momsfritt) samt en eller flera rader med 
     intäktskontering.
     Kundfordranskonteringen innehåller konto, motpart och AO
     Momskonteringen innehåller enbart konto
     Intäktskonteringarna innehåller konto, motpart, ansvar och AO
   - Dessutom skapas för varje faktura statistikuppgifter i form av fakturerade 
     timmar kollektiv personal resp fakturerade timmar tjänstemän. Dessa uppgifter 
     åsätts konto och AO. 
     Lönearterna styr vilka timmar som skall med.
   - Varje kund är antingen extern eller intern (koncernintern). Kontona i 
     kundfordranskonteringen resp intäktskonteringen styrs av detta.
   - Motpartskonteringen hämtas från kunduppgiften (kommer in med kundregistret från 
     annat system)
   - Momskonteringen styrs av en momskod som default finns på kunden (kommer in med 
     kundregistret från annat system). 
     Momskoden kan ändras vid faktureringstillfället. 
   - Det finns ett antal olika faktureringstyper som förutom nedanstående varianter 
     även styr intäktskontot. Följande alternativ kan förekomma:
       1) Förskottsfakturering utan moms. Dvs alla belopp beräknas utan moms om det 
          inte är en slutfaktura. 
       2) Förskottsfakturering med moms. Dvs alla belopp beräknas med den momskod som 
          gäller för fakturan.
       3) Slutfaktura för omomsade förskott.
       4) Slutfaktura för momsade förskott.
       5) Differentierad moms. Dvs varje post i fakturan har egen momskod.
     - Konteringen av fakturan läggs default ut enligt de konteringsregler som gäller 
       för kunden eller AO-numret. Det skall finnas möjlighet att göra manuella 
       justeringar av default-konteringen direkt i registreringsbilden.      
      (PUNKT UNDER KONTERING)
      
   NUMMERSERIE FÖR FAKTUROR
   - Fakturanumret byggs upp med bolagsbegrepp, årtal, arkivställe + löpnummer. 
     Arkivstället styrs av AO-numrets ansvar.
     (PUNKT UNDER GENERELLT)
       
   BESTÄLLARE/KUND 
   - Lägga in rabatt - möjlighet (minus) i pålägget för material och främmande 
     tjänster på  leverantörsfakturor. 
     Lösning : 	I "Faktura regler" lägger man in procentsatser för materiel och 
     främmande tjänster dessa skall även kunna vara negativa.
     (PUNKT J I TILLÄGGSOFFTER)
     
  ORGANISTION    
  - Grunduppgifter som skall skrivas ut på fakturan styrs per bolag. Grunduppgifter 
    kan vara bolagets namn, adress, bankgiro, postgiro, organisationsnummer, 
    VAT-nummer, uppgifter om F-skattesedel, dröjsmålsräntesats).
    (PUNKT UNDER GENERELLT) 
     
UNDER SEKRETESS 
  ANVÄNDARBEHÖRIGHET
  - Behörigheten för resp användare skall kunna styras på projektledarkod resp 
    ansvarskod eller kombinationer av dessa. 
    En viss inloggning skall t ex kunna fakturera för flera olika projektledare eller 
    ansvar.
    (PUNKT UNDER BEHÖRIGHETSSYTEM)
  
  - Vid fastprisfakturering skall projektledaren för huvud-AO (AO med del-nr 00) kunna 
    se upplysningsinformation även för övriga del-nr inom samma huvud-AO.
    (PUNKT UNDER BEHÖRIGHETSSYTEM)
    
  - Behörighetsnivå för att kunna göra preliminär faktura kontra skarp faktura.
    (PUNKT E I TILLÄGGSOFFTER)

UNDER FAKTURERING
  FÖRSTA BILDEN
  - Kunna välja i förstabilden att enbart se preliminärfaktura.
    (PUNKT B I TILLÄGGSOFFTER)
    
  - Möjlighet att "ta bort preliminärfaktura" läggs in. Då återställs allt till 
    ursprunget eller till senast fakturering. 
    Samtidigt ändras det så att man inte kan ta bort en skarp faktura.
    (PUNKT C I TILLÄGGSOFFTER)
    
  - Kunna se bara den preliminär faktura som man har skapat.     
    Lösning : Vi lägger till ett fält på fakturan som säger vem som senast ändrade 
    den preliminära fakturan och lägger till en möjlighet att visa vad jag ändrat på.
    (PUNKT H I TILLÄGGSOFFTER)
    
  - I första bilden i fakturarutinen skriva ut Avtalskoden(Projekt ovan) vid 
    kategori UA/UF. 
    Lösning :	 Lista alla fakturor med aonr som har Projektkoder A01,B01 osv. 
    Idag sparas inte Projektkoden vid inläsning av aonr från Ao- systemet. 
    Ett nytt fällt för Projektkoden måste skapas för fakturan.
    (PUNKT M I TILLÄGGSOFFTER)

  - Kunna välja kategori i "första bilden" i fakturarutinen. 
    Lösning :	Möjlighet att kunna göra urval på en vald kategori och bara se den valda
    kategorien eller samtliga. 
    (PUNKT N I TILLÄGGSOFFTER)

UNDER ÄNDRA FAKTURAPLAN
  GRUND UPPGIFTER 
  - Lägga in rabatt - möjlighet (minus) i pålägget för material och främmande tjänster 
    på  leverantörsfakturor. 
    Lösning : I "Faktura regler" lägger man in procentsatser för materiel och 
    främmande tjänster dessa skall även kunna vara negativa.
    (PUNKT J I TILLÄGGSOFFTER)
    
  FASTPRIS
  - I betalningsplanen skall man kunna sätta antigen procentsatser eller fasta priser 
    för varje delbetalning.
    (PUNKT UNDER FASTPRIS)
  - Det skall finnas möjlighet att titta på nedlagda timmar resp registrerade 
    lev.fakturor som upplysning.
    (PUNKT UNDER FASTPRIS)
    
  AVTAL
  - Det fakturerade beloppet skall kunna fördelas på de olika AO som ligger 
    samfaktureirngskopplade - antingen som %-sats eller som fast belopp.
    (PUNKT UNDER AVTAL)
  AVIKANDE FAKTURA 
  - Avvikande kundnummer mot ao-numrets kund för att skapa möjlighet att ha två eller 
    flera kunder mot samma ao-nummer. Alla kunder utom den ursprungliga faktureras 
    enbart genom fri komplettering. 
    Lösning : Det skall gå att skapa fakturor där man väljer kund och aonr fritt. 
    En ny faktureringsmöjlighet införs i Guru där detta blir möjligt.
    (PUNKT Q I TILLÄGGSOFFTER)

UNDER FAKTURERA 
  ORDERHUVUD
  - Lägga in en bild med faktureringsinfo, "Orderhuvud", vilken ska visas efter man 
    gått in på fakturera. Bilden ska innehålla; kundadress, beställare, kundreferens, 
    vår referens, arbetets omfattning, preliminär faktura nr, bolagskod (enhet), 
    val för fakturatyp (a´conto, slutfaktura, kreditfaktura etc.), momskod, 
    val av "tom faktureringsdatum" och "senast godkända och veckokörda tidsedel 
    avsåg vecka xx". Det ska också visas info om tidigare gjorda faktureringar i 
    denna bild (summa och typ; förskott utan moms, förskott med moms och försäljning).
    (PUNKT D I TILLÄGGSOFFTER)
 
  - Fakturatexten i huvudet skall default lägga in den text som ligger på AO (kommer  
    från AO-systemet). Texten skall vara möjlig att ändra manuellt.
    (PUNKT UNDER UTSKRIFTER)
  
  - Den arbetsomfattning som finns i AO- systemet och återfinns på ao- numrets 
    anmärkning i GURU skall även finnas på den fakturatext man kan skriva in i GURU's 
    faktureringsrutin.   
    (PUNKT G I TILLÄGGSOFFTER)
    
  ALLMÄNT
  - Vid löpande fakturering. Kunna växla mellan sammanställnings -bild o 
    detaljtransaktioner. Både för tidtransaktioner och mtrl. 
    Vid inhopp i rutinen skall sammanställnings -bilderna visas
    för nya transaktioner resp. för hittills fakturerat. 
    (PUNKT A I TILLÄGGSOFFTER)

  - Summering av arbetskostnaden i faktureringsbilden vänds på andra hållet så att de 
    olika faktureringsrubrikerna kommer till vänster och för varje rubrik visas antal,
    pris och summa. Summering av material och övrig kostnaden vänds också på andra 
    hållet och summeras för hela fakturan grupperat på material, främmande tjänst, 
    övrigt.
    (PUNKT F I TILLÄGGSOFFTER)
       
  TIDPOSTER/LÖPANDE RÄKNING
  - Det skall gå att justera start och sluttid alternativt antal upparbetade timmar 
    för varje persons tidskrivning per aonr och dag samt justera befattning och pris 
    per registrering. 
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Man kan ta ställning till om registreringen skall tas med eller ej (ta med direkt,
    inte ta med alls alternativt välja bort vid detta faktureringstillfälle - men få 
    upp posten igen vid nästa fakturering). 
    (PUNKT UNDER LÖPANDE FAKTURERING)

  - Man kan välja att bara ta med registreringar från och med ett datum. För t ex 
    viss person som t ex inte lämnat in sin tidsedel till förra faktureringen - men 
    tiden ändå tagits med manuellt i faktureringen.
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Man skall se sista tidskrivningsdatum för varje person och aonr.
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Vid övertid skall typ av övertid skrivas ut, t ex montör 50 %, montör 100 %.
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Koppla på en fakturabefattning redan vid  tidskrivningen.
 
    Lösning : Det ni menar med befattning här skall översättas med ett nytt begrepp 
    "faktureringsbefattningar".  

    1. Dessa faktureringsbefattningar måste läggas upp i Guru och prissättas i varje 
       prislista i Guru med priser för normaltid, restid och övertid. 
    2. Det måste också vara möjligt att byta ut personens 
       befattning mot dessa faktureringsbefattning i fakturarutinen samt att kunna 
       dela upp en post i två poster, för att kunna ha olika 
       faktureringsbefattningar för samma tidskrivning.   
    3. Det skall kunna vara möjligt att sätta 
       faktureringsbefattning direkt vid tidskrivningen. 
       Detta är en mycket stor förändring. Alla tidskrivningsrutiner måste ändras 
       samt att en databasförändring gällande tidskrivningen måste göras. Detta kräver
       helg arbete alternativt ett större driftstopp under veckan.  
       (PUNKT 0 I TILLÄGGSOFFTER)
 
  FRI KOMPLETERING
  - Det skall finnas vallistor för olika typer av övriga kostnader i 
    tilläggsfaktureringen.  
    (PUNKT UNDER LÖPANDE FAKTURERING)
      
  - Det skall gå att skapa materiellistor för att få ut materielspecifikationer till 
    kund med kundpris.
    (PUNKT UNDER LÖPANDE FAKTURERING)

  EXTERNA FAKTUROR
  - Det skall gå att byta verifikattext på leverantörsfakturorna från ekonomisystemet
   (idag står det leverantörens namn) vid fakturering och ev. även beloppet om det 
   inte räcker med procentpåslagen ovan. 
   (PUNKT UNDER LÖPANDE FAKTURERING)
   
  ACONTON
  - Det skall gå att göra a-contofaktureringar genom att nedlagda timmar och använt 
    material enbart tjänar som upplysning för hur stort belopp fakturan skall ställas 
    ut på. A-contobeloppet skall synas i faktureringsbilden som upplysning vid 
    kommande faktureringstillfällen så att nytt uppskattat belopp kan faktureras 
    a-conto. Vid slutfaktureringen skall a-contobeloppen dras av.
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  FASTPRIS
  - I betalningsplanen skall man kunna sätta antigen procentsatser eller fasta priser 
    för varje delbetalning.
    (PUNKT UNDER FASTPRIS)
  - Det skall finnas möjlighet att titta på nedlagda timmar resp registrerade 
    lev.fakturor som upplysning.
    (PUNKT UNDER FASTPRIS)
    
  AVTAL
  - Det fakturerade beloppet skall kunna fördelas på de olika AO som ligger 
    samfaktureirngskopplade - antingen som %-sats eller som fast belopp.
    (PUNKT UNDER AVTAL)
  
  KONTON FÖR FAKTURERING
   - Varje faktura innehåller en rad med kundfordranskontering, 
     en rad med momskontering (ej vid momsfritt) samt en eller flera rader med 
     intäktskontering.
     Kundfordranskonteringen innehåller konto, motpart och AO
     Momskonteringen innehåller enbart konto
     Intäktskonteringarna innehåller konto, motpart, ansvar och AO
   - Dessutom skapas för varje faktura statistikuppgifter i form av fakturerade 
     timmar kollektiv personal resp fakturerade timmar tjänstemän. Dessa uppgifter 
     åsätts konto och AO. 
     Lönearterna styr vilka timmar som skall med.
   - Varje kund är antingen extern eller intern (koncernintern). Kontona i 
     kundfordranskonteringen resp intäktskonteringen styrs av detta.
   - Motpartskonteringen hämtas från kunduppgiften (kommer in med kundregistret från 
     annat system)
   - Momskonteringen styrs av en momskod som default finns på kunden (kommer in med 
     kundregistret från annat system). 
     Momskoden kan ändras vid faktureringstillfället. 
   - Det finns ett antal olika faktureringstyper som förutom nedanstående varianter 
     även styr intäktskontot. Följande alternativ kan förekomma:
       1) Förskottsfakturering utan moms. Dvs alla belopp beräknas utan moms om det 
          inte är en slutfaktura. 
       2) Förskottsfakturering med moms. Dvs alla belopp beräknas med den momskod som 
          gäller för fakturan.
       3) Slutfaktura för omomsade förskott.
       4) Slutfaktura för momsade förskott.
       5) Differentierad moms. Dvs varje post i fakturan har egen momskod.
     - Konteringen av fakturan läggs default ut enligt de konteringsregler som gäller 
       för kunden eller AO-numret. Det skall finnas möjlighet att göra manuella 
       justeringar av default-konteringen direkt i registreringsbilden.      
      (PUNKT UNDER KONTERING)
  
  UTSKRIFTER
  - A-priser skall visas på fakturan.
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Det skall gå att visa befattningar på fakturan istället för namn på de personer 
    som har skrivit tid. 
    (PUNKT UNDER LÖPANDE FAKTURERING)
    
  - Fakturorna skall kunna skrivas ut som färdiga fakturor.
    (PUNKT UNDER UTSKRIFTER)  

  - Fakturatexten i huvudet skall default lägga in den text som ligger på AO (kommer  
    från AO-systemet). Texten skall vara möjlig att ändra manuellt.
    (PUNKT UNDER UTSKRIFTER) (SE ORDERHUVUD)
  
  SKARPFAKTURERING
  - Fakturadatum = bokföringsdatum. Default-värde är dagens datum. Dock är det 
    vanligt vid månadsskiften att fakturadatum sätts till sista dagen i föregående 
    månad ett par dagar in i efterföljande månad. Kalenderbaserat registreringsdatum 
    styr förfallodagen oavsett fakturadatum.
    Man kan vid varje fakturerings tillfälle sätta fakturadatum samt förfallodatum.
    (PUNKT UNDER GENERELLT)
    
  - Fakturanumret byggs upp med bolagsbegrepp, årtal, arkivställe + löpnummer. 
    Arkivstället styrs av AO-numrets ansvar.
    (PUNKT UNDER GENERELLT) 
    
  - Grunduppgifter som skall skrivas ut på fakturan styrs per bolag. Grunduppgifter 
    kan vara bolagets namn, adress, bankgiro, postgiro, organisationsnummer, 
    VAT-nummer, uppgifter om F-skattesedel, dröjsmålsräntesats).
    (PUNKT UNDER GENERELLT) (SE ORGANISTION)
  
  - Avsändaradress på fakturan som avviker från grunduppgifterna styrs av AO-numrets 
    ansvar.
    (PUNKT UNDER GENERELLT)  

  - Fakturabeloppet är alltid i hela kronor. Ev. öresutjämning läggs i momsbeloppet.
    (PUNKT UNDER GENERELLT)  
  
  - Återrapportering av preliminära fakturor kopplas  till den som producerat den.
    Lösning : Vid skarp fakturering skickas ett medelande till den som har skapat den     
    Preliminära fakturan om det ej är samma person som gör den skarpa.
    (PUNKT 0 I TILLÄGGSOFFTER)
    
UNDER KREDITFAKTURERING   
   - Det skall gå att skapa kreditnotor antingen genom vändning av tidigare faktura 
     alternativt helt manuellt. Vid skapande av kreditering utgående från befintlig 
     faktura skall delposterna komma upp och kunna märkas med krediteras/ej 
     omfakturering, krediteras/omfakturering alternativt krediteras/avvakta 
     till nästa fakturering.
     (PUNKT UNDER LÖPANDE FAKTURERING)
            
UNDER TIDSKRIVNING STANSBILD OCH ÄNDRING AV TID.
  - Koppla på en fakturabefattning redan vid  tidskrivningen.
 
    Lösning : Det ni menar med befattning här skall översättas med ett nytt begrepp 
    "faktureringsbefattningar".  

    1. Dessa faktureringsbefattningar måste läggas upp i Guru och prissättas i varje 
       prislista i Guru med priser för normaltid, restid och övertid. 
    2. Det måste också vara möjligt att byta ut personens 
       befattning mot dessa faktureringsbefattning i fakturarutinen samt att kunna 
       dela upp en post i två poster, för att kunna ha olika 
       faktureringsbefattningar för samma tidskrivning.   
    3. Det skall kunna vara möjligt att sätta 
       faktureringsbefattning direkt vid tidskrivningen. 
       Detta är en mycket stor förändring. Alla tidskrivningsrutiner måste ändras 
       samt att en databasförändring gällande tidskrivningen måste göras. Detta kräver
       helg arbete alternativt ett större driftstopp under veckan.  
      (PUNKT 0 I TILLÄGGSOFFTER)

UNDER INTEGRATION.
  - Till ekonomisystemet skall skickas redovisningstransar med uppgift om t ex 
    verifikationsnummer (=fakturanummer), kontering, belopp.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Till ekonomisystemets kundreskontra skall även skickas reskontratransaktioner med 
    uppgift om t ex kundnr, fakturanr, fakturadatum, förfallodatum, belopp, momsbelopp.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Kundregistret läses in från annat system. Härifrån hämtas uppgift om t ex 
    kundnummer, kundnamn, adressuppgifter, motpart, betalningsvillkor (förfallodag).
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Kalkylen på fastprisprojekten skall läsas in från LOTS.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)

