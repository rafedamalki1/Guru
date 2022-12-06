UNDER REGISTER.
   TEXTER TILL �VERTID.
   - Vid �vertid skall typ av �vertid skrivas ut, t ex mont�r 50 %, mont�r 100 %. 
   (PUNKT UNDER L�PANDE FAKTURERING)

   PRISLISTOR
   - Det skall finnas mallprislistor som kan kopplas till kunderna. 
     Prislistorna skall vara m�rkta med giltighetsdatum - 
     f�r att nya prislistor skall kunna l�ggas upp parallellt med de gamla och 
     automatiskt b�rja g�lla n�r fakturering kommer till ett visst datum.
     (PUNKT UNDER L�PANDE FAKTURERING)
     
   KONTON F�R FAKTURERING
   - Varje faktura inneh�ller en rad med kundfordranskontering, 
     en rad med momskontering (ej vid momsfritt) samt en eller flera rader med 
     int�ktskontering.
     Kundfordranskonteringen inneh�ller konto, motpart och AO
     Momskonteringen inneh�ller enbart konto
     Int�ktskonteringarna inneh�ller konto, motpart, ansvar och AO
   - Dessutom skapas f�r varje faktura statistikuppgifter i form av fakturerade 
     timmar kollektiv personal resp fakturerade timmar tj�nstem�n. Dessa uppgifter 
     �s�tts konto och AO. 
     L�nearterna styr vilka timmar som skall med.
   - Varje kund �r antingen extern eller intern (koncernintern). Kontona i 
     kundfordranskonteringen resp int�ktskonteringen styrs av detta.
   - Motpartskonteringen h�mtas fr�n kunduppgiften (kommer in med kundregistret fr�n 
     annat system)
   - Momskonteringen styrs av en momskod som default finns p� kunden (kommer in med 
     kundregistret fr�n annat system). 
     Momskoden kan �ndras vid faktureringstillf�llet. 
   - Det finns ett antal olika faktureringstyper som f�rutom nedanst�ende varianter 
     �ven styr int�ktskontot. F�ljande alternativ kan f�rekomma:
       1) F�rskottsfakturering utan moms. Dvs alla belopp ber�knas utan moms om det 
          inte �r en slutfaktura. 
       2) F�rskottsfakturering med moms. Dvs alla belopp ber�knas med den momskod som 
          g�ller f�r fakturan.
       3) Slutfaktura f�r omomsade f�rskott.
       4) Slutfaktura f�r momsade f�rskott.
       5) Differentierad moms. Dvs varje post i fakturan har egen momskod.
     - Konteringen av fakturan l�ggs default ut enligt de konteringsregler som g�ller 
       f�r kunden eller AO-numret. Det skall finnas m�jlighet att g�ra manuella 
       justeringar av default-konteringen direkt i registreringsbilden.      
      (PUNKT UNDER KONTERING)
      
   NUMMERSERIE F�R FAKTUROR
   - Fakturanumret byggs upp med bolagsbegrepp, �rtal, arkivst�lle + l�pnummer. 
     Arkivst�llet styrs av AO-numrets ansvar.
     (PUNKT UNDER GENERELLT)
       
   BEST�LLARE/KUND 
   - L�gga in rabatt - m�jlighet (minus) i p�l�gget f�r material och fr�mmande 
     tj�nster p�  leverant�rsfakturor. 
     L�sning : 	I "Faktura regler" l�gger man in procentsatser f�r materiel och 
     fr�mmande tj�nster dessa skall �ven kunna vara negativa.
     (PUNKT J I TILL�GGSOFFTER)
     
  ORGANISTION    
  - Grunduppgifter som skall skrivas ut p� fakturan styrs per bolag. Grunduppgifter 
    kan vara bolagets namn, adress, bankgiro, postgiro, organisationsnummer, 
    VAT-nummer, uppgifter om F-skattesedel, dr�jsm�lsr�ntesats).
    (PUNKT UNDER GENERELLT) 
     
UNDER SEKRETESS 
  ANV�NDARBEH�RIGHET
  - Beh�righeten f�r resp anv�ndare skall kunna styras p� projektledarkod resp 
    ansvarskod eller kombinationer av dessa. 
    En viss inloggning skall t ex kunna fakturera f�r flera olika projektledare eller 
    ansvar.
    (PUNKT UNDER BEH�RIGHETSSYTEM)
  
  - Vid fastprisfakturering skall projektledaren f�r huvud-AO (AO med del-nr 00) kunna 
    se upplysningsinformation �ven f�r �vriga del-nr inom samma huvud-AO.
    (PUNKT UNDER BEH�RIGHETSSYTEM)
    
  - Beh�righetsniv� f�r att kunna g�ra prelimin�r faktura kontra skarp faktura.
    (PUNKT E I TILL�GGSOFFTER)

UNDER FAKTURERING
  F�RSTA BILDEN
  - Kunna v�lja i f�rstabilden att enbart se prelimin�rfaktura.
    (PUNKT B I TILL�GGSOFFTER)
    
  - M�jlighet att "ta bort prelimin�rfaktura" l�ggs in. D� �terst�lls allt till 
    ursprunget eller till senast fakturering. 
    Samtidigt �ndras det s� att man inte kan ta bort en skarp faktura.
    (PUNKT C I TILL�GGSOFFTER)
    
  - Kunna se bara den prelimin�r faktura som man har skapat.     
    L�sning : Vi l�gger till ett f�lt p� fakturan som s�ger vem som senast �ndrade 
    den prelimin�ra fakturan och l�gger till en m�jlighet att visa vad jag �ndrat p�.
    (PUNKT H I TILL�GGSOFFTER)
    
  - I f�rsta bilden i fakturarutinen skriva ut Avtalskoden(Projekt ovan) vid 
    kategori UA/UF. 
    L�sning :	 Lista alla fakturor med aonr som har Projektkoder A01,B01 osv. 
    Idag sparas inte Projektkoden vid inl�sning av aonr fr�n Ao- systemet. 
    Ett nytt f�llt f�r Projektkoden m�ste skapas f�r fakturan.
    (PUNKT M I TILL�GGSOFFTER)

  - Kunna v�lja kategori i "f�rsta bilden" i fakturarutinen. 
    L�sning :	M�jlighet att kunna g�ra urval p� en vald kategori och bara se den valda
    kategorien eller samtliga. 
    (PUNKT N I TILL�GGSOFFTER)

UNDER �NDRA FAKTURAPLAN
  GRUND UPPGIFTER 
  - L�gga in rabatt - m�jlighet (minus) i p�l�gget f�r material och fr�mmande tj�nster 
    p�  leverant�rsfakturor. 
    L�sning : I "Faktura regler" l�gger man in procentsatser f�r materiel och 
    fr�mmande tj�nster dessa skall �ven kunna vara negativa.
    (PUNKT J I TILL�GGSOFFTER)
    
  FASTPRIS
  - I betalningsplanen skall man kunna s�tta antigen procentsatser eller fasta priser 
    f�r varje delbetalning.
    (PUNKT UNDER FASTPRIS)
  - Det skall finnas m�jlighet att titta p� nedlagda timmar resp registrerade 
    lev.fakturor som upplysning.
    (PUNKT UNDER FASTPRIS)
    
  AVTAL
  - Det fakturerade beloppet skall kunna f�rdelas p� de olika AO som ligger 
    samfaktureirngskopplade - antingen som %-sats eller som fast belopp.
    (PUNKT UNDER AVTAL)
  AVIKANDE FAKTURA 
  - Avvikande kundnummer mot ao-numrets kund f�r att skapa m�jlighet att ha tv� eller 
    flera kunder mot samma ao-nummer. Alla kunder utom den ursprungliga faktureras 
    enbart genom fri komplettering. 
    L�sning : Det skall g� att skapa fakturor d�r man v�ljer kund och aonr fritt. 
    En ny faktureringsm�jlighet inf�rs i Guru d�r detta blir m�jligt.
    (PUNKT Q I TILL�GGSOFFTER)

UNDER FAKTURERA 
  ORDERHUVUD
  - L�gga in en bild med faktureringsinfo, "Orderhuvud", vilken ska visas efter man 
    g�tt in p� fakturera. Bilden ska inneh�lla; kundadress, best�llare, kundreferens, 
    v�r referens, arbetets omfattning, prelimin�r faktura nr, bolagskod (enhet), 
    val f�r fakturatyp (a�conto, slutfaktura, kreditfaktura etc.), momskod, 
    val av "tom faktureringsdatum" och "senast godk�nda och veckok�rda tidsedel 
    avs�g vecka xx". Det ska ocks� visas info om tidigare gjorda faktureringar i 
    denna bild (summa och typ; f�rskott utan moms, f�rskott med moms och f�rs�ljning).
    (PUNKT D I TILL�GGSOFFTER)
 
  - Fakturatexten i huvudet skall default l�gga in den text som ligger p� AO (kommer  
    fr�n AO-systemet). Texten skall vara m�jlig att �ndra manuellt.
    (PUNKT UNDER UTSKRIFTER)
  
  - Den arbetsomfattning som finns i AO- systemet och �terfinns p� ao- numrets 
    anm�rkning i GURU skall �ven finnas p� den fakturatext man kan skriva in i GURU's 
    faktureringsrutin.   
    (PUNKT G I TILL�GGSOFFTER)
    
  ALLM�NT
  - Vid l�pande fakturering. Kunna v�xla mellan sammanst�llnings -bild o 
    detaljtransaktioner. B�de f�r tidtransaktioner och mtrl. 
    Vid inhopp i rutinen skall sammanst�llnings -bilderna visas
    f�r nya transaktioner resp. f�r hittills fakturerat. 
    (PUNKT A I TILL�GGSOFFTER)

  - Summering av arbetskostnaden i faktureringsbilden v�nds p� andra h�llet s� att de 
    olika faktureringsrubrikerna kommer till v�nster och f�r varje rubrik visas antal,
    pris och summa. Summering av material och �vrig kostnaden v�nds ocks� p� andra 
    h�llet och summeras f�r hela fakturan grupperat p� material, fr�mmande tj�nst, 
    �vrigt.
    (PUNKT F I TILL�GGSOFFTER)
       
  TIDPOSTER/L�PANDE R�KNING
  - Det skall g� att justera start och sluttid alternativt antal upparbetade timmar 
    f�r varje persons tidskrivning per aonr och dag samt justera befattning och pris 
    per registrering. 
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Man kan ta st�llning till om registreringen skall tas med eller ej (ta med direkt,
    inte ta med alls alternativt v�lja bort vid detta faktureringstillf�lle - men f� 
    upp posten igen vid n�sta fakturering). 
    (PUNKT UNDER L�PANDE FAKTURERING)

  - Man kan v�lja att bara ta med registreringar fr�n och med ett datum. F�r t ex 
    viss person som t ex inte l�mnat in sin tidsedel till f�rra faktureringen - men 
    tiden �nd� tagits med manuellt i faktureringen.
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Man skall se sista tidskrivningsdatum f�r varje person och aonr.
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Vid �vertid skall typ av �vertid skrivas ut, t ex mont�r 50 %, mont�r 100 %.
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Koppla p� en fakturabefattning redan vid  tidskrivningen.
 
    L�sning : Det ni menar med befattning h�r skall �vers�ttas med ett nytt begrepp 
    "faktureringsbefattningar".  

    1. Dessa faktureringsbefattningar m�ste l�ggas upp i Guru och priss�ttas i varje 
       prislista i Guru med priser f�r normaltid, restid och �vertid. 
    2. Det m�ste ocks� vara m�jligt att byta ut personens 
       befattning mot dessa faktureringsbefattning i fakturarutinen samt att kunna 
       dela upp en post i tv� poster, f�r att kunna ha olika 
       faktureringsbefattningar f�r samma tidskrivning.   
    3. Det skall kunna vara m�jligt att s�tta 
       faktureringsbefattning direkt vid tidskrivningen. 
       Detta �r en mycket stor f�r�ndring. Alla tidskrivningsrutiner m�ste �ndras 
       samt att en databasf�r�ndring g�llande tidskrivningen m�ste g�ras. Detta kr�ver
       helg arbete alternativt ett st�rre driftstopp under veckan.  
       (PUNKT 0 I TILL�GGSOFFTER)
 
  FRI KOMPLETERING
  - Det skall finnas vallistor f�r olika typer av �vriga kostnader i 
    till�ggsfaktureringen.  
    (PUNKT UNDER L�PANDE FAKTURERING)
      
  - Det skall g� att skapa materiellistor f�r att f� ut materielspecifikationer till 
    kund med kundpris.
    (PUNKT UNDER L�PANDE FAKTURERING)

  EXTERNA FAKTUROR
  - Det skall g� att byta verifikattext p� leverant�rsfakturorna fr�n ekonomisystemet
   (idag st�r det leverant�rens namn) vid fakturering och ev. �ven beloppet om det 
   inte r�cker med procentp�slagen ovan. 
   (PUNKT UNDER L�PANDE FAKTURERING)
   
  ACONTON
  - Det skall g� att g�ra a-contofaktureringar genom att nedlagda timmar och anv�nt 
    material enbart tj�nar som upplysning f�r hur stort belopp fakturan skall st�llas 
    ut p�. A-contobeloppet skall synas i faktureringsbilden som upplysning vid 
    kommande faktureringstillf�llen s� att nytt uppskattat belopp kan faktureras 
    a-conto. Vid slutfaktureringen skall a-contobeloppen dras av.
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  FASTPRIS
  - I betalningsplanen skall man kunna s�tta antigen procentsatser eller fasta priser 
    f�r varje delbetalning.
    (PUNKT UNDER FASTPRIS)
  - Det skall finnas m�jlighet att titta p� nedlagda timmar resp registrerade 
    lev.fakturor som upplysning.
    (PUNKT UNDER FASTPRIS)
    
  AVTAL
  - Det fakturerade beloppet skall kunna f�rdelas p� de olika AO som ligger 
    samfaktureirngskopplade - antingen som %-sats eller som fast belopp.
    (PUNKT UNDER AVTAL)
  
  KONTON F�R FAKTURERING
   - Varje faktura inneh�ller en rad med kundfordranskontering, 
     en rad med momskontering (ej vid momsfritt) samt en eller flera rader med 
     int�ktskontering.
     Kundfordranskonteringen inneh�ller konto, motpart och AO
     Momskonteringen inneh�ller enbart konto
     Int�ktskonteringarna inneh�ller konto, motpart, ansvar och AO
   - Dessutom skapas f�r varje faktura statistikuppgifter i form av fakturerade 
     timmar kollektiv personal resp fakturerade timmar tj�nstem�n. Dessa uppgifter 
     �s�tts konto och AO. 
     L�nearterna styr vilka timmar som skall med.
   - Varje kund �r antingen extern eller intern (koncernintern). Kontona i 
     kundfordranskonteringen resp int�ktskonteringen styrs av detta.
   - Motpartskonteringen h�mtas fr�n kunduppgiften (kommer in med kundregistret fr�n 
     annat system)
   - Momskonteringen styrs av en momskod som default finns p� kunden (kommer in med 
     kundregistret fr�n annat system). 
     Momskoden kan �ndras vid faktureringstillf�llet. 
   - Det finns ett antal olika faktureringstyper som f�rutom nedanst�ende varianter 
     �ven styr int�ktskontot. F�ljande alternativ kan f�rekomma:
       1) F�rskottsfakturering utan moms. Dvs alla belopp ber�knas utan moms om det 
          inte �r en slutfaktura. 
       2) F�rskottsfakturering med moms. Dvs alla belopp ber�knas med den momskod som 
          g�ller f�r fakturan.
       3) Slutfaktura f�r omomsade f�rskott.
       4) Slutfaktura f�r momsade f�rskott.
       5) Differentierad moms. Dvs varje post i fakturan har egen momskod.
     - Konteringen av fakturan l�ggs default ut enligt de konteringsregler som g�ller 
       f�r kunden eller AO-numret. Det skall finnas m�jlighet att g�ra manuella 
       justeringar av default-konteringen direkt i registreringsbilden.      
      (PUNKT UNDER KONTERING)
  
  UTSKRIFTER
  - A-priser skall visas p� fakturan.
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Det skall g� att visa befattningar p� fakturan ist�llet f�r namn p� de personer 
    som har skrivit tid. 
    (PUNKT UNDER L�PANDE FAKTURERING)
    
  - Fakturorna skall kunna skrivas ut som f�rdiga fakturor.
    (PUNKT UNDER UTSKRIFTER)  

  - Fakturatexten i huvudet skall default l�gga in den text som ligger p� AO (kommer  
    fr�n AO-systemet). Texten skall vara m�jlig att �ndra manuellt.
    (PUNKT UNDER UTSKRIFTER) (SE ORDERHUVUD)
  
  SKARPFAKTURERING
  - Fakturadatum = bokf�ringsdatum. Default-v�rde �r dagens datum. Dock �r det 
    vanligt vid m�nadsskiften att fakturadatum s�tts till sista dagen i f�reg�ende 
    m�nad ett par dagar in i efterf�ljande m�nad. Kalenderbaserat registreringsdatum 
    styr f�rfallodagen oavsett fakturadatum.
    Man kan vid varje fakturerings tillf�lle s�tta fakturadatum samt f�rfallodatum.
    (PUNKT UNDER GENERELLT)
    
  - Fakturanumret byggs upp med bolagsbegrepp, �rtal, arkivst�lle + l�pnummer. 
    Arkivst�llet styrs av AO-numrets ansvar.
    (PUNKT UNDER GENERELLT) 
    
  - Grunduppgifter som skall skrivas ut p� fakturan styrs per bolag. Grunduppgifter 
    kan vara bolagets namn, adress, bankgiro, postgiro, organisationsnummer, 
    VAT-nummer, uppgifter om F-skattesedel, dr�jsm�lsr�ntesats).
    (PUNKT UNDER GENERELLT) (SE ORGANISTION)
  
  - Avs�ndaradress p� fakturan som avviker fr�n grunduppgifterna styrs av AO-numrets 
    ansvar.
    (PUNKT UNDER GENERELLT)  

  - Fakturabeloppet �r alltid i hela kronor. Ev. �resutj�mning l�ggs i momsbeloppet.
    (PUNKT UNDER GENERELLT)  
  
  - �terrapportering av prelimin�ra fakturor kopplas  till den som producerat den.
    L�sning : Vid skarp fakturering skickas ett medelande till den som har skapat den     
    Prelimin�ra fakturan om det ej �r samma person som g�r den skarpa.
    (PUNKT 0 I TILL�GGSOFFTER)
    
UNDER KREDITFAKTURERING   
   - Det skall g� att skapa kreditnotor antingen genom v�ndning av tidigare faktura 
     alternativt helt manuellt. Vid skapande av kreditering utg�ende fr�n befintlig 
     faktura skall delposterna komma upp och kunna m�rkas med krediteras/ej 
     omfakturering, krediteras/omfakturering alternativt krediteras/avvakta 
     till n�sta fakturering.
     (PUNKT UNDER L�PANDE FAKTURERING)
            
UNDER TIDSKRIVNING STANSBILD OCH �NDRING AV TID.
  - Koppla p� en fakturabefattning redan vid  tidskrivningen.
 
    L�sning : Det ni menar med befattning h�r skall �vers�ttas med ett nytt begrepp 
    "faktureringsbefattningar".  

    1. Dessa faktureringsbefattningar m�ste l�ggas upp i Guru och priss�ttas i varje 
       prislista i Guru med priser f�r normaltid, restid och �vertid. 
    2. Det m�ste ocks� vara m�jligt att byta ut personens 
       befattning mot dessa faktureringsbefattning i fakturarutinen samt att kunna 
       dela upp en post i tv� poster, f�r att kunna ha olika 
       faktureringsbefattningar f�r samma tidskrivning.   
    3. Det skall kunna vara m�jligt att s�tta 
       faktureringsbefattning direkt vid tidskrivningen. 
       Detta �r en mycket stor f�r�ndring. Alla tidskrivningsrutiner m�ste �ndras 
       samt att en databasf�r�ndring g�llande tidskrivningen m�ste g�ras. Detta kr�ver
       helg arbete alternativt ett st�rre driftstopp under veckan.  
      (PUNKT 0 I TILL�GGSOFFTER)

UNDER INTEGRATION.
  - Till ekonomisystemet skall skickas redovisningstransar med uppgift om t ex 
    verifikationsnummer (=fakturanummer), kontering, belopp.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Till ekonomisystemets kundreskontra skall �ven skickas reskontratransaktioner med 
    uppgift om t ex kundnr, fakturanr, fakturadatum, f�rfallodatum, belopp, momsbelopp.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Kundregistret l�ses in fr�n annat system. H�rifr�n h�mtas uppgift om t ex 
    kundnummer, kundnamn, adressuppgifter, motpart, betalningsvillkor (f�rfallodag).
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)
    
  - Kalkylen p� fastprisprojekten skall l�sas in fr�n LOTS.
    (PUNKT UNDER INTEGRATION MED ANDRA SYSTEM)

