   /*VALDBALL.I*/
   
   /*ELPOOL*/
   {VALDBELPOOL.I}
   {VALDBELPOOLTKW.I}
   {VALDBELPOOLBM.I}
   {VALDBELPOOLHS.I}
   
   /*EGEN SERVER FÖRETAGEN HOSTAR SJÄLV*/           
  
   {VALDBSUNDNAT.I}
   {VALDBSUND9.I}
   {VALDBMISV.I}
   {VALDBKALM.I}
   {VALDBUMEA.I}
   {VALDBONE.I}
   {VALDBLUL.I}
   {VALDBREJL.I}
   
   /*VATTENFALL*/
   {VALDBVESABUTBIPRO.I}
   {VALDBVESABPRO.I}
   {VALDBVESABDP.I} 
   
   
   /*INFRAOMEXOMSERVER server 1*/
   {VALDBINFRAOMEXOMSERVER.I}
   
   /*WWW2BERGET server 2*/ 
   {VALDBWWW2BERGET.I}
   
   /*EGENBERGET server 3*/
   {VALDBEGENBERGET.I}
   
  
   
   
   
   /*
   uppsagda
   {VALDBALAK.I}
   {VALDBPFBK.I}
   {VALDBLABE.I}
   {VALDBGREL.I}
      {VALDBPSNK.I}
      {VALDBETS.I}
      {VALDBVTGE.I}
      {VALDBEFLA.I}
      {VALDBTECMAR.I}
      {VALDBPENO.I}
      {VALDBALTE.I}
   */
   
  
 

/* portar mm
   WWW2
   
   ============
   KRYPTERAT 
   valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
   AFCON 
   ELPC 
   ONEWWW  
   ==============
   KRYPTERAT
   valdbtemp.APPCON = {PasOewww2.i}. 
   AKEA
   DUTA
   ENKAB
   KEKR
   KRAF
   KRIN
   LABE
   NELK
   NOSS
   PC2U
   PFBK
   POFO
   POWE
   PSNK
   ROJO
   SKOLA
   SVEA
   UMBR
   VSWWW
   WIGA
   WSP 
   C4EN
   ============
   OKRYPTERAT
   Valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"
   ATS
   HJORT
   KEWA
   LAKL
   LIMO
   POLA
   SKEK
   SSEL
   SWECO
   
   ==========
   EGNA 
   KRYPTERAT
   valdbtemp.APPCON = "-AppService AppEgen -H " + {egna.I} + {guruonweb.i} + " " + {ssl.I}. 
   DEMOC
   ELTEL
   LAPP
   BODE
   OBS!
   VALDBSUNDNAT.I -AppService AppEgen MEN ANNAN SERVER
   
   ============
   OKRYPTERAT
   valdbtemp.APPCON = "-AppService APPSINGEL -H " + {egnaapp.I} + " -S 2665"
   ESKOG
   
   NSKOG
   OPPUN
   OXEL
   PICA
   SEKGS
   TECT
   valdbtemp.APPCON = "-AppService APPELKB
   valdbtemp.APPCON = "-AppService APPSKOGSK
   
  

******************************
Infratek

IP 31.216.227.28
2555 udp
2554 tcp
2600-2630 tcp.
************************************   


 
www2

IP 31.216.227.29

11.2
Port   2801 TCP
       2900-2989 TCP
       2802 UDP
valdbtemp.APPCON = '-AppService APPASP -H 31.216.227.29 -S 2802'. 

Pasoe 
TCP 8445 
valdbtemp.APPCON = '-URL https://pas.guruonweb.se:8445/apsv '.
enkab  krin  mini moln noss peno pite

11.6
TCP 2672  Appwww2
UDP 2671  Namewww2
TCP 2400-2500
valdbtemp.APPCON = '-AppService APPWWW2 -H pas.guruonweb.se -S 2671 -ssl'.
åf byns kasab kraf onewww



***************************************
Egna licenser

IP 31.216.227.30
   Pasoe 
   TCP 8445 
   Svensk Energi
valdbtemp.APPCON = '-URL https://egna.guruonweb.se:8445/apsv '. 
darplu sekg
11.6
   
TCP 2672  AppEgen
UDP 2671  NameEgen 
TCP 2400-2500
valdbtemp.APPCON = '-AppService AppEgen -H egna.guruonweb.se -S 2671 -ssl'. 
sundnat


Elkrafberedning
TCP 2554
TCP 2600-2607
UDP 2555

Picab
TCP 2454
TCP 2256-2270
UDP 2455

Singel  Alteco EloSKog GrEl NätSkog 
Oppund Övik Oxel Tectel
TCP 2664
TCP 2631-2660
UDP 2665


SkogsK
TCP 2654
TCP 2608-2615
UDP 2655
Elpool-SRV01  192.168.7.21    ->       31.216.227.28:3328  infra    31.216.227.28
Elpool-SRV02  192.168.7.22    ->       31.216.227.28:3329  www2     31.216.227.29
Elpool-SRV03  192.168.7.23    ->       31.216.227.28:3330  egna     31.216.227.30
   
  SLUTVALDB */

  
   
   