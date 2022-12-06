.DATE   05 JAN 00  14:06:29  RID  678H   22 OKT 99  ELPAO                       
.                                                                          D5766
*RPX 678           V70=VNR   V73=DNR   V71=DAG      Dok./ändr.journal sist EL90 
*SKAPA r3                                                                       
*===============================================================================
@ .          OBS !!!!! OM ANNAT TIMMPRIS SÄTT OMR = PR I 11E OCH 2H             
@ .             ANNARS TAS PRISET FRÅN AONR:ETS ORG I 9E                        
@ .             BÅDE STANDARLÖN OCH OMKOSTNADER. ANVÄNDES PR FÅS INGA           
@ .             OMKOSTNADER.  detta gäller ej nord då de har inviduell          
@ .                                                                             
@ .                                                                             
@CHG V10H4 Emode$ .                                                             
@CHG V11H6 Etype$ .                                                             
@40LDV V99I4=5 .           tid                                                  
@RDL,v171,g,21,6,70 1-132 V150S132 .                                            
@       Srh,v171,g,21,6 'DN' 8-1 '	',M/r,Ö     rnm -2 .                         
@MCH,V171,h,2,V171,g,-2 ' ' 2-3,73-1 	,1,a 6-3,32-1 	,1,a .                     
@srh,v171,g,-0 'dn' 32-1 	,'n' rnm -2 .                                         
@Srh,v171,g,-2,6 'DN@' 46-6 '	',@        rnm -2 .                               
@BRK,V171,G .                                                                   
.                                                                               
*                             OMR-A                      RGR                    
*ENH OMR-P ANSTNR PRIS TID    TI MI AONR   D-NR KK       DATUM    TOT.PRIS      
*===.=====.======.====.======.==.==.======.====.========.========.==========.   
@RDC,v171,g,-2,6 1-132 v150 .                                                   
	v150(6-3)                          v150(39-2) v150(42-2) v150(46-6) V150(53-4) 
@brk,v171,g,-0 rnm -2 .                                                         
@rdb   CAL,v171,g,-2,6 'J(L) ' 24-6,31-2,34-2 '	',A,B,C \                       
A=B+C/60   RNM -2 .                                                             
@SOR,v171,g,-2 '' 2-3 '	',1 .                                                   
@MCH,v171,H,2,V171,G,-2 'D' 2-3,77-3,95-5,101-6 '	',1,A,B,C \                   
2-3,6-5,12-6,19-3 '	',1,B,C,A .                                                 
@CAL,v171,g,-0,6 'J(L) ' 19-4,24-6,67-10  '	',A,B,C \                           
C=A*B   RNM -2 .                                                                
@SOR,v171,g,-2 '' 2-3,37-6,44-4  '	',1,2,3 .                                    
@TOT,v171,g,-0 'SO*' 2-3,24-6,37-6,44-4,67-10 '	',s,+,S,S,+ .                   
@TOT,v171,g,-0 '' 31-5 '	',=  .                                                 
@RNM -2 .                                                                       
@SOR,v171,g,-2 '' 37-6,44-4  '	',1,2    RNM -2 .                                
@TOT,v171,g,-2 'SO*' 37-6,67-10 '	',S,+ .      
@RNM -1 .               FÖR ATT HAMTA KK MM                                     
@     LDV V99=5 .                                                               
@BRK,V171,G .                                                                   
.                                                                               
*                                                                               
*AONR   KK       RGR OMR                                                        
*======.========.===.=====.                                                     
@41INC V99 .                                                                    
@RDL,V171,G,-1,V99,45 37-6 V20H10 .                                             
@RSR 180 .                                                                      
	v20(1-6) V7 V8 V9                                                              
@GTO 41 .                                                                       
@45brk,v171,g,-0 rnm -1 .                                                       
@     MCH,v171,G,-1,V171,G,-2 'D' 2-6,9-8,18-3,22-5 '	',1,A,B,C \               
31-5,37-6,49-8,58-3 '	',C,1,A,B .                                               
@RNM -2 .                                                                       
@SRH,v171,g,-2,6 'DN' 58-3 '	',898     RNM -2 .                                 
@SOR,v171,g,-2 '' 2-3,31-5,49-8  '	',1,2,3  .                                   
@TOT,v171,g,-0 'SO*' 2-3,24-6,31-5,49-6,67-10 '	',S,+,S,S,+ .                   
@TOT,v171,g,-0 '' 58-8 '	',=960000 .                                            
@RNM -2 .              OM INGA KREDIT                                           
@GTO 20 .              DEBET POSTER                                             
@RNM -3 .              DEBET POSTER                                             
@SOR,v171,g,-2 '' 6-5 '	',1 .                                                   
@TOT,v171,g,-0 'SO*' 6-5,24-6,67-10 '	',S,+,+ .                                 
@TOT,v171,g,-0 '' 58-8 '	',=960000 .                                            
@TOT,v171,g,-0 '' 12-6,37-6,49-8,80-1 '	',='',='',='',='K' .                    
@RNM -1 .              KREDIT POSTER                                            
@ADD,V171,G,-1,V171,G,-3   RNM -2 .                                             
@20   CAL,v171,g,-2 'R1' 24-6,67-10 '	',A,B A=A;B=B     RNM -2 .                
@ .              SKAPA R3 FILE                                                  
@LDV V4A6=DATE1$ .                                                              
@LDV V5A8=19V4.                                                                 
@LDV V1A4=25XX .                                                                
@LDV V2A2=35 .                                                                  
@LDV V22A8=  .                                                                  
@LDV V3A6=ZNORM .                                                               
@LDV V4A10='' .                                                                 
@LDV V6S30='' .                                                                 
@LDV V7A12='' .       
@LDV V8A15='' .                                                                 
@LDV V9A15='' .                                                                 
@LDV V201A10='' .                                                               
@LDV V11A10='' .                                                                
@LDV V12A6='' .                                                                 
@LDV V13A8='' .                                                                 
@LDV V14A4='' .                                                                 
@LDV V15A4='' .                                                                 
@LDV V4A6=DATE1$ .                                                              
@IF V4(5-2) GE 01 & LE 07  dc d1=today-8 v4 .                                   
@IF  DEPN$ = 479 ldv  V14=lt10 .    nät                                         
@IF  DEPN$ = 486 ldv  V14=lt11 .    drift                                       
@IF  DEPN$ = 487 ldv  V14=lt12 .    marknad                                     
@IF  DEPN$ = 193 ldv  V14=lt13 .    vd staber                                   
@IF  DEPN$ = 479 ldv  V15=LT10 .                                                
@IF  DEPN$ = 486 ldv  V15=LT11 .                                                
@IF  DEPN$ = 487 ldv  V15=LT12 .                                                
@IF  DEPN$ = 193 ldv  V15=LT13 .                                                
@rdl,256,i,180,3 10-8 v22 .                                                     
@chg v23a8  v22 + 1 .                                                           
@ldv,zr v23=v23 .                                                               
@lok,256,i,180                                                                  
@wrl,256,i,180,3 10-8 '*',v23 .                                                 
@ulk .                                                                          
@DC T2=TIME V12 .                                                               
@LLN,V171,G,-2 V13 .                                                            
@CHG V13 V13 - 4 .                                                              
@LDV,ZR V13=V13 .                                                               
@LDV V99=5 .                                                                    
@BRK,V171,G .                                                                   
252020000105V12v15/usr/sap/KIS/tratt/2520-20000105-V12.v14.V13                  
@151INC V99 .                                                                   
@LDV V5=19DATE1$ .                                                              
@IF V5(7-2) GE 01 & LE 07   rsr 200 .                                           
@ .                                                                             
@RDL,V171,G,-2,V99,155 1-132 V150S132 .                                         
@LDV,ZR V4=V150(12-6) .   anstnr                                                
@LDV,ZR V7=V150(49-8) .   kk-nr                                                 
@LDV,ZR V8=V150(24-6) .   timmar                                                
@chg    V8 v8 * 1000 .  
@LDV,ZR V8=v8 .                                                                 
@LDV,ZR V9=V150(67-10) .  pengar                                                
@chg    V9 v9 * 100 .                                                           
@chg    V9 v9 * 0 .       INGA PENGAR .                                         
@LDV,ZR V9=V9 .                                                                 
@LDV,ZR V201=V150(58-8) .    kontonr                                            
@LDV,ZR V11=2520V150(31-5) .   OMRADE                                           
@IF     V8 = '000000000000000'     GTO 151 .   OM TIMMAR = 0                    
@IF V150(80-1) = 'K' LDV,ZR V11=2520V150(6-5) .   OMRADE                        
v1V2v22V5'00000'V4V3V6(1-10)V7V6(1-18)V8V9 V201V11                              
@GTO 151 .                                                                      
@155brk,v171,g,-0 rnm -2 .                                                      
@  .     UT MED FILEN                                                           
@94 . IF USER$ = 'ELPao'   dsP,v171,G,-2,,,,,,slutfil .                         
@CHG V10H4 Emode$ .                                                             
@CHG V11H6 Etype$ .                                                             
@ADD,V171,G,-2,V10,I,180 .   SPARA FILEN                                        
@REP,V10,I,-0,V10,I,180 .                                                       
@LDV V5=20DATE1$                                                                
@     LDV V243S26='/mapper/tmp/'v5v12 .              hård                       
@     LDV V243S14=v5v12 .              UN                                       
@     LDV V244S37='/mapper/tmp/2520-'v5'-'v12'.'v14 .              UN           
@     LDV V244S25='2520-'v5'-'v12'.'v14  .              UN                      
@IF v171 = 274   REL .                                                          
@ret,v171,G,,,95 v243    rnm -4 .                                               
@ADD,V171,G,-2,V171,G,-4    RNM -2 .                                            
@95  CHG V6a12 user$ .                                                          
@CHG V7i4 depn$ .                                                               
@fil,v171,G,-2,,,190 '/mapper/tmp/'v243 .                                       
@ .  IF USER$ = 'ELPao'   dsP,v171,G,-2,,,,,,v244 .                             
@br  CPR3,V243,V244 .                                                           
@70 GTO RPX 137 .                                                               
@ .     SUBRUTINER                                                              
@ .                                                                             
@180ldv V182=3 .                             NORMAL INGÅNG GER TRÄFF I 3b 13b   
@ .                                                                             
@ldv v110= ,v213=6 if V20(1-1) = 0  gto 6 .      Stående : 0XXxxx          1    
@ ldv v213=7 if v20(1-2) > 99 gto 8 .             Banknr  : BokstavXXXXX        
@IF V182 = 7 GTO 8 .                              Banknr  : BokstavXXXXX        
@ if v364(3-1) ne ' ' gto 50 .                                                  
@ lcv,5 b1 v364 '+'v20(1-1) v101  gto 11 .        Tillf.nr: SiffraXXXXX omr 1  
@5lcv,15 b1 v365 '+'v20(1-1) v101 gto 10 .        -     ''       -             
@50 lcv,55 b1 v364 '+'v20(1-2) v101  gto 11 .     Tillf.nr: SiffraSIFFRA     1 
@55 lcv,15 b1 v365 '+'v20(1-2) v101 gto 10 .        -     ''       -           
@6          lcv,7 b1 v366 '+'v20(2-2) v101  gto 11 .        Stående            
@7lcv,13 b1 v367 '+'v20(2-2) v101 gto 10 .        -     ''       -   6-10      
@8lcv,9 b1 v362 '+'v20(1-2) v101  gto 11 .        Banknr                omr 1  
@9lcv,15 b1 v363 '+'v20(1-2) v101 gto 10 .        -     ''       -   6-10      
@10inc,50 v182 .                                                               
@11if v101 = 4  inc,10 v182 ; if v101 = 7  inc,20 v182 .                       
@  if v101 = 10 inc,30 v182 ; if v101 = 13 inc,40 v182 ; ldv v101=1 gto 21 .   
@13if v366(2-2) ne '**' gto 15 ; ldv v101=1 .                                  
@lzr,v171,B,v182,14 bfn,v171,B,v182,5,14 ' ' 2-10 	,v20  ldv v101=1  gto 21 .  
@14inc v101 if v101 > v330 gto 20 ; inc,10 v182 gto lin -1 .                   
@15ldv,r v182=v182 if v182(4-1) = 3,5 gto lin +1 ; gto 190 .                   
@if v182(4-1) = 3 inc,8 v182 gto lin +1 ; inc,6 v182 .                         
@GTO 31 .                                                                      
@21BFN,V171,B,V182,5,15  ' ' 2-10 	,V20 ,V101 .                                
@CHG V182 V182 + 1 .                        .                                  
@RDL,V171,B,V182,V101  65-8 V7A8 .                                             
@CHG V182 V182 + 1 .                        .                                  
@RDL,V171,B,V182,V101 31-5,88-3  V9A5,V8A3 .                                   
@ESR .                                                                         
@31BFN,V171,B,V182,5,190 ' ' 2-10 	,V20 ,V101 .                                
@CHG V101 V101 + 1 .                        .                                  
@RDL,V171,B,V182,V101  65-8 V7A8 .                                             
@CHG V101 V101 + 1 .                        .                                  
@RDL,V171,B,V182,V101 31-5,88-3  V9A5,V8A3 .                                   
@ESR .                                                                         
@200dc d1=today-8 v5 .                                                         
@ldv v5=19v5 .                                                                 
@ESR .                                                                                                                                                                                                                            
