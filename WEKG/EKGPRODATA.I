/**/
/*------------------------------------------------------------------------
    File        : EKGPRODATA.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Tue Jan 08 09:20:52 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/ 
   /*RESURSER basresurser - resurser,*/
   DEFINE DATASET EKGResursDS FOR ekgresursTT.
   
 
   /*/*KATALOGVISNING*/
   DEFINE DATASET EKGSubKatvisDS FOR ekgsubkatlogerTT.*/
   
   /*KATALOG - subkatalog nivåer, materiel, påslag*/
   DEFINE DATASET EKGSubKatDS FOR ekgsubkatTT, ekgnivaTT, ekgmtrlTT, ekgpaslagTT, ekgregelTT
   DATA-RELATION EKGSubKatDR1 FOR ekgsubkatTT, ekgmtrlTT RELATION-FIELDS(ekgsubkatTT.EKGSUBID, ekgmtrlTT.EKGSUBID, ekgsubkatTT.EBRKAT, ekgmtrlTT.EBRKAT)
   DATA-RELATION EKGSubKatDR2 FOR ekgsubkatTT, ekgnivaTT RELATION-FIELDS(ekgsubkatTT.EKGSUBID, ekgnivaTT.EKGSUBID, ekgsubkatTT.EBRKAT, ekgnivaTT.EBRKAT)
   DATA-RELATION EKGSubKatDR3 FOR ekgsubkatTT, ekgpaslagTT RELATION-FIELDS(ekgsubkatTT.EKGSUBID, ekgpaslagTT.EKGSUBID, ekgsubkatTT.EBRKAT, ekgpaslagTT.EBRKAT)
   DATA-RELATION EKGSubKatDR4 FOR ekgsubkatTT, ekgregelTT RELATION-FIELDS(ekgsubkatTT.EKGSUBID, ekgregelTT.EKGSUBID, ekgsubkatTT.EBRKAT, ekgregelTT.EBRKAT).

   /*KATALOGRESURSER - Kopplingar till ekgresursprisTT, ekgresursnivaTT, ekgresurspaslagTT via SUBQUERYS*/
   DEFINE DATASET EKGKatalogResursDS FOR ekgresursprisTT, ekgresursnivaTT, ekgresurspaslagTT.
   
   /*P5*/
   DEFINE DATASET EKGP5DS FOR ekgp5TT, ekgp5resursTT
   DATA-RELATION ekgp5DR1 FOR ekgp5TT, ekgp5resursTT RELATION-FIELDS(ekgp5TT.EKGSUBID, ekgp5resursTT.EKGSUBID, ekgp5TT.EBRKAT, ekgp5resursTT.EBRKAT, ekgp5TT.P5ARBKOD, ekgp5resursTT.P5ARBKOD, ekgp5TT.P5LOPNR, ekgp5resursTT.P5LOPNR).
   
   /*P4*/
   DEFINE DATASET EKGP4DS FOR ekgp4TT, ekgp4resursTT, ekgp4frekvensTT
   DATA-RELATION ekgp4DR1 FOR ekgp4TT, ekgp4resursTT RELATION-FIELDS(ekgp4TT.EKGSUBID, ekgp4resursTT.EKGSUBID, ekgp4TT.EBRKAT, ekgp4resursTT.EBRKAT, ekgp4TT.P4ARBKOD, ekgp4resursTT.P4ARBKOD, ekgp4TT.P4LOPNR, ekgp4resursTT.P4LOPNR)
   DATA-RELATION ekgp4DR2 FOR ekgp4TT, ekgp4frekvensTT RELATION-FIELDS(ekgp4TT.EKGSUBID, ekgp4frekvensTT.EKGSUBID, ekgp4TT.EBRKAT, ekgp4frekvensTT.EBRKAT, ekgp4TT.P4ARBKOD, ekgp4frekvensTT.P4ARBKOD, ekgp4TT.P4LOPNR, ekgp4frekvensTT.P4LOPNR).
   
   /*P3*/
   DEFINE DATASET EKGP3DS FOR ekgp3TT, ekgp3resursTT, ekgp3frekvensTT
   DATA-RELATION ekgp3DR1 FOR ekgp3TT, ekgp3resursTT RELATION-FIELDS(ekgp3TT.EKGSUBID, ekgp3resursTT.EKGSUBID, ekgp3TT.EBRKAT, ekgp3resursTT.EBRKAT, ekgp3TT.P3ARBKOD, ekgp3resursTT.P3ARBKOD, ekgp3TT.P3LOPNR, ekgp3resursTT.P3LOPNR)
   DATA-RELATION ekgp3DR2 FOR ekgp3TT, ekgp3frekvensTT RELATION-FIELDS(ekgp3TT.EKGSUBID, ekgp3frekvensTT.EKGSUBID, ekgp3TT.EBRKAT, ekgp3frekvensTT.EBRKAT, ekgp3TT.P3ARBKOD, ekgp3frekvensTT.P3ARBKOD, ekgp3TT.P3LOPNR, ekgp3frekvensTT.P3LOPNR).
   
   /*P2*/
   DEFINE DATASET EKGP2DS FOR ekgp2TT, ekgp2resursTT, ekgp2frekvensTT, ekgp2mtrlTT
   DATA-RELATION ekgp2DR1 FOR ekgp2TT, ekgp2resursTT RELATION-FIELDS(ekgp2TT.EKGSUBID, ekgp2resursTT.EKGSUBID, ekgp2TT.EBRKAT, ekgp2resursTT.EBRKAT, ekgp2TT.P2ARBKOD, ekgp2resursTT.P2ARBKOD, ekgp2TT.P2LOPNR, ekgp2resursTT.P2LOPNR)
   DATA-RELATION ekgp2DR2 FOR ekgp2TT, ekgp2frekvensTT RELATION-FIELDS(ekgp2TT.EKGSUBID, ekgp2frekvensTT.EKGSUBID, ekgp2TT.EBRKAT, ekgp2frekvensTT.EBRKAT, ekgp2TT.P2ARBKOD, ekgp2frekvensTT.P2ARBKOD, ekgp2TT.P2LOPNR, ekgp2frekvensTT.P2LOPNR)
   DATA-RELATION ekgp2DR3 FOR ekgp2TT, ekgp2mtrlTT RELATION-FIELDS(ekgp2TT.EKGSUBID, ekgp2mtrlTT.EKGSUBID, ekgp2TT.EBRKAT, ekgp2mtrlTT.EBRKAT, ekgp2TT.P2ARBKOD, ekgp2mtrlTT.P2ARBKOD, ekgp2TT.P2LOPNR, ekgp2mtrlTT.P2LOPNR).
   
   /*P1*/
   DEFINE DATASET EKGP1DS FOR ekgp1TT, ekgp1frekvensTT
   DATA-RELATION ekgp1DR1 FOR ekgp1TT, ekgp1frekvensTT RELATION-FIELDS(ekgp1TT.EKGSUBID, ekgp1frekvensTT.EKGSUBID, ekgp1TT.EBRKAT, ekgp1frekvensTT.EBRKAT, ekgp1TT.P1ARBKOD, ekgp1frekvensTT.P1ARBKOD, ekgp1TT.P1LOPNR, ekgp1frekvensTT.P1LOPNR).
   
   /*ARBKODER P1,P2,P3,ekgarbkodextraanmTT*/
   DEFINE DATASET EKGArbKoderDS FOR ekgp1arbkodTT, ekgp2arbkodTT, ekgp3arbkodTT, ekgarbkodextraanmTT.
   
   
   /*p2kom*/
   DEFINE DATASET EKGP2komDS FOR ekgP2komTT,ekgKoppArbSvepTT. 
  
   
   