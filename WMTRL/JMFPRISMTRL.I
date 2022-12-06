
/*------------------------------------------------------------------------
    File        : JMFPRISMTRL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Mar 19 15:11:22 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/*SALDO MM MtrlJmfPrisTTh den som visas i grid*/
DEFINE TEMP-TABLE MtrlJmfPrisTT NO-UNDO
   FIELD ORDNING AS INTEGER 
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER {GridRubrikListaExtent.i} 
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL  {GridRubrikListaExtent.i}  
   FIELD ENRLEV AS CHARACTER EXTENT 4
   FIELD LAGERSALDO AS INTEGER EXTENT 4 
   FIELD VALD AS LOGICAL EXTENT 4
   FIELD ANMARK AS CHARACTER EXTENT 4
   FIELD ENHETJMF AS CHARACTER
   FIELD MINPRIS AS DECIMAL 
   FIELD TTRECID AS RECID
   FIELD InputLEVKOD AS CHARACTER
   INDEX ENR ORDNING ENR.
