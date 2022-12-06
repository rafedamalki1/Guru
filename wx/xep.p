/*
     Filename: XEP.P
      Created: 03.02.0027 09:38ELPAO     
     Modified: 
*/
/* DEFINE VARIABLE Profile    AS CHARACTER.      /* Profile name for sender */ */
DEFINE VARIABLE Address    AS CHARACTER.      /* Email address of recipient */
DEFINE VARIABLE CC         AS CHARACTER.      /* Email address of CC */
DEFINE VARIABLE BCC        AS CHARACTER.      /* Email address of BCC */
DEFINE VARIABLE Subject    AS CHARACTER.      /* Subject of email */
DEFINE VARIABLE Body       AS CHARACTER.      /* Body text */
DEFINE VARIABLE Filpath    AS CHARACTER.      /* Name of file to attach */
DEFINE VARIABLE Filnamn    AS CHARACTER.      /* Name only  */
DEFINE VARIABLE Sender     AS CHARACTER.      /* Sender  */

ASSIGN
Address = "niklas@elpool.se"
CC = ""
BCC = ""
Subject = "Test"
Body = "Hej hopp.....detta går bra"
Filpath = "Makrolista.doc,Datorkomponenter.doc"
Filnamn = "C:\Mikael\Makrolista.doc,C:\Mikael\Datorkomponenter.doc"
Sender = "elpnj".

RUN EPOSTMAPI.P (INPUT  Address,
                 INPUT  CC,
                 INPUT  BCC,
                 INPUT  Subject,
                 INPUT  Body,
                 INPUT  Filpath,
                 INPUT  Filnamn,
                 INPUT  Sender).
RETURN.
