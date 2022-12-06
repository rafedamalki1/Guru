
/*
 * Written by Scott Auge scott_auge@yahoo.com sauge@amduus.com
 * Copyright (c) 2001 Amduus Information Works, Inc.  www.amduus.com
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Amduus Information Works
 *      Inc. and its contributors.
 * 4. Neither the name of Amduus Information Works, Inc. nor the names of 
 *    its contributors may be used to endorse or promote products derived 
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY AMDUUS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AMDUUS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *
 * Function returns a pure random string.  Not guaranteed to be unique however,
 * but most likely will be.
 */


&IF DEFINED(MAKEID3_I) = 0 &THEN
&GLOBAL-DEFINE MAKEID3_I YEP

DEF VAR RCSVersionMakeSeq AS CHARACTER INIT "$Header: /home/sauge/code/progress/pop3/RCS/MakeID3.i,v 1.1 2002/12/31 00:30:58 sauge Exp $" NO-UNDO.

FUNCTION MakeID3 RETURNS CHARACTER (INPUT pLength AS INTEGER):

  DEF VAR i AS CHARACTER NO-UNDO.

  RUN RandomString (INPUT pLength, OUTPUT i).

  ASSIGN i = i + STRING(ETIME).

  RETURN i.

END. /* FUNCTION MakeID2() */

PROCEDURE RandomString:

  DEF INPUT PARAMETER pLength   AS INTEGER NO-UNDO.
  DEF OUTPUT PARAMETER pString  AS CHARACTER NO-UNDO.

  DEF VAR lAlphabet AS CHARACTER NO-UNDO.
  DEF VAR i         AS INTEGER NO-UNDO.

  ASSIGN 
  lAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  i = 0
  pString = "".

  DO WHILE i < pLength:

    ASSIGN 
    i = i + 1
    pString = pString + SUBSTRING(lAlphabet, RANDOM(1,LENGTH(lAlphabet)), 1).

  END. /* DO WHILE i < pLength */

END. /* PROCEDURE RandomString */

&ENDIF
