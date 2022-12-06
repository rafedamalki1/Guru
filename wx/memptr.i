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
 */


/*
 * Allow string operations on memory pointer based data
 * used for .so and socket operations.
 *
 */

DEF VAR RCSVersionMemptri AS CHARACTER INIT "$Header: /home/sauge/code/progress/pop3/RCS/memptr.i,v 1.2 2002/12/31 00:30:58 sauge Exp $" NO-UNDO. 

PROCEDURE lmemset:

  DEF INPUT-OUTPUT PARAMETER m AS MEMPTR NO-UNDO.
  DEF INPUT PARAMETER c AS CHARACTER NO-UNDO.
  
  DEF VAR i AS INTEGER INIT 1 NO-UNDO.
  
  DO while i < GET-SIZE(m):
    PUT-BYTE (m, i) = ASC (c).
    ASSIGN i = i + 1.
  END.
  
END.

PROCEDURE lmemput:

  DEF INPUT-OUTPUT PARAMETER m AS MEMPTR NO-UNDO.
  DEF INPUT PARAMETER s AS CHARACTER NO-UNDO.

  DEF VAR i AS INTEGER INIT 1 NO-UNDO.

  DO WHILE i < GET-SIZE(m):
    IF i < LENGTH (s) + 1 THEN
      PUT-BYTE (m, i) = ASC(SUBSTRING (s, i, 1)).
    ELSE
      PUT-BYTE (m, i) = 0.
    ASSIGN i = i + 1.
  END.

END.

PROCEDURE lmemget:

  DEF INPUT-OUTPUT PARAMETER  m AS memptr NO-UNDO.
  DEF OUTPUT PARAMETER c AS CHARACTER NO-UNDO.
  
  c = GET-BYTES (m, 1, GET-SIZE(m)).

END.
