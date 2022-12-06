DEFINE VARIABLE res AS LOGICAL.

RUN SendMailWithAttachment.p(
    INPUT "Erik Olsson",
    INPUT "elpool.ume@elpool.se",
    INPUT "Test",
    INPUT "This is a test message" + CHR(10) + "with multiple lines",
    INPUT "c:\autoexec.bat",
    OUTPUT res).
message res.
