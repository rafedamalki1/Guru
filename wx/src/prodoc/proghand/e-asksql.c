#include <stdio.h>
#include <fcntl.h>

main()

{
#define	LEN 		250
	char 	        result[LEN];
	int 		fdi, fdo, nread;
	char 	        request[LEN+8]; /* 8 for "outpipe " + punctuation */
	char 	       *ptr;
	int 		validq, i;

	fdi = open("inpipe", O_WRONLY);
	if (fdi < 0) 
	{ printf("Error on inpipe open\n"); exit(1);}

	strcpy(request, "outpipe \""); /* request starts with 'outpipe "' */

	while (1)
	{
                printf("\n\nEnter your request (type [RETURN] to exit):\n");
		ptr = request+9;
		nread = read(0, ptr, LEN);
		if (nread < 2)
		    exit(0);
		else
		{
			validq = 1;             /* valid query? */
			for (i = 9; i<nread+9; i++)
				if (request[i] == '\"')
				{ printf("Use only single quotes in queries.\n");
					validq = 0;
					break;
				}
		        if (! validq) continue;
			ptr += nread-1;
			*ptr++ = '\"';
			*ptr++ = '\n';
			*ptr++ = '\0';
			write(fdi, request, strlen(request));
			sleep(1);
			fdo = open("outpipe", O_RDONLY);
			if (fdo < 0) 
			{ printf("Error on outpipe open\n"); exit(1);}

			while ((nread = read(fdo, result, LEN)) != 0)
			{
				result[nread] = '\0';
				printf("%s", result);
			}
			close(fdo);
		}
	}
}			
