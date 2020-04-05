/*
	termnl - returns the name of the terminal associated with stdout,
		 if it is not a terminal it returns a blank string.  The
		 string is null terminated, but lennam returns the length
		 of the string excluding the terminated null.
*/

char *ttyname () ;

termnl (devnam, lennam)

char *devnam ;				/* On exit the terminal name */
int  *lennam ;				/* On exit the length of the name */

{
	char *pointer ;

	if ((pointer = ttyname (1)) != 0)	/* Pointer to tty name */
	{
		strcpy (devnam, pointer) ;	/* Copy name to devnam */
		*lennam = strlen (devnam) ;	/* Return its length */
	}
	else
	{
		strcpy (devnam, " ") ;	/* Not a terminal, return blank name */
		*lennam = 1 ;
	}
}

	
