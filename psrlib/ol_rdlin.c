/* ol_rdlin.c - OLAF GETCMD interface to the GNU Readline library */

/* 1991 June 19     Dave Shone */

#include <stdio.h>
#include <string.h> /* <-- added by RCD 12/5/92 */
#include <readline/readline.h>

void ol_rdlin (retstr, len_retstr, new_prompt, len_prompt, ifail)

char *retstr;
int len_retstr;
char *new_prompt;
int *len_prompt;
int *ifail;

{
  int nch, i;
  static char *line_read = (char *)NULL;
  static char *prompt = (char *) NULL;

  *ifail=0;

  /*
    If a line buffer has already been allocated, free it; readline will
    allocate another.
  */
  if (line_read != (char *)NULL)
    {
      free (line_read);
      line_read = (char *)NULL;
    }

  /* 
    If a prompt has already been allocated, free it; a new one 
    (possibly of a different size) will be allocated.
  */
  if (prompt != (char *)NULL)
    {
      free (prompt);
    }

  /*
    If there is a prompt, allocate space for it and set it.
  */
  /* if (len_prompt != 0) */
  if (*len_prompt != 0)   /* <-- surely you mean this! - RCD 12/5/92 */
    {
      prompt = (char *)malloc(*len_prompt+2);
      strncpy(prompt, new_prompt, *len_prompt) ;
      prompt[*len_prompt] = ' ' ;
      prompt[*len_prompt+1] = '\0' ;
    }

  /* 
    Read a line from the user. 
  */
  line_read = readline (prompt);

  /* 
    If the line has any text in it, save it in the history. 
  */
  if (line_read && *line_read)
    {
      add_history (line_read);
    }

  /* 
    Copy the line to the return string, and pad with spaces. 
  */
  nch = strlen(line_read);
  strncpy (retstr, line_read, nch);
  for (i=nch; i<=len_retstr; i++)
    {
      retstr[i] = ' ';
    }
}

