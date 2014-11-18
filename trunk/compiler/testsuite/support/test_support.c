#include <stdio.h>
#include "test_support.h"

typedef enum {pass, fail, does_not_apply, action_required, unknown} status;

static status test_status = fail;

#define max_name_len 15
static char test_name [max_name_len];
static int test_name_len;

void PUT_MSG (_ada_string * msg, int written)
{
  int max_len= 72 - written;
  int i=0;
  int m=msg->lower;
  int n = msg->upper, k;
  while (i + msg->upper - m + 1 > max_len){
    n = m + max_len - i - 1;
    if (msg->data[n - msg->lower] != ' '){
      while (n >= m && msg->data[n + 1 - msg->lower] != ' ') n--;
      if (n < m)
        n = m + max_len - i - 1;
    }
    for (k=0;k<i;k++) putchar (' ');
    printf ("%.*s\n", n - m + 1, msg->data + m - msg->lower);
    i = test_name_len + 9;
    m = n + 1;
    while (m <= msg->upper && msg->data [m - msg->lower] == ' ') m++;
    max_len = 72;
  }
  for (k=0;k<i;k++) putchar (' ');
  printf ("%.*s.\n", n - m + 1, msg->data + m - msg->lower);
}

void TEST (_ada_string * name, _ada_string * descr)
{
  int j;
  test_status = pass;

  if (name->upper - name->lower + 1 <= max_name_len)
    test_name_len = name->upper - name->lower + 1;
  else
    test_name_len = max_name_len;
  
  for (j=0; j<test_name_len; j++)
    test_name[j] = name->data[j];

  printf ("\n,.,. %.*s ACATS 3.0 14-11-02 15:50:00\n",
          test_name_len,
          test_name);
  printf ("---- %.*s ",
          test_name_len,
          test_name);
  PUT_MSG (descr, test_name_len + 6);
}

void RESULT ()
{
  switch (test_status){
  case pass:
    printf ("==== %.*s PASSED ============================.\n",
            test_name_len,
            test_name);
    break;
  case does_not_apply:
    printf ("++++ %.*s NOT-APPLICABLE ++++++++++++++++++++.\n",
            test_name_len,
            test_name);
    break;
  case action_required:
    printf ("!!!! %.*s TENTATIVELY PASSED !!!!!!!!!!!!!!!!.\n",
            test_name_len,
            test_name);
    break;
  default:
    printf ("**** %.*s FAILED ****************************/\n",
            test_name_len,
            test_name);
  }

  test_status = fail;
  test_name_len = 0;
}

