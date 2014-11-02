#include <stdio.h>
#include "test_support.h"

typedef enum {pass, fail, does_not_apply, action_required, unknown} status;

static status test_status = fail;

#define max_name_len 15
static char test_name [max_name_len];
static int test_name_len;

void report__test (_ada_string * name, _ada_string * descr)
{
  int j;
  test_status = pass;

  if (name->upper - name->lower + 1 <= max_name_len)
    test_name_len = name->upper - name->lower + 1;
  else
    test_name_len = max_name_len;
  
  for (j=0; j<test_name_len; j++)
    test_name[j] = name->data[name->lower + j];

  printf ("\n,.,. %.*s ACATS 3.0 14-11-02 15:50:00\n",
          test_name_len,
          test_name);
  printf ("---- %.*s %.*s.\n",
          test_name_len,
          test_name,
          descr->upper - descr->lower + 1,
          descr->data + descr->lower);

}

void report__result ()
{
  switch (test_status){
  case pass:
    printf ("==== %.*s PASSED ============================\n",
            test_name_len,
            test_name);
  case does_not_apply:
    printf ("++++ %.*s NOT-APPLICABLE ++++++++++++++++++++\n",
            test_name_len,
            test_name);
  case action_required:
    printf ("!!!! %.*s TENTATIVELY PASSED !!!!!!!!!!!!!!!!\n",
            test_name_len,
            test_name);
    break;
  default:
    printf ("**** %.*s FAILED ****************************\n",
            test_name_len,
            test_name);
  }

  test_status = fail;
  test_name_len = 0;
}

