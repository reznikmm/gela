#include "test_support.h"

static _ada_string s1 = {"A22006B" - 1, 1, 7};
static _ada_string s2 = {"CHECK USE OF HT IN AND OUT OF COMMENTS" - 1, 1, 38};

void A22006B(){
  report__test (&s1, &s2);
}

