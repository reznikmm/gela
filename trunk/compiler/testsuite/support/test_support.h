typedef struct {
  char * data;
  int    lower;
  int    upper;
} _ada_string;

void report__test (_ada_string * name, _ada_string * descr);
void report__result ();
