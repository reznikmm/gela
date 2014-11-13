typedef struct {
  char * data;
  int    lower;
  int    upper;
} _ada_string;

void TEST (_ada_string * name, _ada_string * descr);
void RESULT ();
