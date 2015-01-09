#include "syntax.ag"
#include "full_name.ag"
#include "env_in.ag"
#include "env_out.ag"
#include "with_list.ag"
#include "name_list.ag"
#include "up.ag"
#include "down.ag"
#include "def_name.ag"
#include "errors.ag"
#include "static_value.ag"

Synthesized attributes
  root_type_definition
   : Gela.Semantic_Types.Type_Index : type_kind;

Rules for root_type_definition.dummy_token :
(.
      ${root_type_definition.type_kind} := 0;
.)

Synthesized attributes
  function_declaration,
  procedure_declaration
   : Gela.Elements.Element_Access : corresponding_type ;

Rules for function_declaration. :
(.
      --  This attribute is custom code
      ${function_declaration.corresponding_type} := null;
.)

Rules for procedure_declaration. :
(.
      --  This attribute is custom code
      ${procedure_declaration.corresponding_type} := null;
.)
