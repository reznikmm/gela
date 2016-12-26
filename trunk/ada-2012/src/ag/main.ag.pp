#include "syntax.ag"
#include "full_name.ag"
#include "env_in.ag"
#include "env_out.ag"
#include "with_list.ag"
#include "name_list.ag"
#include "up.ag"
#include "down.ag"
#include "def_name.ag"
#include "inst.ag"
#include "errors.ag"
#include "static_value.ag"

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

Synthesized attributes
  discrete_simple_expression_range
    : Gela.Semantic_Types.Type_Index : type_index;

Rules for discrete_simple_expression_range. :
(.
      Gela.Pass_Utils.Resolve.Discrete_Range
        (Self.Compilation,
         ${discrete_simple_expression_range.env_in},
         ${Lower_Bound.Up},
         ${Upper_Bound.Up},
         ${discrete_simple_expression_range.type_index});
      
.)
