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
  operator_symbol
   : Gela.Semantic_Types.Auxiliary_Operator_Kinds : kind;

Rules for operator_symbol.operator_symbol_token :
(.
      --  Depends on ${operator_symbol.Down};
      ${operator_symbol.kind} := Gela.Semantic_Types.Is_Operator;
.)


