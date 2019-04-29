# Abstract Syntax Tree Elements

## _Element_

 * [Pragma]
 * _[Defining_Name]_
 * _[Declaration]_
 * _[Definition]_
   * _[Type_Definition]_
      * _[Access_Type]_
   * _[Formal_Type_Definition]_
      * _[Formal_Access_Type]_
   * _[Anonymous_Access_Definition]_
   * _[Constraint]_
 * _[Expression]_
 * _[Association]_
 * _[Statement]_
 * _[Path]_
 * _[Clause]_
   * _[Representation_Clause]_
 * [Exception_Handler]

## Pragma
> _[Element]_
 - Pragma_Token                 : Token
 - Name                         : [Identifier]
 - Left_Bracket_Token           : Token?
 - Arguments                    : [Parameter_Association]*
 - Right_Bracket_Token          : Token?
 - Semicolon_Token              : Token

## _Defining_Name_
> _[Element]_

### Defining_Identifier
> _[Defining_Name]_
 - Identifier_Token : Token
 
### Defining_Character_Literal
> _[Defining_Name]_
 - Character_Literal_Token : Token

### Defining_Operator_Symbol
> _[Defining_Name]_
 - Operator_Symbol_Token : Token

### Defining_Expanded_Name
> _[Defining_Name]_
 - Prefix                : _[Expression]_ {[Selected_Component],[Identifier]}
 - Dot_Token             : Token
 - Selector              : [Defining_Identifier]

## _Declaration_
> _[Element]_

### Type_Declaration
> _[Declaration]_
 - Type_Token            : Token
 - Name                  : [Defining_Identifier]
 - Discriminant_Part     : _[Definition]_? {[Known_Discriminant_Part],[Unknown_Discriminant_Part]}
 - Is_Token              : Token
 - View                  : _[Definition]_ {_[Type_Definition]_,[Private_Type_Definition],[Private_Extension_Definition],[Incomplete_Type_Definition]}
 - With_Token            : Token?
 - Aspects               : [Aspect_Specification]*
 - Semicolon_Token       : Token


### Task_Type_Declaration
> _[Declaration]_
 - Task_Token            : Token
 - Type_Token            : Token
 - Name                  : [Defining_Identifier]
 - Discriminant_Part     : [Known_Discriminant_Part]?
 - With_Token            : Token?
 - Aspects               : [Aspect_Specification]*
 - Is_Token              : Token
 - New_Token             : Token?
 - And_Token             : Token?
 - Progenitors           : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token_2          : Token?
 - View                  : [Task_Definition]
 - Semicolon_Token       : Token

### Protected_Type_Declaration
> _[Declaration]_
 - Protected_Token       : Token
 - Type_Token            : Token
 - Name                  : [Defining_Identifier]
 - Discriminant_Part     : [Known_Discriminant_Part]?
 - With_Token            : Token?
 - Aspects               : [Aspect_Specification]*
 - Is_Token              : Token
 - New_Token             : Token?
 - Progenitors           : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token_2          : Token?
 - View                  : [Protected_Definition]
 - Semicolon_Token       : Token

### Subtype_Declaration
> _[Declaration]_
 - Subtype_Token         : Token
 - Name                  : [Defining_Identifier]
 - Is_Token              : Token
 - View                  : [Subtype_Indication]
 - With_Token            : Token?
 - Aspects               : [Aspect_Specification]*
 - Semicolon_Token       : Token

### Object_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Aliased_Token              : Token?
 - Constant_Token             : Token?
 - Object_Subtype             : _[Definition]_ {[Constrained_Array_Type],[Subtype_Indication],_[Anonymous_Access_Definition]_,[Identifier],[Selected_Component],[Attribute_Reference]}
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Single_Task_Declaration
> _[Declaration]_
 - Task_Token                 : Token
 - Name                       : [Defining_Identifier]
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - New_Token                  : Token?
 - Progenitors                : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token_2               : Token?
 - Object_Subtype             : [Task_Definition]
 - Semicolon_Token            : Token

### Single_Protected_Declaration
> _[Declaration]_
 - Protected_Token            : Token
 - Name                       : [Defining_Identifier]
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - New_Token                  : Token?
 - Progenitors                : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token_2               : Token?
 - Object_Subtype             : [Protected_Definition]
 - Semicolon_Token            : Token

### Number_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Constant_Token             : Token
 - Assignment_Token           : Token
 - Initialization_Expression  : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token            : Token

### Enumeration_Literal_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]

### Discriminant_Specification
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Object_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference]}
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}
 - Semicolon_Token            : Token

### Component_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Object_Subtype             : [Component_Definition]
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Loop_Parameter_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]
 - In_Token                   : Token
 - Reverse_Token              : Token?
 - Object_Subtype             : _[Discrete_Subtype_Definition]_

### Generalized_Iterator_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]
 - In_Token                   : Token
 - Reverse_Token              : Token?
 - Iterator_Name              : _[Expression]_ {^[Box_Expression]}

### Element_Iterator_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]
 - Colon_Token                : Token
 - Subtype_Indication         : [Subtype_Indication]
 - Of_Token                   : Token
 - Reverse_Token              : Token?
 - Iterable_Name              : _[Expression]_ {^[Box_Expression]}

### Procedure_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Procedure_Token            : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Is_Token                   : Token?
 - Abstract_Token             : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Function_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Is_Token                   : Token?
 - Result_Expression          : _[Parenthesized_Expression]_?
 - Abstract_Token             : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Parameter_Specification
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Aliased_Token              : Token?
 - In_Token                   : Token?
 - Out_Token                  : Token?
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Object_Subtype             : _[Element]_ {[Constrained_Array_Type],[Subtype_Indication],_[Anonymous_Access_Definition]_,[Identifier],[Selected_Component],[Attribute_Reference]}
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}

### Procedure_Body_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Procedure_Token            : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Declarative_Part           : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                : Token
 - Statements                 : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token            : Token?
 - Exception_Handlers         : [Exception_Handler]*
 - End_Token                  : Token
 - End_Name                   : _[Expression]_? {[Identifier],[Selected_Component]}
 - Semicolon_Token            : Token

### Function_Body_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Declarative_Part           : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                : Token
 - Statements                 : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token            : Token?
 - Exception_Handlers         : [Exception_Handler]*
 - End_Token                  : Token
 - End_Name                   : _[Expression]_? {[Identifier],[Selected_Component],[Operator_Symbol]}
 - Semicolon_Token            : Token

### Return_Object_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]
 - Colon_Token                : Token
 - Aliased_Token              : Token?
 - Constant_Token             : Token?
 - Object_Subtype             : _[Element]_ {[Subtype_Indication],_[Anonymous_Access_Definition]_}
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}

### Package_Declaration
> _[Declaration]_
 - Package_Token              : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Visible_Part               : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Private_Token              : Token?
 - Private_Part               : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - End_Token                  : Token
 - End_Name                   : _[Expression]_? {[Identifier],[Selected_Component]}
 - Semicolon_Token            : Token
 
### Package_Body_Declaration
> _[Declaration]_
 - Package_Token              : Token
 - Body_Token                 : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Declarative_Part           : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                : Token
 - Statements                 : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token            : Token?
 - Exception_Handlers         : [Exception_Handler]*
 - End_Token                  : Token
 - End_Name                   : _[Expression]_? {[Identifier],[Selected_Component]}
 - Semicolon_Token            : Token

### Object_Renaming_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Object_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Exception_Renaming_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Exception_Token            : Token
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {[Identifier],[Selected_Component]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Procedure_Renaming_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Procedure_Token            : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Renames_Token              : Token?
 - Renamed_Entity             : _[Expression]_? {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Function_Renaming_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Renames_Token              : Token?
 - Renamed_Entity             : _[Expression]_? {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Package_Renaming_Declaration
> _[Declaration]_
 - Package_Token              : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {[Identifier],[Selected_Component]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Generic_Package_Renaming_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Generic_Formal_Part        : _[Element]_*
 - Package_Token              : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {[Identifier],[Selected_Component]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Generic_Procedure_Renaming_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Generic_Formal_Part        : _[Element]_*
 - Procedure_Token            : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {[Identifier],[Selected_Component]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Generic_Function_Renaming_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Generic_Formal_Part        : _[Element]_*
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Renames_Token              : Token
 - Renamed_Entity             : _[Expression]_ {[Identifier],[Selected_Component],[Operator_Symbol]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Task_Body_Declaration
> _[Declaration]_
 - Task_Token                 : Token
 - Body_Token                 : Token
 - Name                       : [Defining_Identifier]
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Declarative_Part           : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                : Token
 - Statements                 : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token            : Token?
 - Exception_Handlers         : [Exception_Handler]*
 - End_Token                  : Token
 - End_Name                   : [Identifier]?
 - Semicolon_Token            : Token

### Protected_Body_Declaration
> _[Declaration]_
 - Protected_Token            : Token
 - Body_Token                 : Token
 - Name                       : [Defining_Identifier]
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Protected_Operation_Items  : _[Element]_+ {[Pragma],_[Declaration]_,_[Clause]_}
 - End_Token                  : Token
 - End_Name                   : [Identifier]?
 - Semicolon_Token            : Token

### Entry_Declaration
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Entry_Token                : Token
 - Name                       : [Defining_Identifier]
 - Left_Bracket_Token         : Token?
 - Entry_Family_Definition    : _[Discrete_Subtype_Definition]_?
 - Right_Bracket_Token        : Token?
 - Left_Bracket_Token_2       : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token_2      : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Entry_Body_Declaration
> _[Declaration]_
 - Entry_Token                : Token
 - Name                       : [Defining_Identifier]
 - Left_Bracket_Token         : Token?
 - Entry_Index_Specification  : [Entry_Index_Specification]
 - Right_Bracket_Token        : Token?
 - Left_Bracket_Token_2       : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token_2      : Token?
 - When_Token                 : Token
 - Entry_Barrier              : _[Expression]_ {^[Box_Expression]}
 - Is_Token                   : Token
 - Declarative_Part           : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                : Token
 - Statements                 : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token            : Token?
 - Exception_Handlers         : [Exception_Handler]*
 - End_Token                  : Token
 - End_Name                   : [Identifier]?
 - Semicolon_Token            : Token

### Entry_Index_Specification
> _[Declaration]_
 - For_Token                  : Token
 - Name                       : [Defining_Identifier]
 - In_Token                   : Token
 - Subtype_Definition         : _[Discrete_Subtype_Definition]_

### Procedure_Body_Stub
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Procedure_Token            : Token
 - Name                       : [Defining_Identifier]
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Is_Token                   : Token?
 - Separate_Token             : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Function_Body_Stub
> _[Declaration]_
 - Not_Token                  : Token?
 - Overriding_Token           : Token?
 - Function_Token             : Token
 - Name                       : [Defining_Identifier]
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Is_Token                   : Token?
 - Separate_Token             : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Package_Body_Stub
> _[Declaration]_
 - Package_Token              : Token
 - Body_Token                 : Token
 - Name                       : [Defining_Identifier]
 - Is_Token                   : Token
 - Separate_Token             : Token
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Task_Body_Stub
> _[Declaration]_
 - Task_Token                 : Token
 - Body_Token                 : Token
 - Name                       : [Defining_Identifier]
 - Is_Token                   : Token
 - Separate_Token             : Token
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Protected_Body_Stub
> _[Declaration]_
 - Protected_Token            : Token
 - Body_Token                 : Token
 - Name                       : [Defining_Identifier]
 - Is_Token                   : Token
 - Separate_Token             : Token
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Exception_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - Exception_Token            : Token
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Choice_Parameter_Specification
> _[Declaration]_
 - Name                       : [Defining_Identifier]

### Generic_Package_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Formal_Part                : _[Element]_* {[Pragma],[Use_Clause],[Formal_Object_Declaration],[Formal_Type_Declaration],[Formal_Procedure_Declaration],[Formal_Function_Declaration],[Formal_Package_Declaration]}
 - Package_Token              : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Is_Token                   : Token
 - Visible_Part               : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Private_Token              : Token?
 - Private_Part               : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - End_Token                  : Token
 - End_Name                   : _[Expression]_? {[Identifier],[Selected_Component]}
 - Semicolon_Token            : Token

### Generic_Procedure_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Formal_Part                : _[Element]_* {[Pragma],[Use_Clause],[Formal_Object_Declaration],[Formal_Type_Declaration],[Formal_Procedure_Declaration],[Formal_Function_Declaration],[Formal_Package_Declaration]}
 - Procedure_Token            : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Generic_Function_Declaration
> _[Declaration]_
 - Generic_Token              : Token
 - Formal_Part                : _[Element]_* {[Pragma],[Use_Clause],[Formal_Object_Declaration],[Formal_Type_Declaration],[Formal_Procedure_Declaration],[Formal_Function_Declaration],[Formal_Package_Declaration]}
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Package_Instantiation
> _[Declaration]_
 - Package_Token              : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Is_Token                   : Token
 - New_Token                  : Token
 - Generic_Package_Name       : _[Expression]_ {[Identifier],[Selected_Component]}
 - Left_Bracket_Token         : Token?
 - Generic_Actual_Part        : [Parameter_Association]*
 - Right_Bracket_Token        : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Procedure_Instantiation
> _[Declaration]_
 - Not_Token                      : Token?
 - Overriding_Token               : Token?
 - Procedure_Token                : Token
 - Name                           : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name]}
 - Is_Token                       : Token
 - New_Token                      : Token
 - Generic_Procedure_Name         : _[Expression]_ {[Identifier],[Selected_Component]}
 - Left_Bracket_Token             : Token?
 - Generic_Actual_Part            : [Parameter_Association]*
 - Right_Bracket_Token            : Token?
 - With_Token                     : Token?
 - Aspects                        : [Aspect_Specification]*
 - Semicolon_Token                : Token

### Function_Instantiation
> _[Declaration]_
 - Not_Token                      : Token?
 - Overriding_Token               : Token?
 - Function_Token                 : Token
 - Name                           : _[Defining_Name]_ {[Defining_Identifier],[Defining_Expanded_Name],[Defining_Operator_Symbol]}
 - Is_Token                       : Token
 - New_Token                      : Token
 - Generic_Function_Name          : _[Expression]_ {[Identifier],[Selected_Component]}
 - Left_Bracket_Token             : Token?
 - Generic_Actual_Part            : [Parameter_Association]*
 - Right_Bracket_Token            : Token?
 - With_Token                     : Token?
 - Aspects                        : [Aspect_Specification]*
 - Semicolon_Token                : Token

### Formal_Object_Declaration
> _[Declaration]_
 - Names                      : [Defining_Identifier]+
 - Colon_Token                : Token
 - In_Token                   : Token?
 - Out_Token                  : Token?
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Object_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Assignment_Token           : Token?
 - Initialization_Expression  : _[Expression]_? {^[Box_Expression]}
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Formal_Type_Declaration
> _[Declaration]_
 - Type_Token            : Token
 - Name                  : [Defining_Identifier]
 - Discriminant_Part     : _[Definition]_? {[Known_Discriminant_Part],[Unknown_Discriminant_Part]}
 - Is_Token              : Token
 - View                  : _[Formal_Type_Definition]_
 - With_Token            : Token?
 - Aspects               : [Aspect_Specification]*
 - Semicolon_Token       : Token

### Formal_Procedure_Declaration
> _[Declaration]_
 - With_Token                 : Token
 - Procedure_Token            : Token
 - Name                       : [Defining_Identifier]
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Is_Token                   : Token?
 - Abstract_Token             : Token?
 - Null_Token                 : Token?
 - Formal_Subprogram_Default  : _[Expression]_?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Formal_Function_Declaration
> _[Declaration]_
 - With_Token                 : Token
 - Function_Token             : Token
 - Name                       : _[Defining_Name]_ {[Defining_Identifier],[Defining_Operator_Symbol]}
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}
 - Is_Token                   : Token?
 - Abstract_Token             : Token?
 - Formal_Subprogram_Default  : _[Expression]_?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

### Formal_Package_Declaration
> _[Declaration]_
 - With_Token                 : Token
 - Package_Token              : Token
 - Name                       : [Defining_Identifier]
 - Is_Token                   : Token
 - New_Token                  : Token
 - Generic_Package_Name       : _[Expression]_
 - Left_Bracket_Token         : Token?
 - Generic_Actual_Part        : [Parameter_Association]*
 - Right_Bracket_Token        : Token?
 - With_Token                 : Token?
 - Aspects                    : [Aspect_Specification]*
 - Semicolon_Token            : Token

## Definition
> _[Element]_

### _Type_Definition_
> _[Definition]_

### Subtype_Indication
> _[Definition]_,_[Discrete_Subtype_Definition]_,_[Discrete_Range]_
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Subtype_Mark               : _[Expression]_ {[Identifier],[Selected_Component],[Attribute_Reference]}
 - Subtype_Constraint         : _[Constraint]_?

### _Constraint_
> _[Definition]_

### Component_Definition
> _[Definition]_
 - Aliased_Token              : Token?
 - Subtype_Indication         : _[Element]_ {[Subtype_Indication],_[Anonymous_Access_Definition]_}

### _Discrete_Subtype_Definition_
> _[Definition]_

### _Discrete_Range_
> _[Definition]_

### Unknown_Discriminant_Part
> _[Definition]_
 - Left_Bracket_Token   : Token
 - Box_Token            : Token
 - Right_Bracket_Token  : Token

### Known_Discriminant_Part
> _[Definition]_
 - Left_Bracket_Token   : Token
 - Discriminants        : [Discriminant_Specification]*
 - Right_Bracket_Token  : Token

### Record_Definition
> _[Definition]_
 - Record_Token      : Token
 - Components        : _[Element]_+ {[Pragma],[Component_Declaration],[Null_Component],[Variant_Part],[Attribute_Definition_Clause]}
 - End_Token         : Token
 - Record_Token_2    : Token

### Null_Component
> _[Definition]_
 - Null_Token                 : Token
 - Semicolon_Token            : Token

### Variant_Part
> _[Definition]_
 - Case_Token               : Token
 - Discriminant_Direct_Name : [Identifier]
 - Is_Token                 : Token
 - Variants                 : [Variant]+
 - End_Token                : Token
 - Case_Token_2             : Token
 - Semicolon_Token          : Token

### Variant
> _[Definition]_
 - When_Token               : Token
 - Choices                  : _[Element]_+ {_[Expression]_,[Discrete_Range],[Others_Choice]}
 - Arrow_Token              : Token
 - Components               : _[Element]_+ {[Pragma],[Component_Declaration],[Null_Component],[Variant_Part],[Attribute_Definition_Clause]}

### Others_Choice
> _[Definition]_
 - Others_Token             : Token

### _Anonymous_Access_Definition_
> _[Definition]_

### Private_Type_Definition
> _[Definition]_
 - Abstract_Token             : Token?
 - Tagged_Token               : Token?
 - Limited_Token              : Token?
 - Private_Token              : Token

### Private_Extension_Definition
> _[Definition]_
 - Abstract_Token              : Token?
 - Limited_Token               : Token?
 - Synchronized_Token          : Token?
 - New_Token                   : Token
 - Ancestor_Subtype_Indication : [Subtype_Indication]
 - And_Token                   : Token?
 - Progenitors                 : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token                  : Token
 - Private_Token               : Token

### Incomplete_Type_Definition
> _[Definition]_
 - Tagged_Token                : Token?

### Task_Definition
> _[Definition]_
 - Visible_Part       : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Private_Token      : Token
 - Private_Part       : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - End_Token          : Token
 - End_Name           : [Identifier]?

### Protected_Definition
> _[Definition]_
 - Visible_Part            : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Private_Token           : Token
 - Private_Part            : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - End_Token               : Token
 - End_Name                : [Identifier]?

### _Formal_Type_Definition_
> _[Definition]_

### Aspect_Specification
> _[Definition]_
 - Aspect_Mark        : _[Expression]_ {[Identifier],[Attribute_Reference]}
 - Arrow_Token        : Token
 - Aspect_Definition  : _[Expression]_ {^[Box_Expression]}

## _Expression_
> _[Element]_

### Box_Expression
> _[Expression]_
 - Box_Token          : Token

### Numeric_Literal
> _[Expression]_
 - Numeric_Literal_Token : Token

### String_Literal
> _[Expression]_
 - String_Literal_Token : Token

### Identifier
> _[Expression]_
 - Identifier_Token : Token

### Operator_Symbol
> _[Expression]_
 - Operator_Symbol_Token : Token

### Character_Literal
> _[Expression]_
 - Character_Literal_Token : Token

### Explicit_Dereference
> _[Expression]_
 - Prefix     : _[Expression]_ {^[Box_Expression]}
 - Dot_Token  : Token
 - All_Token  : Token

### Function_Call
> _[Expression]_
 - Prefix                    : _[Expression]_ {^[Box_Expression]}
 - Left_Bracket_Token        : Token?
 - Function_Call_Parameters  : [Parameter_Association]*
 - Right_Bracket_Token       : Token?

### Indexed_Component
> _[Expression]_
 - Prefix                    : _[Expression]_ {^[Box_Expression]}
 - Left_Bracket_Token        : Token
 - Index_Expressions         : [Parameter_Association]*
 - Right_Bracket_Token       : Token

### Slice
> _[Expression]_
 - Prefix                    : _[Expression]_ {^[Box_Expression]}
 - Left_Bracket_Token        : Token
 - Slice_Range               : _[Discrete_Range]_
 - Right_Bracket_Token       : Token

### Selected_Component
> _[Expression]_
 - Prefix                    : _[Expression]_ {^[Box_Expression]}
 - Dot_Token                 : Token
 - Selector                  : _[Expression]_ {[Identifier],[Character_Literal],[Operator_Symbol]}

### Attribute_Reference
> _[Expression]_
 - Prefix                    : _[Expression]_ {^[Box_Expression]}
 - Apostrophe_Token          : Token
 - Attribute_Designator      : [Identifier]
 - Left_Bracket_Token        : Token
 - Static_Expression         : _[Expression]_? {^[Box_Expression]}
 - Right_Bracket_Token       : Token

### Record_Aggregate
> _[Expression]_
 - Left_Bracket_Token             : Token
 - Component_Associations         : [Record_Component_Association]*
 - Right_Bracket_Token            : Token

### Extension_Aggregate
> _[Expression]_
 - Left_Bracket_Token             : Token
 - Ancestor_Part                  : _[Expression]_ {^[Box_Expression]}
 - With_Token                     : Token
 - Component_Associations         : [Record_Component_Association]*
 - Right_Bracket_Token            : Token

### Array_Aggregate
> _[Expression]_
 - Left_Bracket_Token             : Token
 - Component_Associations         : [Array_Component_Association]*
 - Right_Bracket_Token            : Token

### Short_Circuit_Operation
> _[Expression]_
 - Left_Expression  : _[Expression]_ {^[Box_Expression]}
 - And_Token        : Token?
 - Then_Token       : Token?
 - Or_Token         : Token?
 - Else_Token       : Token?
 - Right_Expression : _[Expression]_ {^[Box_Expression]}

### Membership_Test
> _[Expression]_
 - Membership_Test_Expression : _[Expression]_ {^[Box_Expression]}
 - Not_Token                  : Token?
 - In_Token                   : Token
 - Membership_Test_Choices    : _[Element]_+ {[Simple_Expression_Range],[Attribute_Reference],^[Membership_Test],^[Box_Expression]}

### Null_Literal
> _[Expression]_
 - Null_Literal_Token : Token

### Parenthesized_Expression
> _[Expression]_
 - Left_Bracket_Token         : Token
 - Expression_Parenthesized   : _[Expression]_ {^[Box_Expression]}
 - Right_Bracket_Token        : Token

### Raise_Expression
> _[Expression]_
 - Raise_Token             : Token
 - Exception_Name          : _[Expression]_ {^[Box_Expression]}
 - With_Token              : Token?
 - Associated_Message      : _[Expression]_? {^[Box_Expression]}

### Type_Conversion
> _[Expression]_
 - Converted_Subtype_Mark  : _[Expression]_ {[Identifier],[Selected_Component],[Attribute_Reference]}
 - Left_Bracket_Token      : Token
 - Converted_Expression    : _[Expression]_ {^[Box_Expression]}
 - Right_Bracket_Token     : Token

### Qualified_Expression
> _[Expression]_
 - Qualified_Subtype_Mark  : _[Expression]_ {[Identifier],[Selected_Component],[Attribute_Reference]}
 - Apostrophe_Token        : Token
 - Left_Bracket_Token      : Token
 - Qualified_Expression    : _[Expression]_ {^[Box_Expression]}
 - Right_Bracket_Token     : Token

### Allocator
> _[Expression]_
 - New_Token               : Token
 - Left_Bracket_Token      : Token?
 - Subpool_Name            : _[Expression]_? {^[Box_Expression]}
 - Right_Bracket_Token     : Token?
 - Subtype_Indication      : [Subtype_Indication]?
 - Qualified_Expression    : [Qualified_Expression]?

### Case_Expression
> _[Expression]_
 - Case_Token                 : Token
 - Case_Expression            : _[Expression]_ {^[Box_Expression]}
 - Is_Token                   : Token
 - Case_Expression_Paths      : [Case_Expression_Path]+

### If_Expression
> _[Expression]_
 - If_Token                   : Token
 - Condition_Expression       : _[Expression]_ {^[Box_Expression]}
 - Then_Token                 : Token
 - Then_Expression            : _[Expression]_ {^[Box_Expression]}
 - Elsif_Paths                : [Elsif_Path]*
 - Else_Token                 : Token?
 - Else_Expression            : _[Expression]_? {^[Box_Expression]}

### Quantified_Expression
> _[Expression]_
 - For_Token                    : Token
 - All_Token                    : Token?
 - Some_Token                   : Token?
 - Loop_Parameter_Specification : [Loop_Parameter_Specification]?
 - Generalized_Iterator         : [Generalized_Iterator_Specification]?
 - Element_Iterator             : [Element_Iterator_Specification]?
 - Arrow_Token                  : Token
 - Predicate                    : _[Expression]_ {^[Box_Expression]}

## Association
> _[Element]_

### Discriminant_Association
> _[Association]_
 - Selector_Names              : [Identifier]*
 - Arrow_Token                 : Token?
 - Discriminant_Expression     : _[Expression]_ {^[Box_Expression]}

### Record_Component_Association
> _[Association]_
 - Choices                     : _[Element]_* {[Identifier],[Others_Choice]}
 - Arrow_Token                 : Token?
 - Component_Expression        : _[Expression]_?

### Array_Component_Association
> _[Association]_
 - Choices                     : _[Element]_* {_[Expression]_,[Subtype_Indication],_[Discrete_Range]_,[Others_Choice]}
 - Arrow_Token                 : Token?
 - Component_Expression        : _[Expression]_?

### Parameter_Association
> _[Association]_
 - Formal_Parameter : _[Expression]_? {[Identifier],[Operator_Symbol]}
 - Arrow_Token      : Token?
 - Actual_Parameter : _[Expression]_ {^[Box_Expression]}

## _Statement_
> _[Element]_

### Null_Statement
> _[Statement]_
 - Null_Token                 : Token
 - Semicolon_Token            : Token

### Assignment_Statement
> _[Statement]_
 - Variable_Name              : _[Expression]_ {^[Box_Expression]}
 - Assignment_Token           : Token
 - Expression                 : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token            : Token

### If_Statement
> _[Statement]_
 - If_Token                   : Token
 - Condition_Expression       : _[Expression]_ {^[Box_Expression]}
 - Then_Token                 : Token
 - Then_Statements            : _[Element]_+ {[Pragma],_[Statement]_}
 - Elsif_Paths                : [Elsif_Path]*
 - Else_Token                 : Token?
 - Else_Statements            : _[Element]_* {[Pragma],_[Statement]_}
 - End_Token                  : Token
 - If_Token_2                 : Token
 - Semicolon_Token            : Token

### Case_Statement
> _[Statement]_
 - Case_Token                 : Token
 - Case_Expression            : _[Expression]_ {^[Box_Expression]}
 - Is_Token                   : Token
 - Case_Statement_Paths       : [Case_Path]+
 - End_Token                  : Token
 - Case_Token_2               : Token
 - Semicolon_Token            : Token

### Loop_Statement
> _[Statement]_
 - Statement_Identifier       : [Defining_Identifier]?
 - Colon_Token                : Token?
 - Loop_Token                 : Token
 - Loop_Statements            : _[Element]_+ {[Pragma],_[Statement]_}
 - End_Token                  : Token
 - Loop_Token_2               : Token
 - Statement_Identifier       : [Identifier]?
 - Semicolon_Token            : Token

### While_Loop_Statement
> _[Statement]_
 - Statement_Identifier       : [Defining_Identifier]?
 - Colon_Token                : Token?
 - While_Token                : Token
 - While_Condition            : _[Expression]_ {^[Box_Expression]}
 - Loop_Token                 : Token
 - Loop_Statements            : _[Element]_+ {[Pragma],_[Statement]_}
 - End_Token                  : Token
 - Loop_Token_2               : Token
 - Statement_Identifier       : [Identifier]?
 - Semicolon_Token            : Token

### For_Loop_Statement
> _[Statement]_
 - Statement_Identifier         : [Defining_Identifier]?
 - Colon_Token                  : Token?
 - For_Token                    : Token
 - Loop_Parameter_Specification : [Loop_Parameter_Specification]?
 - Generalized_Iterator         : [Generalized_Iterator_Specification]?
 - Element_Iterator             : [Element_Iterator_Specification]?
 - Loop_Token                   : Token
 - Loop_Statements              : _[Element]_+ {[Pragma],_[Statement]_}
 - End_Token                    : Token
 - Loop_Token_2                 : Token
 - Statement_Identifier         : [Identifier]?
 - Semicolon_Token              : Token

### Block_Statement
> _[Statement]_
 - Statement_Identifier         : [Defining_Identifier]?
 - Colon_Token                  : Token?
 - Declare_Token                : Token?
 - Declarative_Part             : _[Element]_* {[Pragma],_[Declaration]_,_[Clause]_}
 - Begin_Token                  : Token
 - Statements                   : _[Element]_+ {[Pragma],_[Statement]_}
 - Exception_Token              : Token?
 - Exception_Handlers           : [Exception_Handler]*
 - End_Token                    : Token
 - Statement_Identifier         : [Identifier]?
 - Semicolon_Token              : Token

### Exit_Statement
> _[Statement]_
 - Exit_Token                   : Token
 - Exit_Loop_Name               : _[Expression]_? {^[Box_Expression]}
 - When_Token                   : Token?
 - Exit_Condition               : _[Expression]_? {^[Box_Expression]}
 - Semicolon_Token              : Token

### Goto_Statement
> _[Statement]_
 - Goto_Token                   : Token
 - Goto_Label                   : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token              : Token

### Call_Statement
> _[Statement]_
 - Called_Name                  : _[Expression]_ {^[Box_Expression]}
 - Left_Bracket_Token           : Token?
 - Call_Statement_Parameters    : [Parameter_Association]*
 - Right_Bracket_Token          : Token?
 - Semicolon_Token              : Token

### Simple_Return_Statement
> _[Statement]_
 - Return_Token                 : Token
 - Return_Expression            : _[Expression]_? {^[Box_Expression]}
 - Semicolon_Token              : Token

### Extended_Return_Statement
> _[Statement]_
 - Return_Token                 : Token
 - Return_Object_Specification  : [Return_Object_Specification]
 - Do_Token                     : Token?
 - Extended_Return_Statements   : _[Element]_* {[Pragma],_[Statement]_}
 - Exception_Token              : Token?
 - Exception_Handlers           : [Exception_Handler]*
 - End_Token                    : Token?
 - Return_Token_2               : Token?
 - Semicolon_Token              : Token

### Accept_Statement
> _[Statement]_
 - Accept_Token                 : Token
 - Accept_Entry_Direct_Name     : [Identifier]
 - Left_Bracket_Token           : Token?
 - Accept_Entry_Index           : _[Expression]_? {^[Box_Expression]}
 - Right_Bracket_Token          : Token?
 - Left_Bracket_Token_2         : Token?
 - Accept_Parameters            : [Parameter_Specification]*
 - Right_Bracket_Token_2        : Token?
 - Do_Token                     : Token?
 - Accept_Body_Statements       : _[Element]_* {[Pragma],_[Statement]_}
 - Exception_Token              : Token?
 - Exception_Handlers           : [Exception_Handler]*
 - End_Token                    : Token?
 - End_Name                     : [Identifier]?
 - Semicolon_Token              : Token

### Requeue_Statement
> _[Statement]_
 - Requeue_Token                : Token
 - Requeue_Entry_Name           : _[Expression]_ {^[Box_Expression]}
 - With_Token                   : Token?
 - Abort_Token                  : Token?
 - Semicolon_Token              : Token

### Delay_Statement
> _[Statement]_
 - Delay_Token                  : Token
 - Until_Token                  : Token
 - Delay_Expression             : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token              : Token

### Terminate_Alternative_Statement
> _[Statement]_
 - Terminate_Token              : Token
 - Semicolon_Token              : Token

### Select_Statement
> _[Statement]_
 - Select_Token                 : Token
 - Select_Statement_Paths       : _[Path]_+
 - End_Token                    : Token?
 - Select_Token_2               : Token
 - Semicolon_Token              : Token

### Abort_Statement
> _[Statement]_
 - Abort_Token                  : Token
 - Aborted_Tasks                : _[Expression]_+ {^[Box_Expression]}
 - Semicolon_Token              : Token

### Raise_Statement
> _[Statement]_
 - Raise_Token                  : Token
 - Raised_Exception             : _[Expression]_? {^[Box_Expression]}
 - With_Token                   : Token?
 - Associated_Message           : _[Expression]_? {^[Box_Expression]}
 - Semicolon_Token              : Token

### Code_Statement
> _[Statement]_
 - Qualified_Expression         : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token              : Token

## Path
> _[Element]_

### Elsif_Path
> _[Path]_
 - Elsif_Token                : Token
 - Condition_Expression       : _[Expression]_ {^[Box_Expression]}
 - Then_Token                 : Token
 - Then_Statements            : _[Element]_+ {[Pragma],_[Statement]_}

### Case_Path
> _[Path]_
 - When_Token                 : Token
 - Variant_Choices            : _[Element]_+
 - Arrow_Token                : Token?
 - Sequence_Of_Statements     : _[Element]_+ {[Pragma],_[Statement]_}

### Select_Path
> _[Path]_
 - When_Token                 : Token
 - Guard                      : _[Expression]_? {^[Box_Expression]}
 - Arrow_Token                : Token?
 - Sequence_Of_Statements     : _[Element]_+ {[Pragma],_[Statement]_}

### Then_Abort_Path
> _[Path]_
 - Then_Token                 : Token
 - Abort_Token                : Token
 - Sequence_Of_Statements     : _[Element]_+ {[Pragma],_[Statement]_}

### Case_Expression_Path
> _[Path]_
 - When_Token                 : Token
 - Variant_Choices            : _[Element]_+
 - Arrow_Token                : Token?
 - Case_Expression            : _[Expression]_ {^[Box_Expression]}

### Elsif_Expression_Path
> _[Path]_
 - Elsif_Token                : Token
 - Condition_Expression       : _[Expression]_ {^[Box_Expression]}
 - Then_Token                 : Token
 - Then_Expression            : _[Expression]_ {^[Box_Expression]}

## Clause
> _[Element]_

### Use_Clause
> _[Clause]_
 - Use_Token                  : Token
 - All_Token                  : Token?
 - Type_Token                 : Token?
 - Clause_Names               : _[Expression]_+ {^[Box_Expression]}
 - Semicolon_Token            : Token

### With_Clause
> _[Clause]_
 - Limited_Token              : Token?
 - Private_Token              : Token?
 - With_Token                 : Token?
 - Clause_Names               : _[Expression]_+ {^[Box_Expression]}
 - Semicolon_Token            : Token

### _Representation_Clause_
> _[Clause]_

### Component_Clause
> _[Clause]_
 - Representation_Clause_Name : [Identifier]
 - At_Token                   : Token
 - Component_Clause_Position  : _[Expression]_ {^[Box_Expression]}
 - Range_Token                : Token
 - Component_Clause_Range     : [Simple_Expression_Range]
 - Semicolon_Token            : Token

### Derived_Type
> _[Type_Definition]_
 - Abstract_Token             : Token?
 - Limited_Token              : Token?
 - New_Token                  : Token
 - Parent_Subtype_Indication  : _[Expression]_ {^[Box_Expression]}

### Derived_Record_Extension
> _[Type_Definition]_
 - Abstract_Token             : Token?
 - Limited_Token              : Token?
 - New_Token                  : Token
 - Parent_Subtype_Indication  : _[Expression]_ {^[Box_Expression]}
 - And_Token                  : Token?
 - Progenitors                : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token                 : Token
 - Record_Definition          : _[Definition]_

### Enumeration_Type
> _[Type_Definition]_
 - Left_Bracket_Token         : Token
 - Literal_Specifications     : [Enumeration_Literal_Specification]+
 - Right_Bracket_Token        : Token

### Signed_Integer_Type
> _[Type_Definition]_
 - Range_Token                   : Token
 - Integer_Constraint            : [Simple_Expression_Range]

### Modular_Type
> _[Type_Definition]_
 - Mod_Token                     : Token
 - Mod_Static_Expression         : _[Expression]_ {^[Box_Expression]}

### Root_Type
> _[Type_Definition]_

### Floating_Point_Type
> _[Type_Definition]_
 - Digits_Token                  : Token
 - Digits_Expression             : _[Expression]_ {^[Box_Expression]}
 - Real_Range_Constraint         : [Simple_Expression_Range]?

### Ordinary_Fixed_Point_Type
> _[Type_Definition]_
 - Delta_Token                   : Token
 - Delta_Expression              : _[Expression]_ {^[Box_Expression]}
 - Real_Range_Constraint         : [Simple_Expression_Range]

### Decimal_Fixed_Point_Type
> _[Type_Definition]_
 - Delta_Token                   : Token
 - Delta_Expression              : _[Expression]_ {^[Box_Expression]}
 - Digits_Token                  : Token
 - Digits_Expression             : _[Expression]_ {^[Box_Expression]}
 - Real_Range_Constraint         : [Simple_Expression_Range]?

### Unconstrained_Array_Type
> _[Type_Definition]_,_[Formal_Type_Definition]_
 - Array_Token                   : Token
 - Left_Bracket_Token            : Token
 - Index_Subtypes                : _[Expression]_+ {^[Box_Expression]}
 - Right_Bracket_Token           : Token?
 - Of_Token                      : Token?
 - Component_Definition          : [Component_Definition]

### Constrained_Array_Type
> _[Type_Definition]_,_[Formal_Type_Definition]_
 - Array_Token                   : Token
 - Left_Bracket_Token            : Token
 - Index_Subtypes                : _[Discrete_Subtype_Definition]_+
 - Right_Bracket_Token           : Token?
 - Of_Token                      : Token?
 - Component_Definition          : [Component_Definition]

### Record_Type
> _[Type_Definition]_
 - Abstract_Token                : Token
 - Tagged_Token                  : Token
 - Limited_Token                 : Token
 - Record_Definition             : _[Definition]_

### Interface_Type
> _[Type_Definition]_,_[Formal_Type_Definition]_
 - Limited_Token                 : Token?
 - Task_Token                    : Token?
 - Protected_Token               : Token?
 - Synchronized_Token            : Token?
 - Interface_Token               : Token?
 - And_Token                     : Token?
 - Progenitors                   : _[Expression]_* {[Identifier],[Selected_Component]}

### Object_Access_Type
> _[Access_Type]_,_[Formal_Access_Type]_,_[Anonymous_Access_Definition]_
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - All_Token                  : Token?
 - Constant_Token             : Token?
 - Subtype_Indication         : [Subtype_Indication]

### Procedure_Access_Type
> _[Access_Type]_,_[Formal_Access_Type]_,_[Anonymous_Access_Definition]_
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Access_Token               : Token
 - Protected_Token            : Token?
 - Procedure_Token            : Token
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?
 - Return_Token               : Token
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Result_Subtype             : _[Element]_ {[Identifier],[Selected_Component],[Attribute_Reference],_[Anonymous_Access_Definition]_}

### Function_Access_Type
> _[Access_Type]_,_[Formal_Access_Type]_,_[Anonymous_Access_Definition]_
 - Not_Token                  : Token?
 - Null_Token                 : Token?
 - Access_Token               : Token
 - Protected_Token            : Token?
 - Function_Token             : Token
 - Left_Bracket_Token         : Token?
 - Parameters                 : [Parameter_Specification]*
 - Right_Bracket_Token        : Token?

### Formal_Private_Type_Definition
> _[Formal_Type_Definition]_
 - Abstract_Token             : Token?
 - Tagged_Token               : Token?
 - Limited_Token              : Token?
 - Private_Token              : Token

### Formal_Derived_Type_Definition
> _[Formal_Type_Definition]_
 - Limited_Token              : Token?
 - Synchronized_Token         : Token?
 - New_Token                  : Token
 - Private_Token              : Token
 - Subtype_Mark               : _[Expression]_ {^[Box_Expression]}
 - And_Token                  : Token?
 - Progenitors                : _[Expression]_* {[Identifier],[Selected_Component]}
 - With_Token                 : Token?
 - Private_Token              : Token?

### Formal_Discrete_Type_Definition
> _[Formal_Type_Definition]_
 - Left_Bracket_Token         : Token
 - Box_Token                  : Token
 - Right_Bracket_Token        : Token

### Formal_Signed_Integer_Type_Definition
> _[Formal_Type_Definition]_
 - Range_Token                : Token
 - Box_Token                  : Token

### Formal_Modular_Type_Definition
> _[Formal_Type_Definition]_
 - Mod_Token                  : Token
 - Box_Token                  : Token

### Formal_Floating_Point_Definition
> _[Formal_Type_Definition]_
 - Digits_Token               : Token
 - Box_Token                  : Token

### Formal_Ordinary_Fixed_Point_Definition
> _[Formal_Type_Definition]_
 - Delta_Token                : Token
 - Box_Token                  : Token

### Formal_Decimal_Fixed_Point_Definition
> _[Formal_Type_Definition]_
 - Delta_Token                : Token
 - Box_Token                  : Token
 - Digits_Token               : Token
 - Box_Token_2                : Token

## _Formal_Access_Type_
> _[Formal_Type_Definition]_

## _Access_Type_
> _[Type_Definition]_

### Range_Attribute_Reference
> _[Constraint]_,_[Discrete_Subtype_Definition]_,_[Discrete_Range]_
 - Range_Attribute   : [Attribute_Reference]

### Simple_Expression_Range
> _[Constraint]_,_[Discrete_Subtype_Definition]_,_[Discrete_Range]_
 - Lower_Bound       : _[Expression]_ {^[Box_Expression]}
 - Double_Dot_Token  : Token
 - Upper_Bound       : _[Expression]_ {^[Box_Expression]}

### Digits_Constraint
> _[Constraint]_
 - Digits_Token            : Token
 - Digits_Expression       : _[Expression]_ {^[Box_Expression]}
 - Range_Token             : Token_
 - Real_Range_Constraint   : _[Constraint]_?

### Delta_Constraint
> _[Constraint]_
 - Delta_Token             : Token
 - Delta_Expression        : _[Expression]_ {^[Box_Expression]}
 - Range_Token             : Token_
 - Real_Range_Constraint   : _[Constraint]_?

### Index_Constraint
> _[Constraint]_
 - Left_Bracket_Token        : Token
 - Discrete_Ranges           : [Discrete_Range]+
 - Right_Bracket_Token       : Token

### Discriminant_Constraint
> _[Constraint]_
 - Left_Bracket_Token        : Token?
 - Discriminant_Associations : [Discriminant_Association]+
 - Right_Bracket_Token       : Token?

### Attribute_Definition_Clause
> _[Representation_Clause]_
 - For_Token                        : Token
 - Name                             : _[Expression]_ {^[Box_Expression]}
 - Use_Token                        : Token
 - Expression                       : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token                  : Token

### Enumeration_Representation_Clause
> _[Representation_Clause]_
 - For_Token                        : Token
 - Name                             : _[Expression]_ {^[Box_Expression]}
 - Use_Token                        : Token
 - Expression                       : [Array_Aggregate]
 - Semicolon_Token                  : Token

### Record_Representation_Clause
> _[Representation_Clause]_
 - For_Token                        : Token
 - Name                             : _[Expression]_ {^[Box_Expression]}
 - Use_Token                        : Token
 - Record_Token                     : Token
 - At_Token                         : Token?
 - Mod_Token                        : Token?
 - Mod_Clause_Expression            : _[Expression]_? {^[Box_Expression]}
 - Mod_Semicolon_Token              : Token?
 - Component_Clauses                : [Component_Clause]+ 
 - Semicolon_Token                  : Token

### At_Clause
> _[Representation_Clause]_
 - For_Token                        : Token
 - Name                             : [Identifier]
 - Use_Token                        : Token
 - At_Token                         : Token
 - Expression                       : _[Expression]_ {^[Box_Expression]}
 - Semicolon_Token                  : Token

## Exception_Handler
> _[Element]_
 - When_Token                       : Token
 - Choice_Parameter                 : [Choice_Parameter_Specification]?
 - Colon_Token                      : Token?
 - Choices                          : _[Element]_+
 - Arrow_Token                      : Token
 - Statements                       : _[Element]_+ {[Pragma],_[Statement]_}

[Abort_Statement]: ast.md#Abort_Statement
[Accept_Statement]: ast.md#Accept_Statement
[Access_To_Function]: ast.md#Access_To_Function
[Access_To_Object]: ast.md#Access_To_Object
[Access_To_Procedure]: ast.md#Access_To_Procedure
[Access_Type]: ast.md#Access_Type
[Allocator]: ast.md#Allocator
[Anonymous_Access_Definition]: ast.md#Anonymous_Access_Definition
[Array_Aggregate]: ast.md#Array_Aggregate
[Array_Component_Association]: ast.md#Array_Component_Association
[Aspect_Specification]: ast.md#Aspect_Specification
[Assignment_Statement]: ast.md#Assignment_Statement
[Association]: ast.md#Association
[At_Clause]: ast.md#At_Clause
[Attribute_Definition_Clause]: ast.md#Attribute_Definition_Clause
[Attribute_Reference]: ast.md#Attribute_Reference
[Block_Statement]: ast.md#Block_Statement
[Box_Expression]: ast.md#Box_Expression
[Call_Statement]: ast.md#Call_Statement
[Case_Expression]: ast.md#Case_Expression
[Case_Expression_Path]: ast.md#Case_Expression_Path
[Case_Path]: ast.md#Case_Path
[Case_Statement]: ast.md#Case_Statement
[Character_Literal]: ast.md#Character_Literal
[Choice_Parameter_Specification]: ast.md#Choice_Parameter_Specification
[Clause]: ast.md#Clause
[Code_Statement]: ast.md#Code_Statement
[Component_Clause]: ast.md#Component_Clause
[Component_Declaration]: ast.md#Component_Declaration
[Component_Definition]: ast.md#Component_Definition
[Constrained_Array_Type]: ast.md#Constrained_Array_Type
[Constraint]: ast.md#Constraint
[Decimal_Fixed_Point_Type]: ast.md#Decimal_Fixed_Point_Type
[Declaration]: ast.md#Declaration
[Defining_Character_Literal]: ast.md#Defining_Character_Literal
[Defining_Expanded_Name]: ast.md#Defining_Expanded_Name
[Defining_Identifier]: ast.md#Defining_Identifier
[Defining_Name]: ast.md#Defining_Name
[Defining_Operator_Symbol]: ast.md#Defining_Operator_Symbol
[Definition]: ast.md#Definition
[Delay_Statement]: ast.md#Delay_Statement
[Delta_Constraint]: ast.md#Delta_Constraint
[Derived_Record_Extension]: ast.md#Derived_Record_Extension
[Derived_Type]: ast.md#Derived_Type
[Digits_Constraint]: ast.md#Digits_Constraint
[Discrete_Range]: ast.md#Discrete_Range
[Discrete_Subtype_Definition]: ast.md#Discrete_Subtype_Definition
[Discriminant_Association]: ast.md#Discriminant_Association
[Discriminant_Constraint]: ast.md#Discriminant_Constraint
[Discriminant_Specification]: ast.md#Discriminant_Specification
[Element_Iterator_Specification]: ast.md#Element_Iterator_Specification
[Elsif_Expression_Path]: ast.md#Elsif_Expression_Path
[Elsif_Path]: ast.md#Elsif_Path
[Entry_Body_Declaration]: ast.md#Entry_Body_Declaration
[Entry_Declaration]: ast.md#Entry_Declaration
[Entry_Index_Specification]: ast.md#Entry_Index_Specification
[Enumeration_Literal_Specification]: ast.md#Enumeration_Literal_Specification
[Enumeration_Representation_Clause]: ast.md#Enumeration_Representation_Clause
[Enumeration_Type]: ast.md#Enumeration_Type
[Exception_Declaration]: ast.md#Exception_Declaration
[Exception_Handler]: ast.md#Exception_Handler
[Exception_Renaming_Declaration]: ast.md#Exception_Renaming_Declaration
[Exit_Statement]: ast.md#Exit_Statement
[Explicit_Dereference]: ast.md#Explicit_Dereference
[Expression]: ast.md#Expression
[Extended_Return_Statement]: ast.md#Extended_Return_Statement
[Extension_Aggregate]: ast.md#Extension_Aggregate
[Floating_Point_Type]: ast.md#Floating_Point_Type
[For_Loop_Statement]: ast.md#For_Loop_Statement
[Formal_Access_Type]: ast.md#Formal_Access_Type
[Formal_Decimal_Fixed_Point_Definition]: ast.md#Formal_Decimal_Fixed_Point_Definition
[Formal_Derived_Type_Definition]: ast.md#Formal_Derived_Type_Definition
[Formal_Discrete_Type_Definition]: ast.md#Formal_Discrete_Type_Definition
[Formal_Floating_Point_Definition]: ast.md#Formal_Floating_Point_Definition
[Formal_Function_Declaration]: ast.md#Formal_Function_Declaration
[Formal_Modular_Type_Definition]: ast.md#Formal_Modular_Type_Definition
[Formal_Object_Declaration]: ast.md#Formal_Object_Declaration
[Formal_Ordinary_Fixed_Point_Definition]: ast.md#Formal_Ordinary_Fixed_Point_Definition
[Formal_Package_Declaration]: ast.md#Formal_Package_Declaration
[Formal_Private_Type_Definition]: ast.md#Formal_Private_Type_Definition
[Formal_Procedure_Declaration]: ast.md#Formal_Procedure_Declaration
[Formal_Signed_Integer_Type_Definition]: ast.md#Formal_Signed_Integer_Type_Definition
[Formal_Type_Declaration]: ast.md#Formal_Type_Declaration
[Formal_Type_Definition]: ast.md#Formal_Type_Definition
[Function_Access_Type]: ast.md#Function_Access_Type
[Function_Body_Declaration]: ast.md#Function_Body_Declaration
[Function_Call]: ast.md#Function_Call
[Function_Declaration]: ast.md#Function_Declaration
[Function_Instantiation]: ast.md#Function_Instantiation
[Generalized_Iterator_Specification]: ast.md#Generalized_Iterator_Specification
[Generic_Function_Declaration]: ast.md#Generic_Function_Declaration
[Generic_Function_Renaming_Declaration]: ast.md#Generic_Function_Renaming_Declaration
[Generic_Package_Declaration]: ast.md#Generic_Package_Declaration
[Generic_Package_Renaming_Declaration]: ast.md#Generic_Package_Renaming_Declaration
[Generic_Procedure_Declaration]: ast.md#Generic_Procedure_Declaration
[Generic_Procedure_Renaming_Declaration]: ast.md#Generic_Procedure_Renaming_Declaration
[Goto_Statement]: ast.md#Goto_Statement
[Identifier]: ast.md#Identifier
[If_Expression]: ast.md#If_Expression
[If_Statement]: ast.md#If_Statement
[Incomplete_Type_Definition]: ast.md#Incomplete_Type_Definition
[Index_Constraint]: ast.md#Index_Constraint
[Indexed_Component]: ast.md#Indexed_Component
[Interface_Type]: ast.md#Interface_Type
[Known_Discriminant_Part]: ast.md#Known_Discriminant_Part
[Loop_Parameter_Specification]: ast.md#Loop_Parameter_Specification
[Loop_Statement]: ast.md#Loop_Statement
[Membership_Test]: ast.md#Membership_Test
[Modular_Type]: ast.md#Modular_Type
[Null_Component]: ast.md#Null_Component
[Null_Literal]: ast.md#Null_Literal
[Null_Statement]: ast.md#Null_Statement
[Number_Declaration]: ast.md#Number_Declaration
[Numeric_Literal]: ast.md#Numeric_Literal
[Object_Access_Type]: ast.md#Object_Access_Type
[Object_Declaration]: ast.md#Object_Declaration
[Object_Renaming_Declaration]: ast.md#Object_Renaming_Declaration
[Operator_Symbol]: ast.md#Operator_Symbol
[Ordinary_Fixed_Point_Type]: ast.md#Ordinary_Fixed_Point_Type
[Others_Choice]: ast.md#Others_Choice
[Package_Body_Declaration]: ast.md#Package_Body_Declaration
[Package_Body_Stub]: ast.md#Package_Body_Stub
[Package_Declaration]: ast.md#Package_Declaration
[Package_Instantiation]: ast.md#Package_Instantiation
[Package_Renaming_Declaration]: ast.md#Package_Renaming_Declaration
[Parameter_Association]: ast.md#Parameter_Association
[Parameter_Specification]: ast.md#Parameter_Specification
[Parenthesized_Expression]: ast.md#Parenthesized_Expression
[Path]: ast.md#Path
[Pragma]: ast.md#Pragma
[Private_Extension_Definition]: ast.md#Private_Extension_Definition
[Private_Type_Definition]: ast.md#Private_Type_Definition
[Procedure_Access_Type]: ast.md#Procedure_Access_Type
[Procedure_Body_Declaration]: ast.md#Procedure_Body_Declaration
[Procedure_Declaration]: ast.md#Procedure_Declaration
[Procedure_Instantiation]: ast.md#Procedure_Instantiation
[Protected_Body_Declaration]: ast.md#Protected_Body_Declaration
[Protected_Body_Stub]: ast.md#Protected_Body_Stub
[Protected_Definition]: ast.md#Protected_Definition
[Protected_Type_Declaration]: ast.md#Protected_Type_Declaration
[Qualified_Expression]: ast.md#Qualified_Expression
[Quantified_Expression]: ast.md#Quantified_Expression
[Raise_Expression]: ast.md#Raise_Expression
[Raise_Statement]: ast.md#Raise_Statement
[Range_Attribute_Reference]: ast.md#Range_Attribute_Reference
[Record_Aggregate]: ast.md#Record_Aggregate
[Record_Component_Association]: ast.md#Record_Component_Association
[Record_Definition]: ast.md#Record_Definition
[Record_Representation_Clause]: ast.md#Record_Representation_Clause
[Record_Type]: ast.md#Record_Type
[Representation_Clause]: ast.md#Representation_Clause
[Representation_Clause]: ast.md#Representation_Clause
[Requeue_Statement]: ast.md#Requeue_Statement
[Return_Object_Specification]: ast.md#Return_Object_Specification
[Root_Type]: ast.md#Root_Type
[Select_Path]: ast.md#Select_Path
[Select_Statement]: ast.md#Select_Statement
[Selected_Component]: ast.md#Selected_Component
[Short_Circuit]: ast.md#Short_Circuit
[Signed_Integer_Type]: ast.md#Signed_Integer_Type
[Simple_Expression_Range]: ast.md#Simple_Expression_Range
[Simple_Return_Statement]: ast.md#Simple_Return_Statement
[Single_Protected_Declaration]: ast.md#Single_Protected_Declaration
[Single_Task_Declaration]: ast.md#Single_Task_Declaration
[Slice]: ast.md#Slice
[Statement]: ast.md#Statement
[String_Literal]: ast.md#String_Literal
[Subtype_Declaration]: ast.md#Subtype_Declaration
[Subtype_Indication]: ast.md#Subtype_Indication
[Task_Body_Declaration]: ast.md#Task_Body_Declaration
[Task_Body_Stub]: ast.md#Task_Body_Stub
[Task_Definition]: ast.md#Task_Definition
[Task_Type_Declaration]: ast.md#Task_Type_Declaration
[Terminate_Alternative_Statement]: ast.md#Terminate_Alternative_Statement
[Then_Abort_Path]: ast.md#Then_Abort_Path
[Type_Conversion]: ast.md#Type_Conversion
[Type_Declaration]: ast.md#Type_Declaration
[Type_Definition]: ast.md#Type_Definition
[Unconstrained_Array_Type]: ast.md#Unconstrained_Array_Type
[Unknown_Discriminant_Part]: ast.md#Unknown_Discriminant_Part
[Use_Clause]: ast.md#Use_Clause
[Variant]: ast.md#Variant
[Variant_Part]: ast.md#Variant_Part
[While_Loop_Statement]: ast.md#While_Loop_Statement
[With_Clause]: ast.md#With_Clause
