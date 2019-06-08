--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Discrete_Subtype_Definitions;

package Program.Elements.Loop_Parameter_Specifications is

   pragma Pure (Program.Elements.Loop_Parameter_Specifications);

   type Loop_Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Loop_Parameter_Specification_Access is
     access all Loop_Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Loop_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function In_Token
    (Self : Loop_Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Reverse_Token
    (Self : Loop_Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Definition
    (Self : Loop_Parameter_Specification)
      return not null Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access is abstract;

end Program.Elements.Loop_Parameter_Specifications;
