--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;

package Program.Elements.Choice_Parameter_Specifications is

   pragma Pure (Program.Elements.Choice_Parameter_Specifications);

   type Choice_Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Choice_Parameter_Specification_Access is
     access all Choice_Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Choice_Parameter_Specification)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Colon_Token
    (Self : Choice_Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Choice_Parameter_Specifications;
