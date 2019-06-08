--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Element_Vectors;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Generic_Procedure_Declarations is

   pragma Pure (Program.Elements.Generic_Procedure_Declarations);

   type Generic_Procedure_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Procedure_Declaration_Access is
     access all Generic_Procedure_Declaration'Class with Storage_Size => 0;

   not overriding function Generic_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Formal_Parameters
    (Self : Generic_Procedure_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Procedure_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Generic_Procedure_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Generic_Procedure_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Generic_Procedure_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Generic_Procedure_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Generic_Procedure_Declarations;
