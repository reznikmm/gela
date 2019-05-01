--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Elements.Formal_Access_Types;
with Program.Elements.Anonymous_Access_Definitions;
with Program.Tokens;

package Program.Elements.Procedure_Access_Types is

   pragma Pure (Program.Elements.Procedure_Access_Types);

   type Procedure_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type
       and Program.Elements.Formal_Access_Types.Formal_Access_Type
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Procedure_Access_Type_Access is access all Procedure_Access_Type'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Access_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Protected_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Procedure_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Procedure_Access_Type)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Procedure_Access_Types;
