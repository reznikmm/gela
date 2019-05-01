--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Subtype_Indications;
with Program.Elements.Qualified_Expressions;

package Program.Elements.Allocators is

   pragma Pure (Program.Elements.Allocators);

   type Allocator is
     limited interface and Program.Elements.Expressions.Expression;

   type Allocator_Access is access all Allocator'Class with Storage_Size => 0;

   not overriding function New_Token
    (Self : Allocator)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Allocator)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subpool_Name
    (Self : Allocator)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Allocator)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subtype_Indication
    (Self : Allocator)
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access
     is abstract;

   not overriding function Qualified_Expression
    (Self : Allocator)
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
     is abstract;

end Program.Elements.Allocators;
