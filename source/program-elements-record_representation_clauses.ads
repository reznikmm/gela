--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Component_Clauses;

package Program.Elements.Record_Representation_Clauses is

   pragma Pure (Program.Elements.Record_Representation_Clauses);

   type Record_Representation_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type Record_Representation_Clause_Access is
     access all Record_Representation_Clause'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Record_Representation_Clause)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Mod_Clause_Expression
    (Self : Record_Representation_Clause)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Component_Clauses
    (Self : Record_Representation_Clause)
      return not null Program.Elements.Component_Clauses
          .Component_Clause_Vector_Access is abstract;

   type Record_Representation_Clause_Text is limited interface;

   type Record_Representation_Clause_Text_Access is
     access all Record_Representation_Clause_Text'Class with Storage_Size => 0;

   not overriding function To_Record_Representation_Clause_Text
    (Self : aliased Record_Representation_Clause)
      return Record_Representation_Clause_Text_Access is abstract;

   not overriding function For_Token
    (Self : Record_Representation_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Use_Token
    (Self : Record_Representation_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Record_Token
    (Self : Record_Representation_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function At_Token
    (Self : Record_Representation_Clause_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Mod_Token
    (Self : Record_Representation_Clause_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Mod_Semicolon_Token
    (Self : Record_Representation_Clause_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Record_Representation_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Record_Representation_Clauses;
