--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Component_Clauses;
with Program.Elements.Record_Representation_Clauses;
with Program.Element_Visitors;

package Program.Nodes.Record_Representation_Clauses is

   pragma Pure (Program.Nodes.Record_Representation_Clauses);

   type Record_Representation_Clause is
     new Program.Nodes.Node
         and Program.Elements.Record_Representation_Clauses
           .Record_Representation_Clause
         and Program.Elements.Record_Representation_Clauses
           .Record_Representation_Clause_Text
     with private;

   function Create
    (For_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Use_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Mod_Semicolon_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Record_Representation_Clause;

   type Implicit_Record_Representation_Clause is
     new Program.Nodes.Node
         and Program.Elements.Record_Representation_Clauses
           .Record_Representation_Clause
     with private;

   function Create
    (Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Record_Representation_Clause
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Record_Representation_Clause is
     abstract new Program.Nodes.Node
       and Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause
     with record
        Name                  : not null Program.Elements.Expressions
          .Expression_Access;
        Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
        Component_Clauses     : not null Program.Elements.Component_Clauses
          .Component_Clause_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Record_Representation_Clause'Class);

   overriding procedure Visit
    (Self    : not null access Base_Record_Representation_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Record_Representation_Clause)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Mod_Clause_Expression
    (Self : Base_Record_Representation_Clause)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Component_Clauses
    (Self : Base_Record_Representation_Clause)
      return not null Program.Elements.Component_Clauses
          .Component_Clause_Vector_Access;

   overriding function Is_Record_Representation_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean;

   overriding function Is_Representation_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean;

   overriding function Is_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean;

   type Record_Representation_Clause is
     new Base_Record_Representation_Clause
       and Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Text
     with record
        For_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Use_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Record_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        At_Token            : Program.Lexical_Elements.Lexical_Element_Access;
        Mod_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Mod_Semicolon_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Record_Representation_Clause_Text
    (Self : aliased in out Record_Representation_Clause)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Text_Access;

   overriding function For_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Use_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Record_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function At_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Mod_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Mod_Semicolon_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Record_Representation_Clause is
     new Base_Record_Representation_Clause
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Record_Representation_Clause_Text
    (Self : aliased in out Implicit_Record_Representation_Clause)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Representation_Clause)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Representation_Clause)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Representation_Clause)
      return Boolean;

end Program.Nodes.Record_Representation_Clauses;
