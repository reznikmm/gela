--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Subtype_Indications;
with Program.Elements.Qualified_Expressions;
with Program.Elements.Allocators;
with Program.Element_Visitors;

package Program.Nodes.Allocators is

   pragma Preelaborate;

   type Allocator is
     new Program.Nodes.Node and Program.Elements.Allocators.Allocator
         and Program.Elements.Allocators.Allocator_Text
     with private;

   function Create
    (New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access)
      return Allocator;

   type Implicit_Allocator is
     new Program.Nodes.Node and Program.Elements.Allocators.Allocator
     with private;

   function Create
    (Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Allocator
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Allocator is
     abstract new Program.Nodes.Node and Program.Elements.Allocators.Allocator
     with record
        Subpool_Name         : Program.Elements.Expressions.Expression_Access;
        Subtype_Indication   : Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;
        Qualified_Expression : Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Allocator'Class);

   overriding procedure Visit
    (Self    : not null access Base_Allocator;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Subpool_Name
    (Self : Base_Allocator)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Subtype_Indication
    (Self : Base_Allocator)
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access;

   overriding function Qualified_Expression
    (Self : Base_Allocator)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access;

   overriding function Is_Allocator (Self : Base_Allocator) return Boolean;

   overriding function Is_Expression (Self : Base_Allocator) return Boolean;

   type Allocator is
     new Base_Allocator and Program.Elements.Allocators.Allocator_Text
     with record
        New_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Allocator_Text
    (Self : aliased in out Allocator)
      return Program.Elements.Allocators.Allocator_Text_Access;

   overriding function New_Token
    (Self : Allocator)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Allocator)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Allocator)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Allocator is
     new Base_Allocator
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Allocator_Text
    (Self : aliased in out Implicit_Allocator)
      return Program.Elements.Allocators.Allocator_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Allocator)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Allocator)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Allocator)
      return Boolean;

end Program.Nodes.Allocators;
