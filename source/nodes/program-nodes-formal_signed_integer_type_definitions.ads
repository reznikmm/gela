--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Formal_Signed_Integer_Type_Definitions;
with Program.Element_Visitors;

package Program.Nodes.Formal_Signed_Integer_Type_Definitions is

   pragma Pure (Program.Nodes.Formal_Signed_Integer_Type_Definitions);

   type Formal_Signed_Integer_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Signed_Integer_Type_Definitions
           .Formal_Signed_Integer_Type_Definition
         and Program.Elements.Formal_Signed_Integer_Type_Definitions
           .Formal_Signed_Integer_Type_Definition_Text
     with private;

   function Create
    (Range_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Signed_Integer_Type_Definition;

   type Implicit_Formal_Signed_Integer_Type_Definition is
     new Program.Nodes.Node
         and Program.Elements.Formal_Signed_Integer_Type_Definitions
           .Formal_Signed_Integer_Type_Definition
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Signed_Integer_Type_Definition
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Signed_Integer_Type_Definition is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Signed_Integer_Type_Definitions
         .Formal_Signed_Integer_Type_Definition
     with null record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Signed_Integer_Type_Definition'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Signed_Integer_Type_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Formal_Signed_Integer_Type_Definition
    (Self : Base_Formal_Signed_Integer_Type_Definition)
      return Boolean;

   overriding function Is_Formal_Type_Definition
    (Self : Base_Formal_Signed_Integer_Type_Definition)
      return Boolean;

   overriding function Is_Definition
    (Self : Base_Formal_Signed_Integer_Type_Definition)
      return Boolean;

   type Formal_Signed_Integer_Type_Definition is
     new Base_Formal_Signed_Integer_Type_Definition
       and Program.Elements.Formal_Signed_Integer_Type_Definitions
         .Formal_Signed_Integer_Type_Definition_Text
     with record
        Range_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
        Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Formal_Signed_Integer_Type_Definition_Text
    (Self : aliased in out Formal_Signed_Integer_Type_Definition)
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Text_Access;

   overriding function Range_Token
    (Self : Formal_Signed_Integer_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Box_Token
    (Self : Formal_Signed_Integer_Type_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Formal_Signed_Integer_Type_Definition is
     new Base_Formal_Signed_Integer_Type_Definition
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Formal_Signed_Integer_Type_Definition_Text
    (Self : aliased in out Implicit_Formal_Signed_Integer_Type_Definition)
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Signed_Integer_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Signed_Integer_Type_Definition)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Signed_Integer_Type_Definition)
      return Boolean;

end Program.Nodes.Formal_Signed_Integer_Type_Definitions;
