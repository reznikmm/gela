--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Protected_Definitions is

   function Create
    (Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return Protected_Definition is
   begin
      return Result : Protected_Definition :=
        (Visible_Declarations => Visible_Declarations,
         Private_Token => Private_Token,
         Private_Declarations => Private_Declarations, End_Token => End_Token,
         End_Name => End_Name, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Definition is
   begin
      return Result : Implicit_Protected_Definition :=
        (Visible_Declarations => Visible_Declarations,
         Private_Declarations => Private_Declarations, End_Name => End_Name,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Visible_Declarations
    (Self : Base_Protected_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Visible_Declarations;
   end Visible_Declarations;

   overriding function Private_Declarations
    (Self : Base_Protected_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Private_Declarations;
   end Private_Declarations;

   overriding function End_Name
    (Self : Base_Protected_Definition)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.End_Name;
   end End_Name;

   overriding function Private_Token
    (Self : Protected_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function End_Token
    (Self : Protected_Definition)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Protected_Definition'Class) is
   begin
      for Item in Self.Visible_Declarations.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Private_Declarations.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Name.Assigned then
         Set_Enclosing_Element (Self.End_Name, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Protected_Definition
    (Self : Base_Protected_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Protected_Definition;

   overriding function Is_Definition
    (Self : Base_Protected_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Protected_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Protected_Definition (Self);
   end Visit;

   overriding function To_Protected_Definition_Text
    (Self : aliased in out Protected_Definition)
      return Program.Elements.Protected_Definitions
          .Protected_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Protected_Definition_Text;

   overriding function To_Protected_Definition_Text
    (Self : aliased in out Implicit_Protected_Definition)
      return Program.Elements.Protected_Definitions
          .Protected_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Protected_Definition_Text;

end Program.Nodes.Protected_Definitions;
