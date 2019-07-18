--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Pragmas is

   function Create
    (Pragma_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Arguments           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Pragma_Element is
   begin
      return Result : Pragma_Element :=
        (Pragma_Token => Pragma_Token, Name => Name,
         Left_Bracket_Token => Left_Bracket_Token, Arguments => Arguments,
         Right_Bracket_Token => Right_Bracket_Token,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Arguments            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Pragma is
   begin
      return Result : Implicit_Pragma :=
        (Name => Name, Arguments => Arguments,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Pragma)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Arguments
    (Self : Base_Pragma)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.Arguments;
   end Arguments;

   overriding function Pragma_Token
    (Self : Pragma_Element)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Pragma_Token;
   end Pragma_Token;

   overriding function Left_Bracket_Token
    (Self : Pragma_Element)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Pragma_Element)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Semicolon_Token
    (Self : Pragma_Element)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Pragma)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Pragma)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Pragma)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Pragma'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Arguments.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Pragma (Self : Base_Pragma) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Pragma;

   overriding procedure Visit
    (Self    : not null access Base_Pragma;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Pragma_Element (Self);
   end Visit;

   overriding function To_Pragma_Text
    (Self : aliased in out Pragma_Element)
      return Program.Elements.Pragmas.Pragma_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Pragma_Text;

   overriding function To_Pragma_Text
    (Self : aliased in out Implicit_Pragma)
      return Program.Elements.Pragmas.Pragma_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Pragma_Text;

end Program.Nodes.Pragmas;
