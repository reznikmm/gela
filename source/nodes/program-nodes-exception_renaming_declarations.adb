--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Exception_Renaming_Declarations is

   function Create
    (Names             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renames_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Exception : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Exception_Renaming_Declaration is
   begin
      return Result : Exception_Renaming_Declaration :=
        (Names => Names, Colon_Token => Colon_Token,
         Exception_Token => Exception_Token, Renames_Token => Renames_Token,
         Renamed_Exception => Renamed_Exception, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Renamed_Exception    : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exception_Renaming_Declaration is
   begin
      return Result : Implicit_Exception_Renaming_Declaration :=
        (Names => Names, Renamed_Exception => Renamed_Exception,
         Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Exception_Renaming_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Renamed_Exception
    (Self : Base_Exception_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Renamed_Exception;
   end Renamed_Exception;

   overriding function Aspects
    (Self : Base_Exception_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Colon_Token
    (Self : Exception_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Exception_Token
    (Self : Exception_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Exception_Token;
   end Exception_Token;

   overriding function Renames_Token
    (Self : Exception_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Renames_Token;
   end Renames_Token;

   overriding function With_Token
    (Self : Exception_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Exception_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exception_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exception_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exception_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : in out Base_Exception_Renaming_Declaration'Class) is
   begin
      for Item in Self.Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Renamed_Exception, Self'Unchecked_Access);
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Exception_Renaming_Declaration
    (Self : Base_Exception_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Exception_Renaming_Declaration;

   overriding function Is_Declaration
    (Self : Base_Exception_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Exception_Renaming_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Exception_Renaming_Declaration (Self);
   end Visit;

   overriding function To_Exception_Renaming_Declaration_Text
    (Self : in out Exception_Renaming_Declaration)
      return Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Exception_Renaming_Declaration_Text;

   overriding function To_Exception_Renaming_Declaration_Text
    (Self : in out Implicit_Exception_Renaming_Declaration)
      return Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Exception_Renaming_Declaration_Text;

end Program.Nodes.Exception_Renaming_Declarations;
