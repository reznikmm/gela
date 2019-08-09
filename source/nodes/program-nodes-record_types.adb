--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Record_Types is

   function Create
    (Abstract_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Tagged_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Limited_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return Record_Type is
   begin
      return Result : Record_Type :=
        (Abstract_Token => Abstract_Token, Tagged_Token => Tagged_Token,
         Limited_Token => Limited_Token,
         Record_Definition => Record_Definition, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Record_Definition    : not null Program.Elements.Definitions
         .Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Record_Type is
   begin
      return Result : Implicit_Record_Type :=
        (Record_Definition => Record_Definition,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Record_Definition
    (Self : Base_Record_Type)
      return not null Program.Elements.Definitions.Definition_Access is
   begin
      return Self.Record_Definition;
   end Record_Definition;

   overriding function Abstract_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Tagged_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Tagged_Token;
   end Tagged_Token;

   overriding function Limited_Token
    (Self : Record_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Record_Type'Class) is
   begin
      Set_Enclosing_Element (Self.Record_Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Record_Type_Element
    (Self : Base_Record_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Record_Type_Element;

   overriding function Is_Type_Definition_Element
    (Self : Base_Record_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Record_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Record_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Record_Type (Self);
   end Visit;

   overriding function To_Record_Type_Text
    (Self : aliased in out Record_Type)
      return Program.Elements.Record_Types.Record_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Type_Text;

   overriding function To_Record_Type_Text
    (Self : aliased in out Implicit_Record_Type)
      return Program.Elements.Record_Types.Record_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Record_Type_Text;

end Program.Nodes.Record_Types;
