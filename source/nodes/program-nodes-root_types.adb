--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Root_Types is

   function Create return Root_Type is
   begin
      return Result : Root_Type := (Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Root_Type is
   begin
      return Result : Implicit_Root_Type :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Root_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Root_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Root_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Root_Type'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Root_Type_Element
    (Self : Base_Root_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Root_Type_Element;

   overriding function Is_Type_Definition_Element
    (Self : Base_Root_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Root_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Root_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Root_Type (Self);
   end Visit;

   overriding function To_Root_Type_Text
    (Self : aliased in out Root_Type)
      return Program.Elements.Root_Types.Root_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Root_Type_Text;

   overriding function To_Root_Type_Text
    (Self : aliased in out Implicit_Root_Type)
      return Program.Elements.Root_Types.Root_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Root_Type_Text;

end Program.Nodes.Root_Types;
