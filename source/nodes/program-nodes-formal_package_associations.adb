--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Package_Associations is

   function Create
    (Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : Program.Elements.Expressions.Expression_Access;
     Box_Token        : Program.Lexical_Elements.Lexical_Element_Access)
      return Formal_Package_Association is
   begin
      return Result : Formal_Package_Association :=
        (Formal_Parameter => Formal_Parameter, Arrow_Token => Arrow_Token,
         Actual_Parameter => Actual_Parameter, Box_Token => Box_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Formal_Parameter     : Program.Elements.Expressions.Expression_Access;
     Actual_Parameter     : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Package_Association is
   begin
      return Result : Implicit_Formal_Package_Association :=
        (Formal_Parameter => Formal_Parameter,
         Actual_Parameter => Actual_Parameter,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Formal_Parameter
    (Self : Base_Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Formal_Parameter;
   end Formal_Parameter;

   overriding function Actual_Parameter
    (Self : Base_Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Actual_Parameter;
   end Actual_Parameter;

   overriding function Arrow_Token
    (Self : Formal_Package_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Box_Token
    (Self : Formal_Package_Association)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Package_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Package_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Package_Association)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Formal_Package_Association'Class) is
   begin
      if Self.Formal_Parameter.Assigned then
         Set_Enclosing_Element (Self.Formal_Parameter, Self'Unchecked_Access);
      end if;
      if Self.Actual_Parameter.Assigned then
         Set_Enclosing_Element (Self.Actual_Parameter, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Formal_Package_Association_Element
    (Self : Base_Formal_Package_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Package_Association_Element;

   overriding function Is_Association_Element
    (Self : Base_Formal_Package_Association)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Association_Element;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Package_Association;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Package_Association (Self);
   end Visit;

   overriding function To_Formal_Package_Association_Text
    (Self : aliased in out Formal_Package_Association)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Package_Association_Text;

   overriding function To_Formal_Package_Association_Text
    (Self : aliased in out Implicit_Formal_Package_Association)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Package_Association_Text;

end Program.Nodes.Formal_Package_Associations;
