--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Component_Definitions is

   function Create
    (Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Element_Access)
      return Component_Definition is
   begin
      return Result : Component_Definition :=
        (Aliased_Token => Aliased_Token,
         Subtype_Indication => Subtype_Indication, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Indication   : not null Program.Elements.Element_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False)
      return Implicit_Component_Definition is
   begin
      return Result : Implicit_Component_Definition :=
        (Subtype_Indication => Subtype_Indication,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Aliased => Has_Aliased, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Indication
    (Self : Base_Component_Definition)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Subtype_Indication;
   end Subtype_Indication;

   overriding function Aliased_Token
    (Self : Component_Definition)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Aliased_Token;
   end Aliased_Token;

   overriding function Has_Aliased
    (Self : Component_Definition)
      return Boolean is
   begin
      return Self.Aliased_Token.Assigned;
   end Has_Aliased;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Definition)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Aliased
    (Self : Implicit_Component_Definition)
      return Boolean is
   begin
      return Self.Has_Aliased;
   end Has_Aliased;

   procedure Initialize
    (Self : aliased in out Base_Component_Definition'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Indication, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Component_Definition
    (Self : Base_Component_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Component_Definition;

   overriding function Is_Definition
    (Self : Base_Component_Definition)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Component_Definition;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Component_Definition (Self);
   end Visit;

   overriding function To_Component_Definition_Text
    (Self : aliased in out Component_Definition)
      return Program.Elements.Component_Definitions
          .Component_Definition_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Component_Definition_Text;

   overriding function To_Component_Definition_Text
    (Self : aliased in out Implicit_Component_Definition)
      return Program.Elements.Component_Definitions
          .Component_Definition_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Component_Definition_Text;

end Program.Nodes.Component_Definitions;
