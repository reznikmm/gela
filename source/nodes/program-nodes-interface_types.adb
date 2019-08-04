--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Interface_Types is

   function Create
    (Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access)
      return Interface_Type is
   begin
      return Result : Interface_Type :=
        (Limited_Token => Limited_Token, Task_Token => Task_Token,
         Protected_Token => Protected_Token,
         Synchronized_Token => Synchronized_Token,
         Interface_Token => Interface_Token, And_Token => And_Token,
         Progenitors => Progenitors, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Task             : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Synchronized     : Boolean := False)
      return Implicit_Interface_Type is
   begin
      return Result : Implicit_Interface_Type :=
        (Progenitors => Progenitors,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Limited => Has_Limited, Has_Task => Has_Task,
         Has_Protected => Has_Protected, Has_Synchronized => Has_Synchronized,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Progenitors
    (Self : Base_Interface_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Limited_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Task_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Task_Token;
   end Task_Token;

   overriding function Protected_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Protected_Token;
   end Protected_Token;

   overriding function Synchronized_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Synchronized_Token;
   end Synchronized_Token;

   overriding function Interface_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Interface_Token;
   end Interface_Token;

   overriding function And_Token
    (Self : Interface_Type)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.And_Token;
   end And_Token;

   overriding function Has_Limited (Self : Interface_Type) return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Has_Task (Self : Interface_Type) return Boolean is
   begin
      return Self.Task_Token.Assigned;
   end Has_Task;

   overriding function Has_Protected (Self : Interface_Type) return Boolean is
   begin
      return Self.Protected_Token.Assigned;
   end Has_Protected;

   overriding function Has_Synchronized
    (Self : Interface_Type)
      return Boolean is
   begin
      return Self.Synchronized_Token.Assigned;
   end Has_Synchronized;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Limited
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   overriding function Has_Task
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Has_Task;
   end Has_Task;

   overriding function Has_Protected
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Has_Protected;
   end Has_Protected;

   overriding function Has_Synchronized
    (Self : Implicit_Interface_Type)
      return Boolean is
   begin
      return Self.Has_Synchronized;
   end Has_Synchronized;

   procedure Initialize (Self : aliased in out Base_Interface_Type'Class) is
   begin
      for Item in Self.Progenitors.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Interface_Type
    (Self : Base_Interface_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Interface_Type;

   overriding function Is_Type_Definition
    (Self : Base_Interface_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Interface_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Interface_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Interface_Type (Self);
   end Visit;

   overriding function To_Interface_Type_Text
    (Self : aliased in out Interface_Type)
      return Program.Elements.Interface_Types.Interface_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Interface_Type_Text;

   overriding function To_Interface_Type_Text
    (Self : aliased in out Implicit_Interface_Type)
      return Program.Elements.Interface_Types.Interface_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Interface_Type_Text;

end Program.Nodes.Interface_Types;
