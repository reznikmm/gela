--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Function_Calls is

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Function_Call is
   begin
      return Result : Function_Call :=
        (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
         Parameters => Parameters, Right_Bracket_Token => Right_Bracket_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Function_Call is
   begin
      return Result : Implicit_Function_Call :=
        (Prefix => Prefix, Parameters => Parameters,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Function_Call)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Parameters
    (Self : Base_Function_Call)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Left_Bracket_Token
    (Self : Function_Call)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Function_Call)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Function_Call)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Function_Call)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Function_Call)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Function_Call'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Function_Call
    (Self : Base_Function_Call)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Function_Call;

   overriding function Is_Expression
    (Self : Base_Function_Call)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Function_Call;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Function_Call (Self);
   end Visit;

   overriding function To_Function_Call_Text
    (Self : aliased in out Function_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Function_Call_Text;

   overriding function To_Function_Call_Text
    (Self : aliased in out Implicit_Function_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Function_Call_Text;

end Program.Nodes.Function_Calls;
