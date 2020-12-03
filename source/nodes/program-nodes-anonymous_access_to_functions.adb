--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Anonymous_Access_To_Functions is

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return Anonymous_Access_To_Function is
   begin
      return Result : Anonymous_Access_To_Function :=
        (Not_Token => Not_Token, Null_Token => Null_Token,
         Access_Token => Access_Token, Protected_Token => Protected_Token,
         Function_Token => Function_Token,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token,
         Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
         Null_Token_2 => Null_Token_2, Result_Subtype => Result_Subtype,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Not_Null_2       : Boolean := False)
      return Implicit_Anonymous_Access_To_Function is
   begin
      return Result : Implicit_Anonymous_Access_To_Function :=
        (Parameters => Parameters, Result_Subtype => Result_Subtype,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Has_Protected => Has_Protected,
         Has_Not_Null_2 => Has_Not_Null_2, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Parameters
    (Self : Base_Anonymous_Access_To_Function)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Result_Subtype
    (Self : Base_Anonymous_Access_To_Function)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Result_Subtype;
   end Result_Subtype;

   overriding function Not_Token
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Access_Token
    (Self : Anonymous_Access_To_Function)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Access_Token;
   end Access_Token;

   overriding function Protected_Token
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Protected_Token;
   end Protected_Token;

   overriding function Function_Token
    (Self : Anonymous_Access_To_Function)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Function_Token;
   end Function_Token;

   overriding function Left_Bracket_Token
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Return_Token
    (Self : Anonymous_Access_To_Function)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Return_Token;
   end Return_Token;

   overriding function Not_Token_2
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token_2;
   end Not_Token_2;

   overriding function Null_Token_2
    (Self : Anonymous_Access_To_Function)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token_2;
   end Null_Token_2;

   overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Has_Protected
    (Self : Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Protected_Token.Assigned;
   end Has_Protected;

   overriding function Has_Not_Null_2
    (Self : Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Null_Token_2.Assigned;
   end Has_Not_Null_2;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   overriding function Has_Protected
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Has_Protected;
   end Has_Protected;

   overriding function Has_Not_Null_2
    (Self : Implicit_Anonymous_Access_To_Function)
      return Boolean is
   begin
      return Self.Has_Not_Null_2;
   end Has_Not_Null_2;

   procedure Initialize
    (Self : aliased in out Base_Anonymous_Access_To_Function'Class) is
   begin
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Result_Subtype, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Anonymous_Access_To_Function
    (Self : Base_Anonymous_Access_To_Function)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_To_Function;

   overriding function Is_Anonymous_Access_Definition
    (Self : Base_Anonymous_Access_To_Function)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_Definition;

   overriding function Is_Definition
    (Self : Base_Anonymous_Access_To_Function)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Anonymous_Access_To_Function;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Anonymous_Access_To_Function (Self);
   end Visit;

   overriding function To_Anonymous_Access_To_Function_Text
    (Self : aliased in out Anonymous_Access_To_Function)
      return Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Anonymous_Access_To_Function_Text;

   overriding function To_Anonymous_Access_To_Function_Text
    (Self : aliased in out Implicit_Anonymous_Access_To_Function)
      return Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Anonymous_Access_To_Function_Text;

end Program.Nodes.Anonymous_Access_To_Functions;
