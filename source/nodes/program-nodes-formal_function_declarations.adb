--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Function_Declarations is

   function Create
    (With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subprogram_Default  : Program.Elements.Expressions.Expression_Access;
     Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Function_Declaration is
   begin
      return Result : Formal_Function_Declaration :=
        (With_Token => With_Token, Function_Token => Function_Token,
         Name => Name, Left_Bracket_Token => Left_Bracket_Token,
         Parameters => Parameters, Right_Bracket_Token => Right_Bracket_Token,
         Return_Token => Return_Token, Not_Token => Not_Token,
         Null_Token => Null_Token, Result_Subtype => Result_Subtype,
         Is_Token => Is_Token, Abstract_Token => Abstract_Token,
         Subprogram_Default => Subprogram_Default, Box_Token => Box_Token,
         With_Token_2 => With_Token_2, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Subprogram_Default   : Program.Elements.Expressions.Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Box              : Boolean := False)
      return Implicit_Formal_Function_Declaration is
   begin
      return Result : Implicit_Formal_Function_Declaration :=
        (Name => Name, Parameters => Parameters,
         Result_Subtype => Result_Subtype,
         Subprogram_Default => Subprogram_Default, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Has_Abstract => Has_Abstract,
         Has_Box => Has_Box, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Formal_Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Parameters
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Result_Subtype
    (Self : Base_Formal_Function_Declaration)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Result_Subtype;
   end Result_Subtype;

   overriding function Subprogram_Default
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subprogram_Default;
   end Subprogram_Default;

   overriding function Aspects
    (Self : Base_Formal_Function_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function With_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Function_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Function_Token;
   end Function_Token;

   overriding function Left_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Return_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Return_Token;
   end Return_Token;

   overriding function Not_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Is_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function Abstract_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abstract_Token;
   end Abstract_Token;

   overriding function Box_Token
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Box_Token;
   end Box_Token;

   overriding function With_Token_2
    (Self : Formal_Function_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token_2;
   end With_Token_2;

   overriding function Semicolon_Token
    (Self : Formal_Function_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Not_Null
    (Self : Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Has_Abstract
    (Self : Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Abstract_Token.Assigned;
   end Has_Abstract;

   overriding function Has_Box
    (Self : Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Box_Token.Assigned;
   end Has_Box;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   overriding function Has_Abstract
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Has_Abstract;
   end Has_Abstract;

   overriding function Has_Box
    (Self : Implicit_Formal_Function_Declaration)
      return Boolean is
   begin
      return Self.Has_Box;
   end Has_Box;

   procedure Initialize
    (Self : aliased in out Base_Formal_Function_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Result_Subtype, Self'Unchecked_Access);
      if Self.Subprogram_Default.Assigned then
         Set_Enclosing_Element
           (Self.Subprogram_Default, Self'Unchecked_Access);
      end if;
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Formal_Function_Declaration_Element
    (Self : Base_Formal_Function_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Function_Declaration_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Formal_Function_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Function_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Function_Declaration (Self);
   end Visit;

   overriding function To_Formal_Function_Declaration_Text
    (Self : aliased in out Formal_Function_Declaration)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Function_Declaration_Text;

   overriding function To_Formal_Function_Declaration_Text
    (Self : aliased in out Implicit_Formal_Function_Declaration)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Function_Declaration_Text;

end Program.Nodes.Formal_Function_Declarations;
