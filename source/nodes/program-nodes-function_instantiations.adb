--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Function_Instantiations is

   function Create
    (Not_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects               : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Function_Instantiation is
   begin
      return Result : Function_Instantiation :=
        (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
         Function_Token => Function_Token, Name => Name, Is_Token => Is_Token,
         New_Token => New_Token,
         Generic_Function_Name => Generic_Function_Name,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects               : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False;
     Has_Not               : Boolean := False;
     Has_Overriding        : Boolean := False)
      return Implicit_Function_Instantiation is
   begin
      return Result : Implicit_Function_Instantiation :=
        (Name => Name, Generic_Function_Name => Generic_Function_Name,
         Parameters => Parameters, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_Not => Has_Not,
         Has_Overriding => Has_Overriding, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Generic_Function_Name
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Generic_Function_Name;
   end Generic_Function_Name;

   overriding function Parameters
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Aspects
    (Self : Base_Function_Instantiation)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Not_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Overriding_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Overriding_Token;
   end Overriding_Token;

   overriding function Function_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Function_Token;
   end Function_Token;

   overriding function Is_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function New_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function Left_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function With_Token
    (Self : Function_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Function_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Not
    (Self : Function_Instantiation)
      return Boolean is
   begin
      return Self.Not_Token.Assigned;
   end Has_Not;

   overriding function Has_Overriding
    (Self : Function_Instantiation)
      return Boolean is
   begin
      return Self.Overriding_Token.Assigned;
   end Has_Overriding;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Function_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Function_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Function_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not
    (Self : Implicit_Function_Instantiation)
      return Boolean is
   begin
      return Self.Has_Not;
   end Has_Not;

   overriding function Has_Overriding
    (Self : Implicit_Function_Instantiation)
      return Boolean is
   begin
      return Self.Has_Overriding;
   end Has_Overriding;

   procedure Initialize
    (Self : aliased in out Base_Function_Instantiation'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element
        (Self.Generic_Function_Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Function_Instantiation
    (Self : Base_Function_Instantiation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Function_Instantiation;

   overriding function Is_Declaration
    (Self : Base_Function_Instantiation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Function_Instantiation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Function_Instantiation (Self);
   end Visit;

   overriding function To_Function_Instantiation_Text
    (Self : aliased in out Function_Instantiation)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Function_Instantiation_Text;

   overriding function To_Function_Instantiation_Text
    (Self : aliased in out Implicit_Function_Instantiation)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Function_Instantiation_Text;

end Program.Nodes.Function_Instantiations;
