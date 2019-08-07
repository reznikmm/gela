--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Parameter_Specifications is

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Out_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Parameter_Subtype  : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access)
      return Parameter_Specification is
   begin
      return Result : Parameter_Specification :=
        (Names => Names, Colon_Token => Colon_Token,
         Aliased_Token => Aliased_Token, In_Token => In_Token,
         Out_Token => Out_Token, Not_Token => Not_Token,
         Null_Token => Null_Token, Parameter_Subtype => Parameter_Subtype,
         Assignment_Token => Assignment_Token,
         Default_Expression => Default_Expression, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Parameter_Subtype    : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False;
     Has_In               : Boolean := False;
     Has_Out              : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Parameter_Specification is
   begin
      return Result : Implicit_Parameter_Specification :=
        (Names => Names, Parameter_Subtype => Parameter_Subtype,
         Default_Expression => Default_Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Aliased => Has_Aliased, Has_In => Has_In, Has_Out => Has_Out,
         Has_Not_Null => Has_Not_Null, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Parameter_Subtype
    (Self : Base_Parameter_Specification)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Parameter_Subtype;
   end Parameter_Subtype;

   overriding function Default_Expression
    (Self : Base_Parameter_Specification)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Default_Expression;
   end Default_Expression;

   overriding function Colon_Token
    (Self : Parameter_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Aliased_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Aliased_Token;
   end Aliased_Token;

   overriding function In_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Out_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Out_Token;
   end Out_Token;

   overriding function Not_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Assignment_Token
    (Self : Parameter_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function Has_Aliased
    (Self : Parameter_Specification)
      return Boolean is
   begin
      return Self.Aliased_Token.Assigned;
   end Has_Aliased;

   overriding function Has_In
    (Self : Parameter_Specification)
      return Boolean is
   begin
      return Self.In_Token.Assigned;
   end Has_In;

   overriding function Has_Out
    (Self : Parameter_Specification)
      return Boolean is
   begin
      return Self.Out_Token.Assigned;
   end Has_Out;

   overriding function Has_Not_Null
    (Self : Parameter_Specification)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Aliased
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Has_Aliased;
   end Has_Aliased;

   overriding function Has_In
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Has_In;
   end Has_In;

   overriding function Has_Out
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Has_Out;
   end Has_Out;

   overriding function Has_Not_Null
    (Self : Implicit_Parameter_Specification)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   procedure Initialize
    (Self : aliased in out Base_Parameter_Specification'Class) is
   begin
      for Item in Self.Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Parameter_Subtype, Self'Unchecked_Access);
      if Self.Default_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Default_Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Parameter_Specification
    (Self : Base_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Parameter_Specification;

   overriding function Is_Declaration
    (Self : Base_Parameter_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Parameter_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Parameter_Specification (Self);
   end Visit;

   overriding function To_Parameter_Specification_Text
    (Self : aliased in out Parameter_Specification)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Parameter_Specification_Text;

   overriding function To_Parameter_Specification_Text
    (Self : aliased in out Implicit_Parameter_Specification)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Parameter_Specification_Text;

end Program.Nodes.Parameter_Specifications;
