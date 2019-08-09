--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Anonymous_Access_To_Procedures is

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Anonymous_Access_To_Procedure is
   begin
      return Result : Anonymous_Access_To_Procedure :=
        (Not_Token => Not_Token, Null_Token => Null_Token,
         Access_Token => Access_Token, Protected_Token => Protected_Token,
         Procedure_Token => Procedure_Token,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False)
      return Implicit_Anonymous_Access_To_Procedure is
   begin
      return Result : Implicit_Anonymous_Access_To_Procedure :=
        (Parameters => Parameters, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Not_Null => Has_Not_Null, Has_Protected => Has_Protected,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Parameters
    (Self : Base_Anonymous_Access_To_Procedure)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Not_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function Null_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Token;
   end Null_Token;

   overriding function Access_Token
    (Self : Anonymous_Access_To_Procedure)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Access_Token;
   end Access_Token;

   overriding function Protected_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Protected_Token;
   end Protected_Token;

   overriding function Procedure_Token
    (Self : Anonymous_Access_To_Procedure)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Procedure_Token;
   end Procedure_Token;

   overriding function Left_Bracket_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Anonymous_Access_To_Procedure)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Null_Token.Assigned;
   end Has_Not_Null;

   overriding function Has_Protected
    (Self : Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Protected_Token.Assigned;
   end Has_Protected;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not_Null
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Has_Not_Null;
   end Has_Not_Null;

   overriding function Has_Protected
    (Self : Implicit_Anonymous_Access_To_Procedure)
      return Boolean is
   begin
      return Self.Has_Protected;
   end Has_Protected;

   procedure Initialize
    (Self : aliased in out Base_Anonymous_Access_To_Procedure'Class) is
   begin
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Anonymous_Access_To_Procedure_Element
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_To_Procedure_Element;

   overriding function Is_Anonymous_Access_Definition_Element
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Anonymous_Access_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Anonymous_Access_To_Procedure)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Anonymous_Access_To_Procedure;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Anonymous_Access_To_Procedure (Self);
   end Visit;

   overriding function To_Anonymous_Access_To_Procedure_Text
    (Self : aliased in out Anonymous_Access_To_Procedure)
      return Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Anonymous_Access_To_Procedure_Text;

   overriding function To_Anonymous_Access_To_Procedure_Text
    (Self : aliased in out Implicit_Anonymous_Access_To_Procedure)
      return Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Anonymous_Access_To_Procedure_Text;

end Program.Nodes.Anonymous_Access_To_Procedures;
