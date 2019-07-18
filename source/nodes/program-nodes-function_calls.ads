--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Associations;
with Program.Elements.Function_Calls;
with Program.Element_Visitors;

package Program.Nodes.Function_Calls is

   pragma Pure (Program.Nodes.Function_Calls);

   type Function_Call is
     new Program.Nodes.Node and Program.Elements.Function_Calls.Function_Call
         and Program.Elements.Function_Calls.Function_Call_Text
     with private;

   function Create
    (Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Function_Call;

   type Implicit_Function_Call is
     new Program.Nodes.Node and Program.Elements.Function_Calls.Function_Call
     with private;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Function_Call
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Function_Call is
     abstract new Program.Nodes.Node
       and Program.Elements.Function_Calls.Function_Call
     with record
        Prefix     : not null Program.Elements.Expressions.Expression_Access;
        Parameters : not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Function_Call'Class);

   overriding procedure Visit
    (Self    : not null access Base_Function_Call;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Prefix
    (Self : Base_Function_Call)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Function_Call)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Is_Function_Call
    (Self : Base_Function_Call)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Function_Call)
      return Boolean;

   type Function_Call is
     new Base_Function_Call
       and Program.Elements.Function_Calls.Function_Call_Text
     with record
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Function_Call_Text
    (Self : aliased in out Function_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Function_Call)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Function_Call)
      return Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Function_Call is
     new Base_Function_Call
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Function_Call_Text
    (Self : aliased in out Implicit_Function_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Function_Call)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Function_Call)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Function_Call)
      return Boolean;

end Program.Nodes.Function_Calls;
