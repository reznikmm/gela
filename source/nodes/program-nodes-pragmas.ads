--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Parameter_Associations;
with Program.Elements.Pragmas;
with Program.Element_Visitors;

package Program.Nodes.Pragmas is

   pragma Pure (Program.Nodes.Pragmas);

   type Pragma_Element is
     new Program.Nodes.Node and Program.Elements.Pragmas.Pragma_Element
         and Program.Elements.Pragmas.Pragma_Text
     with private;

   function Create
    (Pragma_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Arguments           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Pragma_Element;

   type Implicit_Pragma is
     new Program.Nodes.Node and Program.Elements.Pragmas.Pragma_Element
     with private;

   function Create
    (Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Arguments            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Pragma
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Pragma is
     abstract new Program.Nodes.Node
       and Program.Elements.Pragmas.Pragma_Element
     with record
        Name      : not null Program.Elements.Identifiers.Identifier_Access;
        Arguments : not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Pragma'Class);

   overriding procedure Visit
    (Self    : not null access Base_Pragma;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Pragma)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Arguments
    (Self : Base_Pragma)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Is_Pragma (Self : Base_Pragma) return Boolean;

   type Pragma_Element is
     new Base_Pragma and Program.Elements.Pragmas.Pragma_Text
     with record
        Pragma_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Pragma_Text
    (Self : aliased in out Pragma_Element)
      return Program.Elements.Pragmas.Pragma_Text_Access;

   overriding function Pragma_Token
    (Self : Pragma_Element)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Pragma_Element)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Pragma_Element)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Pragma_Element)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Pragma is
     new Base_Pragma
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Pragma_Text
    (Self : aliased in out Implicit_Pragma)
      return Program.Elements.Pragmas.Pragma_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Pragma)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Pragma)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Pragma)
      return Boolean;

end Program.Nodes.Pragmas;
