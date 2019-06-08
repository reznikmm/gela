--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Defining_Names;
with Program.Tokens;

package Program.Elements.Defining_Identifiers is

   pragma Pure (Program.Elements.Defining_Identifiers);

   type Defining_Identifier is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Identifier_Access is access all Defining_Identifier'Class
     with Storage_Size => 0;

   not overriding function Identifier_Token
    (Self : Defining_Identifier)
      return Program.Tokens.Token_Access is abstract;

   type Defining_Identifier_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Defining_Identifier_Vector_Access is
     access all Defining_Identifier_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Defining_Identifier_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Defining_Identifier;

   function To_Defining_Identifier
    (Self  : Defining_Identifier_Vector'Class;
     Index : Positive)
      return not null Defining_Identifier_Access
     is (Self.Element (Index).To_Defining_Identifier);

end Program.Elements.Defining_Identifiers;
