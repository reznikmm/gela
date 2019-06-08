--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Paths;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Select_Paths is

   pragma Pure (Program.Elements.Select_Paths);

   type Select_Path is limited interface and Program.Elements.Paths.Path;

   type Select_Path_Access is access all Select_Path'Class
     with Storage_Size => 0;

   not overriding function When_Token
    (Self : Select_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Guard
    (Self : Select_Path)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Select_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Statements
    (Self : Select_Path)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type Select_Path_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Select_Path_Vector_Access is access all Select_Path_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Select_Path_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Select_Path;

   function To_Select_Path
    (Self  : Select_Path_Vector'Class;
     Index : Positive)
      return not null Select_Path_Access
     is (Self.Element (Index).To_Select_Path);

end Program.Elements.Select_Paths;
