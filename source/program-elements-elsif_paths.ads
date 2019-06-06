--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Element_Vectors;

package Program.Elements.Elsif_Paths is

   pragma Pure (Program.Elements.Elsif_Paths);

   type Elsif_Path is limited interface and Program.Elements.Paths.Path;

   type Elsif_Path_Access is access all Elsif_Path'Class
     with Storage_Size => 0;

   not overriding function Elsif_Token
    (Self : Elsif_Path)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Condition
    (Self : Elsif_Path)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Then_Token
    (Self : Elsif_Path)
      return Program.Tokens.Token_Access is abstract;

   type Elsif_Path_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Elsif_Path_Vector_Access is access all Elsif_Path_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Elsif_Path_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Elsif_Path;

   function To_Elsif_Path
    (Self  : Elsif_Path_Vector'Class;
     Index : Positive)
      return not null Elsif_Path_Access
     is (Self.Element (Index).To_Elsif_Path);

end Program.Elements.Elsif_Paths;
