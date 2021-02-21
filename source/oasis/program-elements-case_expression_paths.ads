--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Paths;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Case_Expression_Paths is

   pragma Pure (Program.Elements.Case_Expression_Paths);

   type Case_Expression_Path is
     limited interface and Program.Elements.Paths.Path;

   type Case_Expression_Path_Access is access all Case_Expression_Path'Class
     with Storage_Size => 0;

   not overriding function Choices
    (Self : Case_Expression_Path)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Expression
    (Self : Case_Expression_Path)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Case_Expression_Path_Text is limited interface;

   type Case_Expression_Path_Text_Access is
     access all Case_Expression_Path_Text'Class with Storage_Size => 0;

   not overriding function To_Case_Expression_Path_Text
    (Self : in out Case_Expression_Path)
      return Case_Expression_Path_Text_Access is abstract;

   not overriding function When_Token
    (Self : Case_Expression_Path_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Arrow_Token
    (Self : Case_Expression_Path_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Case_Expression_Path_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Case_Expression_Path_Vector_Access is
     access all Case_Expression_Path_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Case_Expression_Path_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Case_Expression_Path;

   function To_Case_Expression_Path
    (Self  : Case_Expression_Path_Vector'Class;
     Index : Positive)
      return not null Case_Expression_Path_Access
     is (Self.Element (Index).To_Case_Expression_Path);

end Program.Elements.Case_Expression_Paths;
