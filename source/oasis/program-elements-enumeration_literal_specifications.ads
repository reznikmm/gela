--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Declarations;
with Program.Elements.Defining_Names;

package Program.Elements.Enumeration_Literal_Specifications is

   pragma Pure (Program.Elements.Enumeration_Literal_Specifications);

   type Enumeration_Literal_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Enumeration_Literal_Specification_Access is
     access all Enumeration_Literal_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Enumeration_Literal_Specification)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   type Enumeration_Literal_Specification_Text is limited interface;

   type Enumeration_Literal_Specification_Text_Access is
     access all Enumeration_Literal_Specification_Text'Class
     with Storage_Size => 0;

   not overriding function To_Enumeration_Literal_Specification_Text
    (Self : in out Enumeration_Literal_Specification)
      return Enumeration_Literal_Specification_Text_Access is abstract;

   type Enumeration_Literal_Specification_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Enumeration_Literal_Specification_Vector_Access is
     access all Enumeration_Literal_Specification_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Enumeration_Literal_Specification_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Enumeration_Literal_Specification;

   function To_Enumeration_Literal_Specification
    (Self  : Enumeration_Literal_Specification_Vector'Class;
     Index : Positive)
      return not null Enumeration_Literal_Specification_Access
     is (Self.Element (Index).To_Enumeration_Literal_Specification);

end Program.Elements.Enumeration_Literal_Specifications;
