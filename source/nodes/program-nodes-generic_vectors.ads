--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Lexical_Elements;

generic
   type Vector_Interface is limited interface and
     Program.Element_Vectors.Element_Vector;

package Program.Nodes.Generic_Vectors is
   pragma Preelaborate;

   type Vector;

   function Create
    (Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
      return Vector;

   type Vector (<>) is new Vector_Interface with private;

   overriding function Get_Length (Self : Vector) return Positive;

   overriding function Element
     (Self  : Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access;

   overriding function Delimiter
     (Self  : Vector;
      Index : Positive)
     return Program.Lexical_Elements.Lexical_Element_Access;

private

   type Element_Array is array (Positive range <>)
     of Program.Elements.Element_Access;

   type Lexical_Element_Array is array (Positive range <>)
     of Program.Lexical_Elements.Lexical_Element_Access;

   type Vector (Elements, Tokens : Natural) is new Vector_Interface with record
      Element_List : Element_Array (1 .. Elements);
      Token_List   : Lexical_Element_Array (1 .. Tokens);
   end record;

end Program.Nodes.Generic_Vectors;
