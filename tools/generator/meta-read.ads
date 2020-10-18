--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with League.Strings;
with Ada.Containers.Vectors;
with Meta.Classes;

package Meta.Read is

   package Class_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Meta.Classes.Class,
      "="          => Meta.Classes."=");

   procedure Read_AST
     (File_Name : League.Strings.Universal_String;
      Result    : out Class_Vectors.Vector);

end Meta.Read;
