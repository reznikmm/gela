--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Ada.Containers.Vectors;

with Program.Compilation_Units;
with Program.Compilations;
with Program.Elements;
with Program.Lexical_Elements;

limited private with Program.Parsers.Nodes;

package Program.Parsers is

   package Element_Vectors is new Ada.Containers.Vectors
     (Positive, Program.Elements.Element_Access, Program.Elements."=");

   package Unit_Vectors is new Ada.Containers.Vectors
     (Positive,
      Program.Compilation_Units.Compilation_Unit_Access,
      Program.Compilation_Units."=");

   procedure Parse
     (Compilation : not null Program.Compilations.Compilation_Access;
      Tokens      : not null Lexical_Elements.Lexical_Element_Vector_Access;
      Subpool     : not null System.Storage_Pools.Subpools.Subpool_Handle;
      Units       : out Unit_Vectors.Vector;
      Pragmas     : out Element_Vectors.Vector);

private

   type Parse_Context is record
      Factory : not null access Program.Parsers.Nodes.Node_Factory;
      Tokens  : not null Lexical_Elements.Lexical_Element_Vector_Access;
      Index   : Positive := 1;
   end record;

end Program.Parsers;
