with Ada;
with Ada.Assertions;
with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Characters;
with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Directories.Information;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Float_Text_IO;
with Ada.Float_Wide_Text_IO;
with Ada.Float_Wide_Wide_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Integer_Wide_Text_IO;
with Ada.Integer_Wide_Wide_Text_IO;
with Ada.IO_Exceptions;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Sequential_IO;
with Ada.Storage_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Bounded;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO.Unbounded_IO;
with Ada.Wide_Characters;
with Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Bounded_IO;
with Ada.Wide_Text_IO.Text_Streams;
with Ada.Wide_Text_IO.Unbounded_IO;
with Ada.Wide_Wide_Characters;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Bounded_IO;
with Ada.Wide_Wide_Text_IO.Text_Streams;
with Ada.Wide_Wide_Text_IO.Unbounded_IO;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;
with System.Address_To_Access_Conversions;
with System.Machine_Code;
with System.Storage_Elements;
with System.Storage_Pools;

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;

-- gela reports ERROR_SYNTAX_MISPLACED_PRAGMA for these:
-- with Ada.Strings.Bounded.Hash;
-- with Ada.Strings.Fixed.Hash;
-- with Ada.Strings.Hash;
-- with Ada.Strings.Unbounded.Hash;
-- with Ada.Strings.Wide_Bounded.Wide_Hash;
-- with Ada.Strings.Wide_Fixed.Wide_Hash;
-- with Ada.Strings.Wide_Hash;
-- with Ada.Strings.Wide_Unbounded.Wide_Hash;
-- with Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash;
-- with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;
-- with Ada.Strings.Wide_Wide_Hash;
-- with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

procedure Example is
begin
   Ada.Text_IO.Put_Line ("Hello world!");
end Example;