--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Scanner_States;

package Program.Source_Buffers is
   pragma Pure;

   type Source_Buffer is limited interface;
   --  Source_Buffer provides an access to source text.

   type Source_Buffer_Access is access all Source_Buffer'Class
     with Storage_Size => 0;

   type Span is record
      From : Positive;
      To   : Natural;
   end record;

   not overriding function Text
     (Self : Source_Buffer;
      Span : Program.Source_Buffers.Span) return Program.Text is abstract;
   --  Return text slice of Span, where Span is positions
   --  in the source measured in encoded text element (such as bytes for UTF-8)

   subtype Character_Length is Natural range 0 .. 6;
   --  Length of one character in encoded text elements

   type Character_Info is record
      Length : Character_Length;
      Class  : Program.Scanner_States.Character_Class;
      --  Class of character for scanner.
   end record;

   type Character_Info_Array is array (Positive range <>) of
     Character_Info;

   not overriding procedure Read
     (Self : in out Source_Buffer;
      Data : out Character_Info_Array;
      Last : out Natural) is abstract;
   --  Read next part of source starting from current position and decode
   --  corresponding character classes and character lengths.

   not overriding procedure Rewind (Self : in out Source_Buffer) is abstract;
   --  Set reading position to begin of the buffer

end Program.Source_Buffers;
