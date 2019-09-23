--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Source_Buffers;

private with Ada.Strings.UTF_Encoding;

package Program.Plain_Source_Buffers is

   type Source_Buffer is limited new Program.Source_Buffers.Source_Buffer
     with private;

   not overriding procedure Initialize
     (Self : in out Source_Buffer;
      Name : Program.Text);
   --  Initialize source buffer with content of file with given Name.
   --  File should be encoded with UTF-8 encoding.

private

   type UTF_8_String_Access is
     access all Ada.Strings.UTF_Encoding.UTF_8_String;

   type Source_Buffer is limited new Program.Source_Buffers.Source_Buffer
   with record
      Text : UTF_8_String_Access;
      From : Positive := 1;
   end record;

   overriding function Text
     (Self : Source_Buffer;
      Span : Program.Source_Buffers.Span) return Program.Text;

   overriding procedure Read
     (Self : in out Source_Buffer;
      Data : out Program.Source_Buffers.Character_Info_Array;
      Last : out Natural);

   overriding procedure Rewind (Self : in out Source_Buffer);

end Program.Plain_Source_Buffers;
