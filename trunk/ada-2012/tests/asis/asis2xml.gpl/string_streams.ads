with Ada.Streams;
with Ada.Strings.Unbounded;

package String_Streams is

   type String_Stream is new Ada.Streams.Root_Stream_Type with private;

   function Get_Text (Self : String_Stream) return String;

private

   type String_Stream is new Ada.Streams.Root_Stream_Type with record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Read
     (Stream : in out String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is null;

   overriding procedure Write
     (Stream : in out String_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

end String_Streams;
