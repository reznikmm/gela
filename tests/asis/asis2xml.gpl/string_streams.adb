package body String_Streams is

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Self : String_Stream) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Text);
   end Get_Text;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out String_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      Text : String (1 .. Item'Length);
      for Text'Address use Item'Address;
      pragma Import (Ada, Text);
   begin
      Ada.Strings.Unbounded.Append (Stream.Text, Text);
   end Write;

end String_Streams;
