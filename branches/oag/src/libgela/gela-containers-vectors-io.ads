with Ada.Streams.Stream_IO;

generic
package Gela.Containers.Vectors.IO is

   procedure Save
     (Output : in     Ada.Streams.Stream_IO.File_Type;
      Object : in     Vector);

   procedure Load
     (Input  : in     Ada.Streams.Stream_IO.File_Type;
      Object : in out Vector);

end Gela.Containers.Vectors.IO;
