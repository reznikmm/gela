package body Gela.Containers.Vectors.IO is

   ----------
   -- Load --
   ----------

   procedure Load
     (Input  : in     Ada.Streams.Stream_IO.File_Type;
      Object : in out Vector)
   is
      use Ada.Streams.Stream_IO;
      S      : constant Stream_Access := Stream (Input);
   begin
      Index_Natural'Read (S, Object.Length);
      Object.Saved_Part := new Table (1 .. Object.Length);
      Table'Read (S, Object.Saved_Part.all);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Output : in     Ada.Streams.Stream_IO.File_Type;
      Object : in     Vector)
   is
      use Ada.Streams.Stream_IO;
      S      : constant Stream_Access := Stream (Output);
      Rest   : Index_Natural := Object.Length;
      Second : Index_Natural;
      Third  : Index_Natural := 0;
   begin
      Index_Natural'Write (S, Object.Length);

      if Object.Saved_Part /= null then
         if Object.Length <= Object.Saved_Part'Length then
            Table'Write (S, Object.Saved_Part (1 .. Object.Length));
            return;
         else
            Table'Write (S, Object.Saved_Part.all);
            Rest := Rest - Object.Saved_Part'Length;
         end if;
      end if;

      while Rest > 0 loop
         Second := 0;

         while Rest > 0 and Second < Level_Size loop
            if Rest >= Level_Size then
               First_Table'Write
                 (S, Object.Third_Level (Third) (Second).all);
               Rest := Rest - Level_Size;
            else
               Table'Write
                 (S, Object.Third_Level (Third) (Second) (0 .. Rest - 1));
               Rest := 0;
            end if;

            Second := Second + 1;
         end loop;

         Third := Third + 1;
      end loop;
   end Save;

end Gela.Containers.Vectors.IO;
