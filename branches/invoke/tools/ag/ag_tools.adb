with League.String_Vectors;

package body AG_Tools is

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name
     (Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Comp     : League.String_Vectors.Universal_String_Vector;
      Pkg_Name : League.Strings.Universal_String;
   begin
      Comp := Name.Split ('.');

      if Comp.Length = 1 then
         return League.Strings.Empty_Universal_String;
      end if;

      Comp.Replace (Comp.Length, League.Strings.Empty_Universal_String);
      Pkg_Name := Comp.Join ('.');
      return Pkg_Name.Slice (1, Pkg_Name.Length - 1);
   end Package_Name;

   ------------
   -- Plural --
   ------------

   function Plural
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Ada_Text : constant League.Strings.Universal_String := To_Ada (Text);
   begin
      if Text.Ends_With ("s") or else
        Text.Ends_With ("x") or else
        Text.Ends_With ("sh") or else
        Text.Ends_With ("ch") or else
        Text.Ends_With ("o")
      then
         return Ada_Text & "es";
      elsif Text.Ends_With ("y") and then not
        (Text.Ends_With ("ay") or else
         Text.Ends_With ("ey") or else
         Text.Ends_With ("iy") or else
         Text.Ends_With ("oy") or else
         Text.Ends_With ("uy"))
      then
         return Ada_Text.Slice (1, Ada_Text.Length - 1) & "ies";
      elsif Text.Ends_With ("f") then
         return Ada_Text.Slice (1, Ada_Text.Length - 1) & "ves";
      elsif Text.Ends_With ("fe") then
         return Ada_Text.Slice (1, Ada_Text.Length - 2) & "ves";
      else
         return Ada_Text & "s";
      end if;
   end Plural;

   -----------------
   -- Return_Type --
   -----------------

   function Return_Type
     (G    : Gela.Grammars.Grammar;
      Part : Gela.Grammars.Part)
      return League.Strings.Universal_String
   is
      use Gela.Grammars;

      Result : League.Strings.Universal_String;
   begin
      if Part.Is_Option then
         if Part.First /= Part.Last then
            raise Constraint_Error;
         end if;

         declare
            Prod : Gela.Grammars.Production renames
              G.Production (Part.First);
         begin
            if Prod.First /= Prod.Last then
               raise Constraint_Error;
            end if;

            return Return_Type (G, G.Part (Prod.First));
         end;

      elsif Part.Is_List_Reference then
         declare
            NT : Gela.Grammars.Non_Terminal renames
              G.Non_Terminal (Part.Denote);
            Prod : Gela.Grammars.Production renames
              G.Production (NT.First);
         begin
            Result := Return_Type (G, G.Part (Prod.First + 1));
            Result.Append ("_Sequence");
            return Result;
         end;
      elsif Part.Is_Terminal_Reference then
         Result.Append ("Token");
      else
         Result := To_Ada (G.Non_Terminal (Part.Denote).Name);
      end if;

      return Result;
   end Return_Type;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List : League.String_Vectors.Universal_String_Vector;
      Piece : League.Strings.Universal_String;
   begin
      List := Text.Split ('_');
      for J in 1 .. List.Length loop
         Piece := List.Element (J);
         Piece.Replace (1, 1, Piece.Slice (1, 1).To_Uppercase);
         List.Replace (J, Piece);
      end loop;

      return List.Join ('_');
   end To_Ada;

end AG_Tools;
