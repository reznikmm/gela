package body Asis.Gela.Strings is
   use Wide_Character_Vectors;
   package X is new Arrays (Positive, Wide_String);
   use X;

   ---------
   -- Add --
   ---------

   procedure Add
     (Object : in out Wide_Character_Vectors.Vector;
      Text   : in     Wide_String;
      Index  :    out Positive)
   is
      Length_Char : constant Wide_Character :=
        Wide_Character'Val (Text'Length);
   begin
      Add (Object, Length_Char, Index);
      Add (Object, Text);
   end Add;

   ---------
   -- Get --
   ---------

   procedure Get
     (Object : in     Wide_Character_Vectors.Vector;
      Text   :    out Wide_String;
      Index  : in     Positive)
   is
   begin
      Get (Object, Index, Text);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Object : Wide_Character_Vectors.Vector;
      Index  : Positive)
      return Wide_String
   is
      Length : constant Natural := Get_Length (Object, Index);
      Result : Wide_String (1 .. Length);
   begin
      Get (Object, Index, Result);
      return Result;
   end Get;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Object : Wide_Character_Vectors.Vector;
      Index  : Positive)
      return Natural
   is
      Length_Char : constant Wide_Character := Get (Object, Index);
   begin
      return Wide_Character'Pos (Length_Char);
   end Get_Length;

end Asis.Gela.Strings;
