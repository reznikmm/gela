--  Auto generated file. Don't edit

package Encodings.Maps.CP_857 is

   function Decode (Char : Character) return Wide_Character;
   pragma Inline (Decode);

   procedure Encode
     (Text        : in     Wide_String;
      Text_Last   :    out Natural;
      Result      :    out Raw_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.CP_857);

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.CP_857);

end Encodings.Maps.CP_857;
