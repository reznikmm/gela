--  Auto generated file. Don't edit
--  Read copyright and license at the end of this file

package body Encodings.Maps.ISO_8859_2 is

   Forward  : Forward_Map  (Character'Val(16#A1#) .. Character'Last) :=
     (Wide_Character'Val( 16#104#),
      Wide_Character'Val( 16#2D8#),
      Wide_Character'Val( 16#141#),
      Wide_Character'Val(  16#A4#),
      Wide_Character'Val( 16#13D#),
      Wide_Character'Val( 16#15A#),
      Wide_Character'Val(  16#A7#),
      Wide_Character'Val(  16#A8#),
      Wide_Character'Val( 16#160#),
      Wide_Character'Val( 16#15E#),
      Wide_Character'Val( 16#164#),
      Wide_Character'Val( 16#179#),
      Wide_Character'Val(  16#AD#),
      Wide_Character'Val( 16#17D#),
      Wide_Character'Val( 16#17B#),
      Wide_Character'Val(  16#B0#),
      Wide_Character'Val( 16#105#),
      Wide_Character'Val( 16#2DB#),
      Wide_Character'Val( 16#142#),
      Wide_Character'Val(  16#B4#),
      Wide_Character'Val( 16#13E#),
      Wide_Character'Val( 16#15B#),
      Wide_Character'Val( 16#2C7#),
      Wide_Character'Val(  16#B8#),
      Wide_Character'Val( 16#161#),
      Wide_Character'Val( 16#15F#),
      Wide_Character'Val( 16#165#),
      Wide_Character'Val( 16#17A#),
      Wide_Character'Val( 16#2DD#),
      Wide_Character'Val( 16#17E#),
      Wide_Character'Val( 16#17C#),
      Wide_Character'Val( 16#154#),
      Wide_Character'Val(  16#C1#),
      Wide_Character'Val(  16#C2#),
      Wide_Character'Val( 16#102#),
      Wide_Character'Val(  16#C4#),
      Wide_Character'Val( 16#139#),
      Wide_Character'Val( 16#106#),
      Wide_Character'Val(  16#C7#),
      Wide_Character'Val( 16#10C#),
      Wide_Character'Val(  16#C9#),
      Wide_Character'Val( 16#118#),
      Wide_Character'Val(  16#CB#),
      Wide_Character'Val( 16#11A#),
      Wide_Character'Val(  16#CD#),
      Wide_Character'Val(  16#CE#),
      Wide_Character'Val( 16#10E#),
      Wide_Character'Val( 16#110#),
      Wide_Character'Val( 16#143#),
      Wide_Character'Val( 16#147#),
      Wide_Character'Val(  16#D3#),
      Wide_Character'Val(  16#D4#),
      Wide_Character'Val( 16#150#),
      Wide_Character'Val(  16#D6#),
      Wide_Character'Val(  16#D7#),
      Wide_Character'Val( 16#158#),
      Wide_Character'Val( 16#16E#),
      Wide_Character'Val(  16#DA#),
      Wide_Character'Val( 16#170#),
      Wide_Character'Val(  16#DC#),
      Wide_Character'Val(  16#DD#),
      Wide_Character'Val( 16#162#),
      Wide_Character'Val(  16#DF#),
      Wide_Character'Val( 16#155#),
      Wide_Character'Val(  16#E1#),
      Wide_Character'Val(  16#E2#),
      Wide_Character'Val( 16#103#),
      Wide_Character'Val(  16#E4#),
      Wide_Character'Val( 16#13A#),
      Wide_Character'Val( 16#107#),
      Wide_Character'Val(  16#E7#),
      Wide_Character'Val( 16#10D#),
      Wide_Character'Val(  16#E9#),
      Wide_Character'Val( 16#119#),
      Wide_Character'Val(  16#EB#),
      Wide_Character'Val( 16#11B#),
      Wide_Character'Val(  16#ED#),
      Wide_Character'Val(  16#EE#),
      Wide_Character'Val( 16#10F#),
      Wide_Character'Val( 16#111#),
      Wide_Character'Val( 16#144#),
      Wide_Character'Val( 16#148#),
      Wide_Character'Val(  16#F3#),
      Wide_Character'Val(  16#F4#),
      Wide_Character'Val( 16#151#),
      Wide_Character'Val(  16#F6#),
      Wide_Character'Val(  16#F7#),
      Wide_Character'Val( 16#159#),
      Wide_Character'Val( 16#16F#),
      Wide_Character'Val(  16#FA#),
      Wide_Character'Val( 16#171#),
      Wide_Character'Val(  16#FC#),
      Wide_Character'Val(  16#FD#),
      Wide_Character'Val( 16#163#),
      Wide_Character'Val( 16#2D9#));

   Ranges   : Maps.Wide_Ranges  (1 ..  45) :=
     ((Wide_Character'Val(   16#0#),Wide_Character'Val(  16#A0#), 1),
      (Wide_Character'Val(  16#A4#),Wide_Character'Val(  16#A4#), 162),
      (Wide_Character'Val(  16#A7#),Wide_Character'Val(  16#A8#), 163),
      (Wide_Character'Val(  16#AD#),Wide_Character'Val(  16#AD#), 165),
      (Wide_Character'Val(  16#B0#),Wide_Character'Val(  16#B0#), 166),
      (Wide_Character'Val(  16#B4#),Wide_Character'Val(  16#B4#), 167),
      (Wide_Character'Val(  16#B8#),Wide_Character'Val(  16#B8#), 168),
      (Wide_Character'Val(  16#C1#),Wide_Character'Val(  16#C2#), 169),
      (Wide_Character'Val(  16#C4#),Wide_Character'Val(  16#C4#), 171),
      (Wide_Character'Val(  16#C7#),Wide_Character'Val(  16#C7#), 172),
      (Wide_Character'Val(  16#C9#),Wide_Character'Val(  16#C9#), 173),
      (Wide_Character'Val(  16#CB#),Wide_Character'Val(  16#CB#), 174),
      (Wide_Character'Val(  16#CD#),Wide_Character'Val(  16#CE#), 175),
      (Wide_Character'Val(  16#D3#),Wide_Character'Val(  16#D4#), 177),
      (Wide_Character'Val(  16#D6#),Wide_Character'Val(  16#D7#), 179),
      (Wide_Character'Val(  16#DA#),Wide_Character'Val(  16#DA#), 181),
      (Wide_Character'Val(  16#DC#),Wide_Character'Val(  16#DD#), 182),
      (Wide_Character'Val(  16#DF#),Wide_Character'Val(  16#DF#), 184),
      (Wide_Character'Val(  16#E1#),Wide_Character'Val(  16#E2#), 185),
      (Wide_Character'Val(  16#E4#),Wide_Character'Val(  16#E4#), 187),
      (Wide_Character'Val(  16#E7#),Wide_Character'Val(  16#E7#), 188),
      (Wide_Character'Val(  16#E9#),Wide_Character'Val(  16#E9#), 189),
      (Wide_Character'Val(  16#EB#),Wide_Character'Val(  16#EB#), 190),
      (Wide_Character'Val(  16#ED#),Wide_Character'Val(  16#EE#), 191),
      (Wide_Character'Val(  16#F3#),Wide_Character'Val(  16#F4#), 193),
      (Wide_Character'Val(  16#F6#),Wide_Character'Val(  16#F7#), 195),
      (Wide_Character'Val(  16#FA#),Wide_Character'Val(  16#FA#), 197),
      (Wide_Character'Val(  16#FC#),Wide_Character'Val(  16#FD#), 198),
      (Wide_Character'Val( 16#102#),Wide_Character'Val( 16#107#), 200),
      (Wide_Character'Val( 16#10C#),Wide_Character'Val( 16#111#), 206),
      (Wide_Character'Val( 16#118#),Wide_Character'Val( 16#11B#), 212),
      (Wide_Character'Val( 16#139#),Wide_Character'Val( 16#13A#), 216),
      (Wide_Character'Val( 16#13D#),Wide_Character'Val( 16#13E#), 218),
      (Wide_Character'Val( 16#141#),Wide_Character'Val( 16#144#), 220),
      (Wide_Character'Val( 16#147#),Wide_Character'Val( 16#148#), 224),
      (Wide_Character'Val( 16#150#),Wide_Character'Val( 16#151#), 226),
      (Wide_Character'Val( 16#154#),Wide_Character'Val( 16#155#), 228),
      (Wide_Character'Val( 16#158#),Wide_Character'Val( 16#15B#), 230),
      (Wide_Character'Val( 16#15E#),Wide_Character'Val( 16#165#), 234),
      (Wide_Character'Val( 16#16E#),Wide_Character'Val( 16#171#), 242),
      (Wide_Character'Val( 16#179#),Wide_Character'Val( 16#17E#), 246),
      (Wide_Character'Val( 16#2C7#),Wide_Character'Val( 16#2C7#), 252),
      (Wide_Character'Val( 16#2D8#),Wide_Character'Val( 16#2D9#), 253),
      (Wide_Character'Val( 16#2DB#),Wide_Character'Val( 16#2DB#), 255),
      (Wide_Character'Val( 16#2DD#),Wide_Character'Val( 16#2DD#), 256));

   Backward : Maps.Backward_Map (1 ..  256) :=
     (Character'Val( 16#0#),
      Character'Val( 16#1#),
      Character'Val( 16#2#),
      Character'Val( 16#3#),
      Character'Val( 16#4#),
      Character'Val( 16#5#),
      Character'Val( 16#6#),
      Character'Val( 16#7#),
      Character'Val( 16#8#),
      Character'Val( 16#9#),
      Character'Val( 16#A#),
      Character'Val( 16#B#),
      Character'Val( 16#C#),
      Character'Val( 16#D#),
      Character'Val( 16#E#),
      Character'Val( 16#F#),
      Character'Val(16#10#),
      Character'Val(16#11#),
      Character'Val(16#12#),
      Character'Val(16#13#),
      Character'Val(16#14#),
      Character'Val(16#15#),
      Character'Val(16#16#),
      Character'Val(16#17#),
      Character'Val(16#18#),
      Character'Val(16#19#),
      Character'Val(16#1A#),
      Character'Val(16#1B#),
      Character'Val(16#1C#),
      Character'Val(16#1D#),
      Character'Val(16#1E#),
      Character'Val(16#1F#),
      Character'Val(16#20#),
      Character'Val(16#21#),
      Character'Val(16#22#),
      Character'Val(16#23#),
      Character'Val(16#24#),
      Character'Val(16#25#),
      Character'Val(16#26#),
      Character'Val(16#27#),
      Character'Val(16#28#),
      Character'Val(16#29#),
      Character'Val(16#2A#),
      Character'Val(16#2B#),
      Character'Val(16#2C#),
      Character'Val(16#2D#),
      Character'Val(16#2E#),
      Character'Val(16#2F#),
      Character'Val(16#30#),
      Character'Val(16#31#),
      Character'Val(16#32#),
      Character'Val(16#33#),
      Character'Val(16#34#),
      Character'Val(16#35#),
      Character'Val(16#36#),
      Character'Val(16#37#),
      Character'Val(16#38#),
      Character'Val(16#39#),
      Character'Val(16#3A#),
      Character'Val(16#3B#),
      Character'Val(16#3C#),
      Character'Val(16#3D#),
      Character'Val(16#3E#),
      Character'Val(16#3F#),
      Character'Val(16#40#),
      Character'Val(16#41#),
      Character'Val(16#42#),
      Character'Val(16#43#),
      Character'Val(16#44#),
      Character'Val(16#45#),
      Character'Val(16#46#),
      Character'Val(16#47#),
      Character'Val(16#48#),
      Character'Val(16#49#),
      Character'Val(16#4A#),
      Character'Val(16#4B#),
      Character'Val(16#4C#),
      Character'Val(16#4D#),
      Character'Val(16#4E#),
      Character'Val(16#4F#),
      Character'Val(16#50#),
      Character'Val(16#51#),
      Character'Val(16#52#),
      Character'Val(16#53#),
      Character'Val(16#54#),
      Character'Val(16#55#),
      Character'Val(16#56#),
      Character'Val(16#57#),
      Character'Val(16#58#),
      Character'Val(16#59#),
      Character'Val(16#5A#),
      Character'Val(16#5B#),
      Character'Val(16#5C#),
      Character'Val(16#5D#),
      Character'Val(16#5E#),
      Character'Val(16#5F#),
      Character'Val(16#60#),
      Character'Val(16#61#),
      Character'Val(16#62#),
      Character'Val(16#63#),
      Character'Val(16#64#),
      Character'Val(16#65#),
      Character'Val(16#66#),
      Character'Val(16#67#),
      Character'Val(16#68#),
      Character'Val(16#69#),
      Character'Val(16#6A#),
      Character'Val(16#6B#),
      Character'Val(16#6C#),
      Character'Val(16#6D#),
      Character'Val(16#6E#),
      Character'Val(16#6F#),
      Character'Val(16#70#),
      Character'Val(16#71#),
      Character'Val(16#72#),
      Character'Val(16#73#),
      Character'Val(16#74#),
      Character'Val(16#75#),
      Character'Val(16#76#),
      Character'Val(16#77#),
      Character'Val(16#78#),
      Character'Val(16#79#),
      Character'Val(16#7A#),
      Character'Val(16#7B#),
      Character'Val(16#7C#),
      Character'Val(16#7D#),
      Character'Val(16#7E#),
      Character'Val(16#7F#),
      Character'Val(16#80#),
      Character'Val(16#81#),
      Character'Val(16#82#),
      Character'Val(16#83#),
      Character'Val(16#84#),
      Character'Val(16#85#),
      Character'Val(16#86#),
      Character'Val(16#87#),
      Character'Val(16#88#),
      Character'Val(16#89#),
      Character'Val(16#8A#),
      Character'Val(16#8B#),
      Character'Val(16#8C#),
      Character'Val(16#8D#),
      Character'Val(16#8E#),
      Character'Val(16#8F#),
      Character'Val(16#90#),
      Character'Val(16#91#),
      Character'Val(16#92#),
      Character'Val(16#93#),
      Character'Val(16#94#),
      Character'Val(16#95#),
      Character'Val(16#96#),
      Character'Val(16#97#),
      Character'Val(16#98#),
      Character'Val(16#99#),
      Character'Val(16#9A#),
      Character'Val(16#9B#),
      Character'Val(16#9C#),
      Character'Val(16#9D#),
      Character'Val(16#9E#),
      Character'Val(16#9F#),
      Character'Val(16#A0#),
      Character'Val(16#A4#),
      Character'Val(16#A7#),
      Character'Val(16#A8#),
      Character'Val(16#AD#),
      Character'Val(16#B0#),
      Character'Val(16#B4#),
      Character'Val(16#B8#),
      Character'Val(16#C1#),
      Character'Val(16#C2#),
      Character'Val(16#C4#),
      Character'Val(16#C7#),
      Character'Val(16#C9#),
      Character'Val(16#CB#),
      Character'Val(16#CD#),
      Character'Val(16#CE#),
      Character'Val(16#D3#),
      Character'Val(16#D4#),
      Character'Val(16#D6#),
      Character'Val(16#D7#),
      Character'Val(16#DA#),
      Character'Val(16#DC#),
      Character'Val(16#DD#),
      Character'Val(16#DF#),
      Character'Val(16#E1#),
      Character'Val(16#E2#),
      Character'Val(16#E4#),
      Character'Val(16#E7#),
      Character'Val(16#E9#),
      Character'Val(16#EB#),
      Character'Val(16#ED#),
      Character'Val(16#EE#),
      Character'Val(16#F3#),
      Character'Val(16#F4#),
      Character'Val(16#F6#),
      Character'Val(16#F7#),
      Character'Val(16#FA#),
      Character'Val(16#FC#),
      Character'Val(16#FD#),
      Character'Val(16#C3#),
      Character'Val(16#E3#),
      Character'Val(16#A1#),
      Character'Val(16#B1#),
      Character'Val(16#C6#),
      Character'Val(16#E6#),
      Character'Val(16#C8#),
      Character'Val(16#E8#),
      Character'Val(16#CF#),
      Character'Val(16#EF#),
      Character'Val(16#D0#),
      Character'Val(16#F0#),
      Character'Val(16#CA#),
      Character'Val(16#EA#),
      Character'Val(16#CC#),
      Character'Val(16#EC#),
      Character'Val(16#C5#),
      Character'Val(16#E5#),
      Character'Val(16#A5#),
      Character'Val(16#B5#),
      Character'Val(16#A3#),
      Character'Val(16#B3#),
      Character'Val(16#D1#),
      Character'Val(16#F1#),
      Character'Val(16#D2#),
      Character'Val(16#F2#),
      Character'Val(16#D5#),
      Character'Val(16#F5#),
      Character'Val(16#C0#),
      Character'Val(16#E0#),
      Character'Val(16#D8#),
      Character'Val(16#F8#),
      Character'Val(16#A6#),
      Character'Val(16#B6#),
      Character'Val(16#AA#),
      Character'Val(16#BA#),
      Character'Val(16#A9#),
      Character'Val(16#B9#),
      Character'Val(16#DE#),
      Character'Val(16#FE#),
      Character'Val(16#AB#),
      Character'Val(16#BB#),
      Character'Val(16#D9#),
      Character'Val(16#F9#),
      Character'Val(16#DB#),
      Character'Val(16#FB#),
      Character'Val(16#AC#),
      Character'Val(16#BC#),
      Character'Val(16#AF#),
      Character'Val(16#BF#),
      Character'Val(16#AE#),
      Character'Val(16#BE#),
      Character'Val(16#B7#),
      Character'Val(16#A2#),
      Character'Val(16#FF#),
      Character'Val(16#B2#),
      Character'Val(16#BD#));

   function Decode (Char : Character) return Wide_Character is
   begin
      return Decode (Char, Forward);
   end Decode;

   procedure Decode
     (Text        : in     Raw_String;
      Text_Last   :    out Natural;
      Result      :    out Wide_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.ISO_8859_2)
   is
   begin
      Decode (Text, Text_Last, Result, Result_Last, Forward);
   end Decode;

   procedure Encode
     (Text        : in     Wide_String;
      Text_Last   :    out Natural;
      Result      :    out Raw_String;
      Result_Last :    out Natural;
      Map         : in     Encoding := Encodings.ISO_8859_2)
   is
   begin
      Encode (Text, Text_Last, Result, Result_Last,
              Ranges, Backward);
   end Encode;

begin
   Encoder_List (Encodings.ISO_8859_2) := Encode'Access;
   Decoder_List (Encodings.ISO_8859_2) := Decode'Access;
end Encodings.Maps.ISO_8859_2;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------