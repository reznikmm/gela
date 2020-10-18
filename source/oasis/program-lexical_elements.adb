--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Lexical_Elements is

   -----------
   -- First --
   -----------

   function First
     (Self : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Element (Self.First_Index);
   end First;

   ----------
   -- Last --
   ----------

   function Last
     (Self : Lexical_Element_Vector'Class)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Element (Self.Last_Index);
   end Last;

end Program.Lexical_Elements;
