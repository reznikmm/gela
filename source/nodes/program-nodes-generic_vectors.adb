--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Generic_Vectors is

   ------------
   -- Create --
   ------------

   function Create
     (Each : Program.Element_Vectors.Iterators.Forward_Iterator'Class)
      return Vector
   is
      Elements, Tokens : Natural := 0;
      Has_Tokens : Boolean := False;
      Index : Positive := 1;
   begin
      for J in Each loop
         Elements := Elements + 1;

         if J.Delimiter.Assigned then
            Has_Tokens := True;
         end if;
      end loop;

      if Has_Tokens then
         Tokens := Elements - 1;
      end if;

      return Result : Vector (Elements, Tokens) do
         for J in Each loop
            Result.Element_List (Index) := J.Element;

            if Index in Result.Token_List'Range then
               Result.Token_List (Index) := J.Delimiter;
            end if;

            Index := Index + 1;
         end loop;
      end return;
   end Create;

   ---------------
   -- Delimiter --
   ---------------

   overriding function Delimiter
     (Self : Vector; Index : Positive)
        return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      if Index in Self.Token_List'Range then
         return Self.Token_List (Index);
      else
         return null;
      end if;
   end Delimiter;

   -------------
   -- Element --
   -------------

   overriding function Element (Self : Vector; Index : Positive)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Element_List (Index);
   end Element;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Vector) return Positive is
   begin
      return Self.Elements;
   end Get_Length;

end Program.Nodes.Generic_Vectors;
