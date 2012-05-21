------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                     http://www.ten15.org/wiki/Ada                        --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $TenDRA: asis-iterator.adb 2455 2006-06-24 19:22:06Z maxr $
--  Purpose:
--  Read-only iterator over tree of ASIS elements

with Asis.Gela;
with Asis.Gela.Contexts;
with Asis.Gela.Elements;
with Asis.Gela.Base_Lists;
with Asis.Gela.Properties;
with Asis.Gela.Compilations;

package body Asis.Iterator is
   package P renames Asis.Gela.Properties;

   ----------------------
   -- Traverse_Element --
   ----------------------

   procedure Traverse_Element
     (Element : in     Asis.Element;
      Control : in out Traverse_Control;
      State   : in out State_Information)
   is
      use Asis.Gela;

      C : constant Compilations.Compilation :=
        Contexts.Get_Compilation  (Element.Unit);

      procedure Walk_Element
        (Item    : in     Element_Index;
         Control : in out Traverse_Control;
         State   : in out State_Information);

      procedure Walk_Element
        (Item    : in     Element_Index;
         Kind    : in     P.Global_Kinds;
         Control : in out Traverse_Control;
         State   : in out State_Information);

      procedure Walk_List
        (List    : in     Element_Index;
         Control : in out Traverse_Control;
         State   : in out State_Information);

      ---------------
      -- Walk_List --
      ---------------

      procedure Walk_List
        (List    : in     Element_Index;
         Control : in out Traverse_Control;
         State   : in out State_Information)
      is
         Item : Element_Index := Elements.Get (C, List, P.Tail);
      begin
         for I in 1 .. Base_Lists.Length (C, List) loop
            Item := Elements.Get (C, Item, P.Next_Element);

            Walk_Element (Item, Control, State);

            exit when Control /= Continue;
         end loop;
      end Walk_List;

      ------------------
      -- Walk_Element --
      ------------------

      procedure Walk_Element
        (Item    : in     Element_Index;
         Control : in out Traverse_Control;
         State   : in out State_Information)
      is
         Kind  : constant P.Global_Kinds := Elements.Global_Kind (C, Item);
      begin
         Walk_Element (Item, Kind, Control, State);
      end Walk_Element;

      ------------------
      -- Walk_Element --
      ------------------

      procedure Walk_Element
        (Item    : in     Element_Index;
         Kind    : in     P.Global_Kinds;
         Control : in out Traverse_Control;
         State   : in out State_Information) is
      begin
         Pre_Operation ((Element.Unit, Item), Control, State);

         if Control = Continue then
            declare
               use type P.Global_Kinds;

               Child      : Element_Index;
               Child_Kind : P.Global_Kinds;
               List       : constant Traverse_List := P.Children (Kind);
            begin
               for I in List'Range loop
                  Child := Elements.Get (C, Item, List (I));

                  if Assigned (Child) then
                     Child_Kind := Elements.Global_Kind (C, Child);

                     if Child_Kind = P.Primary_List then
                        Walk_List (Child, Control, State);
                     else
                        Walk_Element (Child, Child_Kind, Control, State);
                     end if;
                  end if;

                  exit when Control /= Continue;
               end loop;
            end;

            if Control = Abandon_Siblings then
               Control := Continue;
            end if;
         end if;

         if Control = Continue then
            Post_Operation ((Element.Unit, Item), Control, State);
         end if;

         if Control = Abandon_Children then
            Control := Continue;
         end if;
      end Walk_Element;

   begin
      Check_Nil_Element (Element, "Traverse_Element");

      if Control /= Continue then
         return;
      end if;

      Walk_Element (Element.Index, Control, State);
   end Traverse_Element;

end Asis.Iterator;



------------------------------------------------------------------------------
--  Copyright (c) 2006, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
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
