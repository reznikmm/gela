------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Text_IO;
with League.Strings.Hash;

package body AG_Tools.Contexts is

   --------------
   -- Add_With --
   --------------

   procedure Add_With
     (Self : access Context;
      Name : Wide_Wide_String;
      Kind : Unit_Kinds := Body_Unit) is
   begin
      Self.Add_With (League.Strings.To_Universal_String (Name), Kind);
   end Add_With;

   --------------
   -- Add_With --
   --------------

   procedure Add_With
     (Self : access Context;
      Name : League.Strings.Universal_String;
      Kind : Unit_Kinds := Body_Unit)
   is
      Withs : With_Records renames Self.Withs;
   begin
      if Name.Is_Empty or Withs (Kind).Index (Name) > 0 then
         return;
      elsif Kind = Spec_Unit then
         declare
            Body_Index : constant Natural := Withs (Body_Unit).Index (Name);
         begin
            if Body_Index > 0 then
               Withs (Body_Unit).Replace
                 (Body_Index, League.Strings.Empty_Universal_String);
            end if;
         end;
      elsif Withs (Spec_Unit).Index (Name) > 0 then
         return;
      end if;

      Withs (Kind).Append (Name);
   end Add_With;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Attr) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return League.Strings.Hash (Self.Origin) +
        Ada.Containers.Hash_Type (Self.Decl);
   end Hash;

   ------------------
   -- Print_Withes --
   ------------------

   procedure Print_Withes
     (Self : access Context;
      Kind : Unit_Kinds)
   is
      Withs : With_Records renames Self.Withs;
   begin
      Ada.Text_IO.Put_Line ("--  Auto generated file. DO NOT EDIT!!!");
      for J in 1 .. Withs (Kind).Length loop
         declare
            Item : constant League.Strings.Universal_String :=
              Withs (Kind).Element (J);
         begin
            if not Item.Is_Empty then
               Ada.Text_IO.Put ("with ");
               Ada.Text_IO.Put (Item.To_UTF_8_String);
               Ada.Text_IO.Put_Line (";");
            end if;
         end;
      end loop;
   end Print_Withes;

end AG_Tools.Contexts;
