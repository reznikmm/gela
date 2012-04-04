------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

package body Gela.Grammars.Rule_Templates is

   --------------------
   -- Attribute_Name --
   --------------------

   function Attribute_Name
     (Self  : Rule_Template;
      Index : Positive)
      return S.Universal_String
   is
   begin
      return Self.Attrs.Element (Index);
   end Attribute_Name;

   -----------
   -- Count --
   -----------

   function Count (Self : Rule_Template) return Natural is
   begin
      return Self.Parts.Length;
   end Count;

   ------------
   -- Create --
   ------------

   function Create
     (Text : S.Universal_String)
      return Rule_Template
   is
      procedure Append (Part, Attr : S.Universal_String);

      package String_Sets is new Ada.Containers.Ordered_Sets
        (S.Universal_String, S."<", S."=");

      Ready : String_Sets.Set;
      Parts : League.String_Vectors.Universal_String_Vector;
      Attrs : League.String_Vectors.Universal_String_Vector;

      procedure Append (Part, Attr : S.Universal_String) is
      begin
         Parts.Append (Part);
         Attrs.Append (Attr);
      end Append;

      Items : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split ('$');
   begin
      for J in 2 .. Items.Length loop
         declare
            To    : Natural;
            Point : Natural;
            Name  : S.Universal_String;
            Item  : constant S.Universal_String := Items.Element (J);
         begin
            if Item.Starts_With ("{") then
               To    := Item.Index ('}');
               Point := Item.Index ('.');

               if To > 0 and Point < To then
                  Name := Item.Slice (1, To);

                  if not Ready.Contains (Name) then
                     Ready.Insert (Name);

                     Append
                       (Part => Item.Slice (2, Point - 1),
                        Attr => Item.Slice (Point + 1, To - 1));
                  end if;
               end if;
            end if;
         end;
      end loop;

      return (Text  => Text,
              Parts => Parts,
              Attrs => Attrs);
   end Create;

   ---------------
   -- Part_Name --
   ---------------

   function Part_Name
     (Self  : Rule_Template;
      Index : Positive)
      return S.Universal_String
   is
   begin
      return Self.Parts.Element (Index);
   end Part_Name;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Self   : Rule_Template;
      Values : League.String_Vectors.Universal_String_Vector)
     return S.Universal_String
   is
      use type S.Universal_String;

      package Maps is new Ada.Containers.Ordered_Maps
        (S.Universal_String, S.Universal_String);

      Map   : Maps.Map;
      Items : League.String_Vectors.Universal_String_Vector :=
        Self.Text.Split ('$');
   begin
      for J in 1 .. Self.Count loop
         Map.Insert
           (Self.Part_Name (J) & "." & Self.Attribute_Name (J),
            Values.Element (J));
      end loop;

      for J in 2 .. Items.Length loop
         declare
            To    : Natural;
            Point : Natural;
            Name  : S.Universal_String;
            Item  : S.Universal_String := Items.Element (J);
         begin
            if Item.Starts_With ("{") then
               To    := Item.Index ('}');
               Point := Item.Index ('.');

               if To > 0 and Point < To then
                  Name := Item.Slice (2, To - 1);

                  Item.Replace (1, To, Map.Element (Name));

                  Items.Replace (J, Item);
               end if;
            end if;
         end;
      end loop;

      return Items.Join ("");
   end Substitute;

   ----------
   -- Text --
   ----------

   function Text  (Self : Rule_Template) return S.Universal_String is
   begin
      return Self.Text;
   end Text;

end Gela.Grammars.Rule_Templates;
