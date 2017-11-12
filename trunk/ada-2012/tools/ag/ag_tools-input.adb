------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--          Library for dealing with grammars for Gela project,             --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with League.String_Vectors;
with League.Strings.Hash;

with Anagram.Grammars.Reader;
with Anagram.Grammars.Constructors;

with AG_Tools.Check_Ordered; use AG_Tools.Check_Ordered;

package body AG_Tools.Input is

   type NT_List is array
     (Anagram.Grammars.Non_Terminal_Index range <>) of Boolean;

   type NT_List_Access is access all NT_List;

   type NT_NT_Map is array
     (Anagram.Grammars.Non_Terminal_Index range <>,
      Anagram.Grammars.Non_Terminal_Index range <>) of Boolean;

   type NT_NT_Map_Access is access all NT_NT_Map;

   function Macro_Reference
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production)
      return Anagram.Grammars.Non_Terminal_Count;
   --  If Prod is just reference to another NT, return NT.Index.
   --  Return 0 otherwise.

   function List_Reference
     (G    : Anagram.Grammars.Grammar;
      Part : Anagram.Grammars.Part)
      return Anagram.Grammars.Non_Terminal_Count;
   --  If Part is reference to list, return NT.Index of the list
   --  Return 0 otherwise

   procedure Look_For_List
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production);
   --  If any port of Prod is reference to list of some NT then
   --  mark With_List (NT) as True

   procedure Copy
     (G : Anagram.Grammars.Grammar;
      V : in out Anagram.Grammars.Constructors.Constructor;
      Is_Concrete : NT_List);

   procedure Copy_Attr
     (G      : Anagram.Grammars.Grammar;
      V      : in out Anagram.Grammars.Constructors.Constructor;
      Child  : Anagram.Grammars.Non_Terminal_Index;
      Parent : Anagram.Grammars.Non_Terminal_Index;
      Done   : in out League.String_Vectors.Universal_String_Vector);

   procedure Copy_Productions
     (G : Anagram.Grammars.Grammar;
      V : in out Anagram.Grammars.Constructors.Constructor;
      From : Anagram.Grammars.Production_Index;
      To   : Anagram.Grammars.Production_Count;
      PL : in out Anagram.Grammars.Constructors.Production_List);

   procedure Add_Option
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production;
      Part : Anagram.Grammars.Part);
   --  Remember Prod.Part as option

   package String_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => League.Strings.Universal_String,
      Hash                => League.Strings.Hash,
      Equivalent_Elements => League.Strings."=",
      "="                 => League.Strings."=");

   G            : Anagram.Grammars.Grammar_Access;
   Concrete     : NT_List_Access;
   With_List    : NT_List_Access;
   Is_Implement : NT_NT_Map_Access;
   Option_Set   : String_Sets.Set;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production;
      Part : Anagram.Grammars.Part)
   is
      use type League.Strings.Universal_String;
      NT   : Anagram.Grammars.Non_Terminal renames
        G.Non_Terminal (Prod.Parent);
      Name : constant League.Strings.Universal_String :=
        NT.Name & " " & Prod.Name & " " & Part.Name;
   begin
      Option_Set.Insert (Name);
   end Add_Option;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (G : Anagram.Grammars.Grammar;
      V : in out Anagram.Grammars.Constructors.Constructor;
      Is_Concrete : NT_List) is
   begin
      for J in G.Terminal'Range loop
         declare
            T : Anagram.Grammars.Terminal renames G.Terminal (J);
         begin
            V.Create_Terminal (T.Image, T.Precedence);

            for A in T.First_Attribute .. T.Last_Attribute loop
               V.Create_Attribute_Declaration
                 (T.Image,
                  G.Declaration (A).Name,
                  G.Declaration (A).Type_Name);
            end loop;
         end;
      end loop;

      for J in G.Non_Terminal'Range loop
         declare
            N  : Anagram.Grammars.Non_Terminal renames G.Non_Terminal (J);
            PL : Anagram.Grammars.Constructors.Production_List :=
              V.Create_Production_List;
         begin
            Copy_Productions (G, V, N.First, N.Last, PL);

            if N.Is_List then
               V.Create_List (N.Name, PL);
            else
               V.Create_Non_Terminal (N.Name, PL);
            end if;

            for A in N.First_Attribute .. N.Last_Attribute loop
               V.Create_Attribute_Declaration
                 (N.Name,
                  G.Declaration (A).Name,
                  G.Declaration (A).Is_Inherited,
                  G.Declaration (A).Type_Name);
            end loop;

            for K in N.First .. N.Last loop
               declare
                  S : Anagram.Grammars.Production renames G.Production (K);
               begin
                  for Y in S.First_Rule .. S.Last_Rule loop
                     declare
                        R : Anagram.Grammars.Rule renames G.Rule (Y);
                     begin
                        V.Create_Rule (N.Name, S.Name, R.Text);

                        if not Is_Concrete (N.Index) then
                           raise Constraint_Error;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end Copy;

   ---------------
   -- Copy_Attr --
   ---------------

   procedure Copy_Attr
     (G      : Anagram.Grammars.Grammar;
      V      : in out Anagram.Grammars.Constructors.Constructor;
      Child  : Anagram.Grammars.Non_Terminal_Index;
      Parent : Anagram.Grammars.Non_Terminal_Index;
      Done   : in out League.String_Vectors.Universal_String_Vector)
   is
      X : Anagram.Grammars.Non_Terminal renames G.Non_Terminal (Child);
      Y : Anagram.Grammars.Non_Terminal renames G.Non_Terminal (Parent);
   begin
      for K in Y.First_Attribute .. Y.Last_Attribute loop
         declare
            YA : Anagram.Grammars.Attribute_Declaration renames
              G.Declaration (K);
         begin
            if Done.Index (YA.Name) > 0 then
               goto Continue;
            else
               Done.Append (YA.Name);
            end if;

            for J in X.First_Attribute .. X.Last_Attribute loop
               declare
                  use type League.Strings.Universal_String;

                  XA : Anagram.Grammars.Attribute_Declaration renames
                    G.Declaration (J);
                  Text : League.Strings.Universal_String;
               begin
                  if XA.Name = YA.Name then
                     Text.Append ("${");
                     Text.Append (X.Name);
                     Text.Append (".");
                     Text.Append (XA.Name);
                     Text.Append ("} = ${");
                     Text.Append (Y.Name);
                     Text.Append (".");
                     Text.Append (YA.Name);
                     Text.Append ("}");
                     V.Create_Rule (Y.Name, X.Name, Text);
                  end if;
               end;
            end loop;

            <<Continue>>
         end;
      end loop;
   end Copy_Attr;

   ----------------------
   -- Copy_Productions --
   ----------------------

   procedure Copy_Productions
     (G : Anagram.Grammars.Grammar;
      V : in out Anagram.Grammars.Constructors.Constructor;
      From : Anagram.Grammars.Production_Index;
      To   : Anagram.Grammars.Production_Count;
      PL : in out Anagram.Grammars.Constructors.Production_List)
   is
   begin
      for K in From .. To loop
         declare
            S : Anagram.Grammars.Production renames G.Production (K);
            P : Anagram.Grammars.Constructors.Production :=
              V.Create_Production (S.Name, S.Precedence);
         begin
            for X in S.First .. S.Last loop
               declare
                  R : Anagram.Grammars.Part renames G.Part (X);
               begin
                  if R.Is_Terminal_Reference then
                     P.Add (V.Create_Terminal_Reference
                            (R.Name,
                               G.Terminal (R.Denote).Image));
                  elsif R.Is_List_Reference then
                     P.Add (V.Create_List_Reference
                            (R.Name,
                               G.Non_Terminal (R.Denote).Name));
                  elsif R.Is_Non_Terminal_Reference then
                     P.Add (V.Create_Non_Terminal_Reference
                            (R.Name,
                               G.Non_Terminal (R.Denote).Name));
                  else
                     --  remove Option from grammar by replacing its
                     --  nested items
                     declare
                        Nested_Production : Anagram.Grammars.Production renames
                          G.Production (R.First);
                        Nested_Part : Anagram.Grammars.Part renames
                          G.Part (Nested_Production.First);
                     begin
                        Add_Option
                          (G, G.Production (R.Parent), Nested_Part);

                        if Nested_Part.Is_Terminal_Reference then
                           P.Add (V.Create_Terminal_Reference
                                  (Nested_Part.Name,
                                     G.Terminal (Nested_Part.Denote).Image));
                        elsif Nested_Part.Is_List_Reference then
                           P.Add (V.Create_List_Reference
                                  (Nested_Part.Name,
                                     G.Non_Terminal
                                       (Nested_Part.Denote).Name));
                        elsif Nested_Part.Is_Non_Terminal_Reference then
                           P.Add (V.Create_Non_Terminal_Reference
                                  (Nested_Part.Name,
                                     G.Non_Terminal
                                       (Nested_Part.Denote).Name));
                        else
                           raise Constraint_Error with "option in option";
                        end if;
                     end;
                  end if;
               end;
            end loop;

            PL.Add (P);
         end;
      end loop;
   end Copy_Productions;

   -------------
   -- Grammar --
   -------------

   function Grammar return Anagram.Grammars.Grammar_Access is
   begin
      return G;
   end Grammar;

   --------------
   -- Has_List --
   --------------

   function Has_List
     (NT : Anagram.Grammars.Non_Terminal_Index) return Boolean is
   begin
      return With_List (NT);
   end Has_List;
   ---------------
   -- Implement --
   ---------------

   function Implement
     (X, Y : Anagram.Grammars.Non_Terminal_Index) return Boolean is
   begin
      return Is_Implement (X, Y);
   end Implement;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (File_Name : String) is
      use type Anagram.Grammars.Production_Count;
      use type Anagram.Grammars.Non_Terminal_Count;

      F : constant Anagram.Grammars.Grammar :=
        Anagram.Grammars.Reader.Read (File_Name, Tail_List => True);
   begin
      Concrete := new NT_List'(F.Non_Terminal'Range => False);
      With_List := new NT_List'(F.Non_Terminal'Range => False);
      Is_Implement := new NT_NT_Map'(F.Non_Terminal'Range =>
                                       (F.Non_Terminal'Range => False));

      for NT of F.Non_Terminal loop
         if NT.First = NT.Last then
            if Macro_Reference (F, F.Production (NT.First)) = 0 then
               Concrete (NT.Index) := True;
            end if;
         else
            for Prod of F.Production (NT.First .. NT.Last) loop
               if Macro_Reference (F, F.Production (NT.First)) = 0 then
                  Ada.Text_IO.Put_Line (NT.Name.To_UTF_8_String);
                  raise Constraint_Error;
               end if;
            end loop;
         end if;
      end loop;

      for NT of F.Non_Terminal loop
         if not NT.Is_List then
            for Prod of F.Production (NT.First .. NT.Last) loop
               declare
                  Ref : constant Anagram.Grammars.Non_Terminal_Count :=
                    Macro_Reference (F, Prod);
               begin
                  if Ref /= 0 then
                     Is_Implement (Ref, NT.Index) := True;
                  end if;
                  Look_For_List (F, Prod);
               end;
            end loop;
         end if;
      end loop;

      declare
         V : Anagram.Grammars.Constructors.Constructor;
      begin
         Copy (F, V, Concrete.all);

         for Parent in Is_Implement'Range (2) loop
            for Child in Is_Implement'Range (1) loop
               if Is_Implement (Child, Parent) then
                  declare
                     Done : League.String_Vectors.Universal_String_Vector;
                  begin
                     Copy_Attr (F, V, Child, Parent, Done);
                  end;
               end if;
            end loop;
         end loop;

         G := new Anagram.Grammars.Grammar'(V.Complete);
      end;

      Check;
      --  Check if G is ordered
   end Initialize;

   -----------------
   -- Is_Concrete --
   -----------------

   function Is_Concrete
     (NT : Anagram.Grammars.Non_Terminal_Index) return Boolean is
   begin
      return Concrete (NT);
   end Is_Concrete;

   ---------------
   -- Is_Option --
   ---------------

   function Is_Option
     (G    : Anagram.Grammars.Grammar;
      Part : Anagram.Grammars.Part) return Boolean
   is
      use type League.Strings.Universal_String;
      Prod : Anagram.Grammars.Production renames G.Production (Part.Parent);
      NT   : Anagram.Grammars.Non_Terminal renames
        G.Non_Terminal (Prod.Parent);
      Name : constant League.Strings.Universal_String :=
        NT.Name & " " & Prod.Name & " " & Part.Name;
   begin
      return Option_Set.Contains (Name);
   end Is_Option;

   --------------------
   -- List_Reference --
   --------------------

   function List_Reference
     (G    : Anagram.Grammars.Grammar;
      Part : Anagram.Grammars.Part)
      return Anagram.Grammars.Non_Terminal_Count is
   begin
      if Part.Is_List_Reference then
         declare
            NT   : Anagram.Grammars.Non_Terminal renames
              G.Non_Terminal (Part.Denote);
         begin
            return List_Item (G, NT);
         end;
      end if;

      return 0;
   end List_Reference;

   -------------------
   -- Look_For_List --
   -------------------

   procedure Look_For_List
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production)
   is
      use type Anagram.Grammars.Non_Terminal_Count;
      List : Anagram.Grammars.Non_Terminal_Count;
   begin
      for Part of G.Part (Prod.First .. Prod.Last) loop
         List := List_Reference (G, Part);
         if List /= 0 then
            With_List (List) := True;
         end if;
      end loop;
   end Look_For_List;

   ---------------------
   -- Macro_Reference --
   ---------------------

   function Macro_Reference
     (G    : Anagram.Grammars.Grammar;
      Prod : Anagram.Grammars.Production)
      return Anagram.Grammars.Non_Terminal_Count
   is
      use type League.Strings.Universal_String;
      use type Anagram.Grammars.Part_Count;
   begin
      if Prod.First /= Prod.Last then
         return 0;
      end if;

      declare
         Part : Anagram.Grammars.Part renames G.Part (Prod.First);
      begin
         if not Part.Is_Non_Terminal_Reference then
            return 0;
         elsif G.Non_Terminal (Part.Denote).Name /= Part.Name then
            return 0;
         else
            return Part.Denote;
         end if;
      end;
   end Macro_Reference;

   ---------------------
   -- Macro_Reference --
   ---------------------

   function Macro_Reference
     (Prod : Anagram.Grammars.Production)
      return Anagram.Grammars.Non_Terminal_Count is
   begin
      return Macro_Reference (G.all, Prod);
   end Macro_Reference;

end AG_Tools.Input;
