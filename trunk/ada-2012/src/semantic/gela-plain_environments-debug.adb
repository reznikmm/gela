with Gela.Symbol_Sets;
procedure Gela.Plain_Environments.Debug
  (Self  : access Environment_Set;
   Index : Gela.Semantic_Types.Env_Index)
is
   procedure Puts (X : String);
   pragma Import (C, Puts, "puts");

   procedure Print_Region
     (Index : Region_Enum; List : Region_Item_Count);

   procedure Print (Item : Region_Item);

   procedure Print (Item : Region_Item) is
      use type Gela.Elements.Defining_Names.Defining_Name_Access;
      Set    : Gela.Symbol_Sets.Symbol_Set_Access;
      Symbol : Gela.Lexical_Types.Symbol;
   begin
      Set := Self.Context.Symbols;
      if Item.Name = null then
         Symbol := 0;
      else
         Symbol := Item.Name.Full_Name;
      end if;

      Puts ("  begin region " &
              Set.Image (Symbol).To_UTF_8_String & ASCII.NUL);
      Puts ("  end region" & ASCII.NUL);
   end Print;

   ------------------
   -- Print_Region --
   ------------------

   procedure Print_Region (Index : Region_Enum; List : Region_Item_Count) is
   begin
      Puts (Region_Enum'Image (Index) & ASCII.NUL);
      Self.Region.For_Each (List, Print'Access);
   end Print_Region;

   Env : Env_Item;
begin
   Puts ("begin Debug" & ASCII.NUL);

   if Index in Env_Item_Index then
      Env := Self.Env.Element (Index);

      for J in Env.Region_List'Range loop
         Print_Region (J, Env.Region_List (J));
      end loop;
   end if;

   Puts ("end Debug" & ASCII.NUL);
end Gela.Plain_Environments.Debug;
