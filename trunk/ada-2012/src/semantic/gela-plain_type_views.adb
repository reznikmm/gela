with Gela.Element_Visiters;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Discriminant_Parts;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Known_Discriminant_Parts;
with Gela.Elements.Type_Definitions;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Alt_Record_Definitions;
with Gela.Elements.Record_Definitions;
with Gela.Elements.Component_Items;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Variant_Parts;
with Gela.Elements.Variants;

package body Gela.Plain_Type_Views is

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Type_View)
      return Gela.Type_Views.Category_Kinds
   is
   begin
      return Self.Category;
   end Category;

   ------------
   -- Create --
   ------------

   function Create_Full_Type
     (Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
      return Gela.Type_Views.Type_View_Access
   is
      Value : constant Type_View_Access :=
        new Type_View'(Category => Category, Decl => Decl);
   begin
      return Gela.Type_Views.Type_View_Access (Value);
   end Create_Full_Type;

   -------------------
   -- Get_Component --
   -------------------

   overriding function Get_Component
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access;
         end record;

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access);

         overriding procedure Record_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Definitions.
              Record_Definition_Access);

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access);

         overriding procedure Variant
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variants.Variant_Access);

         overriding procedure Variant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access);

      end Get;

      package body Get is

         overriding procedure Component_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Component_Declarations.
              Component_Declaration_Access)
         is
            use type Gela.Lexical_Types.Symbol;
            Names : constant Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Sequence_Access := Node.Names;
            Pos : Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Sequence_Cursor := Names.First;
         begin
            while Pos.Has_Element loop
               if Pos.Element.Full_Name = Symbol then
                  Self.Result := Pos.Element;

                  return;
               end if;

               Pos.Next;
            end loop;
         end Component_Declaration;

         overriding procedure Record_Type_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Type_Definitions.
              Record_Type_Definition_Access)
         is
            X : constant Gela.Elements.Alt_Record_Definitions.
              Alt_Record_Definition_Access := Node.Record_Definition;
         begin
            X.Visit (Self);
         end Record_Type_Definition;

         overriding procedure Record_Definition
           (Self : in out Visiter;
            Node : not null Gela.Elements.Record_Definitions.
              Record_Definition_Access)
         is
            List : constant Gela.Elements.Component_Items.
              Component_Item_Sequence_Access := Node.Record_Components;
            Cursor : Gela.Elements.Component_Items.
              Component_Item_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Record_Definition;

         overriding procedure Variant
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variants.Variant_Access)
         is
            List : constant Gela.Elements.Component_Items.
              Component_Item_Sequence_Access := Node.Record_Components;
            Cursor : Gela.Elements.Component_Items.
              Component_Item_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Variant;

         overriding procedure Variant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access)
         is
            List : constant Gela.Elements.Variants.Variant_Sequence_Access :=
              Node.Variants;
            Cursor : Gela.Elements.Variants.Variant_Sequence_Cursor :=
              List.First;
         begin
            while Cursor.Has_Element loop
               Cursor.Element.Visit (Self);
               Cursor.Next;
            end loop;
         end Variant_Part;
      end Get;

      V : Get.Visiter;
      View : Gela.Elements.Type_Definitions.Type_Definition_Access;
      D : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
        Self.Get_Discriminant (Symbol);
   begin
      if D.Assigned then
         return D;
      else
         View := Self.Decl.Type_Declaration_View;
         View.Visit (V);
         return Gela.Elements.Defining_Names.Defining_Name_Access (V.Result);
      end if;
   end Get_Component;

   ----------------------
   -- Get_Discriminant --
   ----------------------

   overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
      package Get is
         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Defining_Identifiers.
              Defining_Identifier_Access;
         end record;

         overriding procedure Known_Discriminant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Known_Discriminant_Parts.
              Known_Discriminant_Part_Access);
      end Get;

      package body Get is

         overriding procedure Known_Discriminant_Part
           (Self : in out Visiter;
            Node : not null Gela.Elements.Known_Discriminant_Parts.
              Known_Discriminant_Part_Access)
         is
            List : constant Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Sequence_Access := Node.Discriminants;
            Cursor : Gela.Elements.Discriminant_Specifications.
              Discriminant_Specification_Sequence_Cursor := List.First;
         begin
            while Cursor.Has_Element loop
               declare
                  use type Gela.Lexical_Types.Symbol;
                  Names : constant Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Access :=
                      Cursor.Element.Names;
                  Pos : Gela.Elements.Defining_Identifiers.
                    Defining_Identifier_Sequence_Cursor := Names.First;
               begin
                  while Pos.Has_Element loop
                     if Pos.Element.Full_Name = Symbol then
                        Self.Result := Pos.Element;

                        return;
                     end if;

                     Pos.Next;
                  end loop;

                  Cursor.Next;
               end;
            end loop;
         end Known_Discriminant_Part;
      end Get;

      X : constant Gela.Elements.Discriminant_Parts.
        Discriminant_Part_Access := Self.Decl.Discriminant_Part;
      V : Get.Visiter;
   begin
      if X.Assigned then
         X.Visit (V);
         return Gela.Elements.Defining_Names.Defining_Name_Access (V.Result);
      else
         return null;
      end if;
   end Get_Discriminant;

   ----------------------
   -- Is_Expected_Type --
   ----------------------

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Type_Views.Type_View_Access) return Boolean
   is
      use type Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;

      Expected_Category : constant Gela.Type_Views.Category_Kinds :=
        Expected.Category;
   begin
      if Self.Decl = Type_View (Expected.all).Decl then
         return True;
      end if;

      case Expected_Category is
         when Gela.Type_Views.An_Universal_Integer =>
            return Self.Category in Gela.Type_Views.Any_Integer_Type;
         when Gela.Type_Views.An_Universal_Real =>
            return Self.Category in Gela.Type_Views.Any_Real_Type;
         when others =>
            null;
      end case;

      case Self.Category is
         when Gela.Type_Views.An_Universal_Integer =>
            return Expected_Category in Gela.Type_Views.Any_Integer_Type;
         when Gela.Type_Views.An_Universal_Real =>
            return Expected_Category in Gela.Type_Views.Any_Real_Type;
         when others =>
            null;
      end case;

      return False;
   end Is_Expected_Type;

end Gela.Plain_Type_Views;
