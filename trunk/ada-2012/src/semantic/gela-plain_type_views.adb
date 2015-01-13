with Gela.Element_Visiters;
with Gela.Elements.Discriminant_Parts;
with Gela.Elements.Known_Discriminant_Parts;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Defining_Identifiers;

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

end Gela.Plain_Type_Views;
