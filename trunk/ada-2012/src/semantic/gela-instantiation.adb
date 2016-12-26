with Ada.Containers.Hashed_Maps;

with Gela.Element_Cloners;
with Gela.Lexical_Types;
with Gela.Property_Setters;
with Gela.Property_Visiters;

with Gela.Elements.Aspect_Specifications;
with Gela.Elements.Basic_Declarations;
with Gela.Elements.Basic_Declarative_Items;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Names;
with Gela.Elements.Defining_Program_Unit_Names;
with Gela.Elements.Generic_Formals;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Package_Instances;

package body Gela.Instantiation is

   function Hash (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
     return Ada.Containers.Hash_Type;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Gela.Elements.Defining_Names.Defining_Name_Access)
                  return Ada.Containers.Hash_Type is
   begin
      return Self.Hash;
   end Hash;

   package Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Gela.Elements.Defining_Names.Defining_Name_Access,
      Element_Type    => Gela.Elements.Defining_Names.Defining_Name_Access,
      Hash            => Hash,
      Equivalent_Keys => Gela.Elements.Defining_Names."=",
      "="             => Gela.Elements.Defining_Names."=");

   package Cloners is
      type Cloner is new Gela.Element_Cloners.Cloner with record
         Map           : Name_Maps.Map;
         Instance_Name : Gela.Elements.Defining_Names.Defining_Name_Access;
         Template      : access Gela.Elements.Element'Class;
      end record;

      overriding function Clone
        (Self    : in out Cloner;
         Element : access Gela.Elements.Element'Class)
         return Gela.Elements.Element_Access;

      overriding procedure Defining_Identifier
        (Self : in out Cloner;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access);

      overriding procedure Generic_Package_Declaration
        (Self : in out Cloner;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access);

   end Cloners;

   package Setters is
      type Property_Setter
        (Source : Gela.Elements.Element_Access;
         Cloner : access Cloners.Cloner)
          is new Gela.Property_Setters.Property_Setter with null record;

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : in out Gela.Lexical_Types.Symbol);

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : in out Gela.Elements.Defining_Names.Defining_Name_Access);

   end Setters;

   -------------
   -- Setters --
   -------------

   package body Setters is

      ------------------
      -- On_Full_Name --
      ------------------

      overriding procedure On_Full_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : in out Gela.Lexical_Types.Symbol)
      is
         pragma Unreferenced (Element);
         type Property_Visiter is new Gela.Property_Visiters.Property_Visiter
           with null record;

         overriding procedure On_Full_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Lexical_Types.Symbol);

         overriding procedure On_Full_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Lexical_Types.Symbol)
         is
            pragma Unreferenced (Self, Element);
         begin
            Setters.On_Full_Name.Value := Value;
         end On_Full_Name;

         Getter  : aliased Property_Visiter;
         Visiter : Gela.Property_Visiters.Visiter (Getter'Access);
      begin
         Self.Source.Visit (Visiter);
      end On_Full_Name;

      ----------------------
      -- On_Defining_Name --
      ----------------------

      overriding procedure On_Defining_Name
        (Self    : in out Property_Setter;
         Element : Gela.Elements.Element_Access;
         Value   : in out Gela.Elements.Defining_Names.Defining_Name_Access)
      is
         pragma Unreferenced (Element);
         type Property_Visiter is new Gela.Property_Visiters.Property_Visiter
           with null record;

         overriding procedure On_Defining_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Elements.Defining_Names.Defining_Name_Access);

         overriding procedure On_Defining_Name
           (Self    : in out Property_Visiter;
            Element : Gela.Elements.Element_Access;
            Value   : Gela.Elements.Defining_Names.Defining_Name_Access)
         is
            pragma Unreferenced (Self, Element);
         begin
            Setters.On_Defining_Name.Value := Value;
         end On_Defining_Name;

         Getter  : aliased Property_Visiter;
         Visiter : Gela.Property_Visiters.Visiter (Getter'Access);
         Cursor  : Name_Maps.Cursor;
      begin
         Self.Source.Visit (Visiter);

         if Value.Assigned then
            Cursor := Self.Cloner.Map.Find (Value);

            if Name_Maps.Has_Element (Cursor) then
               Value := Name_Maps.Element (Cursor);
            end if;
         end if;
      end On_Defining_Name;

   end Setters;


   -------------
   -- Cloners --
   -------------

   package body Cloners is

      -----------
      -- Clone --
      -----------

      overriding function Clone
        (Self    : in out Cloner;
         Element : access Gela.Elements.Element'Class)
         return Gela.Elements.Element_Access
      is
         Result  : Gela.Elements.Element_Access;
         Setter  : aliased Setters.Property_Setter
           (Element, Self'Unchecked_Access);
         Visiter : Gela.Property_Setters.Visiter (Setter'Access);
      begin
         if Element.Assigned then
            Result := Gela.Element_Cloners.Cloner (Self).Clone (Element);
            Result.Set_Part_Of_Instance;
            Result.Visit (Visiter);
         end if;

         return Result;
      end Clone;

      -------------------------
      -- Defining_Identifier --
      -------------------------

      overriding procedure Defining_Identifier
        (Self : in out Cloner;
         Node : not null Gela.Elements.Defining_Identifiers.
           Defining_Identifier_Access)
      is
         Source : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
           Gela.Elements.Defining_Names.Defining_Name_Access (Node);
         Result : Gela.Elements.Defining_Names.Defining_Name_Access;
      begin
         Gela.Element_Cloners.Cloner (Self).Defining_Identifier (Node);
         Result := Gela.Elements.Defining_Names.Defining_Name_Access (Node);
         Self.Map.Insert (Source, Result);
      end Defining_Identifier;

      ---------------------------------
      -- Generic_Package_Declaration --
      ---------------------------------

      overriding procedure Generic_Package_Declaration
        (Self : in out Cloner;
         Node : not null Gela.Elements.Generic_Package_Declarations.
           Generic_Package_Declaration_Access)
      is
         Formal_Part_Copy               : Gela.Elements.Basic_Declarations.
           Basic_Declaration_Sequence_Access;
         Names                          : Gela.Elements.
           Defining_Program_Unit_Names.Defining_Program_Unit_Name_Access;
         Aspect_Specifications          : Gela.Elements.Aspect_Specifications.
           Aspect_Specification_Sequence_Access;
         Visible_Part_Declarative_Items : Gela.Elements.Basic_Declarative_Items
           .Basic_Declarative_Item_Sequence_Access;
         Private_Part_Declarative_Items : Gela.Elements.Basic_Declarative_Items
           .Basic_Declarative_Item_Sequence_Access;

         Result : Gela.Elements.Package_Instances.Package_Instance_Access;
      begin
         if Self.Template = Node then
            if Node.Generic_Formal_Part not in null then
               declare
                  Item   : Gela.Elements.Basic_Declarations.
                    Basic_Declaration_Access;
                  Cursor : Gela.Elements.Generic_Formals.
                    Generic_Formal_Sequence_Cursor :=
                      Node.Generic_Formal_Part.First;
               begin
                  Formal_Part_Copy := Self.Factory.Basic_Declaration_Sequence;

                  while Cursor.Has_Element loop
                     Item := Gela.Elements.Basic_Declarations.
                       Basic_Declaration_Access
                         (Cloner'Class (Self).Clone (Cursor.Element));
                     Formal_Part_Copy.Append (Item);
                     Cursor.Next;
                  end loop;
               end;
            end if;

            Names := Gela.Elements.Defining_Program_Unit_Names.
              Defining_Program_Unit_Name_Access
                (Cloner'Class (Self).Clone (Self.Instance_Name));

            if Node.Aspect_Specifications not in null then
               declare
                  Item   : Gela.Elements.Aspect_Specifications.
                    Aspect_Specification_Access;
                  Cursor : Gela.Elements.Aspect_Specifications.
                    Aspect_Specification_Sequence_Cursor :=
                      Node.Aspect_Specifications.First;
               begin
                  Aspect_Specifications := Self.Factory.
                    Aspect_Specification_Sequence;

                  while Cursor.Has_Element loop
                     Item := Gela.Elements.Aspect_Specifications.
                       Aspect_Specification_Access
                         (Cloner'Class (Self).Clone (Cursor.Element));
                     Aspect_Specifications.Append (Item);
                     Cursor.Next;
                  end loop;
               end;
            end if;

            if Node.Visible_Part_Declarative_Items not in null then
               declare
                  Item  : Gela.Elements.Basic_Declarative_Items.
                    Basic_Declarative_Item_Access;
                  Cursor  : Gela.Elements.Basic_Declarative_Items.
                    Basic_Declarative_Item_Sequence_Cursor :=
                      Node.Visible_Part_Declarative_Items.First;
               begin
                  Visible_Part_Declarative_Items := Self.Factory.
                    Basic_Declarative_Item_Sequence;

                  while Cursor.Has_Element loop
                     Item := Gela.Elements.Basic_Declarative_Items.
                       Basic_Declarative_Item_Access
                         (Cloner'Class (Self).Clone (Cursor.Element));
                     Visible_Part_Declarative_Items.Append (Item);
                     Cursor.Next;
                  end loop;
               end;
            end if;

            if Node.Private_Part_Declarative_Items not in null then
               declare
                  Item  : Gela.Elements.Basic_Declarative_Items.
                    Basic_Declarative_Item_Access;
                  Cursor : Gela.Elements.Basic_Declarative_Items.
                    Basic_Declarative_Item_Sequence_Cursor :=
                      Node.Private_Part_Declarative_Items.First;
               begin
                  Private_Part_Declarative_Items := Self.Factory.
                    Basic_Declarative_Item_Sequence;

                  while Cursor.Has_Element loop
                     Item := Gela.Elements.Basic_Declarative_Items.
                       Basic_Declarative_Item_Access
                         (Cloner'Class (Self).Clone (Cursor.Element));
                     Private_Part_Declarative_Items.Append (Item);
                     Cursor.Next;
                  end loop;
               end;
            end if;

            Result := Self.Factory.Package_Instance
              (Formal_Part_Copy, Names, Aspect_Specifications,
               Visible_Part_Declarative_Items,
               Private_Part_Declarative_Items);

            Self.Result := Gela.Elements.Element_Access (Result);
         else
            Gela.Element_Cloners.Cloner (Self)
              .Generic_Package_Declaration (Node);
         end if;
      end Generic_Package_Declaration;

   end Cloners;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Expanded     : out Gela.Elements.Element_Access)
   is
      Defining_Name : constant Gela.Elements.Defining_Names
        .Defining_Name_Access := Node.Generic_Unit_Name.Defining_Name;

      Cloner : Cloners.Cloner (Comp.Factory);
   begin
      if Defining_Name.Assigned then
         Cloner.Template := Defining_Name.Enclosing_Element;
         Cloner.Instance_Name := Defining_Name;
         Expanded := Cloner.Clone (Cloner.Template);
      end if;
   end Expand;

end Gela.Instantiation;
