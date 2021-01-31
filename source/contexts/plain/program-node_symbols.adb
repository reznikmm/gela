--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Character_Literals;
with Program.Elements.Defining_Character_Literals;
with Program.Elements.Defining_Expanded_Names;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Operator_Symbols;
with Program.Elements.Identifiers;
with Program.Elements.Operator_Symbols;
with Program.Elements.Package_Declarations;
with Program.Elements.Procedure_Body_Declarations;
with Program.Lexical_Elements;
with Program.Plain_Lexical_Elements;
with Program.Safe_Element_Visitors;

package body Program.Node_Symbols is

   package Symbol_Getters is

      type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Result : Program.Symbols.Symbol := Program.Symbols.No_Symbol;
      end record;

      overriding procedure Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Character_Literals
           .Character_Literal_Access);

      overriding procedure Defining_Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Character_Literals
           .Defining_Character_Literal_Access);

      overriding procedure Defining_Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access);

      overriding procedure Defining_Operator_Symbol
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Operator_Symbols
           .Defining_Operator_Symbol_Access);

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      overriding procedure Operator_Symbol
        (Self    : in out Getter;
         Element : not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access);

   end Symbol_Getters;

   package body Symbol_Getters is

      -----------------------
      -- Character_Literal --
      -----------------------

      overriding procedure Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Character_Literals
         .Character_Literal_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Character_Literal_Text.Character_Literal_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Character_Literal;

      --------------------------------
      -- Defining_Character_Literal --
      --------------------------------

      overriding procedure Defining_Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Character_Literals
           .Defining_Character_Literal_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Defining_Character_Literal_Text.Character_Literal_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Defining_Character_Literal;

      -------------------------
      -- Defining_Identifier --
      -------------------------

      overriding procedure Defining_Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Defining_Identifier_Text.Identifier_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Defining_Identifier;

      ------------------------------
      -- Defining_Operator_Symbol --
      ------------------------------

      overriding procedure Defining_Operator_Symbol
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Operator_Symbols
           .Defining_Operator_Symbol_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Defining_Operator_Symbol_Text.Operator_Symbol_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Defining_Operator_Symbol;

      ----------------
      -- Identifier --
      ----------------

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Identifier_Text.Identifier_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Identifier;

      overriding procedure Operator_Symbol
        (Self    : in out Getter;
         Element : not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access)
      is
         Token : constant Program.Lexical_Elements.Lexical_Element_Access :=
           Element.To_Operator_Symbol_Text.Operator_Symbol_Token;
      begin
         Self.Result := Program.Plain_Lexical_Elements.Lexical_Element
           (Token.all).Symbol;
      end Operator_Symbol;

   end Symbol_Getters;

   package Name_Getters is
      type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Table  : not null access Program.Symbol_Lists.Symbol_List_Table'Class;
         Result : Program.Symbol_Lists.Symbol_List :=
           Program.Symbol_Lists.Empty_Symbol_List;
      end record;

      overriding procedure Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Character_Literals
           .Character_Literal_Access);

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

   end Name_Getters;

   package body Name_Getters is

      -----------------------
      -- Character_Literal --
      -----------------------

      overriding procedure Character_Literal
        (Self    : in out Getter;
         Element : not null Program.Elements.Character_Literals
           .Character_Literal_Access)
      is
         Symbol : constant Program.Symbols.Symbol := Get_Symbol (Element);
      begin
         Self.Table.Find_Or_Create
           (Suffix => Symbol,
            Result => Self.Result);
      end Character_Literal;

      ----------------
      -- Identifier --
      ----------------

      overriding procedure Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         Symbol : constant Program.Symbols.Symbol := Get_Symbol (Element);
      begin
         Self.Table.Find_Or_Create
           (Suffix => Symbol,
            Result => Self.Result);
      end Identifier;

   end Name_Getters;

   package Defining_Name_Getters is

      type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Table  : not null access Program.Symbol_Lists.Symbol_List_Table'Class;
         Result : Program.Symbol_Lists.Symbol_List :=
           Program.Symbol_Lists.Empty_Symbol_List;
      end record;

      overriding procedure Defining_Expanded_Name
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Expanded_Names
           .Defining_Expanded_Name_Access);

      overriding procedure Defining_Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access);

   end Defining_Name_Getters;

   package body Defining_Name_Getters is

      ----------------------------
      -- Defining_Expanded_Name --
      ----------------------------

      overriding procedure Defining_Expanded_Name
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Expanded_Names
           .Defining_Expanded_Name_Access)
      is
         Prefix : Program.Symbol_Lists.Symbol_List;
         Suffix : constant Program.Symbols.Symbol :=
           Get_Symbol (Element.Selector);
      begin
         Name_Symbol (Self.Table.all, Element.Prefix, Prefix);
         Self.Table.Find_Or_Create (Prefix, Suffix, Self.Result);
      end Defining_Expanded_Name;

      -------------------------
      -- Defining_Identifier --
      -------------------------

      overriding procedure Defining_Identifier
        (Self    : in out Getter;
         Element : not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access) is
      begin
         Self.Table.Find_Or_Create
           (Suffix => Program.Node_Symbols.Get_Symbol (Element),
            Result => Self.Result);
      end Defining_Identifier;

   end Defining_Name_Getters;

   package Unit_Symbols is
      type Getter is new Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Table  : not null access Program.Symbol_Lists.Symbol_List_Table'Class;
         Result : Program.Symbol_Lists.Symbol_List :=
           Program.Symbol_Lists.Empty_Symbol_List;
      end record;

      overriding procedure Package_Declaration
        (Self    : in out Getter;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Getter;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

   end Unit_Symbols;

   package body Unit_Symbols is

      overriding procedure Package_Declaration
        (Self    : in out Getter;
         Element : not null Program.Elements.Package_Declarations
           .Package_Declaration_Access) is
      begin
         Defining_Name_Symbol (Self.Table.all, Element.Name, Self.Result);
      end Package_Declaration;

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Getter;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access) is
      begin
         Defining_Name_Symbol (Self.Table.all, Element.Name, Self.Result);
      end Procedure_Body_Declaration;

   end Unit_Symbols;

   --------------------------
   -- Defining_Name_Symbol --
   --------------------------

   procedure Defining_Name_Symbol
     (Self    : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Element : not null Program.Elements.Defining_Names.Defining_Name_Access;
      Result  : out Program.Symbol_Lists.Symbol_List)
   is
      V : Defining_Name_Getters.Getter :=
        (Table => Self'Unchecked_Access, others => <>);
   begin
      V.Visit (Element);
      Result := V.Result;
   end Defining_Name_Symbol;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Name : access Program.Elements.Element'Class)
      return Program.Symbols.Symbol
   is
      G : Symbol_Getters.Getter;
   begin
      G.Visit (Name);

      return G.Result;
   end Get_Symbol;

   -----------------
   -- Name_Symbol --
   -----------------

   procedure Name_Symbol
     (Self    : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Element : not null Program.Elements.Expressions.Expression_Access;
      Result  : out Program.Symbol_Lists.Symbol_List)
   is
      Getter : Name_Getters.Getter :=
        (Table => Self'Unchecked_Access, others => <>);
   begin
      Getter.Visit (Element);
      Result := Getter.Result;
   end Name_Symbol;

   --------------------
   -- Unit_Full_Name --
   --------------------

   procedure Unit_Full_Name
     (Self : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access;
      Name : out Program.Symbol_Lists.Symbol_List)
   is
      Getter : Unit_Symbols.Getter :=
        (Table => Self'Unchecked_Access, others => <>);
   begin
      Getter.Visit (Unit.Unit_Declaration);
      Name := Getter.Result;
   end Unit_Full_Name;

end Program.Node_Symbols;
