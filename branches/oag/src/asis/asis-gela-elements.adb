with Asis.Gela.Strings;
with Asis.Gela.Contexts;
with Asis.Gela.Base_Lists;

with Gela.Decoders; use Gela;
with Gela.Source_Buffers;

package body Asis.Gela.Elements is
   type ASIS_Integer_Array is array (ASIS_Positive range <>) of ASIS_Integer;

   package Storage_Vectors_Arrays is new Compilations.Storage_Vectors.Arrays
     (Array_Index => Asis.ASIS_Positive,
      Array_Type => ASIS_Integer_Array);

   -------------------
   -- Child_Element --
   -------------------

   function Child_Element
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Asis.Element
   is
      C : constant Compilations.Compilation :=
        Asis.Gela.Contexts.Get_Compilation (Element.Unit);

      Result : constant Element_Index := Get (C, Element.Index, Property);
   begin
      return (Unit  => Element.Unit,
              Index => Result);
   end Child_Element;

   --------------------
   -- Child_Elements --
   --------------------

   function Child_Elements
     (Element  : Asis.Element;
      Property : P.Property_Kinds;
      Include_Pragmas : in Boolean := False) return Asis.Element_List
   is
      use Asis.Gela.Base_Lists;

      C : constant Compilations.Compilation :=
        Asis.Gela.Contexts.Get_Compilation (Element.Unit);

      List : constant Element_Index := Get (C, Element.Index, Property);
   begin
      return To_Element_List (C, List, Element.Unit, Include_Pragmas);
   end Child_Elements;

   ------------------
   -- Kind_In_List --
   ------------------

   function Kind_In_List
     (C    : Compilations.Compilation;
      Item : Element_Index;
      List : Global_Kind_List) return Boolean
   is
      use type P.Global_Kinds;
      Kind : constant P.Global_Kinds := Global_Kind (C, Item);
   begin
      for I in List'Range loop
         if Kind = List (I) then
            return True;
         end if;
      end loop;

      return False;
   end Kind_In_List;

   -----------------
   -- New_Element --
   -----------------

   function New_Element
     (C    : Compilations.Compilation;
      Kind : P.Global_Kinds) return Element_Index
   is
      use Asis.Gela.Compilations;
      use Storage_Vectors_Arrays;
      use Compilations.Storage_Vectors;
      Value  : constant Asis.ASIS_Natural := P.Global_Kinds'Pos (Kind);
      Count  : constant Asis.ASIS_Positive := P.Size (Kind);
      Result : Element_Index;
   begin
      Add (C.Storage, Value, Result);
      Add (C.Storage, (1 .. Count => 0));
      return Result;
   end New_Element;

   ---------
   -- Get --
   ---------

   function Get
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Property : P.Property_Kinds) return Property_Value
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      Get (C, Element, Property, Result, Success);

      if not Success then
         raise Internal_Error;
      end if;

      return Result;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Index    : Element_Index) return Property_Value
   is
      use Compilations.Storage_Vectors;
   begin
      return Get (C.Storage, Element + Index);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (C        : in     Compilations.Compilation;
      Element  : in     Element_Index;
      Property : in     P.Property_Kinds;
      Result   :    out Property_Value;
      Success  :    out Boolean)
   is
      use Asis.Gela.Compilations;
      use Storage_Vectors_Arrays;
      use Compilations.Storage_Vectors;
      Value  : constant Asis.ASIS_Natural := Get (C.Storage, Element);
      Kind   : constant P.Global_Kinds := P.Global_Kinds'Val (Value);
      Index  : constant Asis.ASIS_Natural :=
        P.Property_Index (Kind, Property);
   begin
      if Index > 0 then
         Result := Get (C.Storage, Element + Index);
         Success := True;
      else
         Success := False;
      end if;
   end Get;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Boolean
   is
      Result  : Property_Value;
      Success : Boolean;
   begin
      if Assigned (Element) then
         Get (Contexts.Get_Compilation (Element.Unit),
              Element.Index,
              Property,
              Result,
              Success);

         if Success then
            return Boolean'Val (Result);
         end if;
      end if;

      return False;
   end Get_Boolean;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Element  : Asis.Element;
      Property : P.Property_Kinds) return Asis.Program_Text
   is
      C : constant Compilations.Compilation :=
        Asis.Gela.Contexts.Get_Compilation (Element.Unit);

      Image_From : constant Element_Index :=
        Get (C, Element.Index, Property);

      Image_To : constant Element_Index :=
        Get (C, Element.Index, P.Image_End);
   begin
      if Image_To = 0 then
         return Strings.Get (C.Text_Buffer, Positive (Image_From));
      else
         declare
            use Source_Buffers;
            Text  : Wide_String (1 .. 3 * Positive (Image_To - Image_From));
            Last  : Natural;
            Start : constant Cursor := Buffer_Start (C.Buffer.all);
            C1    : constant Cursor := Start + Integer (Image_From);
            C2    : constant Cursor := Start + Integer (Image_To);
         begin
            Decoders.Decode (C.Decoder.all, C1, C2, Text, Last);
            return Text (1 .. Last);
         end;
      end if;
   end Get_Image;

   -----------------
   -- Global_Kind --
   -----------------

   function Global_Kind
     (C       : Compilations.Compilation;
      Element : Element_Index) return P.Global_Kinds
   is
      use Asis.Gela.Compilations;
      use Compilations.Storage_Vectors;
      Value  : constant Asis.ASIS_Natural := Get (C.Storage, Element);
      Kind   : constant P.Global_Kinds := P.Global_Kinds'Val (Value);
   begin
      return Kind;
   end Global_Kind;

   -------------------
   -- Push_Argument --
   -------------------

   procedure Push_Argument
     (C    : in     Compilations.Compilation;
      Call : in     Element_Index;
      Arg  : in     Element_Index)
   is
      use Asis.Gela.Properties;
   begin
      if Global_Kind (C, Call) = A_Function_Call then
         declare
            Assoc_List : constant Element_Index :=
              Get (C, Call, Function_Call_Parameters);

            Assoc  : constant Element_Index :=
              Base_Lists.Get (C, Assoc_List, 1);

            Left  : constant Element_Index :=
              Get (C, Assoc, Actual_Parameter);
         begin
            Set_Start_Position (C, Assoc, Arg);
            Set_Start_Position (C, Call, Arg);

            if Assigned (Left) then
               Push_Argument (C, Left, Arg);
            else
               Set (C, Assoc, Actual_Parameter, Arg);
               Set_End_Position (C, Assoc, Arg);
            end if;
         end;
      else  --  Short_Circuit expression
         declare
            Left : constant Element_Index :=
              Get (C, Call, Short_Circuit_Operation_Left_Expression);
         begin
            if Assigned (Left) then
               Push_Argument (C, Left, Arg);
            else
               Set (C, Call, Short_Circuit_Operation_Left_Expression, Arg);
            end if;

            Set_Start_Position (C, Call, Arg);
         end;
      end if;
   end Push_Argument;

   ---------
   -- Set --
   ---------

   procedure Set
     (C        : Compilations.Compilation;
      Element  : Element_Index;
      Property : P.Property_Kinds;
      Value    : Asis.ASIS_Natural)
   is
      use Asis.Gela.Compilations;
      use Storage_Vectors_Arrays;
      use Compilations.Storage_Vectors;
      Val    : constant Asis.ASIS_Natural := Get (C.Storage, Element);
      Kind   : constant P.Global_Kinds := P.Global_Kinds'Val (Val);
      Index  : constant Asis.ASIS_Positive :=
        P.Property_Index (Kind, Property);
   begin
      Set (C.Storage, Element + Index, Value);
   end Set;

   ----------------------
   -- Set_End_Position --
   ----------------------

   procedure Set_End_Position
     (C       : Compilations.Compilation;
      Element : Element_Index;
      Source  : Element_Index) is
   begin
      Set (C, Element, P.End_Line, Get (C, Source, P.End_Line));
      Set (C, Element, P.End_Column, Get (C, Source, P.End_Column));
   end Set_End_Position;

   ------------------------
   -- Set_Start_Position --
   ------------------------

   procedure Set_Start_Position
     (C       : Compilations.Compilation;
      Element : Element_Index;
      Source  : Element_Index) is
   begin
      Set (C, Element, P.Start_Line, Get (C, Source, P.Start_Line));
      Set (C, Element, P.Start_Column, Get (C, Source, P.Start_Column));
   end Set_Start_Position;

end Asis.Gela.Elements;
