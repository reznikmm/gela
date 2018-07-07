------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Implement Asis.Text.
--  Not implemented yet (except Element_Span)

with Asis.Errors;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Implementation;

package body Asis.Text is

   LF : constant Wide_Character := Wide_Character'Val (10);

   -------------------
   -- Comment_Image --
   -------------------

   function Comment_Image (The_Line : in Line) return Program_Text is
      Span   : Gela.Lexical_Types.Line_Span;
      Source : League.Strings.Universal_String;
   begin
      if Is_Nil (The_Line) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Line is nil");

         raise Asis.Exceptions.ASIS_Inappropriate_Line;
      end if;

      Span := The_Line.Comp.Get_Line_Span (The_Line.Index);
      Source := The_Line.Comp.Source;
      Source.Slice (Span.Comment, Span.Last);

      return Source.To_UTF_16_Wide_String;
   end Comment_Image;

   ----------------------
   -- Compilation_Span --
   ----------------------

   function Compilation_Span
     (Element : in Asis.Element)
      return Span
   is
      Comp : constant Gela.Compilations.Compilation_Access :=
        Element.Data.Enclosing_Compilation;
      Last : constant Gela.Lexical_Types.Line_Count := Comp.Line_Count;
      Span : constant Gela.Lexical_Types.Line_Span :=
        Comp.Get_Line_Span (Last);
   begin
      return (1, 1,
              ASIS_Natural (Last),
              ASIS_Natural (Span.Last - Span.First + 1));
   end Compilation_Span;

   ---------------------------
   -- Compilation_Unit_Span --
   ---------------------------

   function Compilation_Unit_Span (Element : in Asis.Element) return Span is
      pragma Unreferenced (Element);
   begin
      Raise_Not_Implemented ("");
      return Nil_Span;
   end Compilation_Unit_Span;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (The_Line : in Line) return Wide_String is
   begin
      if Is_Nil (The_Line) then
         return "[nil_line]";
      else
         declare
            Unit : constant Wide_String :=
              The_Line.Comp.Text_Name.To_UTF_16_Wide_String;
            Image : constant Wide_String :=
              Gela.Lexical_Types.Line_Count'Wide_Image (The_Line.Index);
         begin
            return Unit & "[" & Image (2 .. Image'Last) & "]";
         end;
      end if;
   end Debug_Image;

   ---------------------
   -- Delimiter_Image --
   ---------------------

   function Delimiter_Image return Wide_String is
   begin
      return (1 => LF);
   end Delimiter_Image;

   -------------------
   -- Element_Image --
   -------------------

   function Element_Image (Element : in Asis.Element) return Program_Text is
      X         : constant Span := Element_Span (Element);
      Prefix    : constant Program_Text :=
        (1 .. Natural (X.First_Column) - 1 => ' ');
      From      : constant Gela.Lexical_Types.Line_Index :=
        Gela.Lexical_Types.Line_Index (X.First_Line);
      To        : constant Gela.Lexical_Types.Line_Index :=
        Gela.Lexical_Types.Line_Index (X.Last_Line);
      Source    : League.Strings.Universal_String;
      Comp      : constant Gela.Compilations.Compilation_Access :=
        Element.Data.Enclosing_Compilation;
      From_Line : constant Gela.Lexical_Types.Line_Span :=
        Comp.Get_Line_Span (From);
      To_Line   : constant Gela.Lexical_Types.Line_Span :=
        Comp.Get_Line_Span (To);
   begin
      Source := Comp.Source;
      Source.Slice
        (From_Line.First + Natural (X.First_Column) - 1,
         To_Line.First + Natural (X.Last_Column) - 1);

      return Prefix & Source.To_UTF_16_Wide_String;
   end Element_Image;

   ------------------
   -- Element_Span --
   ------------------

   function Element_Span (Element : in Asis.Element) return Span is
      use type Gela.Lexical_Types.Token_Count;

      Comp      : constant Gela.Compilations.Compilation_Access :=
        Element.Data.Enclosing_Compilation;
   begin
      if Element.Data.First_Token = 0
        or Element.Data.Last_Token = 0
        or Element.Data.Is_Part_Of_Instance
      then
         return Nil_Span;
      end if;

      declare
         From      : constant Gela.Lexical_Types.Token :=
           Comp.Get_Token (Element.Data.First_Token);
         To        : constant Gela.Lexical_Types.Token :=
           Comp.Get_Token (Element.Data.Last_Token);
         From_Line : constant Gela.Lexical_Types.Line_Span :=
           Comp.Get_Line_Span (From.Line);
         To_Line   : constant Gela.Lexical_Types.Line_Span :=
           Comp.Get_Line_Span (From.Line);
      begin
         return
           (First_Line   => Line_Number_Positive (From.Line),
            First_Column => Character_Position_Positive
              (From.First - From_Line.First + 1),
            Last_Line    => Line_Number (To.Line),
            Last_Column  => Character_Position (To.Last - To_Line.First + 1));
      end;
   end Element_Span;

   -----------------------
   -- First_Line_Number --
   -----------------------

   function First_Line_Number (Element : in Asis.Element) return Line_Number is
      S : constant Span := Element_Span (Element);
   begin
      return S.First_Line;
   end First_Line_Number;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Left  : in Line; Right : in Line) return Boolean is
   begin
      return Is_Identical (Left, Right);
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical (Left  : in Line; Right : in Line) return Boolean is
      use type League.Strings.Universal_String;
      use type Gela.Lexical_Types.Line_Count;
   begin
      return Left.Comp.Text_Name = Right.Comp.Text_Name and
        Left.Index = Right.Index;
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Line) return Boolean is
      use type Gela.Compilations.Compilation_Access;
      use type Gela.Lexical_Types.Line_Count;
   begin
      return Right.Comp = null and Right.Index = 0;
   end Is_Nil;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Line_List) return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Span) return Boolean is
   begin
      return Right = Nil_Span;
   end Is_Nil;

   -----------------------
   -- Is_Text_Available --
   -----------------------

   function Is_Text_Available (Element : in Asis.Element) return Boolean is
      use Asis.Elements;
   begin
      if Is_Nil (Element) or
        Is_Part_Of_Implicit (Element) or
        Is_Part_Of_Instance (Element)
      then
         return False;
      else
         return True;
      end if;
   end Is_Text_Available;

   ----------------------
   -- Last_Line_Number --
   ----------------------

   function Last_Line_Number (Element : in Asis.Element) return Line_Number is
      S : constant Span := Element_Span (Element);
   begin
      return S.Last_Line;
   end Last_Line_Number;

   ------------
   -- Length --
   ------------

   function Length (The_Line : in Line) return Character_Position is
   begin
      return Line_Image (The_Line)'Length;
   end Length;

   ----------------
   -- Line_Image --
   ----------------

   function Line_Image (The_Line : in Line) return Program_Text is
      Span   : Gela.Lexical_Types.Line_Span;
      Source : League.Strings.Universal_String;
   begin
      if Is_Nil (The_Line) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Line is nil");

         raise Asis.Exceptions.ASIS_Inappropriate_Line;
      end if;

      Span := The_Line.Comp.Get_Line_Span (The_Line.Index);
      Source := The_Line.Comp.Source;
      Source.Slice (Span.First, Span.Last);

      return Source.To_UTF_16_Wide_String;
   end Line_Image;

   -----------
   -- Lines --
   -----------

   function Lines (Element : in Asis.Element) return Line_List is
      The_Span : constant Span := Element_Span (Element);
   begin
      if Asis.Elements.Is_Nil (Element) then
         return Nil_Line_List;
      else
         return Lines (Element, The_Span);
      end if;
   end Lines;

   -----------
   -- Lines --
   -----------

   function Lines
     (Element  : in Asis.Element;
      The_Span : in Span)
      return Line_List
   is
      Comp   : Gela.Compilations.Compilation_Access;
      Result : Line_List (The_Span.First_Line .. The_Span.Last_Line);
   begin
      if Is_Nil (The_Span) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Span is nil");

         raise Asis.Exceptions.ASIS_Inappropriate_Line;
      end if;

      Comp := Element.Data.Enclosing_Compilation;

      for J in Result'Range loop
         Result (J).Comp := Comp;
         Result (J).Index := Gela.Lexical_Types.Line_Index (J);
      end loop;

      return Result;
   end Lines;

   -----------
   -- Lines --
   -----------

   function Lines
     (Element    : in Asis.Element;
      First_Line : in Line_Number_Positive;
      Last_Line  : in Line_Number)
      return Line_List
   is
      Comp   : Gela.Compilations.Compilation_Access;
      Result : Line_List (First_Line .. Last_Line);
   begin
      if First_Line > Last_Line then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Span is nil");

         raise Asis.Exceptions.ASIS_Inappropriate_Line;
      end if;

      Comp := Element.Data.Enclosing_Compilation;

      for J in Result'Range loop
         Result (J).Comp := Comp;
         Result (J).Index := Gela.Lexical_Types.Line_Index (J);
      end loop;

      return Result;
   end Lines;

   -----------------------
   -- Non_Comment_Image --
   -----------------------

   function Non_Comment_Image (The_Line : in Line) return Program_Text is
      Span   : Gela.Lexical_Types.Line_Span;
      Source : League.Strings.Universal_String;
   begin
      if Is_Nil (The_Line) then
         Implementation.Set_Status
           (Status    => Asis.Errors.Value_Error,
            Diagnosis => "Line is nil");

         raise Asis.Exceptions.ASIS_Inappropriate_Line;
      end if;

      Span := The_Line.Comp.Get_Line_Span (The_Line.Index);
      Source := The_Line.Comp.Source;
      Source.Slice (Span.First, Span.Comment);

      return Source.To_UTF_16_Wide_String;
   end Non_Comment_Image;

end Asis.Text;



------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
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
