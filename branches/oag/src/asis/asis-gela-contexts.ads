
with Ada.Strings.Wide_Unbounded;

with Gela;
 use Gela;
with Gela.Encodings;
with Gela.Containers.Vectors;

with Asis.Gela.Pools;
with Asis.Gela.Library;
with Asis.Gela.Compilations;
with Asis.Gela.Read_Managers;

with Asis.Text;
with Asis.Extensions.Errors;

package Asis.Gela.Contexts is

   type Context_Node is limited private;

   function Is_Associated (Node : Context_Node) return Boolean;
   function Is_Open       (Node : Context_Node) return Boolean;
   function Name          (Node : Context_Node) return Wide_String;
   function Parameters    (Node : Context_Node) return Wide_String;
   function Encoding      (Node : Context_Node) return Encodings.Encoding;

--   function Is_Valid (Unit : Asis.Compilation_Unit) return Boolean;

   procedure Parse_Parameters
     (Node       : in out Context_Node;
      Context    : in     Asis.Context;
      Name       : in     Wide_String;
      Parameters : in     Wide_String);

   procedure Open       (Node : in out Context_Node);
   procedure Close      (Node : in out Context_Node);
   procedure Dissociate (Node : in out Context_Node);

   procedure Set_Error_Handler
     (Node    : in out Context_Node;
      Handler : in     Extensions.Errors.Error_Handler_Access);

   procedure Report_Error
     (Node     : in out Context_Node;
      Kind     : in     Extensions.Errors.Error_Kind;
      The_Unit : in     Compilation_Unit := Asis.Nil_Compilation_Unit;
      Line     : in     Text.Line_Number        := 0;
      Column   : in     Text.Character_Position := 0;
      Text     : in     Wide_String             := "");

   function Get_Compilation
     (Unit : Asis.Compilation_Unit) return Compilations.Compilation;

   function Get_Unit_Data (Unit : Asis.Compilation_Unit) return Element_Index;

   function Get_Context (Unit : Asis.Compilation_Unit) return Context;

   function Get_Units
     (Context : Asis.Context) return Asis.Compilation_Unit_List;

   function Has_File (Node : Context_Node; File : Wide_String) return Boolean;

   type Context_List is array (Context_Index range 1 .. Context_Index'Last)
     of Context_Node;

private
   package W renames Ada.Strings.Wide_Unbounded;

   -----------------
   -- Unit_Header --
   -----------------

   type Unit_Header is record
      Unit         : Unit_Index;
      Compilation  : Compilations.Compilation;
      Data         : Element_Index;
   end record;

   package Unit_Header_Vectors is new
     Containers.Vectors (Unit_Header, Unit_Index, Pools.Standard_Pool);

   -----------------
   -- Error_Level --
   -----------------

   type Error_Level is (Success, Warning, Error, Fatal);

   ------------------
   -- Context_Node --
   ------------------

   type Context_Node is record
      Is_Open       : Boolean := False;
      Is_Associated : Boolean := False;
      Name          : W.Unbounded_Wide_String;
      Parameters    : W.Unbounded_Wide_String;
      Input_File    : W.Unbounded_Wide_String;
      User_Encoding : Encodings.Encoding;
      Search_Path   : Library.Search_Path;
      Error         : Error_Level;
      Error_Handler : Extensions.Errors.Error_Handler_Access;
      Read_Manager  : Read_Managers.Read_Manager_Access;
      Context       : Asis.Context;
      Unit_Headers  : Unit_Header_Vectors.Vector;
   end record;

end Asis.Gela.Contexts;
