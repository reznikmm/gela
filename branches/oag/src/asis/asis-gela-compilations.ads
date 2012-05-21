with Asis.Gela.Pools;
with Asis.Gela.Strings;

with Gela; use Gela;

with Gela.Decoders;
with Gela.Encodings;
with Gela.Source_Buffers;
with Gela.Containers.Vectors;

package Asis.Gela.Compilations is

   package Storage_Vectors is new Containers.Vectors
     (Item_Type  => Asis.ASIS_Integer,
      Index_Type => Asis.ASIS_Positive,
      Pool       => Pools.Pool);

   type Line is record
      From, To, Comment : Asis.ASIS_Natural;
   end record;

   package Line_Vectors is new Containers.Vectors
     (Item_Type  => Line,
      Index_Type => Asis.ASIS_Positive,
      Pool       => Pools.Pool);

   type Source_Buffer_Access is
     access all Source_Buffers.Source_Buffer'Class;

   type Operator_Image_Array is array (Operator_Kinds) of Natural;

   type Compilation_Node is record
      Text_Buffer : Strings.Text_Buffer;
      Storage     : Storage_Vectors.Vector;
      Pool        : Pools.Pool_State;
      Buffer      : Source_Buffer_Access;
      Encoding    : Encodings.Encoding;
      Decoder     : Decoders.Decoder_Access;
      Line_List   : Line_Vectors.Vector;
      Oper_Images : Operator_Image_Array;
   end record;

   type Compilation is access Compilation_Node;
   for Compilation'Storage_Pool use Pools.Pool;

   function File_Name (Object : Compilation_Node) return Wide_String;

   procedure Create_Compilation
     (Comp     :    out Compilation;
      File     : in     Wide_String;
      Encoding : in     Encodings.Encoding);

   function Get_Operator_Image
     (Comp : Compilation;
      Oper : Operator_Kinds) return ASIS_Positive;

end Asis.Gela.Compilations;
