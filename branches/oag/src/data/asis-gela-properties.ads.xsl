<?xml version="1.0" encoding="koi8-r"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
package Asis.Gela.Properties is
   type Global_Kinds is
     (<xsl:apply-templates select="//node[not (@abstract)]"/>);
   --
  <xsl:apply-templates select="//node[@abstract]" mode="subtype"/>

   type Property_Kinds is
     (<xsl:call-template name="make-unique">
   <xsl:with-param name="nodes" select="//attr[not (@skip)]"/>
 </xsl:call-template>);

   function Property_Index
     (Global_Kind : Global_Kinds;
      Property    : Property_Kinds) return Asis.ASIS_Natural;
   pragma Inline (Property_Index);

   function Size (Global_Kind : Global_Kinds) return Asis.ASIS_Positive;
   pragma Inline (Size);

   function Children (Global_Kind : Global_Kinds) return Traverse_List;

end Asis.Gela.Properties;

</xsl:template>

<xsl:template match="node">
   <xsl:value-of select="@name"/>
   <xsl:if test="@name!='An_Any_Compilation_Unit'">
     <xsl:text>,
      </xsl:text>
   </xsl:if>
</xsl:template>

<xsl:template match="node" mode="subtype">
   <xsl:if test="starts-with(@name,'An_') or starts-with(@name,'A_')">
     <xsl:variable name="childs" select="descendant::node[not (@abstract)]"/>
   subtype <xsl:value-of select="@name"/> is Global_Kinds range
      <xsl:value-of select="$childs[1]/@name"
       /> .. <xsl:if test="@name='An_Access_Definition'">
             <xsl:text>
      </xsl:text>
    </xsl:if>
    <xsl:value-of select="$childs[count($childs)]/@name"/>
    <xsl:text>;

</xsl:text>
   </xsl:if>
</xsl:template>

<xsl:template name="make-unique">
  <xsl:param name="nodes"/>
  <xsl:for-each select="$nodes">
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="first" select="$nodes[@name=$name][1]"/>
    <xsl:if test="generate-id(.)=generate-id($first)">
      <xsl:if test="generate-id(.) != generate-id($nodes[1])">
      <xsl:text>,
      </xsl:text>
      </xsl:if>
      <xsl:value-of select="$name"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
