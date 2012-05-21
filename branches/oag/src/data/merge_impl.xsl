<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml" indent='yes'/>

<xsl:variable name="asis_impl" select="document('asis_impl.xml')"/>

<xsl:template match="/|*|@*">
  <xsl:copy>
   <xsl:apply-templates select="*|@*"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="node">
  <xsl:variable name="name" select="@name"/>
  <xsl:variable name="impl" select="$asis_impl//node[@name=$name]"/>
  <xsl:copy>
   <xsl:apply-templates select="@*"/>
   <xsl:apply-templates select="$impl/attr"/>
   <xsl:apply-templates select="*"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
