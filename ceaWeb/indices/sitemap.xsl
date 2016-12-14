

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
  <body>
  <h2 align="center">Repositorio completo basado en sitemap</h2>
  <table border="1">
    <tr bgcolor="#9acd32">
      <th>Titulo</th>
      <th>Autor</th>
      <th>url</th>
     <th>Fecha de modificaicon</th>
    </tr>
    <xsl:for-each select="urlset/url">
    <tr>
      <td><xsl:value-of select="title"/></td>
      <td><xsl:value-of select="author"/></td>
      <td><xsl:value-of select="loc"/></td>
      <td><xsl:value-of select="lastmod"/></td>
    </tr>
    </xsl:for-each>
  </table>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>
