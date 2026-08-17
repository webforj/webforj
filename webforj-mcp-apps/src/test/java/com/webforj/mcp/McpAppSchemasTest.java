package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

class McpAppSchemasTest {

  @Test
  @DisplayName("Should generate the schema document from a Jackson bound class")
  void shouldGenerateSchemaDocument() {
    JsonNode schema = JsonMapper.shared()
        .readTree(McpAppSchemas.generateSchemaDocument(McpTestViews.TripInput.class));

    assertEquals("object", schema.path("type").asString());
    assertTrue(schema.path("properties").has("name"));
    assertTrue(schema.path("properties").has("styles"));
    assertEquals("name", schema.path("required").path(0).asString());
  }

  @Test
  @DisplayName("Should refuse a null input class")
  void shouldRefuseNullInput() {
    assertThrows(NullPointerException.class, () -> McpAppSchemas.generateSchemaDocument(null));
  }
}
