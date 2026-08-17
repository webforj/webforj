package com.webforj.mcp;

import com.github.victools.jsonschema.generator.OptionPreset;
import com.github.victools.jsonschema.generator.SchemaGenerator;
import com.github.victools.jsonschema.generator.SchemaGeneratorConfigBuilder;
import com.github.victools.jsonschema.generator.SchemaVersion;
import com.github.victools.jsonschema.module.jackson.JacksonOption;
import com.github.victools.jsonschema.module.jackson.JacksonSchemaModule;
import java.util.Objects;
import tools.jackson.databind.JsonNode;

/**
 * Creates JSON Schema documents for the object payloads accepted by MCP tools.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class McpAppSchemas {

  private McpAppSchemas() {}

  static String generateSchemaDocument(Class<?> inputType) {
    Objects.requireNonNull(inputType, "The input class must not be null");

    try {
      JsonNode schema = Generator.INSTANCE.generateSchema(inputType);
      if (!schema.path("type").asString().equals("object")) {
        throw new IllegalArgumentException("The input class " + inputType.getName()
            + " does not produce an object schema. MCP tool arguments must be an object.");
      }

      return schema.toString();
    } catch (IllegalArgumentException e) {
      throw e;
    } catch (RuntimeException e) {
      throw new IllegalArgumentException(
          "The input schema cannot be generated from the class " + inputType.getName(), e);
    }
  }

  private static final class Generator {
    private static final SchemaGenerator INSTANCE = new SchemaGenerator(
        new SchemaGeneratorConfigBuilder(SchemaVersion.DRAFT_2020_12, OptionPreset.PLAIN_JSON)
            .with(new JacksonSchemaModule(JacksonOption.RESPECT_JSONPROPERTY_REQUIRED)).build());

    private Generator() {}
  }
}
