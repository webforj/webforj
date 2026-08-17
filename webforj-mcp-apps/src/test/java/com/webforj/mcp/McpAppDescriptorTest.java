package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.mcp.McpTestViews.BlankDescriptionView;
import com.webforj.mcp.McpTestViews.DashboardView;
import com.webforj.mcp.McpTestViews.HomeView;
import com.webforj.mcp.McpTestViews.NamedView;
import com.webforj.mcp.McpTestViews.PlainView;
import com.webforj.mcp.McpTestViews.SalesReportView;
import com.webforj.mcp.McpTestViews.UserView;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

class McpAppDescriptorTest {

  @Nested
  class ToolName {

    @Test
    @DisplayName("Should derive the tool name from a single segment route")
    void shouldDeriveFromSingleSegment() {
      McpAppDescriptor descriptor = new McpAppDescriptor(DashboardView.class, "/dashboard");

      assertEquals("dashboard", descriptor.getToolName());
    }

    @Test
    @DisplayName("Should join nested route segments with an underscore")
    void shouldJoinNestedSegments() {
      McpAppDescriptor descriptor = new McpAppDescriptor(SalesReportView.class, "/reports/sales");

      assertEquals("reports_sales", descriptor.getToolName());
    }

    @Test
    @DisplayName("Should name the root route after the app")
    void shouldNameRootRoute() {
      McpAppDescriptor descriptor = new McpAppDescriptor(HomeView.class, "/");

      assertEquals("app", descriptor.getToolName());
    }

    @Test
    @DisplayName("Should prefer the name given on the annotation")
    void shouldPreferAnnotatedName() {
      McpAppDescriptor descriptor = new McpAppDescriptor(NamedView.class, "/named");

      assertEquals("custom-tool", descriptor.getToolName());
    }
  }

  @Nested
  class InputSchema {

    @Test
    @DisplayName("Should carry a declared schema verbatim, complex structure included")
    void shouldCarryDeclaredSchemaVerbatim() {
      McpAppDescriptor descriptor = new McpAppDescriptor(McpTestViews.GreetView.class, "/greet");

      assertTrue(descriptor.getInputSchema().contains("\"required\": [\"name\"]"));
      assertTrue(descriptor.getInputSchema().contains("\"enum\": [\"formal\", \"casual\"]"));
      assertTrue(descriptor.getInputSchema().contains("\"lat\": { \"type\": \"number\" }"));
    }

    @Test
    @DisplayName("Should carry no schema when the view declares none")
    void shouldCarryNoSchemaByDefault() {
      McpAppDescriptor descriptor = new McpAppDescriptor(DashboardView.class, "/dashboard");

      assertNull(descriptor.getInputSchema());
    }

    @Test
    @DisplayName("Should reject a schema that is not readable JSON")
    void shouldRejectUnreadableSchema() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(McpTestViews.BrokenSchemaView.class, "/broken"));

      assertTrue(thrown.getMessage().contains("input schema"));
      assertTrue(thrown.getMessage().contains("BrokenSchemaView"));
    }

    @Test
    @DisplayName("Should reject a schema whose root is not an object")
    void shouldRejectNonObjectRootSchema() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(McpTestViews.ArrayRootSchemaView.class, "/array-root"));

      assertTrue(thrown.getMessage().contains("root is not a"));
    }

    @Test
    @DisplayName("Should retain a referenced object schema")
    void shouldRetainReferencedObjectSchema() {
      McpAppDescriptor descriptor =
          new McpAppDescriptor(McpTestViews.ReferencedSchemaView.class, "/referenced");

      assertTrue(descriptor.getInputSchema().contains("\"$ref\""));
    }
  }

  @Nested
  class InputClass {

    @Test
    @DisplayName("Should generate the schema from the declared class, complex structure included")
    void shouldGenerateSchemaFromClass() {
      McpAppDescriptor descriptor = new McpAppDescriptor(McpTestViews.TripView.class, "/trip");

      JsonNode schema = JsonMapper.shared().readTree(descriptor.getInputSchema());

      assertEquals("object", schema.path("type").asString());
      assertEquals("The name of the traveler",
          schema.path("properties").path("name").path("description").asString());
      assertEquals("name", schema.path("required").path(0).asString());
      assertEquals("array", schema.path("properties").path("styles").path("type").asString());
      assertEquals("BUDGET",
          schema.path("properties").path("styles").path("items").path("enum").path(0).asString());
      assertEquals("number", schema.path("properties").path("address").path("properties")
          .path("geo").path("properties").path("lat").path("type").asString());
    }

    @Test
    @DisplayName("Should reject a view declaring the schema in both forms")
    void shouldRejectBothForms() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(McpTestViews.BothFormsView.class, "/both-forms"));

      assertTrue(thrown.getMessage().contains("both"));
      assertTrue(thrown.getMessage().contains("BothFormsView"));
    }
  }

  @Nested
  class Validation {

    @Test
    @DisplayName("Should reject a class without the annotation")
    void shouldRejectUnmarkedClass() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(PlainView.class, "/plain"));

      assertTrue(thrown.getMessage().contains("@McpApp"));
    }

    @Test
    @DisplayName("Should reject a blank description")
    void shouldRejectBlankDescription() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(BlankDescriptionView.class, "/blank"));

      assertTrue(thrown.getMessage().contains("description"));
    }

    @Test
    @DisplayName("Should reject a route that takes required parameters")
    void shouldRejectParameterizedRoute() {
      IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
          () -> new McpAppDescriptor(UserView.class, "/user/:id"));

      assertTrue(thrown.getMessage().contains("/user/:id"));
      assertTrue(thrown.getMessage().contains("custom tool"));
    }

    @Test
    @DisplayName("Should accept a route whose parameters are all optional")
    void shouldAcceptOptionalParameterRoute() {
      McpAppDescriptor descriptor =
          new McpAppDescriptor(McpTestViews.OptionalParamView.class, "/reports/:year?");

      assertEquals("/reports", descriptor.getRoute());
      assertEquals("reports", descriptor.getToolName());
    }

    @Test
    @DisplayName("Should accept a wildcard route and navigate above the wildcard")
    void shouldAcceptWildcardRoute() {
      McpAppDescriptor descriptor =
          new McpAppDescriptor(McpTestViews.WildcardView.class, "/files/:path*");

      assertEquals("/files", descriptor.getRoute());
      assertEquals("files", descriptor.getToolName());
    }

    @Test
    @DisplayName("Should strip layout segments from the route and the tool name")
    void shouldStripLayoutSegments() {
      McpAppDescriptor descriptor =
          new McpAppDescriptor(McpTestViews.LayoutNestedView.class, "/@shell/settings");

      assertEquals("/settings", descriptor.getRoute());
      assertEquals("settings", descriptor.getToolName());
    }
  }

  @Nested
  class DisplayMode {

    @Test
    @DisplayName("Should carry the declared display mode")
    void shouldCarryDeclaredDisplayMode() {
      McpAppDescriptor descriptor = new McpAppDescriptor(McpTestViews.InlineView.class, "/inline");

      assertEquals(McpAppDisplayMode.INLINE, descriptor.getDisplayMode());
    }

    @Test
    @DisplayName("Should ask for fullscreen by default")
    void shouldAskForFullscreenByDefault() {
      McpAppDescriptor descriptor = new McpAppDescriptor(DashboardView.class, "/dashboard");

      assertEquals(McpAppDisplayMode.FULLSCREEN, descriptor.getDisplayMode());
    }

  }

}
