package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.mcp.McpTestViews.DashboardView;
import com.webforj.mcp.McpTestViews.FirstClashingView;
import com.webforj.mcp.McpTestViews.HomeView;
import com.webforj.mcp.McpTestViews.PlainView;
import com.webforj.mcp.McpTestViews.SalesReportView;
import com.webforj.mcp.McpTestViews.SecondClashingView;
import com.webforj.mcp.scanfixture.CatalogView;
import com.webforj.router.RouteRegistry;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.spec.McpSchema.CallToolRequest;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.json.JsonMapper;

class McpAppRegistryTest {

  @Nested
  class Discovery {

    @Test
    @DisplayName("Should read only the marked views out of the registry")
    void shouldReadOnlyMarkedViews() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/dashboard", DashboardView.class);
      registry.register("/plain", PlainView.class);
      registry.register("/reports/sales", SalesReportView.class);

      McpAppRegistry projection = McpAppRegistry.ofRegistry(registry);

      assertEquals(List.of("dashboard", "reports-sales"),
          projection.getDescriptors().stream().map(McpAppDescriptor::getToolName).toList());
    }

    @Test
    @DisplayName("Should order the views by tool name whatever the registry order")
    void shouldOrderViewsByToolName() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/reports/sales", SalesReportView.class);
      registry.register("/", HomeView.class);
      registry.register("/dashboard", DashboardView.class);

      McpAppRegistry projection = McpAppRegistry.ofRegistry(registry);

      assertEquals(List.of("app", "dashboard", "reports-sales"),
          projection.getDescriptors().stream().map(McpAppDescriptor::getToolName).toList());
    }

    @Test
    @DisplayName("Should read no views out of a registry without marked routes")
    void shouldReadNoViews() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/plain", PlainView.class);

      assertTrue(McpAppRegistry.ofRegistry(registry).getDescriptors().isEmpty());
    }

    @Test
    @DisplayName("Should reject two views claiming one tool name")
    void shouldRejectClashingToolNames() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/first", FirstClashingView.class);
      registry.register("/second", SecondClashingView.class);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> McpAppRegistry.ofRegistry(registry));

      assertTrue(thrown.getMessage().contains("same-tool"));
      assertTrue(thrown.getMessage().contains(FirstClashingView.class.getName()));
      assertTrue(thrown.getMessage().contains(SecondClashingView.class.getName()));
    }

    @Test
    @DisplayName("Should find the marked views by scanning packages")
    void shouldFindMarkedViewsByScanning() {
      McpAppRegistry projection =
          McpAppRegistry.ofPackages(new String[] {CatalogView.class.getPackageName()});

      assertEquals(1, projection.getDescriptors().size());
      assertEquals("catalog", projection.getDescriptors().get(0).getToolName());
      assertEquals(CatalogView.class, projection.getDescriptors().get(0).getComponentClass());
    }
  }

  @Nested
  class ToolSpecifications {

    @Test
    @DisplayName("Should build one tool per marked view")
    void shouldBuildOneToolPerView() {
      List<SyncToolSpecification> specifications = projectionOfDashboard().getToolSpecifications();

      assertEquals(1, specifications.size());
      assertEquals("dashboard", specifications.get(0).tool().name());
      assertEquals("Shows the sales dashboard", specifications.get(0).tool().description());
    }

    @Test
    @DisplayName("Should point the tool at the resource of its route")
    void shouldPointToolAtResourceOfItsRoute() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);

      @SuppressWarnings("unchecked")
      Map<String, Object> ui = (Map<String, Object>) specification.tool().meta().get("ui");

      assertEquals("ui://webforj/app/dashboard", ui.get("resourceUri"));
    }

    @Test
    @DisplayName("Should publish the declared input schema on the tool, complex structure intact")
    void shouldPublishDeclaredInputSchema() throws Exception {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/greet", McpTestViews.GreetView.class);

      SyncToolSpecification specification =
          McpAppRegistry.ofRegistry(registry).getToolSpecifications().get(0);
      String published = JsonMapper.shared().writeValueAsString(specification.tool().inputSchema());

      assertTrue(published.contains("\"required\":[\"name\"]"));
      assertTrue(published.contains("\"enum\":[\"formal\",\"casual\"]"));
      assertTrue(published.contains("\"lat\""));
      assertTrue(published.contains("\"lon\""));
    }

    @Test
    @DisplayName("Should declare a tool that takes no arguments")
    void shouldDeclareToolWithoutArguments() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);

      Map<String, Object> schema = specification.tool().inputSchema();

      assertEquals("object", schema.get("type"));
      assertTrue(((Map<?, ?>) schema.get("properties")).isEmpty());
    }

    @Test
    @DisplayName("Should answer a call with the declared display mode of its view")
    void shouldAnswerCallWithDeclaredDisplayMode() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/inline", McpTestViews.InlineView.class);

      SyncToolSpecification specification =
          McpAppRegistry.ofRegistry(registry).getToolSpecifications().get(0);
      CallToolResult result = specification.callHandler().apply(null,
          CallToolRequest.builder("inline").arguments(Map.of()).build());

      @SuppressWarnings("unchecked")
      Map<String, Object> structured = (Map<String, Object>) result.structuredContent();

      assertEquals("inline", structured.get("_displayMode"));
    }

    @Test
    @DisplayName("Should answer a call with the route and the fullscreen default")
    void shouldAnswerCallWithRouteAndFullscreenDefault() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);

      CallToolResult result = specification.callHandler().apply(null,
          CallToolRequest.builder("dashboard").arguments(Map.of()).build());

      @SuppressWarnings("unchecked")
      Map<String, Object> structured = (Map<String, Object>) result.structuredContent();

      assertEquals(Map.of("_route", "/dashboard", "_displayMode", "fullscreen"), structured);
    }
  }

  private static McpAppRegistry projectionOfDashboard() {
    RouteRegistry registry = new RouteRegistry();
    registry.register("/dashboard", DashboardView.class);

    return McpAppRegistry.ofRegistry(registry);
  }
}
