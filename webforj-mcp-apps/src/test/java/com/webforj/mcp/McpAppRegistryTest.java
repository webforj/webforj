package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.mcp.McpTestViews.DashboardView;
import com.webforj.mcp.McpTestViews.FirstClashingView;
import com.webforj.mcp.McpTestViews.HomeView;
import com.webforj.mcp.McpTestViews.PlainView;
import com.webforj.mcp.McpTestViews.SalesReportView;
import com.webforj.mcp.McpTestViews.SecondClashingView;
import com.webforj.mcp.scanfixture.CatalogView;
import com.webforj.router.RouteRegistry;
import io.modelcontextprotocol.server.McpServerFeatures.SyncToolSpecification;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpSchema.CallToolRequest;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
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
    @DisplayName("Should build an opening and an update tool for a view answering updates")
    void shouldBuildOpenAndUpdateToolPerView() {
      List<SyncToolSpecification> specifications = projectionOfLive().getToolSpecifications();

      assertEquals(2, specifications.size());
      assertEquals("live", specifications.get(0).tool().name());
      assertEquals("Shows the live view", specifications.get(0).tool().description());
      assertEquals("live_update", specifications.get(1).tool().name());
      assertTrue(specifications.get(1).tool().description().contains("already open"));
    }

    @Test
    @DisplayName("Should build only the opening tool for a view answering no updates")
    void shouldBuildOnlyOpeningToolWithoutObserver() {
      List<SyncToolSpecification> specifications = projectionOfDashboard().getToolSpecifications();

      assertEquals(1, specifications.size());
      assertEquals("dashboard", specifications.get(0).tool().name());
    }

    @Test
    @DisplayName("Should publish one tool for each action with its object schema")
    void shouldPublishActionTools() throws Exception {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/actions", McpTestViews.ActionsView.class);

      List<SyncToolSpecification> specifications =
          McpAppRegistry.ofRegistry(registry).getToolSpecifications();

      assertEquals(List.of("actions", "actions_filter", "actions_refresh", "actions_summarize"),
          specifications.stream().map(specification -> specification.tool().name()).toList());
      String schema =
          JsonMapper.shared().writeValueAsString(specifications.get(1).tool().inputSchema());
      assertTrue(schema.contains("\"query\""));
      assertTrue(schema.contains("\"limit\""));
      assertNull(specifications.get(1).tool().meta());
    }

    @Test
    @DisplayName("Should use an input method to define the opening tool schema")
    void shouldUseInputMethodSchema() throws Exception {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/input-method", McpTestViews.InputMethodView.class);

      SyncToolSpecification specification =
          McpAppRegistry.ofRegistry(registry).getToolSpecifications().get(0);
      String schema = JsonMapper.shared().writeValueAsString(specification.tool().inputSchema());

      assertTrue(schema.contains("\"query\""));
      assertTrue(schema.contains("\"limit\""));
    }

    @Test
    @DisplayName("Should point the opening tool at the resource of its route")
    void shouldPointToolAtResourceOfItsRoute() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);

      @SuppressWarnings("unchecked")
      Map<String, Object> ui = (Map<String, Object>) specification.tool().meta().get("ui");

      assertEquals("ui://webforj/app/dashboard", ui.get("resourceUri"));
    }

    @Test
    @DisplayName("Should keep the update tool free of resource meta")
    void shouldKeepUpdateToolFreeOfResourceMeta() {
      SyncToolSpecification specification = projectionOfLive().getToolSpecifications().get(1);

      assertNull(specification.tool().meta());
    }

    @Test
    @DisplayName("Should carry the declared input schema on the update tool too")
    void shouldCarryInputSchemaOnUpdateTool() throws Exception {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/greet", McpTestViews.GreetView.class);

      SyncToolSpecification specification =
          McpAppRegistry.ofRegistry(registry).getToolSpecifications().get(1);
      String published = JsonMapper.shared().writeValueAsString(specification.tool().inputSchema());

      assertEquals("greet_update", specification.tool().name());
      assertTrue(published.contains("\"required\":[\"name\"]"));
    }

    @Test
    @DisplayName("Should issue an instance token in the meta of the opening result")
    void shouldIssueInstanceTokenInOpeningResult() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);
      McpSyncServerExchange exchange = mock(McpSyncServerExchange.class);
      when(exchange.sessionId()).thenReturn("session-open");

      CallToolResult result = specification.callHandler().apply(exchange,
          CallToolRequest.builder("dashboard").arguments(Map.of()).build());

      assertNotNull(result.meta().get("webforj/instance"));
    }

    @Test
    @DisplayName("Should refuse an update call of a session that opened no view")
    void shouldRefuseUpdateCallWithoutOpenView() {
      SyncToolSpecification specification = projectionOfLive().getToolSpecifications().get(1);
      McpSyncServerExchange exchange = mock(McpSyncServerExchange.class);
      when(exchange.sessionId()).thenReturn("session-never-opened");

      CallToolResult result = specification.callHandler().apply(exchange,
          CallToolRequest.builder("live_update").arguments(Map.of()).build());

      assertTrue(result.isError());
      assertTrue(((TextContent) result.content().get(0)).text().contains("live"));
    }

    @Test
    @DisplayName("Should reject a view whose name collides with an update tool")
    void shouldRejectNameCollidingWithUpdateTool() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/live", McpTestViews.LiveView.class);
      registry.register("/live-shadow", McpTestViews.UpdateShadowingView.class);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> McpAppRegistry.ofRegistry(registry));

      assertTrue(thrown.getMessage().contains("live_update"));
    }

    @Test
    @DisplayName("Should reject an action whose schema is not an object")
    void shouldRejectScalarActionInput() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/scalar-action", McpTestViews.ScalarActionView.class);

      IllegalArgumentException thrown =
          assertThrows(IllegalArgumentException.class, () -> McpAppRegistry.ofRegistry(registry));

      assertTrue(thrown.getMessage().contains("object schema"));
    }

    @Test
    @DisplayName("Should reject an input method declared with an input class")
    void shouldRejectConflictingInputDeclarations() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/conflicting-input", McpTestViews.ConflictingInputView.class);

      IllegalArgumentException thrown =
          assertThrows(IllegalArgumentException.class, () -> McpAppRegistry.ofRegistry(registry));

      assertTrue(thrown.getMessage().contains("@McpAppInput"));
    }

    @Test
    @DisplayName("Should reject an opening tool that collides with an action tool")
    void shouldRejectActionToolNameCollision() {
      RouteRegistry registry = new RouteRegistry();
      registry.register("/actions", McpTestViews.ActionsView.class);
      registry.register("/action-name-clash", McpTestViews.ActionNameClashView.class);

      IllegalStateException thrown =
          assertThrows(IllegalStateException.class, () -> McpAppRegistry.ofRegistry(registry));

      assertTrue(thrown.getMessage().contains("actions_filter"));
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
      CallToolResult result = specification.callHandler().apply(openingExchange(),
          CallToolRequest.builder("inline").arguments(Map.of()).build());

      @SuppressWarnings("unchecked")
      Map<String, Object> structured = (Map<String, Object>) result.structuredContent();

      assertEquals("inline", structured.get("_displayMode"));
    }

    @Test
    @DisplayName("Should answer a call with the route and the fullscreen default")
    void shouldAnswerCallWithRouteAndFullscreenDefault() {
      SyncToolSpecification specification = projectionOfDashboard().getToolSpecifications().get(0);

      CallToolResult result = specification.callHandler().apply(openingExchange(),
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

  private static McpAppRegistry projectionOfLive() {
    RouteRegistry registry = new RouteRegistry();
    registry.register("/live", McpTestViews.LiveView.class);

    return McpAppRegistry.ofRegistry(registry);
  }

  private static McpSyncServerExchange openingExchange() {
    McpSyncServerExchange exchange = mock(McpSyncServerExchange.class);
    when(exchange.sessionId()).thenReturn("session-" + System.nanoTime());

    return exchange;
  }
}
