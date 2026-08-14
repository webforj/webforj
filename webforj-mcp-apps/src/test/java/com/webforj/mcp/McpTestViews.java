package com.webforj.mcp;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.router.annotation.Route;
import java.util.List;

final class McpTestViews {

  private McpTestViews() {}

  abstract static class BaseView extends Component {

    @Override
    protected void onCreate(Window window) {}

    @Override
    protected void onDestroy() {}
  }

  @Route("/dashboard")
  @McpApp(description = "Shows the sales dashboard")
  static class DashboardView extends BaseView {
  }

  @Route("/reports/sales")
  @McpApp(description = "Shows the sales report")
  static class SalesReportView extends BaseView {
  }

  @Route("/named")
  @McpApp(description = "Shows a named view", name = "custom-tool")
  static class NamedView extends BaseView {
  }

  @Route("/")
  @McpApp(description = "Shows the home page")
  static class HomeView extends BaseView {
  }

  @Route("/plain")
  static class PlainView extends BaseView {
  }

  @Route("/user/:id")
  @McpApp(description = "Shows one user")
  static class UserView extends BaseView {
  }

  @Route("/reports/:year?")
  @McpApp(description = "Shows the reports, current year by default")
  static class OptionalParamView extends BaseView {
  }

  @Route("/greet")
  @McpApp(description = "Greets a person", inputSchema = """
      {
        "type": "object",
        "properties": {
          "name": { "type": "string", "description": "The person to greet" },
          "styles": { "type": "array", "items": { "enum": ["formal", "casual"] } },
          "address": {
            "type": "object",
            "properties": {
              "city": { "type": "string" },
              "geo": {
                "type": "object",
                "properties": {
                  "lat": { "type": "number" },
                  "lon": { "type": "number" }
                }
              }
            }
          }
        },
        "required": ["name"]
      }""")
  static class GreetView extends BaseView {
  }

  @Route("/trip")
  @McpApp(description = "Plans a trip", input = TripInput.class)
  static class TripView extends BaseView {
  }

  @Route("/both-forms")
  @McpApp(description = "Declares the schema in both forms", inputSchema = "{\"type\":\"object\"}",
      input = TripInput.class)
  static class BothFormsView extends BaseView {
  }

  record TripInput(
      @JsonPropertyDescription("The name of the traveler") @JsonProperty(required = true)
      String name,
      @JsonPropertyDescription("The preferred travel styles") List<TravelStyle> styles,
      @JsonPropertyDescription("The destination address") Address address) {

    enum TravelStyle {
      BUDGET, LUXURY
    }

    record Address(@JsonPropertyDescription("The city name") String city, Geo geo) {
    }

    record Geo(double lat, double lon) {
    }
  }

  @Route("/inline")
  @McpApp(description = "Declares its display mode", displayMode = McpAppDisplayMode.INLINE)
  static class InlineView extends BaseView {
  }

  @Route("/broken")
  @McpApp(description = "Carries an unreadable schema", inputSchema = "{not json")
  static class BrokenSchemaView extends BaseView {
  }

  @Route("/array-root")
  @McpApp(description = "Carries a schema whose root is not an object", inputSchema = "[1,2]")
  static class ArrayRootSchemaView extends BaseView {
  }

  @Route("/files/:path*")
  @McpApp(description = "Shows the file browser")
  static class WildcardView extends BaseView {
  }

  @Route("/@shell/settings")
  @McpApp(description = "Shows the settings")
  static class LayoutNestedView extends BaseView {
  }

  @Route("/blank")
  @McpApp(description = "  ")
  static class BlankDescriptionView extends BaseView {
  }

  @Route("/first")
  @McpApp(description = "First view", name = "same-tool")
  static class FirstClashingView extends BaseView {
  }

  @Route("/second")
  @McpApp(description = "Second view", name = "same-tool")
  static class SecondClashingView extends BaseView {
  }

  static class UnroutedView extends BaseView {
  }
}
