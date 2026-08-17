package com.webforj.mcp;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.mcp.annotation.McpAppAction;
import com.webforj.mcp.annotation.McpAppInput;
import com.webforj.mcp.event.McpAppUpdateEvent;
import com.webforj.mcp.observer.McpAppUpdateObserver;
import com.webforj.router.annotation.Route;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.util.List;

public final class McpTestViews {

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

  @Route("/live")
  @McpApp(description = "Shows the live view")
  static class LiveView extends BaseView implements McpAppUpdateObserver {

    @Override
    public CallToolResult onMcpAppUpdate(McpAppUpdateEvent event) {
      return CallToolResult.builder()
          .addTextContent("live " + event.getArguments().path("q").asString("")).build();
    }
  }

  @Route("/failing-live")
  @McpApp(description = "Shows the failing live view")
  static class FailingLiveView extends BaseView implements McpAppUpdateObserver {

    @Override
    public CallToolResult onMcpAppUpdate(McpAppUpdateEvent event) {
      throw new IllegalStateException("the table is gone");
    }
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
  static class GreetView extends BaseView implements McpAppUpdateObserver {

    @Override
    public CallToolResult onMcpAppUpdate(McpAppUpdateEvent event) {
      return CallToolResult.builder().addTextContent("greeted").build();
    }
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

  @Route("/referenced")
  @McpApp(description = "Carries a referenced object schema", inputSchema = """
      {
        "$ref": "#/$defs/filterInput",
        "$defs": {
          "filterInput": {
            "type": "object",
            "properties": {
              "query": { "type": "string" }
            }
          }
        }
      }""")
  static class ReferencedSchemaView extends BaseView {
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

  @Route("/live-shadow")
  @McpApp(description = "Claims the name of an update tool", name = "live_update")
  static class UpdateShadowingView extends BaseView {
  }

  @Route("/actions")
  @McpApp(name = "actions", description = "Shows an actionable view")
  static class ActionsView extends BaseView {

    @McpAppAction(description = "Filters the current values")
    CallToolResult filter(ActionInput input) {
      return CallToolResult.builder().addTextContent("filtered " + input.query()).build();
    }

    @McpAppAction(description = "Refreshes the current values")
    void refresh() {
      // no-op
    }

    @McpAppAction(description = "Summarizes the current values")
    ActionSummary summarize(ActionInput input) {
      return new ActionSummary(input.query(), input.limit());
    }
  }

  @Route("/input-method")
  @McpApp(description = "Shows a view with an input method")
  static class InputMethodView extends BaseView {

    private ActionInput openingInput;

    @McpAppInput
    void receiveOpeningInput(ActionInput input) {
      openingInput = input;
    }

    ActionInput getOpeningInput() {
      return openingInput;
    }
  }

  @Route("/second-input-method")
  @McpApp(description = "Shows a second view with an input method")
  static class SecondInputMethodView extends BaseView {

    private ActionInput openingInput;

    @McpAppInput
    void receiveOpeningInput(ActionInput input) {
      openingInput = input;
    }

    ActionInput getOpeningInput() {
      return openingInput;
    }
  }

  @Route("/external-actions")
  @McpApp(description = "Shows a view with external actions", actions = ExternalActions.class)
  static class ExternalActionsView extends BaseView {
  }

  public static class ExternalActions {

    @McpAppAction(description = "Names the active external view")
    ActionSummary name(ExternalActionsView view, ActionInput input) {
      return new ActionSummary(view.getClass().getSimpleName(), input.limit());
    }
  }

  @Route("/scalar-action")
  @McpApp(description = "Shows an invalid scalar action")
  static class ScalarActionView extends BaseView {

    @McpAppAction(description = "Accepts an invalid scalar")
    public void accept(String input) {}
  }

  @Route("/conflicting-input")
  @McpApp(description = "Shows conflicting input declarations", input = ActionInput.class)
  static class ConflictingInputView extends BaseView {

    @McpAppInput
    public void receiveOpeningInput(ActionInput input) {}
  }

  @Route("/action-name-clash")
  @McpApp(name = "actions_filter", description = "Claims an action tool name")
  static class ActionNameClashView extends BaseView {
  }

  record ActionInput(@JsonPropertyDescription("Text used to filter values") String query,
      @JsonPropertyDescription("Maximum number of values") int limit) {}

  record ActionSummary(String query, int limit) {}

  static class UnroutedView extends BaseView {
  }
}
