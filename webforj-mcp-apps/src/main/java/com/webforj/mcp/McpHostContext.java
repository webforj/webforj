package com.webforj.mcp;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.modelcontextprotocol.spec.McpSchema.Tool;
import java.util.List;
import java.util.Map;

/**
 * The context an MCP host reported, kept current with context change notifications.
 *
 * @param toolInfo the tool call the application renders for
 * @param theme the host theme, {@code light} or {@code dark}
 * @param styles the styling the host hands the application
 * @param displayMode the display mode the application currently renders in
 * @param availableDisplayModes the display modes the host offers
 * @param containerDimensions the dimensions the host gives the application frame
 * @param locale the locale of the host
 * @param timeZone the time zone of the host
 * @param userAgent the user agent of the host
 * @param platform the platform of the host, {@code web}, {@code desktop} or {@code mobile}
 * @param deviceCapabilities the input capabilities of the device
 * @param safeAreaInsets the insets the application keeps clear of host chrome
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
// @formatter:off
@JsonIgnoreProperties(ignoreUnknown = true)
public record McpHostContext(
    ToolInfo toolInfo,
    String theme,
    Styles styles,
    McpAppDisplayMode displayMode,
    List<String> availableDisplayModes,
    ContainerDimensions containerDimensions,
    String locale,
    String timeZone,
    String userAgent,
    String platform,
    DeviceCapabilities deviceCapabilities,
    SafeAreaInsets safeAreaInsets) {

  /**
   * The tool call the application renders for.
   *
   * @param id the request id of the tool call
   * @param tool the tool definition
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ToolInfo(Object id, Tool tool) {
  }

  /**
   * The styling the host hands the application.
   *
   * @param variables the style variables of the host
   * @param css the stylesheet fragments of the host
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Styles(Map<String, String> variables, Css css) {
  }

  /**
   * The stylesheet fragments of the host.
   *
   * @param fonts the font face declarations of the host
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Css(String fonts) {
  }

  /**
   * The dimensions the host gives the application frame. A fixed value and its maximum never
   * arrive together, the host sends one of the two per axis.
   *
   * @param height the fixed frame height in pixels
   * @param maxHeight the greatest frame height the host grows to in pixels
   * @param width the fixed frame width in pixels
   * @param maxWidth the greatest frame width the host grows to in pixels
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record ContainerDimensions(
      Double height, Double maxHeight, Double width, Double maxWidth) {
  }

  /**
   * The input capabilities of the device.
   *
   * @param touch whether the device offers touch input
   * @param hover whether the device offers hover input
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record DeviceCapabilities(Boolean touch, Boolean hover) {
  }

  /**
   * The insets the application keeps clear of host chrome.
   *
   * @param top the top inset in pixels
   * @param right the right inset in pixels
   * @param bottom the bottom inset in pixels
   * @param left the left inset in pixels
   */
  @JsonIgnoreProperties(ignoreUnknown = true)
  public record SafeAreaInsets(Double top, Double right, Double bottom, Double left) {
  }
}
// @formatter:on
