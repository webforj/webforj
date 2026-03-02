package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

/**
 * A renderer that displays a colored dot indicator followed by the column's value function result.
 * The dot color is determined by a configurable mapping of cell values to CSS colors. The column
 * should return a string status value.
 *
 * <pre>{@code
 * StatusDotRenderer<MusicRecord> renderer = new StatusDotRenderer<>();
 * renderer.addMapping("Studio", Theme.SUCCESS);
 * renderer.addMapping("Live", new Color(255, 165, 0));
 * renderer.addMapping("Remix", "var(--dwc-color-warning)");
 * renderer.setDefaultColor(Theme.INFO);
 *
 * table.addColumn("recordingType", MusicRecord::getRecordingType).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class StatusDotRenderer<T> extends Renderer<T> {
  private final Map<String, String> colorMap = new HashMap<>();
  private String defaultColor = "var(--dwc-color-default)";
  private String dotSize = "8px";

  /**
   * Maps a cell value to a dot color.
   *
   * @param value the cell value to match
   * @param color the CSS color to use for the dot
   * @return this renderer
   */
  public StatusDotRenderer<T> addMapping(String value, String color) {
    colorMap.put(value, color);
    fireChangeEvent();
    return this;
  }

  /**
   * Maps a cell value to a dot color using a {@link Color}.
   *
   * @param value the cell value to match
   * @param color the color to use for the dot
   * @return this renderer
   */
  public StatusDotRenderer<T> addMapping(String value, Color color) {
    return addMapping(value, toHex(color));
  }

  /**
   * Maps a cell value to a dot color using a {@link Theme}.
   *
   * @param value the cell value to match
   * @param theme the theme to use for the dot color
   * @return this renderer
   */
  public StatusDotRenderer<T> addMapping(String value, Theme theme) {
    return addMapping(value, toThemeColor(theme));
  }

  /**
   * Removes a value-to-color mapping.
   *
   * @param value the cell value to remove
   * @return this renderer
   */
  public StatusDotRenderer<T> removeMapping(String value) {
    colorMap.remove(value);
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the color map.
   *
   * @return an unmodifiable copy of the color map
   */
  public Map<String, String> getColorMap() {
    return Map.copyOf(colorMap);
  }

  /**
   * Sets the default color used when no mapping is found.
   *
   * @param defaultColor the CSS color value
   * @return this renderer
   */
  public StatusDotRenderer<T> setDefaultColor(String defaultColor) {
    this.defaultColor = defaultColor;
    fireChangeEvent();
    return this;
  }

  /**
   * Sets the default color used when no mapping is found using a {@link Color}.
   *
   * @param defaultColor the color to use
   * @return this renderer
   */
  public StatusDotRenderer<T> setDefaultColor(Color defaultColor) {
    return setDefaultColor(toHex(defaultColor));
  }

  /**
   * Sets the default color used when no mapping is found using a {@link Theme}.
   *
   * @param theme the theme to use for the default color
   * @return this renderer
   */
  public StatusDotRenderer<T> setDefaultColor(Theme theme) {
    return setDefaultColor(toThemeColor(theme));
  }

  /**
   * Returns the default color.
   *
   * @return the default CSS color
   */
  public String getDefaultColor() {
    return defaultColor;
  }

  /**
   * Sets the size of the dot.
   *
   * @param dotSize the CSS size value (e.g., {@code "8px"}, {@code "0.5em"})
   * @return this renderer
   */
  public StatusDotRenderer<T> setDotSize(String dotSize) {
    this.dotSize = dotSize;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the size of the dot.
   *
   * @return the dot size
   */
  public String getDotSize() {
    return dotSize;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    StringBuilder sb = new StringBuilder();
    sb.append("<% var __dotColor__ = '").append(defaultColor).append("'; %>");

    for (Map.Entry<String, String> entry : colorMap.entrySet()) {
      String key = entry.getKey().replace("'", "\\'");
      String color = entry.getValue().replace("'", "\\'");
      sb.append("<% if (String(cell.value) === '").append(key).append("') { __dotColor__ = '")
          .append(color).append("'; } %>");
    }

    sb.append("<span style='display:inline-flex;align-items:center;gap:6px'>");
    sb.append("<span style='display:inline-block;width:").append(dotSize).append(";height:")
        .append(dotSize)
        .append(";border-radius:50%;background:<%= __dotColor__ %>;flex-shrink:0'></span>");
    sb.append("<%= cell.value %>");
    sb.append("</span>");

    return sb.toString();
  }

  private static String toHex(Color color) {
    return String.format("#%02x%02x%02x", color.getRed(), color.getGreen(), color.getBlue());
  }

  private static String toThemeColor(Theme theme) {
    String name = toAttributeValue(theme);
    return "var(--dwc-color-" + name + ")";
  }
}
