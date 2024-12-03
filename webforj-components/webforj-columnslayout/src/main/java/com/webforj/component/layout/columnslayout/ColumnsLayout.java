package com.webforj.component.layout.columnslayout;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasStyle;
import java.util.List;
import java.util.Objects;

/**
 * A component that provides a flexible layout with dynamic columns based on the width of the
 * layout. It adjusts the number of columns automatically according to the specified breakpoints.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@NodeName("dwc-columns-layout")
@SuppressWarnings("squid:S110")
public class ColumnsLayout extends HtmlComponentContainer<ColumnsLayout> {
  static final String COMPONENT_REQUIRED = "component is required";
  static final String COMPONENT_NOT_CHILD = "The component is not a child of this layout";
  static final String PROP_HORIZONTAL_ALIGNMENT = "--dwc-columns-layout-horizontal-alignment";
  static final String PROP_VERTICAL_ALIGNMENT = "--dwc-columns-layout-vertical-alignment";
  static final String PROP_JUSTIFY_SELF = "justify-self";
  static final String PROP_ALIGN_SELF = "align-self";
  static final String PROP_VERTICAL_SPACING = "--dwc-columns-layout-vertical-spacing";
  static final String PROP_HORIZONTAL_SPACING = "--dwc-columns-layout-horizontal-spacing";

  // @formatter:off
  public static final List<Breakpoint> DEFAULT_BREAKPOINTS = List.of(
      new Breakpoint("default", 0, 1),
      new Breakpoint("small", "20em", 1),
      new Breakpoint("medium", "40em", 2)
  );
  // @formatter:on

  /**
   * Represents a columns layout breakpoint.
   */
  public record Breakpoint(String name, String minWidth, int columns) {

    /**
     * Creates a new breakpoint.
     *
     * @param name the name of the breakpoint
     * @param minWidth the minimum width of the breakpoint in pixels
     * @param columns the number of columns
     */
    public Breakpoint(String name, int minWidth, int columns) {
      this(name, minWidth + "px", columns);
    }

    /**
     * Creates a new breakpoint.
     *
     * @param minWidth the minimum width of the breakpoint
     * @param columns the number of columns
     */
    public Breakpoint(String minWidth, int columns) {
      this(minWidth, minWidth, columns);
    }

    /**
     * Creates a new breakpoint.
     *
     * @param minWidth the minimum width of the breakpoint
     * @param columns the number of columns
     */
    public Breakpoint(int minWidth, int columns) {
      this(minWidth + "px", minWidth, columns);
    }
  }

  /**
   * Represents the items horizontal alignment.
   */
  public enum Alignment {
    /**
     * Align the column to the start.
     */
    START("start"),
    /**
     * Align the column to the center.
     */
    CENTER("center"),
    /**
     * Align the column to the end.
     */
    END("end"),
    /**
     * Stretch to fill the layout.
     */
    STRETCH("stretch"),

    /**
     * Align as the items' baselines align.
     */
    BASELINE("baseline"),

    /**
     * Auto alignment.
     */
    AUTO("auto");

    private final String value;

    Alignment(String value) {
      this.value = value;
    }

    /**
     * Get the value of the column alignment.
     *
     * @return the value
     */
    public String getValue() {
      return value;
    }
  }

  // Property descriptors
  private final PropertyDescriptor<List<Breakpoint>> breakpointsProp =
      PropertyDescriptor.property("breakpoints", DEFAULT_BREAKPOINTS);

  /**
   * Creates a new columns layout with default settings.
   */
  public ColumnsLayout() {
    super();
  }

  /**
   * Creates a new columns layout.
   *
   * @param components the components to add to the layout
   */
  public ColumnsLayout(Component... components) {
    super(components);
  }

  /**
   * Creates a new columns layout.
   *
   * @param breakpoints the breakpoints
   * @param components the components to add to the layout
   */
  public ColumnsLayout(List<Breakpoint> breakpoints, Component... components) {
    super(components);
    setBreakpoints(breakpoints);
  }

  /**
   * Creates a new columns layout.
   *
   * @param breakpoints the breakpoints
   */
  public ColumnsLayout(List<Breakpoint> breakpoints) {
    setBreakpoints(breakpoints);
  }

  /**
   * Sets the layout breakpoints.
   *
   * @param breakpoints the breakpoints
   * @return the component itself
   */
  public ColumnsLayout setBreakpoints(List<Breakpoint> breakpoints) {
    set(breakpointsProp, breakpoints);
    return this;
  }

  /**
   * Gets the layout breakpoints.
   *
   * @return the layout breakpoints
   */
  public List<Breakpoint> getBreakpoints() {
    return get(breakpointsProp);
  }

  /**
   * Sets the start column of the given component for the given breakpoint.
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @param column the start column
   * @return the component itself
   */
  public <T extends Component & HasAttribute<T>> ColumnsLayout setColumn(T component,
      String breakpoint, int column) {
    verifyComponent(component);

    String attr = "data-column";
    if (breakpoint != null && !breakpoint.isEmpty()) {
      attr += "-" + breakpoint;
    }

    component.setAttribute(attr, String.valueOf(column));
    return this;
  }

  /**
   * Sets the start column of the given component.
   *
   * @param component the component
   * @param column the start column
   * @return the component itself
   */
  public <T extends Component & HasAttribute<T>> ColumnsLayout setColumn(T component, int column) {
    return setColumn(component, null, column);
  }

  /**
   * Gets the start column of the given component for the given breakpoint.
   *
   * <p>
   * The method will not calculate the column if the column is not set. In this case, the method
   * will return {@code null}.
   * </p>
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @return the start column
   */
  public <T extends Component & HasAttribute<T>> Integer getColumn(T component, String breakpoint) {
    verifyComponent(component);

    String attr = "data-column";
    if (breakpoint != null && !breakpoint.isEmpty()) {
      attr += "-" + breakpoint;
    }

    String value = component.getAttribute(attr);
    if (value == null || value.isEmpty()) {
      return null;
    }

    return Integer.parseInt(value);
  }

  /**
   * Gets the start column of the given component.
   *
   * @param component the component
   * @return the start column
   *
   * @see #getColumn(Component, String)
   */
  public <T extends Component & HasAttribute<T>> int getColumn(T component) {
    return getColumn(component, null);
  }

  /**
   * Sets the column span of the given component for the given breakpoint.
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @param span the column span
   * @return the component itself
   */
  public <T extends Component & HasAttribute<T>> ColumnsLayout setSpan(T component,
      String breakpoint, int span) {
    verifyComponent(component);

    String attr = "data-span";
    if (breakpoint != null && !breakpoint.isEmpty()) {
      attr += "-" + breakpoint;
    }

    component.setAttribute(attr, String.valueOf(span));
    return this;
  }

  /**
   * Sets the column span of the given component.
   *
   * @param component the component
   * @param span the column span
   * @return the component itself
   */
  public <T extends Component & HasAttribute<T>> ColumnsLayout setSpan(T component, int span) {
    return setSpan(component, null, span);
  }

  /**
   * Gets the column span of the given component for the given breakpoint.
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @return the column span
   */
  public <T extends Component & HasAttribute<T>> int getSpan(T component, String breakpoint) {
    verifyComponent(component);

    String attr = "data-span";
    if (breakpoint != null && !breakpoint.isEmpty()) {
      attr += "-" + breakpoint;
    }

    String value = component.getAttribute(attr);
    if (value == null || value.isEmpty()) {
      return 1;
    }

    return Integer.parseInt(value);
  }

  /**
   * Gets the column span of the given component.
   *
   * @param component the component
   * @return the column span
   */
  public <T extends Component & HasAttribute<T>> int getSpan(T component) {
    return getSpan(component, null);
  }

  /**
   * Sets all the columns horizontal alignment.
   *
   * @param alignment the column horizontal alignment
   * @return the component itself
   */
  public ColumnsLayout setHorizontalAlignment(Alignment alignment) {
    setStyle(PROP_HORIZONTAL_ALIGNMENT, alignment.getValue());
    return this;
  }

  /**
   * Sets the column horizontal alignment of the given component.
   *
   * @param component the component
   * @param alignment the column horizontal alignment
   * @return the component itself
   */
  public <T extends Component & HasStyle<T>> ColumnsLayout setHorizontalAlignment(T component,
      Alignment alignment) {
    verifyComponent(component);

    component.setStyle(PROP_JUSTIFY_SELF, alignment.getValue());
    return this;
  }

  /**
   * Gets the column horizontal alignment.
   *
   * @return the column horizontal alignment
   */
  public Alignment getHorizontalAlignment() {
    String value = getStyle(PROP_HORIZONTAL_ALIGNMENT);
    if (value == null || value.isEmpty()) {
      return Alignment.AUTO;
    }

    return Alignment.valueOf(value.toUpperCase());
  }

  /**
   * Gets the column horizontal alignment of the given component.
   *
   * @param component the component
   * @return the column horizontal alignment
   */
  public <T extends Component & HasStyle<T>> Alignment getHorizontalAlignment(T component) {
    verifyComponent(component);

    String value = component.getStyle(PROP_JUSTIFY_SELF);
    if (value == null || value.isEmpty()) {
      return Alignment.AUTO;
    }

    return Alignment.valueOf(value.toUpperCase());
  }

  /**
   * Sets all the columns vertical alignment.
   *
   * @param alignment the column vertical alignment
   * @return the component itself
   */
  public ColumnsLayout setVerticalAlignment(Alignment alignment) {
    setStyle(PROP_VERTICAL_ALIGNMENT, alignment.getValue());
    return this;
  }

  /**
   * Sets the column vertical alignment of the given component.
   *
   * @param component the component
   * @param alignment the column vertical alignment
   * @return the component itself
   */
  public <T extends Component & HasStyle<T>> ColumnsLayout setVerticalAlignment(T component,
      Alignment alignment) {
    verifyComponent(component);

    component.setStyle(PROP_ALIGN_SELF, alignment.getValue());
    return this;
  }

  /**
   * Gets the column vertical alignment.
   *
   * @return the column vertical alignment
   */
  public Alignment getVerticalAlignment() {
    String value = getStyle(PROP_VERTICAL_ALIGNMENT);
    if (value == null || value.isEmpty()) {
      return Alignment.AUTO;
    }

    return Alignment.valueOf(value.toUpperCase());
  }

  /**
   * Gets the column vertical alignment of the given component.
   *
   * @param component the component
   * @return the column vertical alignment
   */
  public <T extends Component & HasStyle<T>> Alignment getVerticalAlignment(T component) {
    verifyComponent(component);

    String value = component.getStyle(PROP_ALIGN_SELF);
    if (value == null || value.isEmpty()) {
      return Alignment.AUTO;
    }

    return Alignment.valueOf(value.toUpperCase());
  }

  /**
   * Sets the vertical spacing between the columns.
   *
   * @param spacing the vertical spacing
   * @return the component itself
   */
  public ColumnsLayout setVerticalSpacing(String spacing) {
    setStyle(PROP_VERTICAL_SPACING, spacing);
    return this;
  }

  /**
   * Sets the vertical spacing between the columns.
   *
   * @param spacing the vertical spacing
   * @return the component itself
   */
  public ColumnsLayout setVerticalSpacing(int spacing) {
    return setVerticalSpacing(spacing + "px");
  }

  /**
   * Gets the vertical spacing between the columns.
   *
   * @return the vertical spacing
   */
  public String getVerticalSpacing() {
    String value = getStyle(PROP_VERTICAL_SPACING);
    return value == null ? "1em" : value;
  }

  /**
   * Sets the horizontal spacing between the columns.
   *
   * @param spacing the horizontal spacing
   * @return the component itself
   */
  public ColumnsLayout setHorizontalSpacing(String spacing) {
    setStyle(PROP_HORIZONTAL_SPACING, spacing);
    return this;
  }

  /**
   * Sets the horizontal spacing between the columns.
   *
   * @param spacing the horizontal spacing
   * @return the component itself
   */
  public ColumnsLayout setHorizontalSpacing(int spacing) {
    return setHorizontalSpacing(spacing + "px");
  }

  /**
   * Gets the horizontal spacing between the columns.
   *
   * @return the horizontal spacing
   */
  public String getHorizontalSpacing() {
    String value = getStyle(PROP_HORIZONTAL_SPACING);
    return value == null ? "1em" : value;
  }

  /**
   * Sets the vertical and horizontal spacing between the columns.
   *
   * @param spacing the vertical and horizontal spacing
   * @return the component itself
   */
  public ColumnsLayout setSpacing(String spacing) {
    setVerticalSpacing(spacing);
    setHorizontalSpacing(spacing);
    return this;
  }

  /**
   * Sets the vertical and horizontal spacing between the columns.
   *
   * @param spacing the vertical and horizontal spacing
   * @return the component itself
   */
  public ColumnsLayout setSpacing(int spacing) {
    return setSpacing(spacing + "px");
  }

  private void verifyComponent(Component component) {
    Objects.requireNonNull(component, COMPONENT_REQUIRED);

    if (!getElement().hasComponent(component)) {
      throw new IllegalArgumentException(COMPONENT_NOT_CHILD);
    }
  }
}
