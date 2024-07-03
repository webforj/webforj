package com.webforj.component.layout.columnslayout;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.html.HtmlComponentContainer;
import com.webforj.concern.HasAttribute;
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

  // @formatter:off
  public static final List<ColumnBreakpoint> DEFAULT_BREAKPOINTS = List.of(
      new ColumnBreakpoint("default", 0, 1),
      new ColumnBreakpoint("small", "20em", 1),
      new ColumnBreakpoint("medium", "40em", 2)
  );
  // @formatter:on

  /**
   * Represents a columns layout breakpoint.
   */
  public record ColumnBreakpoint(String name, String minWidth, int columns) {

    /**
     * Creates a new breakpoint.
     *
     * @param name the name of the breakpoint
     * @param minWidth the minimum width of the breakpoint in pixels
     * @param columns the number of columns
     */
    public ColumnBreakpoint(String name, int minWidth, int columns) {
      this(name, minWidth + "px", columns);
    }

    /**
     * Creates a new breakpoint.
     *
     * @param minWidth the minimum width of the breakpoint
     * @param columns the number of columns
     */
    public ColumnBreakpoint(String minWidth, int columns) {
      this(minWidth, minWidth, columns);
    }

    /**
     * Creates a new breakpoint.
     *
     * @param minWidth the minimum width of the breakpoint
     * @param columns the number of columns
     */
    public ColumnBreakpoint(int minWidth, int columns) {
      this(minWidth + "px", minWidth, columns);
    }
  }

  // Property descriptors
  private final PropertyDescriptor<List<ColumnBreakpoint>> breakpointsProp =
      PropertyDescriptor.property("breakpoints", DEFAULT_BREAKPOINTS);

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
  public ColumnsLayout(List<ColumnBreakpoint> breakpoints, Component... components) {
    super(components);
    setBreakpoints(breakpoints);
  }

  /**
   * Creates a new columns layout.
   *
   * @param breakpoints the breakpoints
   */
  public ColumnsLayout(List<ColumnBreakpoint> breakpoints) {
    setBreakpoints(breakpoints);
  }

  /**
   * Sets the layout breakpoints.
   *
   * @param breakpoints the breakpoints
   * @return the component itself
   */
  public ColumnsLayout setBreakpoints(List<ColumnBreakpoint> breakpoints) {
    set(breakpointsProp, breakpoints);
    return this;
  }

  /**
   * Gets the layout breakpoints.
   *
   * @return the layout breakpoints
   */
  public List<ColumnBreakpoint> getBreakpoints() {
    return get(breakpointsProp);
  }

  /**
   * Sets the column span of the given component for the given breakpoint.
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @param span the column span
   * @return the component itself
   */
  public <T extends Component & HasAttribute<T>> ColumnsLayout setColumnSpan(T component,
      String breakpoint, int span) {
    Objects.requireNonNull(component, COMPONENT_REQUIRED);

    if (!getElement().hasComponent(component)) {
      throw new IllegalArgumentException(COMPONENT_NOT_CHILD);
    }

    String attr = "data-colspan";
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
  public <T extends Component & HasAttribute<T>> ColumnsLayout setColumnSpan(T component,
      int span) {
    return setColumnSpan(component, null, span);
  }

  /**
   * Gets the column span of the given component for the given breakpoint.
   *
   * @param component the component
   * @param breakpoint the breakpoint's name
   * @return the column span
   */
  public <T extends Component & HasAttribute<T>> int getColumnSpan(T component, String breakpoint) {
    Objects.requireNonNull(component, COMPONENT_REQUIRED);

    if (!getElement().hasComponent(component)) {
      throw new IllegalArgumentException(COMPONENT_NOT_CHILD);
    }

    String attr = "data-colspan";
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
  public <T extends Component & HasAttribute<T>> int getColumnSpan(T component) {
    return getColumnSpan(component, null);
  }
}
