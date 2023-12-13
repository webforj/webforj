package org.dwcj.component.window;

import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.Environment;
import org.dwcj.component.Component;
import org.dwcj.exceptions.DwcjAppInitializeException;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Represents a Frame window in the application.
 *
 * <p>
 * Frames are top-level windows that can contain other UI components. Frames are typically used as
 * the main window of an application and cannot be nested.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class Frame extends Window {

  /**
   * Enum representing the various areas of a Frame. Each constant corresponds to a specific area
   * within the window.
   */
  public enum Area {

    /**
     * The top-level window area including title bar, borders, and content.
     */
    WINDOW("BBjTopLevelWindow"),

    /**
     * The center area of the top-level window, typically used for main content.
     */
    CENTER("BBjTopLevelWindow-center"),

    /**
     * The content area within the center of the top-level window, where components are placed.
     */
    CONTENT("BBjTopLevelWindow-content");

    private final String value;

    Area(String value) {
      this.value = value;
    }

    /**
     * Retrieves the string value associated with the area.
     *
     * @return The string value of the area.
     */
    public String getValue() {
      return value;
    }
  }

  /**
   * Constructs a new Frame window.
   *
   * @throws DwcjAppInitializeException If there is an error initializing the Frame.
   */
  public Frame() throws DwcjAppInitializeException {
    this("");
  }

  /**
   * Constructs a new Frame window.
   *
   * @param title The title of the Frame.
   *
   * @throws DwcjAppInitializeException If there is an error initializing the Frame.
   */
  public Frame(String title) throws DwcjAppInitializeException {
    try {
      byte[] flags = new byte[] {(byte) 0x01, (byte) 0x11, (byte) 0x10, (byte) 0x80};
      BasisNumber ctx =
          BasisNumber.createBasisNumber(Environment.getCurrent().getSysGui().getAvailableContext());
      BBjWindow wnd = Environment.getCurrent().getSysGui().addWindow(ctx, "", flags);
      setTitle(title);
      init(wnd);
    } catch (NumberFormatException | BBjException e) {
      throw new DwcjAppInitializeException("Failed to create Frame", e);
    }
  }

  /**
   * Constructs a new Frame window.
   *
   * @param components The components to be added to the Frame.
   *
   * @throws DwcjAppInitializeException If there is an error initializing the Frame.
   */
  public Frame(Component... components) throws DwcjAppInitializeException {
    this();
    add(components);
  }

  /**
   * Constructs a new Frame window.
   *
   * @param title The title of the Frame.
   * @param components The components to be added to the Frame.
   *
   * @throws DwcjAppInitializeException If there is an error initializing the Frame.
   */
  public Frame(String title, Component... components) throws DwcjAppInitializeException {
    this(title);
    add(components);
  }

  /**
   * Constructs a Frame based on an existing BBjWindow.
   *
   * @param bbjWindow The BBjWindow to initialize the Frame with.
   */
  Frame(BBjWindow bbjWindow) {
    init(bbjWindow);
  }

  @Override
  public void add(Component... components) {
    for (Component c : components) {
      if (c instanceof Frame) {
        throw new IllegalArgumentException("Cannot add a Frame to a Frame");
      }
    }

    super.add(components);
  }

  /**
   * Aliases for {@link #setText(String)}.
   *
   * <p>
   * Sets the title of the Frame.
   * </p>
   *
   * @param title The title to be set.
   *
   * @return The component itself.
   */
  public Frame setTitle(String title) {
    setText(title);
    return this;
  }

  /**
   * Aliases for {@link #getText()}.
   *
   * <p>
   * Gets the title of the Frame
   * </p>
   *
   * @return The title of the Frame.
   */
  public String getTitle() {
    return getText();
  }

  /**
   * Sets a CSS property to a specific value.
   *
   * <p>
   * This method is intended to be used to modify a single CSS property of a component.
   * </p>
   *
   * @param property The CSS property to be changed
   * @param value The value to be assigned to the CSS property
   * @param area The area of the Frame to apply the style to.
   *
   * @return @return The component itself.
   */
  public Window setStyle(String property, String value, Area area) {
    BBjTopLevelWindow topLevelWindow = getBbjTopLevelWindow();

    try {
      switch (area) {
        case WINDOW:
          topLevelWindow.setOuterStyle(property, value);
          break;
        case CENTER:
          topLevelWindow.setStyle(property, value);
          break;
        case CONTENT:
          topLevelWindow.setPanelStyle(property, value);
          break;
        default:
          break;
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Window setStyle(String property, String value) {
    return setStyle(property, value, Area.CONTENT);
  }

  /**
   * Gets the value of a CSS property.
   *
   * <p>
   * This method is intended to be used to retrieve the value of a CSS property of a component.
   * </p>
   *
   * @param property The CSS property to be retrieved
   * @param area The area of the Frame to retrieve the style from.
   *
   * @return String containing the value of the CSS property
   */
  public String getStyle(String property, Area area) {
    BBjTopLevelWindow topLevelWindow = getBbjTopLevelWindow();

    try {
      switch (area) {
        case WINDOW:
          return topLevelWindow.getOuterStyle(property);
        case CENTER:
          return topLevelWindow.getStyle(property);
        case CONTENT:
          return topLevelWindow.getPanelStyle(property);
        default:
          return "";
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return getStyle(property, Area.CONTENT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    try {
      return getBbjTopLevelWindow().getComputedPanelStyle(property);
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Adds a CSS class to the list of CSS classes for the component.
   *
   * @param className the name of the CSS class to be added
   * @param area The area of the Frame to add the class to.
   *
   * @return the component itself after adding the class
   */
  public Window addClassName(String className, Area area) {
    BBjTopLevelWindow topLevelWindow = getBbjTopLevelWindow();

    try {
      switch (area) {
        case WINDOW:
          topLevelWindow.addOuterClass(className);
          break;
        case CENTER:
          topLevelWindow.addClass(className);
          break;
        case CONTENT:
          topLevelWindow.addPanelClass(className);
          break;
        default:
          break;
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Window addClassName(String selector) {
    return addClassName(selector, Area.CONTENT);
  }

  /**
   * Removes a CSS class from the list of CSS classes for the component.
   *
   * @param className the name of the CSS class to be removed
   * @param area The area of the Frame to remove the class from.
   *
   * @return the component itself after removing the class
   */
  public Window removeClassName(String className, Area area) {
    BBjTopLevelWindow topLevelWindow = getBbjTopLevelWindow();

    try {
      switch (area) {
        case WINDOW:
          topLevelWindow.removeOuterClass(className);
          break;
        case CENTER:
          topLevelWindow.removeClass(className);
          break;
        case CONTENT:
          topLevelWindow.removePanelClass(className);
          break;
        default:
          break;
      }
    } catch (BBjException e) {
      throw new DwcjRuntimeException(e);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Window removeClassName(String selector) {
    return removeClassName(selector, Area.CONTENT);
  }

  private void init(BBjWindow bbjWindow) {
    setBbjWindow(bbjWindow);
    create(this);
  }

  private BBjTopLevelWindow getBbjTopLevelWindow() {
    return (BBjTopLevelWindow) getBbjWindow();
  }
}
