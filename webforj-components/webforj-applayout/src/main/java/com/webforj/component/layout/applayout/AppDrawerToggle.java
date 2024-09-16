package com.webforj.component.layout.applayout;

import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconButton;
import com.webforj.component.icons.TablerIcon;

/**
 * Server-side component that represents a drawer toggle to be used in an {@link AppLayout}.
 *
 * <p>
 * A drawer toggle is a button that toggles the visibility of a drawer in an {@link AppLayout}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see AppLayout
 * @see IconButton
 * @see Icon
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public class AppDrawerToggle extends IconButton {
  static final Icon DEFAULT_ICON = TablerIcon.create("menu-2");

  /**
   * Creates a new drawer toggle with the given icon name and pool.
   *
   * @param name the icon name
   * @param pool the icon pool
   */
  public AppDrawerToggle(String name, String pool) {
    super(name, pool);
    init();
  }

  /**
   * Creates a new drawer toggle with the given icon.
   *
   * @param icon the icon
   */
  public AppDrawerToggle(Icon icon) {
    super(icon.getName(), icon.getPool());
    init();
  }

  /**
   * Creates a new drawer toggle with the default icon.
   */
  public AppDrawerToggle() {
    super(DEFAULT_ICON);
    init();
  }

  /**
   * Configures the button to be used as a drawer toggle.
   */
  private void init() {
    setAttribute("data-drawer-toggle", "");
  }
}
