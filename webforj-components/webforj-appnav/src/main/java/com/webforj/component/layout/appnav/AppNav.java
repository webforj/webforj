package com.webforj.component.layout.appnav;

import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.layout.appnav.event.AppNavNavigateEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.Router;
import com.webforj.router.history.Location;

/**
 * A side navigation menu with support for hierarchical and flat menus.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
@NodeName("dwc-app-nav")
public class AppNav extends NavigationContainer<AppNav>
    implements HasClassName<AppNav>, HasStyle<AppNav>, HasVisibility<AppNav> {

  // Property descriptors
  private final PropertyDescriptor<Boolean> autoOpenProp =
      PropertyDescriptor.property("autoOpen", true);

  /**
   * Creates a new {@code AppNav} component.
   */
  public AppNav() {
    super();

    onNavigate(event -> {
      Location location = event.getLocation();
      Router.getCurrent().navigate(location);
    });
  }

  /**
   * Configures whether the nav should open automatically the selected item groups.
   *
   *
   * @param autoOpen {@code true} to open automatically the selected item groups
   * @return the component itself
   */
  public AppNav setAutoOpen(boolean autoOpen) {
    set(autoOpenProp, autoOpen);
    return this;
  }

  /**
   * Returns whether the nav should open automatically the selected item groups.
   *
   * @return {@code true} if the nav should open automatically the selected item groups
   */
  public boolean isAutoOpen() {
    return get(autoOpenProp);
  }

  /**
   * Adds a listener for the navigate event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavNavigateEvent> addNavigateListener(
      EventListener<AppNavNavigateEvent> listener) {
    return addEventListener(AppNavNavigateEvent.class, listener);
  }

  /**
   * Alias for {@link #addNavigateListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavNavigateEvent> onNavigate(
      EventListener<AppNavNavigateEvent> listener) {
    return addNavigateListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected final String getSlotName() {
    return "";
  }
}
