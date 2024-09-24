package com.webforj.component.layout.appnav;

import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.layout.appnav.event.AppNavLocationChangedEvent;
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

    onLocationChanged(event -> {
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
   * Adds a listener for the {@link AppNavLocationChangedEvent} event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavLocationChangedEvent> addLocationChangedListener(
      EventListener<AppNavLocationChangedEvent> listener) {
    return addEventListener(AppNavLocationChangedEvent.class, listener);
  }

  /**
   * Alias for {@link #addLocationChangedListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavLocationChangedEvent> onLocationChanged(
      EventListener<AppNavLocationChangedEvent> listener) {
    return addLocationChangedListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected final String getSlotName() {
    return "";
  }
}
