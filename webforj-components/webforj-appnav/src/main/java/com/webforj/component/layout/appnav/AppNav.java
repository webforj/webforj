package com.webforj.component.layout.appnav;

import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.layout.appnav.event.AppNavLocationChangedEvent;
import com.webforj.component.layout.appnav.event.AppNavPinEvent;
import com.webforj.component.layout.appnav.event.AppNavSearchEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.utilities.HtmlText;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


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
  @PropertyMethods(target = Search.class, setter = "setFieldVisible", getter = "isFieldVisible")
  private final PropertyDescriptor<Boolean> searchableProp =
      PropertyDescriptor.property("searchInput", false);
  @PropertyMethods(target = Search.class, setter = "setTerm", getter = "getTerm")
  private final PropertyDescriptor<String> searchTermProp =
      PropertyDescriptor.property("searchTerm", "");
  @PropertyMethods(target = Search.class, setter = "setPlaceholder", getter = "getPlaceholder")
  private final PropertyDescriptor<String> searchPlaceholderProp =
      PropertyDescriptor.property("searchPlaceholder", "Search");
  @PropertyMethods(target = Search.class, setter = "setEmptyMessage", getter = "getEmptyMessage")
  private final PropertyDescriptor<String> emptyMessageProp =
      PropertyDescriptor.property("searchNodata", "No data to display");
  @PropertyMethods(target = Pinning.class, setter = "setEnabled", getter = "isEnabled")
  private final PropertyDescriptor<Boolean> pinnableProp =
      PropertyDescriptor.property("pinnable", false);
  @PropertyMethods(target = Pinning.class, setter = "setAutosave", getter = "isAutosave")
  private final PropertyDescriptor<Boolean> pinAutosaveProp =
      PropertyDescriptor.property("pinAutosave", false);
  @PropertyMethods(target = Pinning.class, setter = "setTitle", getter = "getTitle")
  private final PropertyDescriptor<String> pinnedTitleProp =
      PropertyDescriptor.property("pinnedTitle", "Pinned");
  @PropertyMethods(target = Pinning.class, setter = "setTouchVisible", getter = "isTouchVisible")
  private final PropertyDescriptor<Boolean> pinTouchVisibleProp =
      PropertyDescriptor.property("pinTouchVisible", false);
  // Excluded from the property tester because the setters take an IconDefinition, not the raw
  // string.
  @PropertyExclude
  private final PropertyDescriptor<String> pinUnpinnedIconProp =
      PropertyDescriptor.property("iconUnpinned", "dwc:pin");
  @PropertyExclude
  private final PropertyDescriptor<String> pinPinnedIconProp =
      PropertyDescriptor.property("iconPinned", "dwc:pinned-off");

  private final Search search = new Search();
  private final Pinning pinning = new Pinning();

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
   * Creates a new {@code AppNav} component with the given id.
   *
   * <p>
   * Setting an id (or name) gives pinning autosave a stable storage key, so pinned items are
   * restored across reloads. See {@link Pinning#setAutosave(boolean)}.
   * </p>
   *
   * @param id the id of the component
   * @since 26.01
   */
  public AppNav(String id) {
    this();

    if (id != null) {
      getElement().setAttribute("id", id);
    }
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
   * Returns the configuration object for the navigation's embedded search field.
   *
   * @return the search configuration
   */
  public Search getSearch() {
    return search;
  }

  /**
   * Returns the configuration object for the navigation's pinning.
   *
   * @return the pinning configuration
   */
  public Pinning getPinning() {
    return pinning;
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
   * Adds a listener for the {@link AppNavSearchEvent} event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavSearchEvent> addSearchListener(
      EventListener<AppNavSearchEvent> listener) {
    return addEventListener(AppNavSearchEvent.class, listener);
  }

  /**
   * Alias for {@link #addSearchListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavSearchEvent> onSearch(
      EventListener<AppNavSearchEvent> listener) {
    return addSearchListener(listener);
  }

  /**
   * Adds a listener for the {@link AppNavPinEvent} event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavPinEvent> addPinListener(
      EventListener<AppNavPinEvent> listener) {
    return addEventListener(AppNavPinEvent.class, listener);
  }

  /**
   * Alias for {@link #addPinListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AppNavPinEvent> onPin(EventListener<AppNavPinEvent> listener) {
    return addPinListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected final String getSlotName() {
    return "";
  }

  /**
   * Configures the navigation's embedded search field.
   *
   * @author Hyyan Abo Fakher
   * @since 26.01
   */
  public final class Search {

    /**
     * Sets whether the built in search field is shown.
     *
     * <p>
     * Set to {@code false} to provide your own search field and drive the navigation through
     * {@link #setTerm(String)} and the {@link AppNavSearchEvent} instead.
     * </p>
     *
     * @param fieldVisible {@code true} to show the search field and enable filtering, {@code false}
     *        to hide it
     * @return the search configuration itself
     */
    public Search setFieldVisible(boolean fieldVisible) {
      set(searchableProp, fieldVisible);

      return this;
    }

    /**
     * Checks whether the built in search field is shown.
     *
     * @return {@code true} if the search field is shown, {@code false} otherwise
     */
    public boolean isFieldVisible() {
      return get(searchableProp);
    }

    /**
     * Sets the current search term.
     *
     * @param term the search term
     * @return the search configuration itself
     */
    public Search setTerm(String term) {
      set(searchTermProp, term);

      return this;
    }

    /**
     * Gets the current search term.
     *
     * @return the search term
     */
    public String getTerm() {
      return get(searchTermProp);
    }

    /**
     * Sets the placeholder text of the search field.
     *
     * @param placeholder the placeholder text
     * @return the search configuration itself
     */
    public Search setPlaceholder(String placeholder) {
      set(searchPlaceholderProp, placeholder);

      return this;
    }

    /**
     * Gets the placeholder text of the search field.
     *
     * @return the placeholder text
     */
    public String getPlaceholder() {
      return get(searchPlaceholderProp);
    }

    /**
     * Sets the message shown when a search returns no results.
     *
     * <p>
     * Plain text is rendered as text. To render rich content, wrap the message in
     * {@code <html>...</html>}, for example {@code "<html><strong>Nothing found</strong></html>"}.
     * </p>
     *
     * @param emptyMessage the empty message
     * @return the search configuration itself
     */
    public Search setEmptyMessage(String emptyMessage) {
      set(emptyMessageProp, HtmlText.forHtmlSink(emptyMessage));

      return this;
    }

    /**
     * Gets the message shown when a search returns no results.
     *
     * @return the empty message
     */
    public String getEmptyMessage() {
      return get(emptyMessageProp);
    }
  }

  /**
   * Configures the navigation's pinning.
   *
   * @author Hyyan Abo Fakher
   * @since 26.01
   */
  public final class Pinning {

    /**
     * Sets whether pinning is enabled.
     *
     * <p>
     * When enabled, a pin toggle is shown on navigable leaf items and pinned items are moved into a
     * group at the top of the navigation.
     * </p>
     *
     * @param enabled {@code true} to enable pinning, {@code false} otherwise
     * @return the pinning configuration itself
     */
    public Pinning setEnabled(boolean enabled) {
      set(pinnableProp, enabled);

      return this;
    }

    /**
     * Checks whether pinning is enabled.
     *
     * @return {@code true} if pinning is enabled, {@code false} otherwise
     */
    public boolean isEnabled() {
      return get(pinnableProp);
    }

    /**
     * Sets whether the set of pinned items is persisted to the browser's local storage and restored
     * on reload.
     *
     * <p>
     * Persistence is per browser and requires an id or name on the component. Disable it to take
     * full control of persistence through the {@link AppNavPinEvent} and
     * {@link AppNavItem#setPinned(boolean)} instead, for example to store pins per user.
     * </p>
     *
     * @param autosave {@code true} to persist pins to local storage, {@code false} otherwise
     * @return the pinning configuration itself
     */
    public Pinning setAutosave(boolean autosave) {
      set(pinAutosaveProp, autosave);

      return this;
    }

    /**
     * Checks whether pins are persisted to local storage.
     *
     * @return {@code true} if pins are persisted, {@code false} otherwise
     */
    public boolean isAutosave() {
      return get(pinAutosaveProp);
    }

    /**
     * Sets the title of the pinned group.
     *
     * @param title the title
     * @return the pinning configuration itself
     */
    public Pinning setTitle(String title) {
      set(pinnedTitleProp, title);

      return this;
    }

    /**
     * Gets the title of the pinned group.
     *
     * @return the title
     */
    public String getTitle() {
      return get(pinnedTitleProp);
    }

    /**
     * Sets whether the pin toggle stays visible on touch devices.
     *
     * <p>
     * Touch devices have no hover to reveal the pin, so it is hidden there by default. Enable this
     * to keep the pin visible and tappable on touch.
     * </p>
     *
     * @param touchVisible {@code true} to keep the pin visible on touch, {@code false} otherwise
     * @return the pinning configuration itself
     */
    public Pinning setTouchVisible(boolean touchVisible) {
      set(pinTouchVisibleProp, touchVisible);

      return this;
    }

    /**
     * Checks whether the pin toggle stays visible on touch devices.
     *
     * @return {@code true} if the pin stays visible on touch, {@code false} otherwise
     */
    public boolean isTouchVisible() {
      return get(pinTouchVisibleProp);
    }

    /**
     * Sets the icon shown on items that are not pinned.
     *
     * @param icon the icon
     * @return the pinning configuration itself
     * @throws NullPointerException if {@code icon} is null
     */
    public Pinning setUnpinnedIcon(IconDefinition<?> icon) {
      Objects.requireNonNull(icon, "icon cannot be null");
      set(pinUnpinnedIconProp, String.format("%s:%s", icon.getPool(), icon.getName()));

      return this;
    }

    /**
     * Gets the icon shown on items that are not pinned, in {@code pool:name} form.
     *
     * @return the unpinned icon
     */
    public String getUnpinnedIcon() {
      return get(pinUnpinnedIconProp);
    }

    /**
     * Sets the icon shown on items that are pinned.
     *
     * @param icon the icon
     * @return the pinning configuration itself
     * @throws NullPointerException if {@code icon} is null
     */
    public Pinning setPinnedIcon(IconDefinition<?> icon) {
      Objects.requireNonNull(icon, "icon cannot be null");
      set(pinPinnedIconProp, String.format("%s:%s", icon.getPool(), icon.getName()));

      return this;
    }

    /**
     * Gets the icon shown on items that are pinned, in {@code pool:name} form.
     *
     * @return the pinned icon
     */
    public String getPinnedIcon() {
      return get(pinPinnedIconProp);
    }

    /**
     * Returns the currently pinned items, in tree order.
     *
     * @return the pinned items
     */
    public List<AppNavItem> getItems() {
      List<AppNavItem> result = new ArrayList<>();
      collectPinned(AppNav.this.getItems(), result);

      return result;
    }

    private void collectPinned(List<AppNavItem> items, List<AppNavItem> result) {
      for (AppNavItem item : items) {
        if (item.isPinned()) {
          result.add(item);
        }

        collectPinned(item.getItems(), result);
      }
    }
  }
}
