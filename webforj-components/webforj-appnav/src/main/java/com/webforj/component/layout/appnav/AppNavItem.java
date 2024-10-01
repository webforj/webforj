package com.webforj.component.layout.appnav;


import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasPrefixAndSuffix;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.router.Router;
import com.webforj.router.RouterUtils;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.history.SegmentsBag;
import java.util.Objects;
import java.util.Optional;

/**
 * A menu item for the application navigator {@link AppNav}.
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
@NodeName("dwc-app-nav-item")
public class AppNavItem extends NavigationContainer<AppNavItem> implements HasStyle<AppNavItem>,
    HasClassName<AppNavItem>, HasAttribute<AppNavItem>, HasVisibility<AppNavItem>,
    HasText<AppNavItem>, HasPrefixAndSuffix<AppNavItem>, HasEnablement<AppNavItem> {
  private String path;
  private ParametersBag queryParameters;
  private Component prefix;
  private Component suffix;

  private static final String SLOT_PREFIX = "prefix";
  private static final String SLOT_SUFFIX = "suffix";
  private static final String SLOT_ITEMS = "items";

  // Property descriptors
  @PropertyExclude
  private final PropertyDescriptor<String> pathProp = PropertyDescriptor.property("path", "");
  private final PropertyDescriptor<Boolean> routerIgnoreProp =
      PropertyDescriptor.property("routerIgnore", false);
  private final PropertyDescriptor<NavigationTarget> targetProp =
      PropertyDescriptor.property("target", NavigationTarget.SELF);

  /**
   * Describes the target of the navigation item.
   */
  public enum NavigationTarget {
    /**
     * The current browsing context.
     */
    SELF("_self"),
    /**
     * A new tab, but users can configure browsers to open a new window instead.
     */
    BLANK("_blank"),
    /**
     * The parent browsing context of the current one. If no parent, behaves as {@link #_SELF}.
     */
    PARENT("_parent"),

    /**
     * The topmost browsing context (the "highest" context that’s an ancestor of the current one).
     * If no ancestors, behaves as {@link #_SELF}.
     */
    TOP("_top");

    @SuppressWarnings("unused")
    private final String value;

    NavigationTarget(String value) {
      this.value = value;
    }
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param path the path of the navigation item
   * @param prefix the prefix component of the navigation item
   */
  public AppNavItem(String text, String path, Component prefix) {
    super();
    setText(text);
    setPath(path);

    if (prefix != null) {
      setPrefixComponent(prefix);
    }
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param path the path of the navigation item
   */
  public AppNavItem(String text, String path) {
    this(text, path, null);
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param view the view of the navigation item
   * @param routeParameters the route parameters of the navigation item
   * @param prefix the prefix component of the navigation item
   */
  public AppNavItem(String text, Class<? extends Component> view, ParametersBag routeParameters,
      Component prefix) {
    super();
    setText(text);
    setPath(view, routeParameters);
    if (prefix != null) {
      setPrefixComponent(prefix);
    }
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param view the view of the navigation item
   * @param routeParameters the route parameters of the navigation item
   */
  public AppNavItem(String text, Class<? extends Component> view, ParametersBag routeParameters) {
    this(text, view, routeParameters, null);
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param view the view of the navigation item
   * @param prefix the prefix component of the navigation item
   */
  public AppNavItem(String text, Class<? extends Component> view, Component prefix) {
    this(text, view, null, prefix);
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   * @param view the view of the navigation item
   */
  public AppNavItem(String text, Class<? extends Component> view) {
    this(text, view, null, null);
  }

  /**
   * Constructs a new navigation item.
   *
   * @param text the text of the navigation item
   */
  public AppNavItem(String text) {
    super();
    setText(text);
  }

  /**
   * Sets the path of the navigation item.
   *
   *
   * @param path the path of the navigation item
   * @return the component itself
   */
  public AppNavItem setPath(String path) {
    this.path = path;
    updatePath(path, queryParameters);
    return this;
  }

  /**
   * Sets the path of the navigation item based on the given view and route parameters.
   *
   * @param view the view of the navigation item
   * @param routeParameters the route parameters of the navigation item
   *
   * @return the component itself
   */
  public AppNavItem setPath(Class<? extends Component> view, ParametersBag routeParameters) {
    if (view == null) {
      throw new IllegalArgumentException("View cannot be null");
    }

    Router router = Router.getCurrent();
    if (router == null) {
      throw new IllegalStateException("Router is not initialized");
    }

    Optional<String> uri =
        router.getUri(view, routeParameters == null ? new ParametersBag() : routeParameters);
    if (uri.isEmpty()) {
      throw new IllegalArgumentException("Trying to build a navigation item for a view that is not"
          + " registered in the router: " + view.getName());
    }

    return setPath(uri.get());
  }

  /**
   * Sets the path of the navigation item based on the given view.
   *
   * @param view the view of the navigation item
   *
   * @return the component itself
   */
  public AppNavItem setPath(Class<? extends Component> view) {
    return setPath(view, null);
  }

  /**
   * Gets the path of the navigation item.
   *
   * @return the path of the navigation item
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the query parameters of the navigation item.
   *
   * @param queryParameters the query parameters of the navigation item
   * @return the component itself
   */
  public AppNavItem setQueryParameters(ParametersBag queryParameters) {
    this.queryParameters = queryParameters;
    updatePath(path, queryParameters);
    return this;
  }

  /**
   * Gets the query parameters of the navigation item.
   *
   * @return the query parameters of the navigation item
   */
  public ParametersBag getQueryParameters() {
    return queryParameters;
  }

  /**
   * Gets the full path of the navigation item.
   *
   * @return the full path of the navigation item
   */
  public String getFullPath() {
    return get(pathProp);
  }

  /**
   * Returns whether the navigation item should the router rules and behave as a normal link.
   *
   * @param routerIgnore {@code true} if the navigation item should ignore the router rules and
   *        behave as a normal link, {@code false} otherwise
   * @return the component itself
   */
  public AppNavItem setRouterIgnore(boolean routerIgnore) {
    set(routerIgnoreProp, routerIgnore);
    return this;
  }

  /**
   * Returns whether the navigation item should the router rules and behave as a normal link.
   *
   * @return {@code true} if the navigation item should ignore the router rules and behave as a
   *         normal link, {@code false} otherwise
   */
  public boolean isRouterIgnore() {
    return get(routerIgnoreProp);
  }

  /**
   * Sets the target of the navigation item.
   *
   * @param target the target of the navigation item
   * @return the component itself
   */
  public AppNavItem setTarget(NavigationTarget target) {
    set(targetProp, target);
    return this;
  }

  /**
   * Gets the target of the navigation item.
   *
   * @return the target of the navigation item
   */
  public NavigationTarget getTarget() {
    return get(targetProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppNavItem setPrefixComponent(Component prefix) {
    if (prefix.equals(this.prefix)) {
      return this;
    }

    if (this.prefix != null) {
      this.prefix.destroy();
    }

    this.prefix = prefix;
    getElement().add(SLOT_PREFIX, prefix);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getPrefixComponent() {
    return prefix;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AppNavItem setSuffixComponent(Component suffix) {
    if (suffix.equals(this.suffix)) {
      return this;
    }

    if (this.suffix != null) {
      this.suffix.destroy();
    }

    this.suffix = suffix;
    getElement().add(SLOT_SUFFIX, suffix);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component getSuffixComponent() {
    return suffix;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected final String getSlotName() {
    return SLOT_ITEMS;
  }

  /**
   * Computes the href of the navigation item based on the path and query parameters.
   *
   * @param path the path of the navigation item
   * @param queryParameters the query parameters of the navigation item
   */
  private void updatePath(String path, ParametersBag queryParameters) {
    Objects.requireNonNull(path, "Path cannot be null");


    Location location = new Location(SegmentsBag.of(path),
        queryParameters == null ? new ParametersBag() : queryParameters, "");
    set(pathProp, location.toString());
  }
}
