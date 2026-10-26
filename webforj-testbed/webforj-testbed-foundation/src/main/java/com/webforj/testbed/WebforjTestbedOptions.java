package com.webforj.testbed;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.router.RouteRegistry;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Configures the test context created by {@link WebforjTestbed#setup(WebforjTestbedOptions)}.
 *
 * <p>
 * By default the context starts without an application and without routes. Tests that exercise a
 * full application set the application class with {@link #setApp(Class)}. Tests that exercise
 * routing without an application register routes with {@link #setRouteClasses(Class[])}, scan
 * packages with {@link #setRoutePackages(String[])} or take full control of the registry with
 * {@link #setRegistryConfigurer(Consumer)}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class WebforjTestbedOptions {
  private Class<? extends App> app = null;
  private List<String> routePackages = new ArrayList<>();
  private List<Class<? extends Component>> routeClasses = new ArrayList<>();
  private Consumer<RouteRegistry> registryConfigurer = null;

  /**
   * Sets the application class to launch in the context.
   *
   * <p>
   * The class is launched through the same bootstrap a production deployment uses, so its
   * {@code run} method executes and its annotations, including
   * {@link com.webforj.annotation.Routify @Routify}, take effect. Set an application class when the
   * test exercises whole application behavior. Tests that target single components or single views
   * leave the application unset and use {@link #setRouteClasses(Class[])} or
   * {@link WebforjTestbed#add(Component...)} instead.
   * </p>
   *
   * @param app the application class
   *
   * @return the options themselves
   */
  public WebforjTestbedOptions setApp(Class<? extends App> app) {
    this.app = app;

    return this;
  }

  /**
   * Returns the application class to launch in the context.
   *
   * @return the application class
   */
  public Optional<Class<? extends App>> getApp() {
    return Optional.ofNullable(app);
  }

  /**
   * Sets the packages to scan for components annotated with
   * {@link com.webforj.router.annotation.Route @Route}.
   *
   * <p>
   * Scanning works the way {@link com.webforj.annotation.Routify @Routify} works in a running
   * application. Every component annotated with {@code @Route} inside the given packages is
   * registered, so the context resolves the same route table the application would. Set packages
   * when the test should navigate the real route table. When the test needs only specific views,
   * register them with {@link #setRouteClasses(Class[])} instead.
   * </p>
   *
   * @param routePackages the packages to scan
   *
   * @return the options themselves
   */
  public WebforjTestbedOptions setRoutePackages(String... routePackages) {
    this.routePackages = List.of(routePackages);

    return this;
  }

  /**
   * Returns the packages to scan for components annotated with {@code @Route}.
   *
   * @return the packages to scan
   */
  public List<String> getRoutePackages() {
    return List.copyOf(routePackages);
  }

  /**
   * Sets the component classes annotated with {@link com.webforj.router.annotation.Route @Route} to
   * register in the context.
   *
   * <p>
   * Each class is registered directly in the route registry and its path comes from its
   * {@code @Route} annotation. No package scanning takes place. Register classes when the test
   * should stay isolated to the views it names, which keeps the route table minimal and makes the
   * dependencies of the test explicit. To register everything a package contains, use
   * {@link #setRoutePackages(String[])} instead.
   * </p>
   *
   * @param routeClasses the components to register
   *
   * @return the options themselves
   */
  @SafeVarargs
  public final WebforjTestbedOptions setRouteClasses(Class<? extends Component>... routeClasses) {
    this.routeClasses = List.of(routeClasses);

    return this;
  }

  /**
   * Returns the component classes annotated with {@code @Route} to register in the context.
   *
   * @return the components to register
   */
  public List<Class<? extends Component>> getRouteClasses() {
    return List.copyOf(routeClasses);
  }

  /**
   * Sets the configurer invoked with the route registry of the context.
   *
   * <p>
   * The configurer runs after packages and route classes have been registered and can perform any
   * registration the registry supports. Use it for registrations the other options cannot express,
   * for example {@link RouteRegistry#register(String, Class)} to bind a view to an explicit path
   * regardless of its annotation.
   * </p>
   *
   * @param registryConfigurer the registry configurer
   *
   * @return the options themselves
   */
  public WebforjTestbedOptions setRegistryConfigurer(Consumer<RouteRegistry> registryConfigurer) {
    this.registryConfigurer = registryConfigurer;

    return this;
  }

  /**
   * Returns the configurer invoked with the route registry of the context.
   *
   * @return the registry configurer
   */
  public Optional<Consumer<RouteRegistry>> getRegistryConfigurer() {
    return Optional.ofNullable(registryConfigurer);
  }

  /**
   * Returns whether the options configure any routes.
   *
   * @return true when routes are configured
   */
  public boolean hasRoutes() {
    return !routePackages.isEmpty() || !routeClasses.isEmpty() || registryConfigurer != null;
  }
}
