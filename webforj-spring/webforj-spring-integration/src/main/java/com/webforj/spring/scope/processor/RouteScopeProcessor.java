package com.webforj.spring.scope.processor;

import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.RouteRelation;
import com.webforj.router.Router;
import com.webforj.spring.scope.BeanStore;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.Scope;

/**
 * Spring scope implementation for route-scoped beans in webforJ.
 *
 * <p>
 * This scope binds beans to the lifecycle of a route hierarchy. All components within the same
 * route tree (parent and child routes) share the same instance of a route-scoped bean. When
 * navigating to a different route hierarchy, new bean instances are created.
 * </p>
 *
 * <p>
 * The root component class of the hierarchy acts as the scope identifier. Storage is
 * per-environment (per-request) since Router is created per request.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class RouteScopeProcessor implements Scope {
  private static final Logger logger = System.getLogger(RouteScopeProcessor.class.getName());
  private static final String BEAN_STORE_KEY = "webforj.spring.scope.route.store";

  /**
   * {@inheritDoc}
   */
  @Override
  public Object get(String name, ObjectFactory<?> objectFactory) {
    String scopeId = getScopeId();
    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    return store.get(scopeId, name, objectFactory);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object remove(String name) {
    String scopeId = getScopeIdOrNull();
    if (scopeId == null) {
      return null;
    }

    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    // BeanStore.remove() now handles destruction callback execution
    return store.remove(scopeId, name);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void registerDestructionCallback(String name, Runnable callback) {
    String scopeId = getScopeIdOrNull();
    if (scopeId == null) {
      return;
    }

    BeanStore store = BeanStore.getOrCreate(BEAN_STORE_KEY);
    store.registerDestructionCallback(scopeId, name, callback);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object resolveContextualObject(String key) {
    if ("webforj-route".equals(key)) {
      return getRootComponentClassSafe();
    } else if ("webforj-router".equals(key)) {
      return Router.getCurrent();
    }
    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getConversationId() {
    return getScopeIdOrNull();
  }

  /**
   * Creates a scope ID for the given root component class.
   *
   * @param rootClass the root component class
   * @return the scope identifier
   */
  private static String createScopeId(Class<? extends Component> rootClass) {
    Environment env = Environment.getCurrent();
    if (env == null) {
      throw new IllegalStateException("No Environment available for route scope");
    }

    // Include environment identity to make route scope per-request
    return "webforj-route-" + System.identityHashCode(env) + "-" + rootClass.getName();
  }

  /**
   * Clean up all beans and callbacks for a specific route hierarchy in the current request. This
   * should be called when a route hierarchy is being destroyed.
   *
   * @param rootClass the root component class of the route hierarchy
   */
  public static void cleanupRoute(Class<? extends Component> rootClass) {
    if (Environment.getCurrent() == null) {
      return;
    }

    try {
      String scopeId = createScopeId(rootClass);
      BeanStore.cleanupScopeInstance(BEAN_STORE_KEY, scopeId);
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during route scope cleanup for route ''{0}''",
          rootClass.getSimpleName(), e);
    }
  }

  /**
   * Clean up all route scopes for the current environment. This should be called when the
   * Environment is being destroyed.
   */
  public static void cleanup() {
    if (Environment.getCurrent() == null) {
      return;
    }

    try {
      BeanStore.cleanupAllScopeInstances(BEAN_STORE_KEY);
    } catch (Exception e) {
      logger.log(Level.ERROR, "Error during route scope cleanup", e);
    }
  }

  /**
   * Get the unique identifier for the current route scope.
   *
   * @return the scope identifier
   * @throws IllegalStateException if no route context is available
   */
  private String getScopeId() {
    Class<? extends Component> rootComponent = getRootComponentClass();
    return createScopeId(rootComponent);
  }

  /**
   * Get the unique identifier for the current route scope, or null if not available.
   *
   * @return the scope identifier or null
   */
  private String getScopeIdOrNull() {
    Class<? extends Component> rootComponent = getRootComponentClassSafe();
    if (rootComponent == null) {
      return null;
    }

    return createScopeId(rootComponent);
  }

  /**
   * Get the root component class of the current route hierarchy.
   *
   * @return the root component class
   * @throws IllegalStateException if no route context is available
   */
  private Class<? extends Component> getRootComponentClass() {
    Router router = Router.getCurrent();
    if (router == null) {
      throw new IllegalStateException("No Router available - webforj-route scope is not active");
    }

    // Get the currently active route path (either being rendered or already rendered)
    Optional<RouteRelation<Class<? extends Component>>> activePath =
        router.getRenderer().getActiveRoutePath();

    if (activePath.isPresent()) {
      // Find the first non-Frame component in the hierarchy
      // This ensures different route hierarchies get different scopes
      RouteRelation<Class<? extends Component>> current = activePath.get();

      // If the root is Frame, get the next component
      if (Frame.class.isAssignableFrom(current.getData())) {
        if (current.getChildren() != null && !current.getChildren().isEmpty()) {
          return current.getChildren().get(0).getData();
        }
      }

      return current.getData();
    }

    throw new IllegalStateException("Route scope cannot determine active route hierarchy. "
        + "No route has been rendered yet.");
  }

  /**
   * Get the root component class of the current route hierarchy, returning null if not available.
   *
   * @return the root component class or null
   */
  private Class<? extends Component> getRootComponentClassSafe() {
    try {
      return getRootComponentClass();
    } catch (IllegalStateException e) {
      return null;
    }
  }
}
