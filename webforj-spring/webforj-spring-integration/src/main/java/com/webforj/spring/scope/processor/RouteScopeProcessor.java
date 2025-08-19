package com.webforj.spring.scope.processor;

import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.RouteRelation;
import com.webforj.router.Router;
import com.webforj.spring.scope.BeanStore;
import com.webforj.spring.scope.annotation.SharedFrom;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;
import java.util.Set;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
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
public class RouteScopeProcessor implements Scope, BeanFactoryPostProcessor {
  private static final Logger logger = System.getLogger(RouteScopeProcessor.class.getName());
  private static final String BEAN_STORE_KEY = "webforj.spring.scope.route.store";
  private ConfigurableListableBeanFactory beanFactory;
  private boolean registeredCleanupListener = false;

  /**
   * {@inheritDoc}
   */
  @Override
  public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory)
      throws BeansException {
    this.beanFactory = beanFactory;
    beanFactory.registerScope("webforj-route", this);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object get(String name, ObjectFactory<?> objectFactory) {
    Router router = Router.getCurrent();
    if (router == null) {
      throw new IllegalStateException(
          "Cannot create route-scoped bean '" + name + "' - no Router available");
    }

    String scopeId;

    // Check if bean has @SharedFrom annotation
    SharedFrom sharedFromAnnotation =
        beanFactory != null ? beanFactory.findAnnotationOnBean(name, SharedFrom.class) : null;
    if (sharedFromAnnotation != null) {
      // Use the specified component as the sharing point
      Class<? extends Component> sharingPoint = sharedFromAnnotation.value();

      logger.log(Level.DEBUG, "Bean ''{0}'' has @SharedFrom({1})", name,
          sharingPoint.getSimpleName());

      Optional<RouteRelation<Class<? extends Component>>> activePath =
          router.getRenderer().getActiveRoutePath();

      if (activePath.isPresent()) {
        if (logger.isLoggable(Level.DEBUG)) {
          logger.log(Level.DEBUG, "Current route hierarchy: {0}",
              getHierarchyString(activePath.get()));
        }
      } else {
        logger.log(Level.DEBUG, "No active route path available");
      }

      // Validate that the specified component exists in the current hierarchy
      if (!isComponentInHierarchy(sharingPoint)) {
        String msg = String.format(
            "Component '%s' specified in @SharedFrom for bean '%s' is not in the current route "
                + "hierarchy",
            sharingPoint.getSimpleName(), name);
        throw new IllegalStateException(msg);
      }

      scopeId = createScopeId(sharingPoint);
    } else {
      // Use default root discovery
      scopeId = createScopeId(getRootComponentClass());
    }

    registerCleanupListener(router);
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
   * Clean up all beans and callbacks for a specific route hierarchy in the current request.
   *
   * @param rootClass the root component class of the route hierarchy
   */
  public static void cleanupRoute(Class<? extends Component> rootClass) {
    if (Environment.getCurrent() == null) {
      return;
    }

    try {
      String scopeId = createScopeId(rootClass);
      logger.log(Level.DEBUG, "Cleaning up scope with ID: {0} for component: {1}", scopeId,
          rootClass.getName());
      BeanStore.cleanupScopeInstance(BEAN_STORE_KEY, scopeId);
      logger.log(Level.DEBUG, "Successfully cleaned up scope: {0}", scopeId);
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

  /**
   * Check if a component class is present in the current route hierarchy.
   *
   * @param componentClass the component class to check
   * @return true if the component is in the hierarchy, false otherwise
   */
  private boolean isComponentInHierarchy(Class<? extends Component> componentClass) {
    Router router = Router.getCurrent();
    if (router == null) {
      return false;
    }

    Optional<RouteRelation<Class<? extends Component>>> activePath =
        router.getRenderer().getActiveRoutePath();

    if (activePath.isPresent()) {
      // Traverse the hierarchy to check if the component class is present
      return isComponentInRelation(activePath.get(), componentClass);
    }

    return false;
  }

  /**
   * Recursively check if a component class exists in the route relation tree.
   *
   * @param relation the route relation to check
   * @param componentClass the component class to find
   * @return true if found, false otherwise
   */
  private boolean isComponentInRelation(RouteRelation<Class<? extends Component>> relation,
      Class<? extends Component> componentClass) {
    if (relation.getData().equals(componentClass)) {
      return true;
    }

    if (relation.getChildren() != null) {
      for (RouteRelation<Class<? extends Component>> child : relation.getChildren()) {
        if (isComponentInRelation(child, componentClass)) {
          return true;
        }
      }
    }

    return false;
  }

  /**
   * Get a string representation of the route hierarchy for debugging.
   *
   * @param relation the route relation tree
   * @return string representation of the hierarchy
   */
  private String getHierarchyString(RouteRelation<Class<? extends Component>> relation) {
    StringBuilder sb = new StringBuilder();
    buildHierarchyString(relation, sb, 0);
    return sb.toString();
  }

  private void buildHierarchyString(RouteRelation<Class<? extends Component>> relation,
      StringBuilder sb, int depth) {
    if (depth > 0) {
      sb.append(" -> ");
    }
    sb.append(relation.getData().getSimpleName());
    if (relation.getChildren() != null) {
      for (RouteRelation<Class<? extends Component>> child : relation.getChildren()) {
        buildHierarchyString(child, sb, depth + 1);
      }
    }
  }

  /**
   * Registers a cleanup listener on the router to handle bean cleanup when components are
   * destroyed.
   *
   * @param router the router to register the listener on
   */
  private void registerCleanupListener(Router router) {
    if (registeredCleanupListener) {
      return;
    }

    registeredCleanupListener = true;
    router.addDidLeaveListener(event -> {
      Set<Component> destroyedComponents = event.getContext().getDestroyedComponents();
      if (destroyedComponents == null || destroyedComponents.isEmpty()) {
        logger.log(Level.DEBUG, "No components to cleanup - getDestroyedComponents is empty");
        return;
      }

      destroyedComponents.forEach(component -> {
        Class<? extends Component> componentClass = component.getClass();
        logger.log(Level.DEBUG, "Processing destroyed component: {0}", componentClass.getName());

        // Check if this component has @SharedFrom annotation
        SharedFrom sharedFromAnnotation = componentClass.getAnnotation(SharedFrom.class);

        if (sharedFromAnnotation != null) {
          // This component defines a custom scope root
          // Cleanup beans scoped to the SharedFrom target
          Class<? extends Component> sharingPoint = sharedFromAnnotation.value();
          cleanupRoute(sharingPoint);
          logger.log(Level.DEBUG, "Cleaned up beans for @SharedFrom({0}) on component: {1}",
              sharingPoint.getSimpleName(), componentClass.getSimpleName());
        } else {
          // For components without @SharedFrom, they use default scope (themselves as root)
          // Cleanup beans scoped to this component
          cleanupRoute(componentClass);
          logger.log(Level.DEBUG, "Cleaned up beans for default-scoped component: {0}",
              componentClass.getSimpleName());
        }
      });
    });

    logger.log(Level.DEBUG, "Registered DidLeave listener for route scope cleanup");
  }
}
