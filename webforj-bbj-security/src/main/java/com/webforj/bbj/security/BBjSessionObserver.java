package com.webforj.bbj.security;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.observer.RouteRendererObserver;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Checks the sign in of a visitor before the router creates a route component.
 *
 * <p>
 * The check runs once for each navigation on the calling thread. A check that fails never stops a
 * navigation.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjSessionObserver implements RouteRendererObserver {
  private static final Logger logger = System.getLogger(BBjSessionObserver.class.getName());

  private final BBjAuthenticationService service;
  private NavigationContext checkedContext;

  BBjSessionObserver(BBjAuthenticationService service) {
    this.service = Objects.requireNonNull(service, "service");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onInstantiate(Class<? extends Component> componentClass, NavigationContext context,
      Consumer<Boolean> continueCallback) {
    if (context == checkedContext) {
      continueCallback.accept(true);

      return;
    }

    try {
      service.check();
    } catch (RuntimeException e) {
      warnFailure(e);
    }

    checkedContext = context;
    continueCallback.accept(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> continueCallback) {
    continueCallback.accept(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 0;
  }

  private void warnFailure(Throwable error) {
    logger.log(Level.WARNING, "The sign in check failed with {0}", error.getClass().getName());
  }
}
