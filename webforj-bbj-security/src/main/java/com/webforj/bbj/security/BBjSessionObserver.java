package com.webforj.bbj.security;

import com.webforj.PendingResult;
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
 * A navigation that has to wait for the check is held and goes on when the check ends. A navigation
 * that arrives while another one is held stops the held one, so only the newest navigation is
 * rendered. A check that fails never stops a navigation.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjSessionObserver implements RouteRendererObserver {
  private static final Logger logger = System.getLogger(BBjSessionObserver.class.getName());

  private final BBjAuthenticationService service;
  private Held held;
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

    stopHeld(context);
    PendingResult<Void> check;
    try {
      check = service.startCheck();
    } catch (RuntimeException e) {
      warnFailure(e);
      checkedContext = context;
      continueCallback.accept(true);

      return;
    }

    Held entry = new Held(context, continueCallback);
    held = entry;
    check.thenAccept(done -> release(entry, true));
    check.exceptionally(error -> {
      warnFailure(error);
      release(entry, true);

      return null;
    });
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> continueCallback) {
    stopHeld(context);
    continueCallback.accept(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 0;
  }

  private void stopHeld(NavigationContext context) {
    if (held != null && held.context != context) {
      release(held, false);
    }
  }

  private void release(Held entry, boolean proceed) {
    if (held != entry) {
      return;
    }

    held = null;
    if (proceed) {
      checkedContext = entry.context;
    }

    entry.callback.accept(proceed);
  }

  private void warnFailure(Throwable error) {
    logger.log(Level.WARNING, "The sign in check failed with {0}", error.getClass().getName());
  }

  private static final class Held {
    private final NavigationContext context;
    private final Consumer<Boolean> callback;

    Held(NavigationContext context, Consumer<Boolean> callback) {
      this.context = context;
      this.callback = callback;
    }
  }
}
