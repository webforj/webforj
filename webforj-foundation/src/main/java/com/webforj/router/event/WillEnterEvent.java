package com.webforj.router.event;

import com.webforj.router.NavigationContext;
import java.util.function.Consumer;

/**
 * {@code WillEnterEvent} is an event object which is fired before the router attempts to enter a
 * route and attach its component to the DOM.
 *
 * <p>
 * When this event is fired, the route's component is initialized in memory but not attached to the
 * DOM yet. The {@link #veto()} method should be called to allow proceeding or to veto the attempt
 * to attach the route's component to the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see DidEnterEvent
 * @see WillLeaveEvent
 */
public class WillEnterEvent extends RouteEvent {

  private final transient Consumer<Boolean> allowEnter;

  /**
   * Creates a new {@code WillEnterEvent} instance with the given {@code NavigationContext} instance
   * and the callback consumer to signal whether the router should be allowed to proceed to the next
   * step.
   *
   * @param context the navigation context
   * @param allowEnter the callback consumer to signal whether the router should be allowed to
   *        proceed to the next step
   */
  public WillEnterEvent(NavigationContext context, Consumer<Boolean> allowEnter) {
    super(context);
    this.allowEnter = allowEnter;
  }

  /**
   * Signals whether the router should be allowed to proceed to the next step.
   *
   * @param value when {@code true}, the router will be allowed to proceed to the next step and
   *        attach the route's component to the DOM; when {@code false}, the router will be vetoed
   *        and the route's component will not be attached to the DOM
   *
   * @see #accept()
   * @see #reject()
   */
  public void veto(boolean value) {
    allowEnter.accept(value);
  }

  /**
   * Signals that the router should be allowed to proceed to the next step and attach the route's
   * component to the DOM.
   *
   * @see #reject()
   */
  public void accept() {
    veto(true);
  }

  /**
   * Signals that the router attempt to attach the route's component to the DOM should be vetoed.
   * The route's component will not be attached to the DOM.
   *
   * @see #accept()
   */
  public void reject() {
    veto(false);
  }
}
