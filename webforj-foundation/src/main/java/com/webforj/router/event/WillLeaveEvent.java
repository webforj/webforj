package com.webforj.router.event;

import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.function.Consumer;

/**
 * {@code WillLeaveEvent} is an event object which is fired before the router attempts to leave a
 * route and detach its component from the DOM.
 *
 * <p>
 * When this event is fired, the route's component is still attached to the DOM. The {@link #veto()}
 * method should be called to allow proceeding or to veto the attempt to detach the route's
 * component from the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see DidLeaveEvent
 * @see WillEnterEvent
 */
public class WillLeaveEvent extends RouteEvent {

  private final transient Consumer<Boolean> allowLeave;

  /**
   * Creates a new {@code WillLeaveEvent} instance with the given {@code Router}, {@code Location},
   * {@code ParametersBag}, and {@code Consumer<Boolean>}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   * @param allowLeave the callback consumer to signal whether the router should be allowed to
   *        proceed to the next step
   */
  public WillLeaveEvent(Router router, Location location, ParametersBag parameters,
      Consumer<Boolean> allowLeave) {
    super(router, location, parameters);
    this.allowLeave = allowLeave;
  }

  /**
   * Signals whether the router should be allowed to proceed to the next step.
   *
   * @param value when {@code true}, the router will be allowed to proceed to the next step and
   *        detach the route's component from the DOM; when {@code false}, the router will be vetoed
   *        and the route's component will not be detached from the DOM
   *
   * @see #accept()
   * @see #reject()
   */
  public void veto(boolean value) {
    allowLeave.accept(value);
  }

  /**
   * Signals that the router should be allowed to proceed to the next step and detach the route's
   * component from the DOM.
   *
   * @see #reject()
   */
  public void accept() {
    veto(true);
  }

  /**
   * Signals that the router attempt to detach the route's component from the DOM should be vetoed.
   * The route's component will not be detached from the DOM.
   *
   * @see #accept()
   */
  public void reject() {
    veto(false);
  }
}
