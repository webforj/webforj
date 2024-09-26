package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.event.WillEnterEvent;
import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.observer.DidEnterObserver;
import com.webforj.router.observer.DidLeaveObserver;
import com.webforj.router.observer.RouteRendererObserver;
import com.webforj.router.observer.WillEnterObserver;
import com.webforj.router.observer.WillLeaveObserver;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * The {@code RouteRendererDispatcher} class is responsible for dispatching events to the
 * appropriate observers when lifecycle events occur in the routing and rendering process.
 *
 * <p>
 * This class implements the {@link RouteRendererObserver} interface and acts as a centralized
 * dispatcher for events such as {@link WillEnterEvent}, {@link DidEnterEvent},
 * {@link WillLeaveEvent}, and {@link DidLeaveEvent}. It ensures that these events are correctly
 * fired and handled, including invoking any relevant observers that have registered for these
 * events.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class RouteRendererDispatcher implements RouteRendererObserver {
  private final EventDispatcher eventDispatcher;

  /**
   * Constructs a new {@code RouteRendererDispatcher} with the specified {@link EventDispatcher}.
   *
   * @param eventDispatcher the event dispatcher used to fire events
   */
  public RouteRendererDispatcher(EventDispatcher eventDispatcher) {
    this.eventDispatcher = eventDispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> cb) {

    switch (event) {
      case BEFORE_CREATE:
        fireWillEnterEvent(component, context, cb);
        break;
      case AFTER_CREATE:
        fireDidEnterEvent(component, context);
        cb.accept(true);
        break;
      case BEFORE_DESTROY:
        fireWillLeaveEvent(component, context, cb);
        break;
      case AFTER_DESTROY:
        fireDidLeaveEvent(component, context);
        cb.accept(true);
        break;
      default:
        cb.accept(true);
        break;
    }
  }

  /**
   * Fires the {@link WillEnterEvent} for the specified component.
   *
   * <p>
   * Notifies any registered {@link WillEnterObserver} and dispatches the event through the event
   * dispatcher if the {@link NavigationOptions#isFireEvents()} flag is set. If the
   * {@link NavigationOptions#isInvokeObservers()} flag is set, the observer's
   * {@link WillEnterObserver#onWillEnter(WillEnterEvent, ParametersBag)} method is called.
   *
   * </p>
   *
   * @param component the component that is about to be created
   * @param context the navigation context containing the route parameters and options
   * @param cb the callback to indicate completion; should be called with {@code true} to continue
   *        or {@code false} to halt the operation
   */
  protected void fireWillEnterEvent(Component component, NavigationContext context,
      Consumer<Boolean> cb) {
    WillEnterEvent event = new WillEnterEvent(context, cb);
    Optional<NavigationOptions> options = context.getOptions();

    if (options.isPresent() && options.get().isFireEvents()) {
      eventDispatcher.dispatchEvent(event);
    }

    if (options.isPresent() && options.get().isInvokeObservers()
        && component instanceof WillEnterObserver observer) {
      observer.onWillEnter(event, context.getRouteParameters());
    } else {
      cb.accept(true);
    }
  }

  /**
   * Fires the {@link DidEnterEvent} for the specified component.
   *
   * <p>
   * Notifies any registered {@link DidEnterObserver} and dispatches the event through the event
   * dispatcher if the {@link NavigationOptions#isFireEvents()} flag is set. If the
   * {@link NavigationOptions#isInvokeObservers()} flag is set, the observer's
   * {@link DidEnterObserver#onDidEnter(DidEnterEvent, ParametersBag)} method is called.
   * </p>
   *
   * @param component the component that has just been created
   * @param context the navigation context containing the route parameters and options
   */
  protected void fireDidEnterEvent(Component component, NavigationContext context) {
    DidEnterEvent event = new DidEnterEvent(context);
    Optional<NavigationOptions> options = context.getOptions();

    if (options.isPresent() && options.get().isFireEvents()) {
      eventDispatcher.dispatchEvent(event);
    }

    if (options.isPresent() && options.get().isInvokeObservers()
        && component instanceof DidEnterObserver observer) {
      observer.onDidEnter(event, context.getRouteParameters());
    }
  }

  /**
   * Fires the {@link WillLeaveEvent} for the specified component.
   *
   * <p>
   * Notifies any registered {@link WillLeaveObserver} and dispatches the event through the event
   * dispatcher if the {@link NavigationOptions#isFireEvents()} flag is set. If the
   * {@link NavigationOptions#isInvokeObservers()} flag is set, the observer's
   * {@link WillLeaveObserver#onWillLeave(WillLeaveEvent, ParametersBag)} method is called.
   * </p>
   *
   * @param component the component that is about to be destroyed
   * @param context the navigation context containing the route parameters and options
   * @param cb the callback to indicate completion; should be called with {@code true} to continue
   *        or {@code false} to halt the operation
   */
  protected void fireWillLeaveEvent(Component component, NavigationContext context,
      Consumer<Boolean> cb) {
    WillLeaveEvent event = new WillLeaveEvent(context, cb);
    Optional<NavigationOptions> options = context.getOptions();

    if (options.isPresent() && options.get().isFireEvents()) {
      eventDispatcher.dispatchEvent(event);
    }

    if (options.isPresent() && options.get().isInvokeObservers()
        && component instanceof WillLeaveObserver observer) {
      observer.onWillLeave(event, context.getRouteParameters());
    } else {
      cb.accept(true);
    }
  }

  /**
   * Fires the {@link DidLeaveEvent} for the specified component.
   *
   * <p>
   * Notifies any registered {@link DidLeaveObserver} and dispatches the event through the event
   * dispatcher if the {@link NavigationOptions#isFireEvents()} flag is set. If the
   * {@link NavigationOptions#isInvokeObservers()} flag is set, the observer's
   * {@link DidLeaveObserver#onDidLeave(DidLeaveEvent, ParametersBag)} method is called.
   * </p>
   *
   * @param component the component that has just been destroyed
   * @param context the navigation context containing the route parameters and options
   */
  protected void fireDidLeaveEvent(Component component, NavigationContext context) {
    DidLeaveEvent event = new DidLeaveEvent(context);
    Optional<NavigationOptions> options = context.getOptions();

    if (options.isPresent() && options.get().isFireEvents()) {
      eventDispatcher.dispatchEvent(event);
    }

    if (options.isPresent() && options.get().isInvokeObservers()
        && component instanceof DidLeaveObserver observer) {
      observer.onDidLeave(event, context.getRouteParameters());
    }
  }
}
