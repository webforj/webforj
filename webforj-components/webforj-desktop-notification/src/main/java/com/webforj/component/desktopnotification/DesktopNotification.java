package com.webforj.component.desktopnotification;

import com.webforj.Page;
import com.webforj.annotation.Experimental;
import com.webforj.component.desktopnotification.event.DesktopNotificationClickEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationCloseEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationErrorEvent;
import com.webforj.component.desktopnotification.event.DesktopNotificationOpenEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.utilities.Assets;
import java.util.Objects;
import java.util.UUID;

/**
 * The Notification component is used to configure and display desktop notifications to the user.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@Experimental(since = "25.00")
public class DesktopNotification {
  private final String id = UUID.randomUUID().toString();
  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private boolean openListenerRegistered = false;
  private boolean closeListenerRegistered = false;
  private boolean errorListenerRegistered = false;
  private boolean clickListenerRegistered = false;
  private String title = "";
  private String body = "";
  private String icon = "icons://icon-32x32.png";

  /**
   * Creates a new instance of the DesktopNotification.
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   */
  public DesktopNotification(String title, String body) {
    setTitle(title);
    setBody(body);
  }

  /**
   * Creates a new instance of the DesktopNotification.
   *
   * @param body The body of the notification.
   */
  public DesktopNotification(String body) {
    setBody(body);
  }

  /**
   * Sets The title of the notification.
   *
   * @param title The title of the notification.
   * @return The component itself.
   */
  public DesktopNotification setTitle(String title) {
    Objects.requireNonNull(title, "The title can not be null");
    this.title = title;
    return this;
  }

  /**
   * Gets The title of the notification.
   *
   * @return The title of the notification.
   */
  public String getTitle() {
    return title;
  }

  /**
   * Sets The body of the notification.
   *
   * @param body The body of the notification.
   * @return The component itself.
   */
  public DesktopNotification setBody(String body) {
    Objects.requireNonNull(body, "The body can not be null");
    this.body = body;
    return this;
  }

  /**
   * Gets The body of the notification.
   *
   * @return The body of the notification.
   */
  public String getBody() {
    return body;
  }

  /**
   * Sets The icon of the notification.
   *
   * @param icon The icon of the notification.
   * @return The component itself.
   */
  public DesktopNotification setIcon(String icon) {
    Objects.requireNonNull(icon, "The icon can not be null");
    this.icon = icon;
    return this;
  }

  /**
   * Gets The icon of the notification.
   *
   * @return The icon of the notification.
   */
  public String getIcon() {
    return icon;
  }

  /**
   * Opens the notification.
   *
   * @return The component itself.
   */
  public DesktopNotification open() {
    // @formatter:off
    String scriptTemplate = """
      var __isMobileOrTablet__ = /Mobi|Android|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i
                                .test(navigator.userAgent);
      if(('Notification' in window) && !__isMobileOrTablet__) {
        (async () => {
          const show = () => {
            const n = new Notification('%s', {body: `%s`,icon: `%s`, tag: '%4$s'});
            n.onshow = () => window.dispatchEvent(new CustomEvent('%4$s-opened'));
            n.onclose = () => window.dispatchEvent(new CustomEvent('%4$s-closed'));
            n.onerror = () =>  window.dispatchEvent(new CustomEvent('%4$s-errored'));
            n.onclick = () => window.dispatchEvent(new CustomEvent('%4$s-clicked'));
            window.__desktop__notifications = window.__desktop__notifications || {};
            window.__desktop__notifications['%4$s'] = n;
          };
          let granted = false;
          if (Notification.permission === 'granted') {
            granted = true;
          } else if (Notification.permission !== 'denied') {
            let permission = await Notification.requestPermission();
            granted = permission === 'granted' ? true : false;
          }
          if(granted) show();
        })()
      }""";
    // @formatter:on

    Page.ifPresent(page -> {
      String script = String.format(scriptTemplate, getTitle(), getBody(),
          Assets.resolveImageSource(getIcon()), id);
      page.executeJsVoidAsync(script);
    });

    return this;
  }


  /**
   * Closes the notification.
   *
   * @return The component itself.
   */
  public DesktopNotification close() {
    // @formatter:off
    String scriptTemplate = """
    if(window.__desktop__notifications && window.__desktop__notifications['%1$s']) {
      window.__desktop__notifications['%1$s'].close();
    }""";
    // @formatter:on

    Page.ifPresent(page -> {
      String script = String.format(scriptTemplate, id);
      page.executeJsVoidAsync(script);
    });

    return this;
  }

  /**
   * Shows a notification with the given title and body.
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   * @param icon The icon of the notification.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String title, String body, String icon) {
    return new DesktopNotification(title, body).setIcon(icon).open();
  }

  /**
   * Shows a notification with the given title and body.
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String title, String body) {
    return new DesktopNotification(title, body).open();
  }

  /**
   * Shows a notification with the given body.
   *
   * @param body The body of the notification.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String body) {
    return new DesktopNotification(body).open();
  }

  /**
   * Adds a listener for the notification open event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationOpenEvent> addOpenListener(
      EventListener<DesktopNotificationOpenEvent> listener) {

    if (!openListenerRegistered) {
      Page.ifPresent(page -> {
        page.addEventListener(id + "-opened", event -> { // NOSONAR
          eventDispatcher.dispatchEvent(new DesktopNotificationOpenEvent(this));
        });
      });
      openListenerRegistered = true;
    }

    return eventDispatcher.addListener(DesktopNotificationOpenEvent.class, listener);
  }

  /**
   * Alias for {@link #addOpenListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationOpenEvent> onOpen(
      EventListener<DesktopNotificationOpenEvent> listener) {
    return addOpenListener(listener);
  }

  /**
   * Adds a listener for the notification close event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationCloseEvent> addCloseListener(
      EventListener<DesktopNotificationCloseEvent> listener) {

    if (!closeListenerRegistered) {
      Page.ifPresent(page -> {
        page.addEventListener(id + "-closed", event -> { // NOSONAR
          eventDispatcher.dispatchEvent(new DesktopNotificationCloseEvent(this));
        });
      });
      closeListenerRegistered = true;
    }

    return eventDispatcher.addListener(DesktopNotificationCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationCloseEvent> onClose(
      EventListener<DesktopNotificationCloseEvent> listener) {
    return addCloseListener(listener);
  }

  /**
   * Adds a listener for the notification error event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationErrorEvent> addErrorListener(
      EventListener<DesktopNotificationErrorEvent> listener) {

    if (!errorListenerRegistered) {
      Page.ifPresent(page -> {
        page.addEventListener(id + "-errored", event -> { // NOSONAR
          eventDispatcher.dispatchEvent(new DesktopNotificationErrorEvent(this));
        });
      });
      errorListenerRegistered = true;
    }

    return eventDispatcher.addListener(DesktopNotificationErrorEvent.class, listener);
  }

  /**
   * Alias for {@link #addErrorListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationErrorEvent> onError(
      EventListener<DesktopNotificationErrorEvent> listener) {
    return addErrorListener(listener);
  }

  /**
   * Adds a listener for the notification click event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationClickEvent> addClickListener(
      EventListener<DesktopNotificationClickEvent> listener) {

    if (!clickListenerRegistered) {
      Page.ifPresent(page -> {
        page.addEventListener(id + "-clicked", event -> { // NOSONAR
          eventDispatcher.dispatchEvent(new DesktopNotificationClickEvent(this));
        });
      });
      clickListenerRegistered = true;
    }

    return eventDispatcher.addListener(DesktopNotificationClickEvent.class, listener);
  }

  /**
   * Alias for {@link #addClickListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationClickEvent> onClick(
      EventListener<DesktopNotificationClickEvent> listener) {
    return addClickListener(listener);
  }
}
