package com.webforj.component.desktopnotification;

import com.basis.bbj.iris.bdt.gbf.profile.Event;
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
 * The {@code Notification} component enables applications to configure and display native desktop
 * notifications to users.
 *
 * <p>
 * Desktop notifications allow an application to alert users about new messages or updates while the
 * browser is open. Once permission is granted, the browser can display notifications on the user's
 * screen. However, notifications are only shown after the user interacts with the application—such
 * as by pressing a key or clicking/tapping within the app. Without user interaction, only a
 * notification icon appears in the browser’s address bar, and no visible notification is displayed.
 * </p>
 *
 * <p>
 * To successfully display desktop notifications, the following conditions must be met:
 * <ul>
 * <li>The application must run in a secure context (e.g., over HTTPS).</li>
 * <li>The application must not be in incognito/private browsing mode.</li>
 * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
 * input.</li>
 * <li>The user must have granted permission to display notifications.</li>
 * </ul>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@Experimental(since = "25.00")
public class DesktopNotification {
  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private final String id = UUID.randomUUID().toString();
  private boolean openListenerRegistered = false;
  private boolean closeListenerRegistered = false;
  private boolean errorListenerRegistered = false;
  private boolean clickListenerRegistered = false;
  private String title = "";
  private String body = "";
  private String icon = "icons://icon-32x32.png";

  /**
   * Creates a new instance of the DesktopNotification with the given title and body and error
   * listener.
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   * @param errorListener The error listener.
   */
  public DesktopNotification(String title, String body,
      EventListener<DesktopNotificationErrorEvent> errorListener) {
    setTitle(title);
    setBody(body);
    if (errorListener != null) {
      addErrorListener(errorListener);
    }
  }

  /**
   * Creates a new instance of the DesktopNotification with the given title and body.
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   */
  public DesktopNotification(String title, String body) {
    this(title, body, null);
  }

  /**
   * Creates a new instance of the DesktopNotification with the given body and error listener.
   *
   * @param body The body of the notification.
   * @param errorListener The error listener.
   */
  public DesktopNotification(String body,
      EventListener<DesktopNotificationErrorEvent> errorListener) {
    this("", body, errorListener);
  }

  /**
   * Creates a new instance of the DesktopNotification.
   *
   * @param body The body of the notification.
   */
  public DesktopNotification(String body) {
    this("", body, null);
  }

  /**
   * Sets The title of the notification.
   *
   * @param title The title of the notification.
   * @return The component itself.
   */
  public DesktopNotification setTitle(String title) {
    Objects.requireNonNull(title, "The notification's title can not be null");
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
    Objects.requireNonNull(body, "The notification's body can not be null");
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
   * <p>
   * <b> This feature will not work on all browsers. For example, it will not work on Safari but
   * will work on Chrome and Firefox. </b>
   * </p>
   *
   * @param src The URL of the image. If a URL is provided and begins with {@code context://}, it
   *        will be resolved as a context URL, pointing to the root of your application's resources
   *        folder, and the image URL will be a base64-encoded string of the image. If a URL is
   *        provided and starts with {@code ws://}, it will be resolved as a web server URL,
   *        pointing to the root of the web server, and the image URL will be a fully qualified URL.
   *        if a URL is provided and starts with {@code icons://}, it will be resolved as an icons
   *        URL.
   *
   * @return The component itself.
   */
  public DesktopNotification setIcon(String src) {
    Objects.requireNonNull(src, "The icon can not be null");
    this.icon = src;
    return this;
  }

  /**
   * Gets The icon of the notification.
   *
   * @return The icon of the notification.
   * @see #setIcon(String)
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
          else window.dispatchEvent(new CustomEvent('%4$s-errored'));
        })()
      }
        """;
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
      }
        """;
    // @formatter:on

    Page.ifPresent(page -> {
      String script = String.format(scriptTemplate, id);
      page.executeJsVoidAsync(script);
    });

    return this;
  }

  /**
   * Shows a notification with the given title and body and icon and error listener.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
   *
   * <p>
   * Note that icons are not supported in all browsers. For example, it will not work on Safari but
   * will work on Chrome and Firefox.
   * </p>
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   * @param icon The icon of the notification.
   * @param errorListener The error listener.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String title, String body, String icon,
      EventListener<DesktopNotificationErrorEvent> errorListener) {
    return new DesktopNotification(title, body, errorListener).setIcon(icon).open();
  }

  /**
   * Shows a notification with the given title and body and icon.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
   *
   * <p>
   * Note that icons are not supported in all browsers. For example, it will not work on Safari but
   * will work on Chrome and Firefox.
   * </p>
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
   * Shows a notification with the given title and body and error listener.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
   *
   * @param title The title of the notification.
   * @param body The body of the notification.
   * @param errorListener The error listener.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String title, String body,
      EventListener<DesktopNotificationErrorEvent> errorListener) {
    return new DesktopNotification(title, body, errorListener).open();
  }

  /**
   * Shows a notification with the given title and body.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
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
   * Shows a notification with the given body and error listener.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
   *
   * @param body The body of the notification.
   * @param errorListener The error listener.
   *
   * @return The created notification.
   */
  public static DesktopNotification show(String body,
      EventListener<DesktopNotificationErrorEvent> errorListener) {
    return new DesktopNotification(body, errorListener).open();
  }

  /**
   * Shows a notification with the given body.
   *
   * <p>
   * Desktop notifications allow an application to alert users about new messages or updates while
   * the browser is open. Once permission is granted, the browser can display notifications on the
   * user's screen. However, notifications are only shown after the user interacts with the
   * application—such as by pressing a key or clicking/tapping within the app. Without user
   * interaction, only a notification icon appears in the browser’s address bar, and no visible
   * notification is displayed.
   * </p>
   *
   * <p>
   * To successfully display desktop notifications, the following conditions must be met:
   * <ul>
   * <li>The application must run in a secure context (e.g., over HTTPS).</li>
   * <li>The application must not be in incognito/private browsing mode.</li>
   * <li>The notification must be triggered by a user gesture, such as a button click or keyboard
   * input.</li>
   * <li>The user must have granted permission to display notifications.</li>
   * </ul>
   * </p>
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
      Page.ifPresent(page -> page.addEventListener(id + "-opened",
          event -> eventDispatcher.dispatchEvent(new DesktopNotificationOpenEvent(this))));
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
      Page.ifPresent(page -> page.addEventListener(id + "-closed",
          event -> eventDispatcher.dispatchEvent(new DesktopNotificationCloseEvent(this))));
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
      Page.ifPresent(page -> page.addEventListener(id + "-errored",
          event -> eventDispatcher.dispatchEvent(new DesktopNotificationErrorEvent(this))));
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
   * <p>
   * The listener is triggered when the user clicks on a desktop notification created by this API.
   * <strong>Note:</strong> the click event is used solely for notification purposes and does not
   * automatically bring the application tab or window into focus. Due to browser security and UX
   * policies, the focus behavior must be handled separately by the application and cannot be
   * guaranteed by this event.
   * </p>
   *
   * <p>
   * Limitations include:
   * <ul>
   * <li>The click event is dispatched only when a user actively clicks the notification.</li>
   * <li>Browsers do not permit forcing focus to the app tab solely from a notification click.</li>
   * <li>Any logic to refocus the app must be implemented by the application, keeping in mind that
   * user gestures or additional browser-specific allowances are required.</li>
   * </ul>
   * </p>
   *
   * @param listener the listener to be notified when the notification is clicked
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<DesktopNotificationClickEvent> addClickListener(
      EventListener<DesktopNotificationClickEvent> listener) {

    if (!clickListenerRegistered) {
      Page.ifPresent(page -> page.addEventListener(id + "-clicked",
          event -> eventDispatcher.dispatchEvent(new DesktopNotificationClickEvent(this))));
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

  EventDispatcher getEventDispatcher() {
    return eventDispatcher;
  }
}
