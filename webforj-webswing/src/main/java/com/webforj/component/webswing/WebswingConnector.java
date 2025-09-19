package com.webforj.component.webswing;

import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.webswing.event.WebswingActionEvent;
import com.webforj.component.webswing.event.WebswingInitializeEvent;
import com.webforj.component.webswing.event.WebswingStartEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Objects;
import java.util.Optional;

/**
 * The WebswingConnector component provides integration with Webswing server, enabling remote Swing
 * application rendering in the browser.
 *
 * <p>
 * This component wraps the {@code dwc-webswing-connector} web component and provides a Java API for
 * controlling Webswing instances.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@NodeName("dwc-webswing-connector")
public class WebswingConnector extends ElementComposite implements HasSize<WebswingConnector>,
    HasStyle<WebswingConnector>, HasClassName<WebswingConnector> {
  private PropertyDescriptor<String> urlProp = PropertyDescriptor.property("url", "");
  private PropertyDescriptor<WebswingOptions> optionsProp =
      PropertyDescriptor.property("options", null);

  /**
   * Creates a new instance of the WebswingConnector.
   *
   * @param url the URL endpoint of the Webswing server to connect to
   * @param autoStart whether to start the Webswing instance automatically
   */
  public WebswingConnector(String url, boolean autoStart) {
    super();
    setUrl(url);
    setOptions(new WebswingOptions().setAutoStart(autoStart));
  }

  /**
   * Creates a new instance of the WebswingConnector with auto-start enabled.
   *
   * @param url the URL endpoint of the Webswing server to connect to
   */
  public WebswingConnector(String url) {
    this(url, true);
  }

  /**
   * Creates a new instance of the WebswingConnector.
   */
  public WebswingConnector() {
    super();
  }

  /**
   * Sets the URL endpoint of the Webswing server to connect to.
   *
   * @param url the URL of the Webswing server
   * @return the component itself
   */
  public WebswingConnector setUrl(String url) {
    set(urlProp, url);
    return this;
  }

  /**
   * Gets the URL endpoint of the Webswing server.
   *
   * @return the URL of the Webswing server
   */
  public String getUrl() {
    return get(urlProp);
  }

  /**
   * Sets the configuration options for the Webswing instance initialization.
   *
   * @param options the configuration options
   * @return the component itself
   */
  public WebswingConnector setOptions(WebswingOptions options) {
    set(optionsProp, options);
    return this;
  }

  /**
   * Gets the configuration options for the Webswing instance.
   *
   * @return the configuration options
   */
  public WebswingOptions getOptions() {
    return get(optionsProp);
  }

  /**
   * Initiates the connection to Webswing server and starts the Swing application.
   *
   * <p>
   * This method should be called when {@code autoStart} is set to {@code false} in the options. If
   * {@code autoStart} is {@code true} which is the default, then the Webswing instance starts
   * automatically and this method should not be called.
   * </p>
   *
   * @return the component itself
   */
  public WebswingConnector start() {
    getElement().callJsFunctionVoidAsync("start");
    return this;
  }

  /**
   * Stops and disconnects the current Webswing session but leaves the Swing application running on
   * the server, allowing to reconnect to the same application instance later.
   *
   * <p>
   * If you want to destroy the Webswing instance permanently, then use {@link #destroy()} instead.
   * After calling {@link #destroy()}, the instance cannot be restarted and a new instance must be
   * created to establish a new connection.
   * </p>
   *
   * @return the component itself
   */
  public WebswingConnector stop() {
    getElement().callJsFunctionVoidAsync("stop");
    return this;
  }

  /**
   * Returns the current Webswing instance ID.
   *
   * <p>
   * The instance ID uniquely identifies this connection to the Webswing server.
   * </p>
   *
   * @return an Optional containing the instance ID, or empty if no instance is initialized
   */
  public Optional<String> getInstanceId() {
    var result = getElement().callJsFunction("getInstanceId");
    return Optional.ofNullable(result).map(id -> String.valueOf(id));
  }

  /**
   * Logs out the currently logged in user from the Webswing session.
   *
   * @param tabLogout if true, logs out only from the current tab/window
   * @param closeOnSuccess if true, closes the window/tab after successful logout
   *
   * @return the component itself
   */
  public WebswingConnector logout(boolean tabLogout, boolean closeOnSuccess) {
    getElement().callJsFunctionVoidAsync("logout", tabLogout, closeOnSuccess);
    return this;
  }

  /**
   * Logs out the currently logged in user from the Webswing session.
   *
   * @return the component itself
   */
  public WebswingConnector logout() {
    return logout(true, false);
  }

  /**
   * Triggers a custom action on the server-side Webswing application.
   *
   * @param actionName the identifier of the action to perform
   * @param data optional string data payload to send with the action
   * @param binaryData optional binary data to send with the action
   *
   * @return the component itself
   */
  public WebswingConnector performAction(String actionName, String data, String binaryData) {
    Objects.requireNonNull(actionName, "Action name cannot be null");
    Objects.requireNonNull(data, "Action Data cannot be null");
    Objects.requireNonNull(actionName, "Action BinaryData cannot be null");

    String encodedBinaryData = binaryData;

    if (binaryData != null) {
      encodedBinaryData =
          new String(Base64.getEncoder().encode(binaryData.getBytes(StandardCharsets.UTF_8)),
              StandardCharsets.UTF_8);
    }

    getElement().callJsFunctionVoidAsync("performAction", actionName, data, encodedBinaryData);
    return this;
  }

  /**
   * Triggers a custom action on the server-side Webswing application.
   *
   * @param actionName the identifier of the action to perform
   * @param data optional string data payload to send with the action
   *
   * @return the component itself
   */
  public WebswingConnector performAction(String actionName, String data) {
    return performAction(actionName, data, "");
  }

  /**
   * Triggers a custom action on the server-side Webswing application.
   *
   * @param actionName the identifier of the action to perform
   * @return the component itself
   */
  public WebswingConnector performAction(String actionName) {
    return performAction(actionName, "", "");
  }

  /**
   * Requests a repaint of the Webswing application.
   *
   * <p>
   * Notifies the server that the application needs to refresh its display. This can be useful after
   * window resizing or when display issues occur.
   * </p>
   *
   * @return the component itself
   */
  public WebswingConnector repaint() {
    getElement().callJsFunctionAsync("repaint");
    return this;
  }

  /**
   * Adds a listener for the {@link WebswingInitializeEvent}.
   *
   * <p>
   * This event is fired when the Webswing instance has been successfully initialized and is ready
   * to receive commands.
   * </p>
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingInitializeEvent> addInitializeListener(
      EventListener<WebswingInitializeEvent> listener) {
    return addEventListener(WebswingInitializeEvent.class, listener);
  }

  /**
   * Alias for {@link #addInitializeListener(EventListener)}.
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingInitializeEvent> onInitialize(
      EventListener<WebswingInitializeEvent> listener) {
    return addInitializeListener(listener);
  }

  /**
   * Adds a listener for the {@link WebswingStartEvent}.
   *
   * <p>
   * This event is fired when the Webswing application has been started and is actively running.
   * </p>
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingStartEvent> addStartListener(
      EventListener<WebswingStartEvent> listener) {
    return addEventListener(WebswingStartEvent.class, listener);
  }

  /**
   * Alias for {@link #addStartListener(EventListener)}.
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingStartEvent> onStart(
      EventListener<WebswingStartEvent> listener) {
    return addStartListener(listener);
  }

  /**
   * Adds a listener for the {@link WebswingActionEvent}.
   *
   * <p>
   * This event is fired when a custom action is triggered from the Webswing application.
   * </p>
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingActionEvent> addActionListener(
      EventListener<WebswingActionEvent> listener) {
    return addEventListener(WebswingActionEvent.class, listener);
  }

  /**
   * Alias for {@link #addActionListener(EventListener)}.
   *
   * @param listener the event listener
   * @return a registration object for removing the event listener
   */
  public ListenerRegistration<WebswingActionEvent> onAction(
      EventListener<WebswingActionEvent> listener) {
    return addActionListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    // must block the call before call destroy on super
    getElement().callJsFunction("destroy");
    super.onDestroy();
  }

  /**
   * Gets the underlying {@link Element} instance.
   *
   * @return the underlying Element
   */
  @Override
  public Element getElement() {
    return super.getElement();
  }
}
