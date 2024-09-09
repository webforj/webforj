package com.webforj.component.login;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.login.event.LoginCancelEvent;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasStyle;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * The Login component provides a form for user authentication using a username and password. it
 * supports internationalization, and is responsive across various device sizes.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
@NodeName("dwc-login")
public class Login extends ElementCompositeContainer
    implements HasClassName<Login>, HasStyle<Login>, HasEnablement<Login> {

  /**
   * An enum for indicating whether the user will be required to login for every visit to a client
   * app.
   */
  public enum PasswordMediation {
    /*
     * The user will not be asked to authenticate. The user agent will automatically reauthenticate
     * the user and log them in if possible.
     */
    @SerializedName("silent")
    SILIENT,

    /* The user will always be asked to authenticate, */
    @SerializedName("required")
    REQUIRED,

    /*
     * If credentials can be handed over without user mediation, they will be. It is the default
     * option.
     */
    @SerializedName("optional")
    OPTIONAL;
  }

  // Slots
  private static final String BEFORE_HEADER_SLOT = "before-header";
  private static final String AFTER_HEADER_SLOT = "after-header";
  private static final String BEFORE_CONTENT_SLOT = "before-content";
  private static final String AFTER_CONTENT_SLOT = "after-content";
  private static final String BEFORE_FORM_SLOT = "before-form";
  private static final String AFTER_FORM_SLOT = "after-form";
  private static final String BEFORE_FOOTER_SLOT = "before-footer";
  private static final String AFTER_FOOTER_SLOT = "after-footer";

  // Properties
  private final PropertyDescriptor<Boolean> autoClose =
      PropertyDescriptor.property("autoClose", true);
  private final PropertyDescriptor<Boolean> autoSignin =
      PropertyDescriptor.property("autoSignin", false);
  private final PropertyDescriptor<Boolean> disabled =
      PropertyDescriptor.property("disabled", false);
  private final PropertyDescriptor<Boolean> emptyPassword =
      PropertyDescriptor.property("emptyPassword", false);
  private final PropertyDescriptor<Boolean> error = PropertyDescriptor.property("error", false);
  private final PropertyDescriptor<LoginI18n> i18n =
      PropertyDescriptor.property("i18n", new LoginI18n());
  private final PropertyDescriptor<String> maxWidth =
      PropertyDescriptor.property("maxWidth", "400px");
  private final PropertyDescriptor<String> maxHeight = PropertyDescriptor.property("maxHeight", "");
  private final PropertyDescriptor<Boolean> opened = PropertyDescriptor.property("opened", false);
  private final PropertyDescriptor<String> password = PropertyDescriptor.property("password", "");
  private final PropertyDescriptor<PasswordMediation> passwordMediation =
      PropertyDescriptor.property("passwordMediation", PasswordMediation.OPTIONAL);
  private final PropertyDescriptor<Boolean> rememberme =
      PropertyDescriptor.property("rememberme", true);
  private final PropertyDescriptor<String> username = PropertyDescriptor.property("username", "");

  /**
   * Instantiates a new Login.
   */
  public Login() {
    setAutoClose(false);
    setI18n(getI18n());
    // Make sure to append the login dialog to the end of the body to avoid
    // any stacking issues.
    // This call should be made very early in the lifecycle of the component
    // before any events are added because moving the element will remove all
    // the event listeners
    getElement().whenDefined().thenAccept(c -> {
      getElement().executeJsVoidAsync("document.body.appendChild(component)");
    });
  }

  /**
   * Adds given components to the before header slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToBeforeHeader(Component... component) {
    getElement().add(BEFORE_HEADER_SLOT, component);
    return this;
  }

  /**
   * Adds given components to the after header slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToAfterHeader(Component... component) {
    getElement().add(AFTER_HEADER_SLOT, component);
    return this;
  }

  /**
   * Adds given components to the before content slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToBeforeContent(Component... component) {
    getElement().add(BEFORE_CONTENT_SLOT, component);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void add(Component... components) {
    getElement().add(AFTER_CONTENT_SLOT, components);
  }

  /**
   * Alias for {@link #add(Component)}.
   *
   * <p>
   * Adds the given components as children of this component in the after content slot.
   * </p>
   *
   * @param component the component to add
   * @return the component itself
   */
  public Login addToAfterContent(Component... component) {
    add(component);
    return this;
  }

  /**
   * Adds given components to the before form slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToBeforeForm(Component... component) {
    getElement().add(BEFORE_FORM_SLOT, component);
    return this;
  }

  /**
   * Adds given components to the after form slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToAfterForm(Component... component) {
    getElement().add(AFTER_FORM_SLOT, component);
    return this;
  }

  /**
   * Adds given components to the before footer slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToBeforeFooter(Component... component) {
    getElement().add(BEFORE_FOOTER_SLOT, component);
    return this;
  }

  /**
   * Adds given components to the after footer slot.
   *
   * @param component the components to add
   * @return the component itself
   */
  public Login addToAfterFooter(Component... component) {
    getElement().add(AFTER_FOOTER_SLOT, component);
    return this;
  }

  /**
   * Sets the auto close property.
   *
   * <p>
   * When true, the dialog will be closed after the form is submitted.
   * </p>
   *
   * @param autoClose the auto close property
   * @return the component itself
   */
  public Login setAutoClose(boolean autoClose) {
    set(this.autoClose, autoClose);
    return this;
  }

  /**
   * Checks whether the auto close property is set.
   *
   * @return true if the auto close property is set, false otherwise
   */
  public boolean isAutoClose() {
    return get(autoClose);
  }

  /**
   * Enables or disables auto signin.
   *
   * <p>
   * When true, the dialog will automatically sign in the user if the user has previously signed in
   * and the user agent can provide credentials without user mediation. This feature requires
   * <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/API/PasswordCredential">PasswordCredential</a>
   * support.
   * </p>
   *
   * @param autoSignin true to enable auto signin, false otherwise
   * @return the component itself
   */
  public Login setAutoSignin(boolean autoSignin) {
    set(this.autoSignin, autoSignin);
    return this;
  }

  /**
   * Checks whether the auto signin is enabled.
   *
   * @return true if auto signin is enabled, false otherwise
   */
  public boolean isAutoSignin() {
    return get(autoSignin);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isEnabled() {
    return !get(disabled, true, Boolean.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Login setEnabled(boolean enabled) {
    set(disabled, !enabled);
    return this;
  }

  /**
   * Enables or disables the empty password acceptance.
   *
   * @param emptyPassword true to enable empty password acceptance, false otherwise to disable it
   * @return the component itself
   */
  public Login setEmptyPassword(boolean emptyPassword) {
    set(this.emptyPassword, emptyPassword);
    return this;
  }

  /**
   * Checks whether empty password is accepted.
   *
   * @return true if empty password is accepted, false otherwise
   */
  public boolean isEmptyPassword() {
    return get(emptyPassword);
  }

  /**
   * Shows the login error message.
   *
   * <p>
   * When true, the dialog will show the error message.
   * </p>
   *
   * @param error true to show the error message, false otherwise
   * @return the component itself
   */
  public Login setError(boolean error) {
    set(this.error, error);
    return this;
  }

  /**
   * Checks whether the error section is shown.
   *
   * @return true if the error property is set, false otherwise
   */
  public boolean isError() {
    return get(error);
  }

  /**
   * Sets the i18n object.
   *
   * @param i18n the i18n object
   * @return the component itself
   */
  public Login setI18n(LoginI18n i18n) {
    set(this.i18n, i18n);
    return this;
  }

  /**
   * Gets the used i18n object.
   *
   * @return the i18n object
   */
  public LoginI18n getI18n() {
    return get(i18n);
  }

  /**
   * Sets the dialog max width.
   *
   * @param maxWidth the max width for the dialog
   * @return the component itself
   */
  public Login setMaxWidth(String maxWidth) {
    set(this.maxWidth, maxWidth);
    return this;
  }

  /**
   * Gets the dialog max width.
   *
   * @return the max width property
   */
  public String getMaxWidth() {
    return get(maxWidth);
  }

  /**
   * Sets the dialog max height.
   *
   * @param maxHeight the max height for the dialog
   * @return the component itself
   */
  public Login setMaxHeight(String maxHeight) {
    set(this.maxHeight, maxHeight);
    return this;
  }

  /**
   * Gets the dialog max height.
   *
   * @return the max height for the dialog
   */
  public String getMaxHeight() {
    return get(maxHeight);
  }

  /**
   * Opens the dialog.
   *
   * @return the component itself
   */
  public Login open() {
    set(this.opened, true);
    return this;
  }

  /**
   * Closes the dialog.
   *
   * @return the component itself
   */
  public Login close() {
    set(this.opened, false);
    return this;
  }

  /**
   * Checks whether the dialog is opened.
   *
   * @return true if the opened property is set, false otherwise
   */
  public boolean isOpened() {
    return get(opened, true, Boolean.class);
  }

  /**
   * Sets the default password.
   *
   * <p>
   * The password of the user to be used as a default value.
   * </p>
   *
   * <p>
   * <b>Note:</b> If you set this property, the password will be visible in DOM. Make sure to
   * destroy the component after login to avoid security issues.
   * </p>
   *
   * @param password the password property
   * @return the component itself
   */
  public Login setPassword(String password) {
    set(this.password, password);
    return this;
  }

  /**
   * Gets the default password.
   *
   * @return the default password
   * @see #setPassword(String)
   */
  public String getPassword() {
    return get(password);
  }

  /**
   * Sets the password mediation type.
   *
   * <p>
   * This feature requires <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/API/PasswordCredential">PasswordCredential</a>
   * support.
   * </p>
   *
   * @param passwordMediation the password mediation property
   * @return the component itself
   */
  public Login setPasswordMediation(PasswordMediation passwordMediation) {
    set(this.passwordMediation, passwordMediation);
    return this;
  }

  /**
   * Gets the password mediation type.
   *
   * @return the password mediation type
   * @see #setPasswordMediation(PasswordMediation)
   */
  public PasswordMediation getPasswordMediation() {
    return get(passwordMediation);
  }

  /**
   * Check or uncheck the rememberme checkbox.
   *
   * <p>
   * When true, the component will save the username in the browser's local storage. and will be
   * used as a default value for the next login.
   * </p>
   *
   * @param rememberme true to check the rememberme checkbox, false to uncheck it
   * @return the component itself
   */
  public Login setRememberme(boolean rememberme) {
    set(this.rememberme, rememberme);
    return this;
  }

  /**
   * Checks whether the rememberme checkbox is checked.
   *
   * @return true if the rememberme checkbox is checked, false otherwise
   */
  public boolean isRememberme() {
    return get(rememberme);
  }

  /**
   * Sets the default username.
   *
   * @param username the default username
   * @return the component itself
   */
  public Login setUsername(String username) {
    set(this.username, username);
    return this;
  }

  /**
   * Gets the default username.
   *
   * @return the default username
   */
  public String getUsername() {
    return get(username);
  }

  /**
   * Adds a listener for the submit event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<LoginSubmitEvent> addSubmitListener(
      EventListener<LoginSubmitEvent> listener) {
    return addEventListener(LoginSubmitEvent.class, listener);
  }

  /**
   * Alias for {@link #addSubmitListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<LoginSubmitEvent> onSubmit(EventListener<LoginSubmitEvent> listener) {
    return addSubmitListener(listener);
  }

  /**
   * Adds a listener for the cancel event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<LoginCancelEvent> addCancelListener(
      EventListener<LoginCancelEvent> listener) {
    return addEventListener(LoginCancelEvent.class, listener);
  }

  /**
   * Alias for {@link #addCancelListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<LoginCancelEvent> onCancel(EventListener<LoginCancelEvent> listener) {
    return addCancelListener(listener);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    close();
    super.onDestroy();
  }

  Element getOriginalElement() {
    return getElement();
  }
}
