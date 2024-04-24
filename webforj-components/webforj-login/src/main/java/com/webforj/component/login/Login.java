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
      getElement().executeJsAsync("document.body.appendChild(component)");
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
   * Sets the auto signin property.
   *
   * <p>
   * When true, the dialog will automatically sign in the user if the user has previously signed in
   * and the user agent can provide credentials without user mediation.
   * </p>
   *
   * @param autoSignin the auto signin property
   * @return the component itself
   */
  public Login setAutoSignin(boolean autoSignin) {
    set(this.autoSignin, autoSignin);
    return this;
  }

  /**
   * Checks whether the auto signin property is set.
   *
   * @return true if the auto signin property is set, false otherwise
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
   * Sets the empty password property.
   *
   * <p>
   * When true, the password field will be empty.
   * </p>
   *
   * @param emptyPassword the empty password property
   * @return the component itself
   */
  public Login setEmptyPassword(boolean emptyPassword) {
    set(this.emptyPassword, emptyPassword);
    return this;
  }

  /**
   * Checks whether the empty password property is set.
   *
   * @return true if the empty password property is set, false otherwise
   */
  public boolean isEmptyPassword() {
    return get(emptyPassword);
  }

  /**
   * Sets the i18n property.
   *
   * <p>
   * The i18n property.
   * </p>
   *
   * @param i18n the i18n property
   * @return the component itself
   */
  public Login setI18n(LoginI18n i18n) {
    set(this.i18n, i18n);
    return this;
  }

  /**
   * Gets the i18n property.
   *
   * @return the i18n property
   */
  public LoginI18n getI18n() {
    return get(i18n);
  }

  /**
   * Sets the max width property.
   *
   * <p>
   * The maximum width of the dialog.
   * </p>
   *
   * @param maxWidth the max width property
   * @return the component itself
   */
  public Login setMaxWidth(String maxWidth) {
    set(this.maxWidth, maxWidth);
    return this;
  }

  /**
   * Gets the max width property.
   *
   * @return the max width property
   */
  public String getMaxWidth() {
    return get(maxWidth);
  }

  /**
   * Sets the max height property.
   *
   * <p>
   * The maximum height of the dialog.
   * </p>
   *
   * @param maxHeight the max height property
   * @return the component itself
   */
  public Login setMaxHeight(String maxHeight) {
    set(this.maxHeight, maxHeight);
    return this;
  }

  /**
   * Gets the max height property.
   *
   * @return the max height property
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
   * Checks whether the opened property is set.
   *
   * @return true if the opened property is set, false otherwise
   */
  public boolean isOpened() {
    return get(opened, true, Boolean.class);
  }

  /**
   * Sets the default password property.
   *
   * <p>
   * The password of the user to be used as a default value.
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
   * Gets the password property.
   *
   * @return the password property
   */
  public String getPassword() {
    return get(password);
  }

  /**
   * Sets the password mediation property.
   *
   * <p>
   * The password mediation property.
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
   * Gets the password mediation property.
   *
   * @return the password mediation property
   */
  public PasswordMediation getPasswordMediation() {
    return get(passwordMediation);
  }

  /**
   * Sets the rememberme property.
   *
   * <p>
   * When true, the dialog will remember the user's credentials.
   * </p>
   *
   * @param rememberme the rememberme property
   * @return the component itself
   */
  public Login setRememberme(boolean rememberme) {
    set(this.rememberme, rememberme);
    return this;
  }

  /**
   * Checks whether the rememberme property is set.
   *
   * @return true if the rememberme property is set, false otherwise
   */
  public boolean isRememberme() {
    return get(rememberme);
  }

  /**
   * Sets the default username property.
   *
   * <p>
   * The username of the user to be used as a default value.
   * </p>
   *
   * @param username the username property
   * @return the component itself
   */
  public Login setUsername(String username) {
    set(this.username, username);
    return this;
  }

  /**
   * Gets the username property.
   *
   * @return the username property
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

  Element getOriginalElement() {
    return getElement();
  }
}
