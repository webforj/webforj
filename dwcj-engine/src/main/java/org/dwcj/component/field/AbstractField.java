package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.util.Arrays;
import java.util.List;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Expanse;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasExpanse;
import org.dwcj.component.HasFocus;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.HasValue;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.KeypressEvent;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.EventSinkListenerRegistry;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.KeypressEventSink;
import org.dwcj.component.event.sink.ModifyEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * The Base class for all fields components.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
abstract class AbstractField<T extends AbstractDwcComponent & HasFocus & TabTraversable & HasEnable & HasReadOnly, V>
    extends AbstractDwcComponent implements DwcjFieldComponent, HasEnable, HasReadOnly, HasFocus,
    TabTraversable, HasExpanse<T, Expanse>, HasValue<T, V> {

  // @formatter:off
  /**
   * Enumeration representing the supported values for the field 'autocomplete'.
   */
  public enum Autocomplete {
    /**
     * The browser is not permitted to automatically enter or select a value for this field.
     */
    OFF("off"),

    /**
     * The browser is allowed to automatically complete the input.
     * No guidance is provided as to the type of data expected in the field,
     * so the browser may use its own judgment.
     */
    ON("on"),

    /**
     * The field expects the value to be a person's full name.
     * Using "name" rather than breaking the name down into its components is generally preferred
     * because it avoids dealing with the wide diversity of human names and how they are structured;
     * however, you can use the following autocomplete values if you do need to break the
     * name down into its components:
     *
     * <ul>
     *   <li>{@link #HONORIFIC_PREFIX}</li>
     *   <li>{@link #GIVEN_NAME}</li>
     *   <li>{@link #ADDITIONAL_NAME}</li>
     *   <li>{@link #FAMILY_NAME}</li>
     *   <li>{@link #HONORIFIC_SUFFIX}</li>
     * </ul>
     */
    NAME("name"),

    /**
     * The prefix or title, such as "Mrs.", "Mr.", "Miss", "Ms.", "Dr.", or "Mlle.".
     */
    HONORIFIC_PREFIX("honorific-prefix"),

    /**
     * The given (or "first") name.
     */
    GIVEN_NAME("given-name"),

    /**
     * The middle name.
     */
    ADDITIONAL_NAME("additional-name"),

    /**
     * The family (or "last") name.
     */
    FAMILY_NAME("family-name"),

    /**
     * The suffix, such as "Jr.", "B.Sc.", "PhD.", "MBASW", or "IV".
     */
    HONORIFIC_SUFFIX("honorific-suffix"),

    /**
     * A nickname or handle.
     */
    NICKNAME("nickname"),

    /**
     * An email address.
     */
    EMAIL("email"),

    /**
     * A username or account name.
     */
    USERNAME("username"),

    /**
     * A new password. When creating a new account or changing passwords.
     */
    NEW_PASSWORD("new-password"),

    /**
     * The user's current password.
     */
    CURRENT_PASSWORD("current-password"),

    /**
     * A one-time code used for verifying user identity.
     */
    ONE_TIME_CODE("one-time-code"),

    /**
     * A job title, or the title a person has within an organization, such as
     * "Senior Technical Writer", "President", or "Assistant Troop Leader".
     */
    ORGANIZATION_TITLE("organization-title"),

    /**
     * A company or organization name, such as "Acme Widget Company" or "Girl Scouts of America".
     */
    ORGANIZATION("organization"),

    /**
     * A street address. This can be multiple lines of text and should fully
     * identify the location of the address within its second administrative level
     * (typically a city or town), but should not include the city name, ZIP
     * or postal code, or country name.
     */
    STREET_ADDRESS("street-address"),

    /**
     * Each individual line of the street address. These should only
     * be present if the "street-address" is not present.
     */
    ADDRESS_LINE1("address-line1"),

    /**
     * Each individual line of the street address. These should only
     * be present if the "street-address" is not present.
     */
    ADDRESS_LINE2("address-line2"),

    /**
     * Each individual line of the street address. These should only
     * be present if the "street-address" is not present.
     */
    ADDRESS_LINE3("address-line3"),

    /**
     * The finest-grained administrative level, in addresses which have four levels.
     */
    ADDRESS_LEVEL4("address-level4"),

    /**
     * The third administrative level, in addresses with at least three administrative levels.
     */
    ADDRESS_LEVEL3("address-level3"),

    /**
     * The second administrative level, in addresses with at least two of them.
     * In countries with two administrative levels, this would typically be the city, town, village,
     * or other locality in which the address is located.
     */
    ADDRESS_LEVEL2("address-level2"),

    /**
     * The first administrative level in the address.
     * This is typically the province in which the address is located.
     * In the United States, this would be the state.
     * In Switzerland, the canton.
     * In the United Kingdom, the post town.
     */
    ADDRESS_LEVEL1("address-level1"),

    /**
     * A country or territory code.
     */
    COUNTRY("country"),

    /**
     * A country or territory name.
     */
    COUNTRY_NAME("country-name"),

    /**
     * A postal code (in the United States, this is the ZIP code).
     */
    POSTAL_CODE("postal-code"),

    /**
     * The full name as printed on or associated with a payment instrument such as a credit card.
     * Using a full name field is preferred, typically, over breaking the name into pieces.
     */
    CC_NAME("cc-name"),

    /**
     * A given (first) name as given on a payment instrument like a credit card.
     */
    CC_GIVEN_NAME("cc-given-name"),

    /**
     * A middle name as given on a payment instrument or credit card.
     */
    CC_ADDITIONAL_NAME("cc-additional-name"),

    /**
     * A family name, as given on a credit card.
     */
    CC_FAMILY_NAME("cc-family-name"),

    /**
     * A credit card number or other number identifying a payment method, such as an account number.
     */
    CC_NUMBER("cc-number"),

    /**
     * A payment method expiration date, typically in the form "MM/YY" or "MM/YYYY".
     */
    CC_EXP("cc-exp"),

    /**
     * The month in which the payment method expires.
     */
    CC_EXP_MONTH("cc-exp-month"),

    /**
     * The year in which the payment method expires.
     */
    CC_EXP_YEAR("cc-exp-year"),

    /**
     * The security code for the payment instrument; on credit cards,
     * this is the 3-digit verification number on the back of the card.
     */
    CC_CSC("cc-csc"),

    /**
     * The type of payment instrument (such as "Visa" or "Master Card").
     */
    CC_TYPE("cc-type"),

    /**
     * The currency in which the transaction is to take place.
     */
    TRANSACTION_CURRENCY("transaction-currency"),

    /**
     * The amount, given in the currency specified by "transaction-currency", of the transaction,
     * for a payment form.
     */
    TRANSACTION_AMOUNT("transaction-amount"),

    /**
     * A preferred language, given as a valid BCP 47 language tag.
     */
    LANGUAGE("language"),

    /**
     * A birth date, as a full date.
     */
    BDAY("bday"),

    /**
     * The day of the month of a birth date.
     */
    BDAY_DAY("bday-day"),

    /**
     * The month of the year of a birth date.
     */
    BDAY_MONTH("bday-month"),

    /**
     * The year of a birth date.
     */
    BDAY_YEAR("bday-year"),

    /**
     * A gender identity (such as "Female", "Fa'afafine", "Hijra", "Male", "Nonbinary"),
     * as freeform text without newlines.
     */
    SEX("sex"),

    /**
     * A full telephone number, including the country code.
     * If you need to break the phone number up into its components,
     * you can use these values for those fields:
     * <ul>
     *   <li>{@link #TEL_COUNTRY_CODE}</li>
     *   <li>{@link #TEL_NATIONAL}</li>
     *   <li>{@link #TEL_AREA_CODE}</li>
     *   <li>{@link #TEL_LOCAL}</li>
     *   <li>{@link #TEL_EXTENSION}</li>
     * </ul>
     */
    TEL("tel"),

    /**
     * The country code, such as "1" for the United States, Canada,
     * and other areas in North America and parts of the Caribbean.
     */
    TEL_COUNTRY_CODE("tel-country-code"),

    /**
     * The entire phone number without the country code component,
     * including a country-internal prefix.
     * For the phone number "1-855-555-6502", this field's value would be "855-555-6502".
     */
    TEL_NATIONAL("tel-national"),

    /**
     * The area code, with any country-internal prefix applied if appropriate.
     */
    TEL_AREA_CODE("tel-area-code"),

    /**
     * The phone number without the country or area code.
     * This can be split further into two parts, for phone numbers which have an exchange number
     * and then a number within the exchange.
     * For the phone number "555-6502", use "tel-local-prefix" for "555"
     * and "tel-local-suffix" for "6502".
     */
    TEL_LOCAL("tel-local"),

    /**
     * A telephone extension code within the phone number,
     * such as a room or suite number in a hotel or an office extension in a company.
     */
    TEL_EXTENSION("tel-extension"),

    /**
     * A URL for an instant messaging protocol endpoint, such as "xmpp:username@example.net".
     */
    IMPP("impp"),

    /**
     * A URL, such as a home page or company website address as appropriate given the context
     * of the other fields in the form.
     */
    URL("url"),

    /**
     * The URL of an image representing the person, company, or contact information
     * given in the other fields in the form.
     */
    PHOTO("photo"),

    /**
     * Passkeys generated by the Web Authentication API.
     */
    CURRENT("current"),

    /**
     * Values entered by the user are not stored by the browser.
     * The purpose of this value is to prevent the browser from exposing previously entered
     * values to potential attackers when autocomplete is enabled in the browser settings.
     */
    SECTION("section"),

    /**
     * Special value used by the browser to automatically suggest a password for the current
     * login form. This value is intended to be used when creating a new account or
     * changing passwords, so that the browser can offer assistance in creating a secure password.
     * (See also Preventing autofilling with autocomplete="current-password".)
     */
    NEW_CURRENT_PASSWORD("new-current-password");

    private final String value;

    /**
     * Constructs a new `AutocompleteValue` with the specified value.
     *
     * @param value the autocomplete value
     */
    Autocomplete(String value) {
      this.value = value;
    }

    /**
     * Returns the string value of the `AutocompleteValue`.
     *
     * @return the autocomplete value as a string
     */
    public String getValue() {
      return value;
    }
  }
  // @formatter:on

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkListenerRegistry<ModifyEvent> modifyEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ModifyEventSink(this, dispatcher), ModifyEvent.class);
  private EventSinkListenerRegistry<KeypressEvent> keypressEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new KeypressEventSink(this, dispatcher), KeypressEvent.class);
  private EventSinkListenerRegistry<FocusEvent> focusEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new FocusEventSink(this, dispatcher), FocusEvent.class);
  private EventSinkListenerRegistry<BlurEvent> blurEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new BlurEventSink(this, dispatcher), BlurEvent.class);
  private EventSinkListenerRegistry<MouseEnterEvent> mouseEnterEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseEnterEventSink(this, dispatcher),
          MouseEnterEvent.class);
  private EventSinkListenerRegistry<MouseExitEvent> mouseExitEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseExitEventSink(this, dispatcher),
          MouseExitEvent.class);
  private EventSinkListenerRegistry<RightMouseDownEvent> rightMouseDownEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);

  private Autocomplete autocomplete = Autocomplete.OFF;
  private boolean autoFocus = false;
  private String label = "";
  private boolean required = false;
  private boolean spellcheck = false;

  /**
   * Construct a new field.
   */
  protected AbstractField() {
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * Specify what if any permission the user agent has to provide automated assistance in filling
   * out form field values, as well as guidance to the browser as to the type of information
   * expected in the field.
   *
   * @param autocomplete the autocomplete value
   * @return the component itself
   */
  public T setAutoComplete(Autocomplete autocomplete) {
    this.autocomplete = autocomplete;
    setUnrestrictedProperty("autocomplete", this.autocomplete.getValue());

    return getSelf();
  }

  /**
   * Get the autocomplete value.
   *
   * @return the autocomplete value
   */
  public Autocomplete getAutoComplete() {
    return this.autocomplete;
  }

  /**
   * Set the field's label.
   *
   * <p>
   * A field label is a descriptive text or title that is associated with the field. It provides a
   * brief explanation or prompt to help users understand the purpose or expected input for that
   * particular field. Field labels are not only important for usability but also play a crucial
   * role in accessibility, as they enable screen readers and assistive technologies to provide
   * accurate information and facilitate keyboard navigation.
   * </p>
   *
   * @param label the label
   * @return the component itself
   */
  public T setLabel(String label) {
    this.label = label;
    setUnrestrictedProperty("label", this.label);

    return getSelf();
  }

  /**
   * Get the field's label.
   *
   * @return the field's label
   */
  public String getLabel() {
    return this.label;
  }

  /**
   * Set the field's required state.
   *
   * <p>
   * A field is required when the user must provide a value before submitting a form. This is mainly
   * used in conjunction with the {@link #setLabel(String)} to provide a visual indication to users
   * that the field is required.
   * </p>
   *
   * @param required true if the field is required, false otherwise
   * @return the component itself
   */
  public T setRequired(boolean required) {
    this.required = required;
    setUnrestrictedProperty("required", this.required);

    return getSelf();
  }

  /**
   * Check if the field is required.
   *
   * @return true if the field is required, false otherwise
   */
  public boolean isRequired() {
    return this.required;
  }

  /**
   * Set whether the field's value may be checked for spelling errors.
   *
   * @param spellcheck true if the field's value may be checked for spelling errors, false
   *        otherwise.
   * @return the component itself
   */
  public T setSpellCheck(boolean spellcheck) {
    this.spellcheck = spellcheck;
    setUnrestrictedProperty("spellcheck", this.spellcheck);

    return getSelf();
  }

  /**
   * Check if the field has spellcheck enabled.
   *
   * @return true if the field is spellcheck, false otherwise
   */
  public boolean isSpellCheck() {
    return this.spellcheck;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Expanse getExpanse() {
    return (Expanse) getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T focus() {
    super.focusComponent();
    return getSelf();
  }

  /**
   * When true, should automatically have focus when the app has finished loading.
   *
   * @param autofocus true to automatically have focus when the app has finished loading.
   * @return the component itself
   */
  public T setAutoFocus(boolean autofocus) {
    this.autoFocus = autofocus;
    setUnrestrictedProperty("autofocus", autofocus);
    return getSelf();
  }

  /**
   * Check if the component should automatically have focus when the app has finished loading.
   *
   * @return true if the component should automatically have focus when the app has finished
   *         loading.
   */
  public boolean isAutoFocus() {
    return this.autoFocus;
  }

  /**
   * Check if the component has focus.
   *
   * <p>
   * The method will always reach the client to get the focus state. If the component is not
   * attached to a panel, the method will return false even if the component {@link #focus()} method
   * was called.
   * </p>
   *
   * @return true if the component has focus, false if not.
   */
  public boolean hasFocus() {
    return Boolean.valueOf(String.valueOf(getProperty("hasFocus")));
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTabTraversable(Boolean traversable) {
    super.setComponentTabTraversable(traversable);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isTabTraversable() {
    return super.isComponentTabTraversable();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setReadOnly(Boolean readonly) {
    super.setComponentReadOnly(readonly);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isReadOnly() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setText(String text) {
    super.setText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setVisible(Boolean visible) {
    super.setVisible(visible);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTooltipText(String text) {
    super.setTooltipText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setProperty(String property, Object value) {
    super.setProperty(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setStyle(String property, String value) {
    super.setStyle(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T addClassName(String selector) {
    super.addClassName(selector);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T removeClassName(String selector) {
    super.removeClassName(selector);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("accept", "autoValidate", "autoValidateOnLoad",
        "autoWasValidated", "autocomplete", "autocorrect", "autofocus", "disabled", "expanse",
        "hasFocus", "highlightBehaviors", "invalid", "invalidMessage", "label", "max", "maxlength",
        "min", "minlength", "multiple", "name", "passwordReveal", "pattern", "placeholder",
        "readonly", "required", "showSpinners", "size", "spellcheck", "spinnable", "step",
        "tabTraversable", "type", "valid", "validationIcon", "validationPopoverDistance",
        "validationPopoverPlacement", "validationPopoverSkidding", "validationStyle", "validator",
        "value"));

    return properties;
  }

  /**
   * Get the event dispatcher instance for the component.
   *
   * @return The instance of the event dispatcher.
   */
  EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Add a {@link ModifyEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addModifyListener(EventListener<ModifyEvent> listener) {
    this.modifyEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addModifyListener(EventListener) addModifyListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onModify(EventListener<ModifyEvent> listener) {
    return addModifyListener(listener);
  }

  /**
   * Removes a {@link ModifyEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeModifyListener(EventListener<ModifyEvent> listener) {
    this.modifyEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link KeypressEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addKeypressListener(EventListener<KeypressEvent> listener) {
    this.keypressEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addKeypressListener(EventListener) addKeypressListener}.
   *
   * @see AbstractField #addKeypressListener(EventListener)
   * @param listener The event listener to be removed
   *
   * @return the component itself
   */
  public T onKeypress(EventListener<KeypressEvent> listener) {
    return addKeypressListener(listener);
  }

  /**
   * Removes a {@link KeypressEvent} listener from the component.
   *
   * @param listener The event listener to be removed
   * @return The component itself
   */
  public T removeKeypressListener(EventListener<KeypressEvent> listener) {
    this.keypressEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link FocusEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addFocusListener(EventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a {@link FocusEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link BlurEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addBlurListener(EventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a {@link BlurEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Adds a {@link MouseEnterEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addMouseEnterListener(EventListener) addMouseEnterListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Remove a {@link MouseEnterEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link MouseExitEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addMouseExitListener(EventListener) addMouseExitListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Remove a {@link MouseExitEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add a {@link RightMouseDownEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addRightMouseDownListener(EventListener) addRightMouseDownListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Remove a {@link RightMouseDownEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addEditBox(getText(), flags));
      catchUp();
    } catch (BBjException | IllegalAccessException e) {
      throw new DwcjRuntimeException("Failed to create BBjEditBox", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }

    // catch up event listeners
    this.modifyEventSinkListenerRegistry.catchUp();
    this.keypressEventSinkListenerRegistry.catchUp();
    this.focusEventSinkListenerRegistry.catchUp();
    this.blurEventSinkListenerRegistry.catchUp();
    this.mouseEnterEventSinkListenerRegistry.catchUp();
    this.mouseExitEventSinkListenerRegistry.catchUp();
    this.rightMouseDownEventSinkListenerRegistry.catchUp();

    super.catchUp();
  }

  protected BBjEditBox getBbjControl() {
    try {
      return (BBjEditBox) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  protected T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}
