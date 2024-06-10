package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.concern.HasPattern;
import com.webforj.concern.HasRestoreValue;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * Represents a masked text field.
 *
 * <p>
 * The masked text field is a text field that allows the user to enter text in a specific format.
 * The format is defined by a mask which is a string that contains the characters that the user can
 * enter.
 * </p>
 *
 * <p>
 * The mask format is as follows:
 * </p>
 *
 * <table border="1">
 * <caption>Mask Format</caption>
 * <tr>
 * <th>Mask Character</th>
 * <th>Accepts</th>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Any printable character.</td>
 * </tr>
 * <tr>
 * <td>a</td>
 * <td>Any alphabetic character.</td>
 * </tr>
 * <tr>
 * <td>A</td>
 * <td>Any alphabetic character. Converts lower-case alphabetic characters to upper case.</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Any digit.</td>
 * </tr>
 * <tr>
 * <td>z</td>
 * <td>Any digit or alphabetic character.</td>
 * </tr>
 * <tr>
 * <td>Z</td>
 * <td>Any digit or alphabetic character. Converts lower-case alphabetic characters to upper
 * case.</td>
 * </tr>
 * <tr>
 * <td>Any other character</td>
 * <td>Represents itself.</td>
 * </tr>
 * </table>
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedTextField extends DwcMaskedField<MaskedTextField, String>
    implements HasPattern<MaskedTextField>, HasRestoreValue<MaskedTextField, String> {
  static final String DEFAULT_MASK =
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

  private String pattern = null;
  private String restoreValue = "";

  /**
   * Constructs a new masked text field.
   */
  public MaskedTextField() {
    super();
    setMask(DEFAULT_MASK);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component setName(String name) {
    super.setName(name);
    setUnrestrictedProperty("name", name);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField setValue(String value) {
    setText(value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return getText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField setPattern(String pattern) {
    this.pattern = pattern;
    setUnrestrictedProperty("pattern", pattern);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPattern() {
    return pattern;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField setRestoreValue(String value) {
    BBjInputE field = inferTextField();

    if (field != null) {
      try {
        field.setRestore(value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.restoreValue = value;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getRestoreValue() {
    return restoreValue;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField restoreValue() {
    return setValue(restoreValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputE(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputE", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (restoreValue != null && !restoreValue.isEmpty()) {
      setRestoreValue(this.restoreValue);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String convertValue(String value) {
    return value;
  }

  /**
   * Gets the instance of the underlying BBjInputE control.
   *
   * @return the instance of the control
   */
  protected BBjInputE inferTextField() {
    return (BBjInputE) inferField();
  }
}
