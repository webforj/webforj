package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasPattern;
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
    implements HasPattern<MaskedTextField> {
  static final String DEFAULT_MASK =
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

  private String pattern = null;

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
  protected String convertValue(String value) {
    return value;
  }
}
