package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * Represents a masked number field.
 *
 * <p>
 * The masked number field is a text field that allows the user to enter numbers in a specific
 * format. The format is defined by a mask which is a string that contains the characters that the
 * user can enter.
 * </p>
 *
 * <p>
 * The mask format is as follows:
 * </p>
 *
 * <table border="1">
 * <tr>
 * <th>Character</th>
 * <th>Description</th>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>A zero is always replaced by a digit (0..9).</td>
 * </tr>
 * <tr>
 * <td>#</td>
 * <td>The pound sign is used to suppress leading zeroes. It is replaced by the fill character for
 * leading zeroes to the left of the decimal point. For trailing zeros to the right of the decimal
 * point, it is replaced by a space or a zero. Any other time it is replaced by a digit.</td>
 * </tr>
 * <tr>
 * <td>,</td>
 * <td>To the left of the decimal point, the comma is replaced by the fill character if no digits
 * have yet been placed. Any other time, it results in a comma.</td>
 * </tr>
 * <tr>
 * <td>-</td>
 * <td>The minus sign creates a "-" in the result if the number is negative; otherwise, it is
 * replaced by the fill character.</td>
 * </tr>
 * <tr>
 * <td>+</td>
 * <td>The plus sign becomes a "+" in the result if the number is positive, or a "-" if the number
 * is negative.</td>
 * </tr>
 * <tr>
 * <td>$</td>
 * <td>The dollar sign always results in a dollar sign.</td>
 * </tr>
 * <tr>
 * <td>(</td>
 * <td>A left parenthesis results in a "(" if the number is negative, or the fill character if
 * positive.</td>
 * </tr>
 * <tr>
 * <td>)</td>
 * <td>A right parenthesis results in a ")" if the number is negative, or the fill character if
 * positive.</td>
 * </tr>
 * <tr>
 * <td>CR</td>
 * <td>The characters "CR" are inserted into the number if the number is negative. Two spaces are
 * inserted if the number is positive.</td>
 * </tr>
 * <tr>
 * <td>DR</td>
 * <td>The characters "CR" are inserted into the number if the number is negative. The characters
 * "DR" are inserted if the number is positive.</td>
 * </tr>
 * <tr>
 * <td>*</td>
 * <td>The asterisk "*" is inserted into the number.</td>
 * </tr>
 * <tr>
 * <td>.</td>
 * <td>The decimal point is replaced by a decimal point if any digits appear in the output mask.
 * Otherwise, it is replaced by the fill character. After the decimal point, the fill character
 * becomes a space.</td>
 * </tr>
 * <tr>
 * <td>B</td>
 * <td>The upper case "B" always becomes a space. Any other character is simply copied to the
 * result.</td>
 * </tr>
 * </table>
 *
 * <p>
 * Some of the above characters may possibly float within the mask. These are "-", "+", "$", and
 * "(". If any of these characters is present in the mask, the first one encountered will be moved
 * to the last position where a "#" or "," was replaced by the fill character. If no such position
 * exists, the float character is left where it is.
 * </p>
 *
 * <p>
 * NOTE: A mask within a field does NOT round. For example, when placing a value such as 12.34567
 * into a field that is masked with ###0.00, you'll get 12.34.
 * </p>
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
public class MaskedNumberField extends DwcMaskedField<MaskedNumberField, Float> {
  private String groupCharacter = null;
  private String decimalCharacter = null;
  private boolean negateable = true;

  /**
   * Sets the group character.
   *
   * <p>
   * By default, the group character is browser language dependent. For example, in English, the
   * group character is a comma (,) in German, it is a period (.) in English. This option allows you
   * to override the default group character and set it to a custom character.
   * </p>
   *
   * @param groupCharacter the group character
   * @return the component itself
   */
  public MaskedNumberField setGroupCharacter(String groupCharacter) {
    verifySeparatorsDifferent();

    this.groupCharacter = groupCharacter;

    BBjInputN field = inferNumberField();

    if (field != null) {
      try {
        field.setCommaCharacter(groupCharacter);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Gets the group character.
   *
   * @return the group character
   */
  public String getGroupCharacter() {
    return groupCharacter;
  }

  /**
   * Sets the decimal character.
   *
   * <p>
   * By default, the decimal character is browser language dependent. For example, in English, the
   * decimal character is a period (.) in German, it is a comma (,) in English. This option allows
   * you to override the default decimal character and set it to a custom character.
   * </p>
   *
   * @param decimalCharacter the decimal character
   * @return the component itself
   */
  public MaskedNumberField setDecimalCharacter(String decimalCharacter) {
    verifySeparatorsDifferent();

    this.decimalCharacter = decimalCharacter;

    BBjInputN field = inferNumberField();

    if (field != null) {
      try {
        field.setDotCharacter(decimalCharacter);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Gets the decimal character.
   *
   * @return the decimal character
   */
  public String getDecimalCharacter() {
    return decimalCharacter;
  }

  /**
   * Specifies whether the field will accept negative values.
   *
   * <p>
   * By default, the field is negateable
   * </p>
   *
   * @param negateable whether the field is negateable
   * @return the component itself
   */
  public MaskedNumberField setNegateable(boolean negateable) {
    this.negateable = negateable;

    BBjInputN field = inferNumberField();

    if (field != null) {
      try {
        field.setNegateable(negateable);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Gets whether the field is negateable.
   *
   * @return whether the field is negateable
   */
  public boolean isNegateable() {
    return negateable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberField setValue(Float value) {
    setText(String.valueOf(value));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Float getValue() {
    return convertValue(getText());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputN(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputN", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (groupCharacter != null) {
      setGroupCharacter(groupCharacter);
    }

    if (decimalCharacter != null) {
      setDecimalCharacter(decimalCharacter);
    }

    if (!negateable) {
      setNegateable(negateable);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Float convertValue(String value) {
    try {
      return Float.valueOf(value);
    } catch (NumberFormatException e) {
      return null;
    }
  }

  private BBjInputN inferNumberField() {
    return (BBjInputN) inferField();
  }

  private void verifySeparatorsDifferent() {
    if (groupCharacter != null && decimalCharacter != null
        && groupCharacter.equals(decimalCharacter)) {
      throw new IllegalArgumentException("Group character and decimal character must be different");
    }
  }
}
