package org.dwcj.component.text;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.window.Window;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * A label component.
 *
 * <p>
 * This class represents a label component that can display text within the framework. It extends
 * the {@link DwcComponent} class and implements the {@link HasHorizontalAlignment} interface,
 * allowing you to set the horizontal alignment of the label text.
 * </p>
 *
 * @see DwcComponent
 * @see HasHorizontalAlignment
 *
 * @author Hyyan Abo Fakher
 */
public final class Label extends DwcComponent<Label> implements HasHorizontalAlignment<Label> {

  private boolean lineWrap = true;

  /**
   * Default Constructor to automatically create an empty label.
   */
  public Label() {
    this("", true);
  }

  /**
   * Constructor used to give the label initial text.
   *
   * @param text String value for initial display text
   */
  public Label(String text) {
    this(text, true);
  }

  /**
   * Constructor used to give the label initial text and wether it is linewrapped or not.
   *
   * @param text String value for initial display text
   * @param wrap Boolean value for linewrapping.
   */
  public Label(String text, boolean wrap) {
    setText(text);
    setWrap(wrap);
    setComponentDefaultHorizontalAlignment(Alignment.LEFT);
  }

  /**
   * Sets whether the lines will be wrapped in the label.
   *
   * @param wrap - Specifies whether the lines will be wrapped (false = Not Wrapped, true = Wrapped)
   * @return Returns this
   */
  public Label setWrap(Boolean wrap) {
    if (inferControl() != null) {
      try {
        inferControl().setLineWrap(wrap);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.lineWrap = wrap;
    return this;
  }

  /**
   * Returns whether lines are wrapped in the label.
   *
   * @return Returns whether the lines are wrapped in the component (false = Not Wrapped, true =
   *         Wrapped).
   */
  public boolean isWrap() {
    return this.lineWrap;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Label setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), true);
      setControl(w.addStaticText(getText(), flags));
    } catch (Exception e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (!this.lineWrap) {
      this.setWrap(lineWrap);
    }
  }

  private BBjStaticText inferControl() {
    try {
      return (BBjStaticText) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
