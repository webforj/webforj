package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.annotation.tooling.MetaInf;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * An implementation of a "push" button.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
@MetaInf(group = "dwc.form")
public final class Button extends AbstractDwcButton<Button> {

  /**
   * Construct the button with the given text.
   *
   * @param text the text of the button
   * @param onClickListener the listener to be called when the button is clicked
   */
  public Button(String text, EventListener<ButtonClickEvent> onClickListener) {
    super();
    setText(text);
    addClickListener(onClickListener);
  }

  /**
   * Construct the button with the given text.
   *
   * @param text the text of the button
   */
  public Button(String text) {
    super();
    setText(text);
  }

  /**
   * Construct the button with the given text.
   */
  public Button() {
    this("");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addButton(getText(), flags));
      catchUp();
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create the BBjButton Control", e);
    }
  }
}
