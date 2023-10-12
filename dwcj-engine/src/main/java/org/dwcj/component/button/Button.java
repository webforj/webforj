package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.event.ComponentEventListener;
import org.dwcj.component.window.Window;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * An implementation of a "push" button.
 *
 * <p>
 * A button in a user interface is a visual element that users can click or tap to initiate specific
 * actions or interactions within a software application or system. Buttons feature a label or icon
 * that provides a clear description of their intended function, helping users understand their
 * purpose. These UI component play a fundamental role in enabling user interactivity, making it
 * easier for users to navigate, component, and interact with the software, whether it's a simple
 * "Submit" button on a form or a more complex component within an UI.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public final class Button extends AbstractDwcButton<Button> {

  /**
   * Construct the button with the given text.
   *
   * @param text the text of the button
   * @param onClickListener the listener to be called when the button is clicked
   */
  public Button(String text, ComponentEventListener<ButtonClickEvent> onClickListener) {
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
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addButton(getText(), flags));
    } catch (Exception e) {
      throw new DwcjRuntimeException("Failed to create the BBjButton Control", e);
    }
  }
}
