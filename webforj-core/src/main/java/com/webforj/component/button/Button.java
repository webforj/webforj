package com.webforj.component.button;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.button.event.ButtonClickEvent;
import com.webforj.component.window.Window;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * An implementation of a "push" button.
 *
 * <p>
 * A button in a user interface is a visual element that users can click or tap to initiate specific
 * actions or interactions within a software application or system. Buttons feature a label or icon
 * that provides a clear description of their intended function, helping users understand their
 * purpose. These UI components play a fundamental role in enabling user interactivity, making it
 * easier for users to navigate and interact with the software, whether it's a simple "Submit"
 * button on a form or a more complex component within an UI.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public final class Button extends DwcButton<Button> {

  /**
   * Construct the button with the given text and a {@link ButtonClickEvent}.
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
   * Construct the button without any text set.
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
      throw new WebforjRuntimeException("Failed to create the BBjButton Control", e);
    }
  }
}
