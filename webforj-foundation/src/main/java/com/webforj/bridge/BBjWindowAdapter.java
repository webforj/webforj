package com.webforj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.component.window.Window;

/**
 * This class adapts a BBjWindow for use with webforJ components.
 *
 * <p>
 * It is specifically designed for integrating webforJ components into BBj code. This adapter
 * converts a BBjWindow into a webforJ {@link Window}, enabling the addition of webforJ components
 * in BBj code.
 * </p>
 */
public final class BBjWindowAdapter extends Window {

  /**
   * Constructs a BBjWindowAdapter with the given BBjWindow.
   *
   * @param w the BBjWindow to be adapted
   */
  public BBjWindowAdapter(BBjWindow w) {
    setBbjWindow(w);
    create(this);
  }
}
