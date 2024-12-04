package com.webforj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.component.Component;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * A panel is a container that can be used to group other components.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class Panel extends Window {

  /**
   * Constructs a new Panel window.
   *
   * @param components The components to be added to the Panel.
   */
  public Panel(Component... components) {
    super();
    add(components);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    super.onCreate(window);

    BBjWindow topLevelWindow = window.getBbjWindow();

    try {
      byte finalFlag = 0x00;
      if (Boolean.FALSE.equals(isVisible())) {
        finalFlag += (byte) 0x10;
      }

      byte[] flags = new byte[] {(byte) 0x10, (byte) 0x10, (byte) 0x88, finalFlag};
      BBjWindow wnd = topLevelWindow.addChildWindow(topLevelWindow.getAvailableControlID(), "",
          flags, Environment.getCurrent().getSysGui().getAvailableContext());
      setBbjWindow(wnd);
      setStyle("overflow", "unset");
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to create panel", e);
    }
  }
}
