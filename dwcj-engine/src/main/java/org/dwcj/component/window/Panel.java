package org.dwcj.component.window;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.component.Component;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * A panel is a container that can be used to group other components.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class Panel extends Window {

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
    BBjWindow topLevelWindow = window.getBBjWindow();

    try {
      byte finalFlag = 0x00;
      if (Boolean.FALSE.equals(isVisible())) {
        finalFlag += (byte) 0x10;
      }

      byte[] flags = new byte[] {(byte) 0x00, (byte) 0x10, (byte) 0x88, finalFlag};
      BBjWindow wnd = topLevelWindow.addChildWindow(topLevelWindow.getAvailableControlID(), "",
          flags, Environment.getCurrent().getSysGui().getAvailableContext());
      setBBjWindow(wnd);
      setStyle("overflow", "unset");
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to create panel", e);
    }
  }
}
