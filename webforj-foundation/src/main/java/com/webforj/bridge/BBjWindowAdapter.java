package com.webforj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.webforj.component.window.Window;

/**
 * ********** IMPORTANT: ****************+ This class is only needed for using DWCJ Controls from
 * BBj code. It has no relevance to the Java development with DWCJ. The BBjPanelAdapter converts a
 * BBjWindow into an AbstractDwcPanel so that DWCJ Controls can be added to code that is written in
 * the BBj language.
 */
public final class BBjWindowAdapter extends Window {

  public BBjWindowAdapter(BBjWindow w) {
    setBbjWindow(w);
    create(this);
  }
}
