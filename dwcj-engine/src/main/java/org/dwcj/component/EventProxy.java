package org.dwcj.component;

import com.basis.bbj.proxies.event.BBjEvent;

/**
 * the event proxy interface that is implemented by the BBj server side
 */
public interface EventProxy {
  void onEvent(BBjEvent ev);
}
