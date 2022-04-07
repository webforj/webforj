package org.dwcj.events;

import com.basis.bbj.proxies.event.BBjEvent;

/**
 * the event proxy interface that is implemented by the BBj server side
 */
public interface IEventProxy {
    void onEvent(BBjEvent ev);
}
