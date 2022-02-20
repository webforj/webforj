package org.dwcj.events;

import com.basis.bbj.proxies.event.BBjEvent;

public interface IEventProxy {
    void onEvent(BBjEvent ev);
}
