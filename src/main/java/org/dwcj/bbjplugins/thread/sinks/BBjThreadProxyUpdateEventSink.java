package org.dwcj.bbjplugins.thread.sinks;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.bbjplugins.thread.BBjThreadProxy;

public class BBjThreadProxyUpdateEventSink extends BBjThreadProxyEventSink{

    public BBjThreadProxyUpdateEventSink(BBjThreadProxy proxy) {
        super(proxy, 12352);
    }

    @Override
    public void onEvent(BBjCustomEvent ev){//NOSONAR
        super.onEvent(ev);
    }
}
