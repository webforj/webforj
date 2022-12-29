package org.dwcj.bbjplugins.thread.sinks;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.bbjplugins.thread.BBjThreadProxy;

public class BBjThreadProxyFinishedEventSink extends BBjThreadProxyEventSink{

    public BBjThreadProxyFinishedEventSink(BBjThreadProxy proxy) {
        super(proxy,12350);
    }

    @Override
    public void onEvent(BBjCustomEvent ev){//NOSONAR
        super.onEvent(ev);
    }
}
