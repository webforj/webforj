package org.dwcj.bbjplugins.thread.sinks;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.thread.BBjThreadProxy;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

public class BBjThreadProxyFinishedEventSink extends BBjThreadProxyEventSink{

    public BBjThreadProxyFinishedEventSink(BBjThreadProxy proxy) {
        super(proxy,12350);
    }

    @Override
    public void onEvent(BBjCustomEvent ev){//NOSONAR
        super.onEvent(ev);
    }
}
