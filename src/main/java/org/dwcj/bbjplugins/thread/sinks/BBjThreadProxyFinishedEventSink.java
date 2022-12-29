package org.dwcj.bbjplugins.thread.sinks;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.thread.BBjThreadProxy;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

public class BBjThreadProxyFinishedEventSink {

    private final ArrayList<Consumer<BBjThreadProxy>> consumers = new ArrayList<>();
    private final BBjThreadProxy proxy;
    private final IDwcjBBjBridge dwcjHelper = Environment.getInstance().getDwcjHelper();


    public BBjThreadProxyFinishedEventSink(BBjThreadProxy proxy) {
        this.proxy = proxy;

    }

    public void finishedEvent(BBjCustomEvent ev){//NOSONAR
        for (Consumer<BBjThreadProxy> consumer : consumers) {
            consumer.accept(proxy);
        }
    }

    public void addCallback(Consumer<BBjThreadProxy> c) {

        if (consumers.isEmpty()){
            ArrayList<Object> arglist = new ArrayList<>();
            arglist.add(12350);
            arglist.add(  Environment.getInstance().getDwcjHelper().getEventProxy(this, "finishedEvent"));
            arglist.add( "onEvent");
            dwcjHelper.invokeMethod(proxy.getBBjThreadInstance(),"setCallback",arglist);
        }
        consumers.add(c);

    }
}
