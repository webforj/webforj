package org.dwcj.bbjplugins.thread.sinks;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.thread.BBjThreadProxy;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

public class BBjThreadProxyEventSink {

    protected final ArrayList<Consumer<BBjThreadProxy>> consumers = new ArrayList<>();
    protected final BBjThreadProxy proxy;
    private final IDwcjBBjBridge dwcjHelper = Environment.getInstance().getDwcjHelper();

    private final int eventType;


    protected BBjThreadProxyEventSink(BBjThreadProxy proxy, int eventType) {
        this.proxy = proxy;
        this.eventType = eventType;

    }

    public void onEvent(BBjCustomEvent ev){//NOSONAR
        for (Consumer<BBjThreadProxy> consumer : consumers) {
            consumer.accept(proxy);
        }
    }

    public void addCallback(Consumer<BBjThreadProxy> c) {

        if (consumers.isEmpty()){
            ArrayList<Object> arglist = new ArrayList<>();
            arglist.add(eventType);
            arglist.add(  Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"));
            arglist.add( "onEvent");
            dwcjHelper.invokeMethod(proxy.getBBjThreadInstance(),"setCallback",arglist);
        }
        consumers.add(c);

    }
}
