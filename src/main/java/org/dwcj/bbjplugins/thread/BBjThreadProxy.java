package org.dwcj.bbjplugins.thread;

import org.dwcj.Environment;
import org.dwcj.bbjplugins.thread.sinks.BBjThreadProxyFinishedEventSink;
import org.dwcj.bbjplugins.thread.sinks.BBjThreadProxyUpdateEventSink;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

public final class BBjThreadProxy {

    private final IDwcjBBjBridge dwcjHelper = Environment.getInstance().getDwcjHelper();

    private final Object classInstance;

    private BBjThreadProxyFinishedEventSink finishedEventThreadProxy;
    private BBjThreadProxyUpdateEventSink updateEventThreadProxy;

    public Object getBBjThreadInstance() {
        return classInstance;
    }

    private BBjThreadProxy(){
            classInstance=null;
    }

    public BBjThreadProxy(String className) {
        this.classInstance =  dwcjHelper.createInstance(className);
    }

    public void setValue(String key,  Object value) {
        ArrayList<Object> arglist = new ArrayList<>();
        arglist.add(key);
        arglist.add(value);
        dwcjHelper.invokeMethod(classInstance,"setValue",arglist);
    }

    public Object getValue(String key) {
        ArrayList<Object> arglist = new ArrayList<>();
        arglist.add(key);
        return dwcjHelper.invokeMethod(classInstance, "getValue", arglist);
    }

    public void onFinished(Consumer<BBjThreadProxy> c) {

        if (this.finishedEventThreadProxy == null) {
            this.finishedEventThreadProxy = new BBjThreadProxyFinishedEventSink(this);
        }

        this.finishedEventThreadProxy.addCallback(c);
    }


    public void onUpdate(Consumer<BBjThreadProxy> c) {

        if (this.updateEventThreadProxy == null) {
            this.updateEventThreadProxy = new BBjThreadProxyUpdateEventSink(this);
        }

        this.updateEventThreadProxy.addCallback(c);
    }


    public void start() {
        dwcjHelper.invokeMethod(classInstance,"start",null);
    }

    public void kill() {
        dwcjHelper.invokeMethod(classInstance,"kill",null);
    }

}
