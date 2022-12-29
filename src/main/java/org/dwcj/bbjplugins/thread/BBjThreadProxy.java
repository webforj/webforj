package org.dwcj.bbjplugins.thread;

import org.dwcj.Environment;
import org.dwcj.bbjplugins.thread.sinks.BBjThreadProxyFinishedEventSink;
import org.dwcj.bbjplugins.thread.sinks.BBjThreadProxyUpdateEventSink;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * BBjThreadProxy, the Java-side interface to the BBj Thread Plug-In
 * @see <a href="https://github.com/BBj-Plugins/BBjThread">BBjThread Plugin</a>
 */
public final class BBjThreadProxy {

    private final IDwcjBBjBridge dwcjHelper = Environment.getInstance().getDwcjHelper();

    private final Object classInstance;

    private BBjThreadProxyFinishedEventSink finishedEventThreadProxy;
    private BBjThreadProxyUpdateEventSink updateEventThreadProxy;

    /**
     * Allows direct access to the underlying class that extends BBjThread
     * (This is probably most often not useful to the Java developer, only in some cases.)
     * @return the instance of the BBjThread Object (BBj Object!)
     */
    public Object getBBjThreadInstance() {
        return classInstance;
    }

    private BBjThreadProxy(){
            classInstance=null;
    }

    /**
     * Create a BBjThread class.
     * You will typically pass a BBj class name in the form
     * new BBjThreadProxy("::my/path/to/the/ThreadClass.bbj::ThreadClass")
     * where ThreadClass extends BBjThread and implements the BBj side logic
     * @param className: the BBj class name
     */
    public BBjThreadProxy(String className) {
        this.classInstance =  dwcjHelper.createInstance(className);
    }

    /**
     * Set a value to be prepared and/or exchanged with the background process
     * @param key A String that serves as a key to the parameters map
     * @param value An Object to exchange
     */
    public void setValue(String key,  Object value) {
        ArrayList<Object> arglist = new ArrayList<>();
        arglist.add(key);
        arglist.add(value);
        dwcjHelper.invokeMethod(classInstance,"setValue",arglist);
    }

    /**
     * Retrieve a value manipulated by the background thread
     * @param key A String that serves as a key to the parameters map
     * @return The object passed by the background thread
     */
    public Object getValue(String key) {
        ArrayList<Object> arglist = new ArrayList<>();
        arglist.add(key);
        return dwcjHelper.invokeMethod(classInstance, "getValue", arglist);
    }

    /**
     * register an event callback that is executed as soon as the background thread finishes
     * @param consumer The callback to notify
     */
    public void onFinished(Consumer<BBjThreadProxy> consumer) {

        if (this.finishedEventThreadProxy == null) {
            this.finishedEventThreadProxy = new BBjThreadProxyFinishedEventSink(this);
        }

        this.finishedEventThreadProxy.addCallback(consumer);
    }

    /**
     * register an event callback that is executed whenever the background thread executes update()
     * @param consumer The callback to notify
     */
    public void onUpdate(Consumer<BBjThreadProxy> consumer) {

        if (this.updateEventThreadProxy == null) {
            this.updateEventThreadProxy = new BBjThreadProxyUpdateEventSink(this);
        }

        this.updateEventThreadProxy.addCallback(consumer);
    }

    /**
     * Start the background thread.
     */
    public void start() {
        dwcjHelper.invokeMethod(classInstance,"start",null);
    }

    /**
     * Kill the background thread
     */
    public void kill() {
        dwcjHelper.invokeMethod(classInstance,"kill",null);
    }

    /**
     * Notify the background thread to terminate gracefully.
     * The BBj background thread can use shouldAbort() to determine if the foreground process requested
     * its termination by using the abort() method.
     */
    public void abort() {
        dwcjHelper.invokeMethod(classInstance,"abort",null);
    }

}
