package org.dwcj.bbjplugins.thread;

import com.basis.bbj.proxies.event.BBjCustomEvent;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;

import java.util.ArrayList;
import java.util.function.Consumer;

public class BBjThreadProxy {

    private final IDwcjBBjBridge dwcjHelper = Environment.getInstance().getDwcjHelper();

    private final Object classInstance;
    private final ArrayList<Consumer<BBjThreadProxy>> finishedConsumerList = new ArrayList<>();

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
        if (finishedConsumerList.isEmpty()){
            ArrayList<Object> arglist = new ArrayList<>();
            arglist.add(12350);
            arglist.add(  Environment.getInstance().getDwcjHelper().getEventProxy(this, "finishedEvent"));
            arglist.add( "onEvent");
            dwcjHelper.invokeMethod(classInstance,"setCallback",arglist);
        }
        finishedConsumerList.add(c);
    }

    public void finishedEvent(BBjCustomEvent ev){//NOSONAR
        for (Consumer<BBjThreadProxy> consumer : finishedConsumerList) {
            consumer.accept(this);
        }
    }

    public void start() {
        dwcjHelper.invokeMethod(classInstance,"start",null);
    }
}
