package org.dwcj.events;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;

import java.util.function.Consumer;

public class BBjControlEventSink {

    private final int eventType;
    private final Consumer<IDwcEvent> target;

    public BBjControlEventSink(BBjControl ctrl, int event, Consumer<IDwcEvent> target) {
        this.eventType = event;
        this.target = target;

        try {
            ctrl.setCallback(eventType, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void onEvent(BBjEvent ev) {
        App.consoleLog("onEvent generic");
        target.accept(null);
    }


}
