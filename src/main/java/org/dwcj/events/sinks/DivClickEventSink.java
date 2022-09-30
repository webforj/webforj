package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjMouseDownEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.events.DivClickEvent;
import org.dwcj.panels.Div;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class DivClickEventSink {

    private ArrayList<Consumer<DivClickEvent>> targets;
    private final Div div;

    @SuppressWarnings({"static-access"})
    public DivClickEventSink(Div div, Consumer<DivClickEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.div = div;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(div);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_MOUSE_DOWN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    @SuppressWarnings("java:S1172")
    public void pushEvent(BBjMouseDownEvent ev) { //NOSONAR
        DivClickEvent dwcEv = new DivClickEvent(this.div);
        Iterator<Consumer<DivClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        DivClickEvent dwcEv = new DivClickEvent(div);
        Iterator<Consumer<DivClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<DivClickEvent> callback) {
        targets.add(callback);
    }
}
