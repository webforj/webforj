package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjMouseDownEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.Label;
import org.dwcj.events.DivClickEvent;
import org.dwcj.panels.Div;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class BBjDivClickEventSink {

    private ArrayList<Consumer<DivClickEvent>> targets;
    private final Div div;
    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjDivClickEventSink(Div div, Consumer<DivClickEvent> callback) {
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
        this.ctrl = bbjctrl;
    }

    public void pushEvent(BBjMouseDownEvent ev) {
        DivClickEvent dwc_ev = new DivClickEvent(this.div);
        Iterator<Consumer<DivClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        DivClickEvent dwc_ev = new DivClickEvent(div);
        Iterator<Consumer<DivClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<DivClickEvent> callback) {
        targets.add(callback);
    }
}
