package org.dwcj.component.window.sink;

import com.basis.bbj.proxies.event.BBjMouseDownEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.window.Panel;
import org.dwcj.component.window.event.WindowClickEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class WindowClickEventSink {

    private ArrayList<Consumer<WindowClickEvent>> targets;
    private final Panel div;

    @SuppressWarnings({"static-access"})
    public WindowClickEventSink(Panel div, Consumer<WindowClickEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.div = div;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(div);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_MOUSE_DOWN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }
    
    public WindowClickEventSink(Panel div) {
        this.targets = new ArrayList<>();
        this.div = div;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(div);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_MOUSE_DOWN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java:S1172")
    public void pushEvent(BBjMouseDownEvent ev) { //NOSONAR
        WindowClickEvent dwcEv = new WindowClickEvent(this.div);
        Iterator<Consumer<WindowClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        WindowClickEvent dwcEv = new WindowClickEvent(div);
        Iterator<Consumer<WindowClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<WindowClickEvent> callback) {
        targets.add(callback);
    }
}
