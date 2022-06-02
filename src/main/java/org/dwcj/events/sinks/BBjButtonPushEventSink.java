package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.Button;
import org.dwcj.events.ButtonPushEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class BBjButtonPushEventSink {

    private ArrayList<Consumer<ButtonPushEvent>> targets;
    private final Button button;
    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjButtonPushEventSink(Button btn, Consumer<ButtonPushEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.button = btn;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(btn);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }

        this.ctrl = bbjctrl;
    }

    public void pushEvent(BBjButtonPushEvent ev) {
        ButtonPushEvent dwc_ev = new ButtonPushEvent(this.button);
        Iterator<Consumer<ButtonPushEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        ButtonPushEvent dwc_ev = new ButtonPushEvent(button);
        Iterator<Consumer<ButtonPushEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<ButtonPushEvent> callback) {
        targets.add(callback);
    }
}
