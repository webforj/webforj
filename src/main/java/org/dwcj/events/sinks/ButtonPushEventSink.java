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

public final class ButtonPushEventSink {

    private ArrayList<Consumer<ButtonPushEvent>> targets;
    private final Button button;

    @SuppressWarnings({"static-access"})
    public ButtonPushEventSink(Button btn) {

        this.targets = new ArrayList<>();
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
    }

    public ButtonPushEventSink(Button btn, Consumer<ButtonPushEvent> callback) {

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
    }

    public void pushEvent(BBjButtonPushEvent ev) { // NOSONAR
        ButtonPushEvent dwcEv = new ButtonPushEvent(this.button);
        Iterator<Consumer<ButtonPushEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        ButtonPushEvent dwcEv = new ButtonPushEvent(button);
        Iterator<Consumer<ButtonPushEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ButtonPushEvent> callback) {
        targets.add(callback);
    }
}
