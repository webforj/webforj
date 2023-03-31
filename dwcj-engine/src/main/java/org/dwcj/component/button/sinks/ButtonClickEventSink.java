package org.dwcj.component.button.sinks;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.events.ButtonClickEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class ButtonClickEventSink {

    private ArrayList<Consumer<ButtonClickEvent>> targets;
    private final Button button;

    @SuppressWarnings({"static-access"})
    public ButtonClickEventSink(Button btn) {

        this.targets = new ArrayList<>();
        this.button = btn;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(btn);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public ButtonClickEventSink(Button btn, Consumer<ButtonClickEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.button = btn;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(btn);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void pushEvent(BBjButtonPushEvent ev) { // NOSONAR
        ButtonClickEvent dwcEv = new ButtonClickEvent(this.button);
        Iterator<Consumer<ButtonClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    /**
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        ButtonClickEvent dwcEv = new ButtonClickEvent(button);
        Iterator<Consumer<ButtonClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ButtonClickEvent> callback) {
        targets.add(callback);
    }
}
