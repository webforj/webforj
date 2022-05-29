package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.Button;
import org.dwcj.events.ButtonPushEvent;

import java.util.function.Consumer;

public final class BBjButtonPushEventSink {

    private final Consumer<ButtonPushEvent> target;
    private final Button button;
    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjButtonPushEventSink(Button btn, Consumer<ButtonPushEvent> target) {
        this.target = target;
        this.button = btn;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(btn);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");

        } catch (Exception e) {
            e.printStackTrace();
        }

        this.ctrl = bbjctrl;
    }

    public void onEvent(BBjButtonPushEvent ev) {
        ButtonPushEvent dwc_ev = new ButtonPushEvent(button);
        target.accept(dwc_ev);
    }


}
