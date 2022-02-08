package org.dwcj.events;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

import java.util.function.Consumer;

public class BBjButtonPushEventSink {

    private final Consumer<ButtonPushEvent> target;

    @SuppressWarnings({"static-access"})
    public BBjButtonPushEventSink(BBjControl ctrl, Consumer<ButtonPushEvent> target) {
        this.target = target;

        try {
            ctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void onEvent(BBjButtonPushEvent ev) {
        ButtonPushEvent dwc_ev = new ButtonPushEvent();
        target.accept(dwc_ev);
    }


}
