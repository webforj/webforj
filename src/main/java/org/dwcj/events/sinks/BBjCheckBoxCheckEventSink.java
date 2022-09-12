package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjCheckOffEvent;
import com.basis.bbj.proxies.event.BBjCheckOnEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.CheckBox;
import org.dwcj.events.CheckBoxCheckEvent;

import java.util.function.Consumer;

public final class BBjCheckBoxCheckEventSink {

    private final Consumer<CheckBoxCheckEvent> target;

    private final CheckBox checkBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public BBjCheckBoxCheckEventSink(CheckBox cb, Consumer<CheckBoxCheckEvent> target) {
        this.target = target;
        this.checkBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                    "onEvent");
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java:S1172")
    public void checkOffEvent(BBjCheckOffEvent ev) { //NOSONAR
        CheckBoxCheckEvent dwcEv = new CheckBoxCheckEvent(this.checkBox);
        target.accept(dwcEv);
    }

    @SuppressWarnings("java:S1172")
    public void checkOnEvent(BBjCheckOnEvent ev) { //NOSONAR
        CheckBoxCheckEvent dwcEv = new CheckBoxCheckEvent(this.checkBox);
        target.accept(dwcEv);
    }

}
