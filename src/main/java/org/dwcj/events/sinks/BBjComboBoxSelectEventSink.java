package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.ComboBoxSelectEvent;

import java.util.function.Consumer;

public final class BBjComboBoxSelectEventSink {

    private final Consumer<ComboBoxSelectEvent> target;

    private final ComboBox comboBox;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjComboBoxSelectEventSink(ComboBox cb, Consumer<ComboBoxSelectEvent> target) {
        this.target = target;
        this.comboBox = cb;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }

        this.ctrl = bbjctrl;
    }

    public void selectEvent(BBjListSelectEvent ev) {
        ComboBoxSelectEvent dwc_ev = new ComboBoxSelectEvent(this.comboBox);
        target.accept(dwc_ev);
    }
}
