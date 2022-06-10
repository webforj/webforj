package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.events.ComboBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class BBjComboBoxSelectEventSink {

    private ArrayList<Consumer<ComboBoxSelectEvent>> targets;

    private final ComboBox comboBox;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public BBjComboBoxSelectEventSink(ComboBox cb, Consumer<ComboBoxSelectEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
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
        Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void doSelect(Object key) {
        ComboBoxSelectEvent dwc_ev = new ComboBoxSelectEvent(comboBox);
        dwc_ev.setKey(key);
        Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }
}
