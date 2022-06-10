package org.dwcj.events.sinks.listBox;

import com.basis.bbj.proxies.event.BBjListClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ListBox;
import org.dwcj.events.listBox.ListBoxSelectEvent;

import java.util.function.Consumer;

public class BBjListBoxSelectEventSink {

    private final Consumer<ListBoxSelectEvent> target;

    private final ListBox listBox;

    private final BBjControl ctrl;

    public BBjListBoxSelectEventSink(ListBox listBox, Consumer<ListBoxSelectEvent> target) {
        this.target = target;
        this.listBox = listBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(listBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void selectEvent(BBjListClickEvent ev) {
        ListBoxSelectEvent dwc_ev = new ListBoxSelectEvent(this.listBox);
        target.accept(dwc_ev);
    }
}
