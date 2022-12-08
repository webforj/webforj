package org.dwcj.controls.comboBox.sinks;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.comboBox.ComboBox;
import org.dwcj.controls.comboBox.events.ComboBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class ComboBoxSelectEventSink {

    private ArrayList<Consumer<ComboBoxSelectEvent>> targets = new ArrayList<>();

    private final ComboBox comboBox;

    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public ComboBoxSelectEventSink(ComboBox cb) {
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings({"static-access"})
    public ComboBoxSelectEventSink(ComboBox cb, Consumer<ComboBoxSelectEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java.S1172")
    public void selectEvent(BBjListSelectEvent ev) { //NOSONAR
        ComboBoxSelectEvent dwcEv = new ComboBoxSelectEvent(this.comboBox);
        Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void doSelect(Object key) {
        ComboBoxSelectEvent dwcEv = new ComboBoxSelectEvent(comboBox);
        dwcEv.setKey(key);
        Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ComboBoxSelectEvent> callback) { targets.add(callback); }
}
