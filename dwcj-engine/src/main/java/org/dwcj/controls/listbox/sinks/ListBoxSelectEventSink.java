package org.dwcj.controls.listbox.sinks;

import com.basis.bbj.proxies.event.BBjListClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.listbox.ListBox;
import org.dwcj.controls.listbox.events.ListBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class ListBoxSelectEventSink {

    private final ArrayList<Consumer<ListBoxSelectEvent>> targets;

    private final ListBox listBox;

    @SuppressWarnings({"static-access"})
    public ListBoxSelectEventSink(ListBox listBox) {
        this.targets = new ArrayList<>();
        this.listBox = listBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(listBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public ListBoxSelectEventSink(ListBox listBox, Consumer<ListBoxSelectEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.listBox = listBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(listBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java:S1172")
    public void selectEvent(BBjListClickEvent ev) { //NOSONAR
        ListBoxSelectEvent dwcEv = new ListBoxSelectEvent(this.listBox);
        Iterator<Consumer<ListBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void doSelect(Object key) {
        ListBoxSelectEvent dwcEv = new ListBoxSelectEvent(listBox);
        dwcEv.addKey(key);
        Iterator<Consumer<ListBoxSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ListBoxSelectEvent> callback){
        targets.add(callback);
    }
}
