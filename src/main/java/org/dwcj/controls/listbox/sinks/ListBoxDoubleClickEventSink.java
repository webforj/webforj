package org.dwcj.controls.listbox.sinks;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.listbox.ListBox;
import org.dwcj.controls.listbox.events.ListBoxDoubleClickEvent;

import com.basis.bbj.proxies.event.BBjListDoubleClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;

public class ListBoxDoubleClickEventSink {

    private final ArrayList<Consumer<ListBoxDoubleClickEvent>> targets;
    private final ListBox listBox;

    @SuppressWarnings({"static-access"})
    public ListBoxDoubleClickEventSink(ListBox listBox) {
        this.targets = new ArrayList<>();
        this.listBox = listBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(listBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_DOUBLE_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings({"static-access"})
    public ListBoxDoubleClickEventSink(ListBox listBox, Consumer<ListBoxDoubleClickEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.listBox = listBox;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(listBox);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_DOUBLE_CLICK,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java:S1172")
    public void doubleClickEvent(BBjListDoubleClickEvent ev) { //NOSONAR
        ListBoxDoubleClickEvent dwcEv = new ListBoxDoubleClickEvent(this.listBox);
        Iterator<Consumer<ListBoxDoubleClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void doDoubleClick(Object key) {
        ListBoxDoubleClickEvent dwcEv = new ListBoxDoubleClickEvent(listBox);
        dwcEv.addKey(key);
        Iterator<Consumer<ListBoxDoubleClickEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ListBoxDoubleClickEvent> callback){
        targets.add(callback);
    }
    
}
