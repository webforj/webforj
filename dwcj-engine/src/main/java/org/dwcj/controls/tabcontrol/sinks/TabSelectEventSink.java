package org.dwcj.controls.tabcontrol.sinks;


import com.basis.bbj.proxies.event.BBjTabSelectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.tabcontrol.TabControl;
import org.dwcj.controls.tabcontrol.events.TabSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class TabSelectEventSink {

    private ArrayList<Consumer<TabSelectEvent>> targets;
    private final TabControl tabControl;

    @SuppressWarnings({"static-access"})
    public TabSelectEventSink(TabControl btn) {

        this.targets = new ArrayList<>();
        this.tabControl = btn;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(btn);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_TAB_SELECT, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public TabSelectEventSink(TabControl tabControl, Consumer<TabSelectEvent> callback) {

        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.tabControl = tabControl;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(tabControl);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_BUTTON_PUSH, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "pushEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void pushEvent(BBjTabSelectedEvent ev) { // NOSONAR
        TabSelectEvent dwcEv = null;
        try {
            dwcEv = new TabSelectEvent(this.tabControl, ev.getIndex(), ev.getTitle());
        } catch (BBjException e) {
            Environment.logError(e);
        }
        Iterator<Consumer<TabSelectEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }


    public void addCallback(Consumer<TabSelectEvent> callback) {
        targets.add(callback);
    }
}
