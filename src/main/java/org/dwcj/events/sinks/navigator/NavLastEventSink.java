package org.dwcj.events.sinks.navigator;

import com.basis.bbj.proxies.event.BBjNavigatorMoveLastEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.Navigator;
import org.dwcj.events.navigator.NavigatorLastEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class NavLastEventSink {

    private ArrayList<Consumer<NavigatorLastEvent>> targets;

    private final Navigator navigator;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public NavLastEventSink(Navigator ng, Consumer<NavigatorLastEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.navigator = ng;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navLastEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void navLastEvent(BBjNavigatorMoveLastEvent ev) {
        NavigatorLastEvent dwc_ev = new NavigatorLastEvent(this.navigator);
        Iterator<Consumer<NavigatorLastEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<NavigatorLastEvent> callback) { targets.add(callback); }
}
