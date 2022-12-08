package org.dwcj.controls.navigator.sinks;

import com.basis.bbj.proxies.event.BBjNavigatorMovePreviousEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.navigator.Navigator;
import org.dwcj.controls.navigator.events.NavigatorPreviousEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class NavPreviousEventSink {

    private ArrayList<Consumer<NavigatorPreviousEvent>> targets;

    private final Navigator navigator;

    @SuppressWarnings({"static-access"})
    public NavPreviousEventSink(Navigator ng, Consumer<NavigatorPreviousEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.navigator = ng;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navPreviousEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    public void navPreviousEvent(BBjNavigatorMovePreviousEvent ev) { //NOSONAR
        NavigatorPreviousEvent dwcEv = new NavigatorPreviousEvent(this.navigator);
        Iterator<Consumer<NavigatorPreviousEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<NavigatorPreviousEvent> callback) { targets.add(callback); }
}
