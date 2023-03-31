package org.dwcj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjNavigatorMoveNextEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.event.NavigatorNextEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class NavNextEventSink {

    private ArrayList<Consumer<NavigatorNextEvent>> targets;

    private final Navigator navigator;

    @SuppressWarnings({"static-access"})
    public NavNextEventSink(Navigator ng, Consumer<NavigatorNextEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.navigator = ng;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navNextEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
        
    }

    public void navNextEvent(BBjNavigatorMoveNextEvent ev) { //NOSONAR
        NavigatorNextEvent dwcEv = new NavigatorNextEvent(this.navigator);
        Iterator<Consumer<NavigatorNextEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<NavigatorNextEvent> callback) { targets.add(callback); }
}
