package org.dwcj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjNavigatorMovePreviousEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.event.NavigatorPreviousEvent;

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
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navPreviousEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
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
