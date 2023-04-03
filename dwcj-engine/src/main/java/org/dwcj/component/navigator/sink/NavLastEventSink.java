package org.dwcj.component.navigator.sink;

import com.basis.bbj.proxies.event.BBjNavigatorMoveLastEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.event.NavigatorLastEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class NavLastEventSink {

    private ArrayList<Consumer<NavigatorLastEvent>> targets;

    private final Navigator navigator;


    @SuppressWarnings({"static-access"})
    public NavLastEventSink(Navigator ng, Consumer<NavigatorLastEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.navigator = ng;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navLastEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
        
    }

    public void navLastEvent(BBjNavigatorMoveLastEvent ev) { //NOSONAR
        NavigatorLastEvent dwcEv = new NavigatorLastEvent(this.navigator);
        Iterator<Consumer<NavigatorLastEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<NavigatorLastEvent> callback) { targets.add(callback); }
}
