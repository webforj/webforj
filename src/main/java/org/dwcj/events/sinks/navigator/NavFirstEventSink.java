package org.dwcj.events.sinks.navigator;

import com.basis.bbj.proxies.event.BBjNavigatorMoveFirstEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.Navigator;
import org.dwcj.events.navigator.NavigatorFirstEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class NavFirstEventSink {

    private ArrayList<Consumer<NavigatorFirstEvent>> targets;

    private final Navigator navigator;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public NavFirstEventSink(Navigator ng, Consumer<NavigatorFirstEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.navigator = ng;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(ng);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NAV_FIRST,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"navFirstEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl = bbjctrl;
    }

    public void navFirstEvent(BBjNavigatorMoveFirstEvent ev) {
        NavigatorFirstEvent dwc_ev = new NavigatorFirstEvent(this.navigator);
        Iterator<Consumer<NavigatorFirstEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<NavigatorFirstEvent> callback) { targets.add(callback); }
}
