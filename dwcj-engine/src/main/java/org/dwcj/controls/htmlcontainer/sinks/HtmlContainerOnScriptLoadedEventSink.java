package org.dwcj.controls.htmlcontainer.sinks;

import com.basis.bbj.proxies.event.BBjScriptLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;


import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerOnScriptLoadedEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class HtmlContainerOnScriptLoadedEventSink {

    private ArrayList<Consumer<HtmlContainerOnScriptLoadedEvent>> targets;
    private final HtmlContainer htmlContainer;

    public HtmlContainerOnScriptLoadedEventSink(HtmlContainer container) {

        this.targets = new ArrayList<>();
        this.htmlContainer = container;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(container);
            bbjctrl.setCallback(SysGuiEventConstants.ON_SCRIPT_LOADED,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "scriptLoadedEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void scriptLoadedEvent(BBjScriptLoadedEvent ev) { // NOSONAR
        HtmlContainerOnScriptLoadedEvent dwcEv = new HtmlContainerOnScriptLoadedEvent(this.htmlContainer);
        Iterator<Consumer<HtmlContainerOnScriptLoadedEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }
    
    public void addCallback(Consumer<HtmlContainerOnScriptLoadedEvent> callback) {
        targets.add(callback);
    }
}
