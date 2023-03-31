package org.dwcj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjScriptLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxyif.SysGuiEventConstants;


import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerScriptLoadedEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class HtmlContainerScriptLoadedEventSink {

    private ArrayList<Consumer<HtmlContainerScriptLoadedEvent>> targets;
    private final HtmlContainer htmlContainer;

    public HtmlContainerScriptLoadedEventSink(HtmlContainer container) {

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
        HtmlContainerScriptLoadedEvent dwcEv = new HtmlContainerScriptLoadedEvent(this.htmlContainer);
        Iterator<Consumer<HtmlContainerScriptLoadedEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }
    
    public void addCallback(Consumer<HtmlContainerScriptLoadedEvent> callback) {
        targets.add(callback);
    }
}
