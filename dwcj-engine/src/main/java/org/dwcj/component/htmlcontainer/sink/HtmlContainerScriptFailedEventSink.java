package org.dwcj.component.htmlcontainer.sink;

import com.basis.bbj.proxies.event.BBjScriptFailedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerScriptFailedEvent;

import com.basis.bbj.proxyif.SysGuiEventConstants;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class HtmlContainerScriptFailedEventSink {
    
    private ArrayList<Consumer<HtmlContainerScriptFailedEvent>> targets;
    private final HtmlContainer htmlContainer;

    public HtmlContainerScriptFailedEventSink(HtmlContainer container) {

        this.targets = new ArrayList<>();
        this.htmlContainer = container;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(container);
            bbjctrl.setCallback(SysGuiEventConstants.ON_SCRIPT_FAILED,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "scriptFailedEvent"),
                    "onEvent");

        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void scriptFailedEvent(BBjScriptFailedEvent ev) { // NOSONAR
        HtmlContainerScriptFailedEvent dwcEv = new HtmlContainerScriptFailedEvent(this.htmlContainer);
        Iterator<Consumer<HtmlContainerScriptFailedEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }
    
    public void addCallback(Consumer<HtmlContainerScriptFailedEvent> callback) {
        targets.add(callback);
    }

}
