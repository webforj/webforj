package org.dwcj.component.htmlcontainer.sinks;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.events.HtmlContainerJavascriptEvent;

import com.basis.bbj.proxyif.SysGuiEventConstants;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class HtmlContainerNativeJavascriptEventSink {


    private final ArrayList<Consumer<HtmlContainerJavascriptEvent>> targets;
    private final HtmlContainer container;

    public HtmlContainerNativeJavascriptEventSink(HtmlContainer htmlv) {
        this.targets = new ArrayList<>();
        this.container = htmlv;

        BBjControl bbjctrl;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(SysGuiEventConstants.ON_NATIVE_JAVASCRIPT, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void onEvent(BBjNativeJavaScriptEvent ev) { //NOSONAR
        HtmlContainerJavascriptEvent dwcEv;
        try {
            dwcEv = new HtmlContainerJavascriptEvent(container, ev.getEventMap());
        } catch (BBjException e) {
            dwcEv = new HtmlContainerJavascriptEvent(container, null);
            App.consoleError("Error: could not determine JS event map!");
        }
        Iterator<Consumer<HtmlContainerJavascriptEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<HtmlContainerJavascriptEvent> callback) {
        targets.add(callback);
    }
}
