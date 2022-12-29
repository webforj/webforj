package org.dwcj.controls.htmlcontainer.sinks;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerJavascriptEvent;

import java.util.function.Consumer;

public final class HtmlContainerNativeJavascriptEventSink {


    private final Consumer<HtmlContainerJavascriptEvent> target;
    private final HtmlContainer container;

    public HtmlContainerNativeJavascriptEventSink(HtmlContainer htmlv, Consumer<HtmlContainerJavascriptEvent> target) {
        this.target = target;
        this.container = htmlv;

        BBjControl bbjctrl;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NATIVE_JAVASCRIPT, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
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
        target.accept(dwcEv);
    }
}
