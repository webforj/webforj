package org.dwcj.controls.htmlcontainer.sinks;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerJavascriptEvent;

import java.util.function.Consumer;

public final class HtmlContainerNativeJavascriptEventSink {


    private final Consumer<HtmlContainerJavascriptEvent> target;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public HtmlContainerNativeJavascriptEventSink(HtmlContainer htmlv, Consumer<HtmlContainerJavascriptEvent> target) {
        this.target = target;
        this.container = htmlv;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_NATIVE_JAVASCRIPT, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void onEvent(BBjNativeJavaScriptEvent ev) { //NOSONAR
        HtmlContainerJavascriptEvent dwcEv = new HtmlContainerJavascriptEvent(container);
        target.accept(dwcEv);
    }
}
