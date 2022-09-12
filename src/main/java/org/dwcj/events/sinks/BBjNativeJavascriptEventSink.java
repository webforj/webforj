package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.HtmlContainer;
import org.dwcj.events.JavascriptEvent;

import java.util.function.Consumer;

public final class BBjNativeJavascriptEventSink {


    private final Consumer<JavascriptEvent> target;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public BBjNativeJavascriptEventSink(HtmlContainer htmlv, Consumer<JavascriptEvent> target) {
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
        JavascriptEvent dwcEv = new JavascriptEvent(container);
        target.accept(dwcEv);
    }
}
