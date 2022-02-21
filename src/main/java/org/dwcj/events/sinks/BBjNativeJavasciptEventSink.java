package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjNativeJavaScriptEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.controls.HtmlContainer;
import org.dwcj.events.JavascriptEvent;

import java.util.function.Consumer;

public class BBjNativeJavasciptEventSink {


    private final Consumer<JavascriptEvent> target;
    private final BBjControl ctrl;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public BBjNativeJavasciptEventSink(HtmlContainer htmlv, Consumer<JavascriptEvent> target) {
        this.target = target;
        this.ctrl = htmlv.getControl();
        this.container = htmlv;

        try {
            ctrl.setCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void onEvent(BBjNativeJavaScriptEvent ev) {
        JavascriptEvent dwc_ev = new JavascriptEvent(container);
        target.accept(dwc_ev);
    }
}
