package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.HtmlContainer;
import org.dwcj.events.PageLoadedEvent;

import java.util.function.Consumer;

public final class BBjPageLoadedEventSink {

    private final Consumer<PageLoadedEvent> target;
    private final BBjControl ctrl;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public BBjPageLoadedEventSink(HtmlContainer htmlv, Consumer<PageLoadedEvent> target) {
        this.target = target;

        BBjControl bbjctrl = null;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }

        this.ctrl = bbjctrl;
        this.container = htmlv;

    }

    public void onEvent(BBjPageLoadedEvent ev) {
        PageLoadedEvent dwc_ev = new PageLoadedEvent(container);
        target.accept(dwc_ev);
    }


}
