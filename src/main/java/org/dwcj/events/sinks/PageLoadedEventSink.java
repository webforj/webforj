package org.dwcj.events.sinks;

import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.HtmlContainer;
import org.dwcj.events.PageLoadedEvent;

import java.util.function.Consumer;

public final class PageLoadedEventSink {

    private final Consumer<PageLoadedEvent> target;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public PageLoadedEventSink(HtmlContainer htmlv, Consumer<PageLoadedEvent> target) {
        this.target = target;

        BBjControl bbjctrl = null;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.container = htmlv;

    }

    public void onEvent(BBjPageLoadedEvent ev) { //NOSONAR
        PageLoadedEvent dwcEv = new PageLoadedEvent(container);
        target.accept(dwcEv);
    }


}
