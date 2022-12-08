package org.dwcj.controls.htmlContainer.sinks;

import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.htmlContainer.HtmlContainer;
import org.dwcj.controls.htmlContainer.events.HtmlContainerPageLoadedEvent;

import java.util.function.Consumer;

public final class HtmlContainerPageLoadedEventSink {

    private final Consumer<HtmlContainerPageLoadedEvent> target;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public HtmlContainerPageLoadedEventSink(HtmlContainer htmlv, Consumer<HtmlContainerPageLoadedEvent> target) {
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
        HtmlContainerPageLoadedEvent dwcEv = new HtmlContainerPageLoadedEvent(container);
        target.accept(dwcEv);
    }


}
