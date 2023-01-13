package org.dwcj.controls.htmlcontainer.sinks;

import com.basis.bbj.proxies.event.BBjPageLoadedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.htmlcontainer.events.HtmlContainerPageLoadedEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class HtmlContainerPageLoadedEventSink {

    private final ArrayList<Consumer<HtmlContainerPageLoadedEvent>> targets;
    private final HtmlContainer container;

    @SuppressWarnings({"static-access"})
    public HtmlContainerPageLoadedEventSink(HtmlContainer htmlv) {
        this.targets = new ArrayList<>();
        this.container = htmlv;

        BBjControl bbjctrl = null;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(htmlv);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_PAGE_LOADED, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"), "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }

    }

    public void onEvent(BBjPageLoadedEvent ev) { //NOSONAR
        HtmlContainerPageLoadedEvent dwcEv = new HtmlContainerPageLoadedEvent(container);
        Iterator<Consumer<HtmlContainerPageLoadedEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<HtmlContainerPageLoadedEvent> callback) {
        targets.add(callback);
    }


}