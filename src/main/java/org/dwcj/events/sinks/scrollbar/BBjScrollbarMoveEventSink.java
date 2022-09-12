package org.dwcj.events.sinks.scrollbar;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ScrollBar;
import org.dwcj.events.ScrollbarMoveEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class BBjScrollbarMoveEventSink {

    private ArrayList<Consumer<ScrollbarMoveEvent>> targets;

    private final ScrollBar scrollBar;


    @SuppressWarnings({"static-access"})
    public BBjScrollbarMoveEventSink(ScrollBar sb, Consumer<ScrollbarMoveEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.scrollBar = sb;

        BBjControl bbjctrl = null;
        try{
            bbjctrl = ControlAccessor.getDefault().getBBjControl(sb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CONTROL_SCROLL,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"scrollEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void scrollEvent(BBjControlScrollEvent ev) { //NOSONAR
        ScrollbarMoveEvent dwcEv = new ScrollbarMoveEvent(this.scrollBar);
        Iterator<Consumer<ScrollbarMoveEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ScrollbarMoveEvent> callback) { targets.add(callback); }
}
