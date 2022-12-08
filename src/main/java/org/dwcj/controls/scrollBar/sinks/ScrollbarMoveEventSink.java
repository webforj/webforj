package org.dwcj.controls.scrollBar.sinks;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.scrollBar.ScrollBar;
import org.dwcj.controls.scrollBar.events.ScrollbarMoveEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ScrollbarMoveEventSink {

    private ArrayList<Consumer<ScrollbarMoveEvent>> targets;

    private final ScrollBar scrollBar;


    @SuppressWarnings({"static-access"})
    public ScrollbarMoveEventSink(ScrollBar sb) {
        this.targets = new ArrayList<>();
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


    @SuppressWarnings({"static-access"})
    public ScrollbarMoveEventSink(ScrollBar sb, Consumer<ScrollbarMoveEvent> callback) {
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
