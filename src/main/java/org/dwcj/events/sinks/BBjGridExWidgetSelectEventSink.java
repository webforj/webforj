package org.dwcj.events.sinks;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.BBjGridExWidget;
import org.dwcj.events.BBjGridExWidgetSelectEvent;

import java.util.function.Consumer;

public class BBjGridExWidgetSelectEventSink {

    private final Consumer<BBjGridExWidgetSelectEvent> target;
    private final BBjControl ctrl;
    private final BBjGridExWidget grid;

    @SuppressWarnings({"static-access"})
    public BBjGridExWidgetSelectEventSink(BBjGridExWidget grid, Consumer<BBjGridExWidgetSelectEvent> target) {
        this.target = target;
        this.grid = grid;
        this.ctrl = grid.getControl();

        try {
            ctrl.setCallback(Environment.getInstance().getBBjAPI().ON_GRID_SELECT_ROW, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent", "::BBjGridExWidgetSelectEventProxy.bbj::BBjGridExWidgetSelectEventProxy"), "onEvent");
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void onEvent(String eventString) {
        BBjGridExWidgetSelectEvent dwc_ev = new BBjGridExWidgetSelectEvent(grid, eventString);
        target.accept(dwc_ev);
    }
}
