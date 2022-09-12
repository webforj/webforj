package org.dwcj.events.sinks;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.BBjGridExWidget;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.events.BBjGridExWidgetSelectEvent;

import java.util.function.Consumer;

public final class BBjGridExWidgetSelectEventSink {

    private final Consumer<BBjGridExWidgetSelectEvent> target;
    private final BBjGridExWidget grid;

    @SuppressWarnings({"static-access"})
    public BBjGridExWidgetSelectEventSink(BBjGridExWidget grid, Consumer<BBjGridExWidgetSelectEvent> target) {
        this.target = target;
        this.grid = grid;

        BBjControl bbjctrl = null;
        try {
            bbjctrl=ControlAccessor.getDefault().getBBjControl(grid);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_GRID_SELECT_ROW, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent", "::BBjGridExWidgetSelectEventProxy.bbj::BBjGridExWidgetSelectEventProxy"), "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void onEvent(String eventString) {
        BBjGridExWidgetSelectEvent dwcEv = new BBjGridExWidgetSelectEvent(grid, eventString);
        target.accept(dwcEv);
    }

}
