package org.dwcj.bbjplugins.gridexwidget.sinks;

import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bbjplugins.gridexwidget.GridExWidget;
import org.dwcj.bbjplugins.gridexwidget.events.GridExWidgetSelectEvent;
import org.dwcj.bridge.ControlAccessor;

import java.util.function.Consumer;

public final class GridExWidgetSelectEventSink {

    private final Consumer<GridExWidgetSelectEvent> target;
    private final GridExWidget grid;

    @SuppressWarnings({"static-access"})
    public GridExWidgetSelectEventSink(GridExWidget grid, Consumer<GridExWidgetSelectEvent> target) {
        this.target = target;
        this.grid = grid;

        BBjControl bbjctrl = null;
        try {
            bbjctrl=ControlAccessor.getDefault().getBBjControl(grid);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_GRID_SELECT_ROW, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent", "::BBjGridExWidgetEventProxies.bbj::BBjGridExWidgetSelectEventProxy"), "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public void onEvent(String eventString) {
        GridExWidgetSelectEvent dwcEv = new GridExWidgetSelectEvent(grid, eventString);
        target.accept(dwcEv);
    }

}
