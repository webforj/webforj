package org.dwcj.bbjplugins.gridexwidget.events;

import com.basiscomponents.db.ResultSet;

import org.dwcj.Environment;
import org.dwcj.bbjplugins.gridexwidget.GridExWidget;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.interfaces.ControlEvent;

import java.io.IOException;
import java.text.ParseException;

public final class GridExWidgetSelectEvent implements ControlEvent {

    private final GridExWidget control;
    private final ResultSet selection;

    public GridExWidgetSelectEvent(GridExWidget theGrid, String eventString) {
        ResultSet selectionTmp = null;
        this.control = theGrid;
        try {
            selectionTmp = ResultSet.fromJson(eventString);
        } catch (IOException|ParseException e) {
            Environment.logError(e);
        }
        selection = selectionTmp;
    }

    public GridExWidgetSelectEvent(GridExWidget theGrid, ResultSet selection) {
        this.control = theGrid;
        this.selection = selection;
    }

    @Override
    public AbstractDwcControl getControl() {
        return control;
    }

    /**
     * return the selection that is made effective by the user action that triggered the event
     *
     * @return the selected row(s) / records
     */
    public ResultSet getSelection() {
        return selection;
    }
}
