package org.dwcj.events;

import com.basiscomponents.db.ResultSet;
import org.dwcj.bbjplugins.BBjGridExWidget;
import org.dwcj.controls.AbstractDwcControl;

import java.io.IOException;
import java.text.ParseException;

public final class BBjGridExWidgetSelectEvent implements IDwcEvent {

    private final BBjGridExWidget control;
    private final ResultSet selection;

    public BBjGridExWidgetSelectEvent(BBjGridExWidget theGrid, String eventString) {
        ResultSet selection_tmp = null;
        this.control = theGrid;
        try {
            selection_tmp = ResultSet.fromJson(eventString);
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ParseException e) {
            e.printStackTrace();
        }
        selection = selection_tmp;
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
