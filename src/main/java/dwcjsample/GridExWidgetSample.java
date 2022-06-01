package dwcjsample;

import com.basiscomponents.db.DataRow;
import com.basiscomponents.db.ResultSet;
import org.dwcj.App;
import org.dwcj.bbjplugins.BBjGridExWidget;
import org.dwcj.controls.Label;
import org.dwcj.events.BBjGridExWidgetSelectEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;

import java.text.ParseException;
import java.util.Iterator;

public class GridExWidgetSample extends App {
    @Override
    public void run() throws DwcAppInitializeException {
        AppPanel p = new AppPanel();

        Label headline = new Label("<html><h2>Grid Demo</h2>");
        p.add(headline);

        BBjGridExWidget grid = new BBjGridExWidget();
        p.add(grid);

        grid.setStyle("height", "calc( 100vh - 100px )");
        grid.setStyle("width", "100%");

        ResultSet rs = getData();
        grid.setData(rs);

        Label headline2 = new Label("Some Footer");
        p.add(headline2);

        grid.onSelect(this::onGridSelect);
    }

    private ResultSet getData() {
        ResultSet rs = new ResultSet();

        try {
            DataRow dr = new DataRow();
            dr.setFieldValue("Hello", "Hello");
            dr.setFieldValue("World", "World");
            rs.add(dr);

            dr = new DataRow();
            dr.setFieldValue("Hello", "Hallo");
            dr.setFieldValue("World", "Welt");
            rs.add(dr);

            dr = new DataRow();
            dr.setFieldValue("Hello", "Holà");
            dr.setFieldValue("World", "Mundo");
            rs.add(dr);

        } catch (ParseException e) {
            e.printStackTrace();
        }
        return rs;
    }

    private void onGridSelect(BBjGridExWidgetSelectEvent BBjGridExWidgetSelectEvent) {
        ResultSet sel = BBjGridExWidgetSelectEvent.getSelection();
        Iterator<DataRow> it = sel.iterator();
        while (it.hasNext()) {
            DataRow rec = it.next();
            App.msgbox(rec.toString(), 0, "Selection in Grid");
        }

    }
}
