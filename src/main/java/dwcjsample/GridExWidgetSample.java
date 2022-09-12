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

    public static final String HELLO = "Hello";
    public static final String WORLD = "World";

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
            dr.setFieldValue(HELLO, HELLO);
            dr.setFieldValue(WORLD, WORLD);
            rs.add(dr);

            dr = new DataRow();
            dr.setFieldValue(HELLO, "Hallo");
            dr.setFieldValue(WORLD, "Welt");
            rs.add(dr);

            dr = new DataRow();
            dr.setFieldValue(HELLO, "Holà");
            dr.setFieldValue(WORLD, "Mundo");
            rs.add(dr);

        } catch (ParseException e) {
            e.printStackTrace();
        }
        return rs;
    }

    private void onGridSelect(BBjGridExWidgetSelectEvent bbjGridExWidgetSelectEvent) {
        ResultSet sel = bbjGridExWidgetSelectEvent.getSelection();
        Iterator<DataRow> it = sel.iterator();
        while (it.hasNext()) {
            DataRow rec = it.next();
            App.msgbox(rec.toString(), 0, "Selection in Grid");
        }

    }
}
