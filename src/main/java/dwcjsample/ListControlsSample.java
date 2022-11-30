package dwcjsample;

import org.dwcj.App;
import org.dwcj.controls.ComboBox;
import org.dwcj.controls.ListBox;
import org.dwcj.controls.TextComboBox;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;

public class ListControlsSample extends App {
    @Override
    public void run() throws DwcAppInitializeException {
        AppPanel ap = new AppPanel();
        ap.setStyle("display","grid");
        ap.setStyle("gap","20px");
        ap.setStyle("padding-left","calc( 50vw - 100px )");
        ap.setStyle("padding-right","calc( 50vw - 100px )");

        ComboBox cb = new ComboBox();
        cb.addItem(1,"One");
        cb.addItem(2,"Two");
        cb.addItem(3,"Three");
        ap.add(cb);
        cb.addItem(4,"Four");
        cb.addItem(5,"Five");
        cb.addItem(6,"Six");
        cb.addItem(7,"Seven");


        ListBox lb = new ListBox();
        lb.addItem(1,"One");
        lb.addItem(2,"Two");
        lb.addItem(3,"Three");
        ap.add(lb);
        lb.addItem(4,"Four");
        lb.addItem(5,"Five");
        lb.addItem(6,"Six");
        lb.addItem(7,"Seven");

        TextComboBox tcb = new TextComboBox();
        tcb.addItem(1,"One");
        tcb.addItem(2,"Two");
        tcb.addItem(3,"Three");
        ap.add(tcb);
        tcb.addItem(4,"Four");
        tcb.addItem(5,"Five");
        tcb.addItem(6,"Six");
        tcb.addItem(7,"Seven");


    }
}
