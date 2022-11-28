package org.dwcj.events.sinks.combobox;

import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.combobox.ComboBoxOpenEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class ComboBoxOpenEventSink {

    private ArrayList<Consumer<ComboBoxOpenEvent>> targets = new ArrayList<>();
    private final ComboBox comboBox;
    private BBjControl bbjctrl;


    @SuppressWarnings({"static-access"})
    public ComboBoxOpenEventSink(ComboBox cb) {
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings({"static-access"})
    public ComboBoxOpenEventSink(ComboBox cb, Consumer<ComboBoxOpenEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @SuppressWarnings("java.S1172")
    public void openEvent(BBjListOpenEvent ev) { //NOSONAR
        ComboBoxOpenEvent dwcEv = new ComboBoxOpenEvent(this.comboBox);
        Iterator<Consumer<ComboBoxOpenEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ComboBoxOpenEvent> callback) { targets.add(callback); }


    
}
