package org.dwcj.events.sinks.comboBox;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.comboBox.ComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ComboBoxChangeEventSink {

    private ArrayList<Consumer<ComboBoxChangeEvent>> targets;

    private final ComboBox comboBox;

    private final BBjControl ctrl;

    @SuppressWarnings({"static-access"})
    public ComboBoxChangeEventSink(ComboBox cb, Consumer<ComboBoxChangeEvent> callback) {
        this.targets = new ArrayList<>();
        this.targets.add(callback);
        this.comboBox = cb;

        BBjControl bbjctrl = null;
        try{
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.ctrl=bbjctrl;
    }

    public void changeEvent(BBjListChangeEvent ev) {
        ComboBoxChangeEvent dwc_ev = new ComboBoxChangeEvent(this.comboBox);
        Iterator<Consumer<ComboBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwc_ev);
    }

    public void addCallback(Consumer<ComboBoxChangeEvent> callback) { targets.add(callback); }
}
