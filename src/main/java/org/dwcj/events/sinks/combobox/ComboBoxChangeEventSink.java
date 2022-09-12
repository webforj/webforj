package org.dwcj.events.sinks.combobox;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.ComboBox;
import org.dwcj.events.combobox.ComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ComboBoxChangeEventSink {

    private ArrayList<Consumer<ComboBoxChangeEvent>> targets;

    private final ComboBox comboBox;


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
    }

    public void changeEvent(BBjListChangeEvent ev) { // NOSONAR
        ComboBoxChangeEvent dwcEv = new ComboBoxChangeEvent(this.comboBox);
        Iterator<Consumer<ComboBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ComboBoxChangeEvent> callback) { targets.add(callback); }
}
