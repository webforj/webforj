package org.dwcj.component.choicebox.sink;

import com.basis.bbj.proxies.event.BBjListChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.component.choicebox.ComboBox;
import org.dwcj.component.choicebox.event.ComboBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public class ComboBoxChangeEventSink {

    private ArrayList<Consumer<ComboBoxChangeEvent>> targets;

    private final ComboBox comboBox;


    @SuppressWarnings({"static-access"})
    public ComboBoxChangeEventSink(ComboBox cb) {
        this.targets = new ArrayList<>();
        this.comboBox = cb;

        BBjControl bbjctrl = null;
        try{
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CHANGE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this,"changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }
    
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
            Environment.logError(e);
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
