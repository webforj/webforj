package org.dwcj.component.combobox.sink;

import com.basis.bbj.proxies.event.BBjListOpenEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.combobox.ComboBox;
import org.dwcj.component.combobox.event.ComboBoxOpenEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class TextComboBoxOpenEventSink {

    private ArrayList<Consumer<ComboBoxOpenEvent>> targets = new ArrayList<>();
    private final ComboBox textComboBox;
    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public TextComboBoxOpenEventSink(ComboBox cb) {
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public TextComboBoxOpenEventSink(ComboBox cb, Consumer<ComboBoxOpenEvent> callback) {
        this.targets.add(callback);
        this.textComboBox = cb;

        try {
            bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_OPEN,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "openEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void openEvent(BBjListOpenEvent ev) { //NOSONAR
        ComboBoxOpenEvent dwcEv = new ComboBoxOpenEvent(this.textComboBox);
        Iterator<Consumer<ComboBoxOpenEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ComboBoxOpenEvent> callback) { targets.add(callback); }
    
}
